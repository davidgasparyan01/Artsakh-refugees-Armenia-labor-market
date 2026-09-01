# ==============================================================================
# IMPACT OF REFUGEE INFLUX ON ARMENIA'S LABOR MARKET
# INCLUDES: OLS/IV ESTIMATION, PLACEBO TESTS, WILD CLUSTER BOOTSTRAP,
#           AND FIRST-STAGE INSTRUMENT STRENGTH DIAGNOSTICS

# ==============================================================================
# 0. INSTALL AND LOAD PACKAGES
# ==============================================================================
# install.packages(c("dagitty", "ggdag", "ggplot2", "dplyr", "patchwork", "ggraph", "tidyverse", "lmtest", "sandwich", "AER"))

library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggraph)
library(tidyverse)
library(lmtest)
library(sandwich)
library(AER)


# ==============================================================================
# 1. PROCESS ARMSTAT DATA & BUILD PANEL  
# ==============================================================================
unemp_df <- read_csv("PS-eu-1_20260307-032935.csv", skip = 2, show_col_types = FALSE) %>%
  filter(str_detect(indicators, "by RA marzes and Yerevan")) %>%
  mutate(region = str_trim(str_extract(indicators, "[^-]+$"))) %>%
  select(-indicators) %>% pivot_longer(-region, names_to = "year_raw", values_to = "u_rate", values_transform = list(u_rate = as.character)) %>%
  mutate(year = suppressWarnings(as.numeric(str_remove_all(year_raw, "\\*"))), u_rate = suppressWarnings(as.numeric(u_rate))) %>%
  group_by(region, year) %>% slice_tail(n = 1) %>% ungroup() %>% select(-year_raw)

wage_df <- read_csv("PS-eu-19_20260307-044330.csv", skip = 2, show_col_types = FALSE) %>%
  filter(str_detect(indicators, "by RA Marzes and Yerevan") & !str_detect(indicators, "Sectors of Economy")) %>%
  mutate(region = str_trim(str_extract(indicators, "[^-]+$"))) %>%
  select(-indicators) %>% pivot_longer(-region, names_to = "year_raw", values_to = "wage_k", values_transform = list(wage_k = as.character)) %>%
  filter(year_raw != "2017***") %>%
  mutate(year = suppressWarnings(as.numeric(str_remove_all(year_raw, "\\*"))), wage_k = suppressWarnings(as.numeric(wage_k)) / 1000) %>%
  group_by(region, year) %>% slice_tail(n = 1) %>% ungroup() %>% select(-year_raw)

unemp_lvl_df <- read_tsv("PS-eu-3_20260307-043949.csv", skip = 2, show_col_types = FALSE) %>%
  filter(str_detect(indicators, "by RA marzes and Yerevan")) %>%
  mutate(region = str_trim(str_extract(indicators, "[^-]+$"))) %>%
  select(-indicators) %>% pivot_longer(-region, names_to = "year_raw", values_to = "unemp_lvl", values_transform = list(unemp_lvl = as.character)) %>%
  mutate(year = suppressWarnings(as.numeric(str_remove_all(year_raw, "\\*"))), unemp_lvl = suppressWarnings(as.numeric(unemp_lvl))) %>%
  group_by(region, year) %>% slice_tail(n = 1) %>% ungroup() %>% select(-year_raw)

emp_lvl_df <- read_tsv("PS-eu-13_20260307-043529.csv", skip = 2, show_col_types = FALSE) %>%
  filter(str_detect(indicators, "by RA marzes and Yerevan")) %>%
  mutate(region = str_trim(str_extract(indicators, "[^-]+$"))) %>%
  select(-indicators) %>% pivot_longer(-region, names_to = "year_raw", values_to = "emp_lvl", values_transform = list(emp_lvl = as.character)) %>%
  mutate(year = suppressWarnings(as.numeric(str_remove_all(year_raw, "\\*"))), emp_lvl = suppressWarnings(as.numeric(emp_lvl))) %>%
  group_by(region, year) %>% slice_tail(n = 1) %>% ungroup() %>% select(-year_raw)

wage_raw <- read_csv("PS-eu-19_20260307-044330.csv", skip = 2, show_col_types = FALSE)

real_idx_row <- wage_raw %>% filter(str_detect(indicators, "^\"?Real wage index")) %>% select(-indicators) %>%
  pivot_longer(everything(), names_to = "year_raw", values_to = "real_idx", values_transform = list(real_idx = as.character)) %>%
  filter(year_raw != "2017***") %>%
  mutate(year = suppressWarnings(as.numeric(str_remove_all(year_raw, "\\*"))), real_idx = suppressWarnings(as.numeric(real_idx))) %>%
  filter(!is.na(year), !is.na(real_idx)) %>% group_by(year) %>% slice_tail(n = 1) %>% ungroup() %>% select(year, real_idx)

nat_wage_row <- wage_raw %>% filter(str_detect(indicators, "^\"?Average Monthly Nominal Wages, AMD")) %>% select(-indicators) %>%
  pivot_longer(everything(), names_to = "year_raw", values_to = "nat_wage", values_transform = list(nat_wage = as.character)) %>%
  filter(year_raw != "2017***") %>%
  mutate(year = suppressWarnings(as.numeric(str_remove_all(year_raw, "\\*"))), nat_wage = suppressWarnings(as.numeric(nat_wage))) %>%
  filter(!is.na(year), !is.na(nat_wage)) %>% group_by(year) %>% slice_tail(n = 1) %>% ungroup() %>% select(year, nat_wage)

price_df <- nat_wage_row %>% inner_join(real_idx_row, by = "year") %>% arrange(year) %>% filter(year >= 2008) %>%
  mutate(nom_growth = nat_wage / lag(nat_wage), cpi_ratio = nom_growth / (real_idx / 100),
         cpi_ratio = ifelse(is.na(cpi_ratio), 1, cpi_ratio), price_level = cumprod(cpi_ratio))

wage_df <- wage_df %>% left_join(price_df %>% select(year, price_level), by = "year") %>% mutate(real_wage_k = wage_k / price_level)

pop_data <- read_csv("armenia_population.csv", show_col_types = FALSE) %>%
  rename(region = marz, pop_k = population_thsd) %>%
  mutate(region = ifelse(str_detect(region, "Yerevan"), "Yerevan", region)) %>%
  filter(region != "RA Total") %>% group_by(region, year) %>% slice_tail(n = 1) %>% ungroup() %>% select(region, year, pop_k)

panel <- unemp_df %>%
  left_join(wage_df, by = c("region", "year")) %>%
  left_join(unemp_lvl_df, by = c("region", "year")) %>%
  left_join(emp_lvl_df, by = c("region", "year")) %>%
  left_join(pop_data, by = c("region", "year")) %>%
  filter(year >= 2008) %>%
  mutate(
    region_f = as.factor(str_replace_all(region, " ", "_")),
    year_f   = as.factor(year),
    trend    = year - min(year, na.rm = TRUE),
    post2023 = ifelse(year >= 2023, 1, 0)
  ) %>% drop_na(u_rate, real_wage_k, unemp_lvl, emp_lvl, pop_k, year)

# ==============================================================================
# 2. TREATMENT & INSTRUMENT CONSTRUCTION   (unchanged from original)
# ==============================================================================
ref_recent <- tibble(
  region = c("Aragatsotn", "Ararat", "Armavir", "Gegharkunik", "Kotayk", "Lori", "Shirak", "Syunik", "Tavush", "Vayots Dzor", "Yerevan"),
  ref_oct23_abs = c(2942, 8158, 6185, 3927, 8458, 1881, 3036, 14301, 2791, 3722, 42439),
  ref_dec24_abs = c(3854, 11986, 10970, 3537, 18622, 4229, 4034, 6547, 2962, 2135, 46512)
)
total_shock_23 <- sum(ref_recent$ref_oct23_abs)

panel <- panel %>%
  left_join(ref_recent, by = "region") %>%
  mutate(
    ref_oct23_pct = (ref_oct23_abs / (pop_k * 1000)) * 100,
    ref_dec24_pct = (ref_dec24_abs / (pop_k * 1000)) * 100,
    treat_23_static = post2023 * ref_oct23_pct,
    treat_24_static = post2023 * ref_dec24_pct,
    treat_dynamic = case_when(year == 2023 ~ ref_oct23_pct, year >= 2024 ~ ref_dec24_pct, TRUE ~ 0)
  )

pop_2023 <- pop_data %>% filter(year == 2023)

iom_history_all <- read_csv("demographics_residing_arm.csv", show_col_types = FALSE) %>%
  filter(`Country of Origin Code` == "AZE", Year %in% 2020:2022) %>%
  mutate(
    region_clean = str_replace(location, " : City| : Marz - Province", ""),
    region_clean = str_replace(region_clean, "Gergharkunik", "Gegharkunik"),
    region_clean = ifelse(str_detect(region_clean, "Yerevan"), "Yerevan", region_clean)
  ) %>% filter(region_clean %in% pop_2023$region)

iv_20_data <- iom_history_all %>% filter(Year == 2020) %>%
  group_by(region_clean) %>% summarise(ref_abs = sum(Total, na.rm = TRUE)) %>%
  mutate(total_base = sum(ref_abs)) %>%
  left_join(pop_2023, by = c("region_clean" = "region")) %>%
  mutate(iv_pct_20 = ((ref_abs / total_base) * total_shock_23) / (pop_k * 1000) * 100) %>% select(region_clean, iv_pct_20)

panel <- panel %>% left_join(iv_20_data, by = c("region" = "region_clean")) %>% mutate(treat_IV_20 = post2023 * iv_pct_20)

# ==============================================================================
# 3. CORE CRVE / WILD CLUSTER BOOTSTRAP HELPERS
# ==============================================================================

# Number of clusters currently present in a (possibly filtered) data frame --
# used throughout to set t(G-1) degrees of freedom correctly, including in
# the leave-one-region-out diagnostics where G drops from 11 to 10.
get_G <- function(data, cluster_var = "region_f") {
  length(unique(droplevels(data[[cluster_var]])))
}

# Second-stage (treatment-effect) restricted wild cluster bootstrap (WCR).
wild_cluster_boot_iv <- function(panel_data, iv_model,
                                 dep      = "u_rate",
                                 endog    = "treat_dynamic",
                                 instr    = "treat_IV_20",
                                 controls_str = "real_wage_k",
                                 fe_trend = "region_f + year_f + region_f:trend",
                                 cluster_var = "region_f",
                                 B = 9999, weights = c("rademacher", "webb"),
                                 seed = 20232024, quiet = FALSE) {
  
  set.seed(seed)
  weights <- match.arg(weights, several.ok = TRUE)
  
  dat <- panel_data[as.integer(rownames(model.frame(iv_model))), ]
  cl  <- droplevels(dat[[cluster_var]])
  clusters <- levels(cl); G <- length(clusters); N <- nrow(dat)
  
  ctrl_part <- if (controls_str != "") paste("+", controls_str) else ""
  f_iv_str  <- paste(dep, "~", endog, ctrl_part, "+", fe_trend, "|", instr, ctrl_part, "+", fe_trend)
  f_iv      <- as.formula(f_iv_str)
  
  f_restr_str <- paste(dep, "~", fe_trend, ctrl_part)
  f_restr     <- as.formula(f_restr_str)
  
  # Observed cluster-robust (CRVE, HC1-corrected) t-statistic
  V0    <- sandwich::vcovCL(iv_model, cluster = cl, type = "HC1")
  b_obs <- coef(iv_model)[[endog]]
  se_obs<- sqrt(V0[endog, endog])
  t_obs <- b_obs / se_obs
  
  m_restr <- lm(f_restr, data = dat)
  u_tilde <- residuals(m_restr)
  y_fit0  <- fitted(m_restr)
  
  rademacher <- function(G) sample(c(-1, 1), G, replace = TRUE)
  webb6 <- function(G) sample(c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2)), G, replace = TRUE)
  
  run <- function(wfun, label) {
    t_star <- numeric(B)
    for (b in seq_len(B)) {
      w  <- wfun(G); names(w) <- clusters
      wv <- w[as.character(cl)]
      ds <- dat; ds[[dep]] <- as.numeric(y_fit0 + wv * u_tilde)
      ms <- AER::ivreg(f_iv, data = ds)
      Vs <- sandwich::vcovCL(ms, cluster = cl, type = "HC1")
      t_star[b] <- coef(ms)[[endog]] / sqrt(Vs[endog, endog])
    }
    p_sym <- mean(abs(t_star) >= abs(t_obs))
    p_eq  <- 2 * min(mean(t_star <= t_obs), mean(t_star >= t_obs))
    
    if (!quiet) {
      cat(sprintf("  [%-12s] p(symmetric) = %.4f | p(equal-tail) = %.4f\n", label, p_sym, p_eq))
    }
    c(p_sym = p_sym, p_eq = p_eq)
  }
  
  if (!quiet) {
    cat("=====================================================================\n")
    cat(sprintf(" Wild cluster bootstrap | %s | H0: %s = 0\n", dep, endog))
    cat("=====================================================================\n")
    cat(sprintf("  Estimate (2SLS)      : %+.4f\n", b_obs))
    cat(sprintf("  CRVE (HC1) SE        : %.4f\n",  se_obs))
    cat(sprintf("  CRVE (HC1) t, df=G-1 : %+.4f\n", t_obs))
    cat(sprintf("  Clusters G / Obs N   : %d / %d\n", G, N))
    cat(sprintf("  Bootstrap reps B     : %d\n\n", B))
  }
  
  out <- list(estimate = b_obs, se = se_obs, t = t_obs, G = G, N = N, B = B)
  if ("rademacher" %in% weights) out$rademacher <- run(rademacher, "Rademacher")
  if ("webb"       %in% weights) out$webb       <- run(webb6,      "Webb 6-point")
  invisible(out)
}

# ==============================================================================
# 4. FIRST-STAGE DIAGNOSTICS
# ==============================================================================
first_stage_diagnostics <- function(panel_data, treat_var, controls_str,
                                    B = 999, seed = 2023) {
  set.seed(seed)
  panel_data$region_f <- droplevels(panel_data$region_f) 
  cl <- panel_data$region_f
  G  <- length(levels(cl))
  ctrl_part <- if (controls_str != "") paste("+", controls_str) else ""
  
  f_full  <- as.formula(paste(treat_var, "~ treat_IV_20", ctrl_part, "+ region_f + as.factor(year) + region_f:trend"))
  f_restr <- as.formula(paste(treat_var, "~", ctrl_part, "+ region_f + as.factor(year) + region_f:trend"))
  
  m1 <- lm(f_full,  data = panel_data)
  m0 <- lm(f_restr, data = panel_data)
  
  # --- CRVE (HC1-corrected), clustered by region: primary F-statistic ---
  V1     <- sandwich::vcovCL(m1, cluster = cl, type = "HC1")
  t_obs  <- coef(m1)["treat_IV_20"] / sqrt(V1["treat_IV_20", "treat_IV_20"])
  F_crve <- as.numeric(t_obs^2)
  crve_p <- as.numeric(2 * pt(abs(t_obs), df = G - 1, lower.tail = FALSE))
  
  # --- Webb 6-point wild cluster bootstrap p-value ---
  u_tilde <- residuals(m0)
  y_fit0  <- fitted(m0)
  webb6   <- function(G) sample(c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2)), G, replace = TRUE)
  
  t_star <- numeric(B)
  for (i in seq_len(B)) {
    w <- webb6(G); names(w) <- levels(cl)
    panel_data$y_star <- y_fit0 + w[as.character(cl)] * u_tilde
    ms <- lm(as.formula(paste("y_star ~ treat_IV_20", ctrl_part, "+ region_f + as.factor(year) + region_f:trend")), data = panel_data)
    Vs <- sandwich::vcovCL(ms, cluster = cl, type = "HC1")
    t_star[i] <- coef(ms)["treat_IV_20"] / sqrt(Vs["treat_IV_20", "treat_IV_20"])
  }
  webb_p <- mean(abs(t_star) >= abs(t_obs))
  
  list(F_crve = F_crve, F_hc1raw = F_hc1raw, crve_p = crve_p, webb_p = webb_p, G = G)
}

# ==============================================================================
# 5. PLACEBO TEST FUNCTION
# ==============================================================================
run_placebos <- function(data, dep_var, intensity_type, is_iv = FALSE, controls_str = "real_wage_k") {
  cat(sprintf("\n--- Placebo Tests for %s (Lags -1 to -10) ---\n", dep_var))
  cat(sprintf("%-4s | %-9s | %-7s | %-14s | %-12s\n", "Lag", "Estimate", "p-Asymp", "Asymp. Sig", "Webb WCB p"))
  cat(rep("-", 60), "\n", sep = "")
  
  ctrl_part <- if (controls_str != "") paste0("+ ", controls_str) else ""
  
  for (lag in 1:10) {
    placebo_year <- 2023 - lag
    temp_data <- data %>% filter(year < 2023)
    
    if (intensity_type == "static_23") {
      temp_data$placebo_treat <- ifelse(temp_data$year >= placebo_year, temp_data$ref_oct23_pct, 0)
    } else if (intensity_type == "static_24") {
      temp_data$placebo_treat <- ifelse(temp_data$year >= placebo_year, temp_data$ref_dec24_pct, 0)
    } else {
      temp_data$placebo_treat <- case_when(
        temp_data$year == placebo_year ~ temp_data$ref_oct23_pct,
        temp_data$year >= placebo_year + 1 ~ temp_data$ref_dec24_pct, TRUE ~ 0)
    }
    
    if (is_iv) {
      temp_data$placebo_iv <- ifelse(temp_data$year >= placebo_year, temp_data$iv_pct_20, 0)
      iv_form <- paste(dep_var, "~ placebo_treat", ctrl_part, "+ region_f + as.factor(year) + region_f:trend | placebo_iv", ctrl_part, "+ region_f + as.factor(year) + region_f:trend")
      model <- ivreg(as.formula(iv_form), data = temp_data)
      
      G_here <- get_G(temp_data)
      coefs <- coeftest(model, vcov = vcovCL(model, cluster = ~region_f, type = "HC1"), df = G_here - 1)
      if ("placebo_treat" %in% rownames(coefs)) {
        est  <- coefs["placebo_treat", "Estimate"]
        pval <- coefs["placebo_treat", "Pr(>|t|)"]
        sig  <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
        
        wcb_res <- wild_cluster_boot_iv(
          panel_data = temp_data, iv_model = model, dep = dep_var,
          endog = "placebo_treat", instr = "placebo_iv", controls_str = controls_str,
          B = 999, weights = "webb", quiet = TRUE
        )
        webb_p <- wcb_res$webb["p_sym"]
        webb_sig <- ifelse(webb_p < 0.01, "***", ifelse(webb_p < 0.05, "**", ifelse(webb_p < 0.1, "*", "")))
        
        cat(sprintf("-%-3d | %-9.4f | %-7.4f | %-14s | %-6.4f %s\n", lag, est, pval, sig, webb_p, webb_sig))
      }
    } else {
      base_form <- paste(dep_var, "~ placebo_treat", ctrl_part, "+ region_f + as.factor(year) + region_f:trend")
      model <- lm(as.formula(base_form), data = temp_data)
      G_here <- get_G(temp_data)
      coefs <- coeftest(model, vcov = vcovCL(model, cluster = ~region_f, type = "HC1"), df = G_here - 1)
      if ("placebo_treat" %in% rownames(coefs)) {
        est  <- coefs["placebo_treat", "Estimate"]
        pval <- coefs["placebo_treat", "Pr(>|t|)"]
        sig  <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
        cat(sprintf("-%-3d | %-9.4f | %-7.4f | %-14s | N/A\n", lag, est, pval, sig))
      }
    }
  }
}

# ==============================================================================
# 6. MODEL ESTIMATION FUNCTION
# ==============================================================================
estimate_models <- function(panel_data, dep_var, controls_str = "real_wage_k") {
  results <- list()
  ctrl_part <- if (controls_str != "") paste("+", controls_str) else ""
  
  results$ols_dyn <- lm(as.formula(paste(dep_var, "~ treat_dynamic", ctrl_part, "+ region_f + year_f + region_f:trend")), data = panel_data)
  
  results$iv_pooled <- ivreg(as.formula(paste(dep_var, "~ treat_dynamic", ctrl_part, "+ region_f + year_f + region_f:trend | treat_IV_20", ctrl_part, "+ region_f + year_f + region_f:trend")), data = panel_data)
  
  p23 <- panel_data %>% filter(year <= 2023)
  results$iv_2023 <- ivreg(as.formula(paste(dep_var, "~ treat_23_static", ctrl_part, "+ region_f + year_f + region_f:trend | treat_IV_20", ctrl_part, "+ region_f + year_f + region_f:trend")), data = p23)
  
  p24 <- panel_data %>% filter(year != 2023)
  results$iv_2024 <- ivreg(as.formula(paste(dep_var, "~ treat_24_static", ctrl_part, "+ region_f + year_f + region_f:trend | treat_IV_20", ctrl_part, "+ region_f + year_f + region_f:trend")), data = p24)
  
  return(results)
}

# ==============================================================================
# 7. SECOND-STAGE (TREATMENT-EFFECT) WEBB BOOTSTRAP WRAPPER
# ==============================================================================
run_wcb_for_panel <- function(models, panel_data, dep_var, controls_str) {
  cat(sprintf("\n>>> Running Webb WCB (Second Stage) for %s ...\n", dep_var))
  wcb_pool <- wild_cluster_boot_iv(panel_data, models$iv_pooled, dep = dep_var, endog = "treat_dynamic", instr = "treat_IV_20", controls_str = controls_str, B = 9999, weights = "webb", quiet = TRUE)
  p23 <- panel_data %>% filter(year <= 2023)
  wcb_23 <- wild_cluster_boot_iv(p23, models$iv_2023, dep = dep_var, endog = "treat_23_static", instr = "treat_IV_20", controls_str = controls_str, B = 9999, weights = "webb", quiet = TRUE)
  p24 <- panel_data %>% filter(year != 2023)
  wcb_24 <- wild_cluster_boot_iv(p24, models$iv_2024, dep = dep_var, endog = "treat_24_static", instr = "treat_IV_20", controls_str = controls_str, B = 9999, weights = "webb", quiet = TRUE)
  
  return(c(NA, wcb_pool$webb["p_sym"], wcb_23$webb["p_sym"], wcb_24$webb["p_sym"]))
}

# ==============================================================================
# 7B. WILD BOOTSTRAP ANDERSON-RUBIN (AR) TEST -- FUNCTION DEFINITIONS
# ==============================================================================

# Core AR test for a single specification. Tests H0: beta = beta0 (default 0)
# for the coefficient on `treat_var` in the outcome equation for `dep_var`,
# using `instr_var` as the instrument.
ar_test <- function(panel_data, dep_var, treat_var,
                    instr_var    = "treat_IV_20",
                    controls_str = "real_wage_k",
                    fe_trend     = "region_f + year_f + region_f:trend",
                    cluster_var  = "region_f",
                    beta0 = 0, B = 9999, weights = "webb", seed = 20232024) {
  set.seed(seed)
  dat <- panel_data
  dat[[cluster_var]] <- droplevels(dat[[cluster_var]])
  cl <- dat[[cluster_var]]
  clusters <- levels(cl)
  G <- length(clusters)
  
  ctrl_part <- if (controls_str != "") paste("+", controls_str) else ""
  
  # --- Null-restricted residual of the OUTCOME equation (imposes beta = beta0) ---
  dat$.y_adj <- if (beta0 == 0) dat[[dep_var]] else dat[[dep_var]] - beta0 * dat[[treat_var]]
  f_restr <- as.formula(paste(".y_adj ~", fe_trend, ctrl_part))
  m0 <- lm(f_restr, data = dat)
  eps_restr <- residuals(m0)
  
  # --- Instrument residualized on the same exogenous controls (Z-tilde) ---
  f_z <- as.formula(paste(instr_var, "~", fe_trend, ctrl_part))
  mz <- lm(f_z, data = dat)
  Z_tilde <- residuals(mz)
  
  # --- Cluster-level cross-moment sums: F_j = sum_i Z_tilde_ij * eps_restr_ij ---
  f_ij <- Z_tilde * eps_restr
  F_j  <- tapply(f_ij, cl, sum)[clusters]
  
  # --- Observed AR statistic (raw sum; any fixed rescaling of it would cancel
  #     in the bootstrap p-value below, so none is needed) ---
  S_obs <- sum(F_j)
  
  # --- Wild bootstrap: random cluster-level sign flips of the F_j, no
  #     re-estimation required ---
  rademacher <- function(G) sample(c(-1, 1), G, replace = TRUE)
  webb6      <- function(G) sample(c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2)), G, replace = TRUE)
  wfun <- if (weights == "webb") webb6 else rademacher
  
  S_star <- numeric(B)
  for (b in seq_len(B)) {
    g <- wfun(G)
    S_star[b] <- sum(g * F_j)
  }
  
  p_ar <- mean(abs(S_star) >= abs(S_obs))
  
  list(beta0 = beta0, S_obs = S_obs, F_j = F_j, G = G, B = B,
       weights = weights, p_ar = p_ar)
}

# Runs the AR test for the Pooled / 2023 Only / 2024 Only specifications for
# one outcome panel, mirroring run_wcb_for_panel()'s structure exactly.
run_ar_for_panel <- function(panel_data, dep_var, controls_str, B = 9999) {
  cat(sprintf("\n>>> Running Wild Bootstrap AR Test for %s ...\n", dep_var))
  
  ar_pooled <- ar_test(panel_data, dep_var, "treat_dynamic", controls_str = controls_str, B = B)
  cat(sprintf("  [Pooled   ] AR p-value = %.4f\n", ar_pooled$p_ar))
  
  p23 <- panel_data %>% filter(year <= 2023)
  ar_2023 <- ar_test(p23, dep_var, "treat_23_static", controls_str = controls_str, B = B)
  cat(sprintf("  [2023 Only] AR p-value = %.4f\n", ar_2023$p_ar))
  
  p24 <- panel_data %>% filter(year != 2023)
  ar_2024 <- ar_test(p24, dep_var, "treat_24_static", controls_str = controls_str, B = B)
  cat(sprintf("  [2024 Only] AR p-value = %.4f\n", ar_2024$p_ar))
  
  list(pooled = ar_pooled, y2023 = ar_2023, y2024 = ar_2024)
}

# ==============================================================================
# 8. RUN CORE FIRST-STAGE DIAGNOSTICS ONCE
# ==============================================================================
cat("\n######################################################################\n")
cat("# STEP 1: FIRST-STAGE DIAGNOSTICS (CRVE F-stat + CRVE p + Webb p)     #\n")
cat("######################################################################\n")

fs_with_ctrl_pooled <- first_stage_diagnostics(panel, "treat_dynamic", "real_wage_k")
fs_with_ctrl_2023   <- first_stage_diagnostics(panel %>% filter(year <= 2023), "treat_23_static", "real_wage_k")
fs_with_ctrl_2024   <- first_stage_diagnostics(panel %>% filter(year != 2023), "treat_24_static", "real_wage_k")

fs_no_ctrl_pooled <- first_stage_diagnostics(panel, "treat_dynamic", "")
fs_no_ctrl_2023   <- first_stage_diagnostics(panel %>% filter(year <= 2023), "treat_23_static", "")
fs_no_ctrl_2024   <- first_stage_diagnostics(panel %>% filter(year != 2023), "treat_24_static", "")

# ==============================================================================
# 9. PANEL A: DEPENDENT VARIABLE = UNEMPLOYMENT RATE
# ==============================================================================
cat("\n######################################################################\n")
cat("# PANEL A: DEPENDENT VARIABLE = UNEMPLOYMENT RATE                    #\n")
cat("######################################################################\n")
unemp_models <- estimate_models(panel, "u_rate", controls_str = "real_wage_k")
wcb_pvals_A  <- run_wcb_for_panel(unemp_models, panel, "u_rate", "real_wage_k")
ar_pvals_A   <- run_ar_for_panel(panel, "u_rate", "real_wage_k")
run_placebos(panel, "u_rate", "dynamic", is_iv = TRUE, controls_str = "real_wage_k")

# ==============================================================================
# 10. PANEL B: DEPENDENT VARIABLE = LOG(REAL WAGE), 2008 base
# ==============================================================================
cat("\n######################################################################\n")
cat("# PANEL B: DEPENDENT VARIABLE = LOG(REAL WAGE), 2008 base            #\n")
cat("######################################################################\n")

if (sum(panel$real_wage_k <= 0, na.rm = TRUE) > 0) cat("WARNING: non-positive real_wage_k values present -- log() will produce -Inf/NaN there.\n")
panel$log_real_wage_k <- log(panel$real_wage_k)

rwage_models <- estimate_models(panel, "log_real_wage_k", controls_str = "")
wcb_pvals_B  <- run_wcb_for_panel(rwage_models, panel, "log_real_wage_k", "")
ar_pvals_B   <- run_ar_for_panel(panel, "log_real_wage_k", "")
run_placebos(panel, "log_real_wage_k", "dynamic", is_iv = TRUE, controls_str = "")

# ==============================================================================
# 11. PANEL C: DEPENDENT VARIABLE = UNEMPLOYED PERSONS (thousands)
# ==============================================================================
cat("\n######################################################################\n")
cat("# PANEL C: DEPENDENT VARIABLE = UNEMPLOYED PERSONS (thousands)       #\n")
cat("######################################################################\n")
unemp_lvl_models <- estimate_models(panel, "unemp_lvl", controls_str = "real_wage_k")
wcb_pvals_C      <- run_wcb_for_panel(unemp_lvl_models, panel, "unemp_lvl", "real_wage_k")
ar_pvals_C       <- run_ar_for_panel(panel, "unemp_lvl", "real_wage_k")
run_placebos(panel, "unemp_lvl", "dynamic", is_iv = TRUE, controls_str = "real_wage_k")

# ==============================================================================
# 12. PANEL D: DEPENDENT VARIABLE = EMPLOYED PERSONS (thousands)
# ==============================================================================
cat("\n######################################################################\n")
cat("# PANEL D: DEPENDENT VARIABLE = EMPLOYED PERSONS (thousands)         #\n")
cat("######################################################################\n")
emp_lvl_models <- estimate_models(panel, "emp_lvl", controls_str = "real_wage_k")
wcb_pvals_D    <- run_wcb_for_panel(emp_lvl_models, panel, "emp_lvl", "real_wage_k")
ar_pvals_D     <- run_ar_for_panel(panel, "emp_lvl", "real_wage_k")
run_placebos(panel, "emp_lvl", "dynamic", is_iv = TRUE, controls_str = "real_wage_k")

wcb_pvals <- list(A = wcb_pvals_A, B = wcb_pvals_B, C = wcb_pvals_C, D = wcb_pvals_D)
ar_pvals  <- list(A = ar_pvals_A,  B = ar_pvals_B,  C = ar_pvals_C,  D = ar_pvals_D)


# ==============================================================================
# 13. HTML TABLE GENERATION -- SHARED HELPERS
# ==============================================================================
get_coef <- function(model, treat_var, df) {
  ct <- tryCatch(
    coeftest(model, vcov = vcovCL(model, cluster = ~region_f, type = "HC1"), df = df),
    warning = function(w) suppressWarnings(coeftest(model, vcov = vcovCL(model, cluster = ~region_f, type = "HC1"), df = df))
  )
  if (treat_var %in% rownames(ct)) {
    pval <- ct[treat_var, "Pr(>|t|)"]
    return(list(est = ct[treat_var, "Estimate"], se = ct[treat_var, "Std. Error"], pval = pval,
                stars = ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))))
  }
  return(list(est = NA, se = NA, pval = NA, stars = ""))
}

get_control <- function(model, ctrl_var = "real_wage_k", df) {
  ct <- tryCatch(
    coeftest(model, vcov = vcovCL(model, cluster = ~region_f, type = "HC1"), df = df),
    warning = function(w) suppressWarnings(coeftest(model, vcov = vcovCL(model, cluster = ~region_f, type = "HC1"), df = df))
  )
  if (ctrl_var %in% rownames(ct)) {
    pval <- ct[ctrl_var, "Pr(>|t|)"]
    return(list(est = ct[ctrl_var, "Estimate"], se = ct[ctrl_var, "Std. Error"],
                stars = ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))))
  }
  return(NULL)
}

fmt <- function(x, digits = 3) {
  # Vectorized: works both on single values and on whole vectors/columns.
  out <- formatC(round(x, digits), format = "f", digits = digits)
  out[is.na(x)] <- "\u2014"
  out
}

build_panel_rows <- function(models, panel_label, dep_var_label, show_control = TRUE,
                             wcb_pvals_arr = rep(NA, 4),
                             fs_objs = list(NA, NA, NA, NA),
                             ar_objs = list(NA, NA, NA, NA)) {
  # fs_objs: list of 4 elements aligned with columns (1)-(4); element 1 (OLS)
  # is always NA since the first-stage F only applies to the IV columns.
  # ar_objs: list of 4 elements aligned with columns (1)-(4); element 1 (OLS)
  # is always NA since the Anderson-Rubin test only applies to the IV columns.
  treat_vars <- c("treat_dynamic", "treat_dynamic", "treat_23_static", "treat_24_static")
  model_keys <- c("ols_dyn", "iv_pooled", "iv_2023", "iv_2024")
  
  coef_cells <- se_cells <- ctrl_cells <- ctrl_se <- n_cells <- f_cells <- ""
  
  for (i in seq_along(model_keys)) {
    m  <- models[[model_keys[i]]]
    df_here <- get_G(model.frame(m)) - 1
    ci <- get_coef(m, treat_vars[i], df = df_here)
    coef_cells <- paste0(coef_cells, "<td>", fmt(ci$est), ci$stars, "</td>")
    se_cells   <- paste0(se_cells,   "<td>(", fmt(ci$se), ")</td>")
    n_cells    <- paste0(n_cells,    "<td>", nobs(m), "</td>")
    
    if (is.list(fs_objs[[i]])) {
      # CRVE-based significance stars on the first-stage F, using the same
      # t(G-1) asymptotic p-value (crve_p) already computed in
      # first_stage_diagnostics(), and the same star convention as the
      # coefficient rows above.
      crve_p_here <- fs_objs[[i]]$crve_p
      crve_stars  <- ifelse(crve_p_here < 0.01, "***", ifelse(crve_p_here < 0.05, "**", ifelse(crve_p_here < 0.1, "*", "")))
      f_cells <- paste0(f_cells, "<td>", fmt(fs_objs[[i]]$F_crve, 2), crve_stars, "</td>")
    } else {
      f_cells <- paste0(f_cells, "<td>\u2014</td>")
    }
    
    if (show_control) {
      cc <- get_control(m, df = df_here)
      if (!is.null(cc)) {
        ctrl_cells <- paste0(ctrl_cells, "<td>", fmt(cc$est), cc$stars, "</td>")
        ctrl_se    <- paste0(ctrl_se, "<td>(", fmt(cc$se), ")</td>")
      } else {
        ctrl_cells <- paste0(ctrl_cells, "<td>\u2014</td>")
        ctrl_se    <- paste0(ctrl_se, "<td></td>")
      }
    }
  }
  
  rows <- paste0('<tr class="panel-header"><td colspan="5"><strong>Panel ', panel_label, ': ', dep_var_label, '</strong></td></tr>\n<tr><td>Treatment intensity</td>', coef_cells, '</tr>\n<tr class="se-row"><td></td>', se_cells, '</tr>\n')
  
  if (!all(is.na(wcb_pvals_arr))) {
    boot_cells <- ""
    for (i in 1:4) {
      wald_part <- if (is.na(wcb_pvals_arr[i])) "" else paste0("[p = ", fmt(as.numeric(wcb_pvals_arr[i]), 3), "]")
      # AR p-value sits alongside (beneath) the Wald/Webb p-value in the same
      # cell -- it does NOT replace it. Only present for IV columns where an
      # ar_objs entry was supplied (element 1 / OLS stays blank, as before).
      ar_part <- if (is.list(ar_objs[[i]])) paste0("<br><span style='font-size:11px;color:#555'>AR p = ", fmt(ar_objs[[i]]$p_ar, 3), "</span>") else ""
      boot_cells <- paste0(boot_cells, "<td>", wald_part, ar_part, "</td>")
    }
    rows <- paste0(rows, '<tr class="boot-row"><td><em>Webb Bootstrap p-value</em></td>', boot_cells, '</tr>\n')
  }
  
  if (show_control) rows <- paste0(rows, '<tr><td>Real wage (control)</td>', ctrl_cells, '</tr>\n<tr class="se-row"><td></td>', ctrl_se, '</tr>\n')
  
  rows <- paste0(rows, '<tr><td>First-stage F (CRVE)</td>', f_cells, '</tr>\n')
  
  # First-stage Webb p-value row: left exactly as before, unchanged.
  fs_boot_cells <- ""
  for (i in 1:4) {
    fs_boot_cells <- paste0(fs_boot_cells, if (is.list(fs_objs[[i]]))
      paste0("<td>[p = ", fmt(fs_objs[[i]]$webb_p, 3), "]</td>") else "<td></td>")
  }
  rows <- paste0(rows, '<tr class="boot-row"><td><em>First-stage Webb p-value</em></td>', fs_boot_cells, '</tr>\n')
  
  rows <- paste0(rows, '<tr class="bottom-panel"><td>Observations</td>', n_cells, '</tr>\n')
  return(rows)
}

generate_html_doc <- function(table_title, panel_A, panel_B, panel_C, panel_D, extra_notes = "") {
  paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><title>Regression Results</title><style>
  body { font-family: "Times New Roman", Times, serif; margin: 40px; } table { border-collapse: collapse; width: 100%; font-size: 13px; }
  th, td { padding: 4px 10px; text-align: center; } th { border-top: 2px solid black; border-bottom: 1px solid black; } td:first-child, th:first-child { text-align: left; }
  .panel-header td { text-align: left; padding-top: 14px; border-top: 1px solid #888; } .se-row td { font-size: 12px; color: #555; padding-top: 0px; padding-bottom: 2px; }
  .boot-row td { font-size: 12px; color: #333; padding-top: 0px; padding-bottom: 4px; font-style: italic; } .bottom-panel td { padding-bottom: 6px; }
  .final-border td { border-bottom: 2px solid black; } .note { font-size: 11px; margin-top: 8px; color: #333; line-height: 1.5; }
  caption { font-weight: bold; font-size: 14px; margin-bottom: 8px; text-align: left; } </style></head><body><table><caption>', table_title, '</caption><thead>
<tr><th style="width:20%"></th><th>(1)<br>OLS<br>Dynamic</th><th>(2)<br>IV Pooled<br>2023\u20132024</th><th>(3)<br>IV<br>2023 Only</th><th>(4)<br>IV<br>2024 Only</th></tr>
</thead><tbody>\n', panel_A, panel_B, panel_C, panel_D, '<tr class="final-border"><td colspan="5"></td></tr></tbody></table><p class="note">
Notes: All models include region fixed effects, year fixed effects, and region-specific linear trends.<br>
Panels A, C, and D include real wages (level, thousand AMD) as a control variable; Panel B does not, since real wage is the
dependent variable there, expressed in natural logs (coefficients are approximate semi-elasticities; multiply by 100 for a
percentage effect).<br>
Standard errors, t-statistics, and the first-stage F-statistic are all based on the cluster-robust variance estimator (CRVE), clustered
by region, with the HC1 small-sample correction and t(G\u22121) critical values (G = 11 regions).<br>
IV models use a Bartik instrument constructed from 2020 Artsakhi migration settlement shares.<br>', extra_notes,
         '<br>* p &lt; 0.10, ** p &lt; 0.05, *** p &lt; 0.01.</p></body></html>')
}

fs_row_objs_ctrl   <- list(NA, fs_with_ctrl_pooled, fs_with_ctrl_2023, fs_with_ctrl_2024)
fs_row_objs_noctrl <- list(NA, fs_no_ctrl_pooled,   fs_no_ctrl_2023,   fs_no_ctrl_2024)

ar_objs_A <- list(NA, ar_pvals$A$pooled, ar_pvals$A$y2023, ar_pvals$A$y2024)
ar_objs_B <- list(NA, ar_pvals$B$pooled, ar_pvals$B$y2023, ar_pvals$B$y2024)
ar_objs_C <- list(NA, ar_pvals$C$pooled, ar_pvals$C$y2023, ar_pvals$C$y2024)
ar_objs_D <- list(NA, ar_pvals$D$pooled, ar_pvals$D$y2023, ar_pvals$D$y2024)

table1_notes <- paste0(
  "The line beneath each Webb Bootstrap p-value reports the wild bootstrap Anderson-Rubin (AR) p-value for the same null ",
  "hypothesis (treatment coefficient = 0); the AR test controls size regardless of instrument strength (Wang and Zhang, 2024).<br>",
  "Stars on the First-stage F (CRVE) row denote significance of the CRVE asymptotic t(G\u22121) test on the instrument, using the ",
  "same convention as the coefficient stars; the separate First-stage Webb p-value row below it remains the more conservative benchmark."
)

# --- Table 1: main results (with CRVE-consistent SEs / F-stat) ---
writeLines(generate_html_doc(
  "Table 1: Impact of Refugee Influx on Labor Market Outcomes (2SLS IV and OLS, CRVE-Consistent)",
  build_panel_rows(unemp_models,     "A", "Unemployment Rate (%)",           TRUE,  wcb_pvals$A, fs_row_objs_ctrl,   ar_objs_A),
  build_panel_rows(rwage_models,     "B", "log(Real Wage), 2008 base",       FALSE, wcb_pvals$B, fs_row_objs_noctrl, ar_objs_B),
  build_panel_rows(unemp_lvl_models, "C", "Unemployed Persons (thousands)",  TRUE,  wcb_pvals$C, fs_row_objs_ctrl,   ar_objs_C),
  build_panel_rows(emp_lvl_models,   "D", "Employed Persons (thousands)",    TRUE,  wcb_pvals$D, fs_row_objs_ctrl,   ar_objs_D),
  extra_notes = table1_notes
), "regression_table_1_main.html")


# ==============================================================================
# 14. AR PLACEBO TESTS
# ==============================================================================
run_ar_placebos <- function(data, dep_var, intensity_type = "dynamic", controls_str = "real_wage_k", B = 999) {
  cat(sprintf("\n--- AR Placebo Tests for %s (Lags -1 to -10) ---\n", dep_var))
  cat(sprintf("%-4s | %-10s | %-6s\n", "Lag", "AR p-value", "Sig"))
  cat(rep("-", 30), "\n", sep = "")
  
  for (lag in 1:10) {
    placebo_year <- 2023 - lag
    temp_data <- data %>% filter(year < 2023)
    
    if (intensity_type == "static_23") {
      temp_data$placebo_treat <- ifelse(temp_data$year >= placebo_year, temp_data$ref_oct23_pct, 0)
    } else if (intensity_type == "static_24") {
      temp_data$placebo_treat <- ifelse(temp_data$year >= placebo_year, temp_data$ref_dec24_pct, 0)
    } else {
      temp_data$placebo_treat <- case_when(
        temp_data$year == placebo_year ~ temp_data$ref_oct23_pct,
        temp_data$year >= placebo_year + 1 ~ temp_data$ref_dec24_pct, TRUE ~ 0)
    }
    
    temp_data$placebo_iv <- ifelse(temp_data$year >= placebo_year, temp_data$iv_pct_20, 0)
    
    # Note: fe_trend uses as.factor(year), NOT the pre-built year_f column --
    # temp_data is a year-filtered subset, so year_f would carry unused
    # (post-2023) factor levels; recomputing the factor fresh avoids that.
    ar_res <- ar_test(
      temp_data, dep_var, "placebo_treat",
      instr_var    = "placebo_iv",
      controls_str = controls_str,
      fe_trend     = "region_f + as.factor(year) + region_f:trend",
      B = B
    )
    
    sig <- ifelse(ar_res$p_ar < 0.01, "***", ifelse(ar_res$p_ar < 0.05, "**", ifelse(ar_res$p_ar < 0.1, "*", "")))
    cat(sprintf("-%-3d | %-10.4f | %-6s\n", lag, ar_res$p_ar, sig))
  }
}

cat("\n######################################################################\n")
cat("# STEP 4: AR PLACEBO TESTS                                            #\n")
cat("######################################################################\n")

run_ar_placebos(panel, "u_rate",          controls_str = "real_wage_k")
run_ar_placebos(panel, "log_real_wage_k", controls_str = "")
run_ar_placebos(panel, "unemp_lvl",       controls_str = "real_wage_k")
run_ar_placebos(panel, "emp_lvl",         controls_str = "real_wage_k")

cat("\n===== Script Complete! One HTML table saved:\n")
cat("  1. regression_table_1_main.html\n")
cat("(First-stage instrument strength, leave-one-out sensitivity, regional\n")
cat(" relevance, and the AR weak-instrument-robust test have been folded\n")
cat(" into Table 1 -- see its Webb Bootstrap p-value and First-stage F\n")
cat(" (CRVE) rows -- or removed, per current scope.)\n")
cat("(AR placebo diagnostics printed to console above; no separate file.)\n")
