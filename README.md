# The Impact of the 2023 Artsakhi Refugee Influx on Regional Unemployment in Armenia

Replication code and data for the paper *"The Impact of the 2023 Artsakhi Refugee Influx on Regional Unemployment in Armenia"* by David Gasparyan (MA Economics, Data, and Policy, Central European University; 2026).

## Overview

In September 2023, more than 100,000 ethnic Armenian refugees fled Nagorno-Karabakh (Artsakh) to Armenia, roughly 3% of the country's population. This project estimates the causal effect of that influx on regional labor markets across Armenia's 11 administrative divisions (10 marzes and Yerevan), using a 2008-2024 annual panel.

**Identification.** Refugees may sort into regions based on local economic conditions, so OLS is likely biased. I use a shift-share (Bartik-style) instrument: the regional distribution of the ~91,000 Artsakhi Armenians displaced in 2020, scaled by the national 2023 inflow and the regional population. The 2SLS model includes region fixed effects, year fixed effects, region-specific linear time trends, and regional real wages as a control.

**Inference with few clusters.** With only 11 clusters, standard cluster-robust inference can over-reject. Every second-stage result is therefore reported with (i) CRVE standard errors (HC1, t(G-1) critical values), (ii) a Webb six-point wild cluster bootstrap p-value, and (iii) a wild-bootstrap Anderson-Rubin p-value, which remains valid when the instrument is weak.

## Main finding

A 1-percentage-point increase in refugee intensity lowers the regional unemployment rate by **0.69 percentage points** (Webb bootstrap p = 0.068; Anderson-Rubin p = 0.039). Ten placebo tests with earlier treatment dates show no evidence of pre-existing differential trends. Effects on real wages and on absolute employment and unemployment levels are statistically insignificant. The result is significant only at the 10% level and is based on 11 clusters, so it should be read as suggestive evidence rather than a precise estimate.

## Repository contents

| File | Description |
|---|---|
| `armenia_refugee_analysis.R` | Full analysis script: data processing, instrument construction, estimation, bootstrap inference, placebo tests, table output |
| `PS-eu-1_20260307-032935.csv` | ArmStat: unemployment rate by region and year |
| `PS-eu-3_20260307-043949.csv` | ArmStat: number of unemployed persons by region and year |
| `PS-eu-13_20260307-043529.csv` | ArmStat: number of employed persons by region and year |
| `PS-eu-19_20260307-044330.csv` | ArmStat: nominal wages by region, plus the national nominal wage and real wage index used to build the price deflator |
| `armenia_population.csv` | Regional population by year (compiled from ArmStat demographic handbooks) |
| `demographics_residing_arm.csv` | UNHCR (Humanitarian Data Exchange): displaced populations residing in Armenia by region; used for the 2020 settlement shares |
| `LICENSE` | MIT license |

## Data sources

- **Labor market indicators and wages:** Statistical Committee of the Republic of Armenia (ArmStat), ArmStatBank. Downloaded in March 2026.
- **Regional population:** ArmStat Demographic Handbooks of Armenia.
- **2020 settlement shares (instrument):** UNHCR, *Demographics and Locations of Forcibly Displaced and Stateless People Residing in Armenia*, via the Humanitarian Data Exchange.
- **2023 and 2024 regional refugee counts (treatment intensity):** hardcoded in the script (October 2023 and December 2024 waves).

**Real wages.** Regional consumer price indices are not available, so a national deflator is recovered from ArmStat's average nominal wage and real wage index series, and regional nominal wages are divided by it (2008 = base).

## Requirements

R with the following packages:

```r
install.packages(c("dplyr", "tidyverse", "lmtest", "sandwich", "AER"))
```

## How to run

1. Clone or download this repository and keep all files in one folder.
2. Set your R working directory to that folder (the script reads the CSV files by relative path).
3. Run `armenia_refugee_analysis.R`.

The script runs in this order:

1. Processes the ArmStat files and builds the region-year panel
2. Constructs treatment intensity and the shift-share instrument
3. Runs first-stage diagnostics (CRVE F-statistic and Webb bootstrap p-value)
4. Estimates OLS and 2SLS models for four outcomes: unemployment rate, log real wage, unemployed persons, employed persons
5. Computes Webb wild cluster bootstrap and Anderson-Rubin p-values
6. Runs placebo tests with treatment dates 1 to 10 years before 2023
7. Writes the main results table

**Output.** `regression_table_1_main.html` is saved to the working directory. Placebo test results and bootstrap summaries are printed to the console.

**Note on runtime and reproducibility.** The main bootstraps use 9,999 replications and the script re-estimates IV models inside loops, so a full run can take a while. Random seeds are set in the script for reproducibility.

## Limitations

The main limitation is statistical power: with 11 regional clusters, inference is sensitive to the choice of variance estimator, and the extensive fixed effects and trends may absorb much of the available variation. The design also identifies only the net effect within treated regions, not cross-regional spillovers, and it cannot rule out a lagged demand effect from the separate 2022 inflow of migrants from Russia.

## Citation

Gasparyan, D. (2026). *The Impact of the 2023 Artsakhi Refugee Influx on Regional Unemployment in Armenia.* Central European University.

## Contact

David Gasparyan, dgasparyan01@gmail.com
