# Multi-Season Multinomial Logistic Regression Models

This directory contains the R scripts used to estimate, simplify, evaluate and
compare the multinomial logistic regression models developed using LaLiga data
from the 2022/2023, 2023/2024 and 2024/2025 seasons.

The directory combines two principal areas of analysis:

1. Multinomial models based on conceptual blocks of match variables.
2. Multinomial models focused on the starting formations used by the home and
   visiting teams.

## Dataset

The analyses use the multi-season dataset stored in the corresponding
multi-season directory within `01_data`.

The original Excel file is:

`LaLiga_22-25_completo_v2 (2).xlsx`

The dataset is generally imported into R using the object name:

`d1`

## Study period

The dataset contains match-level observations from the following LaLiga
seasons:

- 2022/2023
- 2023/2024
- 2024/2025

Some variables contain missing values because the corresponding information was
not collected for every match or season.

## Dependent variable

The dependent variable is:

`resultado_partido_local`

This variable describes the final match result from the perspective of the home
team and contains three categories:

- `Victoria`: home-team victory.
- `Empate`: draw.
- `Derrota`: home-team defeat.

The reference outcome is generally:

`Empate`

Therefore, the estimated model equations compare:

- Home victory versus draw.
- Home defeat versus draw.

## Statistical method

The models are estimated using multinomial logistic regression with
`nnet::multinom()`.

A general specification takes the following form:

```r
modelo <- nnet::multinom(
  resultado_partido_local ~ explanatory_variables,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)
