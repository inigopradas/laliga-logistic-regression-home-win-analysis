# 2024/2025 Season Multinomial Logistic Regression Models

This directory contains the R scripts used to estimate, simplify, evaluate and
compare the multinomial logistic regression models developed for the 2024/2025
LaLiga season.

The directory combines two principal areas of analysis:

1. Multinomial models based on conceptual blocks of match variables.
2. Multinomial models focused on the starting formations used by the home and
   visiting teams.

## Dataset

The analyses use the single-season dataset stored in:

`01_data/2024_2025_season/variables_Estudio (9).xlsx`

The dataset is generally imported into R using the object name:

`d`

## Study period

The dataset contains match-level observations from the 2024/2025 LaLiga
season.

## Dependent variable

The dependent variable is:

`resultado_partido_local`

This variable represents the final match result from the perspective of the
home team and contains three categories:

- `Victoria`: home-team victory.
- `Empate`: draw.
- `Derrota`: home-team defeat.

The reference outcome is generally:

`Empate`

Therefore, the model estimates two comparisons:

- Home victory versus draw.
- Home defeat versus draw.

## Statistical method

The models are estimated using multinomial logistic regression with
`nnet::multinom()`.

A general model specification is:

```r
modelo <- nnet::multinom(
  resultado_partido_local ~ explanatory_variables,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)
``
