# 2024/2025 Multinomial Model Results

This directory contains the documents reporting the results of the multinomial
logistic regression analyses conducted using match-level data from the
2024/2025 LaLiga season.

The documents present the results obtained from two principal areas of
analysis:

1. Multinomial logistic regression models organised into conceptual blocks.
2. Multinomial logistic regression models focused on the starting formations
   used by the home and visiting teams.

These documents contain statistical outputs, result tables, graphical
summaries and written interpretations. The reproducible R scripts used to
generate the results are stored separately in the `02_scripts` directory.

## Dataset

The reported results were obtained using the single-season dataset:

`variables_Estudio (9).xlsx`

The original dataset is stored in:

`01_data/2024_2025_season/variables_Estudio (9).xlsx`

The dataset is generally imported into R using the object name:

`d`

## Study period

The dataset contains match-level observations from the 2024/2025 LaLiga
season.

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

Therefore, the reported coefficients correspond to two outcome comparisons:

- Home victory versus draw.
- Home defeat versus draw.

## Statistical method

The reported models were estimated using multinomial logistic regression with
`nnet::multinom()`.

The general model specification is:

```r
resultado_partido_local ~ explanatory_variables
