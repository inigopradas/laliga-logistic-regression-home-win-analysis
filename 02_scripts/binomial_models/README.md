# Binomial Logistic Regression Models

This directory contains the R scripts used to estimate and evaluate the
binomial logistic regression models.

## Dependent variable

The dependent variable is home-team victory:

- `1`: home-team victory.
- `0`: no home-team victory, including draws and away-team victories.

Depending on the script and data-preparation stage, the dependent variable may
be stored as `win_local` or `win_local_num`.

## Directory structure

### 2024_2025_season

This directory contains the binomial logistic regression analyses conducted
using the 2024/2025 LaLiga season dataset.

The dataset is generally imported into R using the object name `d`.

### 2022_2025_seasons

This directory contains the binomial logistic regression analyses conducted
using the combined dataset for the 2022/2023, 2023/2024 and 2024/2025 LaLiga
seasons.

The dataset is generally imported into R using the object name `d1`.

## Model groups

The binomial analysis includes the following analytical blocks:

- Offensive model.
- Defensive model.
- Physical model.
- Match-control model.
- Contextual model.
- General model.
- Home-formation model.
- Visiting-formation model.
- Joint home and visiting formation model.

## Statistical method

The models are estimated using binary logistic regression with `glm()` and
`family = binomial`.

The general specification is:

`win_local ~ explanatory variables`

or:

`win_local_num ~ explanatory variables`

## Model selection

The block and general models are progressively simplified by examining:

- Coefficient significance.
- Akaike information criterion.
- Residual deviance.
- Substantive interpretation.
- Classification performance.

## Model evaluation

Depending on the script, model performance is evaluated using:

- Confusion matrices.
- Overall accuracy.
- Sensitivity for home victory.
- Specificity for no home victory.
- Precision.
- Predicted probabilities.

Unless otherwise indicated, these metrics are calculated using the same
observations employed for model estimation. They should therefore be
interpreted as in-sample descriptive performance.

## Formation models

The formation analyses use grouped versions of the tactical formation
variables:

- `formacion_local_dep`
- `formacion_visit_dep`

Formation categories with limited frequency may be grouped into the residual
category `Otras`.

The main formation models are estimated without an interaction between the
home and visiting formations unless the corresponding script explicitly
indicates otherwise.

## Reproducibility

The scripts should read the original data from the `01_data` directory.

Reusable logistic regression functions are stored in:

`02_scripts/01_functions/logit_funciones.R`

The original Excel files must not be modified or overwritten.
