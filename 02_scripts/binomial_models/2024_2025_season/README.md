# 2024/2025 Season Binomial Models

This directory contains the R scripts used to develop and evaluate the binomial
logistic regression analyses for the 2024/2025 LaLiga season.

## Dataset

The analyses use the single-season dataset stored in:

`01_data/2024_2025_season/variables_Estudio (9).xlsx`

The dataset is generally imported into R using the object name:

`d`

## Study period

The dataset contains match-level observations from the 2024/2025 LaLiga
season.

## Dependent variable

The dependent variable indicates whether the home team won the match:

- `1`: home-team victory.
- `0`: no home-team victory, including draws and away-team victories.

Depending on the data-preparation stage, the variable may be stored as
`win_local` or `win_local_num`.

## Statistical method

The models are estimated using binary logistic regression with `glm()` and a
binomial family:

`family = binomial(link = "logit")`

The general model specification is:

`win_local_num ~ explanatory variables`

The estimated coefficients represent changes in the log-odds of a home-team
victory. Exponentiated coefficients are interpreted as odds ratios.

## Directory organisation

The scripts in this directory are separated into two subdirectories according
to the type of analysis conducted.

### Block analyses

This subdirectory contains the models based on conceptual groups of explanatory
variables and the general specification obtained by combining the variables
retained from those groups.

### Formation analyses

This subdirectory contains the models evaluating the association between the
starting formations used by the home and visiting teams and the probability of
a home-team victory.

Each subdirectory contains its own documentation describing the corresponding
models, variables, procedures and outputs.

## Data preparation

Before model estimation, the relevant variables may require:

- Conversion to the appropriate numerical or categorical format.
- Standardisation of missing-value representations.
- Creation of derived variables.
- Recoding of the binary dependent variable.
- Removal of observations without the information required by a particular
  model.

All transformations should be implemented through reproducible R code. The
original Excel file must not be manually modified or overwritten.

## Model estimation

The scripts may include:

- Initial model estimation.
- Progressive model simplification.
- Examination of coefficient estimates.
- Calculation of odds ratios.
- Estimation of predicted probabilities.
- Comparison of alternative specifications.
- Evaluation of classification performance.

## Model evaluation

Depending on the corresponding analysis, model performance may be evaluated
using:

- Akaike information criterion.
- Residual deviance.
- Confusion matrices.
- Overall accuracy.
- Sensitivity for home victory.
- Specificity for no home victory.
- Precision.
- Predicted probabilities.

Unless otherwise indicated, classification metrics are calculated using the
same observations employed for model estimation. These metrics should
therefore be interpreted as measures of in-sample performance rather than
out-of-sample predictive performance.

## Missing values

The dataset may contain missing values in variables that were unavailable or
not applicable for particular matches.

Each script is responsible for documenting:

- The variables affected by missing values.
- The number of observations excluded.
- Any technical imputations performed.
- The construction of missing-value indicators.
- The final number of observations used by each model.

Technical replacement of a missing value should not be interpreted as evidence
that the original unobserved value was equal to the replacement value.

## Reproducibility

The scripts should be executed from the root directory of the repository.

Reusable functions are stored in:

`02_scripts/functions/`

Generated statistical tables should be stored in the corresponding results
directory.

Generated charts should be stored in the corresponding figures directory.

The original dataset stored in `01_data` must remain unchanged.

## Methodological interpretation

The estimated relationships represent statistical associations and should not
automatically be interpreted as causal effects.

Models containing variables recorded during the match are mainly explanatory
or descriptive because those variables and the final result may influence one
another.

Model performance calculated using the estimation sample may overstate the
performance expected with new, previously unseen matches.
