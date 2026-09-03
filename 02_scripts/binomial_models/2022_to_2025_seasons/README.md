# Multi-Season Binomial Logistic Regression Models

This directory contains the R scripts used to estimate, simplify, evaluate and
compare the binomial logistic regression models developed using LaLiga data
from the 2022/2023, 2023/2024 and 2024/2025 seasons.

The scripts are organised into two main analytical areas:

1. Models based on conceptual blocks of match variables.
2. Models focused on the tactical formations used by the home and visiting
   teams.

## Dataset

The analyses use the multi-season dataset stored in:

`01_data/2022_to_2025_seasons/LaLiga_22-25_completo_v2 (2).xlsx`

The dataset is generally imported into R using the object name:

`d1`

## Study period

The dataset contains match-level observations from the following LaLiga
seasons:

- 2022/2023
- 2023/2024
- 2024/2025

## Dependent variable

The dependent variable represents whether the home team won the match:

- `1`: home-team victory.
- `0`: no home-team victory, including draws and away-team victories.

Depending on the data-preparation stage, this variable may be stored as
`win_local` or `win_local_num`.

## Statistical method

The models are estimated using binary logistic regression with `glm()` and a
binomial family:

`family = binomial(link = "logit")`

The estimated coefficients represent changes in the log-odds of a home-team
victory. Exponentiated coefficients are interpreted as odds ratios.

# Block Models

The block-model scripts organise the explanatory variables into conceptually
related groups. Each block is first estimated using a comparatively broad set
of variables and is subsequently simplified.

## Defensive block

The defensive models evaluate variables associated with the defensive
performance of the home team, including:

- Shots conceded.
- Shots on target conceded.
- Expected goals against.
- Big chances conceded.
- Counterattacking passes conceded.
- Fouls and disciplinary events.
- Corners conceded.
- Defensive actions.
- Long passes conceded.
- Crosses conceded.
- Duels, tackles, interceptions and clearances.

## Offensive block

The offensive models evaluate variables associated with the attacking
performance of the home team, including:

- Shots.
- Shots on target.
- Expected goals.
- Big chances.
- Passing volume.
- Passes into the final third.
- Long passes.
- Crosses and completed crosses.
- Dribbles and attacking duels.
- Corners and free kicks.
- Fouls received.
- Numerical advantages.
- Defensive actions performed by the opposing team.

## Physical block

The physical models evaluate variables associated with physical intensity,
discipline and match availability, including:

- Difference in rest days.
- Fouls committed and received.
- Tackles.
- Tackles won.
- Duels won.
- Yellow and red cards.
- Cards forced.
- Numerical advantages.

## Match-control block

The match-control models evaluate variables associated with the home team's
control and management of the match, including:

- Possession.
- Passing volume.
- Counterattacking passes.
- Corners.
- Corners conceded.
- Free kicks.
- Free kicks conceded.
- Duels won.

## Contextual block

The contextual models evaluate information available before or around the
match, including:

- Season.
- Matchday.
- Previous league position of the home team.
- Previous league position of the visiting team.
- Difference in points.
- Recent form.
- Difference in rest days.

## General model

The general model combines the variables retained from the defensive,
offensive, physical, match-control and contextual blocks.

The objective is to obtain an integrated specification that balances:

- Statistical evidence.
- Model fit.
- Parsimony.
- Football interpretation.
- Classification performance.

## Model simplification

The block and general models are progressively simplified through the removal
of variables with limited statistical contribution.

The selection process considers:

- Coefficient p-values.
- Akaike information criterion.
- Residual deviance.
- Stability of the remaining coefficients.
- Substantive interpretation.
- Classification performance.

The final specification is not selected exclusively on the basis of individual
p-values. Model fit and theoretical relevance are also considered.

# Formation Models

The formation-model scripts examine the association between tactical
formations and the probability of a home-team victory.

## Formation variables

The principal formation variables are:

- `formacion_local_dep`
- `formacion_visit_dep`

Before model estimation, formation labels are cleaned by:

- Removing literal quotation marks.
- Removing unnecessary spaces.
- Standardising the stored formation labels.
- Converting empty values into missing values.
- Converting the cleaned variables into factors.

The first `1` in a formation label represents the goalkeeper. For example,
`1-4-2-3-1` corresponds to the conventional 4-2-3-1 outfield formation.

## Infrequent formations

Formation categories with fewer than ten observations are grouped into the
residual category:

`Otras`

The frequency threshold is applied to reduce the instability associated with
very sparsely represented tactical systems.

Home and visiting formations may be grouped separately because their observed
frequencies can differ.

## Home-formation models

These models use only the formation selected by the home team:

`win_local_num ~ formacion_local_dep`

The models evaluate whether home-victory odds differ according to the home
team's starting formation.

## Visiting-formation models

These models use only the formation selected by the visiting team:

`win_local_num ~ formacion_visit_dep`

A coefficient greater than zero indicates higher home-victory odds when the
visiting team uses the corresponding formation, relative to the reference
formation.

## Joint formation models

The joint models include the home and visiting formations simultaneously:

`win_local_num ~ formacion_local_dep + formacion_visit_dep`

These models estimate the association of each home formation while controlling
for the visiting formation, and the association of each visiting formation
while controlling for the home formation.

Unless explicitly indicated otherwise, the joint models do not include an
interaction between the two formation variables.

## Reference formation

The principal reference formation is generally:

`1-4-2-3-1`

Alternative reference specifications are also estimated to obtain direct
contrasts between the different formation categories.

Changing the reference category does not change:

- The fitted probabilities.
- The model log-likelihood.
- The AIC.
- The overall model fit.
- The classification performance.

Changing the reference category only changes how the individual coefficients
and odds ratios are expressed.

Therefore, the alternative reference specifications should not be interpreted
as independent statistical models.

## Formation outputs

Depending on the script, the formation analyses generate:

- Formation-frequency tables.
- Model coefficients.
- Standard errors.
- Wald statistics.
- P-values.
- Odds ratios.
- Confidence intervals.
- Predicted home-victory probabilities.
- Forest plots.
- Heatmaps of formation combinations.
- Comparisons between observed and estimated probabilities.

# Model Evaluation

The binomial models may be evaluated using a probability threshold of 0.50.

Predicted probabilities equal to or greater than 0.50 are classified as a
home-team victory. Probabilities below 0.50 are classified as no home-team
victory.

The evaluation measures may include:

- Confusion matrix.
- Overall accuracy.
- Sensitivity for home-team victory.
- Specificity for no home-team victory.
- Precision.
- Predicted probabilities.

Unless otherwise indicated, these measures are calculated using the same
observations employed for model estimation. They should therefore be
interpreted as measures of in-sample classification performance rather than
out-of-sample predictive performance.

# Missing Values

Some variables contain missing values because the corresponding information
was not collected for every match or season.

The scripts may perform specific transformations for variables such as:

- Rest days.
- Recent form.
- Previous league position.
- Tactical formations.
- Performance statistics unavailable in earlier seasons.

The treatment of missing values is documented in the corresponding script.
Technical imputation should not be interpreted as evidence that the original
value was equal to the imputed value.

# Reproducibility

The scripts should be executed from the root directory of the repository.

The original multi-season dataset should be read from the `01_data` directory
and must not be modified or overwritten.

Reusable functions are stored in:

`02_scripts/01_functions/`

Generated tables and statistical outputs should be stored in the corresponding
results directory.

Generated charts should be stored in the corresponding figures directory.

# Methodological Notes

The estimated relationships represent statistical associations and should not
be interpreted automatically as causal effects.

Match statistics recorded during the game may be both causes and consequences
of the match result. Consequently, models containing in-match variables are
primarily explanatory and descriptive.

Tactical formations may also be associated with team quality, opposition
strength, match context, player availability and coaching decisions.

In joint formation models without an interaction term, the coefficients
represent adjusted main effects. They do not represent a specific effect for
each individual home-versus-visiting formation matchup.

Estimating matchup-specific conditional effects would require an interaction
between `formacion_local_dep` and `formacion_visit_dep`.
