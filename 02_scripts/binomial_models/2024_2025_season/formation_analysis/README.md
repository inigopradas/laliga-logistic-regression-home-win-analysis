# 2024/2025 Binomial Formation Models

This directory contains the R scripts used to analyse the association between
the starting formations selected by the home and visiting teams and the
probability of a home-team victory during the 2024/2025 LaLiga season.

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

Depending on the data-preparation stage, the dependent variable may be stored
as `win_local` or `win_local_num`.

## Formation variables

The analyses use the starting formations selected by the home and visiting
teams.

The original formation variables are:

- `formacion_local`
- `formacion_visit`

The cleaned and grouped formation variables are:

- `formacion_local_dep`
- `formacion_visit_dep`

The first `1` in each stored formation label represents the goalkeeper. For
example, `1-4-2-3-1` corresponds to the conventional 4-2-3-1 outfield
formation.

## Data preparation

Before model estimation, formation labels are cleaned by:

- Converting the original variables into character format.
- Removing literal quotation marks.
- Removing leading and trailing spaces.
- Standardising formation labels.
- Converting empty strings into missing values.
- Transforming the cleaned variables into factors.
- Removing unused factor levels.

The transformation process is implemented through reproducible R code. The
original Excel file must not be manually modified or overwritten.

## Infrequent formations

Formation categories with fewer than ten observations may be grouped into the
residual category:

`Otras`

The frequency threshold is applied to reduce the instability associated with
formation categories represented by very small numbers of matches.

Home and visiting formations are evaluated separately when identifying
infrequent categories. Consequently, a formation may be retained in one match
condition and grouped into `Otras` in the other if its frequencies differ.

## Main formation categories

Depending on their observed frequencies, the formation analyses may include:

- `1-4-2-3-1`
- `1-3-4-3`
- `1-4-1-4-1`
- `1-4-3-3`
- `1-4-4-2`
- `1-5-3-2`
- `1-5-4-1`
- `Otras`

Only formation categories that are present in the cleaned dataset can be used
as reference categories or included in the estimated models.

## Statistical method

The models are estimated using binary logistic regression with `glm()` and a
binomial family:

`family = binomial(link = "logit")`

The estimated coefficients represent changes in the log-odds of a home-team
victory. Exponentiated coefficients are interpreted as odds ratios.

## Home-formation model

The home-formation model uses only the starting formation selected by the home
team:

`win_local_num ~ formacion_local_dep`

This model evaluates whether the odds of a home-team victory differ according
to the formation selected by the home team.

The estimated associations are unadjusted for the visiting formation because
the visiting formation is not included in this specification.

## Visiting-formation model

The visiting-formation model uses only the starting formation selected by the
visiting team:

`win_local_num ~ formacion_visit_dep`

This model evaluates whether the odds of a home-team victory differ according
to the formation selected by the visiting team.

An odds ratio greater than one indicates higher odds of a home-team victory
when the visiting team uses the corresponding formation, relative to the
visiting reference formation.

The estimated associations are unadjusted for the home formation because the
home formation is not included in this specification.

## Joint formation model

The joint formation model includes the home and visiting formations
simultaneously:

`win_local_num ~ formacion_local_dep + formacion_visit_dep`

This additive specification estimates:

- The association between each home formation and home-victory odds while
  controlling for the visiting formation.
- The association between each visiting formation and home-victory odds while
  controlling for the home formation.

Unless explicitly indicated otherwise, the joint model does not include an
interaction between the home and visiting formations.

Therefore, the coefficients represent adjusted main effects rather than
specific effects for individual tactical matchups.

## Reference formation

The principal reference category is generally:

`1-4-2-3-1`

Alternative specifications may be estimated using the remaining formations as
reference categories to obtain direct contrasts between tactical systems.

Changing the reference category does not alter:

- The fitted probabilities.
- The model log-likelihood.
- The model AIC.
- The residual deviance.
- The overall model fit.
- The classification performance.

Changing the reference category only changes the parameterisation and the
specific contrasts represented by the coefficients and odds ratios.

Alternative reference specifications should therefore not be interpreted as
independent statistical models.

## Odds ratios

The formation coefficients may be transformed into odds ratios using:

`OR = exp(beta)`

The interpretation is:

- `OR > 1`: higher odds of home-team victory relative to the reference
  formation.
- `OR < 1`: lower odds of home-team victory relative to the reference
  formation.
- `OR = 1`: no estimated difference relative to the reference formation.

Confidence intervals are used to evaluate the uncertainty of each estimated
odds ratio.

An interval that includes one does not provide sufficient evidence of a
difference from the reference category at the corresponding confidence level.

## Predicted probabilities

The scripts may calculate predicted home-victory probabilities for:

- Each home formation.
- Each visiting formation.
- Every possible combination of retained home and visiting formations.

Predictions from the additive joint model are based on the sum of the
corresponding home and visiting formation effects.

A predicted value for a formation combination does not imply that the specific
combination has been frequently observed. The number of matches associated
with each pairing should also be examined.

## Observed formation combinations

The scripts may calculate the following information for each observed
home-versus-visiting formation pairing:

- Number of matches.
- Number of home-team victories.
- Observed proportion of home-team victories.
- Model-estimated probability of home-team victory.
- Difference between the observed proportion and estimated probability.

Formation combinations with very small sample sizes should be interpreted with
particular caution.

## Model outputs

Depending on the script, the formation analyses may produce:

- Formation-frequency tables.
- Logistic regression summaries.
- Coefficient estimates.
- Standard errors.
- Wald statistics.
- P-values.
- Odds ratios.
- Confidence intervals.
- Predicted home-victory probabilities.
- Observed home-victory proportions.
- Confusion matrices.
- Classification metrics.
- Forest plots.
- Formation-frequency charts.
- Formation-combination heatmaps.
- Observed-versus-estimated probability plots.

## Model evaluation

Predicted probabilities may be converted into binary classifications using a
threshold of 0.50:

- Probability equal to or greater than 0.50: predicted home victory.
- Probability below 0.50: predicted no home victory.

Classification performance may be evaluated using:

- Confusion matrices.
- Overall accuracy.
- Sensitivity for home-team victory.
- Specificity for no home-team victory.
- Precision.

Unless otherwise indicated, these metrics are calculated using the same
observations employed to estimate the models. They should therefore be
interpreted as measures of in-sample performance rather than out-of-sample
predictive performance.

## Interaction between formations

The principal joint specification does not include an interaction between the
home and visiting formations.

A model without interaction assumes that the association of a home formation
is constant across visiting formations and that the association of a visiting
formation is constant across home formations.

Estimating a specific effect for each tactical matchup would require the
following specification:

`win_local_num ~ formacion_local_dep * formacion_visit_dep`

An interaction model would require adequate observations for each formation
combination. Sparse combinations may produce unstable coefficients, very wide
confidence intervals or estimation problems.

## Missing values

Observations with missing values in the dependent variable or the formation
variables are excluded from the corresponding model.

The scripts should document:

- The number of observations before filtering.
- The number of observations excluded.
- The final model sample.
- The frequency of each retained formation.
- The number of matches associated with each formation combination.

## Reproducibility

The scripts should be executed from the root directory of the repository.

The original dataset must be read from the `01_data` directory and must not be
modified or overwritten.

Reusable functions should be loaded from the functions directory within
`02_scripts`.

Generated statistical tables should be stored in the corresponding results
directory.

Generated figures should be stored in the corresponding figures directory.

## Methodological interpretation

The results describe statistical associations between starting formations and
home-team victory.

The estimates should not automatically be interpreted as causal effects of
tactical formations.

Formation selection may depend on:

- Team quality.
- Opposition strength.
- Player availability.
- Coaching preferences.
- Match location.
- Expected match difficulty.
- Recent performance.
- Injuries and suspensions.
- Tactical decisions made before the match.

These factors may also be associated with the match result.

The grouped category `Otras` combines several different tactical systems and
should not be interpreted as a single homogeneous formation.
