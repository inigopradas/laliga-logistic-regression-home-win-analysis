# Multi-Season Binomial Model Results

This directory contains the documents reporting the results of the binomial
logistic regression analyses conducted using LaLiga data from the 2022/2023,
2023/2024 and 2024/2025 seasons.

The documents present the results obtained from two main areas of analysis:

1. Binomial logistic regression models organised into conceptual blocks.
2. Binomial logistic regression models focused on home and visiting
   formations.

These documents contain statistical outputs, result tables, model comparisons
and written interpretations. The reproducible R scripts used to generate these
results are stored separately in the `02_scripts` directory.

## Dataset

The reported results were obtained using the multi-season dataset:

`LaLiga_22-25_completo_v2 (2).xlsx`

The dataset is generally imported into R using the object name:

`d1`

The study period includes the following LaLiga seasons:

- 2022/2023
- 2023/2024
- 2024/2025

## Dependent variable

The binomial dependent variable indicates whether the home team won the match:

- `1`: home-team victory.
- `0`: no home-team victory, including draws and away-team victories.

Depending on the corresponding analysis script, the variable may be stored as
`win_local` or `win_local_num`.

## Statistical method

The reported models were estimated using binary logistic regression with a
logit link.

The general model specification is:

`win_local_num ~ explanatory variables`

The estimated coefficients represent changes in the log-odds of a home-team
victory.

Exponentiated coefficients are presented as odds ratios when required.

# Results from the Block Models

The block-model documents report the results obtained from models organised
according to conceptually related groups of explanatory variables.

The principal analytical blocks are:

- Offensive model.
- Defensive model.
- Physical model.
- Match-control model.
- Contextual model.
- General model.

## Offensive results

The offensive results describe the association between attacking-performance
variables and the probability of a home-team victory.

Depending on the final specification, the reported variables may include:

- Shots.
- Shots on target.
- Expected goals.
- Clear scoring opportunities.
- Passing variables.
- Passes into the final third.
- Long passes.
- Crosses.
- Dribbles.
- Corners.
- Free kicks.
- Fouls received.
- Other indicators of attacking production.

## Defensive results

The defensive results describe the association between defensive-performance
variables and the probability of a home-team victory.

Depending on the final specification, the reported variables may include:

- Shots conceded.
- Shots on target conceded.
- Expected goals against.
- Clear scoring opportunities conceded.
- Corners conceded.
- Crosses conceded.
- Defensive duels.
- Tackles.
- Interceptions.
- Clearances.
- Fouls committed.
- Disciplinary variables.

## Physical results

The physical results describe the association between physical, disciplinary
and recovery-related variables and the probability of a home-team victory.

Depending on the final specification, the reported variables may include:

- Difference in rest days.
- Fouls committed and received.
- Tackles.
- Tackles won.
- Duels won.
- Yellow cards.
- Red cards.
- Numerical advantages.

## Match-control results

The match-control results describe the association between variables related to
possession, passing, territory and general match management and the probability
of a home-team victory.

Depending on the final specification, the reported variables may include:

- Ball possession.
- Total and successful passes.
- Passing accuracy.
- Passes into the final third.
- Counterattacking passes.
- Corners.
- Free kicks.
- Duels won.

## Contextual results

The contextual results describe the association between pre-match information
and the probability of a home-team victory.

Depending on the final specification, the reported variables may include:

- Matchday.
- Previous league positions.
- Difference in league position.
- Difference in points.
- Recent form.
- Difference in recent form.
- Rest-related variables.
- Indicators for information unavailable at the start of the season.

## General-model results

The general model combines the variables retained from the individual
conceptual blocks.

The corresponding documents may report:

- The initial general specification.
- The progressive simplification process.
- Variables retained in the selected model.
- Coefficient estimates.
- Standard errors.
- Wald statistics.
- P-values.
- Odds ratios.
- Model-fit statistics.
- Predicted probabilities.
- Classification metrics.

The selected general model aims to balance statistical evidence, model fit,
parsimony and football interpretation.

# Results from the Formation Models

The formation documents report the results of the binomial analyses focused on
the starting tactical formations used by the home and visiting teams.

## Formation variables

The principal formation variables are:

- `formacion_local_dep`
- `formacion_visit_dep`

The first `1` in a stored formation label represents the goalkeeper. For
example, `1-4-2-3-1` corresponds to the conventional 4-2-3-1 outfield
formation.

Formation categories with limited frequencies may be grouped into:

`Otras`

## Home-formation results

The home-only formation results are obtained from models with the general
specification:

`win_local_num ~ formacion_local_dep`

These results describe whether the odds of a home-team victory differ according
to the starting formation selected by the home team.

The estimates are not adjusted for the visiting formation.

## Visiting-formation results

The visiting-only formation results are obtained from models with the general
specification:

`win_local_num ~ formacion_visit_dep`

These results describe whether the odds of a home-team victory differ according
to the starting formation selected by the visiting team.

The estimates are not adjusted for the home formation.

## Joint formation results

The joint formation results are obtained from models with the general
specification:

`win_local_num ~ formacion_local_dep + formacion_visit_dep`

These models estimate:

- Home-formation associations adjusted for the visiting formation.
- Visiting-formation associations adjusted for the home formation.

Unless explicitly stated otherwise, these models do not include an interaction
between the two formation variables.

The reported coefficients therefore represent adjusted main effects rather than
specific effects for individual formation matchups.

## Reference formations

The principal reference formation is generally:

`1-4-2-3-1`

Alternative reference specifications may also be reported to provide direct
comparisons between tactical formations.

Changing the reference category does not alter:

- Fitted probabilities.
- Log-likelihood.
- Akaike information criterion.
- Residual deviance.
- Overall model fit.
- Classification performance.

It only changes the contrasts represented by the individual coefficients and
odds ratios.

Alternative reference specifications should therefore be interpreted as
different parameterisations of the same underlying model rather than as
independent models.

## Formation results reported

Depending on the document, the formation results may include:

- Formation frequencies.
- Number of matches by formation.
- Number of home victories.
- Observed home-victory proportions.
- Logistic regression coefficients.
- Standard errors.
- Wald statistics.
- P-values.
- Odds ratios.
- Confidence intervals.
- Predicted home-victory probabilities.
- Formation-combination results.
- Forest plots.
- Heatmaps.
- Observed-versus-estimated comparisons.

Results based on formation categories or pairings with few observations should
be interpreted cautiously.

# Model Fit and Classification Results

The documents may report the following measures of model fit and
classification performance:

- Number of observations.
- Number of explanatory variables.
- Log-likelihood.
- Akaike information criterion.
- Residual deviance.
- Confusion matrix.
- Overall accuracy.
- Sensitivity for home-team victory.
- Specificity for no home-team victory.
- Precision.
- Balanced accuracy.

Predicted probabilities may be converted into binary classifications using a
threshold of 0.50:

- Probability equal to or greater than 0.50: predicted home victory.
- Probability below 0.50: predicted no home victory.

Unless otherwise indicated, the reported classification measures were
calculated using the same observations employed for model estimation. They
should therefore be interpreted as in-sample performance rather than as
out-of-sample predictive performance.

# Statistical Interpretation

## Coefficients

A positive logistic regression coefficient indicates higher estimated log-odds
of a home-team victory, holding the other variables in the model constant.

A negative coefficient indicates lower estimated log-odds of a home-team
victory.

## Odds ratios

The odds ratio is calculated by exponentiating the corresponding coefficient.

Its general interpretation is:

- `OR > 1`: higher odds of a home-team victory.
- `OR < 1`: lower odds of a home-team victory.
- `OR = 1`: no estimated change in the odds.

The reference category and the units of the explanatory variable must always be
considered when interpreting an odds ratio.

## Statistical evidence

The documents may classify results according to the following thresholds:

- `p < 0.05`: statistically significant at the 5% level.
- `0.05 <= p < 0.10`: marginal statistical evidence at the 10% level.
- `p >= 0.10`: no statistical evidence under the selected thresholds.

Statistical significance does not necessarily imply a large, relevant or
causal effect.

# Relationship with the R Scripts

The documents in this directory report results generated by the R scripts
stored in:

`02_scripts/binomial_models/2022_to_2025_seasons/`

The documents should not be treated as substitutes for the corresponding
scripts.

Whenever a model specification, reference category, data-cleaning decision or
estimation sample is changed, the corresponding script should be executed
again before the results document is updated.

# Missing Values

Some variables contain missing values because the relevant information was not
collected for every match or season.

The number of observations may therefore differ between models.

The result documents should identify, where relevant:

- The original sample size.
- The final model sample.
- Variables affected by missing values.
- Observations excluded from estimation.
- Technical imputations or derived indicators.
- Limitations caused by incomplete information.

# Document Status

The files in this directory contain statistical results and supporting
interpretations for the multi-season binomial analyses.

Depending on their current stage, the documents may contain:

- Complete statistical outputs.
- Working result tables.
- Preliminary interpretations.
- Material selected for the final report.
- Supplementary results intended for the appendices.

Only explicitly identified final versions should be treated as definitive
outputs.

# Reproducibility

The original dataset is stored in `01_data`.

The R scripts used to generate the reported results are stored in `02_scripts`.

Figures generated from the analyses should be stored in the corresponding
figures directory.

The original dataset must not be modified or overwritten.

All changes in the statistical analysis should first be implemented in the
corresponding R script and then reflected in the result documents.

# Methodological Notes

The reported relationships represent statistical associations and should not
automatically be interpreted as causal effects.

Variables recorded during the match may be both causes and consequences of the
developing match result.

Tactical formation selection may depend on team quality, opposition strength,
player availability, coaching decisions, injuries, suspensions and expected
match difficulty.

The category `Otras` combines different tactical systems and should not be
interpreted as a homogeneous formation.

Classification measures calculated from the estimation sample may overstate the
performance expected for new, previously unseen matches.
