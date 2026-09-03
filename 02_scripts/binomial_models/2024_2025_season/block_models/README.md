# 2024/2025 Binomial Block Models

This directory contains the R scripts used to estimate, simplify, evaluate and
compare the binomial logistic regression models organised into conceptual
blocks for the 2024/2025 LaLiga season.

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

## Statistical method

The models are estimated using binary logistic regression with `glm()` and a
binomial family:

`family = binomial(link = "logit")`

The estimated coefficients represent changes in the log-odds of a home-team
victory. Exponentiated coefficients are interpreted as odds ratios.

## Analytical blocks

The explanatory variables are organised into five conceptual blocks:

1. Offensive block.
2. Defensive block.
3. Physical block.
4. Match-control block.
5. Contextual block.

A general model is subsequently constructed using the variables retained from
the individual blocks.

## Offensive block

The offensive block evaluates variables associated with the attacking
performance of the home team.

Depending on the specification, this block may include variables related to:

- Total shots.
- Shots on target.
- Expected goals.
- Big scoring chances.
- Goals scored.
- Passing volume.
- Successful passes.
- Passes into the final third.
- Long passes.
- Crosses and completed crosses.
- Dribbles.
- Attacking duels.
- Corners.
- Free kicks.
- Fouls received.
- Numerical advantages.
- Other indicators of attacking production.

The offensive models examine whether greater attacking production is
associated with a higher probability of home-team victory.

## Defensive block

The defensive block evaluates variables associated with the defensive
performance of the home team and the attacking production conceded to the
visiting team.

Depending on the specification, this block may include variables related to:

- Total shots conceded.
- Shots on target conceded.
- Expected goals against.
- Big scoring chances conceded.
- Goals conceded.
- Corners conceded.
- Free kicks conceded.
- Opposition passing.
- Long passes conceded.
- Crosses conceded.
- Defensive duels.
- Tackles.
- Interceptions.
- Clearances.
- Fouls committed.
- Yellow and red cards.
- Other indicators of defensive performance.

The defensive models examine whether limiting the visiting team's attacking
production is associated with a higher probability of home-team victory.

## Physical block

The physical block evaluates variables associated with physical intensity,
discipline, recovery time and numerical conditions during the match.

Depending on the specification, this block may include:

- Rest days.
- Difference in rest days.
- Fouls committed.
- Fouls received.
- Tackles and tackles won.
- Duels and duels won.
- Yellow cards.
- Red cards.
- Cards received by the visiting team.
- Cards forced by the home team.
- Numerical advantages.
- Other indicators of physical or disciplinary performance.

The physical models examine whether physical intensity, recovery time and
disciplinary events are associated with the probability of home-team victory.

## Match-control block

The match-control block evaluates variables associated with possession,
territorial control, passing and management of the match.

Depending on the specification, this block may include:

- Ball possession.
- Total passes.
- Successful passes.
- Passing accuracy.
- Passes into the final third.
- Counterattacking passes.
- Long passes.
- Corners.
- Corners conceded.
- Free kicks.
- Free kicks conceded.
- Duels won.
- Other indicators of territorial or technical control.

The match-control models examine whether greater control of possession and
territory is associated with a higher probability of home-team victory.

## Contextual block

The contextual block evaluates variables describing the competitive situation
before the match.

Depending on the specification, this block may include:

- Matchday.
- Previous league position of the home team.
- Previous league position of the visiting team.
- Difference in league position.
- Points accumulated before the match.
- Difference in points.
- Recent form of the home team.
- Recent form of the visiting team.
- Difference in recent form.
- Rest days.
- Difference in rest days.
- Indicators for unavailable previous-match information.
- Other pre-match contextual variables.

The contextual models examine whether differences in competitive position,
recent performance and rest are associated with the probability of home-team
victory.

## General model

The general model combines the variables retained from the offensive,
defensive, physical, match-control and contextual blocks.

Its purpose is to obtain an integrated specification that explains home-team
victory while balancing:

- Statistical evidence.
- Model fit.
- Parsimony.
- Stability of the estimated coefficients.
- Football interpretation.
- Classification performance.

Variables appearing in more than one conceptual block should only be included
once in the general model.

## Model simplification

Each initial block model may be simplified progressively by removing variables
with limited statistical contribution.

The simplification process considers:

- Individual coefficient p-values.
- Akaike information criterion.
- Residual deviance.
- Changes in the remaining coefficients.
- Theoretical and football relevance.
- Number of observations retained.
- Classification performance.
- Potential redundancy among explanatory variables.

The final model should not be selected exclusively through individual
coefficient p-values. Statistical fit, interpretability and the analytical
purpose of the model should also be considered.

## Model comparison

Alternative specifications may be compared using:

- Number of observations.
- Number of estimated parameters.
- Log-likelihood.
- Akaike information criterion.
- Residual deviance.
- Likelihood-ratio tests for nested models.
- Classification metrics.

Models should be compared using the same estimation sample whenever the
comparison depends on likelihood-based measures.

## Predicted probabilities

The fitted models generate an estimated probability of home-team victory for
each observation.

A classification threshold of 0.50 may be used:

- Estimated probability equal to or greater than 0.50: predicted home victory.
- Estimated probability below 0.50: predicted no home victory.

The threshold should be stated explicitly in the corresponding analysis
script.

## Model evaluation

The scripts may evaluate classification performance using:

- Confusion matrices.
- Overall accuracy.
- Sensitivity for home-team victory.
- Specificity for no home-team victory.
- Precision.
- Predicted probabilities.

Unless otherwise indicated, these measures are calculated using the same
observations employed for model estimation. They should therefore be
interpreted as measures of in-sample descriptive performance rather than
out-of-sample predictive performance.

## Missing values

The number of observations used by each model may differ because `glm()`
excludes observations containing missing values in any variable included in
the corresponding specification.

The scripts should document:

- The original number of observations.
- The variables containing missing values.
- Any recoding or technical imputation performed.
- The number of observations used in each model.
- Whether compared models were estimated using the same observations.

Technical imputations should be justified and should not be interpreted as
observed values.

## Main outputs

Depending on the script, the block analyses may generate:

- Model summaries.
- Coefficient tables.
- Standard errors.
- Wald statistics.
- P-values.
- Odds ratios.
- Confidence intervals.
- AIC and residual-deviance comparisons.
- Predicted probabilities.
- Confusion matrices.
- Accuracy, sensitivity, specificity and precision.
- Coefficient plots.
- Comparative model-performance figures.

## Reproducibility

The scripts should be executed from the root directory of the repository.

The original dataset must be read from the `01_data` directory and must not be
modified or overwritten.

Reusable functions should be loaded from the corresponding functions directory
inside `02_scripts`.

Generated statistical tables should be stored in the corresponding results
directory.

Generated figures should be stored in the corresponding figures directory.

## Methodological interpretation

The estimated coefficients represent conditional statistical associations
between the explanatory variables and the probability of home-team victory.

The estimates should not automatically be interpreted as causal effects.

Variables measured during the match, such as shots, expected goals, possession
or disciplinary events, may be both causes and consequences of the evolving
match result. Models containing these variables are therefore primarily
explanatory and descriptive.

Classification metrics calculated with the estimation sample may overstate the
performance expected for future, previously unseen matches.
