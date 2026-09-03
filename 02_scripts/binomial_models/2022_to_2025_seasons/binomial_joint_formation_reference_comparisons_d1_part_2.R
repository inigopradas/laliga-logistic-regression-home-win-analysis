# ==============================================================================
# Title: Multi-season joint formation reference comparisons
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script analyses the association between home and visiting tactical
# formations and the probability of a home-team victory using match-level data
# from the 2022/2023, 2023/2024 and 2024/2025 LaLiga seasons.
#
# The dependent variable is binary and distinguishes between a home-team
# victory and no home-team victory. The explanatory variables are the grouped
# formations used by the home and visiting teams.
#
# A binomial logistic regression model without an interaction term is estimated
# using the home and visiting formations as additive categorical predictors.
# The model therefore measures the association of each formation with the
# probability of a home victory while controlling for whether the formation
# corresponds to the home or visiting team.
#
# The reference categories of the home and visiting formation variables are
# changed systematically. This procedure allows the coefficients, standard
# errors and p-values associated with the different formation contrasts to be
# inspected directly without changing the fitted probabilities or the general
# structure of the underlying model.
#
# The script estimates the model for multiple combinations of reference
# formations, including 1-4-1-4-1, 1-4-3-3, 1-4-3-1-2, 1-4-4-1-1,
# 1-4-4-2, 1-4-5-1, 1-5-3-2, 1-5-4-1 and the grouped category "Otras"
# as home-team reference formations.
#
# For each home reference formation, the visiting-team reference is varied
# across the retained formation categories. The corresponding regression
# summaries provide the adjusted comparisons between tactical systems in the
# additive formation model.
#
# Dataset:
#   d1, containing match-level observations from the 2022/2023, 2023/2024 and
#   2024/2025 LaLiga seasons.
#
# Dependent variable:
#   win_local
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory, including draws and away-team victories
#
# Explanatory variables:
#   formacion_local_dep
#   formacion_visit_dep
#
# Statistical method:
#   Binary logistic regression estimated with glm() and family = binomial.
#
# Model specification:
#   Additive model without an interaction between the home and visiting
#   formations.
#
# Reference formations examined:
#   1-3-4-2-1
#   1-3-4-3
#   1-3-5-2
#   1-4-1-3-2
#   1-4-1-4-1
#   1-4-2-3-1
#   1-4-3-1-2
#   1-4-3-3
#   1-4-4-1-1
#   1-4-4-2
#   1-4-5-1
#   1-5-3-2
#   1-5-4-1
#   Otras
#
# Main outputs:
#   Logistic regression summaries for the different combinations of home and
#   visiting reference formations in the multi-season dataset.
# ==============================================================================

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-1-4-1")

#4141-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_4141_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4141_4231)

#4141-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_4141_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4141_3421)

#4141-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_4141_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4141_343)

#4141-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_4141_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4141_352)

#4141-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_4141_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4141_4132)

#4141-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_4141_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4141_4141)

#4141-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_4141_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4141_4312)

#4141-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_4141_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4141_433)

#4141-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_4141_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4141_4411)

#4141-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_4141_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4141_442)

#4141-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_4141_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4141_451)

#4141-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_4141_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4141_532)

#4141-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_4141_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4141_541)

#4141-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_4141_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                    data = d1,
                    family = binomial)

summary(m1_4141_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-3-3")

#433-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_433_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_433_4231)

#433-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_433_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_433_3421)

#433-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_433_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_433_343)

#433-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_433_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_433_352)

#433-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_433_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_433_4132)

#433-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_433_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_433_4141)

#433-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_433_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_433_4312)

#433-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_433_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_433_433)

#433-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_433_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_433_4411)

#433-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_433_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_433_442)

#433-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_433_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_433_451)

#433-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_433_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_433_532)

#433-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_433_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_433_541)

#433-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_433_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                    data = d1,
                    family = binomial)

summary(m1_433_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-3-1-2")

#4312-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_4312_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4312_4231)

#4312-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_4312_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4312_3421)

#4312-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_4312_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4312_343)

#4312-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_4312_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4312_352)

#4312-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_4312_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4312_4132)

#4312-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_4312_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4312_4141)

#4312-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_4312_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4312_4312)

#4312-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_4312_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4312_433)

#4312-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_4312_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4312_4411)

#4312-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_4312_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4312_442)

#4312-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_4312_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4312_451)

#4312-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_4312_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4312_532)

#4312-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_4312_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4312_541)

#4312-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_4312_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                    data = d1,
                    family = binomial)

summary(m1_4312_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-4-1-1")

#4411-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_4411_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4411_4231)

#4411-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_4411_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4411_3421)

#4411-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_4411_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4411_343)

#4411-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_4411_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4411_352)

#4411-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_4411_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4411_4132)

#4411-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_4411_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4411_4141)

#4411-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_4411_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4411_4312)

#4411-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_4411_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4411_433)

#4411-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_4411_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4411_4411)

#4411-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_4411_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4411_442)

#4411-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_4411_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4411_451)

#4411-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_4411_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4411_532)

#4411-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_4411_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4411_541)

#4411-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_4411_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                    data = d1,
                    family = binomial)

summary(m1_4411_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-4-2")

#442-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_442_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_442_4231)

#442-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_442_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_442_3421)

#442-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_442_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_442_343)

#442-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_442_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_442_352)

#442-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_442_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_442_4132)

#442-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_442_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_442_4141)

#442-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_442_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_442_4312)

#442-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_442_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_442_433)

#442-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_442_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_442_4411)

#442-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_442_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_442_442)

#442-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_442_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_442_451)

#442-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_442_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_442_532)

#442-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_442_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_442_541)

#442-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_442_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                    data = d1,
                    family = binomial)

summary(m1_442_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-5-1")

#451-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_451_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_451_4231)

#451-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_451_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_451_3421)

#451-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_451_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_451_343)

#451-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_451_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_451_352)

#451-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_451_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_451_4132)

#451-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_451_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_451_4141)

#451-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_451_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_451_4312)

#451-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_451_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_451_433)

#451-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_451_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_451_4411)

#451-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_451_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_451_442)

#451-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_451_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_451_451)

#451-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_451_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_451_532)

#451-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_451_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_451_541)

#451-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_451_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_451_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-5-3-2")

#532-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_532_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_532_4231)

#532-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_532_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_532_3421)

#532-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_532_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_532_343)

#532-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_532_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_532_352)

#532-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_532_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_532_4132)

#532-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_532_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_532_4141)

#532-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_532_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_532_4312)

#532-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_532_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_532_433)

#532-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_532_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_532_4411)

#532-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_532_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_532_442)

#532-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_532_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_532_451)

#532-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_532_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_532_532)

#532-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_532_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_532_541)

#532-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_532_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_532_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-5-4-1")

#541-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_541_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_541_4231)

#541-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_541_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_541_3421)

#541-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_541_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_541_343)

#541-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_541_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_541_352)

#541-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_541_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_541_4132)

#541-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_541_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_541_4141)

#541-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_541_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_541_4312)

#541-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_541_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_541_433)

#541-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_541_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_541_4411)

#541-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_541_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_541_442)

#541-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_541_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_541_451)

#541-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_541_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_541_532)

#541-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_541_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_541_541)

#541-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_541_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_541_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "Otras")

#Otras-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_Otras_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_Otras_4231)

#Otras-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_Otras_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_Otras_3421)

#Otras-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_Otras_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_Otras_343)

#Otras-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_Otras_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_Otras_352)

#Otras-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_Otras_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_Otras_4132)

#Otras-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_Otras_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_Otras_4141)

#Otras-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_Otras_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_Otras_4312)

#Otras-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_Otras_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_Otras_433)

#Otras-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_Otras_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_Otras_4411)

#Otras-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_Otras_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_Otras_442)

#Otras-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_Otras_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_Otras_451)

#Otras-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_Otras_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_Otras_532)

#Otras-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_Otras_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_Otras_541)

#Otras-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_Otras_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_Otras_Otras)
