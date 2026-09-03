# ==============================================================================
# Title: Multi-season joint formation reference comparisons, Part 1
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script estimates a series of binary logistic regression models to
# analyse the association between home and visiting tactical formations and
# the probability of a home-team victory across the 2022/2023, 2023/2024 and
# 2024/2025 LaLiga seasons.
#
# The dependent variable distinguishes between a home-team victory and no
# home-team victory. The explanatory variables are the tactical formations
# used by the home and visiting teams.
#
# Each model includes the home and visiting formations as additive categorical
# predictors. No interaction term is included. Therefore, the estimated
# coefficients represent the association of each formation with home-victory
# odds while controlling for the formation used by the opposing team.
#
# The reference categories of the home and visiting formation variables are
# changed systematically. This reparameterisation allows direct inspection of
# the coefficients, standard errors and p-values associated with different
# formation contrasts while preserving the fitted probabilities and overall
# model fit.
#
# This part covers the combinations in which the home-team reference formation
# is 1-4-2-3-1, 1-3-4-2-1, 1-3-4-3, 1-3-5-2 or 1-4-1-3-2. For each home-team
# reference, the visiting-team reference is varied across all retained tactical
# formations and the grouped category "Otras".
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
# Home-team reference formations covered:
#   1-4-2-3-1
#   1-3-4-2-1
#   1-3-4-3
#   1-3-5-2
#   1-4-1-3-2
#
# Visiting-team reference formations examined:
#   1-4-2-3-1
#   1-3-4-2-1
#   1-3-4-3
#   1-3-5-2
#   1-4-1-3-2
#   1-4-1-4-1
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
#   Binary logistic regression summaries for the different combinations of
#   home and visiting reference 

library(readxl)   # para leer Excel
library(dplyr)    # para manipular datos

getwd()
d1 <- read_excel("LaLiga_22-25_completo_v2 (2).xlsx")
d <- read_excel("variables_Estudio (9).xlsx")
#de aqui hay muchos valores que salen como NA porque no se recopilaron (de d1)
#voy a calcular el contraste de las formaciones

str(d1)
head(d1)
d1$win_local = factor(d1$win_local)
d1$temporada = factor(d1$temporada)
d1$formacion_local = factor(d1$formacion_local)
d1$formacion_visit = factor(d1$formacion_visit)

source("logit_funciones.R")


# Quitar comillas internas y espacios
d1$formacion_local = gsub('"', '', trimws(as.character(d1$formacion_local)))
d1$formacion_visit = gsub('"', '', trimws(as.character(d1$formacion_visit)))

# Convertir a factor
d1$formacion_local = factor(d1$formacion_local)
d1$formacion_visit = factor(d1$formacion_visit)

# Comprobar niveles
levels(d1$formacion_local)
levels(d1$formacion_visit)

#Modelo sin interaccion entre las formaciones


table(d1$formacion_local)
table(d1$formacion_visit)


# =========================================================
# FRECUENCIAS
# =========================================================
freq1_local = table(d1$formacion_local)
freq1_visit = table(d1$formacion_visit)

sort(freq1_local)
sort(freq1_visit)

# =========================================================
# AGRUPAR FORMACIONES RARAS EN "Otras"
# =========================================================
umbral = 10
# Crear copias en character para recodificar
d1$formacion_local_dep = as.character(d1$formacion_local)
d1$formacion_visit_dep = as.character(d1$formacion_visit)

# Reemplazar niveles raros por "Otras"
d1$formacion_local_dep[d1$formacion_local_dep %in% names(freq1_local[freq1_local < umbral])] = "Otras"
d1$formacion_visit_dep[d1$formacion_visit_dep %in% names(freq1_visit[freq1_visit < umbral])] = "Otras"

# Volver a factor
d1$formacion_local_dep = factor(d1$formacion_local_dep)
d1$formacion_visit_dep = factor(d1$formacion_visit_dep)

# Ver frecuencias nuevas
table(d1$formacion_local_dep)
table(d1$formacion_visit_dep)

# =========================================================
# REFERENCIAS de variables depuradas
# =========================================================
d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-2-3-1")

#4231-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_4231_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                        data = d1,
                        family = binomial)

summary(m1_4231_4231)
#############################################################################
# =========================================================
# MATRIZ DE CONFUSION Y ACCURACY EN TODA LA MUESTRA
# =========================================================

# Probabilidades predichas
pred1_prob = predict(m1_4231_4231, newdata = d1, type = "response")

# Clasificación binaria con umbral 0.5
pred1_y = ifelse(pred1_prob >= 0.5, 1, 0)

# Matriz de confusión
mc1 = table(Real = d1$win_local, Predicho = pred1_y)
mc1
#############################################################################
#4231-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_4231_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_3421)

#4231-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_4231_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_343)

#4231-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_4231_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_352)

#4231-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_4231_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_4132)

#4231-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_4231_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_4141)

#4231-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_4231_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_4312)

#4231-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_4231_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_433)

#4231-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_4231_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_4411)

#4231-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_4231_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_442)

#4231-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_4231_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_451)

#4231-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_4231_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_532)

#4231-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_4231_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_541)

#4231-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_4231_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4231_Otras)


###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-3-4-2-1")

#3421-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_3421_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_3421_4231)

#3421-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_3421_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_3421_3421)

#3421-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_3421_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_3421_343)

#3421-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_3421_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_3421_352)

#3421-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_3421_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_3421_4132)

#3421-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_3421_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_3421_4141)

#3421-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_3421_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_3421_4312)

#3421-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_3421_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_3421_433)

#3421-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_3421_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_3421_4411)

#3421-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_3421_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_3421_442)

#3421-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_3421_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_3421_451)

#3421-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_3421_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_3421_532)

#3421-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_3421_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_3421_541)

#3421-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_3421_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                    data = d1,
                    family = binomial)

summary(m1_3421_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-3-4-3")

#343-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_343_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_343_4231)

#343-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_343_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_343_3421)

#343-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_343_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_343_343)

#343-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_343_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_343_352)

#343-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_343_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_343_4132)

#343-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_343_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_343_4141)

#343-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_343_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_343_4312)

#343-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_343_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_343_433)

#343-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_343_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_343_4411)

#343-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_343_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_343_442)

#343-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_343_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_343_451)

#343-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_343_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_343_532)

#343-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_343_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_343_541)

#343-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_343_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                    data = d1,
                    family = binomial)

summary(m1_343_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-3-5-2")

#352-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_352_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_352_4231)

#352-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_352_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_352_3421)

#352-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_352_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_352_343)

#352-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_352_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_352_352)

#352-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_352_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_352_4132)

#352-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_352_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_352_4141)

#352-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_352_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_352_4312)

#352-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_352_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_352_433)

#352-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_352_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_352_4411)

#352-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_352_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_352_442)

#352-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_352_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_352_451)

#352-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_352_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_352_532)

#352-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_352_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_352_541)

#352-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_352_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_352_Otras)

###############################################################################
###############################################################################

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-1-3-2")

#4132-4231

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")

m1_4132_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4132_4231)

#4132-3421

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")

m1_4132_3421 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4132_3421)

#4132-343

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")

m1_4132_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_4132_343)

#4132-352

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")

m1_4132_352 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_4132_352)

#4132-4132

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")

m1_4132_4132 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4132_4132)

#4132-4141

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")

m1_4132_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4132_4141)

#4132-4312

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")

m1_4132_4312 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4132_4312)

#4132-433

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")

m1_4132_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_4132_433)

#4132-4411

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")

m1_4132_4411 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d1,
                  family = binomial)

summary(m1_4132_4411)

#4132-442

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")

m1_4132_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_4132_442)

#4132-451

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")

m1_4132_451 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_4132_451)

#4132-532

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")

m1_4132_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_4132_532)

#4132-541

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")

m1_4132_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d1,
                 family = binomial)

summary(m1_4132_541)

#4132-Otras

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")

m1_4132_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d1,
                   family = binomial)

summary(m1_4132_Otras)



