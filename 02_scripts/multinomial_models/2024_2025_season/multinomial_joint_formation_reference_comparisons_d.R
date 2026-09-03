# ==============================================================================
# Title: Single-season multinomial joint formation reference comparisons
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script analyses the association between the tactical formations used by
# the home and visiting teams and the final match outcome during the 2024/2025
# LaLiga season.
#
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat. The draw is established as the reference outcome, so the
# estimated coefficients compare home victory versus draw and home defeat
# versus draw.
#
# Before model estimation, the home and visiting formation variables are
# cleaned by removing unnecessary spaces and literal quotation marks. Both
# variables are then converted into factors. Infrequent tactical systems are
# represented through the grouped category "Otras".
#
# The models include the home formation and visiting formation simultaneously
# as additive categorical explanatory variables. No interaction term is
# included. Therefore, the estimated effect of each home formation is adjusted
# for the formation used by the visiting team, and the estimated effect of each
# visiting formation is adjusted for the formation used by the home team.
#
# The reference categories of both formation variables are changed
# systematically. This reparameterisation allows direct comparisons among the
# retained tactical systems without modifying the fitted probabilities,
# log-likelihood or overall fit of the underlying additive model.
#
# For every combination of home and visiting reference formations, the script
# estimates a multinomial logistic regression model and extracts coefficients,
# standard errors, bilateral Wald p-values and odds ratios. Results with
# statistical evidence at the 5% and 10% levels are identified using previously
# defined extraction functions.
#
# The reference formations examined include 1-4-2-3-1, 1-3-4-3, 1-4-1-4-1,
# 1-4-3-3, 1-4-4-2, 1-5-3-2, 1-5-4-1 and the grouped category "Otras".
#
# The principal model uses 1-4-2-3-1 as the reference formation for both teams.
# This model is also used to obtain predicted probabilities, predicted outcome
# classes and classification-performance measures.
#
# Model performance is evaluated using a confusion matrix, overall accuracy,
# class-specific sensitivity, class-specific precision and balanced accuracy.
# The evaluation is conducted using the same observations employed to estimate
# the model.
#
# Dataset:
#   d, containing match-level observations from the 2024/2025 LaLiga season.
#
# Dependent variable:
#   resultado_partido_local
#
# Outcome categories:
#   Victoria, Empate and Derrota
#
# Reference outcome:
#   Empate
#
# Explanatory variables:
#   formacion_local_dep
#   formacion_visit_dep
#
# Principal formation references:
#   Home team: 1-4-2-3-1
#   Visiting team: 1-4-2-3-1
#
# Statistical method:
#   Multinomial logistic regression estimated with nnet::multinom().
#
# Model specification:
#   Additive model without an interaction between the home and visiting
#   formations.
#
# Principal model:
#   m_d_4231_4231
#
# Main home-reference groups:
#   1-4-2-3-1
#   1-3-4-3
#   1-4-1-4-1
#   1-4-3-3
#   1-4-4-2
#   1-5-4-1
#   Otras
#
# Visiting reference categories:
#   1-4-2-3-1
#   1-3-4-3
#   1-4-1-4-1
#   1-4-3-3
#   1-4-4-2
#   1-5-3-2
#   1-5-4-1
#   Otras
#
# Required functions:
#   extraer_resultados_formaciones_multinom()
#   extraer_significativos_formaciones()
#
# Main outputs:
#   Multinomial model summaries, coefficient tables, odds ratios, p-values,
#   significant and marginal formation comparisons, predicted probabilities,
#   predicted outcome classes, confusion matrices, overall accuracy, balanced
#   accuracy, class-specific sensitivity and class-specific precision.
#
# Important methodological notes:
#   Changing the reference categories does not create substantively different
#   models. It only changes the parameterisation used to express the same
#   additive relationships among the formations and match outcomes.
#
#   The home and visiting formation coefficients represent adjusted main
#   effects, not specific formation-pair interactions. A direct contrast for a
#   particular tactical matchup would require an interaction term between the
#   home and visiting formation variables.
#
#   Classification metrics are calculated in sample and should therefore be
#   interpreted as descriptive measures of model fit rather than as evidence
#   of out-of-sample predictive performance.
# ==============================================================================

d$formacion_local_dep <- trimws(
  as.character(d$formacion_local)
)

d$formacion_visit_dep <- trimws(
  as.character(d$formacion_visit)
)

# Eliminar posibles comillas literales
d$formacion_local_dep <- gsub(
  '"',
  "",
  d$formacion_local_dep,
  fixed = TRUE
)

d$formacion_visit_dep <- gsub(
  '"',
  "",
  d$formacion_visit_dep,
  fixed = TRUE
)

# Convertir en factores
d$formacion_local_dep <- factor(
  d$formacion_local_dep
)

d$formacion_visit_dep <- factor(
  d$formacion_visit_dep
)

# =========================================================
# 0. PREPARAR LAS VARIABLES
# =========================================================

# Limpiar la variable dependiente
d$resultado_partido_local <- trimws(
  as.character(d$resultado_partido_local)
)

# Convertir en factor
d$resultado_partido_local <- factor(
  d$resultado_partido_local
)

# Establecer Empate como referencia
d$resultado_partido_local <- relevel(
  d$resultado_partido_local,
  ref = "Empate"
)


# Limpiar la formación local
d$formacion_local_dep <- trimws(
  as.character(d$formacion_local_dep)
)

# Eliminar posibles comillas literales
d$formacion_local_dep <- gsub(
  '"',
  "",
  d$formacion_local_dep,
  fixed = TRUE
)

# Convertir en factor
d$formacion_local_dep <- factor(
  d$formacion_local_dep
)


# Limpiar la formación visitante
d$formacion_visit_dep <- trimws(
  as.character(d$formacion_visit_dep)
)

# Eliminar posibles comillas literales
d$formacion_visit_dep <- gsub(
  '"',
  "",
  d$formacion_visit_dep,
  fixed = TRUE
)

# Convertir en factor
d$formacion_visit_dep <- factor(
  d$formacion_visit_dep
)


# Comprobar niveles después de la limpieza
levels(d$resultado_partido_local)
levels(d$formacion_local_dep)
levels(d$formacion_visit_dep)


# Comprobar frecuencias
table(
  d$formacion_local_dep,
  useNA = "always"
)

table(
  d$formacion_visit_dep,
  useNA = "always"
)


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL: 4231
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-2-3-1"
)


# =========================================================
# 1. 4231 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_d_4231_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_4231)


# Resultados completos por nivel
resultados_d_4231_4231 <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_4231
  )

resultados_d_4231_4231


# Significativos al 5 %
significativos_d_4231_4231 <-
  extraer_significativos_formaciones(
    resultados_d_4231_4231,
    nivel = 0.05
  )

significativos_d_4231_4231


# Significativos o marginales al 10 %
significativos_10_d_4231_4231 <-
  extraer_significativos_formaciones(
    resultados_d_4231_4231,
    nivel = 0.10
  )

significativos_10_d_4231_4231


# =========================================================
# 2. 4231 LOCAL CONTRA 343 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_d_4231_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_343)


# Resultados completos por nivel
resultados_d_4231_343 <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_343
  )

resultados_d_4231_343


# Significativos al 5 %
significativos_d_4231_343 <-
  extraer_significativos_formaciones(
    resultados_d_4231_343,
    nivel = 0.05
  )

significativos_d_4231_343


# Significativos o marginales al 10 %
significativos_10_d_4231_343 <-
  extraer_significativos_formaciones(
    resultados_d_4231_343,
    nivel = 0.10
  )

significativos_10_d_4231_343


# =========================================================
# 3. 4231 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_d_4231_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_4141)


# Resultados completos por nivel
resultados_d_4231_4141 <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_4141
  )

resultados_d_4231_4141


# Significativos al 5 %
significativos_d_4231_4141 <-
  extraer_significativos_formaciones(
    resultados_d_4231_4141,
    nivel = 0.05
  )

significativos_d_4231_4141


# Significativos o marginales al 10 %
significativos_10_d_4231_4141 <-
  extraer_significativos_formaciones(
    resultados_d_4231_4141,
    nivel = 0.10
  )

significativos_10_d_4231_4141


# =========================================================
# 4. 4231 LOCAL CONTRA 433 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_d_4231_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_433)


# Resultados completos por nivel
resultados_d_4231_433 <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_433
  )

resultados_d_4231_433


# Significativos al 5 %
significativos_d_4231_433 <-
  extraer_significativos_formaciones(
    resultados_d_4231_433,
    nivel = 0.05
  )

significativos_d_4231_433


# Significativos o marginales al 10 %
significativos_10_d_4231_433 <-
  extraer_significativos_formaciones(
    resultados_d_4231_433,
    nivel = 0.10
  )

significativos_10_d_4231_433


# =========================================================
# 5. 4231 LOCAL CONTRA 442 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_d_4231_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_442)


# Resultados completos por nivel
resultados_d_4231_442 <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_442
  )

resultados_d_4231_442


# Significativos al 5 %
significativos_d_4231_442 <-
  extraer_significativos_formaciones(
    resultados_d_4231_442,
    nivel = 0.05
  )

significativos_d_4231_442


# Significativos o marginales al 10 %
significativos_10_d_4231_442 <-
  extraer_significativos_formaciones(
    resultados_d_4231_442,
    nivel = 0.10
  )

significativos_10_d_4231_442


# =========================================================
# 6. 4231 LOCAL CONTRA 532 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_d_4231_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_532)


# Resultados completos por nivel
resultados_d_4231_532 <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_532
  )

resultados_d_4231_532


# Significativos al 5 %
significativos_d_4231_532 <-
  extraer_significativos_formaciones(
    resultados_d_4231_532,
    nivel = 0.05
  )

significativos_d_4231_532


# Significativos o marginales al 10 %
significativos_10_d_4231_532 <-
  extraer_significativos_formaciones(
    resultados_d_4231_532,
    nivel = 0.10
  )

significativos_10_d_4231_532


# =========================================================
# 7. 4231 LOCAL CONTRA 541 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_d_4231_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_541)


# Resultados completos por nivel
resultados_d_4231_541 <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_541
  )

resultados_d_4231_541


# Significativos al 5 %
significativos_d_4231_541 <-
  extraer_significativos_formaciones(
    resultados_d_4231_541,
    nivel = 0.05
  )

significativos_d_4231_541


# Significativos o marginales al 10 %
significativos_10_d_4231_541 <-
  extraer_significativos_formaciones(
    resultados_d_4231_541,
    nivel = 0.10
  )

significativos_10_d_4231_541


# =========================================================
# 8. 4231 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_d_4231_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4231_Otras)


# Resultados completos por nivel
resultados_d_4231_Otras <-
  extraer_resultados_formaciones_multinom(
    m_d_4231_Otras
  )

resultados_d_4231_Otras


# Significativos al 5 %
significativos_d_4231_Otras <-
  extraer_significativos_formaciones(
    resultados_d_4231_Otras,
    nivel = 0.05
  )

significativos_d_4231_Otras


# Significativos o marginales al 10 %
significativos_10_d_4231_Otras <-
  extraer_significativos_formaciones(
    resultados_d_4231_Otras,
    nivel = 0.10
  )

significativos_10_d_4231_Otras

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-3-4-3
# BASE DE DATOS: d
# TODAS LAS REFERENCIAS DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL: 343
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-3-4-3"
)


# =========================================================
# 1. 343 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_d_343_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_4231)


# Resultados completos por nivel
resultados_d_343_4231 <-
  extraer_resultados_formaciones_multinom(
    m_d_343_4231
  )

resultados_d_343_4231


# Significativos al 5 %
significativos_d_343_4231 <-
  extraer_significativos_formaciones(
    resultados_d_343_4231,
    nivel = 0.05
  )

significativos_d_343_4231


# Significativos o marginales al 10 %
significativos_10_d_343_4231 <-
  extraer_significativos_formaciones(
    resultados_d_343_4231,
    nivel = 0.10
  )

significativos_10_d_343_4231


# =========================================================
# 2. 343 LOCAL CONTRA 343 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_d_343_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_343)


# Resultados completos por nivel
resultados_d_343_343 <-
  extraer_resultados_formaciones_multinom(
    m_d_343_343
  )

resultados_d_343_343


# Significativos al 5 %
significativos_d_343_343 <-
  extraer_significativos_formaciones(
    resultados_d_343_343,
    nivel = 0.05
  )

significativos_d_343_343


# Significativos o marginales al 10 %
significativos_10_d_343_343 <-
  extraer_significativos_formaciones(
    resultados_d_343_343,
    nivel = 0.10
  )

significativos_10_d_343_343


# =========================================================
# 3. 343 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_d_343_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_4141)


# Resultados completos por nivel
resultados_d_343_4141 <-
  extraer_resultados_formaciones_multinom(
    m_d_343_4141
  )

resultados_d_343_4141


# Significativos al 5 %
significativos_d_343_4141 <-
  extraer_significativos_formaciones(
    resultados_d_343_4141,
    nivel = 0.05
  )

significativos_d_343_4141


# Significativos o marginales al 10 %
significativos_10_d_343_4141 <-
  extraer_significativos_formaciones(
    resultados_d_343_4141,
    nivel = 0.10
  )

significativos_10_d_343_4141


# =========================================================
# 4. 343 LOCAL CONTRA 433 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_d_343_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_433)


# Resultados completos por nivel
resultados_d_343_433 <-
  extraer_resultados_formaciones_multinom(
    m_d_343_433
  )

resultados_d_343_433


# Significativos al 5 %
significativos_d_343_433 <-
  extraer_significativos_formaciones(
    resultados_d_343_433,
    nivel = 0.05
  )

significativos_d_343_433


# Significativos o marginales al 10 %
significativos_10_d_343_433 <-
  extraer_significativos_formaciones(
    resultados_d_343_433,
    nivel = 0.10
  )

significativos_10_d_343_433


# =========================================================
# 5. 343 LOCAL CONTRA 442 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_d_343_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_442)


# Resultados completos por nivel
resultados_d_343_442 <-
  extraer_resultados_formaciones_multinom(
    m_d_343_442
  )

resultados_d_343_442


# Significativos al 5 %
significativos_d_343_442 <-
  extraer_significativos_formaciones(
    resultados_d_343_442,
    nivel = 0.05
  )

significativos_d_343_442


# Significativos o marginales al 10 %
significativos_10_d_343_442 <-
  extraer_significativos_formaciones(
    resultados_d_343_442,
    nivel = 0.10
  )

significativos_10_d_343_442


# =========================================================
# 6. 343 LOCAL CONTRA 532 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_d_343_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_532)


# Resultados completos por nivel
resultados_d_343_532 <-
  extraer_resultados_formaciones_multinom(
    m_d_343_532
  )

resultados_d_343_532


# Significativos al 5 %
significativos_d_343_532 <-
  extraer_significativos_formaciones(
    resultados_d_343_532,
    nivel = 0.05
  )

significativos_d_343_532


# Significativos o marginales al 10 %
significativos_10_d_343_532 <-
  extraer_significativos_formaciones(
    resultados_d_343_532,
    nivel = 0.10
  )

significativos_10_d_343_532


# =========================================================
# 7. 343 LOCAL CONTRA 541 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_d_343_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_541)


# Resultados completos por nivel
resultados_d_343_541 <-
  extraer_resultados_formaciones_multinom(
    m_d_343_541
  )

resultados_d_343_541


# Significativos al 5 %
significativos_d_343_541 <-
  extraer_significativos_formaciones(
    resultados_d_343_541,
    nivel = 0.05
  )

significativos_d_343_541


# Significativos o marginales al 10 %
significativos_10_d_343_541 <-
  extraer_significativos_formaciones(
    resultados_d_343_541,
    nivel = 0.10
  )

significativos_10_d_343_541


# =========================================================
# 8. 343 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_d_343_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_343_Otras)


# Resultados completos por nivel
resultados_d_343_Otras <-
  extraer_resultados_formaciones_multinom(
    m_d_343_Otras
  )

resultados_d_343_Otras


# Significativos al 5 %
significativos_d_343_Otras <-
  extraer_significativos_formaciones(
    resultados_d_343_Otras,
    nivel = 0.05
  )

significativos_d_343_Otras


# Significativos o marginales al 10 %
significativos_10_d_343_Otras <-
  extraer_significativos_formaciones(
    resultados_d_343_Otras,
    nivel = 0.10
  )

significativos_10_d_343_Otras

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-1-4-1
# BASE DE DATOS: d
# TODAS LAS REFERENCIAS DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL: 4141
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-1-4-1"
)


# =========================================================
# 1. 4141 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_d_4141_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_4231)


# Resultados completos por nivel
resultados_d_4141_4231 <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_4231
  )

resultados_d_4141_4231


# Significativos al 5 %
significativos_d_4141_4231 <-
  extraer_significativos_formaciones(
    resultados_d_4141_4231,
    nivel = 0.05
  )

significativos_d_4141_4231


# Significativos o marginales al 10 %
significativos_10_d_4141_4231 <-
  extraer_significativos_formaciones(
    resultados_d_4141_4231,
    nivel = 0.10
  )

significativos_10_d_4141_4231


# =========================================================
# 2. 4141 LOCAL CONTRA 343 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_d_4141_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_343)


# Resultados completos por nivel
resultados_d_4141_343 <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_343
  )

resultados_d_4141_343


# Significativos al 5 %
significativos_d_4141_343 <-
  extraer_significativos_formaciones(
    resultados_d_4141_343,
    nivel = 0.05
  )

significativos_d_4141_343


# Significativos o marginales al 10 %
significativos_10_d_4141_343 <-
  extraer_significativos_formaciones(
    resultados_d_4141_343,
    nivel = 0.10
  )

significativos_10_d_4141_343


# =========================================================
# 3. 4141 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_d_4141_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_4141)


# Resultados completos por nivel
resultados_d_4141_4141 <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_4141
  )

resultados_d_4141_4141


# Significativos al 5 %
significativos_d_4141_4141 <-
  extraer_significativos_formaciones(
    resultados_d_4141_4141,
    nivel = 0.05
  )

significativos_d_4141_4141


# Significativos o marginales al 10 %
significativos_10_d_4141_4141 <-
  extraer_significativos_formaciones(
    resultados_d_4141_4141,
    nivel = 0.10
  )

significativos_10_d_4141_4141


# =========================================================
# 4. 4141 LOCAL CONTRA 433 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_d_4141_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_433)


# Resultados completos por nivel
resultados_d_4141_433 <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_433
  )

resultados_d_4141_433


# Significativos al 5 %
significativos_d_4141_433 <-
  extraer_significativos_formaciones(
    resultados_d_4141_433,
    nivel = 0.05
  )

significativos_d_4141_433


# Significativos o marginales al 10 %
significativos_10_d_4141_433 <-
  extraer_significativos_formaciones(
    resultados_d_4141_433,
    nivel = 0.10
  )

significativos_10_d_4141_433


# =========================================================
# 5. 4141 LOCAL CONTRA 442 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_d_4141_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_442)


# Resultados completos por nivel
resultados_d_4141_442 <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_442
  )

resultados_d_4141_442


# Significativos al 5 %
significativos_d_4141_442 <-
  extraer_significativos_formaciones(
    resultados_d_4141_442,
    nivel = 0.05
  )

significativos_d_4141_442


# Significativos o marginales al 10 %
significativos_10_d_4141_442 <-
  extraer_significativos_formaciones(
    resultados_d_4141_442,
    nivel = 0.10
  )

significativos_10_d_4141_442


# =========================================================
# 6. 4141 LOCAL CONTRA 532 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_d_4141_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_532)


# Resultados completos por nivel
resultados_d_4141_532 <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_532
  )

resultados_d_4141_532


# Significativos al 5 %
significativos_d_4141_532 <-
  extraer_significativos_formaciones(
    resultados_d_4141_532,
    nivel = 0.05
  )

significativos_d_4141_532


# Significativos o marginales al 10 %
significativos_10_d_4141_532 <-
  extraer_significativos_formaciones(
    resultados_d_4141_532,
    nivel = 0.10
  )

significativos_10_d_4141_532


# =========================================================
# 7. 4141 LOCAL CONTRA 541 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_d_4141_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_541)


# Resultados completos por nivel
resultados_d_4141_541 <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_541
  )

resultados_d_4141_541


# Significativos al 5 %
significativos_d_4141_541 <-
  extraer_significativos_formaciones(
    resultados_d_4141_541,
    nivel = 0.05
  )

significativos_d_4141_541


# Significativos o marginales al 10 %
significativos_10_d_4141_541 <-
  extraer_significativos_formaciones(
    resultados_d_4141_541,
    nivel = 0.10
  )

significativos_10_d_4141_541


# =========================================================
# 8. 4141 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_d_4141_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_4141_Otras)


# Resultados completos por nivel
resultados_d_4141_Otras <-
  extraer_resultados_formaciones_multinom(
    m_d_4141_Otras
  )

resultados_d_4141_Otras


# Significativos al 5 %
significativos_d_4141_Otras <-
  extraer_significativos_formaciones(
    resultados_d_4141_Otras,
    nivel = 0.05
  )

significativos_d_4141_Otras


# Significativos o marginales al 10 %
significativos_10_d_4141_Otras <-
  extraer_significativos_formaciones(
    resultados_d_4141_Otras,
    nivel = 0.10
  )

significativos_10_d_4141_Otras

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-3-3
# BASE DE DATOS: d
# TODAS LAS REFERENCIAS DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL: 433
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-3-3"
)


# =========================================================
# 1. 433 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_d_433_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_4231)


# Resultados completos por nivel
resultados_d_433_4231 <-
  extraer_resultados_formaciones_multinom(
    m_d_433_4231
  )

resultados_d_433_4231


# Significativos al 5 %
significativos_d_433_4231 <-
  extraer_significativos_formaciones(
    resultados_d_433_4231,
    nivel = 0.05
  )

significativos_d_433_4231


# Significativos o marginales al 10 %
significativos_10_d_433_4231 <-
  extraer_significativos_formaciones(
    resultados_d_433_4231,
    nivel = 0.10
  )

significativos_10_d_433_4231


# =========================================================
# 2. 433 LOCAL CONTRA 343 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_d_433_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_343)


# Resultados completos por nivel
resultados_d_433_343 <-
  extraer_resultados_formaciones_multinom(
    m_d_433_343
  )

resultados_d_433_343


# Significativos al 5 %
significativos_d_433_343 <-
  extraer_significativos_formaciones(
    resultados_d_433_343,
    nivel = 0.05
  )

significativos_d_433_343


# Significativos o marginales al 10 %
significativos_10_d_433_343 <-
  extraer_significativos_formaciones(
    resultados_d_433_343,
    nivel = 0.10
  )

significativos_10_d_433_343


# =========================================================
# 3. 433 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_d_433_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_4141)


# Resultados completos por nivel
resultados_d_433_4141 <-
  extraer_resultados_formaciones_multinom(
    m_d_433_4141
  )

resultados_d_433_4141


# Significativos al 5 %
significativos_d_433_4141 <-
  extraer_significativos_formaciones(
    resultados_d_433_4141,
    nivel = 0.05
  )

significativos_d_433_4141


# Significativos o marginales al 10 %
significativos_10_d_433_4141 <-
  extraer_significativos_formaciones(
    resultados_d_433_4141,
    nivel = 0.10
  )

significativos_10_d_433_4141


# =========================================================
# 4. 433 LOCAL CONTRA 433 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_d_433_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_433)


# Resultados completos por nivel
resultados_d_433_433 <-
  extraer_resultados_formaciones_multinom(
    m_d_433_433
  )

resultados_d_433_433


# Significativos al 5 %
significativos_d_433_433 <-
  extraer_significativos_formaciones(
    resultados_d_433_433,
    nivel = 0.05
  )

significativos_d_433_433


# Significativos o marginales al 10 %
significativos_10_d_433_433 <-
  extraer_significativos_formaciones(
    resultados_d_433_433,
    nivel = 0.10
  )

significativos_10_d_433_433


# =========================================================
# 5. 433 LOCAL CONTRA 442 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_d_433_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_442)


# Resultados completos por nivel
resultados_d_433_442 <-
  extraer_resultados_formaciones_multinom(
    m_d_433_442
  )

resultados_d_433_442


# Significativos al 5 %
significativos_d_433_442 <-
  extraer_significativos_formaciones(
    resultados_d_433_442,
    nivel = 0.05
  )

significativos_d_433_442


# Significativos o marginales al 10 %
significativos_10_d_433_442 <-
  extraer_significativos_formaciones(
    resultados_d_433_442,
    nivel = 0.10
  )

significativos_10_d_433_442


# =========================================================
# 6. 433 LOCAL CONTRA 532 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_d_433_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_532)


# Resultados completos por nivel
resultados_d_433_532 <-
  extraer_resultados_formaciones_multinom(
    m_d_433_532
  )

resultados_d_433_532


# Significativos al 5 %
significativos_d_433_532 <-
  extraer_significativos_formaciones(
    resultados_d_433_532,
    nivel = 0.05
  )

significativos_d_433_532


# Significativos o marginales al 10 %
significativos_10_d_433_532 <-
  extraer_significativos_formaciones(
    resultados_d_433_532,
    nivel = 0.10
  )

significativos_10_d_433_532


# =========================================================
# 7. 433 LOCAL CONTRA 541 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_d_433_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_541)


# Resultados completos por nivel
resultados_d_433_541 <-
  extraer_resultados_formaciones_multinom(
    m_d_433_541
  )

resultados_d_433_541


# Significativos al 5 %
significativos_d_433_541 <-
  extraer_significativos_formaciones(
    resultados_d_433_541,
    nivel = 0.05
  )

significativos_d_433_541


# Significativos o marginales al 10 %
significativos_10_d_433_541 <-
  extraer_significativos_formaciones(
    resultados_d_433_541,
    nivel = 0.10
  )

significativos_10_d_433_541


# =========================================================
# 8. 433 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_d_433_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_433_Otras)


# Resultados completos por nivel
resultados_d_433_Otras <-
  extraer_resultados_formaciones_multinom(
    m_d_433_Otras
  )

resultados_d_433_Otras


# Significativos al 5 %
significativos_d_433_Otras <-
  extraer_significativos_formaciones(
    resultados_d_433_Otras,
    nivel = 0.05
  )

significativos_d_433_Otras


# Significativos o marginales al 10 %
significativos_10_d_433_Otras <-
  extraer_significativos_formaciones(
    resultados_d_433_Otras,
    nivel = 0.10
  )

significativos_10_d_433_Otras

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-4-2
# BASE DE DATOS: d
# TODAS LAS REFERENCIAS DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL: 442
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-4-2"
)


# =========================================================
# 1. 442 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_d_442_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_4231)


# Resultados completos por nivel
resultados_d_442_4231 <-
  extraer_resultados_formaciones_multinom(
    m_d_442_4231
  )

resultados_d_442_4231


# Significativos al 5 %
significativos_d_442_4231 <-
  extraer_significativos_formaciones(
    resultados_d_442_4231,
    nivel = 0.05
  )

significativos_d_442_4231


# Significativos o marginales al 10 %
significativos_10_d_442_4231 <-
  extraer_significativos_formaciones(
    resultados_d_442_4231,
    nivel = 0.10
  )

significativos_10_d_442_4231


# =========================================================
# 2. 442 LOCAL CONTRA 343 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_d_442_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_343)


# Resultados completos por nivel
resultados_d_442_343 <-
  extraer_resultados_formaciones_multinom(
    m_d_442_343
  )

resultados_d_442_343


# Significativos al 5 %
significativos_d_442_343 <-
  extraer_significativos_formaciones(
    resultados_d_442_343,
    nivel = 0.05
  )

significativos_d_442_343


# Significativos o marginales al 10 %
significativos_10_d_442_343 <-
  extraer_significativos_formaciones(
    resultados_d_442_343,
    nivel = 0.10
  )

significativos_10_d_442_343


# =========================================================
# 3. 442 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_d_442_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_4141)


# Resultados completos por nivel
resultados_d_442_4141 <-
  extraer_resultados_formaciones_multinom(
    m_d_442_4141
  )

resultados_d_442_4141


# Significativos al 5 %
significativos_d_442_4141 <-
  extraer_significativos_formaciones(
    resultados_d_442_4141,
    nivel = 0.05
  )

significativos_d_442_4141


# Significativos o marginales al 10 %
significativos_10_d_442_4141 <-
  extraer_significativos_formaciones(
    resultados_d_442_4141,
    nivel = 0.10
  )

significativos_10_d_442_4141


# =========================================================
# 4. 442 LOCAL CONTRA 433 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_d_442_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_433)


# Resultados completos por nivel
resultados_d_442_433 <-
  extraer_resultados_formaciones_multinom(
    m_d_442_433
  )

resultados_d_442_433


# Significativos al 5 %
significativos_d_442_433 <-
  extraer_significativos_formaciones(
    resultados_d_442_433,
    nivel = 0.05
  )

significativos_d_442_433


# Significativos o marginales al 10 %
significativos_10_d_442_433 <-
  extraer_significativos_formaciones(
    resultados_d_442_433,
    nivel = 0.10
  )

significativos_10_d_442_433


# =========================================================
# 5. 442 LOCAL CONTRA 442 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_d_442_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_442)


# Resultados completos por nivel
resultados_d_442_442 <-
  extraer_resultados_formaciones_multinom(
    m_d_442_442
  )

resultados_d_442_442


# Significativos al 5 %
significativos_d_442_442 <-
  extraer_significativos_formaciones(
    resultados_d_442_442,
    nivel = 0.05
  )

significativos_d_442_442


# Significativos o marginales al 10 %
significativos_10_d_442_442 <-
  extraer_significativos_formaciones(
    resultados_d_442_442,
    nivel = 0.10
  )

significativos_10_d_442_442


# =========================================================
# 6. 442 LOCAL CONTRA 532 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_d_442_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_532)


# Resultados completos por nivel
resultados_d_442_532 <-
  extraer_resultados_formaciones_multinom(
    m_d_442_532
  )

resultados_d_442_532


# Significativos al 5 %
significativos_d_442_532 <-
  extraer_significativos_formaciones(
    resultados_d_442_532,
    nivel = 0.05
  )

significativos_d_442_532


# Significativos o marginales al 10 %
significativos_10_d_442_532 <-
  extraer_significativos_formaciones(
    resultados_d_442_532,
    nivel = 0.10
  )

significativos_10_d_442_532


# =========================================================
# 7. 442 LOCAL CONTRA 541 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_d_442_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_541)


# Resultados completos por nivel
resultados_d_442_541 <-
  extraer_resultados_formaciones_multinom(
    m_d_442_541
  )

resultados_d_442_541


# Significativos al 5 %
significativos_d_442_541 <-
  extraer_significativos_formaciones(
    resultados_d_442_541,
    nivel = 0.05
  )

significativos_d_442_541


# Significativos o marginales al 10 %
significativos_10_d_442_541 <-
  extraer_significativos_formaciones(
    resultados_d_442_541,
    nivel = 0.10
  )

significativos_10_d_442_541


# =========================================================
# 8. 442 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_d_442_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_442_Otras)


# Resultados completos por nivel
resultados_d_442_Otras <-
  extraer_resultados_formaciones_multinom(
    m_d_442_Otras
  )

resultados_d_442_Otras


# Significativos al 5 %
significativos_d_442_Otras <-
  extraer_significativos_formaciones(
    resultados_d_442_Otras,
    nivel = 0.05
  )

significativos_d_442_Otras


# Significativos o marginales al 10 %
significativos_10_d_442_Otras <-
  extraer_significativos_formaciones(
    resultados_d_442_Otras,
    nivel = 0.10
  )

significativos_10_d_442_Otras

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-5-4-1
# BASE DE DATOS: d
# TODAS LAS REFERENCIAS DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL: 541
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-5-4-1"
)


# =========================================================
# 1. 541 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_d_541_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_4231)


# Resultados completos por nivel
resultados_d_541_4231 <-
  extraer_resultados_formaciones_multinom(
    m_d_541_4231
  )

resultados_d_541_4231


# Significativos al 5 %
significativos_d_541_4231 <-
  extraer_significativos_formaciones(
    resultados_d_541_4231,
    nivel = 0.05
  )

significativos_d_541_4231


# Significativos o marginales al 10 %
significativos_10_d_541_4231 <-
  extraer_significativos_formaciones(
    resultados_d_541_4231,
    nivel = 0.10
  )

significativos_10_d_541_4231


# =========================================================
# 2. 541 LOCAL CONTRA 343 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_d_541_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_343)


# Resultados completos por nivel
resultados_d_541_343 <-
  extraer_resultados_formaciones_multinom(
    m_d_541_343
  )

resultados_d_541_343


# Significativos al 5 %
significativos_d_541_343 <-
  extraer_significativos_formaciones(
    resultados_d_541_343,
    nivel = 0.05
  )

significativos_d_541_343


# Significativos o marginales al 10 %
significativos_10_d_541_343 <-
  extraer_significativos_formaciones(
    resultados_d_541_343,
    nivel = 0.10
  )

significativos_10_d_541_343


# =========================================================
# 3. 541 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_d_541_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_4141)


# Resultados completos por nivel
resultados_d_541_4141 <-
  extraer_resultados_formaciones_multinom(
    m_d_541_4141
  )

resultados_d_541_4141


# Significativos al 5 %
significativos_d_541_4141 <-
  extraer_significativos_formaciones(
    resultados_d_541_4141,
    nivel = 0.05
  )

significativos_d_541_4141


# Significativos o marginales al 10 %
significativos_10_d_541_4141 <-
  extraer_significativos_formaciones(
    resultados_d_541_4141,
    nivel = 0.10
  )

significativos_10_d_541_4141


# =========================================================
# 4. 541 LOCAL CONTRA 433 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_d_541_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_433)


# Resultados completos por nivel
resultados_d_541_433 <-
  extraer_resultados_formaciones_multinom(
    m_d_541_433
  )

resultados_d_541_433


# Significativos al 5 %
significativos_d_541_433 <-
  extraer_significativos_formaciones(
    resultados_d_541_433,
    nivel = 0.05
  )

significativos_d_541_433


# Significativos o marginales al 10 %
significativos_10_d_541_433 <-
  extraer_significativos_formaciones(
    resultados_d_541_433,
    nivel = 0.10
  )

significativos_10_d_541_433


# =========================================================
# 5. 541 LOCAL CONTRA 442 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_d_541_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_442)


# Resultados completos por nivel
resultados_d_541_442 <-
  extraer_resultados_formaciones_multinom(
    m_d_541_442
  )

resultados_d_541_442


# Significativos al 5 %
significativos_d_541_442 <-
  extraer_significativos_formaciones(
    resultados_d_541_442,
    nivel = 0.05
  )

significativos_d_541_442


# Significativos o marginales al 10 %
significativos_10_d_541_442 <-
  extraer_significativos_formaciones(
    resultados_d_541_442,
    nivel = 0.10
  )

significativos_10_d_541_442


# =========================================================
# 6. 541 LOCAL CONTRA 532 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_d_541_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_532)


# Resultados completos por nivel
resultados_d_541_532 <-
  extraer_resultados_formaciones_multinom(
    m_d_541_532
  )

resultados_d_541_532


# Significativos al 5 %
significativos_d_541_532 <-
  extraer_significativos_formaciones(
    resultados_d_541_532,
    nivel = 0.05
  )

significativos_d_541_532


# Significativos o marginales al 10 %
significativos_10_d_541_532 <-
  extraer_significativos_formaciones(
    resultados_d_541_532,
    nivel = 0.10
  )

significativos_10_d_541_532


# =========================================================
# 7. 541 LOCAL CONTRA 541 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_d_541_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_541)


# Resultados completos por nivel
resultados_d_541_541 <-
  extraer_resultados_formaciones_multinom(
    m_d_541_541
  )

resultados_d_541_541


# Significativos al 5 %
significativos_d_541_541 <-
  extraer_significativos_formaciones(
    resultados_d_541_541,
    nivel = 0.05
  )

significativos_d_541_541


# Significativos o marginales al 10 %
significativos_10_d_541_541 <-
  extraer_significativos_formaciones(
    resultados_d_541_541,
    nivel = 0.10
  )

significativos_10_d_541_541


# =========================================================
# 8. 541 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_d_541_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_541_Otras)


# Resultados completos por nivel
resultados_d_541_Otras <-
  extraer_resultados_formaciones_multinom(
    m_d_541_Otras
  )

resultados_d_541_Otras


# Significativos al 5 %
significativos_d_541_Otras <-
  extraer_significativos_formaciones(
    resultados_d_541_Otras,
    nivel = 0.05
  )

significativos_d_541_Otras


# Significativos o marginales al 10 %
significativos_10_d_541_Otras <-
  extraer_significativos_formaciones(
    resultados_d_541_Otras,
    nivel = 0.10
  )

significativos_10_d_541_Otras

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: OTRAS
# BASE DE DATOS: d
# TODAS LAS REFERENCIAS DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL: OTRAS
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "Otras"
)


# =========================================================
# 1. OTRAS LOCAL CONTRA 4231 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_d_Otras_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_4231)


# Resultados completos por nivel
resultados_d_Otras_4231 <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_4231
  )

resultados_d_Otras_4231


# Significativos al 5 %
significativos_d_Otras_4231 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_4231,
    nivel = 0.05
  )

significativos_d_Otras_4231


# Significativos o marginales al 10 %
significativos_10_d_Otras_4231 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_4231,
    nivel = 0.10
  )

significativos_10_d_Otras_4231


# =========================================================
# 2. OTRAS LOCAL CONTRA 343 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_d_Otras_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_343)


# Resultados completos por nivel
resultados_d_Otras_343 <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_343
  )

resultados_d_Otras_343


# Significativos al 5 %
significativos_d_Otras_343 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_343,
    nivel = 0.05
  )

significativos_d_Otras_343


# Significativos o marginales al 10 %
significativos_10_d_Otras_343 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_343,
    nivel = 0.10
  )

significativos_10_d_Otras_343


# =========================================================
# 3. OTRAS LOCAL CONTRA 4141 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_d_Otras_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_4141)


# Resultados completos por nivel
resultados_d_Otras_4141 <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_4141
  )

resultados_d_Otras_4141


# Significativos al 5 %
significativos_d_Otras_4141 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_4141,
    nivel = 0.05
  )

significativos_d_Otras_4141


# Significativos o marginales al 10 %
significativos_10_d_Otras_4141 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_4141,
    nivel = 0.10
  )

significativos_10_d_Otras_4141


# =========================================================
# 4. OTRAS LOCAL CONTRA 433 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_d_Otras_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_433)


# Resultados completos por nivel
resultados_d_Otras_433 <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_433
  )

resultados_d_Otras_433


# Significativos al 5 %
significativos_d_Otras_433 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_433,
    nivel = 0.05
  )

significativos_d_Otras_433


# Significativos o marginales al 10 %
significativos_10_d_Otras_433 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_433,
    nivel = 0.10
  )

significativos_10_d_Otras_433


# =========================================================
# 5. OTRAS LOCAL CONTRA 442 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_d_Otras_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_442)


# Resultados completos por nivel
resultados_d_Otras_442 <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_442
  )

resultados_d_Otras_442


# Significativos al 5 %
significativos_d_Otras_442 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_442,
    nivel = 0.05
  )

significativos_d_Otras_442


# Significativos o marginales al 10 %
significativos_10_d_Otras_442 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_442,
    nivel = 0.10
  )

significativos_10_d_Otras_442


# =========================================================
# 6. OTRAS LOCAL CONTRA 532 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_d_Otras_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_532)


# Resultados completos por nivel
resultados_d_Otras_532 <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_532
  )

resultados_d_Otras_532


# Significativos al 5 %
significativos_d_Otras_532 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_532,
    nivel = 0.05
  )

significativos_d_Otras_532


# Significativos o marginales al 10 %
significativos_10_d_Otras_532 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_532,
    nivel = 0.10
  )

significativos_10_d_Otras_532


# =========================================================
# 7. OTRAS LOCAL CONTRA 541 VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_d_Otras_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_541)


# Resultados completos por nivel
resultados_d_Otras_541 <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_541
  )

resultados_d_Otras_541


# Significativos al 5 %
significativos_d_Otras_541 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_541,
    nivel = 0.05
  )

significativos_d_Otras_541


# Significativos o marginales al 10 %
significativos_10_d_Otras_541 <-
  extraer_significativos_formaciones(
    resultados_d_Otras_541,
    nivel = 0.10
  )

significativos_10_d_Otras_541


# =========================================================
# 8. OTRAS LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_d_Otras_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_d_Otras_Otras)


# Resultados completos por nivel
resultados_d_Otras_Otras <-
  extraer_resultados_formaciones_multinom(
    m_d_Otras_Otras
  )

resultados_d_Otras_Otras


# Significativos al 5 %
significativos_d_Otras_Otras <-
  extraer_significativos_formaciones(
    resultados_d_Otras_Otras,
    nivel = 0.05
  )

significativos_d_Otras_Otras


# Significativos o marginales al 10 %
significativos_10_d_Otras_Otras <-
  extraer_significativos_formaciones(
    resultados_d_Otras_Otras,
    nivel = 0.10
  )

significativos_10_d_Otras_Otras

# =========================================================
# =========================================================
# 1. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Extraer directamente el model frame utilizado en la estimación
datos_formaciones_d <- model.frame(
  m_d_4231_4231
)

# Eliminar posibles niveles no utilizados
datos_formaciones_d <- droplevels(
  datos_formaciones_d
)


# Número de observaciones empleadas
nrow(datos_formaciones_d)

# =========================================================
# 2. PROBABILIDADES ESTIMADAS
# =========================================================

prob_formaciones_d <- predict(
  m_d_4231_4231,
  newdata = datos_formaciones_d,
  type = "probs"
)


# Primeras probabilidades en escala 0-1
head(prob_formaciones_d)


# Primeras probabilidades en porcentaje
round(
  head(prob_formaciones_d * 100),
  2
)


# Comprobar que las probabilidades suman 1
rowSums(
  prob_formaciones_d
)[1:10]


# Comprobar todas las observaciones
all(
  abs(
    rowSums(prob_formaciones_d) - 1
  ) < 1e-8
)


# =========================================================
# 3. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_formaciones_d <- predict(
  m_d_4231_4231,
  newdata = datos_formaciones_d,
  type = "class"
)


# Primeras predicciones
head(pred_formaciones_d)


# Distribución de los resultados predichos
table(
  pred_formaciones_d,
  useNA = "always"
)


# =========================================================
# 4. PREPARAR RESULTADOS REALES Y PREDICHOS
# =========================================================

# Niveles de la variable dependiente utilizados por el modelo
niveles_resultado_formaciones_d <- levels(
  datos_formaciones_d$resultado_partido_local
)

niveles_resultado_formaciones_d


# Resultados reales
real_formaciones_d <- factor(
  datos_formaciones_d$resultado_partido_local,
  levels = niveles_resultado_formaciones_d
)


# Resultados predichos con los mismos niveles
pred_formaciones_d <- factor(
  pred_formaciones_d,
  levels = niveles_resultado_formaciones_d
)


# Comprobar longitudes
length(real_formaciones_d)
length(pred_formaciones_d)

length(real_formaciones_d) ==
  length(pred_formaciones_d)


# =========================================================
# 5. MATRIZ DE CONFUSIÓN
# =========================================================

mc_formaciones_d <- table(
  Real = real_formaciones_d,
  Predicho = pred_formaciones_d
)

mc_formaciones_d


# Matriz con totales por filas y columnas
addmargins(
  mc_formaciones_d
)


# =========================================================
# 6. ACCURACY TOTAL
# =========================================================

accuracy_formaciones_d <- sum(
  diag(mc_formaciones_d)
) / sum(mc_formaciones_d)

accuracy_formaciones_d


# =========================================================
# 7. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_formaciones_d <- diag(
  mc_formaciones_d
) / rowSums(mc_formaciones_d)


# Evitar NaN o Inf
sensibilidad_formaciones_d[
  is.nan(sensibilidad_formaciones_d) |
    is.infinite(sensibilidad_formaciones_d)
] <- NA

sensibilidad_formaciones_d


# =========================================================
# 8. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_formaciones_d <- diag(
  mc_formaciones_d
) / colSums(mc_formaciones_d)


# Evitar NaN o Inf si una categoría nunca se predice
precision_formaciones_d[
  is.nan(precision_formaciones_d) |
    is.infinite(precision_formaciones_d)
] <- NA

precision_formaciones_d


# =========================================================
# 9. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_formaciones_d <- mean(
  sensibilidad_formaciones_d,
  na.rm = TRUE
)

balanced_accuracy_formaciones_d


# =========================================================
# 10. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_formaciones_d <- data.frame(
  Categoria = niveles_resultado_formaciones_d,
  
  Sensibilidad = round(
    as.numeric(
      sensibilidad_formaciones_d
    ),
    4
  ),
  
  Precision = round(
    as.numeric(
      precision_formaciones_d
    ),
    4
  )
)

metricas_formaciones_d


# =========================================================
# 11. TABLA DE MÉTRICAS GENERALES
# =========================================================

metricas_generales_formaciones_d <- data.frame(
  Modelo = "Formación local + formación visitante",
  
  Accuracy = round(
    accuracy_formaciones_d,
    4
  ),
  
  Balanced_Accuracy = round(
    balanced_accuracy_formaciones_d,
    4
  )
)

metricas_generales_formaciones_d
# =========================================================
# COMPROBAR LA FORMACIÓN VISITANTE DE d
# =========================================================

print(
  levels(
    factor(
      d$formacion_visit_dep
    )
  )
)

print(
  table(
    d$formacion_visit_dep,
    useNA = "always"
  )
)

print(
  "Otras" %in%
    levels(
      factor(
        d$formacion_visit_dep
      )
    )
)
# =========================================================
# RECONSTRUIR OTRAS EN LA FORMACIÓN VISITANTE
# =========================================================

formaciones_principales_d <- c(
  "1-4-2-3-1",
  "1-3-4-3",
  "1-4-1-4-1",
  "1-4-3-3",
  "1-4-4-2",
  "1-5-3-2",
  "1-5-4-1"
)

# Partir preferentemente de la variable original
if ("formacion_visit" %in% names(d)) {
  
  formacion_visitante_original_d <-
    trimws(
      as.character(
        d$formacion_visit
      )
    )
  
} else {
  
  formacion_visitante_original_d <-
    trimws(
      as.character(
        d$formacion_visit_dep
      )
    )
}
formacion_visitante_original_d <- gsub(
  '"',
  "",
  formacion_visitante_original_d,
  fixed = TRUE
)
d$formacion_visit_dep <-
  ifelse(
    is.na(formacion_visitante_original_d),
    NA_character_,
    ifelse(
      formacion_visitante_original_d %in%
        formaciones_principales_d,
      formacion_visitante_original_d,
      "Otras"
    )
  )
d$formacion_visit_dep <- factor(
  d$formacion_visit_dep,
  levels = c(
    formaciones_principales_d,
    "Otras"
  )
)
print(
  table(
    d$formacion_visit_dep,
    useNA = "always"
  )
)

print(
  levels(
    d$formacion_visit_dep
  )
)

print(
  sum(
    d$formacion_visit_dep == "Otras",
    na.rm = TRUE
  )
)
# =========================================================
# 8. 4231 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d$formacion_local_dep <- relevel(
  factor(
    d$formacion_local_dep
  ),
  ref = "1-4-2-3-1"
)

d$formacion_visit_dep <- relevel(
  factor(
    d$formacion_visit_dep
  ),
  ref = "Otras"
)

m_d_4231_Otras <- nnet::multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(
  m_d_4231_Otras
)

