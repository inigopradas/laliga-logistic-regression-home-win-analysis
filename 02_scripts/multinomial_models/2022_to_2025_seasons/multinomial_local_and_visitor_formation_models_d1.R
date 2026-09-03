# ==============================================================================
# Title: Multi-season multinomial models for home and visiting formations
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script analyses the association between tactical formations and match
# outcomes using LaLiga data from the 2022/2023, 2023/2024 and 2024/2025
# seasons.
#
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat. The draw is used as the reference outcome. The tactical
# formations used by the home and visiting teams are analysed separately.
#
# The first part estimates multinomial logistic regression models using only
# the home-team formation as the explanatory variable. The reference formation
# is changed successively to obtain direct comparisons among all retained
# formation categories.
#
# The second part estimates equivalent multinomial logistic regression models
# using only the visiting-team formation. The visiting reference category is
# also changed successively to obtain comparisons among all retained tactical
# formations.
#
# For each reference specification, the script extracts coefficients, standard
# errors, odds ratios and bilateral Wald p-values. Results with evidence at the
# 5% and 10% significance levels are identified using the previously defined
# result-extraction functions.
#
# The 1-4-2-3-1 formation is used as the principal reference category for the
# evaluation of the home-only and visitor-only models. Their classification
# performance is assessed using predicted match outcomes, confusion matrices,
# overall accuracy, class-specific sensitivity, class-specific precision and
# balanced accuracy.
#
# Dataset:
#   d1, containing match-level observations from the 2022/2023, 2023/2024 and
#   2024/2025 LaLiga seasons.
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
# Principal reference formation:
#   1-4-2-3-1
#
# Statistical method:
#   Multinomial logistic regression estimated with nnet::multinom().
#
# Home-formation models:
#   m2_local_4231, m2_local_3421, m2_local_343, m2_local_352,
#   m2_local_4132, m2_local_4141, m2_local_4312, m2_local_433,
#   m2_local_4411, m2_local_442, m2_local_451, m2_local_532,
#   m2_local_541 and m2_local_Otras.
#
# Principal home-formation model:
#   m2_local_4231
#
# Visiting-formation models:
#   m3_visit_4231, m3_visit_3421, m3_visit_343, m3_visit_352,
#   m3_visit_4132, m3_visit_4141, m3_visit_4312, m3_visit_433,
#   m3_visit_4411, m3_visit_442, m3_visit_451, m3_visit_532,
#   m3_visit_541 and m3_visit_Otras.
#
# Principal visiting-formation model:
#   m3_visit_4231
#
# Required functions:
#   extraer_resultados_formaciones_multinom()
#   extraer_significativos_formaciones()
#
# Main


# =========================================================
# 1. FORMACIÓN LOCAL DE REFERENCIA: 4231
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-2-3-1"
)

m2_local_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_4231)

resultados_local_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_4231
  )

resultados_local_4231_d1

# Significativos al 5 %
significativos_local_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4231_d1,
    nivel = 0.05
  )

significativos_local_4231_d1

# Significativos o marginales al 10 %
significativos_10_local_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4231_d1,
    nivel = 0.10
  )

significativos_10_local_4231_d1


# =========================================================
# 2. FORMACIÓN LOCAL DE REFERENCIA: 3421
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-3-4-2-1"
)

m2_local_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_3421)

resultados_local_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_3421
  )

resultados_local_3421_d1

# Significativos al 5 %
significativos_local_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_local_3421_d1,
    nivel = 0.05
  )

significativos_local_3421_d1

# Significativos o marginales al 10 %
significativos_10_local_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_local_3421_d1,
    nivel = 0.10
  )

significativos_10_local_3421_d1


# =========================================================
# 3. FORMACIÓN LOCAL DE REFERENCIA: 343
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-3-4-3"
)

m2_local_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_343)

resultados_local_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_343
  )

resultados_local_343_d1

# Significativos al 5 %
significativos_local_343_d1 <-
  extraer_significativos_formaciones(
    resultados_local_343_d1,
    nivel = 0.05
  )

significativos_local_343_d1

# Significativos o marginales al 10 %
significativos_10_local_343_d1 <-
  extraer_significativos_formaciones(
    resultados_local_343_d1,
    nivel = 0.10
  )

significativos_10_local_343_d1


# =========================================================
# 4. FORMACIÓN LOCAL DE REFERENCIA: 352
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-3-5-2"
)

m2_local_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_352)

resultados_local_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_352
  )

resultados_local_352_d1

# Significativos al 5 %
significativos_local_352_d1 <-
  extraer_significativos_formaciones(
    resultados_local_352_d1,
    nivel = 0.05
  )

significativos_local_352_d1

# Significativos o marginales al 10 %
significativos_10_local_352_d1 <-
  extraer_significativos_formaciones(
    resultados_local_352_d1,
    nivel = 0.10
  )

significativos_10_local_352_d1


# =========================================================
# 5. FORMACIÓN LOCAL DE REFERENCIA: 4132
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-1-3-2"
)

m2_local_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_4132)

resultados_local_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_4132
  )

resultados_local_4132_d1

# Significativos al 5 %
significativos_local_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4132_d1,
    nivel = 0.05
  )

significativos_local_4132_d1

# Significativos o marginales al 10 %
significativos_10_local_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4132_d1,
    nivel = 0.10
  )

significativos_10_local_4132_d1


# =========================================================
# 6. FORMACIÓN LOCAL DE REFERENCIA: 4141
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-1-4-1"
)

m2_local_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_4141)

resultados_local_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_4141
  )

resultados_local_4141_d1

# Significativos al 5 %
significativos_local_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4141_d1,
    nivel = 0.05
  )

significativos_local_4141_d1

# Significativos o marginales al 10 %
significativos_10_local_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4141_d1,
    nivel = 0.10
  )

significativos_10_local_4141_d1


# =========================================================
# 7. FORMACIÓN LOCAL DE REFERENCIA: 4312
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-3-1-2"
)

m2_local_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_4312)

resultados_local_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_4312
  )

resultados_local_4312_d1

# Significativos al 5 %
significativos_local_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4312_d1,
    nivel = 0.05
  )

significativos_local_4312_d1

# Significativos o marginales al 10 %
significativos_10_local_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4312_d1,
    nivel = 0.10
  )

significativos_10_local_4312_d1


# =========================================================
# 8. FORMACIÓN LOCAL DE REFERENCIA: 433
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-3-3"
)

m2_local_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_433)

resultados_local_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_433
  )

resultados_local_433_d1

# Significativos al 5 %
significativos_local_433_d1 <-
  extraer_significativos_formaciones(
    resultados_local_433_d1,
    nivel = 0.05
  )

significativos_local_433_d1

# Significativos o marginales al 10 %
significativos_10_local_433_d1 <-
  extraer_significativos_formaciones(
    resultados_local_433_d1,
    nivel = 0.10
  )

significativos_10_local_433_d1


# =========================================================
# 9. FORMACIÓN LOCAL DE REFERENCIA: 4411
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-4-1-1"
)

m2_local_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_4411)

resultados_local_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_4411
  )

resultados_local_4411_d1

# Significativos al 5 %
significativos_local_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4411_d1,
    nivel = 0.05
  )

significativos_local_4411_d1

# Significativos o marginales al 10 %
significativos_10_local_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_local_4411_d1,
    nivel = 0.10
  )

significativos_10_local_4411_d1


# =========================================================
# 10. FORMACIÓN LOCAL DE REFERENCIA: 442
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-4-2"
)

m2_local_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_442)

resultados_local_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_442
  )

resultados_local_442_d1

# Significativos al 5 %
significativos_local_442_d1 <-
  extraer_significativos_formaciones(
    resultados_local_442_d1,
    nivel = 0.05
  )

significativos_local_442_d1

# Significativos o marginales al 10 %
significativos_10_local_442_d1 <-
  extraer_significativos_formaciones(
    resultados_local_442_d1,
    nivel = 0.10
  )

significativos_10_local_442_d1


# =========================================================
# 11. FORMACIÓN LOCAL DE REFERENCIA: 451
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-5-1"
)

m2_local_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_451)

resultados_local_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_451
  )

resultados_local_451_d1

# Significativos al 5 %
significativos_local_451_d1 <-
  extraer_significativos_formaciones(
    resultados_local_451_d1,
    nivel = 0.05
  )

significativos_local_451_d1

# Significativos o marginales al 10 %
significativos_10_local_451_d1 <-
  extraer_significativos_formaciones(
    resultados_local_451_d1,
    nivel = 0.10
  )

significativos_10_local_451_d1


# =========================================================
# 12. FORMACIÓN LOCAL DE REFERENCIA: 532
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-5-3-2"
)

m2_local_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_532)

resultados_local_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_532
  )

resultados_local_532_d1

# Significativos al 5 %
significativos_local_532_d1 <-
  extraer_significativos_formaciones(
    resultados_local_532_d1,
    nivel = 0.05
  )

significativos_local_532_d1

# Significativos o marginales al 10 %
significativos_10_local_532_d1 <-
  extraer_significativos_formaciones(
    resultados_local_532_d1,
    nivel = 0.10
  )

significativos_10_local_532_d1


# =========================================================
# 13. FORMACIÓN LOCAL DE REFERENCIA: 541
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-5-4-1"
)

m2_local_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_541)

resultados_local_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_541
  )

resultados_local_541_d1

# Significativos al 5 %
significativos_local_541_d1 <-
  extraer_significativos_formaciones(
    resultados_local_541_d1,
    nivel = 0.05
  )

significativos_local_541_d1

# Significativos o marginales al 10 %
significativos_10_local_541_d1 <-
  extraer_significativos_formaciones(
    resultados_local_541_d1,
    nivel = 0.10
  )

significativos_10_local_541_d1


# =========================================================
# 14. FORMACIÓN LOCAL DE REFERENCIA: OTRAS
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "Otras"
)

m2_local_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m2_local_Otras)

resultados_local_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m2_local_Otras
  )

resultados_local_Otras_d1

# Significativos al 5 %
significativos_local_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_local_Otras_d1,
    nivel = 0.05
  )

significativos_local_Otras_d1

# Significativos o marginales al 10 %
significativos_10_local_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_local_Otras_d1,
    nivel = 0.10
  )

significativos_10_local_Otras_d1

# =========================================================
# MATRIZ DE CONFUSIÓN
# MODELO: SOLO FORMACIÓN LOCAL
# Modelo utilizado: m2_local_4231
# =========================================================


# =========================================================
# 1. DATOS UTILIZADOS POR EL MODELO
# =========================================================

datos_formacion_local_d1 <- model.frame(
  formula = formula(m2_local_4231),
  data = d1,
  na.action = na.omit
)

datos_formacion_local_d1 <- droplevels(
  datos_formacion_local_d1
)



# =========================================================
# 2. PREDICCIONES
# =========================================================

pred_formacion_local_d1 <- predict(
  m2_local_4231,
  newdata = datos_formacion_local_d1,
  type = "class"
)

head(pred_formacion_local_d1)

table(
  pred_formacion_local_d1,
  useNA = "always"
)


# =========================================================
# 3. RESULTADOS REALES Y NIVELES
# =========================================================

niveles_resultado_formacion_local <- levels(
  d1$resultado_partido_local
)

real_formacion_local_d1 <- factor(
  datos_formacion_local_d1$resultado_partido_local,
  levels = niveles_resultado_formacion_local
)

pred_formacion_local_d1 <- factor(
  pred_formacion_local_d1,
  levels = niveles_resultado_formacion_local
)

# Comprobar que las longitudes coinciden
length(real_formacion_local_d1)
length(pred_formacion_local_d1)

length(real_formacion_local_d1) ==
  length(pred_formacion_local_d1)


# =========================================================
# 4. MATRIZ DE CONFUSIÓN
# =========================================================

mc_formacion_local_d1 <- table(
  Real = real_formacion_local_d1,
  Predicho = pred_formacion_local_d1
)

mc_formacion_local_d1

# Matriz con totales
addmargins(
  mc_formacion_local_d1
)


# =========================================================
# 5. ACCURACY
# =========================================================

accuracy_formacion_local_d1 <- sum(
  diag(mc_formacion_local_d1)
) / sum(mc_formacion_local_d1)

accuracy_formacion_local_d1


# =========================================================
# 6. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_formacion_local_d1 <- diag(
  mc_formacion_local_d1
) / rowSums(mc_formacion_local_d1)

sensibilidad_formacion_local_d1[
  is.nan(sensibilidad_formacion_local_d1) |
    is.infinite(sensibilidad_formacion_local_d1)
] <- NA

sensibilidad_formacion_local_d1


# =========================================================
# 7. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_formacion_local_d1 <- diag(
  mc_formacion_local_d1
) / colSums(mc_formacion_local_d1)

precision_formacion_local_d1[
  is.nan(precision_formacion_local_d1) |
    is.infinite(precision_formacion_local_d1)
] <- NA

precision_formacion_local_d1


# =========================================================
# 8. BALANCED ACCURACY
# =========================================================

balanced_accuracy_formacion_local_d1 <- mean(
  sensibilidad_formacion_local_d1,
  na.rm = TRUE
)

balanced_accuracy_formacion_local_d1


# =========================================================
# 9. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_formacion_local_d1 <- data.frame(
  Categoria = niveles_resultado_formacion_local,
  Sensibilidad = round(
    as.numeric(sensibilidad_formacion_local_d1),
    4
  ),
  Precision = round(
    as.numeric(precision_formacion_local_d1),
    4
  )
)

metricas_formacion_local_d1


# =========================================================
# 10. MÉTRICAS GENERALES
# =========================================================

metricas_generales_formacion_local_d1 <- data.frame(
  Modelo = "Solo formación local",
  Accuracy = round(
    accuracy_formacion_local_d1,
    4
  ),
  Balanced_Accuracy = round(
    balanced_accuracy_formacion_local_d1,
    4
  )
)

metricas_generales_formacion_local_d1
##############################################################################################
# =========================================================
# MODELOS MULTINOMIALES SEGÚN LA FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Variable explicativa: formacion_visit_dep
# Categoría de referencia del resultado: Empate
# Base de datos: d1
# =========================================================

# =========================================================
# 1. FORMACIÓN VISITANTE DE REFERENCIA: 4231
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m3_visit_4231 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_4231)

resultados_visit_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_4231
  )

resultados_visit_4231_d1

# Significativos al 5 %
significativos_visit_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4231_d1,
    nivel = 0.05
  )

significativos_visit_4231_d1

# Significativos o marginales al 10 %
significativos_10_visit_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4231_d1,
    nivel = 0.10
  )

significativos_10_visit_4231_d1


# =========================================================
# 2. FORMACIÓN VISITANTE DE REFERENCIA: 3421
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m3_visit_3421 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_3421)

resultados_visit_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_3421
  )

resultados_visit_3421_d1

# Significativos al 5 %
significativos_visit_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_3421_d1,
    nivel = 0.05
  )

significativos_visit_3421_d1

# Significativos o marginales al 10 %
significativos_10_visit_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_3421_d1,
    nivel = 0.10
  )

significativos_10_visit_3421_d1


# =========================================================
# 3. FORMACIÓN VISITANTE DE REFERENCIA: 343
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m3_visit_343 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_343)

resultados_visit_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_343
  )

resultados_visit_343_d1

# Significativos al 5 %
significativos_visit_343_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_343_d1,
    nivel = 0.05
  )

significativos_visit_343_d1

# Significativos o marginales al 10 %
significativos_10_visit_343_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_343_d1,
    nivel = 0.10
  )

significativos_10_visit_343_d1


# =========================================================
# 4. FORMACIÓN VISITANTE DE REFERENCIA: 352
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m3_visit_352 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_352)

resultados_visit_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_352
  )

resultados_visit_352_d1

# Significativos al 5 %
significativos_visit_352_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_352_d1,
    nivel = 0.05
  )

significativos_visit_352_d1

# Significativos o marginales al 10 %
significativos_10_visit_352_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_352_d1,
    nivel = 0.10
  )

significativos_10_visit_352_d1


# =========================================================
# 5. FORMACIÓN VISITANTE DE REFERENCIA: 4132
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m3_visit_4132 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_4132)

resultados_visit_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_4132
  )

resultados_visit_4132_d1

# Significativos al 5 %
significativos_visit_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4132_d1,
    nivel = 0.05
  )

significativos_visit_4132_d1

# Significativos o marginales al 10 %
significativos_10_visit_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4132_d1,
    nivel = 0.10
  )

significativos_10_visit_4132_d1


# =========================================================
# 6. FORMACIÓN VISITANTE DE REFERENCIA: 4141
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m3_visit_4141 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_4141)

resultados_visit_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_4141
  )

resultados_visit_4141_d1

# Significativos al 5 %
significativos_visit_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4141_d1,
    nivel = 0.05
  )

significativos_visit_4141_d1

# Significativos o marginales al 10 %
significativos_10_visit_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4141_d1,
    nivel = 0.10
  )

significativos_10_visit_4141_d1


# =========================================================
# 7. FORMACIÓN VISITANTE DE REFERENCIA: 4312
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m3_visit_4312 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_4312)

resultados_visit_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_4312
  )

resultados_visit_4312_d1

# Significativos al 5 %
significativos_visit_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4312_d1,
    nivel = 0.05
  )

significativos_visit_4312_d1

# Significativos o marginales al 10 %
significativos_10_visit_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4312_d1,
    nivel = 0.10
  )

significativos_10_visit_4312_d1


# =========================================================
# 8. FORMACIÓN VISITANTE DE REFERENCIA: 433
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m3_visit_433 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_433)

resultados_visit_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_433
  )

resultados_visit_433_d1

# Significativos al 5 %
significativos_visit_433_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_433_d1,
    nivel = 0.05
  )

significativos_visit_433_d1

# Significativos o marginales al 10 %
significativos_10_visit_433_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_433_d1,
    nivel = 0.10
  )

significativos_10_visit_433_d1


# =========================================================
# 9. FORMACIÓN VISITANTE DE REFERENCIA: 4411
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m3_visit_4411 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_4411)

resultados_visit_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_4411
  )

resultados_visit_4411_d1

# Significativos al 5 %
significativos_visit_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4411_d1,
    nivel = 0.05
  )

significativos_visit_4411_d1

# Significativos o marginales al 10 %
significativos_10_visit_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_4411_d1,
    nivel = 0.10
  )

significativos_10_visit_4411_d1


# =========================================================
# 10. FORMACIÓN VISITANTE DE REFERENCIA: 442
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m3_visit_442 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_442)

resultados_visit_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_442
  )

resultados_visit_442_d1

# Significativos al 5 %
significativos_visit_442_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_442_d1,
    nivel = 0.05
  )

significativos_visit_442_d1

# Significativos o marginales al 10 %
significativos_10_visit_442_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_442_d1,
    nivel = 0.10
  )

significativos_10_visit_442_d1


# =========================================================
# 11. FORMACIÓN VISITANTE DE REFERENCIA: 451
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m3_visit_451 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_451)

resultados_visit_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_451
  )

resultados_visit_451_d1

# Significativos al 5 %
significativos_visit_451_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_451_d1,
    nivel = 0.05
  )

significativos_visit_451_d1

# Significativos o marginales al 10 %
significativos_10_visit_451_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_451_d1,
    nivel = 0.10
  )

significativos_10_visit_451_d1


# =========================================================
# 12. FORMACIÓN VISITANTE DE REFERENCIA: 532
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m3_visit_532 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_532)

resultados_visit_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_532
  )

resultados_visit_532_d1

# Significativos al 5 %
significativos_visit_532_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_532_d1,
    nivel = 0.05
  )

significativos_visit_532_d1

# Significativos o marginales al 10 %
significativos_10_visit_532_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_532_d1,
    nivel = 0.10
  )

significativos_10_visit_532_d1


# =========================================================
# 13. FORMACIÓN VISITANTE DE REFERENCIA: 541
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m3_visit_541 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_541)

resultados_visit_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_541
  )

resultados_visit_541_d1

# Significativos al 5 %
significativos_visit_541_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_541_d1,
    nivel = 0.05
  )

significativos_visit_541_d1

# Significativos o marginales al 10 %
significativos_10_visit_541_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_541_d1,
    nivel = 0.10
  )

significativos_10_visit_541_d1


# =========================================================
# 14. FORMACIÓN VISITANTE DE REFERENCIA: OTRAS
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m3_visit_Otras <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m3_visit_Otras)

resultados_visit_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m3_visit_Otras
  )

resultados_visit_Otras_d1

# Significativos al 5 %
significativos_visit_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_Otras_d1,
    nivel = 0.05
  )

significativos_visit_Otras_d1

# Significativos o marginales al 10 %
significativos_10_visit_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_visit_Otras_d1,
    nivel = 0.10
  )

significativos_10_visit_Otras_d1

# =========================================================
# MATRIZ DE CONFUSIÓN
# MODELO: SOLO FORMACIÓN VISITANTE
# Modelo utilizado: m3_visit_4231
# =========================================================


# =========================================================
# 1. DATOS UTILIZADOS POR EL MODELO
# =========================================================

datos_formacion_visitante_d1 <- model.frame(
  formula = formula(m3_visit_4231),
  data = d1,
  na.action = na.omit
)

datos_formacion_visitante_d1 <- droplevels(
  datos_formacion_visitante_d1
)



# =========================================================
# 2. PREDICCIONES
# =========================================================

pred_formacion_visitante_d1 <- predict(
  m3_visit_4231,
  newdata = datos_formacion_visitante_d1,
  type = "class"
)

head(pred_formacion_visitante_d1)

table(
  pred_formacion_visitante_d1,
  useNA = "always"
)


# =========================================================
# 3. RESULTADOS REALES Y NIVELES
# =========================================================

niveles_resultado_formacion_visitante <- levels(
  d1$resultado_partido_local
)

real_formacion_visitante_d1 <- factor(
  datos_formacion_visitante_d1$resultado_partido_local,
  levels = niveles_resultado_formacion_visitante
)

pred_formacion_visitante_d1 <- factor(
  pred_formacion_visitante_d1,
  levels = niveles_resultado_formacion_visitante
)

# Comprobar que las longitudes coinciden
length(real_formacion_visitante_d1)
length(pred_formacion_visitante_d1)

length(real_formacion_visitante_d1) ==
  length(pred_formacion_visitante_d1)


# =========================================================
# 4. MATRIZ DE CONFUSIÓN
# =========================================================

mc_formacion_visitante_d1 <- table(
  Real = real_formacion_visitante_d1,
  Predicho = pred_formacion_visitante_d1
)

mc_formacion_visitante_d1

# Matriz con totales
addmargins(
  mc_formacion_visitante_d1
)


# =========================================================
# 5. ACCURACY
# =========================================================

accuracy_formacion_visitante_d1 <- sum(
  diag(mc_formacion_visitante_d1)
) / sum(mc_formacion_visitante_d1)

accuracy_formacion_visitante_d1


# =========================================================
# 6. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_formacion_visitante_d1 <- diag(
  mc_formacion_visitante_d1
) / rowSums(mc_formacion_visitante_d1)

sensibilidad_formacion_visitante_d1[
  is.nan(sensibilidad_formacion_visitante_d1) |
    is.infinite(sensibilidad_formacion_visitante_d1)
] <- NA

sensibilidad_formacion_visitante_d1


# =========================================================
# 7. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_formacion_visitante_d1 <- diag(
  mc_formacion_visitante_d1
) / colSums(mc_formacion_visitante_d1)

precision_formacion_visitante_d1[
  is.nan(precision_formacion_visitante_d1) |
    is.infinite(precision_formacion_visitante_d1)
] <- NA

precision_formacion_visitante_d1


# =========================================================
# 8. BALANCED ACCURACY
# =========================================================

balanced_accuracy_formacion_visitante_d1 <- mean(
  sensibilidad_formacion_visitante_d1,
  na.rm = TRUE
)

balanced_accuracy_formacion_visitante_d1


# =========================================================
# 9. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_formacion_visitante_d1 <- data.frame(
  Categoria = niveles_resultado_formacion_visitante,
  Sensibilidad = round(
    as.numeric(sensibilidad_formacion_visitante_d1),
    4
  ),
  Precision = round(
    as.numeric(precision_formacion_visitante_d1),
    4
  )
)

metricas_formacion_visitante_d1


# =========================================================
# 10. MÉTRICAS GENERALES
# =========================================================

metricas_generales_formacion_visitante_d1 <- data.frame(
  Modelo = "Solo formación visitante",
  Accuracy = round(
    accuracy_formacion_visitante_d1,
    4
  ),
  Balanced_Accuracy = round(
    balanced_accuracy_formacion_visitante_d1,
    4
  )
)

metricas_generales_formacion_visitante_d1
