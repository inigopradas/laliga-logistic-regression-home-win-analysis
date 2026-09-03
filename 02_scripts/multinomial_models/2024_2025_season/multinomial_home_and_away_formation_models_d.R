# ==============================================================================
# Title: Single-season multinomial models for home and visiting formations
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script analyses the association between tactical formations and match
# outcomes during the 2024/2025 LaLiga season. Separate multinomial logistic
# regression models are estimated for the formation used by the home team and
# the formation used by the visiting team.
#
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat. The draw is established as the reference outcome.
# Consequently, the estimated coefficients compare home victory versus draw
# and home defeat versus draw.
#
# Before estimation, the match-outcome and formation variables are cleaned by
# removing unnecessary spaces and literal quotation marks. The variables are
# subsequently converted into factors, and their levels and frequency
# distributions are checked.
#
# The first part estimates models containing only the home-team formation as
# the explanatory variable. The reference formation is changed successively
# across the retained tactical categories to obtain direct pairwise contrasts
# between formations.
#
# The second part estimates equivalent models containing only the visiting-team
# formation. The visiting reference category is also changed successively to
# obtain direct comparisons among the retained tactical systems.
#
# The formation categories used as references include 1-4-2-3-1, 1-3-4-3,
# 1-4-1-4-1, 1-4-3-3, 1-4-4-2, 1-5-3-2, 1-5-4-1 and the grouped category
# "Otras", when applicable.
#
# For each reference specification, the script extracts coefficient estimates,
# standard errors, odds ratios and bilateral Wald p-values. Results with
# statistical evidence at the 5% level and results that are significant or
# marginal at the 10% level are identified using previously defined extraction
# functions.
#
# The models using 1-4-2-3-1 as the reference formation are selected as the
# representative specifications for classification analysis. These models are
# used to calculate outcome probabilities and predicted match-result classes.
#
# Classification performance is evaluated using confusion matrices, overall
# accuracy, class-specific sensitivity, class-specific precision and balanced
# accuracy. The metrics are calculated separately for the home-formation model
# and the visiting-formation model.
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
# Principal reference formation:
#   1-4-2-3-1
#
# Statistical method:
#   Multinomial logistic regression estimated with nnet::multinom().
#
# Home-formation model specification:
#   resultado_partido_local ~ formacion_local_dep
#
# Visiting-formation model specification:
#   resultado_partido_local ~ formacion_visit_dep
#
# Home-formation models:
#   m_local_d_4231
#   m_local_d_343
#   m_local_d_4141
#   m_local_d_433
#   m_local_d_442
#   m_local_d_541
#   m_local_d_Otras
#
# Principal home-formation model:
#   m_local_d_4231
#
# Visiting-formation models:
#   m_visit_d_4231
#   m_visit_d_343
#   m_visit_d_4141
#   m_visit_d_433
#   m_visit_d_442
#   m_visit_d_532
#   m_visit_d_541
#   m_visit_d_Otras
#
# Principal visiting-formation model:
#   m_visit_d_4231
#
# Required functions:
#   extraer_resultados_formaciones_multinom()
#   extraer_significativos_formaciones()
#
# Main outputs:
#   Multinomial model summaries, coefficient tables, odds ratios, p-values,
#   statistically significant and marginal formation contrasts, estimated
#   outcome probabilities, predicted match-result classes, confusion matrices,
#   overall accuracy, balanced accuracy, class-specific sensitivity and
#   class-specific precision.
#
# Important methodological notes:
#   Changing the reference formation does not alter the fitted probabilities,
#   log-likelihood or overall fit of the model. It only changes the
#   parameterisation used to express the contrasts between tactical systems.
#
#   The home-only and visitor-only models estimate unadjusted associations
#   because each model contains only one formation variable. The home-formation
#   results are not adjusted for the visiting formation, and the visiting-
#   formation results are not adjusted for the home formation.
#
#   Classification metrics are calculated using the same observations employed
#   to estimate each model. They should therefore be interpreted as in-sample
#   descriptive performance rather than out-of-sample predictive performance.
#
#   The estimated associations should not be interpreted as causal effects of
#   tactical formations on match outcomes.
# ==============================================================================

# =========================================================


# =========================================================
# 0. PREPARACIÓN DE LAS VARIABLES
# =========================================================

# Limpiar posibles espacios en la variable dependiente
d$resultado_partido_local <- trimws(
  as.character(d$resultado_partido_local)
)

# Convertir el resultado en factor
d$resultado_partido_local <- factor(
  d$resultado_partido_local
)

# Establecer Empate como categoría de referencia
d$resultado_partido_local <- relevel(
  d$resultado_partido_local,
  ref = "Empate"
)


# Limpiar posibles espacios en la formación local
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


# Comprobar niveles
levels(d$resultado_partido_local)
levels(d$formacion_local_dep)


# Comprobar distribuciones
table(
  d$resultado_partido_local,
  useNA = "always"
)

table(
  d$formacion_local_dep,
  useNA = "always"
)


# =========================================================
# 1. FORMACIÓN LOCAL DE REFERENCIA: 4231
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-2-3-1"
)

m_local_d_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_local_d_4231)


# Resultados completos por nivel
resultados_local_d_4231 <-
  extraer_resultados_formaciones_multinom(
    m_local_d_4231
  )

resultados_local_d_4231


# Significativos al 5 %
significativos_local_d_4231 <-
  extraer_significativos_formaciones(
    resultados_local_d_4231,
    nivel = 0.05
  )

significativos_local_d_4231


# Significativos o marginales al 10 %
significativos_10_local_d_4231 <-
  extraer_significativos_formaciones(
    resultados_local_d_4231,
    nivel = 0.10
  )

significativos_10_local_d_4231


# =========================================================
# 2. FORMACIÓN LOCAL DE REFERENCIA: 343
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-3-4-3"
)

m_local_d_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_local_d_343)


# Resultados completos por nivel
resultados_local_d_343 <-
  extraer_resultados_formaciones_multinom(
    m_local_d_343
  )

resultados_local_d_343


# Significativos al 5 %
significativos_local_d_343 <-
  extraer_significativos_formaciones(
    resultados_local_d_343,
    nivel = 0.05
  )

significativos_local_d_343


# Significativos o marginales al 10 %
significativos_10_local_d_343 <-
  extraer_significativos_formaciones(
    resultados_local_d_343,
    nivel = 0.10
  )

significativos_10_local_d_343


# =========================================================
# 3. FORMACIÓN LOCAL DE REFERENCIA: 4141
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-1-4-1"
)

m_local_d_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_local_d_4141)


# Resultados completos por nivel
resultados_local_d_4141 <-
  extraer_resultados_formaciones_multinom(
    m_local_d_4141
  )

resultados_local_d_4141


# Significativos al 5 %
significativos_local_d_4141 <-
  extraer_significativos_formaciones(
    resultados_local_d_4141,
    nivel = 0.05
  )

significativos_local_d_4141


# Significativos o marginales al 10 %
significativos_10_local_d_4141 <-
  extraer_significativos_formaciones(
    resultados_local_d_4141,
    nivel = 0.10
  )

significativos_10_local_d_4141


# =========================================================
# 4. FORMACIÓN LOCAL DE REFERENCIA: 433
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-3-3"
)

m_local_d_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_local_d_433)


# Resultados completos por nivel
resultados_local_d_433 <-
  extraer_resultados_formaciones_multinom(
    m_local_d_433
  )

resultados_local_d_433


# Significativos al 5 %
significativos_local_d_433 <-
  extraer_significativos_formaciones(
    resultados_local_d_433,
    nivel = 0.05
  )

significativos_local_d_433


# Significativos o marginales al 10 %
significativos_10_local_d_433 <-
  extraer_significativos_formaciones(
    resultados_local_d_433,
    nivel = 0.10
  )

significativos_10_local_d_433


# =========================================================
# 5. FORMACIÓN LOCAL DE REFERENCIA: 442
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-4-4-2"
)

m_local_d_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_local_d_442)


# Resultados completos por nivel
resultados_local_d_442 <-
  extraer_resultados_formaciones_multinom(
    m_local_d_442
  )

resultados_local_d_442


# Significativos al 5 %
significativos_local_d_442 <-
  extraer_significativos_formaciones(
    resultados_local_d_442,
    nivel = 0.05
  )

significativos_local_d_442


# Significativos o marginales al 10 %
significativos_10_local_d_442 <-
  extraer_significativos_formaciones(
    resultados_local_d_442,
    nivel = 0.10
  )

significativos_10_local_d_442


# =========================================================
# 6. FORMACIÓN LOCAL DE REFERENCIA: 541
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "1-5-4-1"
)

m_local_d_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_local_d_541)


# Resultados completos por nivel
resultados_local_d_541 <-
  extraer_resultados_formaciones_multinom(
    m_local_d_541
  )

resultados_local_d_541


# Significativos al 5 %
significativos_local_d_541 <-
  extraer_significativos_formaciones(
    resultados_local_d_541,
    nivel = 0.05
  )

significativos_local_d_541


# Significativos o marginales al 10 %
significativos_10_local_d_541 <-
  extraer_significativos_formaciones(
    resultados_local_d_541,
    nivel = 0.10
  )

significativos_10_local_d_541


# =========================================================
# 7. FORMACIÓN LOCAL DE REFERENCIA: OTRAS
# =========================================================

d$formacion_local_dep <- relevel(
  factor(d$formacion_local_dep),
  ref = "Otras"
)

m_local_d_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_local_d_Otras)


# Resultados completos por nivel
resultados_local_d_Otras <-
  extraer_resultados_formaciones_multinom(
    m_local_d_Otras
  )

resultados_local_d_Otras


# Significativos al 5 %
significativos_local_d_Otras <-
  extraer_significativos_formaciones(
    resultados_local_d_Otras,
    nivel = 0.05
  )

significativos_local_d_Otras


# Significativos o marginales al 10 %
significativos_10_local_d_Otras <-
  extraer_significativos_formaciones(
    resultados_local_d_Otras,
    nivel = 0.10
  )

significativos_10_local_d_Otras

# =========================================================
# MATRIZ DE CONFUSIÓN
# MODELO: SOLO FORMACIÓN LOCAL
# Base de datos: d
# Modelo representativo: m_local_d_4231
# =========================================================


# =========================================================
# 1. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Extraer exactamente las observaciones utilizadas
datos_formacion_local_d <- model.frame(
  m_local_d_4231
)

# Eliminar posibles niveles no utilizados
datos_formacion_local_d <- droplevels(
  datos_formacion_local_d
)

# Comprobar el número de observaciones
nrow(datos_formacion_local_d)



# =========================================================
# 2. PROBABILIDADES ESTIMADAS
# =========================================================

prob_formacion_local_d <- predict(
  m_local_d_4231,
  newdata = datos_formacion_local_d,
  type = "probs"
)

# Primeras probabilidades
head(prob_formacion_local_d)

# Primeras probabilidades expresadas en porcentaje
round(
  head(prob_formacion_local_d * 100),
  2
)

# Número de observaciones con probabilidades
nrow(prob_formacion_local_d)

# Comprobar las primeras filas
rowSums(
  prob_formacion_local_d
)[1:10]

# Comprobar que todas las filas suman aproximadamente 1
all(
  abs(
    rowSums(prob_formacion_local_d) - 1
  ) < 1e-8
)


# =========================================================
# 3. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_formacion_local_d <- predict(
  m_local_d_4231,
  newdata = datos_formacion_local_d,
  type = "class"
)

# Primeras predicciones
head(pred_formacion_local_d)

# Distribución de resultados predichos
table(
  pred_formacion_local_d,
  useNA = "always"
)


# =========================================================
# 4. PREPARAR RESULTADOS REALES Y PREDICHOS
# =========================================================

# Niveles del resultado utilizados por el modelo
niveles_resultado_formacion_local_d <- levels(
  datos_formacion_local_d$resultado_partido_local
)

niveles_resultado_formacion_local_d


# Resultados reales
real_formacion_local_d <- factor(
  datos_formacion_local_d$resultado_partido_local,
  levels = niveles_resultado_formacion_local_d
)


# Resultados predichos con los mismos niveles
pred_formacion_local_d <- factor(
  pred_formacion_local_d,
  levels = niveles_resultado_formacion_local_d
)


# Comprobar las longitudes
length(real_formacion_local_d)

length(pred_formacion_local_d)

# Debe devolver TRUE
length(real_formacion_local_d) ==
  length(pred_formacion_local_d)


# =========================================================
# 5. MATRIZ DE CONFUSIÓN
# =========================================================

mc_formacion_local_d <- table(
  Real = real_formacion_local_d,
  Predicho = pred_formacion_local_d
)

mc_formacion_local_d


# Matriz con totales por filas y columnas
addmargins(
  mc_formacion_local_d
)


# =========================================================
# 6. ACCURACY TOTAL
# =========================================================

accuracy_formacion_local_d <- sum(
  diag(mc_formacion_local_d)
) / sum(mc_formacion_local_d)

accuracy_formacion_local_d


# =========================================================
# 7. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_formacion_local_d <- diag(
  mc_formacion_local_d
) / rowSums(mc_formacion_local_d)


# Evitar NaN o Inf
sensibilidad_formacion_local_d[
  is.nan(sensibilidad_formacion_local_d) |
    is.infinite(sensibilidad_formacion_local_d)
] <- NA

sensibilidad_formacion_local_d


# =========================================================
# 8. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_formacion_local_d <- diag(
  mc_formacion_local_d
) / colSums(mc_formacion_local_d)


# Evitar NaN o Inf si alguna categoría nunca se predice
precision_formacion_local_d[
  is.nan(precision_formacion_local_d) |
    is.infinite(precision_formacion_local_d)
] <- NA

precision_formacion_local_d


# =========================================================
# 9. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_formacion_local_d <- mean(
  sensibilidad_formacion_local_d,
  na.rm = TRUE
)

balanced_accuracy_formacion_local_d


# =========================================================
# 10. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_formacion_local_d <- data.frame(
  Categoria = niveles_resultado_formacion_local_d,
  
  Sensibilidad = round(
    as.numeric(
      sensibilidad_formacion_local_d
    ),
    4
  ),
  
  Precision = round(
    as.numeric(
      precision_formacion_local_d
    ),
    4
  )
)

metricas_formacion_local_d


# =========================================================
# 11. TABLA DE MÉTRICAS GENERALES
# =========================================================

metricas_generales_formacion_local_d <- data.frame(
  Modelo = "Solo formación local",
  
  Accuracy = round(
    accuracy_formacion_local_d,
    4
  ),
  
  Balanced_Accuracy = round(
    balanced_accuracy_formacion_local_d,
    4
  )
)

metricas_generales_formacion_local_d

#######################################################################
# =========================================================
# MODELOS MULTINOMIALES SEGÚN LA FORMACIÓN VISITANTE
# Base de datos: d
# Variable dependiente: resultado_partido_local
# Variable explicativa: formacion_visit_dep
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# 0. PREPARACIÓN DE LAS VARIABLES
# =========================================================

# Limpiar posibles espacios en la variable dependiente
d$resultado_partido_local <- trimws(
  as.character(d$resultado_partido_local)
)

# Convertir el resultado en factor
d$resultado_partido_local <- factor(
  d$resultado_partido_local
)

# Establecer Empate como categoría de referencia
d$resultado_partido_local <- relevel(
  d$resultado_partido_local,
  ref = "Empate"
)


# Limpiar posibles espacios en la formación visitante
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


# Comprobar los niveles
levels(d$resultado_partido_local)

levels(d$formacion_visit_dep)


# Comprobar las distribuciones
table(
  d$resultado_partido_local,
  useNA = "always"
)

table(
  d$formacion_visit_dep,
  useNA = "always"
)


# =========================================================
# 1. FORMACIÓN VISITANTE DE REFERENCIA: 4231
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m_visit_d_4231 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_4231)


# Resultados completos por nivel
resultados_visit_d_4231 <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_4231
  )

resultados_visit_d_4231


# Significativos al 5 %
significativos_visit_d_4231 <-
  extraer_significativos_formaciones(
    resultados_visit_d_4231,
    nivel = 0.05
  )

significativos_visit_d_4231


# Significativos o marginales al 10 %
significativos_10_visit_d_4231 <-
  extraer_significativos_formaciones(
    resultados_visit_d_4231,
    nivel = 0.10
  )

significativos_10_visit_d_4231


# =========================================================
# 2. FORMACIÓN VISITANTE DE REFERENCIA: 343
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-3-4-3"
)

m_visit_d_343 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_343)


# Resultados completos por nivel
resultados_visit_d_343 <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_343
  )

resultados_visit_d_343


# Significativos al 5 %
significativos_visit_d_343 <-
  extraer_significativos_formaciones(
    resultados_visit_d_343,
    nivel = 0.05
  )

significativos_visit_d_343


# Significativos o marginales al 10 %
significativos_10_visit_d_343 <-
  extraer_significativos_formaciones(
    resultados_visit_d_343,
    nivel = 0.10
  )

significativos_10_visit_d_343


# =========================================================
# 3. FORMACIÓN VISITANTE DE REFERENCIA: 4141
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m_visit_d_4141 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_4141)


# Resultados completos por nivel
resultados_visit_d_4141 <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_4141
  )

resultados_visit_d_4141


# Significativos al 5 %
significativos_visit_d_4141 <-
  extraer_significativos_formaciones(
    resultados_visit_d_4141,
    nivel = 0.05
  )

significativos_visit_d_4141


# Significativos o marginales al 10 %
significativos_10_visit_d_4141 <-
  extraer_significativos_formaciones(
    resultados_visit_d_4141,
    nivel = 0.10
  )

significativos_10_visit_d_4141


# =========================================================
# 4. FORMACIÓN VISITANTE DE REFERENCIA: 433
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-3-3"
)

m_visit_d_433 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_433)


# Resultados completos por nivel
resultados_visit_d_433 <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_433
  )

resultados_visit_d_433


# Significativos al 5 %
significativos_visit_d_433 <-
  extraer_significativos_formaciones(
    resultados_visit_d_433,
    nivel = 0.05
  )

significativos_visit_d_433


# Significativos o marginales al 10 %
significativos_10_visit_d_433 <-
  extraer_significativos_formaciones(
    resultados_visit_d_433,
    nivel = 0.10
  )

significativos_10_visit_d_433


# =========================================================
# 5. FORMACIÓN VISITANTE DE REFERENCIA: 442
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-4-4-2"
)

m_visit_d_442 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_442)


# Resultados completos por nivel
resultados_visit_d_442 <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_442
  )

resultados_visit_d_442


# Significativos al 5 %
significativos_visit_d_442 <-
  extraer_significativos_formaciones(
    resultados_visit_d_442,
    nivel = 0.05
  )

significativos_visit_d_442


# Significativos o marginales al 10 %
significativos_10_visit_d_442 <-
  extraer_significativos_formaciones(
    resultados_visit_d_442,
    nivel = 0.10
  )

significativos_10_visit_d_442


# =========================================================
# 6. FORMACIÓN VISITANTE DE REFERENCIA: 532
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-3-2"
)

m_visit_d_532 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_532)


# Resultados completos por nivel
resultados_visit_d_532 <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_532
  )

resultados_visit_d_532


# Significativos al 5 %
significativos_visit_d_532 <-
  extraer_significativos_formaciones(
    resultados_visit_d_532,
    nivel = 0.05
  )

significativos_visit_d_532


# Significativos o marginales al 10 %
significativos_10_visit_d_532 <-
  extraer_significativos_formaciones(
    resultados_visit_d_532,
    nivel = 0.10
  )

significativos_10_visit_d_532


# =========================================================
# 7. FORMACIÓN VISITANTE DE REFERENCIA: 541
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "1-5-4-1"
)

m_visit_d_541 <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_541)


# Resultados completos por nivel
resultados_visit_d_541 <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_541
  )

resultados_visit_d_541


# Significativos al 5 %
significativos_visit_d_541 <-
  extraer_significativos_formaciones(
    resultados_visit_d_541,
    nivel = 0.05
  )

significativos_visit_d_541


# Significativos o marginales al 10 %
significativos_10_visit_d_541 <-
  extraer_significativos_formaciones(
    resultados_visit_d_541,
    nivel = 0.10
  )

significativos_10_visit_d_541


# =========================================================
# 8. FORMACIÓN VISITANTE DE REFERENCIA: OTRAS
# =========================================================

d$formacion_visit_dep <- relevel(
  factor(d$formacion_visit_dep),
  ref = "Otras"
)

m_visit_d_Otras <- multinom(
  resultado_partido_local ~
    formacion_visit_dep,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m_visit_d_Otras)


# Resultados completos por nivel
resultados_visit_d_Otras <-
  extraer_resultados_formaciones_multinom(
    m_visit_d_Otras
  )

resultados_visit_d_Otras


# Significativos al 5 %
significativos_visit_d_Otras <-
  extraer_significativos_formaciones(
    resultados_visit_d_Otras,
    nivel = 0.05
  )

significativos_visit_d_Otras


# Significativos o marginales al 10 %
significativos_10_visit_d_Otras <-
  extraer_significativos_formaciones(
    resultados_visit_d_Otras,
    nivel = 0.10
  )

significativos_10_visit_d_Otras

# =========================================================
# MATRIZ DE CONFUSIÓN
# MODELO: SOLO FORMACIÓN VISITANTE
# Base de datos: d
# Modelo representativo: m_visit_d_4231
# =========================================================


# =========================================================
# 1. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Recuperar exactamente las observaciones usadas
# durante la estimación del modelo
datos_formacion_visitante_d <- model.frame(
  m_visit_d_4231
)

# Eliminar posibles niveles no utilizados
datos_formacion_visitante_d <- droplevels(
  datos_formacion_visitante_d
)

# Comprobar el número de observaciones
nrow(datos_formacion_visitante_d)


# =========================================================
# 2. PROBABILIDADES ESTIMADAS
# =========================================================

prob_formacion_visitante_d <- predict(
  m_visit_d_4231,
  newdata = datos_formacion_visitante_d,
  type = "probs"
)

# Primeras probabilidades en escala 0-1
head(prob_formacion_visitante_d)

# Primeras probabilidades en porcentaje
round(
  head(prob_formacion_visitante_d * 100),
  2
)

# Número de observaciones con probabilidades estimadas
nrow(prob_formacion_visitante_d)

# Comprobar las primeras sumas
rowSums(
  prob_formacion_visitante_d
)[1:10]

# Comprobar que todas las filas suman aproximadamente 1
all(
  abs(
    rowSums(prob_formacion_visitante_d) - 1
  ) < 1e-8
)


# =========================================================
# 3. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_formacion_visitante_d <- predict(
  m_visit_d_4231,
  newdata = datos_formacion_visitante_d,
  type = "class"
)

# Primeras predicciones
head(pred_formacion_visitante_d)

# Distribución de los resultados predichos
table(
  pred_formacion_visitante_d,
  useNA = "always"
)


# =========================================================
# 4. PREPARAR LOS RESULTADOS REALES Y PREDICHOS
# =========================================================

# Niveles de la variable dependiente utilizados por el modelo
niveles_resultado_formacion_visitante_d <- levels(
  datos_formacion_visitante_d$resultado_partido_local
)

niveles_resultado_formacion_visitante_d


# Resultados reales
real_formacion_visitante_d <- factor(
  datos_formacion_visitante_d$resultado_partido_local,
  levels = niveles_resultado_formacion_visitante_d
)


# Resultados predichos con los mismos niveles
pred_formacion_visitante_d <- factor(
  pred_formacion_visitante_d,
  levels = niveles_resultado_formacion_visitante_d
)


# Comprobar las longitudes
length(real_formacion_visitante_d)

length(pred_formacion_visitante_d)

# Debe devolver TRUE
length(real_formacion_visitante_d) ==
  length(pred_formacion_visitante_d)


# =========================================================
# 5. MATRIZ DE CONFUSIÓN
# =========================================================

mc_formacion_visitante_d <- table(
  Real = real_formacion_visitante_d,
  Predicho = pred_formacion_visitante_d
)

mc_formacion_visitante_d


# Matriz con totales por filas y columnas
addmargins(
  mc_formacion_visitante_d
)


# =========================================================
# 6. ACCURACY TOTAL
# =========================================================

accuracy_formacion_visitante_d <- sum(
  diag(mc_formacion_visitante_d)
) / sum(mc_formacion_visitante_d)

accuracy_formacion_visitante_d


# =========================================================
# 7. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_formacion_visitante_d <- diag(
  mc_formacion_visitante_d
) / rowSums(mc_formacion_visitante_d)


# Evitar NaN o Inf
sensibilidad_formacion_visitante_d[
  is.nan(sensibilidad_formacion_visitante_d) |
    is.infinite(sensibilidad_formacion_visitante_d)
] <- NA

sensibilidad_formacion_visitante_d


# =========================================================
# 8. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_formacion_visitante_d <- diag(
  mc_formacion_visitante_d
) / colSums(mc_formacion_visitante_d)


# Evitar NaN o Inf si una categoría nunca se predice
precision_formacion_visitante_d[
  is.nan(precision_formacion_visitante_d) |
    is.infinite(precision_formacion_visitante_d)
] <- NA

precision_formacion_visitante_d


# =========================================================
# 9. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_formacion_visitante_d <- mean(
  sensibilidad_formacion_visitante_d,
  na.rm = TRUE
)

balanced_accuracy_formacion_visitante_d


# =========================================================
# 10. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_formacion_visitante_d <- data.frame(
  Categoria = niveles_resultado_formacion_visitante_d,
  
  Sensibilidad = round(
    as.numeric(
      sensibilidad_formacion_visitante_d
    ),
    4
  ),
  
  Precision = round(
    as.numeric(
      precision_formacion_visitante_d
    ),
    4
  )
)

metricas_formacion_visitante_d


# =========================================================
# 11. TABLA DE MÉTRICAS GENERALES
# =========================================================

metricas_generales_formacion_visitante_d <- data.frame(
  Modelo = "Solo formación visitante",
  
  Accuracy = round(
    accuracy_formacion_visitante_d,
    4
  ),
  
  Balanced_Accuracy = round(
    balanced_accuracy_formacion_visitante_d,
    4
  )
)

metricas_generales_formacion_visitante_d

