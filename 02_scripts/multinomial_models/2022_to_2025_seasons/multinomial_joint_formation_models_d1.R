# ==============================================================================
# Title: Multi-season joint multinomial formation models, Part 1
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script estimates multinomial logistic regression models to analyse the
# association between the tactical formations used by the home and visiting
# teams and the final match outcome across the 2022/2023, 2023/2024 and
# 2024/2025 LaLiga seasons.
#
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat. The draw is used as the reference outcome. Therefore, the
# model coefficients compare home victory versus draw and home defeat versus
# draw.
#
# The explanatory variables are the tactical formations used by the home and
# visiting teams. Both variables are included simultaneously as additive
# categorical predictors. No interaction term is included between the two
# formation variables.
#
# Before model estimation, formation labels are cleaned by removing quotation
# marks and unnecessary spaces. The formation variables are converted into
# factors, and tactical systems represented by fewer than ten observations are
# grouped into the residual category "Otras".
#
# A helper function is defined to extract statistically relevant formation
# coefficients from the complete result tables. By default, the function
# excludes the intercept and retains results whose original p-value is below a
# specified significance threshold.
#
# The reference categories of the home and visiting formation variables are
# changed systematically. This reparameterisation provides direct coefficient
# contrasts between the retained tactical systems while preserving the fitted
# probabilities, likelihood and overall fit of the underlying multinomial
# model.
#
# The complete analysis uses each retained home formation as the reference
# category in turn. For every home reference formation, the visiting reference
# category is also changed successively across all the retained formations.
# This procedure provides the complete set of pairwise coefficient contrasts
# for the home and visiting formation variables.
#
# For every reference specification, the script extracts the model
# coefficients, standard errors, Wald statistics, bilateral p-values and
# odds ratios. Results are reported using both the conventional 5% threshold
# and the broader 10% threshold for marginal statistical evidence.
#
# After estimating the complete series of reference specifications, the model
# using "Otras" as the reference category for both formation variables is used
# to calculate predicted outcome classes and classification-performance
# measures.
#
# The final section evaluates the representative multinomial model through a
# confusion matrix, overall accuracy, class-specific sensitivity,
# class-specific precision and balanced accuracy.
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
# Treatment of infrequent formations:
#   Home and visiting formation categories represented by fewer than ten
#   observations are grouped separately into the category "Otras".
#
# Statistical method:
#   Multinomial logistic regression estimated with nnet::multinom().
#
# Model specification:
#   resultado_partido_local ~
#     formacion_local_dep +
#     formacion_visit_dep
#
# Interaction structure:
#   No interaction is included between the home and visiting formations.
#
# Home reference formations examined in the complete analysis:
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
# Visiting reference formations examined for every home reference:
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
# Total reference specifications:
#   The complete procedure considers 14 home reference formations and
#   14 visiting reference formations, producing 196 reference-category
#   specifications of the same additive multinomial model.
#
# Required packages:
#   readxl
#   dplyr
#   nnet
#
# Required functions:
#   extraer_resultados_formaciones_multinom()
#   extraer_significativos_formaciones()
#
# Representative model used for classification:
#   m1_Otras_Otras
#
# Main outputs:
#   Multinomial regression summaries, coefficient tables, odds ratios,
#   p-values, statistically significant and marginal formation contrasts,
#   predicted outcome classes, a confusion matrix, overall accuracy,
#   class-specific sensitivity, class-specific precision and balanced
#   accuracy.
#
# Important methodological notes:
#   Changing the reference categories does not produce a genuinely different
#   statistical model. It only changes the parameterisation used to express
#   the same additive relationships among formations and match outcomes.
#
#   Accordingly, the 196 reference-category specifications should not be
#   interpreted as 196 independent models. They are alternative
#   parameterisations of the same additive multinomial specification.
#
#   Because no interaction term is included, the models estimate adjusted main
#   effects of home and visiting formations. They do not estimate a specific
#   tactical effect for each individual home-versus-visiting formation pairing.
#
#   A conditional effect associated with a particular tactical matchup would
#   require an interaction term between formacion_local_dep and
#   formacion_visit_dep.
#
#   The classification metrics are calculated using the same observations
#   employed to estimate the model. They should therefore be interpreted as
#   in-sample descriptive performance rather than out-of-sample predictive
#   performance.
# ==============================================================================


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
# FUNCIÓN PARA EXTRAER COEFICIENTES SIGNIFICATIVOS
# =========================================================

extraer_significativos_formaciones <- function(
    tabla_resultados,
    nivel = 0.05,
    incluir_intercepto = FALSE) {
  
  # Comprobar que la tabla contiene la columna necesaria
  if (!"P_valor_original" %in% names(tabla_resultados)) {
    stop(
      paste0(
        "La tabla debe contener la columna ",
        "'P_valor_original'."
      )
    )
  }
  
  # Seleccionar resultados con p-valor válido
  resultado <- tabla_resultados %>%
    filter(
      !is.na(P_valor_original)
    ) %>%
    filter(
      P_valor_original < nivel
    )
  
  # Excluir el intercepto por defecto
  if (!incluir_intercepto) {
    
    resultado <- resultado %>%
      filter(
        Variable != "(Intercept)"
      )
  }
  
  # Ordenar por comparación y p-valor
  resultado <- resultado %>%
    arrange(
      Comparacion,
      P_valor_original
    )
  
  return(resultado)
}
# =========================================================
# 5. 4231 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-2-3-1"
)

m1_4231_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_4231)

resultados_4231_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_4231
  )

resultados_4231_4231_d1

# Significativos al 5 %
significativos_4231_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4231_d1,
    nivel = 0.05
  )

significativos_4231_4231_d1

# Significativos o marginales al 10 %
significativos_10_4231_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4231_d1,
    nivel = 0.10
  )

significativos_10_4231_4231_d1


# =========================================================
# 6. 4231 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-3-4-2-1"
)

m1_4231_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_3421)

resultados_4231_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_3421
  )

resultados_4231_3421_d1

significativos_4231_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_3421_d1,
    nivel = 0.05
  )

significativos_4231_3421_d1

significativos_10_4231_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_3421_d1,
    nivel = 0.10
  )

significativos_10_4231_3421_d1


# =========================================================
# 7. 4231 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-3-4-3"
)

m1_4231_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_343)

resultados_4231_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_343
  )

resultados_4231_343_d1

significativos_4231_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_343_d1,
    nivel = 0.05
  )

significativos_4231_343_d1

significativos_10_4231_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_343_d1,
    nivel = 0.10
  )

significativos_10_4231_343_d1


# =========================================================
# 8. 4231 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-3-5-2"
)

m1_4231_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_352)

resultados_4231_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_352
  )

resultados_4231_352_d1

significativos_4231_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_352_d1,
    nivel = 0.05
  )

significativos_4231_352_d1

significativos_10_4231_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_352_d1,
    nivel = 0.10
  )

significativos_10_4231_352_d1


# =========================================================
# 9. 4231 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-1-3-2"
)

m1_4231_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_4132)

resultados_4231_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_4132
  )

resultados_4231_4132_d1

significativos_4231_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4132_d1,
    nivel = 0.05
  )

significativos_4231_4132_d1

significativos_10_4231_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4132_d1,
    nivel = 0.10
  )

significativos_10_4231_4132_d1


# =========================================================
# 10. 4231 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-1-4-1"
)

m1_4231_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_4141)

resultados_4231_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_4141
  )

resultados_4231_4141_d1

significativos_4231_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4141_d1,
    nivel = 0.05
  )

significativos_4231_4141_d1

significativos_10_4231_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4141_d1,
    nivel = 0.10
  )

significativos_10_4231_4141_d1


# =========================================================
# 11. 4231 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-3-1-2"
)

m1_4231_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_4312)

resultados_4231_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_4312
  )

resultados_4231_4312_d1

significativos_4231_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4312_d1,
    nivel = 0.05
  )

significativos_4231_4312_d1

significativos_10_4231_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4312_d1,
    nivel = 0.10
  )

significativos_10_4231_4312_d1


# =========================================================
# 12. 4231 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-3-3"
)

m1_4231_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_433)

resultados_4231_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_433
  )

resultados_4231_433_d1

significativos_4231_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_433_d1,
    nivel = 0.05
  )

significativos_4231_433_d1

significativos_10_4231_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_433_d1,
    nivel = 0.10
  )

significativos_10_4231_433_d1


# =========================================================
# 13. 4231 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-4-1-1"
)

m1_4231_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_4411)

resultados_4231_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_4411
  )

resultados_4231_4411_d1

significativos_4231_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4411_d1,
    nivel = 0.05
  )

significativos_4231_4411_d1

significativos_10_4231_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_4411_d1,
    nivel = 0.10
  )

significativos_10_4231_4411_d1


# =========================================================
# 14. 4231 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-4-2"
)

m1_4231_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_442)

resultados_4231_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_442
  )

resultados_4231_442_d1

significativos_4231_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_442_d1,
    nivel = 0.05
  )

significativos_4231_442_d1

significativos_10_4231_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_442_d1,
    nivel = 0.10
  )

significativos_10_4231_442_d1


# =========================================================
# 15. 4231 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-4-5-1"
)

m1_4231_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_451)

resultados_4231_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_451
  )

resultados_4231_451_d1

significativos_4231_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_451_d1,
    nivel = 0.05
  )

significativos_4231_451_d1

significativos_10_4231_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_451_d1,
    nivel = 0.10
  )

significativos_10_4231_451_d1


# =========================================================
# 16. 4231 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-5-3-2"
)

m1_4231_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_532)

resultados_4231_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_532
  )

resultados_4231_532_d1

significativos_4231_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_532_d1,
    nivel = 0.05
  )

significativos_4231_532_d1

significativos_10_4231_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_532_d1,
    nivel = 0.10
  )

significativos_10_4231_532_d1


# =========================================================
# 17. 4231 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "1-5-4-1"
)

m1_4231_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_541)

resultados_4231_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_541
  )

resultados_4231_541_d1

significativos_4231_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_541_d1,
    nivel = 0.05
  )

significativos_4231_541_d1

significativos_10_4231_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_541_d1,
    nivel = 0.10
  )

significativos_10_4231_541_d1


# =========================================================
# 18. 4231 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  d1$formacion_visit_dep,
  ref = "Otras"
)

m1_4231_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4231_Otras)

resultados_4231_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4231_Otras
  )

resultados_4231_Otras_d1

significativos_4231_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_Otras_d1,
    nivel = 0.05
  )

significativos_4231_Otras_d1

significativos_10_4231_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4231_Otras_d1,
    nivel = 0.10
  )

significativos_10_4231_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-3-4-2-1
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-3-4-2-1"
)


# =========================================================
# 1. 3421 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_3421_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_4231)

resultados_3421_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_4231
  )

resultados_3421_4231_d1

# Significativos al 5 %
significativos_3421_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4231_d1,
    nivel = 0.05
  )

significativos_3421_4231_d1

# Significativos o marginales al 10 %
significativos_10_3421_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4231_d1,
    nivel = 0.10
  )

significativos_10_3421_4231_d1


# =========================================================
# 2. 3421 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_3421_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_3421)

resultados_3421_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_3421
  )

resultados_3421_3421_d1

# Significativos al 5 %
significativos_3421_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_3421_d1,
    nivel = 0.05
  )

significativos_3421_3421_d1

# Significativos o marginales al 10 %
significativos_10_3421_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_3421_d1,
    nivel = 0.10
  )

significativos_10_3421_3421_d1


# =========================================================
# 3. 3421 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_3421_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_343)

resultados_3421_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_343
  )

resultados_3421_343_d1

# Significativos al 5 %
significativos_3421_343_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_343_d1,
    nivel = 0.05
  )

significativos_3421_343_d1

# Significativos o marginales al 10 %
significativos_10_3421_343_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_343_d1,
    nivel = 0.10
  )

significativos_10_3421_343_d1


# =========================================================
# 4. 3421 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_3421_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_352)

resultados_3421_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_352
  )

resultados_3421_352_d1

# Significativos al 5 %
significativos_3421_352_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_352_d1,
    nivel = 0.05
  )

significativos_3421_352_d1

# Significativos o marginales al 10 %
significativos_10_3421_352_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_352_d1,
    nivel = 0.10
  )

significativos_10_3421_352_d1


# =========================================================
# 5. 3421 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_3421_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_4132)

resultados_3421_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_4132
  )

resultados_3421_4132_d1

# Significativos al 5 %
significativos_3421_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4132_d1,
    nivel = 0.05
  )

significativos_3421_4132_d1

# Significativos o marginales al 10 %
significativos_10_3421_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4132_d1,
    nivel = 0.10
  )

significativos_10_3421_4132_d1


# =========================================================
# 6. 3421 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_3421_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_4141)

resultados_3421_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_4141
  )

resultados_3421_4141_d1

# Significativos al 5 %
significativos_3421_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4141_d1,
    nivel = 0.05
  )

significativos_3421_4141_d1

# Significativos o marginales al 10 %
significativos_10_3421_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4141_d1,
    nivel = 0.10
  )

significativos_10_3421_4141_d1


# =========================================================
# 7. 3421 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_3421_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_4312)

resultados_3421_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_4312
  )

resultados_3421_4312_d1

# Significativos al 5 %
significativos_3421_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4312_d1,
    nivel = 0.05
  )

significativos_3421_4312_d1

# Significativos o marginales al 10 %
significativos_10_3421_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4312_d1,
    nivel = 0.10
  )

significativos_10_3421_4312_d1


# =========================================================
# 8. 3421 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_3421_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_433)

resultados_3421_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_433
  )

resultados_3421_433_d1

# Significativos al 5 %
significativos_3421_433_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_433_d1,
    nivel = 0.05
  )

significativos_3421_433_d1

# Significativos o marginales al 10 %
significativos_10_3421_433_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_433_d1,
    nivel = 0.10
  )

significativos_10_3421_433_d1


# =========================================================
# 9. 3421 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_3421_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_4411)

resultados_3421_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_4411
  )

resultados_3421_4411_d1

# Significativos al 5 %
significativos_3421_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4411_d1,
    nivel = 0.05
  )

significativos_3421_4411_d1

# Significativos o marginales al 10 %
significativos_10_3421_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_4411_d1,
    nivel = 0.10
  )

significativos_10_3421_4411_d1


# =========================================================
# 10. 3421 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_3421_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_442)

resultados_3421_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_442
  )

resultados_3421_442_d1

# Significativos al 5 %
significativos_3421_442_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_442_d1,
    nivel = 0.05
  )

significativos_3421_442_d1

# Significativos o marginales al 10 %
significativos_10_3421_442_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_442_d1,
    nivel = 0.10
  )

significativos_10_3421_442_d1


# =========================================================
# 11. 3421 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_3421_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_451)

resultados_3421_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_451
  )

resultados_3421_451_d1

# Significativos al 5 %
significativos_3421_451_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_451_d1,
    nivel = 0.05
  )

significativos_3421_451_d1

# Significativos o marginales al 10 %
significativos_10_3421_451_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_451_d1,
    nivel = 0.10
  )

significativos_10_3421_451_d1


# =========================================================
# 12. 3421 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_3421_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_532)

resultados_3421_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_532
  )

resultados_3421_532_d1

# Significativos al 5 %
significativos_3421_532_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_532_d1,
    nivel = 0.05
  )

significativos_3421_532_d1

# Significativos o marginales al 10 %
significativos_10_3421_532_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_532_d1,
    nivel = 0.10
  )

significativos_10_3421_532_d1


# =========================================================
# 13. 3421 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_3421_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_541)

resultados_3421_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_541
  )

resultados_3421_541_d1

# Significativos al 5 %
significativos_3421_541_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_541_d1,
    nivel = 0.05
  )

significativos_3421_541_d1

# Significativos o marginales al 10 %
significativos_10_3421_541_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_541_d1,
    nivel = 0.10
  )

significativos_10_3421_541_d1


# =========================================================
# 14. 3421 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_3421_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_3421_Otras)

resultados_3421_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_3421_Otras
  )

resultados_3421_Otras_d1

# Significativos al 5 %
significativos_3421_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_Otras_d1,
    nivel = 0.05
  )

significativos_3421_Otras_d1

# Significativos o marginales al 10 %
significativos_10_3421_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_3421_Otras_d1,
    nivel = 0.10
  )

significativos_10_3421_Otras_d1

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-3-4-3
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-3-4-3"
)


# =========================================================
# 1. 343 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_343_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_4231)

resultados_343_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_4231
  )

resultados_343_4231_d1

# Significativos al 5 %
significativos_343_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4231_d1,
    nivel = 0.05
  )

significativos_343_4231_d1

# Significativos o marginales al 10 %
significativos_10_343_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4231_d1,
    nivel = 0.10
  )

significativos_10_343_4231_d1


# =========================================================
# 2. 343 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_343_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_3421)

resultados_343_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_3421
  )

resultados_343_3421_d1

# Significativos al 5 %
significativos_343_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_343_3421_d1,
    nivel = 0.05
  )

significativos_343_3421_d1

# Significativos o marginales al 10 %
significativos_10_343_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_343_3421_d1,
    nivel = 0.10
  )

significativos_10_343_3421_d1


# =========================================================
# 3. 343 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_343_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_343)

resultados_343_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_343
  )

resultados_343_343_d1

# Significativos al 5 %
significativos_343_343_d1 <-
  extraer_significativos_formaciones(
    resultados_343_343_d1,
    nivel = 0.05
  )

significativos_343_343_d1

# Significativos o marginales al 10 %
significativos_10_343_343_d1 <-
  extraer_significativos_formaciones(
    resultados_343_343_d1,
    nivel = 0.10
  )

significativos_10_343_343_d1


# =========================================================
# 4. 343 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_343_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_352)

resultados_343_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_352
  )

resultados_343_352_d1

# Significativos al 5 %
significativos_343_352_d1 <-
  extraer_significativos_formaciones(
    resultados_343_352_d1,
    nivel = 0.05
  )

significativos_343_352_d1

# Significativos o marginales al 10 %
significativos_10_343_352_d1 <-
  extraer_significativos_formaciones(
    resultados_343_352_d1,
    nivel = 0.10
  )

significativos_10_343_352_d1


# =========================================================
# 5. 343 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_343_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_4132)

resultados_343_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_4132
  )

resultados_343_4132_d1

# Significativos al 5 %
significativos_343_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4132_d1,
    nivel = 0.05
  )

significativos_343_4132_d1

# Significativos o marginales al 10 %
significativos_10_343_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4132_d1,
    nivel = 0.10
  )

significativos_10_343_4132_d1


# =========================================================
# 6. 343 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_343_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_4141)

resultados_343_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_4141
  )

resultados_343_4141_d1

# Significativos al 5 %
significativos_343_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4141_d1,
    nivel = 0.05
  )

significativos_343_4141_d1

# Significativos o marginales al 10 %
significativos_10_343_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4141_d1,
    nivel = 0.10
  )

significativos_10_343_4141_d1


# =========================================================
# 7. 343 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_343_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_4312)

resultados_343_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_4312
  )

resultados_343_4312_d1

# Significativos al 5 %
significativos_343_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4312_d1,
    nivel = 0.05
  )

significativos_343_4312_d1

# Significativos o marginales al 10 %
significativos_10_343_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4312_d1,
    nivel = 0.10
  )

significativos_10_343_4312_d1


# =========================================================
# 8. 343 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_343_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_433)

resultados_343_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_433
  )

resultados_343_433_d1

# Significativos al 5 %
significativos_343_433_d1 <-
  extraer_significativos_formaciones(
    resultados_343_433_d1,
    nivel = 0.05
  )

significativos_343_433_d1

# Significativos o marginales al 10 %
significativos_10_343_433_d1 <-
  extraer_significativos_formaciones(
    resultados_343_433_d1,
    nivel = 0.10
  )

significativos_10_343_433_d1


# =========================================================
# 9. 343 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_343_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_4411)

resultados_343_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_4411
  )

resultados_343_4411_d1

# Significativos al 5 %
significativos_343_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4411_d1,
    nivel = 0.05
  )

significativos_343_4411_d1

# Significativos o marginales al 10 %
significativos_10_343_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_343_4411_d1,
    nivel = 0.10
  )

significativos_10_343_4411_d1


# =========================================================
# 10. 343 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_343_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_442)

resultados_343_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_442
  )

resultados_343_442_d1

# Significativos al 5 %
significativos_343_442_d1 <-
  extraer_significativos_formaciones(
    resultados_343_442_d1,
    nivel = 0.05
  )

significativos_343_442_d1

# Significativos o marginales al 10 %
significativos_10_343_442_d1 <-
  extraer_significativos_formaciones(
    resultados_343_442_d1,
    nivel = 0.10
  )

significativos_10_343_442_d1


# =========================================================
# 11. 343 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_343_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_451)

resultados_343_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_451
  )

resultados_343_451_d1

# Significativos al 5 %
significativos_343_451_d1 <-
  extraer_significativos_formaciones(
    resultados_343_451_d1,
    nivel = 0.05
  )

significativos_343_451_d1

# Significativos o marginales al 10 %
significativos_10_343_451_d1 <-
  extraer_significativos_formaciones(
    resultados_343_451_d1,
    nivel = 0.10
  )

significativos_10_343_451_d1


# =========================================================
# 12. 343 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_343_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_532)

resultados_343_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_532
  )

resultados_343_532_d1

# Significativos al 5 %
significativos_343_532_d1 <-
  extraer_significativos_formaciones(
    resultados_343_532_d1,
    nivel = 0.05
  )

significativos_343_532_d1

# Significativos o marginales al 10 %
significativos_10_343_532_d1 <-
  extraer_significativos_formaciones(
    resultados_343_532_d1,
    nivel = 0.10
  )

significativos_10_343_532_d1


# =========================================================
# 13. 343 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_343_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_541)

resultados_343_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_541
  )

resultados_343_541_d1

# Significativos al 5 %
significativos_343_541_d1 <-
  extraer_significativos_formaciones(
    resultados_343_541_d1,
    nivel = 0.05
  )

significativos_343_541_d1

# Significativos o marginales al 10 %
significativos_10_343_541_d1 <-
  extraer_significativos_formaciones(
    resultados_343_541_d1,
    nivel = 0.10
  )

significativos_10_343_541_d1


# =========================================================
# 14. 343 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_343_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_343_Otras)

resultados_343_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_343_Otras
  )

resultados_343_Otras_d1

# Significativos al 5 %
significativos_343_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_343_Otras_d1,
    nivel = 0.05
  )

significativos_343_Otras_d1

# Significativos o marginales al 10 %
significativos_10_343_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_343_Otras_d1,
    nivel = 0.10
  )

significativos_10_343_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-3-5-2
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-3-5-2"
)


# =========================================================
# 1. 352 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_352_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_4231)

resultados_352_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_4231
  )

resultados_352_4231_d1

# Significativos al 5 %
significativos_352_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4231_d1,
    nivel = 0.05
  )

significativos_352_4231_d1

# Significativos o marginales al 10 %
significativos_10_352_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4231_d1,
    nivel = 0.10
  )

significativos_10_352_4231_d1


# =========================================================
# 2. 352 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_352_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_3421)

resultados_352_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_3421
  )

resultados_352_3421_d1

# Significativos al 5 %
significativos_352_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_352_3421_d1,
    nivel = 0.05
  )

significativos_352_3421_d1

# Significativos o marginales al 10 %
significativos_10_352_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_352_3421_d1,
    nivel = 0.10
  )

significativos_10_352_3421_d1


# =========================================================
# 3. 352 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_352_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_343)

resultados_352_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_343
  )

resultados_352_343_d1

# Significativos al 5 %
significativos_352_343_d1 <-
  extraer_significativos_formaciones(
    resultados_352_343_d1,
    nivel = 0.05
  )

significativos_352_343_d1

# Significativos o marginales al 10 %
significativos_10_352_343_d1 <-
  extraer_significativos_formaciones(
    resultados_352_343_d1,
    nivel = 0.10
  )

significativos_10_352_343_d1


# =========================================================
# 4. 352 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_352_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_352)

resultados_352_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_352
  )

resultados_352_352_d1

# Significativos al 5 %
significativos_352_352_d1 <-
  extraer_significativos_formaciones(
    resultados_352_352_d1,
    nivel = 0.05
  )

significativos_352_352_d1

# Significativos o marginales al 10 %
significativos_10_352_352_d1 <-
  extraer_significativos_formaciones(
    resultados_352_352_d1,
    nivel = 0.10
  )

significativos_10_352_352_d1


# =========================================================
# 5. 352 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_352_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_4132)

resultados_352_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_4132
  )

resultados_352_4132_d1

# Significativos al 5 %
significativos_352_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4132_d1,
    nivel = 0.05
  )

significativos_352_4132_d1

# Significativos o marginales al 10 %
significativos_10_352_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4132_d1,
    nivel = 0.10
  )

significativos_10_352_4132_d1


# =========================================================
# 6. 352 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_352_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_4141)

resultados_352_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_4141
  )

resultados_352_4141_d1

# Significativos al 5 %
significativos_352_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4141_d1,
    nivel = 0.05
  )

significativos_352_4141_d1

# Significativos o marginales al 10 %
significativos_10_352_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4141_d1,
    nivel = 0.10
  )

significativos_10_352_4141_d1


# =========================================================
# 7. 352 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_352_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_4312)

resultados_352_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_4312
  )

resultados_352_4312_d1

# Significativos al 5 %
significativos_352_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4312_d1,
    nivel = 0.05
  )

significativos_352_4312_d1

# Significativos o marginales al 10 %
significativos_10_352_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4312_d1,
    nivel = 0.10
  )

significativos_10_352_4312_d1


# =========================================================
# 8. 352 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_352_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_433)

resultados_352_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_433
  )

resultados_352_433_d1

# Significativos al 5 %
significativos_352_433_d1 <-
  extraer_significativos_formaciones(
    resultados_352_433_d1,
    nivel = 0.05
  )

significativos_352_433_d1

# Significativos o marginales al 10 %
significativos_10_352_433_d1 <-
  extraer_significativos_formaciones(
    resultados_352_433_d1,
    nivel = 0.10
  )

significativos_10_352_433_d1


# =========================================================
# 9. 352 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_352_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_4411)

resultados_352_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_4411
  )

resultados_352_4411_d1

# Significativos al 5 %
significativos_352_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4411_d1,
    nivel = 0.05
  )

significativos_352_4411_d1

# Significativos o marginales al 10 %
significativos_10_352_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_352_4411_d1,
    nivel = 0.10
  )

significativos_10_352_4411_d1


# =========================================================
# 10. 352 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_352_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_442)

resultados_352_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_442
  )

resultados_352_442_d1

# Significativos al 5 %
significativos_352_442_d1 <-
  extraer_significativos_formaciones(
    resultados_352_442_d1,
    nivel = 0.05
  )

significativos_352_442_d1

# Significativos o marginales al 10 %
significativos_10_352_442_d1 <-
  extraer_significativos_formaciones(
    resultados_352_442_d1,
    nivel = 0.10
  )

significativos_10_352_442_d1


# =========================================================
# 11. 352 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_352_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_451)

resultados_352_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_451
  )

resultados_352_451_d1

# Significativos al 5 %
significativos_352_451_d1 <-
  extraer_significativos_formaciones(
    resultados_352_451_d1,
    nivel = 0.05
  )

significativos_352_451_d1

# Significativos o marginales al 10 %
significativos_10_352_451_d1 <-
  extraer_significativos_formaciones(
    resultados_352_451_d1,
    nivel = 0.10
  )

significativos_10_352_451_d1


# =========================================================
# 12. 352 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_352_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_532)

resultados_352_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_532
  )

resultados_352_532_d1

# Significativos al 5 %
significativos_352_532_d1 <-
  extraer_significativos_formaciones(
    resultados_352_532_d1,
    nivel = 0.05
  )

significativos_352_532_d1

# Significativos o marginales al 10 %
significativos_10_352_532_d1 <-
  extraer_significativos_formaciones(
    resultados_352_532_d1,
    nivel = 0.10
  )

significativos_10_352_532_d1


# =========================================================
# 13. 352 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_352_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_541)

resultados_352_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_541
  )

resultados_352_541_d1

# Significativos al 5 %
significativos_352_541_d1 <-
  extraer_significativos_formaciones(
    resultados_352_541_d1,
    nivel = 0.05
  )

significativos_352_541_d1

# Significativos o marginales al 10 %
significativos_10_352_541_d1 <-
  extraer_significativos_formaciones(
    resultados_352_541_d1,
    nivel = 0.10
  )

significativos_10_352_541_d1


# =========================================================
# 14. 352 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_352_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_352_Otras)

resultados_352_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_352_Otras
  )

resultados_352_Otras_d1

# Significativos al 5 %
significativos_352_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_352_Otras_d1,
    nivel = 0.05
  )

significativos_352_Otras_d1

# Significativos o marginales al 10 %
significativos_10_352_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_352_Otras_d1,
    nivel = 0.10
  )

significativos_10_352_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-1-3-2
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-1-3-2"
)


# =========================================================
# 1. 4132 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_4132_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_4231)

resultados_4132_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_4231
  )

resultados_4132_4231_d1

# Significativos al 5 %
significativos_4132_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4231_d1,
    nivel = 0.05
  )

significativos_4132_4231_d1

# Significativos o marginales al 10 %
significativos_10_4132_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4231_d1,
    nivel = 0.10
  )

significativos_10_4132_4231_d1


# =========================================================
# 2. 4132 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_4132_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_3421)

resultados_4132_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_3421
  )

resultados_4132_3421_d1

# Significativos al 5 %
significativos_4132_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_3421_d1,
    nivel = 0.05
  )

significativos_4132_3421_d1

# Significativos o marginales al 10 %
significativos_10_4132_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_3421_d1,
    nivel = 0.10
  )

significativos_10_4132_3421_d1


# =========================================================
# 3. 4132 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_4132_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_343)

resultados_4132_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_343
  )

resultados_4132_343_d1

# Significativos al 5 %
significativos_4132_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_343_d1,
    nivel = 0.05
  )

significativos_4132_343_d1

# Significativos o marginales al 10 %
significativos_10_4132_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_343_d1,
    nivel = 0.10
  )

significativos_10_4132_343_d1


# =========================================================
# 4. 4132 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_4132_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_352)

resultados_4132_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_352
  )

resultados_4132_352_d1

# Significativos al 5 %
significativos_4132_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_352_d1,
    nivel = 0.05
  )

significativos_4132_352_d1

# Significativos o marginales al 10 %
significativos_10_4132_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_352_d1,
    nivel = 0.10
  )

significativos_10_4132_352_d1


# =========================================================
# 5. 4132 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_4132_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_4132)

resultados_4132_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_4132
  )

resultados_4132_4132_d1

# Significativos al 5 %
significativos_4132_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4132_d1,
    nivel = 0.05
  )

significativos_4132_4132_d1

# Significativos o marginales al 10 %
significativos_10_4132_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4132_d1,
    nivel = 0.10
  )

significativos_10_4132_4132_d1


# =========================================================
# 6. 4132 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_4132_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_4141)

resultados_4132_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_4141
  )

resultados_4132_4141_d1

# Significativos al 5 %
significativos_4132_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4141_d1,
    nivel = 0.05
  )

significativos_4132_4141_d1

# Significativos o marginales al 10 %
significativos_10_4132_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4141_d1,
    nivel = 0.10
  )

significativos_10_4132_4141_d1


# =========================================================
# 7. 4132 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_4132_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_4312)

resultados_4132_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_4312
  )

resultados_4132_4312_d1

# Significativos al 5 %
significativos_4132_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4312_d1,
    nivel = 0.05
  )

significativos_4132_4312_d1

# Significativos o marginales al 10 %
significativos_10_4132_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4312_d1,
    nivel = 0.10
  )

significativos_10_4132_4312_d1


# =========================================================
# 8. 4132 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_4132_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_433)

resultados_4132_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_433
  )

resultados_4132_433_d1

# Significativos al 5 %
significativos_4132_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_433_d1,
    nivel = 0.05
  )

significativos_4132_433_d1

# Significativos o marginales al 10 %
significativos_10_4132_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_433_d1,
    nivel = 0.10
  )

significativos_10_4132_433_d1


# =========================================================
# 9. 4132 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_4132_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_4411)

resultados_4132_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_4411
  )

resultados_4132_4411_d1

# Significativos al 5 %
significativos_4132_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4411_d1,
    nivel = 0.05
  )

significativos_4132_4411_d1

# Significativos o marginales al 10 %
significativos_10_4132_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_4411_d1,
    nivel = 0.10
  )

significativos_10_4132_4411_d1


# =========================================================
# 10. 4132 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_4132_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_442)

resultados_4132_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_442
  )

resultados_4132_442_d1

# Significativos al 5 %
significativos_4132_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_442_d1,
    nivel = 0.05
  )

significativos_4132_442_d1

# Significativos o marginales al 10 %
significativos_10_4132_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_442_d1,
    nivel = 0.10
  )

significativos_10_4132_442_d1


# =========================================================
# 11. 4132 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_4132_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_451)

resultados_4132_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_451
  )

resultados_4132_451_d1

# Significativos al 5 %
significativos_4132_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_451_d1,
    nivel = 0.05
  )

significativos_4132_451_d1

# Significativos o marginales al 10 %
significativos_10_4132_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_451_d1,
    nivel = 0.10
  )

significativos_10_4132_451_d1


# =========================================================
# 12. 4132 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_4132_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_532)

resultados_4132_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_532
  )

resultados_4132_532_d1

# Significativos al 5 %
significativos_4132_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_532_d1,
    nivel = 0.05
  )

significativos_4132_532_d1

# Significativos o marginales al 10 %
significativos_10_4132_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_532_d1,
    nivel = 0.10
  )

significativos_10_4132_532_d1


# =========================================================
# 13. 4132 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_4132_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_541)

resultados_4132_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_541
  )

resultados_4132_541_d1

# Significativos al 5 %
significativos_4132_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_541_d1,
    nivel = 0.05
  )

significativos_4132_541_d1

# Significativos o marginales al 10 %
significativos_10_4132_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_541_d1,
    nivel = 0.10
  )

significativos_10_4132_541_d1


# =========================================================
# 14. 4132 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_4132_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4132_Otras)

resultados_4132_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4132_Otras
  )

resultados_4132_Otras_d1

# Significativos al 5 %
significativos_4132_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_Otras_d1,
    nivel = 0.05
  )

significativos_4132_Otras_d1

# Significativos o marginales al 10 %
significativos_10_4132_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4132_Otras_d1,
    nivel = 0.10
  )

significativos_10_4132_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-1-4-1
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-1-4-1"
)


# =========================================================
# 1. 4141 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_4141_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_4231)

resultados_4141_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_4231
  )

resultados_4141_4231_d1

# Significativos al 5 %
significativos_4141_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4231_d1,
    nivel = 0.05
  )

significativos_4141_4231_d1

# Significativos o marginales al 10 %
significativos_10_4141_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4231_d1,
    nivel = 0.10
  )

significativos_10_4141_4231_d1


# =========================================================
# 2. 4141 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_4141_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_3421)

resultados_4141_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_3421
  )

resultados_4141_3421_d1

# Significativos al 5 %
significativos_4141_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_3421_d1,
    nivel = 0.05
  )

significativos_4141_3421_d1

# Significativos o marginales al 10 %
significativos_10_4141_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_3421_d1,
    nivel = 0.10
  )

significativos_10_4141_3421_d1


# =========================================================
# 3. 4141 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_4141_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_343)

resultados_4141_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_343
  )

resultados_4141_343_d1

# Significativos al 5 %
significativos_4141_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_343_d1,
    nivel = 0.05
  )

significativos_4141_343_d1

# Significativos o marginales al 10 %
significativos_10_4141_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_343_d1,
    nivel = 0.10
  )

significativos_10_4141_343_d1


# =========================================================
# 4. 4141 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_4141_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_352)

resultados_4141_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_352
  )

resultados_4141_352_d1

# Significativos al 5 %
significativos_4141_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_352_d1,
    nivel = 0.05
  )

significativos_4141_352_d1

# Significativos o marginales al 10 %
significativos_10_4141_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_352_d1,
    nivel = 0.10
  )

significativos_10_4141_352_d1


# =========================================================
# 5. 4141 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_4141_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_4132)

resultados_4141_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_4132
  )

resultados_4141_4132_d1

# Significativos al 5 %
significativos_4141_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4132_d1,
    nivel = 0.05
  )

significativos_4141_4132_d1

# Significativos o marginales al 10 %
significativos_10_4141_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4132_d1,
    nivel = 0.10
  )

significativos_10_4141_4132_d1


# =========================================================
# 6. 4141 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_4141_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_4141)

resultados_4141_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_4141
  )

resultados_4141_4141_d1

# Significativos al 5 %
significativos_4141_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4141_d1,
    nivel = 0.05
  )

significativos_4141_4141_d1

# Significativos o marginales al 10 %
significativos_10_4141_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4141_d1,
    nivel = 0.10
  )

significativos_10_4141_4141_d1


# =========================================================
# 7. 4141 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_4141_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_4312)

resultados_4141_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_4312
  )

resultados_4141_4312_d1

# Significativos al 5 %
significativos_4141_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4312_d1,
    nivel = 0.05
  )

significativos_4141_4312_d1

# Significativos o marginales al 10 %
significativos_10_4141_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4312_d1,
    nivel = 0.10
  )

significativos_10_4141_4312_d1


# =========================================================
# 8. 4141 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_4141_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_433)

resultados_4141_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_433
  )

resultados_4141_433_d1

# Significativos al 5 %
significativos_4141_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_433_d1,
    nivel = 0.05
  )

significativos_4141_433_d1

# Significativos o marginales al 10 %
significativos_10_4141_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_433_d1,
    nivel = 0.10
  )

significativos_10_4141_433_d1


# =========================================================
# 9. 4141 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_4141_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_4411)

resultados_4141_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_4411
  )

resultados_4141_4411_d1

# Significativos al 5 %
significativos_4141_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4411_d1,
    nivel = 0.05
  )

significativos_4141_4411_d1

# Significativos o marginales al 10 %
significativos_10_4141_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_4411_d1,
    nivel = 0.10
  )

significativos_10_4141_4411_d1


# =========================================================
# 10. 4141 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_4141_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_442)

resultados_4141_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_442
  )

resultados_4141_442_d1

# Significativos al 5 %
significativos_4141_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_442_d1,
    nivel = 0.05
  )

significativos_4141_442_d1

# Significativos o marginales al 10 %
significativos_10_4141_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_442_d1,
    nivel = 0.10
  )

significativos_10_4141_442_d1


# =========================================================
# 11. 4141 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_4141_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_451)

resultados_4141_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_451
  )

resultados_4141_451_d1

# Significativos al 5 %
significativos_4141_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_451_d1,
    nivel = 0.05
  )

significativos_4141_451_d1

# Significativos o marginales al 10 %
significativos_10_4141_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_451_d1,
    nivel = 0.10
  )

significativos_10_4141_451_d1


# =========================================================
# 12. 4141 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_4141_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_532)

resultados_4141_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_532
  )

resultados_4141_532_d1

# Significativos al 5 %
significativos_4141_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_532_d1,
    nivel = 0.05
  )

significativos_4141_532_d1

# Significativos o marginales al 10 %
significativos_10_4141_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_532_d1,
    nivel = 0.10
  )

significativos_10_4141_532_d1


# =========================================================
# 13. 4141 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_4141_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_541)

resultados_4141_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_541
  )

resultados_4141_541_d1

# Significativos al 5 %
significativos_4141_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_541_d1,
    nivel = 0.05
  )

significativos_4141_541_d1

# Significativos o marginales al 10 %
significativos_10_4141_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_541_d1,
    nivel = 0.10
  )

significativos_10_4141_541_d1


# =========================================================
# 14. 4141 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_4141_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4141_Otras)

resultados_4141_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4141_Otras
  )

resultados_4141_Otras_d1

# Significativos al 5 %
significativos_4141_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_Otras_d1,
    nivel = 0.05
  )

significativos_4141_Otras_d1

# Significativos o marginales al 10 %
significativos_10_4141_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4141_Otras_d1,
    nivel = 0.10
  )

significativos_10_4141_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-3-1-2
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-3-1-2"
)


# =========================================================
# 1. 4312 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_4312_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_4231)

resultados_4312_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_4231
  )

resultados_4312_4231_d1

# Significativos al 5 %
significativos_4312_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4231_d1,
    nivel = 0.05
  )

significativos_4312_4231_d1

# Significativos o marginales al 10 %
significativos_10_4312_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4231_d1,
    nivel = 0.10
  )

significativos_10_4312_4231_d1


# =========================================================
# 2. 4312 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_4312_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_3421)

resultados_4312_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_3421
  )

resultados_4312_3421_d1

# Significativos al 5 %
significativos_4312_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_3421_d1,
    nivel = 0.05
  )

significativos_4312_3421_d1

# Significativos o marginales al 10 %
significativos_10_4312_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_3421_d1,
    nivel = 0.10
  )

significativos_10_4312_3421_d1


# =========================================================
# 3. 4312 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_4312_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_343)

resultados_4312_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_343
  )

resultados_4312_343_d1

# Significativos al 5 %
significativos_4312_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_343_d1,
    nivel = 0.05
  )

significativos_4312_343_d1

# Significativos o marginales al 10 %
significativos_10_4312_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_343_d1,
    nivel = 0.10
  )

significativos_10_4312_343_d1


# =========================================================
# 4. 4312 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_4312_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_352)

resultados_4312_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_352
  )

resultados_4312_352_d1

# Significativos al 5 %
significativos_4312_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_352_d1,
    nivel = 0.05
  )

significativos_4312_352_d1

# Significativos o marginales al 10 %
significativos_10_4312_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_352_d1,
    nivel = 0.10
  )

significativos_10_4312_352_d1


# =========================================================
# 5. 4312 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_4312_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_4132)

resultados_4312_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_4132
  )

resultados_4312_4132_d1

# Significativos al 5 %
significativos_4312_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4132_d1,
    nivel = 0.05
  )

significativos_4312_4132_d1

# Significativos o marginales al 10 %
significativos_10_4312_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4132_d1,
    nivel = 0.10
  )

significativos_10_4312_4132_d1


# =========================================================
# 6. 4312 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_4312_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_4141)

resultados_4312_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_4141
  )

resultados_4312_4141_d1

# Significativos al 5 %
significativos_4312_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4141_d1,
    nivel = 0.05
  )

significativos_4312_4141_d1

# Significativos o marginales al 10 %
significativos_10_4312_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4141_d1,
    nivel = 0.10
  )

significativos_10_4312_4141_d1


# =========================================================
# 7. 4312 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_4312_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_4312)

resultados_4312_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_4312
  )

resultados_4312_4312_d1

# Significativos al 5 %
significativos_4312_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4312_d1,
    nivel = 0.05
  )

significativos_4312_4312_d1

# Significativos o marginales al 10 %
significativos_10_4312_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4312_d1,
    nivel = 0.10
  )

significativos_10_4312_4312_d1


# =========================================================
# 8. 4312 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_4312_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_433)

resultados_4312_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_433
  )

resultados_4312_433_d1

# Significativos al 5 %
significativos_4312_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_433_d1,
    nivel = 0.05
  )

significativos_4312_433_d1

# Significativos o marginales al 10 %
significativos_10_4312_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_433_d1,
    nivel = 0.10
  )

significativos_10_4312_433_d1


# =========================================================
# 9. 4312 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_4312_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_4411)

resultados_4312_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_4411
  )

resultados_4312_4411_d1

# Significativos al 5 %
significativos_4312_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4411_d1,
    nivel = 0.05
  )

significativos_4312_4411_d1

# Significativos o marginales al 10 %
significativos_10_4312_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_4411_d1,
    nivel = 0.10
  )

significativos_10_4312_4411_d1


# =========================================================
# 10. 4312 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_4312_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_442)

resultados_4312_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_442
  )

resultados_4312_442_d1

# Significativos al 5 %
significativos_4312_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_442_d1,
    nivel = 0.05
  )

significativos_4312_442_d1

# Significativos o marginales al 10 %
significativos_10_4312_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_442_d1,
    nivel = 0.10
  )

significativos_10_4312_442_d1


# =========================================================
# 11. 4312 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_4312_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_451)

resultados_4312_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_451
  )

resultados_4312_451_d1

# Significativos al 5 %
significativos_4312_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_451_d1,
    nivel = 0.05
  )

significativos_4312_451_d1

# Significativos o marginales al 10 %
significativos_10_4312_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_451_d1,
    nivel = 0.10
  )

significativos_10_4312_451_d1


# =========================================================
# 12. 4312 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_4312_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_532)

resultados_4312_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_532
  )

resultados_4312_532_d1

# Significativos al 5 %
significativos_4312_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_532_d1,
    nivel = 0.05
  )

significativos_4312_532_d1

# Significativos o marginales al 10 %
significativos_10_4312_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_532_d1,
    nivel = 0.10
  )

significativos_10_4312_532_d1


# =========================================================
# 13. 4312 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_4312_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_541)

resultados_4312_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_541
  )

resultados_4312_541_d1

# Significativos al 5 %
significativos_4312_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_541_d1,
    nivel = 0.05
  )

significativos_4312_541_d1

# Significativos o marginales al 10 %
significativos_10_4312_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_541_d1,
    nivel = 0.10
  )

significativos_10_4312_541_d1


# =========================================================
# 14. 4312 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_4312_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4312_Otras)

resultados_4312_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4312_Otras
  )

resultados_4312_Otras_d1

# Significativos al 5 %
significativos_4312_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_Otras_d1,
    nivel = 0.05
  )

significativos_4312_Otras_d1

# Significativos o marginales al 10 %
significativos_10_4312_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4312_Otras_d1,
    nivel = 0.10
  )

significativos_10_4312_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-3-3
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-3-3"
)


# =========================================================
# 1. 433 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_433_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_4231)

resultados_433_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_4231
  )

resultados_433_4231_d1

# Significativos al 5 %
significativos_433_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4231_d1,
    nivel = 0.05
  )

significativos_433_4231_d1

# Significativos o marginales al 10 %
significativos_10_433_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4231_d1,
    nivel = 0.10
  )

significativos_10_433_4231_d1


# =========================================================
# 2. 433 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_433_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_3421)

resultados_433_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_3421
  )

resultados_433_3421_d1

# Significativos al 5 %
significativos_433_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_433_3421_d1,
    nivel = 0.05
  )

significativos_433_3421_d1

# Significativos o marginales al 10 %
significativos_10_433_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_433_3421_d1,
    nivel = 0.10
  )

significativos_10_433_3421_d1


# =========================================================
# 3. 433 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_433_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_343)

resultados_433_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_343
  )

resultados_433_343_d1

# Significativos al 5 %
significativos_433_343_d1 <-
  extraer_significativos_formaciones(
    resultados_433_343_d1,
    nivel = 0.05
  )

significativos_433_343_d1

# Significativos o marginales al 10 %
significativos_10_433_343_d1 <-
  extraer_significativos_formaciones(
    resultados_433_343_d1,
    nivel = 0.10
  )

significativos_10_433_343_d1


# =========================================================
# 4. 433 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_433_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_352)

resultados_433_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_352
  )

resultados_433_352_d1

# Significativos al 5 %
significativos_433_352_d1 <-
  extraer_significativos_formaciones(
    resultados_433_352_d1,
    nivel = 0.05
  )

significativos_433_352_d1

# Significativos o marginales al 10 %
significativos_10_433_352_d1 <-
  extraer_significativos_formaciones(
    resultados_433_352_d1,
    nivel = 0.10
  )

significativos_10_433_352_d1


# =========================================================
# 5. 433 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_433_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_4132)

resultados_433_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_4132
  )

resultados_433_4132_d1

# Significativos al 5 %
significativos_433_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4132_d1,
    nivel = 0.05
  )

significativos_433_4132_d1

# Significativos o marginales al 10 %
significativos_10_433_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4132_d1,
    nivel = 0.10
  )

significativos_10_433_4132_d1


# =========================================================
# 6. 433 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_433_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_4141)

resultados_433_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_4141
  )

resultados_433_4141_d1

# Significativos al 5 %
significativos_433_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4141_d1,
    nivel = 0.05
  )

significativos_433_4141_d1

# Significativos o marginales al 10 %
significativos_10_433_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4141_d1,
    nivel = 0.10
  )

significativos_10_433_4141_d1


# =========================================================
# 7. 433 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_433_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_4312)

resultados_433_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_4312
  )

resultados_433_4312_d1

# Significativos al 5 %
significativos_433_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4312_d1,
    nivel = 0.05
  )

significativos_433_4312_d1

# Significativos o marginales al 10 %
significativos_10_433_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4312_d1,
    nivel = 0.10
  )

significativos_10_433_4312_d1


# =========================================================
# 8. 433 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_433_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_433)

resultados_433_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_433
  )

resultados_433_433_d1

# Significativos al 5 %
significativos_433_433_d1 <-
  extraer_significativos_formaciones(
    resultados_433_433_d1,
    nivel = 0.05
  )

significativos_433_433_d1

# Significativos o marginales al 10 %
significativos_10_433_433_d1 <-
  extraer_significativos_formaciones(
    resultados_433_433_d1,
    nivel = 0.10
  )

significativos_10_433_433_d1


# =========================================================
# 9. 433 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_433_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_4411)

resultados_433_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_4411
  )

resultados_433_4411_d1

# Significativos al 5 %
significativos_433_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4411_d1,
    nivel = 0.05
  )

significativos_433_4411_d1

# Significativos o marginales al 10 %
significativos_10_433_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_433_4411_d1,
    nivel = 0.10
  )

significativos_10_433_4411_d1


# =========================================================
# 10. 433 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_433_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_442)

resultados_433_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_442
  )

resultados_433_442_d1

# Significativos al 5 %
significativos_433_442_d1 <-
  extraer_significativos_formaciones(
    resultados_433_442_d1,
    nivel = 0.05
  )

significativos_433_442_d1

# Significativos o marginales al 10 %
significativos_10_433_442_d1 <-
  extraer_significativos_formaciones(
    resultados_433_442_d1,
    nivel = 0.10
  )

significativos_10_433_442_d1


# =========================================================
# 11. 433 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_433_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_451)

resultados_433_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_451
  )

resultados_433_451_d1

# Significativos al 5 %
significativos_433_451_d1 <-
  extraer_significativos_formaciones(
    resultados_433_451_d1,
    nivel = 0.05
  )

significativos_433_451_d1

# Significativos o marginales al 10 %
significativos_10_433_451_d1 <-
  extraer_significativos_formaciones(
    resultados_433_451_d1,
    nivel = 0.10
  )

significativos_10_433_451_d1


# =========================================================
# 12. 433 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_433_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_532)

resultados_433_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_532
  )

resultados_433_532_d1

# Significativos al 5 %
significativos_433_532_d1 <-
  extraer_significativos_formaciones(
    resultados_433_532_d1,
    nivel = 0.05
  )

significativos_433_532_d1

# Significativos o marginales al 10 %
significativos_10_433_532_d1 <-
  extraer_significativos_formaciones(
    resultados_433_532_d1,
    nivel = 0.10
  )

significativos_10_433_532_d1


# =========================================================
# 13. 433 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_433_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_541)

resultados_433_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_541
  )

resultados_433_541_d1

# Significativos al 5 %
significativos_433_541_d1 <-
  extraer_significativos_formaciones(
    resultados_433_541_d1,
    nivel = 0.05
  )

significativos_433_541_d1

# Significativos o marginales al 10 %
significativos_10_433_541_d1 <-
  extraer_significativos_formaciones(
    resultados_433_541_d1,
    nivel = 0.10
  )

significativos_10_433_541_d1


# =========================================================
# 14. 433 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_433_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_433_Otras)

resultados_433_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_433_Otras
  )

resultados_433_Otras_d1

# Significativos al 5 %
significativos_433_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_433_Otras_d1,
    nivel = 0.05
  )

significativos_433_Otras_d1

# Significativos o marginales al 10 %
significativos_10_433_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_433_Otras_d1,
    nivel = 0.10
  )

significativos_10_433_Otras_d1

# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-4-1-1
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-4-1-1"
)


# =========================================================
# 1. 4411 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_4411_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_4231)

resultados_4411_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_4231
  )

resultados_4411_4231_d1

# Significativos al 5 %
significativos_4411_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4231_d1,
    nivel = 0.05
  )

significativos_4411_4231_d1

# Significativos o marginales al 10 %
significativos_10_4411_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4231_d1,
    nivel = 0.10
  )

significativos_10_4411_4231_d1


# =========================================================
# 2. 4411 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_4411_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_3421)

resultados_4411_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_3421
  )

resultados_4411_3421_d1

# Significativos al 5 %
significativos_4411_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_3421_d1,
    nivel = 0.05
  )

significativos_4411_3421_d1

# Significativos o marginales al 10 %
significativos_10_4411_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_3421_d1,
    nivel = 0.10
  )

significativos_10_4411_3421_d1


# =========================================================
# 3. 4411 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_4411_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_343)

resultados_4411_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_343
  )

resultados_4411_343_d1

# Significativos al 5 %
significativos_4411_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_343_d1,
    nivel = 0.05
  )

significativos_4411_343_d1

# Significativos o marginales al 10 %
significativos_10_4411_343_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_343_d1,
    nivel = 0.10
  )

significativos_10_4411_343_d1


# =========================================================
# 4. 4411 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_4411_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_352)

resultados_4411_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_352
  )

resultados_4411_352_d1

# Significativos al 5 %
significativos_4411_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_352_d1,
    nivel = 0.05
  )

significativos_4411_352_d1

# Significativos o marginales al 10 %
significativos_10_4411_352_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_352_d1,
    nivel = 0.10
  )

significativos_10_4411_352_d1


# =========================================================
# 5. 4411 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_4411_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_4132)

resultados_4411_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_4132
  )

resultados_4411_4132_d1

# Significativos al 5 %
significativos_4411_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4132_d1,
    nivel = 0.05
  )

significativos_4411_4132_d1

# Significativos o marginales al 10 %
significativos_10_4411_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4132_d1,
    nivel = 0.10
  )

significativos_10_4411_4132_d1


# =========================================================
# 6. 4411 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_4411_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_4141)

resultados_4411_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_4141
  )

resultados_4411_4141_d1

# Significativos al 5 %
significativos_4411_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4141_d1,
    nivel = 0.05
  )

significativos_4411_4141_d1

# Significativos o marginales al 10 %
significativos_10_4411_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4141_d1,
    nivel = 0.10
  )

significativos_10_4411_4141_d1


# =========================================================
# 7. 4411 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_4411_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_4312)

resultados_4411_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_4312
  )

resultados_4411_4312_d1

# Significativos al 5 %
significativos_4411_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4312_d1,
    nivel = 0.05
  )

significativos_4411_4312_d1

# Significativos o marginales al 10 %
significativos_10_4411_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4312_d1,
    nivel = 0.10
  )

significativos_10_4411_4312_d1


# =========================================================
# 8. 4411 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_4411_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_433)

resultados_4411_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_433
  )

resultados_4411_433_d1

# Significativos al 5 %
significativos_4411_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_433_d1,
    nivel = 0.05
  )

significativos_4411_433_d1

# Significativos o marginales al 10 %
significativos_10_4411_433_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_433_d1,
    nivel = 0.10
  )

significativos_10_4411_433_d1


# =========================================================
# 9. 4411 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_4411_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_4411)

resultados_4411_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_4411
  )

resultados_4411_4411_d1

# Significativos al 5 %
significativos_4411_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4411_d1,
    nivel = 0.05
  )

significativos_4411_4411_d1

# Significativos o marginales al 10 %
significativos_10_4411_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_4411_d1,
    nivel = 0.10
  )

significativos_10_4411_4411_d1


# =========================================================
# 10. 4411 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_4411_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_442)

resultados_4411_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_442
  )

resultados_4411_442_d1

# Significativos al 5 %
significativos_4411_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_442_d1,
    nivel = 0.05
  )

significativos_4411_442_d1

# Significativos o marginales al 10 %
significativos_10_4411_442_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_442_d1,
    nivel = 0.10
  )

significativos_10_4411_442_d1


# =========================================================
# 11. 4411 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_4411_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_451)

resultados_4411_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_451
  )

resultados_4411_451_d1

# Significativos al 5 %
significativos_4411_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_451_d1,
    nivel = 0.05
  )

significativos_4411_451_d1

# Significativos o marginales al 10 %
significativos_10_4411_451_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_451_d1,
    nivel = 0.10
  )

significativos_10_4411_451_d1


# =========================================================
# 12. 4411 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_4411_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_532)

resultados_4411_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_532
  )

resultados_4411_532_d1

# Significativos al 5 %
significativos_4411_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_532_d1,
    nivel = 0.05
  )

significativos_4411_532_d1

# Significativos o marginales al 10 %
significativos_10_4411_532_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_532_d1,
    nivel = 0.10
  )

significativos_10_4411_532_d1


# =========================================================
# 13. 4411 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_4411_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_541)

resultados_4411_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_541
  )

resultados_4411_541_d1

# Significativos al 5 %
significativos_4411_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_541_d1,
    nivel = 0.05
  )

significativos_4411_541_d1

# Significativos o marginales al 10 %
significativos_10_4411_541_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_541_d1,
    nivel = 0.10
  )

significativos_10_4411_541_d1


# =========================================================
# 14. 4411 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_4411_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_4411_Otras)

resultados_4411_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_4411_Otras
  )

resultados_4411_Otras_d1

# Significativos al 5 %
significativos_4411_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_Otras_d1,
    nivel = 0.05
  )

significativos_4411_Otras_d1

# Significativos o marginales al 10 %
significativos_10_4411_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_4411_Otras_d1,
    nivel = 0.10
  )

significativos_10_4411_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-4-2
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-4-2"
)


# =========================================================
# 1. 442 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_442_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_4231)

resultados_442_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_4231
  )

resultados_442_4231_d1

significativos_442_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4231_d1,
    nivel = 0.05
  )

significativos_442_4231_d1

significativos_10_442_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4231_d1,
    nivel = 0.10
  )

significativos_10_442_4231_d1


# =========================================================
# 2. 442 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_442_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_3421)

resultados_442_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_3421
  )

resultados_442_3421_d1

significativos_442_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_442_3421_d1,
    nivel = 0.05
  )

significativos_442_3421_d1

significativos_10_442_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_442_3421_d1,
    nivel = 0.10
  )

significativos_10_442_3421_d1


# =========================================================
# 3. 442 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_442_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_343)

resultados_442_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_343
  )

resultados_442_343_d1

significativos_442_343_d1 <-
  extraer_significativos_formaciones(
    resultados_442_343_d1,
    nivel = 0.05
  )

significativos_442_343_d1

significativos_10_442_343_d1 <-
  extraer_significativos_formaciones(
    resultados_442_343_d1,
    nivel = 0.10
  )

significativos_10_442_343_d1


# =========================================================
# 4. 442 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_442_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_352)

resultados_442_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_352
  )

resultados_442_352_d1

significativos_442_352_d1 <-
  extraer_significativos_formaciones(
    resultados_442_352_d1,
    nivel = 0.05
  )

significativos_442_352_d1

significativos_10_442_352_d1 <-
  extraer_significativos_formaciones(
    resultados_442_352_d1,
    nivel = 0.10
  )

significativos_10_442_352_d1


# =========================================================
# 5. 442 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_442_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_4132)

resultados_442_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_4132
  )

resultados_442_4132_d1

significativos_442_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4132_d1,
    nivel = 0.05
  )

significativos_442_4132_d1

significativos_10_442_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4132_d1,
    nivel = 0.10
  )

significativos_10_442_4132_d1


# =========================================================
# 6. 442 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_442_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_4141)

resultados_442_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_4141
  )

resultados_442_4141_d1

significativos_442_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4141_d1,
    nivel = 0.05
  )

significativos_442_4141_d1

significativos_10_442_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4141_d1,
    nivel = 0.10
  )

significativos_10_442_4141_d1


# =========================================================
# 7. 442 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_442_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_4312)

resultados_442_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_4312
  )

resultados_442_4312_d1

significativos_442_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4312_d1,
    nivel = 0.05
  )

significativos_442_4312_d1

significativos_10_442_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4312_d1,
    nivel = 0.10
  )

significativos_10_442_4312_d1


# =========================================================
# 8. 442 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_442_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_433)

resultados_442_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_433
  )

resultados_442_433_d1

significativos_442_433_d1 <-
  extraer_significativos_formaciones(
    resultados_442_433_d1,
    nivel = 0.05
  )

significativos_442_433_d1

significativos_10_442_433_d1 <-
  extraer_significativos_formaciones(
    resultados_442_433_d1,
    nivel = 0.10
  )

significativos_10_442_433_d1


# =========================================================
# 9. 442 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_442_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_4411)

resultados_442_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_4411
  )

resultados_442_4411_d1

significativos_442_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4411_d1,
    nivel = 0.05
  )

significativos_442_4411_d1

significativos_10_442_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_442_4411_d1,
    nivel = 0.10
  )

significativos_10_442_4411_d1


# =========================================================
# 10. 442 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_442_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_442)

resultados_442_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_442
  )

resultados_442_442_d1

significativos_442_442_d1 <-
  extraer_significativos_formaciones(
    resultados_442_442_d1,
    nivel = 0.05
  )

significativos_442_442_d1

significativos_10_442_442_d1 <-
  extraer_significativos_formaciones(
    resultados_442_442_d1,
    nivel = 0.10
  )

significativos_10_442_442_d1


# =========================================================
# 11. 442 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_442_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_451)

resultados_442_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_451
  )

resultados_442_451_d1

significativos_442_451_d1 <-
  extraer_significativos_formaciones(
    resultados_442_451_d1,
    nivel = 0.05
  )

significativos_442_451_d1

significativos_10_442_451_d1 <-
  extraer_significativos_formaciones(
    resultados_442_451_d1,
    nivel = 0.10
  )

significativos_10_442_451_d1


# =========================================================
# 12. 442 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_442_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_532)

resultados_442_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_532
  )

resultados_442_532_d1

significativos_442_532_d1 <-
  extraer_significativos_formaciones(
    resultados_442_532_d1,
    nivel = 0.05
  )

significativos_442_532_d1

significativos_10_442_532_d1 <-
  extraer_significativos_formaciones(
    resultados_442_532_d1,
    nivel = 0.10
  )

significativos_10_442_532_d1


# =========================================================
# 13. 442 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_442_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_541)

resultados_442_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_541
  )

resultados_442_541_d1

significativos_442_541_d1 <-
  extraer_significativos_formaciones(
    resultados_442_541_d1,
    nivel = 0.05
  )

significativos_442_541_d1

significativos_10_442_541_d1 <-
  extraer_significativos_formaciones(
    resultados_442_541_d1,
    nivel = 0.10
  )

significativos_10_442_541_d1


# =========================================================
# 14. 442 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_442_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_442_Otras)

resultados_442_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_442_Otras
  )

resultados_442_Otras_d1

significativos_442_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_442_Otras_d1,
    nivel = 0.05
  )

significativos_442_Otras_d1

significativos_10_442_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_442_Otras_d1,
    nivel = 0.10
  )

significativos_10_442_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-4-5-1
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Categoría de referencia del resultado: Empate
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-4-5-1"
)


# =========================================================
# 1. 451 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_451_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_4231)

resultados_451_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_4231
  )

resultados_451_4231_d1

significativos_451_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4231_d1,
    nivel = 0.05
  )

significativos_451_4231_d1

significativos_10_451_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4231_d1,
    nivel = 0.10
  )

significativos_10_451_4231_d1


# =========================================================
# 2. 451 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_451_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_3421)

resultados_451_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_3421
  )

resultados_451_3421_d1

significativos_451_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_451_3421_d1,
    nivel = 0.05
  )

significativos_451_3421_d1

significativos_10_451_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_451_3421_d1,
    nivel = 0.10
  )

significativos_10_451_3421_d1


# =========================================================
# 3. 451 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_451_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_343)

resultados_451_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_343
  )

resultados_451_343_d1

significativos_451_343_d1 <-
  extraer_significativos_formaciones(
    resultados_451_343_d1,
    nivel = 0.05
  )

significativos_451_343_d1

significativos_10_451_343_d1 <-
  extraer_significativos_formaciones(
    resultados_451_343_d1,
    nivel = 0.10
  )

significativos_10_451_343_d1


# =========================================================
# 4. 451 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_451_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_352)

resultados_451_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_352
  )

resultados_451_352_d1

significativos_451_352_d1 <-
  extraer_significativos_formaciones(
    resultados_451_352_d1,
    nivel = 0.05
  )

significativos_451_352_d1

significativos_10_451_352_d1 <-
  extraer_significativos_formaciones(
    resultados_451_352_d1,
    nivel = 0.10
  )

significativos_10_451_352_d1


# =========================================================
# 5. 451 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_451_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_4132)

resultados_451_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_4132
  )

resultados_451_4132_d1

significativos_451_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4132_d1,
    nivel = 0.05
  )

significativos_451_4132_d1

significativos_10_451_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4132_d1,
    nivel = 0.10
  )

significativos_10_451_4132_d1


# =========================================================
# 6. 451 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_451_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_4141)

resultados_451_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_4141
  )

resultados_451_4141_d1

significativos_451_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4141_d1,
    nivel = 0.05
  )

significativos_451_4141_d1

significativos_10_451_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4141_d1,
    nivel = 0.10
  )

significativos_10_451_4141_d1


# =========================================================
# 7. 451 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_451_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_4312)

resultados_451_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_4312
  )

resultados_451_4312_d1

significativos_451_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4312_d1,
    nivel = 0.05
  )

significativos_451_4312_d1

significativos_10_451_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4312_d1,
    nivel = 0.10
  )

significativos_10_451_4312_d1


# =========================================================
# 8. 451 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_451_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_433)

resultados_451_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_433
  )

resultados_451_433_d1

significativos_451_433_d1 <-
  extraer_significativos_formaciones(
    resultados_451_433_d1,
    nivel = 0.05
  )

significativos_451_433_d1

significativos_10_451_433_d1 <-
  extraer_significativos_formaciones(
    resultados_451_433_d1,
    nivel = 0.10
  )

significativos_10_451_433_d1


# =========================================================
# 9. 451 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_451_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_4411)

resultados_451_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_4411
  )

resultados_451_4411_d1

significativos_451_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4411_d1,
    nivel = 0.05
  )

significativos_451_4411_d1

significativos_10_451_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_451_4411_d1,
    nivel = 0.10
  )

significativos_10_451_4411_d1


# =========================================================
# 10. 451 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_451_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_442)

resultados_451_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_442
  )

resultados_451_442_d1

significativos_451_442_d1 <-
  extraer_significativos_formaciones(
    resultados_451_442_d1,
    nivel = 0.05
  )

significativos_451_442_d1

significativos_10_451_442_d1 <-
  extraer_significativos_formaciones(
    resultados_451_442_d1,
    nivel = 0.10
  )

significativos_10_451_442_d1


# =========================================================
# 11. 451 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_451_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_451)

resultados_451_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_451
  )

resultados_451_451_d1

significativos_451_451_d1 <-
  extraer_significativos_formaciones(
    resultados_451_451_d1,
    nivel = 0.05
  )

significativos_451_451_d1

significativos_10_451_451_d1 <-
  extraer_significativos_formaciones(
    resultados_451_451_d1,
    nivel = 0.10
  )

significativos_10_451_451_d1


# =========================================================
# 12. 451 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_451_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_532)

resultados_451_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_532
  )

resultados_451_532_d1

significativos_451_532_d1 <-
  extraer_significativos_formaciones(
    resultados_451_532_d1,
    nivel = 0.05
  )

significativos_451_532_d1

significativos_10_451_532_d1 <-
  extraer_significativos_formaciones(
    resultados_451_532_d1,
    nivel = 0.10
  )

significativos_10_451_532_d1


# =========================================================
# 13. 451 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_451_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_541)

resultados_451_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_541
  )

resultados_451_541_d1

significativos_451_541_d1 <-
  extraer_significativos_formaciones(
    resultados_451_541_d1,
    nivel = 0.05
  )

significativos_451_541_d1

significativos_10_451_541_d1 <-
  extraer_significativos_formaciones(
    resultados_451_541_d1,
    nivel = 0.10
  )

significativos_10_451_541_d1


# =========================================================
# 14. 451 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_451_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_451_Otras)

resultados_451_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_451_Otras
  )

resultados_451_Otras_d1

significativos_451_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_451_Otras_d1,
    nivel = 0.05
  )

significativos_451_Otras_d1

significativos_10_451_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_451_Otras_d1,
    nivel = 0.10
  )

significativos_10_451_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-5-3-2
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-5-3-2"
)


# =========================================================
# 1. 532 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_532_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_4231)

resultados_532_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_4231
  )

resultados_532_4231_d1

# Significativos al 5 %
significativos_532_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4231_d1,
    nivel = 0.05
  )

significativos_532_4231_d1

# Significativos o marginales al 10 %
significativos_10_532_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4231_d1,
    nivel = 0.10
  )

significativos_10_532_4231_d1


# =========================================================
# 2. 532 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_532_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_3421)

resultados_532_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_3421
  )

resultados_532_3421_d1

# Significativos al 5 %
significativos_532_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_532_3421_d1,
    nivel = 0.05
  )

significativos_532_3421_d1

# Significativos o marginales al 10 %
significativos_10_532_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_532_3421_d1,
    nivel = 0.10
  )

significativos_10_532_3421_d1


# =========================================================
# 3. 532 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_532_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_343)

resultados_532_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_343
  )

resultados_532_343_d1

# Significativos al 5 %
significativos_532_343_d1 <-
  extraer_significativos_formaciones(
    resultados_532_343_d1,
    nivel = 0.05
  )

significativos_532_343_d1

# Significativos o marginales al 10 %
significativos_10_532_343_d1 <-
  extraer_significativos_formaciones(
    resultados_532_343_d1,
    nivel = 0.10
  )

significativos_10_532_343_d1


# =========================================================
# 4. 532 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_532_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_352)

resultados_532_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_352
  )

resultados_532_352_d1

# Significativos al 5 %
significativos_532_352_d1 <-
  extraer_significativos_formaciones(
    resultados_532_352_d1,
    nivel = 0.05
  )

significativos_532_352_d1

# Significativos o marginales al 10 %
significativos_10_532_352_d1 <-
  extraer_significativos_formaciones(
    resultados_532_352_d1,
    nivel = 0.10
  )

significativos_10_532_352_d1


# =========================================================
# 5. 532 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_532_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_4132)

resultados_532_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_4132
  )

resultados_532_4132_d1

# Significativos al 5 %
significativos_532_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4132_d1,
    nivel = 0.05
  )

significativos_532_4132_d1

# Significativos o marginales al 10 %
significativos_10_532_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4132_d1,
    nivel = 0.10
  )

significativos_10_532_4132_d1


# =========================================================
# 6. 532 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_532_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_4141)

resultados_532_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_4141
  )

resultados_532_4141_d1

# Significativos al 5 %
significativos_532_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4141_d1,
    nivel = 0.05
  )

significativos_532_4141_d1

# Significativos o marginales al 10 %
significativos_10_532_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4141_d1,
    nivel = 0.10
  )

significativos_10_532_4141_d1


# =========================================================
# 7. 532 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_532_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_4312)

resultados_532_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_4312
  )

resultados_532_4312_d1

# Significativos al 5 %
significativos_532_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4312_d1,
    nivel = 0.05
  )

significativos_532_4312_d1

# Significativos o marginales al 10 %
significativos_10_532_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4312_d1,
    nivel = 0.10
  )

significativos_10_532_4312_d1


# =========================================================
# 8. 532 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_532_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_433)

resultados_532_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_433
  )

resultados_532_433_d1

# Significativos al 5 %
significativos_532_433_d1 <-
  extraer_significativos_formaciones(
    resultados_532_433_d1,
    nivel = 0.05
  )

significativos_532_433_d1

# Significativos o marginales al 10 %
significativos_10_532_433_d1 <-
  extraer_significativos_formaciones(
    resultados_532_433_d1,
    nivel = 0.10
  )

significativos_10_532_433_d1


# =========================================================
# 9. 532 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_532_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_4411)

resultados_532_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_4411
  )

resultados_532_4411_d1

# Significativos al 5 %
significativos_532_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4411_d1,
    nivel = 0.05
  )

significativos_532_4411_d1

# Significativos o marginales al 10 %
significativos_10_532_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_532_4411_d1,
    nivel = 0.10
  )

significativos_10_532_4411_d1


# =========================================================
# 10. 532 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_532_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_442)

resultados_532_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_442
  )

resultados_532_442_d1

# Significativos al 5 %
significativos_532_442_d1 <-
  extraer_significativos_formaciones(
    resultados_532_442_d1,
    nivel = 0.05
  )

significativos_532_442_d1

# Significativos o marginales al 10 %
significativos_10_532_442_d1 <-
  extraer_significativos_formaciones(
    resultados_532_442_d1,
    nivel = 0.10
  )

significativos_10_532_442_d1


# =========================================================
# 11. 532 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_532_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_451)

resultados_532_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_451
  )

resultados_532_451_d1

# Significativos al 5 %
significativos_532_451_d1 <-
  extraer_significativos_formaciones(
    resultados_532_451_d1,
    nivel = 0.05
  )

significativos_532_451_d1

# Significativos o marginales al 10 %
significativos_10_532_451_d1 <-
  extraer_significativos_formaciones(
    resultados_532_451_d1,
    nivel = 0.10
  )

significativos_10_532_451_d1


# =========================================================
# 12. 532 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_532_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_532)

resultados_532_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_532
  )

resultados_532_532_d1

# Significativos al 5 %
significativos_532_532_d1 <-
  extraer_significativos_formaciones(
    resultados_532_532_d1,
    nivel = 0.05
  )

significativos_532_532_d1

# Significativos o marginales al 10 %
significativos_10_532_532_d1 <-
  extraer_significativos_formaciones(
    resultados_532_532_d1,
    nivel = 0.10
  )

significativos_10_532_532_d1


# =========================================================
# 13. 532 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_532_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_541)

resultados_532_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_541
  )

resultados_532_541_d1

# Significativos al 5 %
significativos_532_541_d1 <-
  extraer_significativos_formaciones(
    resultados_532_541_d1,
    nivel = 0.05
  )

significativos_532_541_d1

# Significativos o marginales al 10 %
significativos_10_532_541_d1 <-
  extraer_significativos_formaciones(
    resultados_532_541_d1,
    nivel = 0.10
  )

significativos_10_532_541_d1


# =========================================================
# 14. 532 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_532_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_532_Otras)

resultados_532_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_532_Otras
  )

resultados_532_Otras_d1

# Significativos al 5 %
significativos_532_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_532_Otras_d1,
    nivel = 0.05
  )

significativos_532_Otras_d1

# Significativos o marginales al 10 %
significativos_10_532_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_532_Otras_d1,
    nivel = 0.10
  )

significativos_10_532_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: 1-5-4-1
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "1-5-4-1"
)


# =========================================================
# 1. 541 LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_541_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_4231)

resultados_541_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_4231
  )

resultados_541_4231_d1

# Significativos al 5 %
significativos_541_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4231_d1,
    nivel = 0.05
  )

significativos_541_4231_d1

# Significativos o marginales al 10 %
significativos_10_541_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4231_d1,
    nivel = 0.10
  )

significativos_10_541_4231_d1


# =========================================================
# 2. 541 LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_541_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_3421)

resultados_541_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_3421
  )

resultados_541_3421_d1

# Significativos al 5 %
significativos_541_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_541_3421_d1,
    nivel = 0.05
  )

significativos_541_3421_d1

# Significativos o marginales al 10 %
significativos_10_541_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_541_3421_d1,
    nivel = 0.10
  )

significativos_10_541_3421_d1


# =========================================================
# 3. 541 LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_541_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_343)

resultados_541_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_343
  )

resultados_541_343_d1

# Significativos al 5 %
significativos_541_343_d1 <-
  extraer_significativos_formaciones(
    resultados_541_343_d1,
    nivel = 0.05
  )

significativos_541_343_d1

# Significativos o marginales al 10 %
significativos_10_541_343_d1 <-
  extraer_significativos_formaciones(
    resultados_541_343_d1,
    nivel = 0.10
  )

significativos_10_541_343_d1


# =========================================================
# 4. 541 LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_541_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_352)

resultados_541_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_352
  )

resultados_541_352_d1

# Significativos al 5 %
significativos_541_352_d1 <-
  extraer_significativos_formaciones(
    resultados_541_352_d1,
    nivel = 0.05
  )

significativos_541_352_d1

# Significativos o marginales al 10 %
significativos_10_541_352_d1 <-
  extraer_significativos_formaciones(
    resultados_541_352_d1,
    nivel = 0.10
  )

significativos_10_541_352_d1


# =========================================================
# 5. 541 LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_541_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_4132)

resultados_541_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_4132
  )

resultados_541_4132_d1

# Significativos al 5 %
significativos_541_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4132_d1,
    nivel = 0.05
  )

significativos_541_4132_d1

# Significativos o marginales al 10 %
significativos_10_541_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4132_d1,
    nivel = 0.10
  )

significativos_10_541_4132_d1


# =========================================================
# 6. 541 LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_541_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_4141)

resultados_541_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_4141
  )

resultados_541_4141_d1

# Significativos al 5 %
significativos_541_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4141_d1,
    nivel = 0.05
  )

significativos_541_4141_d1

# Significativos o marginales al 10 %
significativos_10_541_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4141_d1,
    nivel = 0.10
  )

significativos_10_541_4141_d1


# =========================================================
# 7. 541 LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_541_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_4312)

resultados_541_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_4312
  )

resultados_541_4312_d1

# Significativos al 5 %
significativos_541_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4312_d1,
    nivel = 0.05
  )

significativos_541_4312_d1

# Significativos o marginales al 10 %
significativos_10_541_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4312_d1,
    nivel = 0.10
  )

significativos_10_541_4312_d1


# =========================================================
# 8. 541 LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_541_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_433)

resultados_541_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_433
  )

resultados_541_433_d1

# Significativos al 5 %
significativos_541_433_d1 <-
  extraer_significativos_formaciones(
    resultados_541_433_d1,
    nivel = 0.05
  )

significativos_541_433_d1

# Significativos o marginales al 10 %
significativos_10_541_433_d1 <-
  extraer_significativos_formaciones(
    resultados_541_433_d1,
    nivel = 0.10
  )

significativos_10_541_433_d1


# =========================================================
# 9. 541 LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_541_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_4411)

resultados_541_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_4411
  )

resultados_541_4411_d1

# Significativos al 5 %
significativos_541_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4411_d1,
    nivel = 0.05
  )

significativos_541_4411_d1

# Significativos o marginales al 10 %
significativos_10_541_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_541_4411_d1,
    nivel = 0.10
  )

significativos_10_541_4411_d1


# =========================================================
# 10. 541 LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_541_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_442)

resultados_541_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_442
  )

resultados_541_442_d1

# Significativos al 5 %
significativos_541_442_d1 <-
  extraer_significativos_formaciones(
    resultados_541_442_d1,
    nivel = 0.05
  )

significativos_541_442_d1

# Significativos o marginales al 10 %
significativos_10_541_442_d1 <-
  extraer_significativos_formaciones(
    resultados_541_442_d1,
    nivel = 0.10
  )

significativos_10_541_442_d1


# =========================================================
# 11. 541 LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_541_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_451)

resultados_541_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_451
  )

resultados_541_451_d1

# Significativos al 5 %
significativos_541_451_d1 <-
  extraer_significativos_formaciones(
    resultados_541_451_d1,
    nivel = 0.05
  )

significativos_541_451_d1

# Significativos o marginales al 10 %
significativos_10_541_451_d1 <-
  extraer_significativos_formaciones(
    resultados_541_451_d1,
    nivel = 0.10
  )

significativos_10_541_451_d1


# =========================================================
# 12. 541 LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_541_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_532)

resultados_541_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_532
  )

resultados_541_532_d1

# Significativos al 5 %
significativos_541_532_d1 <-
  extraer_significativos_formaciones(
    resultados_541_532_d1,
    nivel = 0.05
  )

significativos_541_532_d1

# Significativos o marginales al 10 %
significativos_10_541_532_d1 <-
  extraer_significativos_formaciones(
    resultados_541_532_d1,
    nivel = 0.10
  )

significativos_10_541_532_d1


# =========================================================
# 13. 541 LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_541_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_541)

resultados_541_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_541
  )

resultados_541_541_d1

# Significativos al 5 %
significativos_541_541_d1 <-
  extraer_significativos_formaciones(
    resultados_541_541_d1,
    nivel = 0.05
  )

significativos_541_541_d1

# Significativos o marginales al 10 %
significativos_10_541_541_d1 <-
  extraer_significativos_formaciones(
    resultados_541_541_d1,
    nivel = 0.10
  )

significativos_10_541_541_d1


# =========================================================
# 14. 541 LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_541_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_541_Otras)

resultados_541_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_541_Otras
  )

resultados_541_Otras_d1

# Significativos al 5 %
significativos_541_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_541_Otras_d1,
    nivel = 0.05
  )

significativos_541_Otras_d1

# Significativos o marginales al 10 %
significativos_10_541_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_541_Otras_d1,
    nivel = 0.10
  )

significativos_10_541_Otras_d1
# =========================================================
# FORMACIÓN LOCAL DE REFERENCIA: OTRAS
# TODAS LAS COMBINACIONES DE FORMACIÓN VISITANTE
# Variable dependiente: resultado_partido_local
# Categoría de referencia del resultado: Empate
# =========================================================


# =========================================================
# REFERENCIA DE LA FORMACIÓN LOCAL
# =========================================================

d1$formacion_local_dep <- relevel(
  factor(d1$formacion_local_dep),
  ref = "Otras"
)


# =========================================================
# 1. OTRAS LOCAL CONTRA 4231 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-2-3-1"
)

m1_Otras_4231 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_4231)

resultados_Otras_4231_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_4231
  )

resultados_Otras_4231_d1

# Significativos al 5 %
significativos_Otras_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4231_d1,
    nivel = 0.05
  )

significativos_Otras_4231_d1

# Significativos o marginales al 10 %
significativos_10_Otras_4231_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4231_d1,
    nivel = 0.10
  )

significativos_10_Otras_4231_d1


# =========================================================
# 2. OTRAS LOCAL CONTRA 3421 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-2-1"
)

m1_Otras_3421 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_3421)

resultados_Otras_3421_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_3421
  )

resultados_Otras_3421_d1

# Significativos al 5 %
significativos_Otras_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_3421_d1,
    nivel = 0.05
  )

significativos_Otras_3421_d1

# Significativos o marginales al 10 %
significativos_10_Otras_3421_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_3421_d1,
    nivel = 0.10
  )

significativos_10_Otras_3421_d1


# =========================================================
# 3. OTRAS LOCAL CONTRA 343 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-4-3"
)

m1_Otras_343 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_343)

resultados_Otras_343_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_343
  )

resultados_Otras_343_d1

# Significativos al 5 %
significativos_Otras_343_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_343_d1,
    nivel = 0.05
  )

significativos_Otras_343_d1

# Significativos o marginales al 10 %
significativos_10_Otras_343_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_343_d1,
    nivel = 0.10
  )

significativos_10_Otras_343_d1


# =========================================================
# 4. OTRAS LOCAL CONTRA 352 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-3-5-2"
)

m1_Otras_352 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_352)

resultados_Otras_352_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_352
  )

resultados_Otras_352_d1

# Significativos al 5 %
significativos_Otras_352_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_352_d1,
    nivel = 0.05
  )

significativos_Otras_352_d1

# Significativos o marginales al 10 %
significativos_10_Otras_352_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_352_d1,
    nivel = 0.10
  )

significativos_10_Otras_352_d1


# =========================================================
# 5. OTRAS LOCAL CONTRA 4132 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-3-2"
)

m1_Otras_4132 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_4132)

resultados_Otras_4132_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_4132
  )

resultados_Otras_4132_d1

# Significativos al 5 %
significativos_Otras_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4132_d1,
    nivel = 0.05
  )

significativos_Otras_4132_d1

# Significativos o marginales al 10 %
significativos_10_Otras_4132_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4132_d1,
    nivel = 0.10
  )

significativos_10_Otras_4132_d1


# =========================================================
# 6. OTRAS LOCAL CONTRA 4141 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-1-4-1"
)

m1_Otras_4141 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_4141)

resultados_Otras_4141_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_4141
  )

resultados_Otras_4141_d1

# Significativos al 5 %
significativos_Otras_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4141_d1,
    nivel = 0.05
  )

significativos_Otras_4141_d1

# Significativos o marginales al 10 %
significativos_10_Otras_4141_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4141_d1,
    nivel = 0.10
  )

significativos_10_Otras_4141_d1


# =========================================================
# 7. OTRAS LOCAL CONTRA 4312 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-1-2"
)

m1_Otras_4312 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_4312)

resultados_Otras_4312_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_4312
  )

resultados_Otras_4312_d1

# Significativos al 5 %
significativos_Otras_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4312_d1,
    nivel = 0.05
  )

significativos_Otras_4312_d1

# Significativos o marginales al 10 %
significativos_10_Otras_4312_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4312_d1,
    nivel = 0.10
  )

significativos_10_Otras_4312_d1


# =========================================================
# 8. OTRAS LOCAL CONTRA 433 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-3-3"
)

m1_Otras_433 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_433)

resultados_Otras_433_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_433
  )

resultados_Otras_433_d1

# Significativos al 5 %
significativos_Otras_433_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_433_d1,
    nivel = 0.05
  )

significativos_Otras_433_d1

# Significativos o marginales al 10 %
significativos_10_Otras_433_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_433_d1,
    nivel = 0.10
  )

significativos_10_Otras_433_d1


# =========================================================
# 9. OTRAS LOCAL CONTRA 4411 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-1-1"
)

m1_Otras_4411 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_4411)

resultados_Otras_4411_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_4411
  )

resultados_Otras_4411_d1

# Significativos al 5 %
significativos_Otras_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4411_d1,
    nivel = 0.05
  )

significativos_Otras_4411_d1

# Significativos o marginales al 10 %
significativos_10_Otras_4411_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_4411_d1,
    nivel = 0.10
  )

significativos_10_Otras_4411_d1


# =========================================================
# 10. OTRAS LOCAL CONTRA 442 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-4-2"
)

m1_Otras_442 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_442)

resultados_Otras_442_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_442
  )

resultados_Otras_442_d1

# Significativos al 5 %
significativos_Otras_442_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_442_d1,
    nivel = 0.05
  )

significativos_Otras_442_d1

# Significativos o marginales al 10 %
significativos_10_Otras_442_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_442_d1,
    nivel = 0.10
  )

significativos_10_Otras_442_d1


# =========================================================
# 11. OTRAS LOCAL CONTRA 451 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-4-5-1"
)

m1_Otras_451 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_451)

resultados_Otras_451_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_451
  )

resultados_Otras_451_d1

# Significativos al 5 %
significativos_Otras_451_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_451_d1,
    nivel = 0.05
  )

significativos_Otras_451_d1

# Significativos o marginales al 10 %
significativos_10_Otras_451_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_451_d1,
    nivel = 0.10
  )

significativos_10_Otras_451_d1


# =========================================================
# 12. OTRAS LOCAL CONTRA 532 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-3-2"
)

m1_Otras_532 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_532)

resultados_Otras_532_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_532
  )

resultados_Otras_532_d1

# Significativos al 5 %
significativos_Otras_532_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_532_d1,
    nivel = 0.05
  )

significativos_Otras_532_d1

# Significativos o marginales al 10 %
significativos_10_Otras_532_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_532_d1,
    nivel = 0.10
  )

significativos_10_Otras_532_d1


# =========================================================
# 13. OTRAS LOCAL CONTRA 541 VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "1-5-4-1"
)

m1_Otras_541 <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_541)

resultados_Otras_541_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_541
  )

resultados_Otras_541_d1

# Significativos al 5 %
significativos_Otras_541_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_541_d1,
    nivel = 0.05
  )

significativos_Otras_541_d1

# Significativos o marginales al 10 %
significativos_10_Otras_541_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_541_d1,
    nivel = 0.10
  )

significativos_10_Otras_541_d1


# =========================================================
# 14. OTRAS LOCAL CONTRA OTRAS VISITANTE
# =========================================================

d1$formacion_visit_dep <- relevel(
  factor(d1$formacion_visit_dep),
  ref = "Otras"
)

m1_Otras_Otras <- multinom(
  resultado_partido_local ~
    formacion_local_dep +
    formacion_visit_dep,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

summary(m1_Otras_Otras)

resultados_Otras_Otras_d1 <-
  extraer_resultados_formaciones_multinom(
    m1_Otras_Otras
  )

resultados_Otras_Otras_d1

# Significativos al 5 %
significativos_Otras_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_Otras_d1,
    nivel = 0.05
  )

significativos_Otras_Otras_d1

# Significativos o marginales al 10 %
significativos_10_Otras_Otras_d1 <-
  extraer_significativos_formaciones(
    resultados_Otras_Otras_d1,
    nivel = 0.10
  )

significativos_10_Otras_Otras_d1

###################################################################
# =========================================================
# MATRIZ DE CONFUSIÓN DEL MODELO m1_Otras_Otras
# =========================================================


# =========================================================
# 1. PREDICCIÓN DE CLASE
# =========================================================

pred_Otras_Otras <- predict(
  m1_Otras_Otras,
  newdata = d1,
  type = "class"
)

# Primeras predicciones
head(pred_Otras_Otras)

# Distribución de resultados predichos
table(
  pred_Otras_Otras,
  useNA = "always"
)


# =========================================================
# 2. IGUALAR LOS NIVELES
# =========================================================

niveles_resultado_Otras_Otras <- levels(
  d1$resultado_partido_local
)

real_Otras_Otras <- factor(
  d1$resultado_partido_local,
  levels = niveles_resultado_Otras_Otras
)

pred_Otras_Otras <- factor(
  pred_Otras_Otras,
  levels = niveles_resultado_Otras_Otras
)


# =========================================================
# 3. MATRIZ DE CONFUSIÓN
# =========================================================

mc_Otras_Otras <- table(
  Real = real_Otras_Otras,
  Predicho = pred_Otras_Otras
)

mc_Otras_Otras

# Matriz con totales por filas y columnas
addmargins(mc_Otras_Otras)


# =========================================================
# 4. ACCURACY TOTAL
# =========================================================

accuracy_Otras_Otras <- sum(
  diag(mc_Otras_Otras)
) / sum(mc_Otras_Otras)

accuracy_Otras_Otras


# =========================================================
# 5. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_Otras_Otras <- diag(
  mc_Otras_Otras
) / rowSums(mc_Otras_Otras)

sensibilidad_Otras_Otras[
  is.nan(sensibilidad_Otras_Otras) |
    is.infinite(sensibilidad_Otras_Otras)
] <- NA

sensibilidad_Otras_Otras


# =========================================================
# 6. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_Otras_Otras <- diag(
  mc_Otras_Otras
) / colSums(mc_Otras_Otras)

precision_Otras_Otras[
  is.nan(precision_Otras_Otras) |
    is.infinite(precision_Otras_Otras)
] <- NA

precision_Otras_Otras


# =========================================================
# 7. BALANCED ACCURACY
# =========================================================

balanced_accuracy_Otras_Otras <- mean(
  sensibilidad_Otras_Otras,
  na.rm = TRUE
)

balanced_accuracy_Otras_Otras


# =========================================================
# 8. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_Otras_Otras <- data.frame(
  Categoria = niveles_resultado_Otras_Otras,
  Sensibilidad = round(
    as.numeric(sensibilidad_Otras_Otras),
    4
  ),
  Precision = round(
    as.numeric(precision_Otras_Otras),
    4
  )
)

metricas_Otras_Otras


