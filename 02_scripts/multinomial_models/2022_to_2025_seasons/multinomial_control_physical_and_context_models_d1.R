# ==============================================================================
# Title: Multi-season match-control, physical and contextual multinomial models
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops, simplifies and evaluates multinomial logistic
# regression models for three analytical dimensions of LaLiga match
# performance across multiple seasons: match control, physical performance
# and competitive context.
#
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat. The draw is used as the reference category, so the
# estimated coefficients compare home defeat versus draw and home victory
# versus draw.
#
# The match-control block examines possession, home and opposition passing,
# corners, free kicks and duels won. Passing-zone variables are excluded
# because they were not consistently collected across the multi-season
# dataset. The complete specification is simplified by removing home corners,
# and the resulting model is retained as the final match-control model.
#
# The physical block examines rest differences, fouls, tackles, duels, yellow
# cards, red cards and variables associated with dismissals and numerical
# advantages. Before model estimation, the script checks data availability,
# variable types, missing values, sample size and predictor variability.
# Structural missing values for the timing of red cards and numerical
# advantages are coded as zero when the corresponding event did not occur.
#
# The physical model is progressively simplified using global
# likelihood-ratio tests. Variables with limited contribution are removed
# across successive specifications, and the selected model is retained as the
# final physical multinomial model.
#
# The contextual block examines the competitive circumstances preceding each
# match. Its predictors include the difference in rest days, the previous
# league positions of the home and visiting teams and their recent form.
# The variable indicating the absence of previous rest information is excluded
# because it previously produced non-estimable standard errors due to
# insufficient variability.
#
# For the control, physical and contextual models, the script extracts
# coefficients, odds ratios and bilateral Wald p-values. Results are presented
# separately for home defeat versus draw and home victory versus draw, with
# statistical evidence identified at the 5% and 10% levels.
#
# Global likelihood-ratio tests are used to evaluate the overall contribution
# of each explanatory variable. Model fit is assessed through the Akaike
# information criterion, log-likelihood and pseudo-R-squared statistics.
#
# Predicted probabilities and outcome classes are calculated using the complete
# observations available for each model. Classification performance is
# evaluated through confusion matrices, overall accuracy, balanced accuracy,
# class-specific sensitivity and class-specific precision.
#
# Dataset:
#   d1, containing match-level observations from multiple LaLiga seasons.
#
# Dependent variable:
#   resultado_partido_local
#
# Outcome categories:
#   Victoria, Empate and Derrota
#
# Reference category:
#   Empate
#
# Statistical method:
#   Multinomial logistic regression estimated with nnet::multinom().
#
# Model-selection method:
#   Manual reduction supported by global likelihood-ratio tests, coefficient
#   significance, model fit and substantive interpretation.
#
# Match-control candidate models:
#   modelo_multinom_control_d1
#   modelo_multinom_control_d1_1
#
# Final match-control model:
#   modelo_multinom_control_d1_final
#
# Physical candidate models:
#   modelo_multinom_fisico_d1
#   modelo_multinom_fisico_d1_1 to modelo_multinom_fisico_d1_7
#
# Final physical model:
#   modelo_multinom_fisico_d1_final
#
# Contextual model:
#   modelo_multinom_contexto_d1
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, global likelihood-ratio tests,
#   AIC, log-likelihood, pseudo-R-squared, predicted probabilities, confusion
#   matrices, accuracy, balanced accuracy, class-specific sensitivity and
#   class-specific precision.
# ==============================================================================


# =========================================================
# 6. MODELO MULTINOMIAL DE CONTROL COMPLETO
# =========================================================
#quito las zonas de pases porque no se recopilaban

modelo_multinom_control_d1 <- multinom(
  resultado_partido_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    corners_local +
    corners_concedidos_local +
    tiros_libres_local +
    tiros_libres_concedidos_local +
    duelos_ganados_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

# Resumen
summary(modelo_multinom_control_d1)



# =========================================================
# 8. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_control_d1 <- extraer_resultados_multinom(
  modelo_multinom_control_d1
)

# Tabla completa
tabla_control_d1

# Tabla ordenada
tabla_control_d1 %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 9. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_control_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 10. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_control_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 11. DERROTA FRENTE A EMPATE
# =========================================================

tabla_control_derrota_d1 <- tabla_control_d1 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_control_derrota_d1


# =========================================================
# 12. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_control_victoria_d1 <- tabla_control_d1 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_control_victoria_d1


# =========================================================
# 13. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_control_d1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_control_d1,
  datos = d1
)

test_global_control_d1


# Variables globalmente significativas al 5 %
test_global_control_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# Variables significativas o marginales al 10 %
test_global_control_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 14. BONDAD DE AJUSTE
# =========================================================

AIC_control_d1 <- AIC(
  modelo_multinom_control_d1
)

logLik_control_d1 <- logLik(
  modelo_multinom_control_d1
)

pseudo_R2_control_d1 <- pR2(
  modelo_multinom_control_d1
)

AIC_control_d1
logLik_control_d1
pseudo_R2_control_d1


# =========================================================
# 15. DATOS UTILIZADOS POR EL MODELO
# =========================================================

datos_control_d1 <- model.frame(
  formula = formula(
    modelo_multinom_control_d1
  ),
  data = d1,
  na.action = na.omit
)

datos_control_d1 <- droplevels(
  datos_control_d1
)

# Número de filas utilizadas
nrow(datos_control_d1)


# =========================================================
# 16. PROBABILIDADES ESTIMADAS
# =========================================================

prob_control_d1 <- predict(
  modelo_multinom_control_d1,
  newdata = datos_control_d1,
  type = "probs"
)

head(prob_control_d1)

# En porcentaje
round(
  head(prob_control_d1 * 100),
  2
)

# Número de probabilidades
nrow(prob_control_d1)

# Comprobar que suman 1
rowSums(prob_control_d1)[1:10]

all(
  abs(
    rowSums(prob_control_d1) - 1
  ) < 1e-8
)


# =========================================================
# 17. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_control_d1 <- predict(
  modelo_multinom_control_d1,
  newdata = datos_control_d1,
  type = "class"
)

head(pred_control_d1)

table(
  pred_control_d1,
  useNA = "always"
)


# =========================================================
# 18. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado_control_d1 <- levels(
  d1$resultado_partido_local
)

real_control_d1 <- factor(
  datos_control_d1$resultado_partido_local,
  levels = niveles_resultado_control_d1
)

pred_control_d1 <- factor(
  pred_control_d1,
  levels = niveles_resultado_control_d1
)

# Comprobar longitudes
length(real_control_d1)
length(pred_control_d1)

length(real_control_d1) ==
  length(pred_control_d1)

# Matriz
mc_control_d1 <- table(
  Real = real_control_d1,
  Predicho = pred_control_d1
)

mc_control_d1
addmargins(mc_control_d1)


# =========================================================
# 19. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

accuracy_control_d1 <- sum(
  diag(mc_control_d1)
) / sum(mc_control_d1)

sensibilidad_control_d1 <- diag(
  mc_control_d1
) / rowSums(mc_control_d1)

precision_control_d1 <- diag(
  mc_control_d1
) / colSums(mc_control_d1)

sensibilidad_control_d1[
  is.nan(sensibilidad_control_d1) |
    is.infinite(sensibilidad_control_d1)
] <- NA

precision_control_d1[
  is.nan(precision_control_d1) |
    is.infinite(precision_control_d1)
] <- NA

balanced_accuracy_control_d1 <- mean(
  sensibilidad_control_d1,
  na.rm = TRUE
)

metricas_control_d1 <- data.frame(
  Categoria = niveles_resultado_control_d1,
  Sensibilidad = round(
    as.numeric(sensibilidad_control_d1),
    4
  ),
  Precision = round(
    as.numeric(precision_control_d1),
    4
  )
)

accuracy_control_d1
sensibilidad_control_d1
precision_control_d1
balanced_accuracy_control_d1
metricas_control_d1

# =========================================================
# 6. MODELO MULTINOMIAL DE CONTROL COMPLETO
# =========================================================
#quito las zonas de pases porque no se recopilaban

modelo_multinom_control_d1_1 <- multinom(
  resultado_partido_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    corners_concedidos_local +
    tiros_libres_local +
    tiros_libres_concedidos_local +
    duelos_ganados_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

# Resumen
summary(modelo_multinom_control_d1_1)

test_global_control_d1_1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_control_d1_1,
  datos = d1
)

test_global_control_d1_1

modelo_multinom_control_d1__final <-
  modelo_multinom_control_d1_1

# =========================================================
# BLOQUE CONTROL DEL PARTIDO MULTINOMIAL FINAL
# Base de datos: d1
# Categoría de referencia: Empate
# =========================================================

modelo_multinom_control_d1_final <- multinom(
  resultado_partido_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    corners_concedidos_local +
    tiros_libres_local +
    tiros_libres_concedidos_local +
    duelos_ganados_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)

# Resumen
summary(modelo_multinom_control_d1_final)

# =========================================================
# 6. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_control_d1_final <- extraer_resultados_multinom(
  modelo_multinom_control_d1_final
)

# Tabla completa
tabla_control_d1_final

# Tabla ordenada
tabla_control_d1_final %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 7. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_control_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 8. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_control_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 9. DERROTA FRENTE A EMPATE
# =========================================================

tabla_control_derrota_d1_final <-
  tabla_control_d1_final %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_control_derrota_d1_final


# =========================================================
# 10. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_control_victoria_d1_final <-
  tabla_control_d1_final %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_control_victoria_d1_final


# =========================================================
# 11. TEST GLOBAL DE CADA VARIABLE
# =========================================================

# Se utiliza la función test_lr_multinom()
# que ya tienes definida con dos argumentos.

test_global_control_d1_final <- test_lr_multinom(
  modelo_completo = modelo_multinom_control_d1_final,
  datos = d1
)

test_global_control_d1_final


# Variables globalmente significativas al 5 %
test_global_control_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# Variables globalmente significativas o marginales al 10 %
test_global_control_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 12. BONDAD DE AJUSTE
# =========================================================

AIC_control_d1_final <- AIC(
  modelo_multinom_control_d1_final
)

logLik_control_d1_final <- logLik(
  modelo_multinom_control_d1_final
)

pseudo_R2_control_d1_final <- pR2(
  modelo_multinom_control_d1_final
)

AIC_control_d1_final
logLik_control_d1_final
pseudo_R2_control_d1_final



# =========================================================
# 14. PROBABILIDADES ESTIMADAS
# =========================================================

prob_control_d1_final <- predict(
  modelo_multinom_control_d1_final,
  newdata = datos_control_d1_final,
  type = "probs"
)

# Primeras probabilidades
head(prob_control_d1_final)

# Primeras probabilidades en porcentaje
round(
  head(prob_control_d1_final * 100),
  2
)

# Número de partidos
nrow(prob_control_d1_final)

# Comprobar las primeras sumas
rowSums(
  prob_control_d1_final
)[1:10]

# Comprobar todas las probabilidades
all(
  abs(
    rowSums(prob_control_d1_final) - 1
  ) < 1e-8
)


# =========================================================
# 15. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_control_d1_final <- predict(
  modelo_multinom_control_d1_final,
  newdata = datos_control_d1_final,
  type = "class"
)

# Primeras predicciones
head(pred_control_d1_final)

# Distribución de las predicciones
table(
  pred_control_d1_final,
  useNA = "always"
)


# =========================================================
# 16. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_control_d1_final <- levels(
  d1$resultado_partido_local
)

niveles_resultado_control_d1_final


# Resultados reales
real_control_d1_final <- factor(
  datos_control_d1_final$resultado_partido_local,
  levels = niveles_resultado_control_d1_final
)


# Predicciones con los mismos niveles
pred_control_d1_final <- factor(
  pred_control_d1_final,
  levels = niveles_resultado_control_d1_final
)


# Comprobar longitudes
length(real_control_d1_final)
length(pred_control_d1_final)

length(real_control_d1_final) ==
  length(pred_control_d1_final)


# Matriz de confusión
mc_control_d1_final <- table(
  Real = real_control_d1_final,
  Predicho = pred_control_d1_final
)

mc_control_d1_final

# Matriz con totales
addmargins(
  mc_control_d1_final
)


# =========================================================
# 17. ACCURACY TOTAL
# =========================================================

accuracy_control_d1_final <- sum(
  diag(mc_control_d1_final)
) / sum(mc_control_d1_final)

accuracy_control_d1_final


# =========================================================
# 18. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_control_d1_final <- diag(
  mc_control_d1_final
) / rowSums(mc_control_d1_final)

# Evitar NaN o Inf
sensibilidad_control_d1_final[
  is.nan(sensibilidad_control_d1_final) |
    is.infinite(sensibilidad_control_d1_final)
] <- NA

sensibilidad_control_d1_final


# =========================================================
# 19. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_control_d1_final <- diag(
  mc_control_d1_final
) / colSums(mc_control_d1_final)

# Evitar NaN o Inf
precision_control_d1_final[
  is.nan(precision_control_d1_final) |
    is.infinite(precision_control_d1_final)
] <- NA

precision_control_d1_final


# =========================================================
# 20. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_control_d1_final <- mean(
  sensibilidad_control_d1_final,
  na.rm = TRUE
)

balanced_accuracy_control_d1_final


# =========================================================
# 21. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_control_d1_final <- data.frame(
  Categoria = niveles_resultado_control_d1_final,
  Sensibilidad = round(
    as.numeric(
      sensibilidad_control_d1_final
    ),
    4
  ),
  Precision = round(
    as.numeric(
      precision_control_d1_final
    ),
    4
  )
)

metricas_control_d1_final


# =========================================================
# 22. TABLA DE MÉTRICAS GENERALES
# =========================================================

resultados_clasificacion_control_d1_final <- data.frame(
  Metrica = c(
    "Accuracy",
    "Balanced Accuracy"
  ),
  Valor = c(
    accuracy_control_d1_final,
    balanced_accuracy_control_d1_final
  )
)

resultados_clasificacion_control_d1_final <-
  resultados_clasificacion_control_d1_final %>%
  mutate(
    Valor = round(
      Valor,
      4
    )
  )

resultados_clasificacion_control_d1_final

##################################################
# =========================================================
# BLOQUE FÍSICO MULTINOMIAL COMPLETO
# Base de datos: d1
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. PREPARAR LA VARIABLE RESPUESTA
# =========================================================

# Limpiar posibles espacios
d1$resultado_partido_local <- trimws(
  as.character(d1$resultado_partido_local)
)

# Convertir a factor
d1$resultado_partido_local <- factor(
  d1$resultado_partido_local
)

# Establecer Empate como categoría de referencia
d1$resultado_partido_local <- relevel(
  d1$resultado_partido_local,
  ref = "Empate"
)

# Comprobar categorías
levels(d1$resultado_partido_local)

# Distribución absoluta
table(
  d1$resultado_partido_local,
  useNA = "always"
)

# Distribución relativa
prop.table(
  table(d1$resultado_partido_local)
)


# =========================================================
# 2. VARIABLES DEL BLOQUE FÍSICO
# =========================================================

variables_fisicas_d1 <- c(
  "diff_descanso",
  "faltas_local",
  "faltas_recibidas_local",
  "entradas_local",
  "entradas_ganadas_local",
  "entradas_concedidas_local",
  "entradas_ganadas_concedidas_local",
  "duelos_ganados_local",
  "amarillas_local",
  "amarillas_forzadas_local",
  "rojas_local",
  "rojas_forzadas_local",
  "min_roja_local",
  "min_ventaja_numerica_local"
)


# =========================================================
# 3. COMPROBAR QUE EXISTEN TODAS LAS VARIABLES
# =========================================================

variables_fisicas_no_encontradas_d1 <-
  variables_fisicas_d1[
    !variables_fisicas_d1 %in% names(d1)
  ]

variables_fisicas_no_encontradas_d1

# Si devuelve character(0), todas las variables existen.


# =========================================================
# 4. COMPROBAR EL TIPO DE LAS VARIABLES
# =========================================================

str(
  d1[
    variables_fisicas_d1[
      variables_fisicas_d1 %in% names(d1)
    ]
  ]
)

# Las variables deberían estar almacenadas como numéricas.


# =========================================================
# 5. TRATAR VALORES AUSENTES ESTRUCTURALES
# =========================================================

# Esta sustitución solo es correcta si NA significa que
# el acontecimiento no se produjo durante el partido.

# Si no hubo expulsión local, el minuto se codifica como 0
d1$min_roja_local[
  is.na(d1$min_roja_local)
] <- 0

# Si no hubo ventaja numérica, el minuto se codifica como 0
d1$min_ventaja_numerica_local[
  is.na(d1$min_ventaja_numerica_local)
] <- 0


# Comprobar las distribuciones después de la transformación
summary(d1$min_roja_local)

summary(
  d1$min_ventaja_numerica_local
)

table(
  d1$min_roja_local,
  useNA = "always"
)

table(
  d1$min_ventaja_numerica_local,
  useNA = "always"
)


# =========================================================
# 6. COMPROBAR VALORES AUSENTES
# =========================================================

valores_ausentes_fisico_d1 <- colSums(
  is.na(
    d1[
      c(
        "resultado_partido_local",
        variables_fisicas_d1
      )
    ]
  )
)

valores_ausentes_fisico_d1


# Número de observaciones completas
filas_completas_fisico_d1 <- sum(
  complete.cases(
    d1[
      c(
        "resultado_partido_local",
        variables_fisicas_d1
      )
    ]
  )
)

filas_completas_fisico_d1


# Número de observaciones que se excluirían
filas_excluidas_fisico_d1 <- sum(
  !complete.cases(
    d1[
      c(
        "resultado_partido_local",
        variables_fisicas_d1
      )
    ]
  )
)

filas_excluidas_fisico_d1


# =========================================================
# 7. COMPROBAR VARIABILIDAD DE LAS VARIABLES
# =========================================================

variabilidad_fisico_d1 <- data.frame(
  Variable = variables_fisicas_d1,
  
  Valores_unicos = sapply(
    d1[variables_fisicas_d1],
    function(x) {
      length(
        unique(
          x[!is.na(x)]
        )
      )
    }
  ),
  
  Varianza = sapply(
    d1[variables_fisicas_d1],
    function(x) {
      if (is.numeric(x)) {
        var(
          x,
          na.rm = TRUE
        )
      } else {
        NA_real_
      }
    }
  )
)

variabilidad_fisico_d1


# Mostrar variables constantes, si las hubiera
variabilidad_fisico_d1 %>%
  filter(Valores_unicos <= 1)

# Si aparece alguna variable con un solo valor único
# o varianza igual a 0, deberá excluirse del modelo.


# =========================================================
# 8. MODELO FÍSICO MULTINOMIAL COMPLETO
# =========================================================

modelo_multinom_fisico_d1 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_ganadas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_local +
    amarillas_forzadas_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1)





# =========================================================
# 10. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_fisico_d1 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d1
)


# Tabla completa
tabla_fisico_d1


# Tabla completa ordenada
tabla_fisico_d1 %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 11. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_fisico_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 12. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_fisico_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 13. DERROTA FRENTE A EMPATE
# =========================================================

tabla_fisico_derrota_d1 <- tabla_fisico_d1 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_fisico_derrota_d1


# =========================================================
# 14. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_fisico_victoria_d1 <- tabla_fisico_d1 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_fisico_victoria_d1


# =========================================================
# 15. TEST GLOBAL DE CADA VARIABLE
# =========================================================

# Se utiliza la función test_lr_multinom()
# que ya tienes definida con dos argumentos.

test_global_fisico_d1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1,
  datos = d1
)

test_global_fisico_d1


# =========================================================
# 16. VARIABLES GLOBALMENTE SIGNIFICATIVAS AL 5 %
# =========================================================

test_global_fisico_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# =========================================================
# 17. VARIABLES GLOBALES SIGNIFICATIVAS O MARGINALES AL 10 %
# =========================================================

test_global_fisico_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 18. BONDAD DE AJUSTE
# =========================================================

AIC_fisico_d1 <- AIC(
  modelo_multinom_fisico_d1
)

logLik_fisico_d1 <- logLik(
  modelo_multinom_fisico_d1
)

pseudo_R2_fisico_d1 <- pR2(
  modelo_multinom_fisico_d1
)


# Mostrar medidas de ajuste
AIC_fisico_d1
logLik_fisico_d1
pseudo_R2_fisico_d1


# =========================================================
# 19. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Recuperar exactamente las observaciones utilizadas
# por el modelo físico.

datos_fisico_d1 <- model.frame(
  formula = formula(
    modelo_multinom_fisico_d1
  ),
  data = d1,
  na.action = na.omit
)


# Eliminar posibles niveles no utilizados
datos_fisico_d1 <- droplevels(
  datos_fisico_d1
)


# Número de observaciones completas
nrow(datos_fisico_d1)




# =========================================================
# 20. PROBABILIDADES ESTIMADAS
# =========================================================

prob_fisico_d1 <- predict(
  modelo_multinom_fisico_d1,
  newdata = datos_fisico_d1,
  type = "probs"
)


# Primeras probabilidades en escala 0-1
head(prob_fisico_d1)


# Primeras probabilidades en porcentaje
round(
  head(prob_fisico_d1 * 100),
  2
)


# Número total de probabilidades estimadas
nrow(prob_fisico_d1)


# Comprobar las primeras sumas
rowSums(
  prob_fisico_d1
)[1:10]


# Comprobar todas las probabilidades
all(
  abs(
    rowSums(prob_fisico_d1) - 1
  ) < 1e-8
)


# =========================================================
# 21. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_fisico_d1 <- predict(
  modelo_multinom_fisico_d1,
  newdata = datos_fisico_d1,
  type = "class"
)


# Primeras predicciones
head(pred_fisico_d1)


# Distribución de los resultados predichos
table(
  pred_fisico_d1,
  useNA = "always"
)


# =========================================================
# 22. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_fisico_d1 <- levels(
  d1$resultado_partido_local
)

niveles_resultado_fisico_d1


# Resultados reales
real_fisico_d1 <- factor(
  datos_fisico_d1$resultado_partido_local,
  levels = niveles_resultado_fisico_d1
)


# Predicciones con los mismos niveles
pred_fisico_d1 <- factor(
  pred_fisico_d1,
  levels = niveles_resultado_fisico_d1
)


# Comprobar longitudes
length(real_fisico_d1)
length(pred_fisico_d1)


# Las longitudes deben coincidir
length(real_fisico_d1) ==
  length(pred_fisico_d1)


# Construir la matriz de confusión
mc_fisico_d1 <- table(
  Real = real_fisico_d1,
  Predicho = pred_fisico_d1
)

mc_fisico_d1


# Matriz con totales
addmargins(
  mc_fisico_d1
)


# =========================================================
# 23. ACCURACY TOTAL
# =========================================================

accuracy_fisico_d1 <- sum(
  diag(mc_fisico_d1)
) / sum(mc_fisico_d1)

accuracy_fisico_d1


# =========================================================
# 24. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_fisico_d1 <- diag(
  mc_fisico_d1
) / rowSums(mc_fisico_d1)


# Evitar NaN o Inf
sensibilidad_fisico_d1[
  is.nan(sensibilidad_fisico_d1) |
    is.infinite(sensibilidad_fisico_d1)
] <- NA

sensibilidad_fisico_d1


# =========================================================
# 25. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_fisico_d1 <- diag(
  mc_fisico_d1
) / colSums(mc_fisico_d1)


# Evitar NaN o Inf
precision_fisico_d1[
  is.nan(precision_fisico_d1) |
    is.infinite(precision_fisico_d1)
] <- NA

precision_fisico_d1


# =========================================================
# 26. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_fisico_d1 <- mean(
  sensibilidad_fisico_d1,
  na.rm = TRUE
)

balanced_accuracy_fisico_d1


# =========================================================
# 27. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_fisico_d1 <- data.frame(
  Categoria = niveles_resultado_fisico_d1,
  
  Sensibilidad = round(
    as.numeric(
      sensibilidad_fisico_d1
    ),
    4
  ),
  
  Precision = round(
    as.numeric(
      precision_fisico_d1
    ),
    4
  )
)

metricas_fisico_d1


# =========================================================
# 28. TABLA DE MÉTRICAS GENERALES
# =========================================================

resultados_clasificacion_fisico_d1 <- data.frame(
  Metrica = c(
    "Accuracy",
    "Balanced Accuracy"
  ),
  
  Valor = c(
    accuracy_fisico_d1,
    balanced_accuracy_fisico_d1
  )
)


# Redondear los resultados
resultados_clasificacion_fisico_d1 <-
  resultados_clasificacion_fisico_d1 %>%
  mutate(
    Valor = round(
      Valor,
      4
    )
  )

resultados_clasificacion_fisico_d1


#depuro el modelo fisico, quito amarillas forzadas local
modelo_multinom_fisico_d1_1 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_ganadas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1_1)

test_global_fisico_d1_1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_1,
  datos = d1
)

test_global_fisico_d1_1
#luego entradas ganadas local
modelo_multinom_fisico_d1_2 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1_2)

test_global_fisico_d1_2 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_2,
  datos = d1
)

test_global_fisico_d1_2

#ahora min roja local
modelo_multinom_fisico_d1_3 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1_3)

test_global_fisico_d1_3 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_3,
  datos = d1
)

test_global_fisico_d1_3

#ahora entradas local

modelo_multinom_fisico_d1_4 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1_4)

test_global_fisico_d1_4 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_4,
  datos = d1
)

test_global_fisico_d1_4

#entradas_ganadas_concedidas_local  

modelo_multinom_fisico_d1_5 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    amarillas_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1_5)

test_global_fisico_d1_5 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_5,
  datos = d1
)

test_global_fisico_d1_5

#amarillas local

modelo_multinom_fisico_d1_6 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1_6)

test_global_fisico_d1_6 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_6,
  datos = d1
)

test_global_fisico_d1_6

#ahora rojas forzadas local

modelo_multinom_fisico_d1_7 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_fisico_d1_7)

test_global_fisico_d1_7 <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_7,
  datos = d1
)

test_global_fisico_d1_7

#veo faltas local y faltas recibidas, empeora el modelo asi que me quedo aqui

# =========================================================
# BLOQUE FÍSICO MULTINOMIAL FINAL
# Base de datos: d1
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. ASIGNAR EL MODELO FÍSICO FINAL
# =========================================================

# El modelo físico d1_7 pasa a ser el modelo final
modelo_multinom_fisico_d1_final <-
  modelo_multinom_fisico_d1_7


# Resumen general
summary(modelo_multinom_fisico_d1_final)




# =========================================================
# 7. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_fisico_d1_final <- extraer_resultados_multinom(
  modelo_multinom_fisico_d1_final
)


# Tabla completa
tabla_fisico_d1_final


# Tabla completa ordenada
tabla_fisico_d1_final %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 8. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_fisico_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 9. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_fisico_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 10. DERROTA FRENTE A EMPATE
# =========================================================

tabla_fisico_derrota_d1_final <-
  tabla_fisico_d1_final %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_fisico_derrota_d1_final


# =========================================================
# 11. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_fisico_victoria_d1_final <-
  tabla_fisico_d1_final %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_fisico_victoria_d1_final


# =========================================================
# 12. TEST GLOBAL DE CADA VARIABLE
# =========================================================

# La función test_lr_multinom() debe estar definida previamente.

test_global_fisico_d1_final <- test_lr_multinom(
  modelo_completo = modelo_multinom_fisico_d1_final,
  datos = d1
)

test_global_fisico_d1_final


# =========================================================
# 13. VARIABLES GLOBALMENTE SIGNIFICATIVAS AL 5 %
# =========================================================

test_global_fisico_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# =========================================================
# 14. VARIABLES GLOBALES SIGNIFICATIVAS O MARGINALES AL 10 %
# =========================================================

test_global_fisico_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 15. BONDAD DE AJUSTE
# =========================================================

AIC_fisico_d1_final <- AIC(
  modelo_multinom_fisico_d1_final
)

logLik_fisico_d1_final <- logLik(
  modelo_multinom_fisico_d1_final
)

pseudo_R2_fisico_d1_final <- pR2(
  modelo_multinom_fisico_d1_final
)


# Mostrar las medidas de ajuste
AIC_fisico_d1_final
logLik_fisico_d1_final
pseudo_R2_fisico_d1_final


# =========================================================
# 16. DATOS UTILIZADOS POR EL MODELO FINAL
# =========================================================

# Recuperar exactamente las observaciones completas
# utilizadas por el modelo físico final.

datos_fisico_d1_final <- model.frame(
  formula = formula(
    modelo_multinom_fisico_d1_final
  ),
  data = d1,
  na.action = na.omit
)




# =========================================================
# 17. PROBABILIDADES ESTIMADAS
# =========================================================

prob_fisico_d1_final <- predict(
  modelo_multinom_fisico_d1_final,
  newdata = datos_fisico_d1_final,
  type = "probs"
)


# Primeras probabilidades en escala 0-1
head(prob_fisico_d1_final)


# Primeras probabilidades en porcentaje
round(
  head(prob_fisico_d1_final * 100),
  2
)


# Número total de partidos con probabilidades
nrow(prob_fisico_d1_final)


# Comprobar que las primeras probabilidades suman 1
rowSums(
  prob_fisico_d1_final
)[1:10]


# Comprobar que todas suman aproximadamente 1
all(
  abs(
    rowSums(prob_fisico_d1_final) - 1
  ) < 1e-8
)


# =========================================================
# 18. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_fisico_d1_final <- predict(
  modelo_multinom_fisico_d1_final,
  newdata = datos_fisico_d1_final,
  type = "class"
)


# Primeras predicciones
head(pred_fisico_d1_final)


# Distribución de los resultados predichos
table(
  pred_fisico_d1_final,
  useNA = "always"
)


# =========================================================
# 19. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_fisico_d1_final <- levels(
  d1$resultado_partido_local
)

niveles_resultado_fisico_d1_final


# Resultados reales
real_fisico_d1_final <- factor(
  datos_fisico_d1_final$resultado_partido_local,
  levels = niveles_resultado_fisico_d1_final
)


# Predicciones con los mismos niveles
pred_fisico_d1_final <- factor(
  pred_fisico_d1_final,
  levels = niveles_resultado_fisico_d1_final
)


# Comprobar longitudes
length(real_fisico_d1_final)
length(pred_fisico_d1_final)


# Deben coincidir
length(real_fisico_d1_final) ==
  length(pred_fisico_d1_final)


# Matriz de confusión
mc_fisico_d1_final <- table(
  Real = real_fisico_d1_final,
  Predicho = pred_fisico_d1_final
)

mc_fisico_d1_final


# Matriz con totales por filas y columnas
addmargins(
  mc_fisico_d1_final
)


# =========================================================
# 20. ACCURACY TOTAL
# =========================================================

accuracy_fisico_d1_final <- sum(
  diag(mc_fisico_d1_final)
) / sum(mc_fisico_d1_final)

accuracy_fisico_d1_final


# =========================================================
# 21. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_fisico_d1_final <- diag(
  mc_fisico_d1_final
) / rowSums(mc_fisico_d1_final)


# Evitar NaN o Inf
sensibilidad_fisico_d1_final[
  is.nan(sensibilidad_fisico_d1_final) |
    is.infinite(sensibilidad_fisico_d1_final)
] <- NA

sensibilidad_fisico_d1_final


# =========================================================
# 22. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_fisico_d1_final <- diag(
  mc_fisico_d1_final
) / colSums(mc_fisico_d1_final)


# Evitar NaN o Inf
precision_fisico_d1_final[
  is.nan(precision_fisico_d1_final) |
    is.infinite(precision_fisico_d1_final)
] <- NA

precision_fisico_d1_final


# =========================================================
# 23. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_fisico_d1_final <- mean(
  sensibilidad_fisico_d1_final,
  na.rm = TRUE
)

balanced_accuracy_fisico_d1_final


# =========================================================
# 24. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_fisico_d1_final <- data.frame(
  Categoria = niveles_resultado_fisico_d1_final,
  Sensibilidad = round(
    as.numeric(
      sensibilidad_fisico_d1_final
    ),
    4
  ),
  Precision = round(
    as.numeric(
      precision_fisico_d1_final
    ),
    4
  )
)

metricas_fisico_d1_final


#ahora paso al modelo de contexto
# =========================================================
# BLOQUE DE CONTEXTO MULTINOMIAL
# Base de datos: d1
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. PREPARAR LA VARIABLE RESPUESTA
# =========================================================

# Limpiar posibles espacios
d1$resultado_partido_local <- trimws(
  as.character(d1$resultado_partido_local)
)

# Convertir a factor
d1$resultado_partido_local <- factor(
  d1$resultado_partido_local
)

# Establecer Empate como categoría de referencia
d1$resultado_partido_local <- relevel(
  d1$resultado_partido_local,
  ref = "Empate"
)

# Comprobar los niveles
levels(d1$resultado_partido_local)

# Distribución absoluta
table(
  d1$resultado_partido_local,
  useNA = "always"
)

# Distribución relativa
prop.table(
  table(d1$resultado_partido_local)
)


# =========================================================
# 2. VARIABLES DEL BLOQUE DE CONTEXTO
# =========================================================

variables_contexto_d1 <- c(
  "diff_descanso",
  "pos_previa_local",
  "pos_previa_visitante",
  "forma_local_5",
  "forma_visitante_5"
)

# No se incluye no_descanso_previo porque anteriormente
# produjo errores estándar NaN debido a falta de variabilidad.


# =========================================================
# 3. COMPROBAR QUE EXISTEN TODAS LAS VARIABLES
# =========================================================

variables_contexto_no_encontradas_d1 <-
  variables_contexto_d1[
    !variables_contexto_d1 %in% names(d1)
  ]

variables_contexto_no_encontradas_d1

# Si devuelve character(0), todas las variables existen.


# =========================================================
# 4. COMPROBAR LOS TIPOS DE LAS VARIABLES
# =========================================================

str(
  d1[
    variables_contexto_d1[
      variables_contexto_d1 %in% names(d1)
    ]
  ]
)

# Todas las variables deberían ser numéricas.


# =========================================================
# 5. ASEGURAR QUE LAS VARIABLES SON NUMÉRICAS
# =========================================================

d1$diff_descanso <- as.numeric(
  as.character(d1$diff_descanso)
)

d1$pos_previa_local <- as.numeric(
  as.character(d1$pos_previa_local)
)

d1$pos_previa_visitante <- as.numeric(
  as.character(d1$pos_previa_visitante)
)

d1$forma_local_5 <- as.numeric(
  as.character(d1$forma_local_5)
)

d1$forma_visitante_5 <- as.numeric(
  as.character(d1$forma_visitante_5)
)


# Comprobar nuevamente las estructuras
str(
  d1[
    variables_contexto_d1
  ]
)


# =========================================================
# 6. COMPROBAR VALORES AUSENTES
# =========================================================

valores_ausentes_contexto_d1 <- colSums(
  is.na(
    d1[
      c(
        "resultado_partido_local",
        variables_contexto_d1
      )
    ]
  )
)

valores_ausentes_contexto_d1


# Número de observaciones completas
filas_completas_contexto_d1 <- sum(
  complete.cases(
    d1[
      c(
        "resultado_partido_local",
        variables_contexto_d1
      )
    ]
  )
)

filas_completas_contexto_d1


# Número de observaciones excluidas
filas_excluidas_contexto_d1 <- sum(
  !complete.cases(
    d1[
      c(
        "resultado_partido_local",
        variables_contexto_d1
      )
    ]
  )
)

filas_excluidas_contexto_d1


# =========================================================
# 7. COMPROBAR VARIABILIDAD DE LAS VARIABLES
# =========================================================

variabilidad_contexto_d1 <- data.frame(
  Variable = variables_contexto_d1,
  
  Valores_unicos = sapply(
    d1[variables_contexto_d1],
    function(x) {
      length(
        unique(
          x[!is.na(x)]
        )
      )
    }
  ),
  
  Varianza = sapply(
    d1[variables_contexto_d1],
    function(x) {
      if (is.numeric(x)) {
        var(
          x,
          na.rm = TRUE
        )
      } else {
        NA_real_
      }
    }
  )
)

variabilidad_contexto_d1


# Mostrar variables constantes, si las hubiera
variabilidad_contexto_d1 %>%
  filter(Valores_unicos <= 1)

# Si no devuelve ninguna fila, todas las variables
# presentan variación.


# =========================================================
# 8. MODELO MULTINOMIAL DE CONTEXTO
# =========================================================

modelo_multinom_contexto_d1 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_contexto_d1)


# Comprobar la fórmula utilizada
formula(modelo_multinom_contexto_d1)


# Número de observaciones utilizadas
nobs(modelo_multinom_contexto_d1)


# =========================================================
# 9. COMPROBAR ERRORES ESTÁNDAR
# =========================================================

resumen_contexto_d1 <- summary(
  modelo_multinom_contexto_d1
)


# Mostrar coeficientes
resumen_contexto_d1$coefficients


# Mostrar errores estándar
resumen_contexto_d1$standard.errors


# Comprobar si aparece algún NaN
any(
  is.nan(
    resumen_contexto_d1$standard.errors
  )
)


# Comprobar si existe algún valor no finito
any(
  !is.finite(
    resumen_contexto_d1$standard.errors
  )
)

# Los dos resultados deberían ser FALSE.


# =========================================================
# 10. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_contexto_d1 <- extraer_resultados_multinom(
  modelo_multinom_contexto_d1
)


# Mostrar tabla completa
tabla_contexto_d1


# Mostrar tabla completa ordenada
tabla_contexto_d1 %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 11. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_contexto_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 12. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_contexto_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 13. DERROTA FRENTE A EMPATE
# =========================================================

tabla_contexto_derrota_d1 <- tabla_contexto_d1 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_contexto_derrota_d1


# =========================================================
# 14. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_contexto_victoria_d1 <- tabla_contexto_d1 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_contexto_victoria_d1


# =========================================================
# 15. TEST GLOBAL DE CADA VARIABLE
# =========================================================

# Se utiliza la función test_lr_multinom()
# que ya tienes definida con dos argumentos.

test_global_contexto_d1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_contexto_d1,
  datos = d1
)

test_global_contexto_d1


# =========================================================
# 16. VARIABLES GLOBALMENTE SIGNIFICATIVAS AL 5 %
# =========================================================

test_global_contexto_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# =========================================================
# 17. VARIABLES GLOBALES SIGNIFICATIVAS O MARGINALES AL 10 %
# =========================================================

test_global_contexto_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 18. BONDAD DE AJUSTE
# =========================================================

AIC_contexto_d1 <- AIC(
  modelo_multinom_contexto_d1
)

logLik_contexto_d1 <- logLik(
  modelo_multinom_contexto_d1
)

pseudo_R2_contexto_d1 <- pR2(
  modelo_multinom_contexto_d1
)


# Mostrar las medidas de ajuste
AIC_contexto_d1
logLik_contexto_d1
pseudo_R2_contexto_d1


# =========================================================
# 19. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Recuperar exactamente las observaciones completas
# utilizadas por el modelo de contexto.

datos_contexto_d1 <- model.frame(
  formula = formula(
    modelo_multinom_contexto_d1
  ),
  data = d1,
  na.action = na.omit
)


# Eliminar niveles no utilizados
datos_contexto_d1 <- droplevels(
  datos_contexto_d1
)


# Número de observaciones completas
nrow(datos_contexto_d1)




# =========================================================
# 20. PROBABILIDADES ESTIMADAS
# =========================================================

prob_contexto_d1 <- predict(
  modelo_multinom_contexto_d1,
  newdata = datos_contexto_d1,
  type = "probs"
)


# Primeras probabilidades en escala 0-1
head(prob_contexto_d1)


# Primeras probabilidades en porcentaje
round(
  head(prob_contexto_d1 * 100),
  2
)


# Número de partidos con probabilidades estimadas
nrow(prob_contexto_d1)


# Comprobar las primeras sumas
rowSums(
  prob_contexto_d1
)[1:10]


# Comprobar que todas las probabilidades suman aproximadamente 1
all(
  abs(
    rowSums(prob_contexto_d1) - 1
  ) < 1e-8
)


# =========================================================
# 21. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_contexto_d1 <- predict(
  modelo_multinom_contexto_d1,
  newdata = datos_contexto_d1,
  type = "class"
)


# Primeras predicciones
head(pred_contexto_d1)


# Distribución de los resultados predichos
table(
  pred_contexto_d1,
  useNA = "always"
)


# =========================================================
# 22. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_contexto_d1 <- levels(
  d1$resultado_partido_local
)

niveles_resultado_contexto_d1


# Resultados observados
real_contexto_d1 <- factor(
  datos_contexto_d1$resultado_partido_local,
  levels = niveles_resultado_contexto_d1
)


# Predicciones con los mismos niveles
pred_contexto_d1 <- factor(
  pred_contexto_d1,
  levels = niveles_resultado_contexto_d1
)


# Comprobar longitudes
length(real_contexto_d1)
length(pred_contexto_d1)


# Las longitudes deben coincidir
length(real_contexto_d1) ==
  length(pred_contexto_d1)


# Construir la matriz de confusión
mc_contexto_d1 <- table(
  Real = real_contexto_d1,
  Predicho = pred_contexto_d1
)

mc_contexto_d1


# Matriz con totales
addmargins(
  mc_contexto_d1
)


# =========================================================
# 23. ACCURACY TOTAL
# =========================================================

accuracy_contexto_d1 <- sum(
  diag(mc_contexto_d1)
) / sum(mc_contexto_d1)

accuracy_contexto_d1


# =========================================================
# 24. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_contexto_d1 <- diag(
  mc_contexto_d1
) / rowSums(mc_contexto_d1)


# Evitar NaN o Inf
sensibilidad_contexto_d1[
  is.nan(sensibilidad_contexto_d1) |
    is.infinite(sensibilidad_contexto_d1)
] <- NA

sensibilidad_contexto_d1


# =========================================================
# 25. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_contexto_d1 <- diag(
  mc_contexto_d1
) / colSums(mc_contexto_d1)


# Evitar NaN o Inf
precision_contexto_d1[
  is.nan(precision_contexto_d1) |
    is.infinite(precision_contexto_d1)
] <- NA

precision_contexto_d1


# =========================================================
# 26. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_contexto_d1 <- mean(
  sensibilidad_contexto_d1,
  na.rm = TRUE
)

balanced_accuracy_contexto_d1


# =========================================================
# 27. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_contexto_d1 <- data.frame(
  Categoria = niveles_resultado_contexto_d1,
  
  Sensibilidad = round(
    as.numeric(
      sensibilidad_contexto_d1
    ),
    4
  ),
  
  Precision = round(
    as.numeric(
      precision_contexto_d1
    ),
    4
  )
)

metricas_contexto_d1


# =========================================================
# 28. TABLA DE MÉTRICAS GENERALES
# =========================================================

resultados_clasificacion_contexto_d1 <- data.frame(
  Metrica = c(
    "Accuracy",
    "Balanced Accuracy"
  ),
  
  Valor = c(
    accuracy_contexto_d1,
    balanced_accuracy_contexto_d1
  )
)


# Redondear resultados
resultados_clasificacion_contexto_d1 <-
  resultados_clasificacion_contexto_d1 %>%
  mutate(
    Valor = round(
      Valor,
      4
    )
  )

resultados_clasificacion_contexto_d1


