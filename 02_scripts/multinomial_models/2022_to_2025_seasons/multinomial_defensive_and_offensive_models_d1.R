# ==============================================================================
# Title: Multi-season defensive and offensive multinomial models
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops, simplifies and evaluates defensive and offensive
# multinomial logistic regression models using the multi-season LaLiga dataset.
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat, with the draw used as the reference category.
#
# The defensive block initially includes expected goals against, big chances
# conceded, shots on target conceded, corners conceded, passes and long passes
# conceded, crosses conceded, interceptions, clearances and disciplinary
# variables. Global likelihood-ratio tests are used to assess the contribution
# of each predictor. The model is subsequently reduced by removing variables
# with limited global contribution, producing a final defensive specification.
#
# The offensive block includes shots on target, big chances, passes in the
# final third, crosses, red-card variables and clearances by the home team and
# its opponent. Variables unavailable or without variation across the
# multi-season dataset are excluded. The offensive specification is simplified
# using global likelihood-ratio tests to obtain the final offensive model.
#
# The script defines an updated likelihood-ratio testing function that compares
# each complete multinomial model with reduced specifications estimated from
# exactly the same complete observations. This function provides the LR
# statistic, degrees of freedom, p-value and significance classification for
# every explanatory variable.
#
# For the final defensive and offensive models, the script extracts
# coefficients, odds ratios and bilateral Wald p-values. Results are reported
# separately for home defeat versus draw and home victory versus draw, with
# statistical evidence identified at the 5% and 10% levels.
#
# Model fit is assessed using the Akaike information criterion, log-likelihood
# and pseudo-R-squared statistics. Predicted probabilities and outcome classes
# are calculated using the complete observations employed by each model.
#
# Classification performance is evaluated through confusion matrices, overall
# accuracy, balanced accuracy, class-specific sensitivity and class-specific
# precision. Summary tables are also created for the principal classification
# metrics of the final defensive and offensive models.
#
# Dataset:
#   d1, containing multiple LaLiga seasons.
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
# Defensive candidate models:
#   modelo_multinom_defensivo_d1
#   modelo_multinom_defensivo_d1_1
#   modelo_multinom_defensivo_d1_2
#
# Final defensive model:
#   modelo_multinom_defensivo_d1_final
#
# Offensive candidate models:
#   modelo_multinom_ofensivo_d1
#   modelo_multinom_ofensivo_d1_1
#
# Final offensive model:
#   modelo_multinom_ofensivo_d1_final
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, global likelihood-ratio tests,
#   AIC, log-likelihood, pseudo-R-squared, predicted probabilities, confusion
#   matrices, accuracy, balanced accuracy, class-specific sensitivity and
#   class-specific precision.
# ==============================================================================
# =========================================================
# 6. MODELO DEFENSIVO MULTINOMIAL COMPLETO
# =========================================================

modelo_multinom_defensivo_d1 <- multinom(
  resultado_partido_local ~
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    intercepciones_local +
    despejes_local +
    rojas_local +
    rojas_forzadas_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE
)

# Resumen general
summary(modelo_multinom_defensivo_d1)



# =========================================================
# 7. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_defensivo_d1 <- extraer_resultados_multinom(
  modelo_multinom_defensivo_d1
)

# Mostrar tabla completa ordenada
tabla_defensivo_d1 %>%
  arrange(Resultado, P_valor)


# =========================================================
# 8. COEFICIENTES SIGNIFICATIVOS AL 5%
# =========================================================

tabla_defensivo_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 9. COEFICIENTES SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_defensivo_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 10. RESULTADOS SEPARADOS POR COMPARACIÓN
# =========================================================

# Derrota frente a Empate
tabla_defensivo_derrota_d1 <- tabla_defensivo_d1 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_defensivo_derrota_d1


# Victoria frente a Empate
tabla_defensivo_victoria_d1 <- tabla_defensivo_d1 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_defensivo_victoria_d1

#aqui da error test_lr y por tanto la he modificado
# =========================================================
# FUNCIÓN PARA TEST GLOBAL LR DE CADA VARIABLE
# =========================================================

test_lr_multinom <- function(modelo_completo, datos) {
  
  # Recuperar la fórmula del modelo completo
  formula_completa <- formula(modelo_completo)
  
  # Crear un model frame con todas las variables del modelo
  # y eliminar casos incompletos una sola vez
  datos_completos <- model.frame(
    formula = formula_completa,
    data = datos,
    na.action = na.omit
  )
  
  # Eliminar niveles no utilizados de las variables categóricas
  datos_completos <- droplevels(datos_completos)
  
  # Volver a estimar el modelo completo sobre las mismas filas
  # que se utilizarán en todos los modelos reducidos
  modelo_completo_fijo <- multinom(
    formula = formula_completa,
    data = datos_completos,
    trace = FALSE
  )
  
  # Obtener los nombres de las variables explicativas
  variables <- attr(
    terms(modelo_completo_fijo),
    "term.labels"
  )
  
  # Log-verosimilitud del modelo completo
  logLik_completo <- logLik(modelo_completo_fijo)
  
  ll_completo <- as.numeric(logLik_completo)
  df_completo <- attr(logLik_completo, "df")
  
  # Crear tabla donde se almacenan los resultados
  resultados <- data.frame(
    Variable = character(),
    LR_stat = numeric(),
    df = numeric(),
    p_value_original = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Estimar un modelo reducido por cada variable
  for (variable in variables) {
    
    # Crear fórmula eliminando una variable cada vez
    formula_reducida <- update(
      formula_completa,
      paste(". ~ . -", variable)
    )
    
    # Estimar el modelo reducido con las mismas observaciones
    modelo_reducido <- multinom(
      formula = formula_reducida,
      data = datos_completos,
      trace = FALSE
    )
    
    # Log-verosimilitud del modelo reducido
    logLik_reducido <- logLik(modelo_reducido)
    
    ll_reducido <- as.numeric(logLik_reducido)
    df_reducido <- attr(logLik_reducido, "df")
    
    # Test de razón de verosimilitud
    LR <- 2 * (ll_completo - ll_reducido)
    
    # Diferencia de grados de libertad
    df_diff <- df_completo - df_reducido
    
    # P-valor del test
    p_val <- pchisq(
      LR,
      df = df_diff,
      lower.tail = FALSE
    )
    
    # Guardar el resultado
    resultados <- rbind(
      resultados,
      data.frame(
        Variable = variable,
        LR_stat = LR,
        df = df_diff,
        p_value_original = p_val,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Ordenar y presentar la tabla
  resultados <- resultados %>%
    arrange(p_value_original) %>%
    mutate(
      Significativa_5 = case_when(
        is.na(p_value_original) ~ "No estimable",
        p_value_original < 0.05 ~ "Sí",
        TRUE ~ "No"
      ),
      Significativa_10 = case_when(
        is.na(p_value_original) ~ "No estimable",
        p_value_original < 0.10 ~ "Sí",
        TRUE ~ "No"
      ),
      LR_stat = round(LR_stat, 4),
      p_value = round(p_value_original, 4)
    ) %>%
    select(
      Variable,
      LR_stat,
      df,
      p_value,
      Significativa_5,
      Significativa_10
    )
  
  return(resultados)
}
# =========================================================
# 11. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_defensivo_d1 <- test_lr_multinom(
  modelo_multinom_defensivo_d1,
  datos = d1
)

test_global_defensivo_d1


# =========================================================
# 12. BONDAD DE AJUSTE
# =========================================================

AIC_defensivo_d1 <- AIC(
  modelo_multinom_defensivo_d1
)

logLik_defensivo_d1 <- logLik(
  modelo_multinom_defensivo_d1
)

pseudo_R2_defensivo_d1 <- pR2(
  modelo_multinom_defensivo_d1
)

AIC_defensivo_d1
logLik_defensivo_d1
pseudo_R2_defensivo_d1


# =========================================================
# 13. PROBABILIDADES ESTIMADAS
# =========================================================

prob_defensivo_d1 <- predict(
  modelo_multinom_defensivo_d1,
  newdata = d1,
  type = "probs"
)

# Primeros partidos en escala 0-1
head(prob_defensivo_d1)

# Primeros partidos en porcentaje
round(
  head(prob_defensivo_d1 * 100),
  2
)

# Número de partidos con probabilidades estimadas
nrow(prob_defensivo_d1)

# Comprobar que las probabilidades suman 1
rowSums(prob_defensivo_d1)[1:10]


# =========================================================
# 14. PREDICCIÓN DE RESULTADO
# =========================================================

pred_defensivo_d1 <- predict(
  modelo_multinom_defensivo_d1,
  newdata = d1,
  type = "class"
)

head(pred_defensivo_d1)


# =========================================================
# 15. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado_defensivo <- levels(
  d1$resultado_partido_local
)

real_defensivo_d1 <- factor(
  d1$resultado_partido_local,
  levels = niveles_resultado_defensivo
)

pred_defensivo_d1 <- factor(
  pred_defensivo_d1,
  levels = niveles_resultado_defensivo
)

mc_defensivo_d1 <- table(
  Real = real_defensivo_d1,
  Predicho = pred_defensivo_d1
)

mc_defensivo_d1
addmargins(mc_defensivo_d1)


# =========================================================
# 16. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

# Accuracy total
accuracy_defensivo_d1 <- sum(
  diag(mc_defensivo_d1)
) / sum(mc_defensivo_d1)

# Sensibilidad por categoría
sensibilidad_defensivo_d1 <- diag(
  mc_defensivo_d1
) / rowSums(mc_defensivo_d1)

# Precisión por categoría
precision_defensivo_d1 <- diag(
  mc_defensivo_d1
) / colSums(mc_defensivo_d1)

# Evitar NaN e Inf
sensibilidad_defensivo_d1[
  is.nan(sensibilidad_defensivo_d1) |
    is.infinite(sensibilidad_defensivo_d1)
] <- NA

precision_defensivo_d1[
  is.nan(precision_defensivo_d1) |
    is.infinite(precision_defensivo_d1)
] <- NA

# Sensibilidad media de las tres categorías
balanced_accuracy_defensivo_d1 <- mean(
  sensibilidad_defensivo_d1,
  na.rm = TRUE
)

# Tabla de métricas
metricas_defensivo_d1 <- data.frame(
  Categoria = niveles_resultado_defensivo,
  Sensibilidad = round(
    as.numeric(sensibilidad_defensivo_d1),
    4
  ),
  Precision = round(
    as.numeric(precision_defensivo_d1),
    4
  )
)

accuracy_defensivo_d1
balanced_accuracy_defensivo_d1
metricas_defensivo_d1

#depuramos la variable menos significativa usando la funcion test_lr, en este caso es intercepciones_local

modelo_multinom_defensivo_d1_1 <- multinom(
  resultado_partido_local ~
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    rojas_local +
    rojas_forzadas_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE
)

# Resumen general
summary(modelo_multinom_defensivo_d1_1)

test_global_defensivo_d1_1 <- test_lr_multinom(
  modelo_multinom_defensivo_d1_1,
  datos = d1
)

test_global_defensivo_d1_1

#ahora rojas forzadas_local
modelo_multinom_defensivo_d1_2 <- multinom(
  resultado_partido_local ~
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    rojas_local
    ,
  data = d1,
  trace = FALSE,
  Hess = TRUE
)

# Resumen general
summary(modelo_multinom_defensivo_d1_2)

test_global_defensivo_d1_2 <- test_lr_multinom(
  modelo_multinom_defensivo_d1_2,
  datos = d1
)

test_global_defensivo_d1_2

#este es el modelo final


# =========================================================
# MODELO DEFENSIVO MULTINOMIAL FINAL
# Base de datos: d1
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. ASIGNAR EL MODELO DEFINITIVO
# =========================================================

# El modelo d1_2 pasa a ser el modelo defensivo final
modelo_multinom_defensivo_d1_final <-
  modelo_multinom_defensivo_d1_2

# Resumen general
summary(modelo_multinom_defensivo_d1_final)

# Comprobar la fórmula del modelo final
formula(modelo_multinom_defensivo_d1_final)



# =========================================================
# 2. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_defensivo_d1_final <- extraer_resultados_multinom(
  modelo_multinom_defensivo_d1_final
)

# Mostrar tabla completa ordenada
tabla_defensivo_d1_final %>%
  arrange(Resultado, P_valor)


# =========================================================
# 3. COEFICIENTES SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_defensivo_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 4. COEFICIENTES SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_defensivo_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 5. RESULTADOS SEPARADOS POR COMPARACIÓN
# =========================================================

# Derrota frente a Empate
tabla_defensivo_derrota_d1_final <-
  tabla_defensivo_d1_final %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_defensivo_derrota_d1_final


# Victoria frente a Empate
tabla_defensivo_victoria_d1_final <-
  tabla_defensivo_d1_final %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_defensivo_victoria_d1_final



# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_defensivo_d1_final <- test_lr_multinom(
  modelo_completo = modelo_multinom_defensivo_d1_final,
  datos = d1
)

test_global_defensivo_d1_final


# Variables globalmente significativas al 5 %
test_global_defensivo_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# Variables globalmente significativas o marginales al 10 %
test_global_defensivo_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 8. BONDAD DE AJUSTE
# =========================================================

AIC_defensivo_d1_final <- AIC(
  modelo_multinom_defensivo_d1_final
)

logLik_defensivo_d1_final <- logLik(
  modelo_multinom_defensivo_d1_final
)

pseudo_R2_defensivo_d1_final <- pR2(
  modelo_multinom_defensivo_d1_final
)

AIC_defensivo_d1_final
logLik_defensivo_d1_final
pseudo_R2_defensivo_d1_final


# =========================================================
# 9. PREPARAR LOS DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Construimos una base que contiene exactamente las variables
# y las observaciones válidas del modelo defensivo final.
#
# Esto evita problemas de longitud en las probabilidades,
# predicciones y matriz de confusión cuando existen valores NA.

datos_defensivo_d1_final <- model.frame(
  formula = formula(modelo_multinom_defensivo_d1_final),
  data = d1,
  na.action = na.omit
)

# Eliminar posibles niveles sin observaciones
datos_defensivo_d1_final <- droplevels(
  datos_defensivo_d1_final
)

# Número de observaciones utilizadas en las predicciones
nrow(datos_defensivo_d1_final)



# =========================================================
# 10. PROBABILIDADES ESTIMADAS
# =========================================================

prob_defensivo_d1_final <- predict(
  modelo_multinom_defensivo_d1_final,
  newdata = datos_defensivo_d1_final,
  type = "probs"
)

# Primeros partidos en escala 0-1
head(prob_defensivo_d1_final)

# Primeros partidos en porcentaje
round(
  head(prob_defensivo_d1_final * 100),
  2
)

# Número de filas de probabilidades
nrow(prob_defensivo_d1_final)

# Comprobar que las probabilidades suman 1
rowSums(
  prob_defensivo_d1_final
)[1:10]

# Comprobar si todas las probabilidades suman aproximadamente 1
all(
  abs(
    rowSums(prob_defensivo_d1_final) - 1
  ) < 1e-8
)


# =========================================================
# 11. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_defensivo_d1_final <- predict(
  modelo_multinom_defensivo_d1_final,
  newdata = datos_defensivo_d1_final,
  type = "class"
)

head(pred_defensivo_d1_final)

# Distribución de los resultados predichos
table(
  pred_defensivo_d1_final,
  useNA = "always"
)


# =========================================================
# 12. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_defensivo_d1_final <- levels(
  d1$resultado_partido_local
)

niveles_resultado_defensivo_d1_final


# Resultados reales de las observaciones utilizadas
real_defensivo_d1_final <- factor(
  datos_defensivo_d1_final$resultado_partido_local,
  levels = niveles_resultado_defensivo_d1_final
)


# Resultados predichos
pred_defensivo_d1_final <- factor(
  pred_defensivo_d1_final,
  levels = niveles_resultado_defensivo_d1_final
)


# Comprobar que ambas variables tienen la misma longitud
length(real_defensivo_d1_final)
length(pred_defensivo_d1_final)

length(real_defensivo_d1_final) ==
  length(pred_defensivo_d1_final)


# Construir la matriz de confusión
mc_defensivo_d1_final <- table(
  Real = real_defensivo_d1_final,
  Predicho = pred_defensivo_d1_final
)

mc_defensivo_d1_final

# Matriz con los totales por filas y columnas
addmargins(mc_defensivo_d1_final)


# =========================================================
# 13. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

# Accuracy total
accuracy_defensivo_d1_final <- sum(
  diag(mc_defensivo_d1_final)
) / sum(mc_defensivo_d1_final)


# Sensibilidad por categoría
sensibilidad_defensivo_d1_final <- diag(
  mc_defensivo_d1_final
) / rowSums(mc_defensivo_d1_final)


# Precisión por categoría
precision_defensivo_d1_final <- diag(
  mc_defensivo_d1_final
) / colSums(mc_defensivo_d1_final)


# Evitar NaN e Inf en la sensibilidad
sensibilidad_defensivo_d1_final[
  is.nan(sensibilidad_defensivo_d1_final) |
    is.infinite(sensibilidad_defensivo_d1_final)
] <- NA


# Evitar NaN e Inf en la precisión
precision_defensivo_d1_final[
  is.nan(precision_defensivo_d1_final) |
    is.infinite(precision_defensivo_d1_final)
] <- NA


# Balanced accuracy:
# media de la sensibilidad de las tres categorías
balanced_accuracy_defensivo_d1_final <- mean(
  sensibilidad_defensivo_d1_final,
  na.rm = TRUE
)


# Tabla de métricas por categoría
metricas_defensivo_d1_final <- data.frame(
  Categoria = niveles_resultado_defensivo_d1_final,
  Sensibilidad = round(
    as.numeric(sensibilidad_defensivo_d1_final),
    4
  ),
  Precision = round(
    as.numeric(precision_defensivo_d1_final),
    4
  )
)


# Mostrar las métricas
accuracy_defensivo_d1_final
balanced_accuracy_defensivo_d1_final
metricas_defensivo_d1_final


# =========================================================
# 14. RESULTADOS GENERALES DE CLASIFICACIÓN
# =========================================================

resultados_clasificacion_defensivo_d1_final <- data.frame(
  Metrica = c(
    "Accuracy",
    "Balanced Accuracy"
  ),
  Valor = c(
    accuracy_defensivo_d1_final,
    balanced_accuracy_defensivo_d1_final
  )
)

resultados_clasificacion_defensivo_d1_final <-
  resultados_clasificacion_defensivo_d1_final %>%
  mutate(
    Valor = round(Valor, 4)
  )

resultados_clasificacion_defensivo_d1_final

# =========================================================
# BLOQUE OFENSIVO MULTINOMIAL COMPLETO

# =========================================================
# 5. MODELO OFENSIVO MULTINOMIAL COMPLETO
# =========================================================

modelo_multinom_ofensivo_d1 <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    big_chances_local +
    pases_U3_local +
    centros_local +
    rojas_local +
    rojas_forzadas_local +
    despejes_concedidos_local +
    despejes_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)
#no aparece recuperaciones porque vale 0 para todas las recopilaciones

# Resumen general del modelo
summary(modelo_multinom_ofensivo_d1)


# =========================================================
# 6. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_ofensivo_d1 <- extraer_resultados_multinom(
  modelo_multinom_ofensivo_d1
)


# Mostrar tabla completa
tabla_ofensivo_d1


# Mostrar tabla completa ordenada
tabla_ofensivo_d1 %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 7. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_ofensivo_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 8. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_ofensivo_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 9. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_derrota_d1 <- tabla_ofensivo_d1 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_ofensivo_derrota_d1


# =========================================================
# 10. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_victoria_d1 <- tabla_ofensivo_d1 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_ofensivo_victoria_d1




# =========================================================
# 12. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_ofensivo_d1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_ofensivo_d1,
  datos = d1
)

test_global_ofensivo_d1


# Variables globalmente significativas al 5 %
test_global_ofensivo_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# Variables globalmente significativas o marginales al 10 %
test_global_ofensivo_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 13. BONDAD DE AJUSTE
# =========================================================

AIC_ofensivo_d1 <- AIC(
  modelo_multinom_ofensivo_d1
)

logLik_ofensivo_d1 <- logLik(
  modelo_multinom_ofensivo_d1
)

pseudo_R2_ofensivo_d1 <- pR2(
  modelo_multinom_ofensivo_d1
)


# Mostrar las medidas de ajuste
AIC_ofensivo_d1
logLik_ofensivo_d1
pseudo_R2_ofensivo_d1


# =========================================================
# 14. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Crear una base con exactamente las observaciones completas
# utilizadas por el modelo ofensivo
datos_ofensivo_d1 <- model.frame(
  formula = formula(modelo_multinom_ofensivo_d1),
  data = d1,
  na.action = na.omit
)

# Eliminar posibles niveles sin observaciones
datos_ofensivo_d1 <- droplevels(
  datos_ofensivo_d1
)


# Número de observaciones de la base utilizada
nrow(datos_ofensivo_d1)




# =========================================================
# 15. PROBABILIDADES ESTIMADAS
# =========================================================

prob_ofensivo_d1 <- predict(
  modelo_multinom_ofensivo_d1,
  newdata = datos_ofensivo_d1,
  type = "probs"
)


# Primeros seis partidos en escala 0-1
head(prob_ofensivo_d1)


# Primeros seis partidos en porcentaje
round(
  head(prob_ofensivo_d1 * 100),
  2
)


# Número total de partidos con probabilidades estimadas
nrow(prob_ofensivo_d1)


# Comprobar que las primeras filas suman 1
rowSums(
  prob_ofensivo_d1
)[1:10]


# Comprobar que todas las probabilidades suman aproximadamente 1
all(
  abs(
    rowSums(prob_ofensivo_d1) - 1
  ) < 1e-8
)


# =========================================================
# 16. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_ofensivo_d1 <- predict(
  modelo_multinom_ofensivo_d1,
  newdata = datos_ofensivo_d1,
  type = "class"
)


# Primeros resultados predichos
head(pred_ofensivo_d1)


# Distribución de los resultados predichos
table(
  pred_ofensivo_d1,
  useNA = "always"
)


# =========================================================
# 17. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_ofensivo_d1 <- levels(
  d1$resultado_partido_local
)

niveles_resultado_ofensivo_d1


# Resultados reales de las observaciones utilizadas
real_ofensivo_d1 <- factor(
  datos_ofensivo_d1$resultado_partido_local,
  levels = niveles_resultado_ofensivo_d1
)


# Predicciones con los mismos niveles
pred_ofensivo_d1 <- factor(
  pred_ofensivo_d1,
  levels = niveles_resultado_ofensivo_d1
)


# Comprobar las longitudes
length(real_ofensivo_d1)
length(pred_ofensivo_d1)


# Las longitudes deben coincidir
length(real_ofensivo_d1) ==
  length(pred_ofensivo_d1)


# Construir la matriz de confusión
mc_ofensivo_d1 <- table(
  Real = real_ofensivo_d1,
  Predicho = pred_ofensivo_d1
)

mc_ofensivo_d1


# Añadir totales por filas y columnas
addmargins(
  mc_ofensivo_d1
)


# =========================================================
# 18. ACCURACY TOTAL
# =========================================================

accuracy_ofensivo_d1 <- sum(
  diag(mc_ofensivo_d1)
) / sum(mc_ofensivo_d1)

accuracy_ofensivo_d1


# =========================================================
# 19. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_ofensivo_d1 <- diag(
  mc_ofensivo_d1
) / rowSums(mc_ofensivo_d1)


# Evitar NaN o Inf
sensibilidad_ofensivo_d1[
  is.nan(sensibilidad_ofensivo_d1) |
    is.infinite(sensibilidad_ofensivo_d1)
] <- NA

sensibilidad_ofensivo_d1


# =========================================================
# 20. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_ofensivo_d1 <- diag(
  mc_ofensivo_d1
) / colSums(mc_ofensivo_d1)


# Evitar NaN o Inf
precision_ofensivo_d1[
  is.nan(precision_ofensivo_d1) |
    is.infinite(precision_ofensivo_d1)
] <- NA

precision_ofensivo_d1


# =========================================================
# 21. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_ofensivo_d1 <- mean(
  sensibilidad_ofensivo_d1,
  na.rm = TRUE
)

balanced_accuracy_ofensivo_d1


# =========================================================
# 22. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_ofensivo_d1 <- data.frame(
  Categoria = niveles_resultado_ofensivo_d1,
  Sensibilidad = round(
    as.numeric(sensibilidad_ofensivo_d1),
    4
  ),
  Precision = round(
    as.numeric(precision_ofensivo_d1),
    4
  )
)

metricas_ofensivo_d1


# =========================================================
# 23. TABLA DE MÉTRICAS GENERALES
# =========================================================

resultados_clasificacion_ofensivo_d1 <- data.frame(
  Metrica = c(
    "Accuracy",
    "Balanced Accuracy"
  ),
  Valor = c(
    accuracy_ofensivo_d1,
    balanced_accuracy_ofensivo_d1
  )
)

resultados_clasificacion_ofensivo_d1 <-
  resultados_clasificacion_ofensivo_d1 %>%
  mutate(
    Valor = round(
      Valor,
      4
    )
  )

resultados_clasificacion_ofensivo_d1

modelo_multinom_ofensivo_d1_1 <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local +
    rojas_forzadas_local +
    despejes_concedidos_local +
    despejes_local,
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)
#no aparece recuperaciones porque vale 0 para todas las recopilaciones

# Resumen general del modelo
summary(modelo_multinom_ofensivo_d1_1)

test_global_ofensivo_d1_1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_ofensivo_d1_1,
  datos = d1
)

test_global_ofensivo_d1_1

# =========================================================
# MODELO OFENSIVO MULTINOMIAL FINAL
# Base de datos: d1
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. ASIGNAR EL MODELO OFENSIVO FINAL
# =========================================================

# El modelo ofensivo d1_1 pasa a ser el modelo final
modelo_multinom_ofensivo_d1_final <-
  modelo_multinom_ofensivo_d1_1


# Resumen general del modelo final
summary(modelo_multinom_ofensivo_d1_final)

# =========================================================
# 2. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_ofensivo_d1_final <- extraer_resultados_multinom(
  modelo_multinom_ofensivo_d1_final
)


# Mostrar tabla completa
tabla_ofensivo_d1_final


# Mostrar tabla completa ordenada
tabla_ofensivo_d1_final %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 3. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_ofensivo_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 4. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_ofensivo_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 5. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_derrota_d1_final <-
  tabla_ofensivo_d1_final %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_ofensivo_derrota_d1_final


# =========================================================
# 6. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_victoria_d1_final <-
  tabla_ofensivo_d1_final %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_ofensivo_victoria_d1_final


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

# La función test_lr_multinom() debe estar definida previamente

test_global_ofensivo_d1_final <- test_lr_multinom(
  modelo_completo = modelo_multinom_ofensivo_d1_final,
  datos = d1
)

test_global_ofensivo_d1_final


# =========================================================
# 8. VARIABLES GLOBALMENTE SIGNIFICATIVAS AL 5 %
# =========================================================

test_global_ofensivo_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# =========================================================
# 9. VARIABLES GLOBALES SIGNIFICATIVAS O MARGINALES AL 10 %
# =========================================================

test_global_ofensivo_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 10. BONDAD DE AJUSTE
# =========================================================

AIC_ofensivo_d1_final <- AIC(
  modelo_multinom_ofensivo_d1_final
)

logLik_ofensivo_d1_final <- logLik(
  modelo_multinom_ofensivo_d1_final
)

pseudo_R2_ofensivo_d1_final <- pR2(
  modelo_multinom_ofensivo_d1_final
)


# Mostrar las medidas de ajuste
AIC_ofensivo_d1_final
logLik_ofensivo_d1_final
pseudo_R2_ofensivo_d1_final


# =========================================================
# 11. DATOS UTILIZADOS POR EL MODELO FINAL
# =========================================================

# Crear una base que contenga exactamente las observaciones
# completas utilizadas por el modelo ofensivo final

datos_ofensivo_d1_final <- model.frame(
  formula = formula(
    modelo_multinom_ofensivo_d1_final
  ),
  data = d1,
  na.action = na.omit
)


# Eliminar niveles sin observaciones
datos_ofensivo_d1_final <- droplevels(
  datos_ofensivo_d1_final
)


# Número de observaciones completas
nrow(datos_ofensivo_d1_final)




# =========================================================
# 12. PROBABILIDADES ESTIMADAS
# =========================================================

prob_ofensivo_d1_final <- predict(
  modelo_multinom_ofensivo_d1_final,
  newdata = datos_ofensivo_d1_final,
  type = "probs"
)


# Primeros partidos en escala 0-1
head(prob_ofensivo_d1_final)


# Primeros partidos en porcentaje
round(
  head(prob_ofensivo_d1_final * 100),
  2
)


# Número total de partidos con probabilidades estimadas
nrow(prob_ofensivo_d1_final)


# Comprobar que las primeras probabilidades suman 1
rowSums(
  prob_ofensivo_d1_final
)[1:10]


# Comprobar que todas las probabilidades suman aproximadamente 1
all(
  abs(
    rowSums(prob_ofensivo_d1_final) - 1
  ) < 1e-8
)


# =========================================================
# 13. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_ofensivo_d1_final <- predict(
  modelo_multinom_ofensivo_d1_final,
  newdata = datos_ofensivo_d1_final,
  type = "class"
)


# Primeros resultados predichos
head(pred_ofensivo_d1_final)


# Distribución de los resultados predichos
table(
  pred_ofensivo_d1_final,
  useNA = "always"
)


# =========================================================
# 14. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_ofensivo_d1_final <- levels(
  d1$resultado_partido_local
)

niveles_resultado_ofensivo_d1_final


# Resultados reales
real_ofensivo_d1_final <- factor(
  datos_ofensivo_d1_final$resultado_partido_local,
  levels = niveles_resultado_ofensivo_d1_final
)


# Predicciones con los mismos niveles
pred_ofensivo_d1_final <- factor(
  pred_ofensivo_d1_final,
  levels = niveles_resultado_ofensivo_d1_final
)


# Comprobar las longitudes
length(real_ofensivo_d1_final)
length(pred_ofensivo_d1_final)


# Las longitudes deben coincidir
length(real_ofensivo_d1_final) ==
  length(pred_ofensivo_d1_final)


# Construir la matriz de confusión
mc_ofensivo_d1_final <- table(
  Real = real_ofensivo_d1_final,
  Predicho = pred_ofensivo_d1_final
)

mc_ofensivo_d1_final


# Añadir totales por filas y columnas
addmargins(
  mc_ofensivo_d1_final
)


# =========================================================
# 15. ACCURACY TOTAL
# =========================================================

accuracy_ofensivo_d1_final <- sum(
  diag(mc_ofensivo_d1_final)
) / sum(mc_ofensivo_d1_final)

accuracy_ofensivo_d1_final


# =========================================================
# 16. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_ofensivo_d1_final <- diag(
  mc_ofensivo_d1_final
) / rowSums(mc_ofensivo_d1_final)


# Evitar NaN o Inf
sensibilidad_ofensivo_d1_final[
  is.nan(sensibilidad_ofensivo_d1_final) |
    is.infinite(sensibilidad_ofensivo_d1_final)
] <- NA

sensibilidad_ofensivo_d1_final


# =========================================================
# 17. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_ofensivo_d1_final <- diag(
  mc_ofensivo_d1_final
) / colSums(mc_ofensivo_d1_final)


# Evitar NaN o Inf
precision_ofensivo_d1_final[
  is.nan(precision_ofensivo_d1_final) |
    is.infinite(precision_ofensivo_d1_final)
] <- NA

precision_ofensivo_d1_final


# =========================================================
# 18. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_ofensivo_d1_final <- mean(
  sensibilidad_ofensivo_d1_final,
  na.rm = TRUE
)

balanced_accuracy_ofensivo_d1_final


# =========================================================
# 19. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_ofensivo_d1_final <- data.frame(
  Categoria = niveles_resultado_ofensivo_d1_final,
  Sensibilidad = round(
    as.numeric(
      sensibilidad_ofensivo_d1_final
    ),
    4
  ),
  Precision = round(
    as.numeric(
      precision_ofensivo_d1_final
    ),
    4
  )
)

metricas_ofensivo_d1_final


