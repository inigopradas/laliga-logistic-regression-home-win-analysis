# ==============================================================================
# Title: Multi-season general multinomial model selection and visualisation
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops, simplifies and evaluates a general multinomial logistic
# regression model using match-level data from multiple LaLiga seasons. The
# dependent variable distinguishes between a home-team victory, a draw and a
# home-team defeat, with the draw used as the reference category.
#
# The initial general model combines explanatory variables from four analytical
# dimensions: competitive context, physical performance, attacking performance
# and defensive performance. These variables describe rest conditions,
# previous league positions, recent form, fouls, tackles, duels, disciplinary
# events, attacking production and defensive exposure.
#
# The script checks the estimated coefficients and standard errors and extracts
# coefficients, odds ratios and bilateral Wald p-values. Results are presented
# separately for home defeat versus draw and home victory versus draw, with
# statistical evidence identified at the 5% and 10% levels.
#
# Global likelihood-ratio tests are used to assess the overall contribution of
# each explanatory variable. Model fit is evaluated using the Akaike information
# criterion, log-likelihood and pseudo-R-squared statistics. Correlation
# matrices and manually calculated variance inflation factors are used to
# examine potential multicollinearity among the predictors.
#
# The initial specification is progressively simplified by removing variables
# with limited global contribution. The successive candidate models document
# the manual selection process until the final reduced general model is
# obtained.
#
# The final specification combines rest difference, previous league positions,
# visiting-team recent form, tackles conceded, duels won, home red cards,
# numerical-advantage duration, home shots on target, home big chances, home
# crosses, opposition clearances, expected goals against, big chances conceded,
# shots on target conceded, corners conceded, long passes conceded, crosses
# conceded and home clearances.
#
# Predicted probabilities and outcome classes are calculated using the complete
# observations employed by the final model. Classification performance is
# evaluated through a confusion matrix, overall accuracy, balanced accuracy,
# class-specific sensitivity and class-specific precision.
#
# The script also produces graphical summaries of the final model. These
# include a logarithmic odds-ratio plot with 95% confidence intervals, a
# confusion-matrix heatmap and a grouped bar chart comparing sensitivity and
# precision across the three outcome categories.
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
#   Manual backward reduction supported by global likelihood-ratio tests,
#   coefficient significance, AIC, model fit, multicollinearity assessment and
#   substantive interpretation.
#
# Candidate models:
#   modelo_multinom_general_d1
#   modelo_multinom_general_d1_1
#   modelo_multinom_general_d1_2
#   modelo_multinom_general_d1_3
#   modelo_multinom_general_d1_4
#   modelo_multinom_general_d1_5
#
# Final general model:
#   modelo_multinom_general_d1_final
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, global likelihood-ratio tests,
#   AIC, log-likelihood, pseudo-R-squared, correlation matrices, variance
#   inflation factors, predicted probabilities, confusion matrices, accuracy,
#   balanced accuracy, class-specific sensitivity, class-specific precision
#   and graphical summaries of the final general model.
# ==============================================================================

# =========================================================
# 1. MODELO MULTINOMIAL GENERAL
# =========================================================

modelo_multinom_general_d1 <- multinom(
  resultado_partido_local ~
    
    # Contexto
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    
    # Físico
    faltas_local +
    faltas_recibidas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local +
    
    # Ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_forzadas_local +
    despejes_concedidos_local +
    
    # Defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local,
  
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_general_d1)


# Fórmula utilizada
formula(modelo_multinom_general_d1)


# Número de observaciones utilizadas
nobs(modelo_multinom_general_d1)


# =========================================================
# 2. COMPROBAR ERRORES ESTÁNDAR
# =========================================================

resumen_general_d1 <- summary(
  modelo_multinom_general_d1
)


# Coeficientes
resumen_general_d1$coefficients


# Errores estándar
resumen_general_d1$standard.errors


# Comprobar si aparece algún NaN
any(
  is.nan(
    resumen_general_d1$standard.errors
  )
)


# Comprobar si aparece algún valor no finito
any(
  !is.finite(
    resumen_general_d1$standard.errors
  )
)

# Ambos resultados deberían ser FALSE.


# =========================================================
# 3. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_general_d1 <- extraer_resultados_multinom(
  modelo_multinom_general_d1
)


# Mostrar tabla completa
tabla_general_d1


# Mostrar tabla ordenada por resultado y p-valor
tabla_general_d1 %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 4. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_general_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 5. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_general_d1 %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 6. DERROTA FRENTE A EMPATE
# =========================================================

tabla_general_derrota_d1 <- tabla_general_d1 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_general_derrota_d1


# =========================================================
# 7. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_general_victoria_d1 <- tabla_general_d1 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_general_victoria_d1


# =========================================================
# 8. TEST GLOBAL DE CADA VARIABLE
# =========================================================

# La función test_lr_multinom() debe estar definida previamente.

test_global_general_d1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_general_d1,
  datos = d1
)

test_global_general_d1


# =========================================================
# 9. VARIABLES GLOBALMENTE SIGNIFICATIVAS AL 5 %
# =========================================================

test_global_general_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# =========================================================
# 10. VARIABLES GLOBALES SIGNIFICATIVAS O MARGINALES AL 10 %
# =========================================================

test_global_general_d1 %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 11. BONDAD DE AJUSTE
# =========================================================

AIC_general_d1 <- AIC(
  modelo_multinom_general_d1
)

logLik_general_d1 <- logLik(
  modelo_multinom_general_d1
)

pseudo_R2_general_d1 <- pR2(
  modelo_multinom_general_d1
)


# Mostrar resultados
AIC_general_d1
logLik_general_d1
pseudo_R2_general_d1


# =========================================================
# 12. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Crear una base con exactamente las observaciones completas
# utilizadas por el modelo general.

datos_general_d1 <- model.frame(
  formula = formula(
    modelo_multinom_general_d1
  ),
  data = d1,
  na.action = na.omit
)


# Eliminar posibles niveles no utilizados
datos_general_d1 <- droplevels(
  datos_general_d1
)


# Número de observaciones de la base empleada
nrow(datos_general_d1)


# Número de observaciones utilizadas por el modelo
nobs(modelo_multinom_general_d1)


# Comprobar que coinciden
nrow(datos_general_d1) ==
  nobs(modelo_multinom_general_d1)


# =========================================================
# 13. PROBABILIDADES ESTIMADAS
# =========================================================

prob_general_d1 <- predict(
  modelo_multinom_general_d1,
  newdata = datos_general_d1,
  type = "probs"
)


# Primeras probabilidades en escala 0-1
head(prob_general_d1)


# Primeras probabilidades en porcentaje
round(
  head(prob_general_d1 * 100),
  2
)


# Número de partidos con probabilidades estimadas
nrow(prob_general_d1)


# Comprobar las primeras sumas
rowSums(
  prob_general_d1
)[1:10]


# Comprobar que todas las probabilidades suman aproximadamente 1
all(
  abs(
    rowSums(prob_general_d1) - 1
  ) < 1e-8
)


# =========================================================
# 14. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_general_d1 <- predict(
  modelo_multinom_general_d1,
  newdata = datos_general_d1,
  type = "class"
)


# Primeros resultados predichos
head(pred_general_d1)


# Distribución de las predicciones
table(
  pred_general_d1,
  useNA = "always"
)


# =========================================================
# 15. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_general_d1 <- levels(
  d1$resultado_partido_local
)

niveles_resultado_general_d1


# Resultados reales de las observaciones utilizadas
real_general_d1 <- factor(
  datos_general_d1$resultado_partido_local,
  levels = niveles_resultado_general_d1
)


# Predicciones con los mismos niveles
pred_general_d1 <- factor(
  pred_general_d1,
  levels = niveles_resultado_general_d1
)


# Comprobar longitudes
length(real_general_d1)
length(pred_general_d1)


# Deben coincidir
length(real_general_d1) ==
  length(pred_general_d1)


# Construir la matriz de confusión
mc_general_d1 <- table(
  Real = real_general_d1,
  Predicho = pred_general_d1
)

mc_general_d1


# Matriz con totales por filas y columnas
addmargins(
  mc_general_d1
)


# =========================================================
# 16. ACCURACY TOTAL
# =========================================================

accuracy_general_d1 <- sum(
  diag(mc_general_d1)
) / sum(mc_general_d1)

accuracy_general_d1


# =========================================================
# 17. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_general_d1 <- diag(
  mc_general_d1
) / rowSums(mc_general_d1)


# Evitar NaN o Inf
sensibilidad_general_d1[
  is.nan(sensibilidad_general_d1) |
    is.infinite(sensibilidad_general_d1)
] <- NA

sensibilidad_general_d1


# =========================================================
# 18. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_general_d1 <- diag(
  mc_general_d1
) / colSums(mc_general_d1)


# Evitar NaN o Inf
precision_general_d1[
  is.nan(precision_general_d1) |
    is.infinite(precision_general_d1)
] <- NA

precision_general_d1


# =========================================================
# 19. BALANCED ACCURACY
# =========================================================

# Media de la sensibilidad de las tres categorías
balanced_accuracy_general_d1 <- mean(
  sensibilidad_general_d1,
  na.rm = TRUE
)

balanced_accuracy_general_d1


# =========================================================
# 20. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_general_d1 <- data.frame(
  Categoria = niveles_resultado_general_d1,
  
  Sensibilidad = round(
    as.numeric(
      sensibilidad_general_d1
    ),
    4
  ),
  
  Precision = round(
    as.numeric(
      precision_general_d1
    ),
    4
  )
)

metricas_general_d1


# =========================================================
# 21. TABLA DE MÉTRICAS GENERALES
# =========================================================

resultados_clasificacion_general_d1 <- data.frame(
  Metrica = c(
    "Accuracy",
    "Balanced Accuracy"
  ),
  
  Valor = c(
    accuracy_general_d1,
    balanced_accuracy_general_d1
  )
)


# Redondear los resultados
resultados_clasificacion_general_d1 <-
  resultados_clasificacion_general_d1 %>%
  mutate(
    Valor = round(
      Valor,
      4
    )
  )

resultados_clasificacion_general_d1


# =========================================================
# 22. COMPROBAR POSIBLE MULTICOLINEALIDAD
# =========================================================

variables_modelo_general_d1 <- c(
  "diff_descanso",
  "pos_previa_local",
  "pos_previa_visitante",
  "forma_local_5",
  "forma_visitante_5",
  "faltas_local",
  "faltas_recibidas_local",
  "entradas_concedidas_local",
  "duelos_ganados_local",
  "rojas_local",
  "min_ventaja_numerica_local",
  "tiros_puerta_local",
  "big_chances_local",
  "centros_local",
  "rojas_forzadas_local",
  "despejes_concedidos_local",
  "xGA_local",
  "big_chances_concedidas_local",
  "tiros_puerta_concedidos_local",
  "corners_concedidos_local",
  "pases_U3_concedidos_local",
  "pases_largos_concedidos_local",
  "centros_concedidos_local",
  "despejes_local"
)


# Matriz de correlaciones
correlaciones_general_d1 <- cor(
  datos_general_d1[
    variables_modelo_general_d1
  ],
  use = "complete.obs"
)


# Mostrar matriz redondeada
round(
  correlaciones_general_d1,
  3
)


# =========================================================
# 23. FUNCIÓN PARA CALCULAR VIF MANUALMENTE
# =========================================================

calcular_vif_manual <- function(datos, variables) {
  
  resultados <- data.frame(
    Variable = variables,
    VIF = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(variables)) {
    
    variable_respuesta <- variables[i]
    
    variables_explicativas <- variables[-i]
    
    formula_auxiliar <- as.formula(
      paste(
        variable_respuesta,
        "~",
        paste(
          variables_explicativas,
          collapse = " + "
        )
      )
    )
    
    modelo_auxiliar <- lm(
      formula = formula_auxiliar,
      data = datos,
      na.action = na.omit
    )
    
    r2 <- summary(
      modelo_auxiliar
    )$r.squared
    
    resultados$VIF[i] <- 1 / (
      1 - r2
    )
  }
  
  resultados <- resultados %>%
    mutate(
      VIF = round(
        VIF,
        3
      )
    ) %>%
    arrange(
      desc(VIF)
    )
  
  return(resultados)
}


# =========================================================
# 24. VIF DEL MODELO GENERAL
# =========================================================

vif_general_d1 <- calcular_vif_manual(
  datos = datos_general_d1,
  variables = variables_modelo_general_d1
)

vif_general_d1


# Variables con VIF superior a 5
vif_general_d1 %>%
  filter(VIF > 5)


# Variables con VIF superior a 10
vif_general_d1 %>%
  filter(VIF > 10)

#depuramos hasta sacar todas significativas, aqui quito faltas recibidas local
modelo_multinom_general_d1_1 <- multinom(
  resultado_partido_local ~
    
    # Contexto
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    
    # Físico
    faltas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local +
    
    # Ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_forzadas_local +
    despejes_concedidos_local +
    
    # Defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local,
  
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_general_d1_1)

test_global_general_d1_1 <- test_lr_multinom(
  modelo_completo = modelo_multinom_general_d1_1,
  datos = d1
)

test_global_general_d1_1

#ahora pases U3 concedidos local

modelo_multinom_general_d1_2 <- multinom(
  resultado_partido_local ~
    
    # Contexto
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    
    # Físico
    faltas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local +
    
    # Ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_forzadas_local +
    despejes_concedidos_local +
    
    # Defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local,
  
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_general_d1_2)

test_global_general_d1_2 <- test_lr_multinom(
  modelo_completo = modelo_multinom_general_d1_2,
  datos = d1
)

test_global_general_d1_2

#ahora rojas forzadas local

modelo_multinom_general_d1_3 <- multinom(
  resultado_partido_local ~
    
    # Contexto
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    
    # Físico
    faltas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local +
    
    # Ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    despejes_concedidos_local +
    
    # Defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local,
  
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_general_d1_3)

test_global_general_d1_3 <- test_lr_multinom(
  modelo_completo = modelo_multinom_general_d1_3,
  datos = d1
)

test_global_general_d1_3

#ahora quito faltas local

modelo_multinom_general_d1_4 <- multinom(
  resultado_partido_local ~
    
    # Contexto
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    
    # Físico
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local +
    
    # Ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    despejes_concedidos_local +
    
    # Defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local,
  
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_general_d1_4)

test_global_general_d1_4 <- test_lr_multinom(
  modelo_completo = modelo_multinom_general_d1_4,
  datos = d1
)

test_global_general_d1_4

#ahora quito forma local 5

modelo_multinom_general_d1_5 <- multinom(
  resultado_partido_local ~
    
    # Contexto
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_visitante_5 +
    
    # Físico
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local +
    
    # Ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    despejes_concedidos_local +
    
    # Defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local,
  
  data = d1,
  trace = FALSE,
  Hess = TRUE,
  na.action = na.omit
)


# Resumen general
summary(modelo_multinom_general_d1_5)

test_global_general_d1_5 <- test_lr_multinom(
  modelo_completo = modelo_multinom_general_d1_5,
  datos = d1
)

test_global_general_d1_5

#ahora todas son significativas, voy a ir quitando una a una a ver si baja el AIC
#he visto que baja el AIC, por lo tanto las dejo donde estan

# =========================================================
# MODELO MULTINOMIAL GENERAL FINAL DEPURADO
# Base de datos: d1
# Categoría de referencia: Empate
# =========================================================

modelo_multinom_general_d1_final <-
  modelo_multinom_general_d1_5

summary(
  modelo_multinom_general_d1_final
)

# Fórmula utilizada
formula(
  modelo_multinom_general_d1_final
)

# Número de observaciones utilizadas
nobs(
  modelo_multinom_general_d1_final
)


# =========================================================
# 11. COMPROBAR ERRORES ESTÁNDAR
# =========================================================

resumen_general_d1_final <- summary(
  modelo_multinom_general_d1_final
)

# Coeficientes
resumen_general_d1_final$coefficients

# Errores estándar
resumen_general_d1_final$standard.errors

# Comprobar si existe algún NaN
any(
  is.nan(
    resumen_general_d1_final$standard.errors
  )
)

# Comprobar si existe algún valor no finito
any(
  !is.finite(
    resumen_general_d1_final$standard.errors
  )
)

# Ambos resultados deberían ser FALSE.


# =========================================================
# 12. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_general_d1_final <- extraer_resultados_multinom(
  modelo_multinom_general_d1_final
)

# Mostrar tabla completa
tabla_general_d1_final

# Mostrar tabla ordenada
tabla_general_d1_final %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 13. RESULTADOS SIGNIFICATIVOS AL 5 %
# =========================================================

tabla_general_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.05) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 14. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10 %
# =========================================================

tabla_general_d1_final %>%
  filter(!is.na(P_valor)) %>%
  filter(P_valor < 0.10) %>%
  arrange(
    Resultado,
    P_valor
  )


# =========================================================
# 15. DERROTA FRENTE A EMPATE
# =========================================================

tabla_general_derrota_d1_final <-
  tabla_general_d1_final %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_general_derrota_d1_final


# =========================================================
# 16. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_general_victoria_d1_final <-
  tabla_general_d1_final %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_general_victoria_d1_final


# =========================================================
# 17. TEST GLOBAL DE CADA VARIABLE
# =========================================================

# La función test_lr_multinom() debe estar definida previamente.

test_global_general_d1_final <- test_lr_multinom(
  modelo_completo = modelo_multinom_general_d1_final,
  datos = d1
)

test_global_general_d1_final


# =========================================================
# 18. VARIABLES GLOBALMENTE SIGNIFICATIVAS AL 5 %
# =========================================================

test_global_general_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.05)


# =========================================================
# 19. VARIABLES GLOBALES SIGNIFICATIVAS O MARGINALES AL 10 %
# =========================================================

test_global_general_d1_final %>%
  filter(!is.na(p_value)) %>%
  filter(p_value < 0.10)


# =========================================================
# 20. BONDAD DE AJUSTE
# =========================================================

AIC_general_d1_final <- AIC(
  modelo_multinom_general_d1_final
)

logLik_general_d1_final <- logLik(
  modelo_multinom_general_d1_final
)

pseudo_R2_general_d1_final <- pR2(
  modelo_multinom_general_d1_final
)

# Mostrar resultados
AIC_general_d1_final
logLik_general_d1_final
pseudo_R2_general_d1_final


# =========================================================
# 21. DATOS UTILIZADOS POR EL MODELO
# =========================================================

# Crear una base con exactamente las observaciones completas
# utilizadas por el modelo general final.

datos_general_d1_final <- model.frame(
  formula = formula(
    modelo_multinom_general_d1_final
  ),
  data = d1,
  na.action = na.omit
)

# Eliminar posibles niveles no utilizados
datos_general_d1_final <- droplevels(
  datos_general_d1_final
)

# Número de observaciones completas
nrow(datos_general_d1_final)

# Número de observaciones utilizadas por el modelo
nobs(modelo_multinom_general_d1_final)

# Comprobar que coinciden
nrow(datos_general_d1_final) ==
  nobs(modelo_multinom_general_d1_final)


# =========================================================
# 22. PROBABILIDADES ESTIMADAS
# =========================================================

prob_general_d1_final <- predict(
  modelo_multinom_general_d1_final,
  newdata = datos_general_d1_final,
  type = "probs"
)

# Primeras probabilidades en escala 0-1
head(prob_general_d1_final)

# Primeras probabilidades en porcentaje
round(
  head(prob_general_d1_final * 100),
  2
)

# Número de partidos
nrow(prob_general_d1_final)

# Comprobar las primeras sumas
rowSums(
  prob_general_d1_final
)[1:10]

# Comprobar que todas suman aproximadamente 1
all(
  abs(
    rowSums(prob_general_d1_final) - 1
  ) < 1e-8
)


# =========================================================
# 23. PREDICCIÓN DEL RESULTADO
# =========================================================

pred_general_d1_final <- predict(
  modelo_multinom_general_d1_final,
  newdata = datos_general_d1_final,
  type = "class"
)

# Primeras predicciones
head(pred_general_d1_final)

# Distribución de las predicciones
table(
  pred_general_d1_final,
  useNA = "always"
)


# =========================================================
# 24. MATRIZ DE CONFUSIÓN
# =========================================================

# Niveles de la variable dependiente
niveles_resultado_general_d1_final <- levels(
  d1$resultado_partido_local
)

niveles_resultado_general_d1_final


# Resultados reales
real_general_d1_final <- factor(
  datos_general_d1_final$resultado_partido_local,
  levels = niveles_resultado_general_d1_final
)


# Predicciones con los mismos niveles
pred_general_d1_final <- factor(
  pred_general_d1_final,
  levels = niveles_resultado_general_d1_final
)


# Comprobar longitudes
length(real_general_d1_final)
length(pred_general_d1_final)

length(real_general_d1_final) ==
  length(pred_general_d1_final)


# Construir la matriz de confusión
mc_general_d1_final <- table(
  Real = real_general_d1_final,
  Predicho = pred_general_d1_final
)

mc_general_d1_final


# Matriz con totales
addmargins(
  mc_general_d1_final
)


# =========================================================
# 25. ACCURACY TOTAL
# =========================================================

accuracy_general_d1_final <- sum(
  diag(mc_general_d1_final)
) / sum(mc_general_d1_final)

accuracy_general_d1_final


# =========================================================
# 26. SENSIBILIDAD POR CATEGORÍA
# =========================================================

sensibilidad_general_d1_final <- diag(
  mc_general_d1_final
) / rowSums(mc_general_d1_final)

# Evitar NaN o Inf
sensibilidad_general_d1_final[
  is.nan(sensibilidad_general_d1_final) |
    is.infinite(sensibilidad_general_d1_final)
] <- NA

sensibilidad_general_d1_final


# =========================================================
# 27. PRECISIÓN POR CATEGORÍA
# =========================================================

precision_general_d1_final <- diag(
  mc_general_d1_final
) / colSums(mc_general_d1_final)

# Evitar NaN o Inf
precision_general_d1_final[
  is.nan(precision_general_d1_final) |
    is.infinite(precision_general_d1_final)
] <- NA

precision_general_d1_final


# =========================================================
# 28. BALANCED ACCURACY
# =========================================================

# Media de las sensibilidades de las tres categorías
balanced_accuracy_general_d1_final <- mean(
  sensibilidad_general_d1_final,
  na.rm = TRUE
)

balanced_accuracy_general_d1_final


# =========================================================
# 29. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_general_d1_final <- data.frame(
  Categoria = niveles_resultado_general_d1_final,
  
  Sensibilidad = round(
    as.numeric(
      sensibilidad_general_d1_final
    ),
    4
  ),
  
  Precision = round(
    as.numeric(
      precision_general_d1_final
    ),
    4
  )
)

metricas_general_d1_final


# =========================================================
# 30. TABLA DE MÉTRICAS GENERALES
# =========================================================

resultados_clasificacion_general_d1_final <- data.frame(
  Metrica = c(
    "Accuracy",
    "Balanced Accuracy"
  ),
  
  Valor = c(
    accuracy_general_d1_final,
    balanced_accuracy_general_d1_final
  )
)

resultados_clasificacion_general_d1_final <-
  resultados_clasificacion_general_d1_final %>%
  mutate(
    Valor = round(
      Valor,
      4
    )
  )

resultados_clasificacion_general_d1_final


# =========================================================
# 31. MATRIZ DE CORRELACIONES
# =========================================================

correlaciones_general_d1_final <- cor(
  datos_general_d1_final[
    variables_general_d1_final
  ],
  use = "complete.obs"
)

round(
  correlaciones_general_d1_final,
  3
)


# =========================================================
# 32. FUNCIÓN PARA CALCULAR EL VIF MANUAL
# =========================================================

calcular_vif_manual <- function(datos, variables) {
  
  resultados <- data.frame(
    Variable = variables,
    VIF = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(variables)) {
    
    variable_respuesta <- variables[i]
    variables_explicativas <- variables[-i]
    
    formula_auxiliar <- as.formula(
      paste(
        variable_respuesta,
        "~",
        paste(
          variables_explicativas,
          collapse = " + "
        )
      )
    )
    
    modelo_auxiliar <- lm(
      formula = formula_auxiliar,
      data = datos,
      na.action = na.omit
    )
    
    r2 <- summary(
      modelo_auxiliar
    )$r.squared
    
    # Controlar el caso extremo de R2 igual a 1
    if (is.na(r2) || r2 >= 1) {
      resultados$VIF[i] <- Inf
    } else {
      resultados$VIF[i] <- 1 / (1 - r2)
    }
  }
  
  resultados <- resultados %>%
    mutate(
      VIF = round(
        VIF,
        3
      )
    ) %>%
    arrange(
      desc(VIF)
    )
  
  return(resultados)
}


# =========================================================
# 33. VIF DEL MODELO GENERAL FINAL
# =========================================================

vif_general_d1_final <- calcular_vif_manual(
  datos = datos_general_d1_final,
  variables = variables_general_d1_final
)

vif_general_d1_final


# Variables con VIF superior a 5
vif_general_d1_final %>%
  filter(VIF > 5)


# Variables con VIF superior a 10
vif_general_d1_final %>%
  filter(VIF > 10)


# =========================================================
# 34. TABLA RESUMEN DEL AJUSTE
# =========================================================

resumen_ajuste_general_d1_final <- data.frame(
  Indicador = c(
    "Número de observaciones",
    "AIC",
    "Log-verosimilitud",
    "Accuracy",
    "Balanced Accuracy"
  ),
  Valor = c(
    nobs(modelo_multinom_general_d1_final),
    as.numeric(AIC_general_d1_final),
    as.numeric(logLik_general_d1_final),
    accuracy_general_d1_final,
    balanced_accuracy_general_d1_final
  )
)

resumen_ajuste_general_d1_final <- 
  resumen_ajuste_general_d1_final %>%
  mutate(
    Valor = round(
      Valor,
      4
    )
  )

resumen_ajuste_general_d1_final

# =========================================================
# GRAFICAS DEL MODELO GENERAL MULTINOMIAL DEPURADO
# Base de datos: d1
# Categoria de referencia: Empate
# Solo muestra completa, sin train/test
# =========================================================


# =========================================================
# 0. PAQUETES
# =========================================================

# Instalar solo si no estan disponibles
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("scales")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
# =========================================================
# 1. DATOS UTILIZADOS POR EL MODELO GENERAL FINAL
# =========================================================

datos_general_d1_final <- model.frame(
  modelo_multinom_general_d1_final
)

datos_general_d1_final <- droplevels(
  datos_general_d1_final
)

# Comprobar numero de observaciones
nrow(datos_general_d1_final)

# Debe devolver TRUE
nrow(datos_general_d1_final) ==
  nobs(modelo_multinom_general_d1_final)
# =========================================================
# 2. RAZONES DE POSIBILIDADES E INTERVALOS DE CONFIANZA
# =========================================================

resumen_general_grafico_d1 <- summary(
  modelo_multinom_general_d1_final
)

coef_general_grafico_d1 <-
  resumen_general_grafico_d1$coefficients

se_general_grafico_d1 <-
  resumen_general_grafico_d1$standard.errors

# Estadisticos de Wald
z_general_grafico_d1 <-
  coef_general_grafico_d1 /
  se_general_grafico_d1

# Valores p bilaterales
p_general_grafico_d1 <- 2 * (
  1 - pnorm(
    abs(z_general_grafico_d1)
  )
)
# =========================================================
# 2.1. TABLAS EN FORMATO LARGO
# =========================================================

tabla_coef_general_grafico_d1 <- as.data.frame(
  as.table(coef_general_grafico_d1)
)

colnames(tabla_coef_general_grafico_d1) <- c(
  "Resultado",
  "Variable",
  "Coeficiente"
)


tabla_se_general_grafico_d1 <- as.data.frame(
  as.table(se_general_grafico_d1)
)

colnames(tabla_se_general_grafico_d1) <- c(
  "Resultado",
  "Variable",
  "Error_estandar"
)


tabla_p_general_grafico_d1 <- as.data.frame(
  as.table(p_general_grafico_d1)
)

colnames(tabla_p_general_grafico_d1) <- c(
  "Resultado",
  "Variable",
  "P_valor"
)
# =========================================================
# 2.2. CALCULAR RAZONES E INTERVALOS
# =========================================================

datos_or_general_d1 <- tabla_coef_general_grafico_d1 %>%
  left_join(
    tabla_se_general_grafico_d1,
    by = c(
      "Resultado",
      "Variable"
    )
  ) %>%
  left_join(
    tabla_p_general_grafico_d1,
    by = c(
      "Resultado",
      "Variable"
    )
  ) %>%
  mutate(
    Razon_posibilidades = exp(
      Coeficiente
    ),
    
    IC_inferior = exp(
      Coeficiente -
        1.96 * Error_estandar
    ),
    
    IC_superior = exp(
      Coeficiente +
        1.96 * Error_estandar
    ),
    
    Comparacion = case_when(
      Resultado == "Derrota" ~
        "Derrota frente a Empate",
      
      Resultado == "Victoria" ~
        "Victoria frente a Empate",
      
      TRUE ~ as.character(Resultado)
    ),
    
    Significacion = case_when(
      P_valor < 0.05 ~
        "p < 0,05",
      
      P_valor >= 0.05 &
        P_valor < 0.10 ~
        "0,05 <= p < 0,10",
      
      TRUE ~
        "p >= 0,10"
    )
  ) %>%
  
  # Excluir el termino independiente
  filter(
    Variable != "(Intercept)"
  ) %>%
  
  # Mostrar resultados significativos o marginales
  filter(
    P_valor < 0.10
  )

# =========================================================
# 2.3. ETIQUETAS DE LAS VARIABLES
# =========================================================

etiquetas_general_d1 <- c(
  "diff_descanso" =
    "diferencia de descanso",
  
  "pos_previa_local" =
    "posición previa local",
  
  "pos_previa_visitante" =
    "posición previa visitante",
  
  "forma_visitante_5" =
    "forma visitante 5",
  
  "entradas_concedidas_local" =
    "entradas concedidas local",
  
  "duelos_ganados_local" =
    "duelos ganados local",
  
  "rojas_local" =
    "expulsiones local",
  
  "min_ventaja_numerica_local" =
    "tiempo de ventaja numérica local",
  
  "tiros_puerta_local" =
    "tiros a puerta local",
  
  "big_chances_local" =
    "grandes ocasiones local",
  
  "centros_local" =
    "centros local",
  
  "despejes_concedidos_local" =
    "despejes concedidos local",
  
  "xGA_local" =
    "xGA local",
  
  "big_chances_concedidas_local" =
    "grandes ocasiones concedidas local",
  
  "tiros_puerta_concedidos_local" =
    "tiros a puerta concedidos local",
  
  "corners_concedidos_local" =
    "córneres concedidos local",
  
  "pases_largos_concedidos_local" =
    "pases largos concedidos local",
  
  "centros_concedidos_local" =
    "centros concedidos local",
  
  "despejes_local" =
    "despejes local"
)


datos_or_general_d1 <- datos_or_general_d1 %>%
  mutate(
    Etiqueta = recode(
      Variable,
      !!!etiquetas_general_d1
    ),
    
    Comparacion = factor(
      Comparacion,
      levels = c(
        "Derrota frente a Empate",
        "Victoria frente a Empate"
      )
    ),
    
    Significacion = factor(
      Significacion,
      levels = c(
        "0,05 <= p < 0,10",
        "p < 0,05"
      ),
      labels = c(
        "0,05 ≤ p < 0,10",
        "p < 0,05"
      )
    )
  )
# =========================================================
# 2.4. ORDEN DE LAS VARIABLES EN CADA PANEL
# =========================================================

datos_or_general_d1 <- datos_or_general_d1 %>%
  group_by(Comparacion) %>%
  arrange(
    Razon_posibilidades,
    .by_group = TRUE
  ) %>%
  mutate(
    Etiqueta_ordenada = factor(
      Etiqueta,
      levels = unique(Etiqueta)
    )
  ) %>%
  ungroup()
# =========================================================
# 2.5. GRAFICO DE RAZONES DE POSIBILIDADES
# =========================================================

grafico_or_general_d1 <- ggplot(
  datos_or_general_d1,
  aes(
    x = Razon_posibilidades,
    y = Etiqueta_ordenada,
    color = Comparacion,
    shape = Significacion
  )
) +
  
  # Linea de ausencia de asociacion
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  # Intervalos de confianza
  geom_errorbarh(
    aes(
      xmin = IC_inferior,
      xmax = IC_superior
    ),
    height = 0,
    linewidth = 0.6,
    color = "grey35"
  ) +
  
  # Estimaciones puntuales
  geom_point(
    size = 2.8
  ) +
  
  # Un panel para cada comparacion
  facet_wrap(
    ~ Comparacion,
    ncol = 1,
    scales = "free_y"
  ) +
  
  # Escala logaritmica
  scale_x_log10(
    breaks = c(
      0.05,
      0.10,
      0.25,
      0.50,
      0.75,
      1,
      1.50,
      2,
      4,
      10,
      20
    ),
    
    labels = label_number(
      decimal.mark = ",",
      accuracy = 0.01
    ),
    
    expand = expansion(
      mult = c(
        0.06,
        0.08
      )
    )
  ) +
  
  # Colores iguales a los graficos anteriores
  scale_color_manual(
    values = c(
      "Derrota frente a Empate" =
        "#C44E52",
      
      "Victoria frente a Empate" =
        "#4C72B0"
    )
  ) +
  
  # Circulo: significativo
  # Triangulo: marginal
  scale_shape_manual(
    values = c(
      "0,05 ≤ p < 0,10" = 17,
      "p < 0,05" = 16
    )
  ) +
  
  labs(
    title =
      "Modelo general multinomial depurado",
    
    subtitle = paste0(
      "Razones de posibilidades e intervalos ",
      "de confianza del 95 %. ",
      "Categoría de referencia: Empate"
    ),
    
    x =
      "Razón de posibilidades, escala logarítmica",
    
    y = NULL,
    
    color =
      "Comparación",
    
    shape =
      "Significación"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "grey92"
    ),
    
    axis.text.y = element_text(
      size = 8.5
    ),
    
    legend.position = "bottom",
    
    legend.box = "horizontal",
    
    plot.margin = margin(
      10,
      15,
      10,
      10
    )
  )


grafico_or_general_d1
# =========================================================
# 3. PREDICCIONES DEL MODELO GENERAL FINAL
# =========================================================

pred_general_grafico_d1 <- predict(
  modelo_multinom_general_d1_final,
  newdata = datos_general_d1_final,
  type = "class"
)


orden_categorias_general <- c(
  "Empate",
  "Derrota",
  "Victoria"
)


real_general_grafico_d1 <- factor(
  datos_general_d1_final$resultado_partido_local,
  levels = orden_categorias_general
)


pred_general_grafico_d1 <- factor(
  pred_general_grafico_d1,
  levels = orden_categorias_general
)


# Debe devolver TRUE
length(real_general_grafico_d1) ==
  length(pred_general_grafico_d1)
# =========================================================
# 3.1. MATRIZ DE CONFUSION
# =========================================================

mc_general_grafico_d1 <- table(
  Real = real_general_grafico_d1,
  Predicho = pred_general_grafico_d1
)

mc_general_grafico_d1

addmargins(
  mc_general_grafico_d1
)
# =========================================================
# 3.2. DATOS PARA EL GRAFICO
# =========================================================

datos_mc_general_d1 <- as.data.frame(
  mc_general_grafico_d1
)

colnames(datos_mc_general_d1) <- c(
  "Real",
  "Predicho",
  "Frecuencia"
)


datos_mc_general_d1 <- datos_mc_general_d1 %>%
  group_by(Real) %>%
  mutate(
    Porcentaje =
      Frecuencia / sum(Frecuencia),
    
    Etiqueta = paste0(
      Frecuencia,
      "\n",
      sprintf(
        "%.1f%%",
        Porcentaje * 100
      )
    )
  ) %>%
  ungroup()


# Orden horizontal
datos_mc_general_d1$Predicho <- factor(
  datos_mc_general_d1$Predicho,
  levels = c(
    "Empate",
    "Derrota",
    "Victoria"
  )
)


# Con este orden, Victoria aparece arriba
datos_mc_general_d1$Real <- factor(
  datos_mc_general_d1$Real,
  levels = c(
    "Empate",
    "Derrota",
    "Victoria"
  )
)
# =========================================================
# 3.3. GRAFICO DE LA MATRIZ DE CONFUSION
# =========================================================

grafico_mc_general_d1 <- ggplot(
  datos_mc_general_d1,
  aes(
    x = Predicho,
    y = Real,
    fill = Porcentaje
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  
  geom_text(
    aes(
      label = Etiqueta
    ),
    size = 4,
    color = "black",
    lineheight = 0.95
  ) +
  
  scale_fill_gradient(
    low = "#F7F8FC",
    high = "#4C72B0",
    
    limits = c(
      0,
      1
    ),
    
    breaks = c(
      0.20,
      0.40,
      0.60,
      0.80
    ),
    
    labels = label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    
    name = "Porcentaje"
  ) +
  
  coord_equal() +
  
  labs(
    title =
      "Matriz de confusión del modelo general depurado",
    
    subtitle = paste0(
      "Porcentajes calculados dentro de ",
      "cada resultado real"
    ),
    
    x =
      "Resultado predicho",
    
    y =
      "Resultado real"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0
    ),
    
    panel.grid = element_blank(),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 10
    ),
    
    legend.position = "right",
    
    plot.margin = margin(
      10,
      15,
      10,
      10
    )
  )


grafico_mc_general_d1
# =========================================================
# 4. SENSIBILIDAD Y PRECISION
# =========================================================

sensibilidad_general_grafico_d1 <- diag(
  mc_general_grafico_d1
) / rowSums(
  mc_general_grafico_d1
)


precision_general_grafico_d1 <- diag(
  mc_general_grafico_d1
) / colSums(
  mc_general_grafico_d1
)


# Evitar valores no finitos
sensibilidad_general_grafico_d1[
  !is.finite(
    sensibilidad_general_grafico_d1
  )
] <- NA


precision_general_grafico_d1[
  !is.finite(
    precision_general_grafico_d1
  )
] <- NA
# =========================================================
# 4.1. TABLA DE METRICAS
# =========================================================

metricas_grafico_general_d1 <- data.frame(
  Categoria =
    orden_categorias_general,
  
  Precision = as.numeric(
    precision_general_grafico_d1[
      orden_categorias_general
    ]
  ),
  
  Sensibilidad = as.numeric(
    sensibilidad_general_grafico_d1[
      orden_categorias_general
    ]
  )
)


metricas_grafico_general_d1
metricas_largas_general_d1 <-
  metricas_grafico_general_d1 %>%
  pivot_longer(
    cols = c(
      Precision,
      Sensibilidad
    ),
    
    names_to =
      "Metrica",
    
    values_to =
      "Proporcion"
  ) %>%
  mutate(
    Categoria = factor(
      Categoria,
      levels = c(
        "Empate",
        "Derrota",
        "Victoria"
      )
    ),
    
    Metrica = factor(
      Metrica,
      levels = c(
        "Precision",
        "Sensibilidad"
      ),
      labels = c(
        "Precisión",
        "Sensibilidad"
      )
    )
  )
# =========================================================
# 4.2. GRAFICO DE BARRAS
# =========================================================

grafico_metricas_general_d1 <- ggplot(
  metricas_largas_general_d1,
  aes(
    x = Categoria,
    y = Proporcion,
    fill = Metrica
  )
) +
  
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.70
  ) +
  
  geom_text(
    aes(
      label = label_percent(
        accuracy = 0.1,
        decimal.mark = ","
      )(Proporcion)
    ),
    
    position = position_dodge(
      width = 0.75
    ),
    
    vjust = -0.5,
    size = 3.8
  ) +
  
  scale_fill_manual(
    values = c(
      "Precisión" =
        "#DD8452",
      
      "Sensibilidad" =
        "#4C72B0"
    )
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      1.05
    ),
    
    breaks = seq(
      0,
      1,
      by = 0.15
    ),
    
    labels = label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    
    expand = expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
  
  labs(
    title = paste0(
      "Sensibilidad y precisión del ",
      "modelo general depurado"
    ),
    
    x = NULL,
    
    y =
      "Proporción",
    
    fill = NULL
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      hjust = 0
    ),
    
    panel.grid.major.x =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    legend.position =
      "bottom",
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      10,
      15,
      10,
      10
    )
  )


grafico_metricas_general_d1
