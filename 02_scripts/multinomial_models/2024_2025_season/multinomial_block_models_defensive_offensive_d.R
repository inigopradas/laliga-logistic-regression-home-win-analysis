# ==============================================================================
# Title: Defensive and offensive multinomial block models
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops defensive and offensive multinomial logistic regression
# models to explain match outcomes during the 2024/2025 season. The dependent
# variable distinguishes between home-team victory, draw and home-team defeat,
# with the draw used as the reference category.
#
# The script defines a reusable function that extracts the estimated
# coefficients, odds ratios and bilateral Wald p-values from multinomial
# regression models. The resulting tables identify coefficients that are
# statistically significant at the 5% level or marginally significant at the
# 10% level.
#
# The defensive block assesses the association between match outcomes and
# defensive performance variables, including expected goals against, big
# chances conceded, shots on target conceded, corners conceded, passes
# conceded in the final third, long passes conceded, crosses conceded,
# interceptions, clearances and disciplinary variables. A reduced defensive
# specification is estimated after removing the red-card variables. The
# complete and reduced specifications are compared using the Akaike information
# criterion and a likelihood-ratio test.
#
# The offensive block assesses the association between match outcomes and
# attacking production, passing in advanced areas, crossing, disciplinary
# events, recoveries and clearances. Successive reduced specifications are
# estimated by removing red-card variables and home recoveries. The final
# offensive specification retains shots on target, big chances, passes in the
# final third, crosses, recoveries and clearances related to both the home team
# and its opponent.
#
# For each principal specification, the script calculates predicted
# probabilities for draws, defeats and victories and verifies that the three
# probabilities sum to one for every observation. Predicted outcome classes
# are then used to construct confusion matrices.
#
# Classification performance is evaluated using overall accuracy,
# class-specific sensitivity, class-specific precision and balanced accuracy,
# defined as the mean sensitivity across the three outcome categories.
#
# The script also separates the regression results into the comparisons of
# defeat versus draw and victory versus draw. Global likelihood-ratio tests are
# used to assess the overall contribution of each explanatory variable to the
# corresponding multinomial block model.
#
# Dataset:
#   d, corresponding to the 2024/2025 season.
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
# Variable-selection approach:
#   Manual reduction of the defensive and offensive specifications based on
#   coefficient significance, global likelihood-ratio tests, AIC comparisons
#   and substantive interpretation.
#
# Main defensive models:
#   modelo_multinom_defensivo_d
#   modelo_multinom_defensivo_d_1
#
# Main offensive models:
#   modelo_multinom_ofensivo_d
#   modelo_multinom_ofensivo_d1
#   modelo_multinom_ofensivo_d2
#   modelo_multinom_ofensivo_d3
#
# Final defensive specification:
#   modelo_multinom_defensivo_d_1
#
# Final offensive specification:
#   modelo_multinom_ofensivo_d3
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, global likelihood-ratio tests,
#   predicted probabilities, confusion matrices, accuracy, balanced accuracy,
#   class-specific sensitivity and class-specific precision.
# ==============================================================================

extraer_resultados_multinom <- function(modelo) {
  
  resumen <- summary(modelo)
  
  coeficientes <- resumen$coefficients
  errores <- resumen$standard.errors
  
  z <- coeficientes / errores
  p <- 2 * (1 - pnorm(abs(z)))
  OR <- exp(coeficientes)
  
  tabla_coef <- as.data.frame(as.table(coeficientes))
  colnames(tabla_coef) <- c("Resultado", "Variable", "Coeficiente")
  
  tabla_or <- as.data.frame(as.table(OR))
  colnames(tabla_or) <- c("Resultado", "Variable", "Odds_Ratio")
  
  tabla_p <- as.data.frame(as.table(p))
  colnames(tabla_p) <- c("Resultado", "Variable", "P_valor")
  
  tabla <- tabla_coef %>%
    left_join(tabla_or, by = c("Resultado", "Variable")) %>%
    left_join(tabla_p, by = c("Resultado", "Variable")) %>%
    mutate(
      Coeficiente = round(Coeficiente, 4),
      Odds_Ratio = round(Odds_Ratio, 4),
      P_valor = round(P_valor, 4),
      Significativa_5 = ifelse(P_valor < 0.05, "Sí", "No"),
      Significativa_10 = ifelse(P_valor < 0.10, "Sí", "No")
    )
  
  return(tabla)
}

################################################################


# =========================================================
# BLOQUE DEFENSIVO MULTINOMIAL - DATOS d
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. COMPROBACIONES PREVIAS
# =========================================================

# Comprobar categorías de la variable respuesta
table(d$resultado_partido_local)

# Asegurarnos de que es factor
d$resultado_partido_local <- factor(d$resultado_partido_local)

# Establecer Empate como categoría de referencia
d$resultado_partido_local <- relevel(
  d$resultado_partido_local,
  ref = "Empate"
)

# Comprobar niveles
levels(d$resultado_partido_local)


# =========================================================
# 2. MODELO DEFENSIVO MULTINOMIAL COMPLETO
# =========================================================

modelo_multinom_defensivo_d <- multinom(
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
  data = d,
  trace = FALSE
)

# Resumen general del modelo
summary(modelo_multinom_defensivo_d)


# =========================================================
# 3. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_defensivo_d <- extraer_resultados_multinom(
  modelo_multinom_defensivo_d
)

# Mostrar tabla completa
tabla_defensivo_d


# =========================================================
# 4. COEFICIENTES SIGNIFICATIVOS AL 5%
# =========================================================

tabla_defensivo_d %>%
  filter(P_valor < 0.05)


# =========================================================
# 5. COEFICIENTES SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_defensivo_d %>%
  filter(P_valor < 0.10)


# =========================================================
# 6. RESULTADOS SEPARADOS POR COMPARACIÓN
# =========================================================

# Derrota frente a Empate
tabla_defensivo_derrota_d <- tabla_defensivo_d %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_defensivo_derrota_d


# Victoria frente a Empate
tabla_defensivo_victoria_d <- tabla_defensivo_d %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_defensivo_victoria_d


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_defensivo_d <- test_lr_multinom(
  modelo_multinom_defensivo_d,
  d
)

test_global_defensivo_d




# =========================================================
# 8. PROBABILIDADES ESTIMADAS
# =========================================================

prob_defensivo_d <- predict(
  modelo_multinom_defensivo_d,
  newdata = d,
  type = "probs"
)

# Primeros partidos en porcentaje
round(head(prob_defensivo_d * 100), 2)

# Comprobar que las probabilidades suman 1
rowSums(prob_defensivo_d)[1:10]


# =========================================================
# 10. PREDICCIÓN DE RESULTADO
# =========================================================

pred_defensivo_d <- predict(
  modelo_multinom_defensivo_d,
  newdata = d,
  type = "class"
)

head(pred_defensivo_d)


# =========================================================
# 11. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado <- levels(d$resultado_partido_local)

real_defensivo_d <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_defensivo_d <- factor(
  pred_defensivo_d,
  levels = niveles_resultado
)

mc_defensivo_d <- table(
  Real = real_defensivo_d,
  Predicho = pred_defensivo_d
)

mc_defensivo_d
addmargins(mc_defensivo_d)


# =========================================================
# 12. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

# Accuracy total
accuracy_defensivo_d <-
  sum(diag(mc_defensivo_d)) / sum(mc_defensivo_d)

# Sensibilidad por categoría
sensibilidad_defensivo_d <-
  diag(mc_defensivo_d) / rowSums(mc_defensivo_d)

# Precisión por categoría
precision_defensivo_d <-
  diag(mc_defensivo_d) / colSums(mc_defensivo_d)

# Evitar NaN o Inf si alguna categoría no se predice
sensibilidad_defensivo_d[
  is.nan(sensibilidad_defensivo_d) |
    is.infinite(sensibilidad_defensivo_d)
] <- NA

precision_defensivo_d[
  is.nan(precision_defensivo_d) |
    is.infinite(precision_defensivo_d)
] <- NA

# Sensibilidad media de las tres categorías
balanced_accuracy_defensivo_d <-
  mean(sensibilidad_defensivo_d, na.rm = TRUE)

# Tabla de métricas
metricas_defensivo_d <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_defensivo_d),
    4
  ),
  Precision = round(
    as.numeric(precision_defensivo_d),
    4
  )
)

accuracy_defensivo_d
balanced_accuracy_defensivo_d
metricas_defensivo_d
#vamos a pasar a depurar varaibles no significativas. Tenemos rojas_local, puedo quitar pases_U3_concedidos_local
#quito rojas, he quitado primero las local y luego la visitante porque ninguna es significativa
modelo_multinom_defensivo_d_1 <- multinom(
  resultado_partido_local ~
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    intercepciones_local +
    despejes_local 
  ,
  data = d,
  trace = FALSE
)

# Resumen general del modelo
summary(modelo_multinom_defensivo_d_1)

tabla_defensivo_d_1 <- extraer_resultados_multinom(
  modelo_multinom_defensivo_d_1
)

# Mostrar tabla completa
tabla_defensivo_d_1


# =========================================================
# 4. COEFICIENTES SIGNIFICATIVOS AL 5%
# =========================================================

tabla_defensivo_d_1 %>%
  filter(P_valor < 0.05)


# =========================================================
# 5. COEFICIENTES SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_defensivo_d_1 %>%
  filter(P_valor < 0.10)


# =========================================================
# 6. RESULTADOS SEPARADOS POR COMPARACIÓN
# =========================================================

# Derrota frente a Empate
tabla_defensivo_derrota_d_1 <- tabla_defensivo_d_1 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_defensivo_derrota_d_1


# Victoria frente a Empate
tabla_defensivo_victoria_d_1 <- tabla_defensivo_d_1 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_defensivo_victoria_d_1


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_defensivo_d_1 <- test_lr_multinom(
  modelo_multinom_defensivo_d_1,
  d
)

test_global_defensivo_d_1
# =========================================================
# 8. PROBABILIDADES ESTIMADAS
# =========================================================

prob_defensivo_d_1 <- predict(
  modelo_multinom_defensivo_d_1,
  newdata = d,
  type = "probs"
)

# Primeros partidos en porcentaje
round(head(prob_defensivo_d_1 * 100), 2)

# Comprobar que las probabilidades suman 1
rowSums(prob_defensivo_d_1)[1:10]


# =========================================================
# 10. PREDICCIÓN DE RESULTADO
# =========================================================

pred_defensivo_d_1 <- predict(
  modelo_multinom_defensivo_d_1,
  newdata = d,
  type = "class"
)

head(pred_defensivo_d_1)


# =========================================================
# 11. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado <- levels(d$resultado_partido_local)

real_defensivo_d_1 <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_defensivo_d_1 <- factor(
  pred_defensivo_d_1,
  levels = niveles_resultado
)

mc_defensivo_d_1 <- table(
  Real = real_defensivo_d_1,
  Predicho = pred_defensivo_d_1
)

mc_defensivo_d_1
addmargins(mc_defensivo_d_1)


# =========================================================
# 12. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

# Accuracy total
accuracy_defensivo_d_1 <-
  sum(diag(mc_defensivo_d_1)) / sum(mc_defensivo_d_1)

# Sensibilidad por categoría
sensibilidad_defensivo_d_1 <-
  diag(mc_defensivo_d_1) / rowSums(mc_defensivo_d_1)

# Precisión por categoría
precision_defensivo_d_1 <-
  diag(mc_defensivo_d_1) / colSums(mc_defensivo_d_1)

# Evitar NaN o Inf si alguna categoría no se predice
sensibilidad_defensivo_d_1[
  is.nan(sensibilidad_defensivo_d_1) |
    is.infinite(sensibilidad_defensivo_d_1)
] <- NA

precision_defensivo_d_1[
  is.nan(precision_defensivo_d_1) |
    is.infinite(precision_defensivo_d_1)
] <- NA

# Sensibilidad media de las tres categorías
balanced_accuracy_defensivo_d_1 <-
  mean(sensibilidad_defensivo_d_1, na.rm = TRUE)

# Tabla de métricas
metricas_defensivo_d_1 <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_defensivo_d_1),
    4
  ),
  Precision = round(
    as.numeric(precision_defensivo_d_1),
    4
  )
)

accuracy_defensivo_d_1
balanced_accuracy_defensivo_d_1
metricas_defensivo_d_1


# Comparación de AIC
AIC(
  modelo_multinom_defensivo_d,
  modelo_multinom_defensivo_d_1
)

# Test de razón de verosimilitud
anova(
  modelo_multinom_defensivo_d_1,
  modelo_multinom_defensivo_d,
  test = "Chisq"
)
################################################

# =========================================================
# BLOQUE OFENSIVO MULTINOMIAL - DATOS d
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. COMPROBACIONES PREVIAS
# =========================================================

# Distribución de la variable dependiente
table(d$resultado_partido_local)
prop.table(table(d$resultado_partido_local))

# Limpiar posibles espacios
d$resultado_partido_local <- trimws(
  as.character(d$resultado_partido_local)
)

# Convertir a factor
d$resultado_partido_local <- factor(
  d$resultado_partido_local
)

# Establecer Empate como categoría de referencia
d$resultado_partido_local <- relevel(
  d$resultado_partido_local,
  ref = "Empate"
)

# Comprobar niveles
levels(d$resultado_partido_local)


# =========================================================
# 2. COMPROBAR LAS VARIABLES DEL BLOQUE OFENSIVO
# =========================================================

variables_ofensivas_d <- c(
  "tiros_puerta_local",
  "big_chances_local",
  "pases_U3_local",
  "centros_local",
  "rojas_local",
  "recuperaciones_concedidas_local",
  "despejes_concedidos_local",
  "recuperaciones_local",
  "despejes_local"
)

# Comprobar si existen todas las variables
variables_ofensivas_d[
  !variables_ofensivas_d %in% names(d)
]

# Si devuelve character(0), existen todas.


# =========================================================
# 3. COMPROBAR VALORES AUSENTES
# =========================================================

colSums(
  is.na(
    d[
      c(
        "resultado_partido_local",
        variables_ofensivas_d
      )
    ]
  )
)


# =========================================================
# 4. MODELO OFENSIVO MULTINOMIAL COMPLETO
# =========================================================

modelo_multinom_ofensivo_d <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    big_chances_local +
    pases_U3_local +
    centros_local +
    rojas_local +
    rojas_forzadas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    despejes_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_ofensivo_d)


# =========================================================
# 5. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_ofensivo_d <- extraer_resultados_multinom(
  modelo_multinom_ofensivo_d
)

# Tabla completa
tabla_ofensivo_d


# =========================================================
# 6. RESULTADOS SIGNIFICATIVOS AL 5%
# =========================================================

tabla_ofensivo_d %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 7. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_ofensivo_d %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 8. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_derrota_d <- tabla_ofensivo_d %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_ofensivo_derrota_d


# =========================================================
# 9. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_victoria_d <- tabla_ofensivo_d %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_ofensivo_victoria_d


# =========================================================
# 10. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_ofensivo_d <- test_lr_multinom(
  modelo_multinom_ofensivo_d,
  d
)

test_global_ofensivo_d


# =========================================================
# 11. BONDAD DE AJUSTE
# =========================================================

AIC_ofensivo_d <- AIC(
  modelo_multinom_ofensivo_d
)

logLik_ofensivo_d <- logLik(
  modelo_multinom_ofensivo_d
)

AIC_ofensivo_d
logLik_ofensivo_d


# =========================================================
# 12. PROBABILIDADES ESTIMADAS
# =========================================================

prob_ofensivo_d <- predict(
  modelo_multinom_ofensivo_d,
  newdata = d,
  type = "probs"
)

# Primeros seis partidos en escala 0-1
head(prob_ofensivo_d)

# Primeros seis partidos en porcentaje
round(
  head(prob_ofensivo_d * 100),
  2
)

# Número total de partidos con probabilidades
nrow(prob_ofensivo_d)

# Comprobar que cada fila suma 1
rowSums(prob_ofensivo_d)[1:10]


# =========================================================
# 13. PREDICCIÓN DE RESULTADO
# =========================================================

pred_ofensivo_d <- predict(
  modelo_multinom_ofensivo_d,
  newdata = d,
  type = "class"
)

head(pred_ofensivo_d)


# =========================================================
# 14. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado <- levels(
  d$resultado_partido_local
)

real_ofensivo_d <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_ofensivo_d <- factor(
  pred_ofensivo_d,
  levels = niveles_resultado
)

mc_ofensivo_d <- table(
  Real = real_ofensivo_d,
  Predicho = pred_ofensivo_d
)

mc_ofensivo_d
addmargins(mc_ofensivo_d)


# =========================================================
# 15. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

# Accuracy
accuracy_ofensivo_d <-
  sum(diag(mc_ofensivo_d)) /
  sum(mc_ofensivo_d)

# Sensibilidad por categoría
sensibilidad_ofensivo_d <-
  diag(mc_ofensivo_d) /
  rowSums(mc_ofensivo_d)

# Precisión por categoría
precision_ofensivo_d <-
  diag(mc_ofensivo_d) /
  colSums(mc_ofensivo_d)

# Evitar NaN e Inf si alguna categoría nunca se predice
sensibilidad_ofensivo_d[
  is.nan(sensibilidad_ofensivo_d) |
    is.infinite(sensibilidad_ofensivo_d)
] <- NA

precision_ofensivo_d[
  is.nan(precision_ofensivo_d) |
    is.infinite(precision_ofensivo_d)
] <- NA

# Media de sensibilidad entre las tres categorías
sensibilidad_media_ofensivo_d <-
  mean(
    sensibilidad_ofensivo_d,
    na.rm = TRUE
  )

# Tabla de métricas por categoría
metricas_ofensivo_d <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_ofensivo_d),
    4
  ),
  Precision = round(
    as.numeric(precision_ofensivo_d),
    4
  )
)

accuracy_ofensivo_d
sensibilidad_media_ofensivo_d
metricas_ofensivo_d

#quito rojas visitante

modelo_multinom_ofensivo_d1 <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    big_chances_local +
    pases_U3_local +
    centros_local +
    rojas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    despejes_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_ofensivo_d1)


# =========================================================
# 5. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_ofensivo_d1 <- extraer_resultados_multinom(
  modelo_multinom_ofensivo_d1
)

# Tabla completa
tabla_ofensivo_d1

#quito rojas local:

modelo_multinom_ofensivo_d2 <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    big_chances_local +
    pases_U3_local +
    centros_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    despejes_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_ofensivo_d2)


# =========================================================
# 5. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_ofensivo_d2 <- extraer_resultados_multinom(
  modelo_multinom_ofensivo_d2
)

# Tabla completa
tabla_ofensivo_d2

#quito recuperaciones local
modelo_multinom_ofensivo_d3 <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    big_chances_local +
    pases_U3_local +
    centros_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    despejes_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_ofensivo_d3)


# =========================================================
# 5. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_ofensivo_d3 <- extraer_resultados_multinom(
  modelo_multinom_ofensivo_d3
)

# Tabla completa
tabla_ofensivo_d3

#con esto me sirve:
# =========================================================
# 6. RESULTADOS SIGNIFICATIVOS AL 5%
# =========================================================

tabla_ofensivo_d3 %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 7. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_ofensivo_d3 %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 8. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_derrota_d3 <- tabla_ofensivo_d3 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_ofensivo_derrota_d3


# =========================================================
# 9. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_ofensivo_victoria_d3 <- tabla_ofensivo_d3 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_ofensivo_victoria_d3


# =========================================================
# 10. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_ofensivo_d <- test_lr_multinom(
  modelo_multinom_ofensivo_d,
  d
)

test_global_ofensivo_d


# =========================================================
# 11. BONDAD DE AJUSTE
# =========================================================

AIC_ofensivo_d3 <- AIC(
  modelo_multinom_ofensivo_d3
)

logLik_ofensivo_d3 <- logLik(
  modelo_multinom_ofensivo_d3
)

AIC_ofensivo_d3
logLik_ofensivo_d3


# =========================================================
# 12. PROBABILIDADES ESTIMADAS
# =========================================================

prob_ofensivo_d3 <- predict(
  modelo_multinom_ofensivo_d3,
  newdata = d,
  type = "probs"
)

# Primeros seis partidos en escala 0-1
head(prob_ofensivo_d3)

# Primeros seis partidos en porcentaje
round(
  head(prob_ofensivo_d3 * 100),
  2
)

# Número total de partidos con probabilidades
nrow(prob_ofensivo_d3)

# Comprobar que cada fila suma 1
rowSums(prob_ofensivo_d3)[1:10]


# =========================================================
# 13. PREDICCIÓN DE RESULTADO
# =========================================================

pred_ofensivo_d3 <- predict(
  modelo_multinom_ofensivo_d3,
  newdata = d,
  type = "class"
)

head(pred_ofensivo_d3)


# =========================================================
# 14. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado3 <- levels(
  d$resultado_partido_local
)

real_ofensivo_d3 <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_ofensivo_d3 <- factor(
  pred_ofensivo_d3,
  levels = niveles_resultado
)

mc_ofensivo_d3 <- table(
  Real = real_ofensivo_d3,
  Predicho = pred_ofensivo_d3
)

mc_ofensivo_d3
addmargins(mc_ofensivo_d3)


# =========================================================
# 15. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

# Accuracy
accuracy_ofensivo_d3 <-
  sum(diag(mc_ofensivo_d3)) /
  sum(mc_ofensivo_d3)

# Sensibilidad por categoría
sensibilidad_ofensivo_d3 <-
  diag(mc_ofensivo_d3) /
  rowSums(mc_ofensivo_d3)

# Precisión por categoría
precision_ofensivo_d3 <-
  diag(mc_ofensivo_d3) /
  colSums(mc_ofensivo_d3)

# Evitar NaN e Inf si alguna categoría nunca se predice
sensibilidad_ofensivo_d3[
  is.nan(sensibilidad_ofensivo_d3) |
    is.infinite(sensibilidad_ofensivo_d3)
] <- NA

precision_ofensivo_d3[
  is.nan(precision_ofensivo_d3) |
    is.infinite(precision_ofensivo_d3)
] <- NA

# Media de sensibilidad entre las tres categorías
sensibilidad_media_ofensivo_d3 <-
  mean(
    sensibilidad_ofensivo_d3,
    na.rm = TRUE
  )

# Tabla de métricas por categoría
metricas_ofensivo_d3 <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_ofensivo_d3),
    4
  ),
  Precision = round(
    as.numeric(precision_ofensivo_d3),
    4
  )
)

accuracy_ofensivo_d3
sensibilidad_media_ofensivo_d3
metricas_ofensivo_d3
