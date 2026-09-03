# ==============================================================================
# Title: Physical and contextual multinomial model selection
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops and evaluates the physical and contextual blocks of the
# multinomial logistic regression analysis for the 2024/2025 season. The
# dependent variable distinguishes between a home-team victory, a draw and a
# home-team defeat, with the draw used as the reference category.
#
# The physical block is progressively simplified through the removal of
# disciplinary, physical and match-intensity variables. The candidate models
# include rest difference, fouls, tackles, duels, yellow cards, red cards and
# variables related to dismissals and numerical advantages. Successive
# specifications are compared according to statistical significance and model
# fit.
#
# The final physical model includes the difference in rest days, home-team
# duels won and home-team red cards. This specification is compared formally
# with an alternative physical model using a likelihood-ratio test.
#
# The contextual block examines the association between match outcomes and the
# circumstances preceding each match. Its predictors include rest information,
# previous league positions and the recent form of the home and visiting teams.
# A reduced contextual specification is obtained after removing the difference
# in rest days.
#
# For the selected physical and contextual models, the script extracts
# coefficients, odds ratios and bilateral Wald p-values. Results are presented
# separately for home defeat versus draw and home victory versus draw, with
# statistical evidence identified at the 5% and 10% significance levels.
#
# Global likelihood-ratio tests are used to assess the overall contribution of
# the explanatory variables. The script also calculates predicted
# probabilities and predicted outcome classes for the complete sample.
#
# Classification performance is evaluated through confusion matrices, overall
# accuracy, class-specific sensitivity, class-specific precision and mean
# sensitivity across the three match-outcome categories.
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
# Model-selection approach:
#   Manual backward reduction based on coefficient significance, model fit,
#   likelihood-ratio comparisons and substantive interpretation.
#
# Physical candidate models:
#   modelo_multinom_fisico_d1 to modelo_multinom_fisico_d13
#
# Final physical model:
#   modelo_multinom_fisico_final_d
#
# Contextual candidate models:
#   modelo_multinom_contexto_d
#   modelo_multinom_contexto_d1
#
# Final contextual model:
#   modelo_multinom_contexto_final
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, global likelihood-ratio tests,
#   predicted probabilities, confusion matrices, accuracy, mean sensitivity
#   and class-specific sensitivity and precision.
# ==============================================================================

#depuro amarillas forzadas


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
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d1)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d1 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d1
)

tabla_fisico_d1

##########################################
# quito amarillas local

modelo_multinom_fisico_d2 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_ganadas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d2)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d2 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d2
)

tabla_fisico_d2
#quito min ventaja numerica
modelo_multinom_fisico_d3 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_ganadas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d3)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d3 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d3
)

tabla_fisico_d3
#quito faltas recibidas local
modelo_multinom_fisico_d4 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    entradas_local +
    entradas_ganadas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d4)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d4 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d4
)

tabla_fisico_d4
#quito entradas ganadas local
modelo_multinom_fisico_d5 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    faltas_local +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d5)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d5 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d5
)

tabla_fisico_d5
#quito faltas local
modelo_multinom_fisico_d6 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d6)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d6 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d6
)

tabla_fisico_d6
#quito min roja local
modelo_multinom_fisico_d7 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local 
    ,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d7)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d7 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d7
)

tabla_fisico_d7
#quito entradas ganadas concedidas local
modelo_multinom_fisico_d8 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    entradas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d8)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d8 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d8
)

tabla_fisico_d8
#quito rojas forzadas local
modelo_multinom_fisico_d9 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    entradas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d9)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d9 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d9
)

tabla_fisico_d9
#quito entradas local
modelo_multinom_fisico_d10 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d10)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d10 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d10
)

tabla_fisico_d10
#quito duelos ganados local
modelo_multinom_fisico_d11 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    entradas_concedidas_local +
    rojas_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d11)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d11 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d11
)

tabla_fisico_d11
#aquí ha subido el AIC
#quito rojas local
modelo_multinom_fisico_d12 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    entradas_concedidas_local
  ,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d12)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d12 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d12
)

tabla_fisico_d12


#paro y me quedo con el modelo10 porque es el que mejor calidad de ajuste tiene

#he probado a eliminar la variable entradas concedidas y ha bajado su AIC
modelo_multinom_fisico_d13 <- multinom(
  resultado_partido_local ~
    diff_descanso +
    duelos_ganados_local +
    rojas_local,
  data = d,
  trace = FALSE
)
summary(modelo_multinom_fisico_d13)
tabla_fisico_d13 <- extraer_resultados_multinom(
  modelo_multinom_fisico_d13
)
tabla_fisico_d13


# Guardamos el modelo d13 con un nombre definitivo
modelo_multinom_fisico_final_d <- modelo_multinom_fisico_d13


# =========================================================
# 1. RESUMEN DEL MODELO FINAL
# =========================================================

summary(modelo_multinom_fisico_final_d)


# =========================================================
# 2. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_fisico_final_d <- extraer_resultados_multinom(
  modelo_multinom_fisico_final_d
)

# Tabla completa
tabla_fisico_final_d


# =========================================================
# 3. RESULTADOS SIGNIFICATIVOS AL 5%
# =========================================================

tabla_fisico_final_d %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 4. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_fisico_final_d %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 5. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_fisico_final_derrota_d <- tabla_fisico_final_d %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_fisico_final_derrota_d


# =========================================================
# 6. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_fisico_final_victoria_d <- tabla_fisico_final_d %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_fisico_final_victoria_d


# =========================================================
# 7. TEST GLOBAL DEL MODELO FINAL
# =========================================================

test_global_fisico_final_d <- test_lr_multinom(
  modelo_multinom_fisico_final_d,
  d
)

test_global_fisico_final_d



# =========================================================
# 9. COMPARACIÓN FORMAL ENTRE d10 Y d13
# =========================================================


# Test de razón de verosimilitud
anova(
  modelo_multinom_fisico_final_d,
  modelo_multinom_fisico_d10,
  test = "Chisq"
)


# =========================================================
# 10. PROBABILIDADES ESTIMADAS
# =========================================================

prob_fisico_final_d <- predict(
  modelo_multinom_fisico_final_d,
  newdata = d,
  type = "probs"
)

# Primeros seis partidos en escala 0-1
head(prob_fisico_final_d)


# =========================================================
# 11. PREDICCIÓN DE LA CATEGORÍA
# =========================================================

pred_fisico_final_d <- predict(
  modelo_multinom_fisico_final_d,
  newdata = d,
  type = "class"
)

head(pred_fisico_final_d)


# =========================================================
# 12. PREPARAR NIVELES DE RESULTADO
# =========================================================

niveles_resultado <- levels(
  d$resultado_partido_local
)

niveles_resultado

real_fisico_final_d <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_fisico_final_d <- factor(
  pred_fisico_final_d,
  levels = niveles_resultado
)


# =========================================================
# 13. MATRIZ DE CONFUSIÓN
# =========================================================

mc_fisico_final_d <- table(
  Real = real_fisico_final_d,
  Predicho = pred_fisico_final_d
)

mc_fisico_final_d

# Matriz incluyendo totales
addmargins(mc_fisico_final_d)


# =========================================================
# 14. ACCURACY TOTAL
# =========================================================

accuracy_fisico_final_d <-
  sum(diag(mc_fisico_final_d)) /
  sum(mc_fisico_final_d)

accuracy_fisico_final_d


# =========================================================
# 15. SENSIBILIDAD POR CATEGORÍA
# =========================================================

# De todos los casos reales de cada categoría,
# qué proporción identifica correctamente el modelo

sensibilidad_fisico_final_d <-
  diag(mc_fisico_final_d) /
  rowSums(mc_fisico_final_d)

sensibilidad_fisico_final_d


# =========================================================
# 16. PRECISIÓN POR CATEGORÍA
# =========================================================

# De todas las predicciones de cada categoría,
# qué proporción era realmente de esa categoría

precision_fisico_final_d <-
  diag(mc_fisico_final_d) /
  colSums(mc_fisico_final_d)

precision_fisico_final_d


# =========================================================
# 17. CORREGIR POSIBLES NaN O Inf
# =========================================================

sensibilidad_fisico_final_d[
  is.nan(sensibilidad_fisico_final_d) |
    is.infinite(sensibilidad_fisico_final_d)
] <- NA

precision_fisico_final_d[
  is.nan(precision_fisico_final_d) |
    is.infinite(precision_fisico_final_d)
] <- NA


# =========================================================
# 18. SENSIBILIDAD MEDIA
# =========================================================

sensibilidad_media_fisico_final_d <-
  mean(
    sensibilidad_fisico_final_d,
    na.rm = TRUE
  )

sensibilidad_media_fisico_final_d


# =========================================================
# 19. TABLA DE MÉTRICAS POR CATEGORÍA
# =========================================================

metricas_fisico_final_d <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_fisico_final_d),
    4
  ),
  Precision = round(
    as.numeric(precision_fisico_final_d),
    4
  )
)

metricas_fisico_final_d





# =========================================================
# 7. SIGNIFICATIVAS AL 5%
# =========================================================

tabla_fisico_d10 %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 8. SIGNIFICATIVAS O MARGINALES AL 10%
# =========================================================

tabla_fisico_d10 %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 9. DERROTA FRENTE A EMPATE
# =========================================================

tabla_fisico_derrota_d10 <- tabla_fisico_d10 %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_fisico_derrota_d10


# =========================================================
# 10. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_fisico_victoria_d10 <- tabla_fisico_d10 %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_fisico_victoria_d10


# =========================================================
# 11. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_fisico_d10 <- test_lr_multinom(
  modelo_multinom_fisico_d10,
  d
)

test_global_fisico_d10




# =========================================================
# 13. PROBABILIDADES ESTIMADAS
# =========================================================

prob_fisico_d10 <- predict(
  modelo_multinom_fisico_d10,
  newdata = d,
  type = "probs"
)

# Primeras probabilidades
head(prob_fisico_d10)




# =========================================================
# 14. PREDICCIÓN DE CLASE
# =========================================================

pred_fisico_d10 <- predict(
  modelo_multinom_fisico_d10,
  newdata = d,
  type = "class"
)

head(pred_fisico_d10)


# =========================================================
# 15. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado10 <- levels(
  d$resultado_partido_local
)

real_fisico_d10 <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado10
)

pred_fisico_d10 <- factor(
  pred_fisico_d10,
  levels = niveles_resultado10
)

mc_fisico_d10 <- table(
  Real = real_fisico_d10,
  Predicho = pred_fisico_d10
)

mc_fisico_d10


# =========================================================
# 16. MÉTRICAS
# =========================================================

accuracy_fisico_d10 <-
  sum(diag(mc_fisico_d10)) /
  sum(mc_fisico_d10)

sensibilidad_fisico_d10 <-
  diag(mc_fisico_d10) /
  rowSums(mc_fisico_d10)

precision_fisico_d10 <-
  diag(mc_fisico_d10) /
  colSums(mc_fisico_d10)

sensibilidad_fisico_d10[
  is.nan(sensibilidad_fisico_d10) |
    is.infinite(sensibilidad_fisico_d10)
] <- NA

precision_fisico_d10[
  is.nan(precision_fisico_d10) |
    is.infinite(precision_fisico_d10)
] <- NA

sensibilidad_media_fisico_d10 <-
  mean(
    sensibilidad_fisico_d10,
    na.rm = TRUE
  )

metricas_fisico_d10 <- data.frame(
  Categoria = niveles_resultado10,
  Sensibilidad = round(
    as.numeric(sensibilidad_fisico_d10),
    4
  ),
  Precision = round(
    as.numeric(precision_fisico_d10),
    4
  )
)

accuracy_fisico_d10
sensibilidad_media_fisico_d10
metricas_fisico_d10
#############################################################################

modelo_multinom_contexto_d <- multinom(
  resultado_partido_local ~
    diff_descanso +
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d,
  trace = FALSE
)
summary(modelo_multinom_contexto_d)

tabla_contexto_d <- extraer_resultados_multinom(
  modelo_multinom_contexto_d
)

tabla_contexto_d
#quito diferencia descanso
modelo_multinom_contexto_d1 <- multinom(
  resultado_partido_local ~
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d,
  trace = FALSE
)
summary(modelo_multinom_contexto_d1)

tabla_contexto_d1 <- extraer_resultados_multinom(
  modelo_multinom_contexto_d1
)

tabla_contexto_d1

modelo_multinom_contexto_final <- modelo_multinom_contexto_d1


tabla_contexto_final_d <- extraer_resultados_multinom(
  modelo_multinom_contexto_final
)

# Tabla completa
tabla_contexto_final_d


# =========================================================
# 7. RESULTADOS SIGNIFICATIVOS AL 5%
# =========================================================

tabla_contexto_final_d %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 8. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_contexto_final_d %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 9. DERROTA FRENTE A EMPATE
# =========================================================

tabla_contexto_final_derrota_d <- tabla_contexto_final_d %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_contexto_final_derrota_d


# =========================================================
# 10. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_contexto_final_victoria_d <- tabla_contexto_final_d %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_contexto_final_victoria_d


# =========================================================
# 11. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_contexto_final_d <- test_lr_multinom(
  modelo_multinom_contexto_final,
  d
)

test_global_contexto_final_d


# =========================================================
# 13. PROBABILIDADES ESTIMADAS
# =========================================================

prob_contexto_final_d <- predict(
  modelo_multinom_contexto_final,
  newdata = d,
  type = "probs"
)

# Primeros seis partidos
head(prob_contexto_final_d)

# Probabilidades expresadas en porcentaje
round(
  head(prob_contexto_final_d * 100),
  2
)

# Número de partidos
nrow(prob_contexto_final_d)

# Comprobar que cada fila suma 1
rowSums(prob_contexto_final_d)[1:10]

# Para abrir el conjunto completo:
# View(as.data.frame(prob_contexto_final_d))


# =========================================================
# 14. PREDICCIÓN DE RESULTADO
# =========================================================

pred_contexto_final_d <- predict(
  modelo_multinom_contexto_final,
  newdata = d,
  type = "class"
)

head(pred_contexto_final_d)


# =========================================================
# 15. PREPARAR NIVELES
# =========================================================

niveles_resultado <- levels(
  d$resultado_partido_local
)

real_contexto_final_d <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_contexto_final_d <- factor(
  pred_contexto_final_d,
  levels = niveles_resultado
)


# =========================================================
# 16. MATRIZ DE CONFUSIÓN
# =========================================================

mc_contexto_final_d <- table(
  Real = real_contexto_final_d,
  Predicho = pred_contexto_final_d
)

mc_contexto_final_d

# Con totales
addmargins(mc_contexto_final_d)


# =========================================================
# 17. ACCURACY
# =========================================================

accuracy_contexto_final_d <-
  sum(diag(mc_contexto_final_d)) /
  sum(mc_contexto_final_d)

accuracy_contexto_final_d


# =========================================================
# 18. SENSIBILIDAD POR CATEGORÍA
# =========================================================

# De todos los casos reales de cada categoría,
# qué proporción detecta correctamente el modelo

sensibilidad_contexto_final_d <-
  diag(mc_contexto_final_d) /
  rowSums(mc_contexto_final_d)

sensibilidad_contexto_final_d


# =========================================================
# 19. PRECISIÓN POR CATEGORÍA
# =========================================================

# De todas las predicciones de cada categoría,
# qué proporción era realmente de esa categoría

precision_contexto_final_d <-
  diag(mc_contexto_final_d) /
  colSums(mc_contexto_final_d)

precision_contexto_final_d


# =========================================================
# 20. CORREGIR POSIBLES NaN O Inf
# =========================================================

sensibilidad_contexto_final_d[
  is.nan(sensibilidad_contexto_final_d) |
    is.infinite(sensibilidad_contexto_final_d)
] <- NA

precision_contexto_final_d[
  is.nan(precision_contexto_final_d) |
    is.infinite(precision_contexto_final_d)
] <- NA


# =========================================================
# 21. SENSIBILIDAD MEDIA
# =========================================================

sensibilidad_media_contexto_final_d <-
  mean(
    sensibilidad_contexto_final_d,
    na.rm = TRUE
  )

sensibilidad_media_contexto_final_d


# =========================================================
# 22. TABLA DE MÉTRICAS
# =========================================================

metricas_contexto_final_d <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_contexto_final_d),
    4
  ),
  Precision = round(
    as.numeric(precision_contexto_final_d),
    4
  )
)

metricas_contexto_final_d



