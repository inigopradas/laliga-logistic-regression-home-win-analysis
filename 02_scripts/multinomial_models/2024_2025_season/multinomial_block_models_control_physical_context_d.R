# ==============================================================================
# Title: Match-control, physical and contextual multinomial block models
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops multinomial logistic regression models for three
# analytical dimensions of football match performance during the 2024/2025
# season: match control, physical performance and competitive context.
#
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat. The draw is used as the reference category, meaning that
# the estimated coefficients compare defeat versus draw and victory versus draw.
#
# The match-control block examines possession, passing volume, opposition
# passing, territorial distribution of passes, corners, free kicks and duels.
# The model is progressively simplified by removing free-kick variables.
# Multicollinearity is examined using correlation matrices and variance
# inflation factors. Alternative specifications are also estimated using
# possession alone, passing variables alone, the home team's share of total
# passes and the number of passes per percentage point of possession.
#
# The complete reduced match-control specification is ultimately retained.
# This model includes possession, home and opposition passing, passing
# distribution across territorial zones, home and opposition corners and
# home-team duels won.
#
# The physical block examines rest differences, fouls, tackles, duels,
# disciplinary actions and the timing of red-card or numerical-advantage
# events. Structural missing values for red-card timing and numerical advantage
# are coded as zero when the corresponding event did not occur.
#
# The contextual block examines the competitive circumstances preceding each
# match. Its predictors include the difference in rest days, the absence of
# previous rest information, the previous league positions of both teams and
# the recent form of the home and visiting teams.
#
# For each block, the script extracts coefficients, odds ratios and bilateral
# Wald p-values. Results are reported separately for defeat versus draw and
# victory versus draw, with evidence identified at the 5% and 10% levels.
# Global likelihood-ratio tests are also used to evaluate the overall
# contribution of each explanatory variable.
#
# Model fit is assessed using the Akaike information criterion and
# log-likelihood. Predicted probabilities are calculated for all three match
# outcomes and converted into predicted classes. Classification performance is
# evaluated using confusion matrices, overall accuracy, class-specific
# sensitivity, class-specific precision and mean sensitivity across the three
# outcome categories.
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
# Main match-control models:
#   modelo_multinom_control_d
#   modelo_multinom_control_d1
#   modelo_multinom_control_d2
#   modelo_control_solo_posesion_d
#   modelo_control_solo_pases_d
#   modelo_control_cuota_pases_d
#   modelo_pases_por_punto_posesion
#
# Final match-control specification:
#   modelo_multinom_control_d2
#
# Main physical model:
#   modelo_multinom_fisico_d
#
# Main contextual model:
#   modelo_multinom_contexto_d
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, global likelihood-ratio tests,
#   VIF results, correlation matrices, model-fit statistics, predicted
#   probabilities, confusion matrices, accuracy, mean sensitivity and
#   class-specific sensitivity and precision.
# ==============================================================================

# =========================================================
# 1. COMPROBACIONES PREVIAS
# =========================================================

# Distribución de la variable dependiente
table(d$resultado_partido_local)

# Distribución en porcentaje
prop.table(
  table(d$resultado_partido_local)
)

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
# 2. VARIABLES DEL BLOQUE DE CONTROL
# =========================================================

variables_control_d <- c(
  "posesion_local",
  "pases_local",
  "pases_en_contra_local",
  "zona2_pases_pct",
  "zona3_pases_pct",
  "corners_local",
  "corners_concedidos_local",
  "tiros_libres_local",
  "tiros_libres_concedidos_local",
  "duelos_ganados_local"
)

# Comprobar si todas las variables existen en d
variables_control_d[
  !variables_control_d %in% names(d)
]

# Si devuelve:
# character(0)
# significa que existen todas las variables.
#
# Si aparece algún nombre, comprueba cómo se llama
# exactamente esa columna mediante:
# names(d)


# =========================================================
# 3. COMPROBAR TIPO DE LAS VARIABLES
# =========================================================

str(
  d[
    variables_control_d[
      variables_control_d %in% names(d)
    ]
  ]
)

# Las variables del bloque deberían ser numéricas.
# Comprueba individualmente si alguna aparece como character
# o factor antes de estimar el modelo.


# =========================================================
# 4. COMPROBAR VALORES AUSENTES
# =========================================================

colSums(
  is.na(
    d[
      c(
        "resultado_partido_local",
        variables_control_d[
          variables_control_d %in% names(d)
        ]
      )
    ]
  )
)


# =========================================================
# 5. MODELO MULTINOMIAL DE CONTROL COMPLETO
# =========================================================

modelo_multinom_control_d <- multinom(
  resultado_partido_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    zona2_pases_pct +
    zona3_pases_pct +
    corners_local +
    corners_concedidos_local +
    tiros_libres_local +
    tiros_libres_concedidos_local +
    duelos_ganados_local,
  data = d,
  trace = FALSE
)

# Resumen del modelo
summary(modelo_multinom_control_d)


# =========================================================
# 6. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_control_d <- extraer_resultados_multinom(
  modelo_multinom_control_d
)

# Tabla completa
tabla_control_d


# =========================================================
# 7. RESULTADOS SIGNIFICATIVOS AL 5%
# =========================================================

tabla_control_d %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 8. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_control_d %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 9. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_control_derrota_d <- tabla_control_d %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_control_derrota_d


# =========================================================
# 10. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_control_victoria_d <- tabla_control_d %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_control_victoria_d


# =========================================================
# 11. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_control_d <- test_lr_multinom(
  modelo_multinom_control_d,
  d
)

test_global_control_d


# =========================================================
# 12. BONDAD DE AJUSTE
# =========================================================

AIC_control_d <- AIC(
  modelo_multinom_control_d
)

logLik_control_d <- logLik(
  modelo_multinom_control_d
)

AIC_control_d
logLik_control_d


# =========================================================
# 13. PROBABILIDADES ESTIMADAS
# =========================================================

prob_control_d <- predict(
  modelo_multinom_control_d,
  newdata = d,
  type = "probs"
)

# Primeros seis partidos en escala 0-1
head(prob_control_d)

# Primeros seis partidos en porcentaje
round(
  head(prob_control_d * 100),
  2
)

# Número total de partidos
nrow(prob_control_d)

# Comprobar que las probabilidades suman 1
rowSums(prob_control_d)[1:10]


# =========================================================
# 14. PREDICCIÓN DE RESULTADO
# =========================================================

pred_control_d <- predict(
  modelo_multinom_control_d,
  newdata = d,
  type = "class"
)

head(pred_control_d)


# =========================================================
# 15. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado <- levels(
  d$resultado_partido_local
)

real_control_d <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_control_d <- factor(
  pred_control_d,
  levels = niveles_resultado
)

mc_control_d <- table(
  Real = real_control_d,
  Predicho = pred_control_d
)

mc_control_d

# Matriz con totales
addmargins(mc_control_d)


# =========================================================
# 16. MÉTRICAS DE CLASIFICACIÓN
# =========================================================

# Accuracy total
accuracy_control_d <-
  sum(diag(mc_control_d)) /
  sum(mc_control_d)

# Sensibilidad:
# de todos los resultados reales de cada categoría,
# qué proporción identifica correctamente el modelo
sensibilidad_control_d <-
  diag(mc_control_d) /
  rowSums(mc_control_d)

# Precisión:
# de todas las predicciones de cada categoría,
# qué proporción era realmente de esa categoría
precision_control_d <-
  diag(mc_control_d) /
  colSums(mc_control_d)

# Evitar NaN o Inf si una categoría nunca se predice
sensibilidad_control_d[
  is.nan(sensibilidad_control_d) |
    is.infinite(sensibilidad_control_d)
] <- NA

precision_control_d[
  is.nan(precision_control_d) |
    is.infinite(precision_control_d)
] <- NA

# Sensibilidad media entre las tres categorías
sensibilidad_media_control_d <-
  mean(
    sensibilidad_control_d,
    na.rm = TRUE
  )

# Tabla de métricas
metricas_control_d <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_control_d),
    4
  ),
  Precision = round(
    as.numeric(precision_control_d),
    4
  )
)

accuracy_control_d
sensibilidad_media_control_d
metricas_control_d


# =========================================================
# 17. OBJETOS PRINCIPALES DEL MODELO DE CONTROL
# =========================================================

tabla_control_d

tabla_control_derrota_d
tabla_control_victoria_d

test_global_control_d

AIC_control_d
logLik_control_d

mc_control_d
accuracy_control_d
sensibilidad_media_control_d
metricas_control_d

#depuro tiros libres concedidos

modelo_multinom_control_d1 <- multinom(
  resultado_partido_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    zona2_pases_pct +
    zona3_pases_pct +
    corners_local +
    corners_concedidos_local +
    tiros_libres_local +
    duelos_ganados_local,
  data = d,
  trace = FALSE
)

# Resumen del modelo
summary(modelo_multinom_control_d1)


# =========================================================
# 6. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_control_d1 <- extraer_resultados_multinom(
  modelo_multinom_control_d1
)

# Tabla completa
tabla_control_d1

#sigo depurando con tiros libres local

modelo_multinom_control_d2 <- multinom(
  resultado_partido_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    zona2_pases_pct +
    zona3_pases_pct +
    corners_local +
    corners_concedidos_local +
    duelos_ganados_local,
  data = d,
  trace = FALSE
)

# Resumen del modelo
summary(modelo_multinom_control_d2)


# =========================================================
# 6. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_control_d2 <- extraer_resultados_multinom(
  modelo_multinom_control_d2
)

# Tabla completa
tabla_control_d2


library(car)

modelo_control_auxiliar <- lm(
  posesion_local ~
    pases_local +
    pases_en_contra_local +
    zona2_pases_pct +
    zona3_pases_pct +
    corners_local +
    corners_concedidos_local +
    duelos_ganados_local,
  data = d
)

vif(modelo_control_auxiliar)

variables_control_cor <- d %>%
  select(
    posesion_local,
    pases_local,
    pases_en_contra_local,
    zona2_pases_pct,
    zona3_pases_pct,
    corners_local,
    corners_concedidos_local,
    duelos_ganados_local
  )

round(
  cor(
    variables_control_cor,
    use = "complete.obs"
  ),
  3
)

vif(modelo_multinom_control_d2)

####################################################
# =========================================================
# FUNCIÓN PARA CALCULAR VIF MANUALMENTE
# =========================================================

calcular_vif_manual <- function(datos, variables) {
  
  resultados <- data.frame(
    Variable = variables,
    VIF = NA_real_
  )
  
  for (i in seq_along(variables)) {
    
    variable_respuesta <- variables[i]
    variables_explicativas <- variables[-i]
    
    formula_auxiliar <- as.formula(
      paste(
        variable_respuesta,
        "~",
        paste(variables_explicativas, collapse = " + ")
      )
    )
    
    modelo_auxiliar <- lm(
      formula_auxiliar,
      data = datos
    )
    
    r2 <- summary(modelo_auxiliar)$r.squared
    
    resultados$VIF[i] <- 1 / (1 - r2)
  }
  
  resultados$VIF <- round(resultados$VIF, 3)
  
  resultados <- resultados %>%
    arrange(desc(VIF))
  
  return(resultados)
}

variables_vif_control <- c(
  "posesion_local",
  "pases_local",
  "pases_en_contra_local",
  "zona2_pases_pct",
  "zona3_pases_pct",
  "corners_local",
  "corners_concedidos_local",
  "duelos_ganados_local"
)

vif_control_correcto <- calcular_vif_manual(
  datos = d,
  variables = variables_vif_control
)

vif_control_correcto

#elimino los pases local y rivales

modelo_control_solo_posesion_d <- update(
  modelo_multinom_control_d2,
  . ~ . - pases_local - pases_en_contra_local
)

summary(modelo_control_solo_posesion_d)

tabla_control_solo_posesion_d <- extraer_resultados_multinom(
  modelo_control_solo_posesion_d
)

tabla_control_solo_posesion_d

modelo_control_solo_pases_d <- update(
  modelo_multinom_control_d2,
  . ~ . - posesion_local
)

summary(modelo_control_solo_pases_d)

tabla_control_solo_pases_d <- extraer_resultados_multinom(
  modelo_control_solo_pases_d
)

tabla_control_solo_pases_d


AIC(
  modelo_multinom_control_d2,
  modelo_control_solo_posesion_d,
  modelo_control_solo_pases_d
)
anova(  modelo_control_solo_posesion_d,  modelo_multinom_control_d2,  test = "Chisq")
anova(  modelo_control_solo_pases_d,  modelo_multinom_control_d2,  test = "Chisq")

########################################################

d$cuota_pases_local <- d$pases_local / (
  d$pases_local + d$pases_en_contra_local
)

# Comprobar distribución
summary(d$cuota_pases_local)

# Evitar valores no válidos, por seguridad
sum(is.na(d$cuota_pases_local))
sum(is.infinite(d$cuota_pases_local))


# =========================================================
# MODELO CON CUOTA DE PASES
# =========================================================

modelo_control_cuota_pases_d <- multinom(
  resultado_partido_local ~
    cuota_pases_local +
    zona2_pases_pct +
    zona3_pases_pct +
    corners_local +
    corners_concedidos_local +
    duelos_ganados_local,
  data = d,
  trace = FALSE
)

summary(modelo_control_cuota_pases_d)

tabla_control_cuota_pases_d <- extraer_resultados_multinom(
  modelo_control_cuota_pases_d
)

tabla_control_cuota_pases_d

variables_vif_cuota_pases <- c(
  "cuota_pases_local",
  "zona2_pases_pct",
  "zona3_pases_pct",
  "corners_local",
  "corners_concedidos_local",
  "duelos_ganados_local"
)

vif_cuota_pases <- calcular_vif_manual(
  datos = d,
  variables = variables_vif_cuota_pases
)

vif_cuota_pases

##########################################################3
d$pases_por_posesion <- d$pases_local / d$posesion_local

d$posesion_local_pct <- d$posesion_local * 100

d$pases_por_punto_posesion <- d$pases_local / d$posesion_local_pct

modelo_pases_por_punto_posesion <- multinom(
  resultado_partido_local ~
    pases_por_punto_posesion +
    zona2_pases_pct +
    zona3_pases_pct +
    corners_local +
    corners_concedidos_local +
    duelos_ganados_local,
  data = d,
  trace = FALSE
)

summary(modelo_pases_por_punto_posesion)

tabla_pases_por_punto_posesion <- extraer_resultados_multinom(
  modelo_pases_por_punto_posesion
)

tabla_pases_por_punto_posesion


##############################################
#finalmente uso el completo depurado
modelo_multinom_control_d2 <- multinom(
  resultado_partido_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    zona2_pases_pct +
    zona3_pases_pct +
    corners_local +
    corners_concedidos_local +
    duelos_ganados_local,
  data = d,
  trace = FALSE
)

# Resumen del modelo
summary(modelo_multinom_control_d2)


# =========================================================
# 6. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_control_d2 <- extraer_resultados_multinom(
  modelo_multinom_control_d2
)

# Tabla completa
tabla_control_d2

  #=========================================================
  # BLOQUE FÍSICO MULTINOMIAL - DATOS d
  # Categoría de referencia: Empate
  # =========================================================


# =========================================================
# 1. PREPARAR LA VARIABLE RESPUESTA
# =========================================================

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

# Comprobar categorías
levels(d$resultado_partido_local)

# Distribución del resultado
table(d$resultado_partido_local)

prop.table(
  table(d$resultado_partido_local)
)

#=========================================================
# BLOQUE FÍSICO MULTINOMIAL - DATOS d
# Categoría de referencia: Empate
# =========================================================


# =========================================================
# 1. PREPARAR LA VARIABLE RESPUESTA
# =========================================================

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

# Comprobar categorías
levels(d$resultado_partido_local)

# Distribución del resultado
table(d$resultado_partido_local)

prop.table(
  table(d$resultado_partido_local)
)



# =========================================================
# 4. TRATAR VALORES AUSENTES ESTRUCTURALES
# =========================================================

# Si no hubo roja, el minuto de la roja se codifica como 0
d$min_roja_local[
  is.na(d$min_roja_local)
] <- 0

# Si no hubo ventaja numérica, el minuto se codifica como 0
d$min_ventaja_numerica_local[
  is.na(d$min_ventaja_numerica_local)
] <- 0



# =========================================================
# 5. MODELO FÍSICO MULTINOMIAL COMPLETO
# =========================================================

modelo_multinom_fisico_d <- multinom(
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
  data = d,
  trace = FALSE
)

summary(modelo_multinom_fisico_d)


# =========================================================
# 6. TABLA DE COEFICIENTES, OR Y P-VALORES
# =========================================================

tabla_fisico_d <- extraer_resultados_multinom(
  modelo_multinom_fisico_d
)

tabla_fisico_d


# =========================================================
# 7. SIGNIFICATIVAS AL 5%
# =========================================================

tabla_fisico_d %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 8. SIGNIFICATIVAS O MARGINALES AL 10%
# =========================================================

tabla_fisico_d %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 9. DERROTA FRENTE A EMPATE
# =========================================================

tabla_fisico_derrota_d <- tabla_fisico_d %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_fisico_derrota_d


# =========================================================
# 10. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_fisico_victoria_d <- tabla_fisico_d %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_fisico_victoria_d


# =========================================================
# 11. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_fisico_d <- test_lr_multinom(
  modelo_multinom_fisico_d,
  d
)

test_global_fisico_d


# =========================================================
# 12. BONDAD DE AJUSTE
# =========================================================

AIC_fisico_d <- AIC(
  modelo_multinom_fisico_d
)

logLik_fisico_d <- logLik(
  modelo_multinom_fisico_d
)

AIC_fisico_d
logLik_fisico_d


# =========================================================
# 13. PROBABILIDADES ESTIMADAS
# =========================================================

prob_fisico_d <- predict(
  modelo_multinom_fisico_d,
  newdata = d,
  type = "probs"
)

# Primeras probabilidades
head(prob_fisico_d)

# En porcentaje
round(
  head(prob_fisico_d * 100),
  2
)

# Comprobar que suman 1
rowSums(prob_fisico_d)[1:10]


# =========================================================
# 14. PREDICCIÓN DE CLASE
# =========================================================

pred_fisico_d <- predict(
  modelo_multinom_fisico_d,
  newdata = d,
  type = "class"
)

head(pred_fisico_d)


# =========================================================
# 15. MATRIZ DE CONFUSIÓN
# =========================================================

niveles_resultado <- levels(
  d$resultado_partido_local
)

real_fisico_d <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_fisico_d <- factor(
  pred_fisico_d,
  levels = niveles_resultado
)

mc_fisico_d <- table(
  Real = real_fisico_d,
  Predicho = pred_fisico_d
)

mc_fisico_d
addmargins(mc_fisico_d)


# =========================================================
# 16. MÉTRICAS
# =========================================================

accuracy_fisico_d <-
  sum(diag(mc_fisico_d)) /
  sum(mc_fisico_d)

sensibilidad_fisico_d <-
  diag(mc_fisico_d) /
  rowSums(mc_fisico_d)

precision_fisico_d <-
  diag(mc_fisico_d) /
  colSums(mc_fisico_d)

sensibilidad_fisico_d[
  is.nan(sensibilidad_fisico_d) |
    is.infinite(sensibilidad_fisico_d)
] <- NA

precision_fisico_d[
  is.nan(precision_fisico_d) |
    is.infinite(precision_fisico_d)
] <- NA

sensibilidad_media_fisico_d <-
  mean(
    sensibilidad_fisico_d,
    na.rm = TRUE
  )

metricas_fisico_d <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_fisico_d),
    4
  ),
  Precision = round(
    as.numeric(precision_fisico_d),
    4
  )
)

accuracy_fisico_d
sensibilidad_media_fisico_d
metricas_fisico_d



###########################################################
#vamos con contexto de partido


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


# =========================================================
# 5. SIGNIFICATIVAS AL 5%
# =========================================================

tabla_contexto_d %>%
  filter(P_valor < 0.05) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 6. SIGNIFICATIVAS O MARGINALES AL 10%
# =========================================================

tabla_contexto_d %>%
  filter(P_valor < 0.10) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 7. DERROTA FRENTE A EMPATE
# =========================================================

tabla_contexto_derrota_d <- tabla_contexto_d %>%
  filter(Resultado == "Derrota") %>%
  arrange(P_valor)

tabla_contexto_derrota_d


# =========================================================
# 8. VICTORIA FRENTE A EMPATE
# =========================================================

tabla_contexto_victoria_d <- tabla_contexto_d %>%
  filter(Resultado == "Victoria") %>%
  arrange(P_valor)

tabla_contexto_victoria_d


# =========================================================
# 9. TEST GLOBAL DE VARIABLES
# =========================================================

test_global_contexto_d <- test_lr_multinom(
  modelo_multinom_contexto_d,
  d
)

test_global_contexto_d


# =========================================================
# 10. BONDAD DE AJUSTE
# =========================================================

AIC_contexto_d <- AIC(
  modelo_multinom_contexto_d
)

logLik_contexto_d <- logLik(
  modelo_multinom_contexto_d
)

AIC_contexto_d
logLik_contexto_d


# =========================================================
# 11. PROBABILIDADES ESTIMADAS
# =========================================================

prob_contexto_d <- predict(
  modelo_multinom_contexto_d,
  newdata = d,
  type = "probs"
)

head(prob_contexto_d)

round(
  head(prob_contexto_d * 100),
  2
)

rowSums(prob_contexto_d)[1:10]


# =========================================================
# 12. PREDICCIÓN Y MATRIZ DE CONFUSIÓN
# =========================================================

pred_contexto_d <- predict(
  modelo_multinom_contexto_d,
  newdata = d,
  type = "class"
)

niveles_resultado <- levels(
  d$resultado_partido_local
)

real_contexto_d <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_contexto_d <- factor(
  pred_contexto_d,
  levels = niveles_resultado
)

mc_contexto_d <- table(
  Real = real_contexto_d,
  Predicho = pred_contexto_d
)

mc_contexto_d
addmargins(mc_contexto_d)


# =========================================================
# 13. MÉTRICAS
# =========================================================

accuracy_contexto_d <-
  sum(diag(mc_contexto_d)) /
  sum(mc_contexto_d)

sensibilidad_contexto_d <-
  diag(mc_contexto_d) /
  rowSums(mc_contexto_d)

precision_contexto_d <-
  diag(mc_contexto_d) /
  colSums(mc_contexto_d)

sensibilidad_contexto_d[
  is.nan(sensibilidad_contexto_d) |
    is.infinite(sensibilidad_contexto_d)
] <- NA

precision_contexto_d[
  is.nan(precision_contexto_d) |
    is.infinite(precision_contexto_d)
] <- NA

sensibilidad_media_contexto_d <-
  mean(
    sensibilidad_contexto_d,
    na.rm = TRUE
  )

metricas_contexto_d <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_contexto_d),
    4
  ),
  Precision = round(
    as.numeric(precision_contexto_d),
    4
  )
)

accuracy_contexto_d
sensibilidad_media_contexto_d
metricas_contexto_d
