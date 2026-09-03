# ==============================================================================
# Title: General multinomial model development and selection
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops, simplifies and evaluates a general multinomial logistic
# regression model for match outcomes during the 2024/2025 season. The
# dependent variable distinguishes between a home-team victory, a draw and a
# home-team defeat, with the draw used as the reference category.
#
# The initial general model combines predictors selected from the physical,
# contextual, defensive and offensive analytical blocks. These predictors
# describe rest conditions, disciplinary events, previous league positions,
# recent form, defensive exposure, attacking production, recoveries and
# clearances.
#
# The script extracts coefficient estimates, odds ratios and bilateral Wald
# p-values for the complete general model. Results are presented separately
# for home defeat versus draw and home victory versus draw, with statistical
# evidence identified at the 5% and 10% levels.
#
# Global likelihood-ratio tests are used to assess the overall contribution of
# each explanatory variable. Multicollinearity among the predictors is also
# examined through manually calculated variance inflation factors.
#
# The general specification is progressively simplified by removing variables
# with limited global contribution. The successive candidate models document
# the manual selection process until a reduced final specification is obtained
# in which the retained predictors provide relevant explanatory information.
#
# The final model includes home red cards, the absence of previous rest
# information, the previous league positions of both teams, big chances
# conceded, shots on target conceded, corners conceded, long passes conceded,
# crosses conceded, home clearances, home shots on target, home big chances,
# recoveries conceded and opposition clearances.
#
# Model fit is assessed using pseudo-R-squared statistics and global
# likelihood-ratio tests. Predicted probabilities are calculated for the three
# possible match outcomes and converted into predicted outcome classes.
#
# Classification performance is evaluated through a confusion matrix, overall
# accuracy, class-specific sensitivity, class-specific precision and mean
# sensitivity across the three outcome categories.
#
# Dataset:
#   d, corresponding to the 2024/2025 season.
#
# Dependent variable:
#   resultado_partido_
# =========================================================
# MODELO GENERAL MULTINOMIAL - DATOS d
# Categoría de referencia: Empate
# =========================================================


modelo_general <- multinom(
  resultado_partido_local ~
    #fisico
    diff_descanso +
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    #defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    intercepciones_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    pases_U3_local +
    centros_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
    ,
  data = d,
  trace = FALSE
)

summary(modelo_general)


# =========================================================
# 2. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_general <- extraer_resultados_multinom(
  modelo_general
)

tabla_general


# =========================================================
# 3. RESULTADOS SIGNIFICATIVOS AL 5%
# =========================================================

tabla_general %>%
  filter(
    Variable != "(Intercept)",
    P_valor < 0.05
  ) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 4. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_general %>%
  filter(
    Variable != "(Intercept)",
    P_valor < 0.10
  ) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 5. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_general_derrota <- tabla_general %>%
  filter(
    Resultado == "Derrota",
    Variable != "(Intercept)"
  ) %>%
  arrange(P_valor)

tabla_general_derrota


# =========================================================
# 6. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_general_victoria <- tabla_general %>%
  filter(
    Resultado == "Victoria",
    Variable != "(Intercept)"
  ) %>%
  arrange(P_valor)

tabla_general_victoria


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general <- test_lr_multinom(
  modelo_general,
  d
)

test_global_general


# Variables globalmente significativas al 5%
test_global_general %>%
  filter(p_value < 0.05)


# Variables significativas o marginales al 10%
test_global_general %>%
  filter(p_value < 0.10)


# Posibles candidatas a depuración
test_global_general %>%
  filter(p_value >= 0.10) %>%
  arrange(desc(p_value))


# =========================================================
# 11. PSEUDO R CUADRADO
# =========================================================

pseudo_R2_general <- pR2(
  modelo_general
)

pseudo_R2_general


# =========================================================
# 12. PROBABILIDADES ESTIMADAS
# =========================================================

prob_general <- predict(
  modelo_general,
  newdata = d,
  type = "probs"
)

# Primeros seis partidos
head(prob_general)

# En porcentaje
round(
  head(prob_general * 100),
  2
)

# Comprobar número de filas
nrow(prob_general)

# Comprobar que las probabilidades suman 1
rowSums(prob_general)[1:10]

# Para visualizar todas:
# View(as.data.frame(prob_general))


# =========================================================
# 13. PREDICCIÓN DE LA CATEGORÍA
# =========================================================

pred_general <- predict(
  modelo_general,
  newdata = d,
  type = "class"
)

head(pred_general)


# =========================================================
# 14. PREPARAR NIVELES
# =========================================================

niveles_resultado <- levels(
  d$resultado_partido_local
)

real_general <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_general <- factor(
  pred_general,
  levels = niveles_resultado
)


# =========================================================
# 15. MATRIZ DE CONFUSIÓN
# =========================================================

mc_general <- table(
  Real = real_general,
  Predicho = pred_general
)

mc_general

# Matriz con totales
addmargins(mc_general)


# =========================================================
# 16. ACCURACY TOTAL
# =========================================================

accuracy_general <-
  sum(diag(mc_general)) /
  sum(mc_general)

accuracy_general


# =========================================================
# 17. SENSIBILIDAD POR CATEGORÍA
# =========================================================

# De todos los casos reales de una categoría,
# qué proporción identifica correctamente el modelo

sensibilidad_general <-
  diag(mc_general) /
  rowSums(mc_general)

sensibilidad_general


# =========================================================
# 18. PRECISIÓN POR CATEGORÍA
# =========================================================

# De todas las predicciones de una categoría,
# qué proporción pertenecía realmente a esa categoría

precision_general <-
  diag(mc_general) /
  colSums(mc_general)

precision_general




# =========================================================
# 20. SENSIBILIDAD MEDIA
# =========================================================

sensibilidad_media_general <- mean(
  sensibilidad_general,
  na.rm = TRUE
)

sensibilidad_media_general

# =========================================================
# 26. LISTA DE VARIABLES PARA EL VIF
# =========================================================

variables_general_d <- c(
  
  # Bloque físico
  "diff_descanso",
  "duelos_ganados_local",
  "rojas_local",
  
  # Bloque de contexto
  "no_descanso_previo",
  "pos_previa_local",
  "pos_previa_visitante",
  "forma_local_5",
  "forma_visitante_5",
  
  # Bloque defensivo
  "xGA_local",
  "big_chances_concedidas_local",
  "tiros_puerta_concedidos_local",
  "corners_concedidos_local",
  "pases_U3_concedidos_local",
  "pases_largos_concedidos_local",
  "centros_concedidos_local",
  "intercepciones_local",
  "despejes_local",
  
  # Bloque ofensivo
  "tiros_puerta_local",
  "big_chances_local",
  "pases_U3_local",
  "centros_local",
  "recuperaciones_concedidas_local",
  "despejes_concedidos_local"
)


# =========================================================
# 27. CALCULAR VIF DEL MODELO GENERAL
# =========================================================

vif_general <- calcular_vif_manual(
  datos = d,
  variables = variables_general_d
)

vif_general
#sale una baja colienalidad

##############################################################
#empiezo a depurar por pases_u3_local

modelo_general1 <- multinom(
  resultado_partido_local ~
    #fisico
    diff_descanso +
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    #defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_U3_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    intercepciones_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general1)


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general1 <- test_lr_multinom(
  modelo_general1,
  d
)

test_global_general1

#depuro pases_U3_concedidos_local  

modelo_general2 <- multinom(
  resultado_partido_local ~
    #fisico
    diff_descanso +
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    #defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    intercepciones_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general2)


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general2 <- test_lr_multinom(
  modelo_general2,
  d
)

test_global_general2

#depuro diff_descanso  

modelo_general3 <- multinom(
  resultado_partido_local ~
    #fisico
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    #defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    intercepciones_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general3)




# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general3 <- test_lr_multinom(
  modelo_general3,
  d
)

test_global_general3

#depuro intercepciones local

modelo_general4 <- multinom(
  resultado_partido_local ~
    #fisico
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    #defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general4)



# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general4 <- test_lr_multinom(
  modelo_general4,
  d
)

test_global_general4

#depuro centros local


modelo_general5 <- multinom(
  resultado_partido_local ~
    #fisico
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    #defensivo
    xGA_local +
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general5)



# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general5 <- test_lr_multinom(
  modelo_general5,
  d
)

test_global_general5

#depuro xGA

modelo_general6 <- multinom(
  resultado_partido_local ~
    #fisico
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5 +
    #defensivo
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general6)


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general6 <- test_lr_multinom(
  modelo_general6,
  d
)

test_global_general6

#depuro forma visitante

modelo_general7 <- multinom(
  resultado_partido_local ~
    #fisico
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    #defensivo
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general7)



# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general7 <- test_lr_multinom(
  modelo_general7,
  d
)

test_global_general7

#depuro forma local 5

modelo_general8<- multinom(
  resultado_partido_local ~
    #fisico
    duelos_ganados_local +
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    #defensivo
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general8)



# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general8 <- test_lr_multinom(
  modelo_general8,
  d
)

test_global_general8

#depuro duelos ganados local

modelo_general9<- multinom(
  resultado_partido_local ~
    #fisico
    rojas_local +
    #contexto
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    #defensivo
    big_chances_concedidas_local +
    tiros_puerta_concedidos_local +
    corners_concedidos_local +
    pases_largos_concedidos_local +
    centros_concedidos_local +
    despejes_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local 
  ,
  data = d,
  trace = FALSE
)

summary(modelo_general9)



# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general9 <- test_lr_multinom(
  modelo_general9,
  d
)

test_global_general9

#ya con todas las variables significativas, saco el resto del codigo

# =========================================================
# MODELO GENERAL MULTINOMIAL DEPURADO - DATOS d
# Categoría de referencia: Empate
# =========================================================
modelo_general_depurado <- modelo_general9

# =========================================================
# 2. TABLA DE COEFICIENTES, ODDS RATIOS Y P-VALORES
# =========================================================

tabla_general_depurado <- extraer_resultados_multinom(
  modelo_general_depurado
)

tabla_general_depurado


# =========================================================
# 3. RESULTADOS SIGNIFICATIVOS AL 5%
# =========================================================

tabla_general_depurado %>%
  filter(
    Variable != "(Intercept)",
    P_valor < 0.05
  ) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 4. RESULTADOS SIGNIFICATIVOS O MARGINALES AL 10%
# =========================================================

tabla_general_depurado %>%
  filter(
    Variable != "(Intercept)",
    P_valor < 0.10
  ) %>%
  arrange(Resultado, P_valor)


# =========================================================
# 5. RESULTADOS: DERROTA FRENTE A EMPATE
# =========================================================

tabla_general_depurado_derrota <- tabla_general_depurado %>%
  filter(
    Resultado == "Derrota",
    Variable != "(Intercept)"
  ) %>%
  arrange(P_valor)

tabla_general_depurado_derrota


# =========================================================
# 6. RESULTADOS: VICTORIA FRENTE A EMPATE
# =========================================================

tabla_general_depurado_victoria <- tabla_general_depurado %>%
  filter(
    Resultado == "Victoria",
    Variable != "(Intercept)"
  ) %>%
  arrange(P_valor)

tabla_general_depurado_victoria


# =========================================================
# 7. TEST GLOBAL DE CADA VARIABLE
# =========================================================

test_global_general_depurado <- test_lr_multinom(
  modelo_general_depurado,
  d
)

test_global_general_depurado


# =========================================================
# 10. PSEUDO R CUADRADO
# =========================================================

pseudo_R2_general_depurado <- pR2(
  modelo_general_depurado
)

pseudo_R2_general_depurado


# =========================================================
# 11. PROBABILIDADES ESTIMADAS
# =========================================================

prob_general_depurado <- predict(
  modelo_general_depurado,
  newdata = d,
  type = "probs"
)

# Primeros seis partidos
head(prob_general_depurado)

# Primeros seis partidos en porcentaje
round(
  head(prob_general_depurado * 100),
  2
)


# =========================================================
# 12. PREDICCIÓN DE LA CATEGORÍA
# =========================================================

pred_general_depurado <- predict(
  modelo_general_depurado,
  newdata = d,
  type = "class"
)

head(pred_general_depurado)


# =========================================================
# 13. PREPARAR NIVELES
# =========================================================

niveles_resultado <- levels(
  d$resultado_partido_local
)

real_general_depurado <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_general_depurado <- factor(
  pred_general_depurado,
  levels = niveles_resultado
)


# =========================================================
# 14. MATRIZ DE CONFUSIÓN
# =========================================================

mc_general_depurado <- table(
  Real = real_general_depurado,
  Predicho = pred_general_depurado
)

mc_general_depurado

# Matriz con totales
addmargins(mc_general_depurado)


# =========================================================
# 15. ACCURACY TOTAL
# =========================================================

accuracy_general_depurado <-
  sum(diag(mc_general_depurado)) /
  sum(mc_general_depurado)

accuracy_general_depurado


# =========================================================
# 16. SENSIBILIDAD POR CATEGORÍA
# =========================================================

# De todos los partidos que realmente pertenecían a
# cada categoría, qué proporción identifica el modelo.

sensibilidad_general_depurado <-
  diag(mc_general_depurado) /
  rowSums(mc_general_depurado)

sensibilidad_general_depurado


# =========================================================
# 17. PRECISIÓN POR CATEGORÍA
# =========================================================

# De todos los partidos predichos como cada categoría,
# qué proporción pertenecía realmente a dicha categoría.

precision_general_depurado <-
  diag(mc_general_depurado) /
  colSums(mc_general_depurado)

precision_general_depurado



# =========================================================
# 19. SENSIBILIDAD MEDIA
# =========================================================

sensibilidad_media_general_depurado <- mean(
  sensibilidad_general_depurado,
  na.rm = TRUE
)

sensibilidad_media_general_depurado

