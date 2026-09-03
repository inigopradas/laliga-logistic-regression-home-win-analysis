# ==============================================================================
# Title: Binomial block models and general model for home-team victory
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops and compares a set of binomial logistic regression
# models designed to explain the probability of a home-team victory during
# the 2024/2025 season.
#
# The script first imports the match-level dataset and prepares the variables
# required for the statistical analysis. This preparation includes inspecting
# the dataset, assigning the appropriate variable classes, cleaning the rest
# and recent-form variables, constructing an indicator for matches without
# previous rest information, calculating the difference in rest days between
# the home and visiting teams, and defining previous league positions both as
# categorical and numerical variables.
#
# An initial binomial logistic regression, identified as Model A, is estimated
# using attacking, defensive, contextual, rest and recent-form variables.
# Model A is examined through coefficient estimates, odds ratios, 95%
# confidence intervals, pseudo-R-squared statistics, predicted probabilities,
# a confusion matrix and classification accuracy.
#
# The main part of the script constructs separate logistic regression models
# for five analytical dimensions:
#
#   1. Defensive performance
#   2. Offensive performance
#   3. Physical and disciplinary performance
#   4. Match control
#   5. Competitive context
#
# The defensive, offensive and physical specifications are progressively
# reduced by manually removing predictors with comparatively high p-values.
# The successive models are retained to document the variable-selection
# process and to examine changes in model fit, residual deviance, statistical
# significance and the Akaike information criterion.
#
# The script also evaluates an interaction between home shots on target and
# home big chances. The interaction model is compared with the selected
# offensive model through a likelihood-ratio test.
#
# Alternative contextual specifications are estimated by treating previous
# league positions either as categorical factors or as numerical variables.
# These specifications are compared using the AIC, residual deviance, number
# of estimated parameters and a likelihood-ratio test. The numerical
# specification is retained as the final contextual model because it provides
# a more parsimonious representation of previous league position.
#
# The selected variables from the individual analytical blocks are combined
# in a general binomial logistic regression. This model is progressively
# simplified to obtain the final general specification, identified as
# modelo_alfa. The final model is evaluated using odds ratios, confidence
# intervals, pseudo-R-squared, predicted probabilities, a confusion matrix
# and in-sample classification accuracy.
#
# The script standardises the names of the selected offensive, defensive,
# physical, match-control, contextual and general models. It then defines
# reusable functions to extract model-selection statistics and calculate
# classification metrics, including accuracy, sensitivity, specificity and
# precision.
#
# Finally, the script produces figures comparing the alternative coding of
# previous league positions, the evolution of the offensive and defensive
# selection processes, model fit and classification performance, coefficient
# estimates for the block models, coefficient estimates for the final general
# model, and confusion matrices for the principal models.
#
# Input:
#   variables_Estudio (9).xlsx
#
# Dependent variable:
#   win_local
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory, including draws and away-team victories
#
# Statistical framework:
#   Binomial logistic regression estimated with glm() and family = binomial.
#
# Variable-selection procedure:
#   Manual backward elimination based primarily on coefficient p-values,
#   complemented by comparisons of AIC, residual deviance, parsimony and
#   substantive interpretability.
#
# Final block models:
#   modelo_ofensivo_final_d = modelo_ofensivo23
#   modelo_defensivo_final_d = modelo_defensivo23
#   modelo_fisico_final_d = modelo_fisico10
#   modelo_control_final_d = modelo_control_partido
#   modelo_contexto_final_d = modelo_contexto_numerico
#
# Final general model:
#   modelo_general_final_d = modelo_alfa
#
# Main outputs:
#   Model summaries, odds-ratio tables, confidence intervals, pseudo-R-squared
#   statistics, AIC and residual-deviance comparisons, classification metrics,
#   coefficient plots, model-comparison figures and confusion-matrix figures.
#
# Important methodological note:
#   Classification metrics are calculated using the same observations employed
#   to estimate the models. These metrics therefore describe in-sample fit and
#   should not be interpreted as out-of-sample predictive performance.
# ==============================================================================

library(readxl)   # para leer Excel
library(dplyr)    # para manipular datos
d <- read_excel("variables_Estudio (9).xlsx")
str(d)
head(d)
d$win_local = factor(d$win_local)
d$temporada = factor(d$temporada)
d$formacion_local = factor(d$formacion_local)
d$formacion_visit = factor(d$formacion_visit)
d$pos_previa_local = factor(d$pos_previa_local)
d$pos_previa_visitante = factor(d$pos_previa_visitante)

d$equipo_local <- as.character(d$equipo_local)
d$equipo_visitante <- as.character(d$equipo_visitante)

class(d$equipo_local)

# Indicador de que el descanso no aplica (jornada 1)
d$no_descanso_previo <- ifelse(is.na(d$descanso_local), 1, 0)

# Imputación técnica(convierto los NA que hacen char a numeric para hacer la diferencia)
d$descanso_local[is.na(d$descanso_local)] <- 0
d$descanso_visit[is.na(d$descanso_visit)] <- 0

d$descanso_local[d$descanso_local %in% c("", "-", "NA")] <- NA
d$descanso_visit[d$descanso_visit %in% c("", "-", "NA")] <- NA
d$descanso_local <- as.numeric(d$descanso_local)
d$descanso_visit <- as.numeric(d$descanso_visit)

# Diferencia de descanso
d$diff_descanso <- d$descanso_local - d$descanso_visit


#Forma (dejo las primeras 5 jornadas como 0)
d$forma_local_5[is.na(d$forma_local_5)] <- 0
d$forma_visitante_5[is.na(d$forma_visitante_5)] <- 0
#Un mayor valor de forma reciente incrementa la probabilidad de victoria

# --- DESCANSO: LIMPIEZA CORRECTA ---
# 1. Sustituimos texto incorrecto por NA
d$descanso_local[d$descanso_local %in% c("", "-", "NA")]   <- NA
d$descanso_visit[d$descanso_visit %in% c("", "-", "NA")]   <- NA

# 2. Convertimos a número
d$descanso_local <- as.numeric(d$descanso_local)
d$descanso_visit <- as.numeric(d$descanso_visit)

# 3. Indicador de jornada sin descanso previo
d$no_descanso_previo <- ifelse(is.na(d$descanso_local), 1, 0)

# 4. Imputamos NA = 0 SOLO para poder calcular diff
d$descanso_local[is.na(d$descanso_local)] <- 0
d$descanso_visit[is.na(d$descanso_visit)] <- 0

# 5. Diferencia de descanso
d$diff_descanso <- d$descanso_local - d$descanso_visit

# --- FORMA ---
d$forma_local_5[is.na(d$forma_local_5)]       <- 0
d$forma_visitante_5[is.na(d$forma_visitante_5)] <- 0

d$forma_local_5[d$forma_local_5 == "NA"] <- NA
d$forma_visitante_5[d$forma_visitante_5 == "NA"] <- NA


d$forma_local_5      <- as.numeric(d$forma_local_5)
d$forma_visitante_5  <- as.numeric(d$forma_visitante_5)


d$forma_local_5[is.na(d$forma_local_5)] <- 0
d$forma_visitante_5[is.na(d$forma_visitante_5)] <- 0

d$pos_previa_local <- factor(
  as.numeric(as.character(d$pos_previa_local)),
  levels = 1:20
)

d$pos_previa_visitante <- factor(
  as.numeric(as.character(d$pos_previa_visitante)),
  levels = 1:20
)

levels(d$pos_previa_local)
levels(d$pos_previa_visitante)

modelo_A <- glm(
  win_local ~
    tiros_puerta_local +
    xG_local +
    xGA_local +
    tiros_puerta_concedidos_local +
    posesion_local +
    diferencia_puntos_local +
    diff_descanso +
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d,
  family = binomial
)

summary(modelo_A)

library(broom)

# Tabla de Odds Ratios con IC 95%
OR_A <- tidy(modelo_A, conf.int = TRUE, exponentiate = TRUE)

# Mostramos la tabla ordenada por importancia (tamaño del OR)
OR_A[order(abs(OR_A$estimate), decreasing = TRUE), ]


#Grafico de coeficientes (log-odds)
library(ggplot2)
library(broom)

coef_df <- tidy(modelo_A, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]

ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Coeficientes del Modelo Logístico – Modelo A",
    x = "Coeficiente (log-odds)",
    y = "Variables"
  ) +
  theme_minimal(base_size = 13)


library(pscl)
pR2(modelo_A)

# Probabilidades predichas
pred_prob_A <- predict(modelo_A, type = "response")

# Clasificación (0/1)
pred_A <- ifelse(pred_prob_A >= 0.5, 1, 0)

# Matriz de confusión
table(real = d$win_local, pred = pred_A)

# Accuracy
mean(pred_A == d$win_local)


install.packages("pscl")

library(pscl)
pR2(modelo_A)

# Probabilidades predichas
pred_prob_A <- predict(modelo_A, type = "response")

# Clasificación
pred_A <- ifelse(pred_prob_A >= 0.5, 1, 0)

# Accuracy
accuracy_A <- mean(pred_A == d$win_local)
accuracy_A

#MODELO B: Por bloques
modelo_defensivo_completo <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    amarillas_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    toques_area_penal_concedidos_local +
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_U3_efectivos_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)

summary(modelo_defensivo_completo)

#ahora elimino la menos significativa, que es la de faltas U3 por su pvalor = 0.981113

modelo_defensivo1 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    amarillas_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_U3_efectivos_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)

summary(modelo_defensivo1)

#ahora la siguiente que es centros rematados concedidos por pvalor = 0.875731

modelo_defensivo2 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    amarillas_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_U3_efectivos_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)

summary(modelo_defensivo2)

#ahora la siguiente: pases U3 efectivos concedidos

modelo_defensivo3 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    amarillas_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)

summary(modelo_defensivo3)

#ahora es tiros libres con pvalor = 0.784290

modelo_defensivo4 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    amarillas_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local +  
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)

summary(modelo_defensivo4)

#ahora es faltas con pvalor = 0.700202

modelo_defensivo5 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    amarillas_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local +  
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo5)

#ahora es amarillas con pvalor = 0.62307

modelo_defensivo6 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local +  
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo6)

#ahora es entradas ganadas con pvalor = 0.61974

modelo_defensivo7 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local +  
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo7)

#ahora min roja local pvalor = 0.52482

modelo_defensivo8 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local +  
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo8)

#min ventaja numerica pvalor = 0.482534

modelo_defensivo9 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    corners_concedidos_local + 
    entradas_local +  
    toques_area_penal_concedidos_local +
    pases_al_U3_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo9)

#pases al U3 concedidos pvalor = 0.489355 

modelo_defensivo10 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    corners_concedidos_local + 
    entradas_local +  
    toques_area_penal_concedidos_local +
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo10)

#ahora toques area penal concedidos pvalor = 0.52738

modelo_defensivo11 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    corners_concedidos_local + 
    entradas_local +  
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo11)

# tiros pvalor = 0.52040

modelo_defensivo12 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    corners_concedidos_local + 
    entradas_local +  
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo12)

#regates efectivos pvalor = 0.50753

modelo_defensivo13 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    corners_concedidos_local + 
    entradas_local +  
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo13)

# entradas pvalor = 0.56289

modelo_defensivo14 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo14)

# regates pvalor = 0.67015

modelo_defensivo15 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo15)

#pases en contra pvalor = 0.38457

modelo_defensivo16 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo16)

#tiros puerta pvalor = 0.23200

modelo_defensivo17 <- glm(
  win_local ~ 
    xGA_local + 
    big_chances_concedidas_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo17)

#pases largos efectivos pvalor = 0.22426

modelo_defensivo18 <- glm(
  win_local ~ 
    xGA_local + 
    big_chances_concedidas_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    intercepciones_local + 
    recuperaciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo18)

#recuperaciones pvalor = 0.21631

modelo_defensivo19 <- glm(
  win_local ~ 
    xGA_local + 
    big_chances_concedidas_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    intercepciones_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo19)

#intercepciones pvalor = 0.23415

modelo_defensivo20 <- glm(
  win_local ~ 
    xGA_local + 
    big_chances_concedidas_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo20)

#duelos ganados pvalor = 0.14864

modelo_defensivo21 <- glm(
  win_local ~ 
    xGA_local + 
    big_chances_concedidas_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo21)

#aqui son todos significativos, si se puede depurar mas, voy a quitar big chances por ser menos que xGA

modelo_defensivo22 <- glm(
  win_local ~ 
    xGA_local + 
    rojas_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+
    centros_concedidos_local	+ 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo22)
#es curioso pq me sale la de rojas, pero no la de intercepciones, el depurado inicial estaba mal hecho
#ahora todas son significativas, si acaso quito rojas local y veo como queda
#aqui el Residual deviance: 430.65 y AIC es 446.65

modelo_defensivo23 <- glm(
  win_local ~ 
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo23)

#ahora con este ultimo modelo ha empeorado el residuo a 434.95 y el AIC a 448.95
#sin embargo, todas las variables han disminuido su pvalor
#pruebo a quitar despejes a ver que ocurre

modelo_defensivo24 <- glm(
  win_local ~ 
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local
  ,
  
  data = d,
  family = binomial
)
summary(modelo_defensivo24)

#ahora el AIC a 453.33 y el residuo a 441.33
#no me gusta



##########################################################################################

#depurado modelo ofensivo

modelo_ofensivo_completo <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    corners_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    amarillas_forzadas_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo_completo)

#quito ventaja numerica pvalor = 0.960667

modelo_ofensivo1 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    corners_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    amarillas_forzadas_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo1)

#corners local pvalor = 0.905289

modelo_ofensivo2 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    amarillas_forzadas_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo2)

#amarillas forzadas local pvalor = 0.867659    

modelo_ofensivo3 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo3)

#ahora pases local pvalor = 0.560243

modelo_ofensivo4 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo4)

#rojas forzadas local pvalor = 0.517546

modelo_ofensivo5 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo5)

#pases largos local pvalor = 0.483970    

modelo_ofensivo6 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo6)

#pases U3 efectivos local pvalor = 0.511861

modelo_ofensivo7 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    regates_efectivos_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo7)

#regates efectivos local pvalor = 0.509554

modelo_ofensivo8 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo8)

#regates local pvalor = 0.596964

modelo_ofensivo9 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo9)

#min roja local pvalor = 0.428835

modelo_ofensivo10 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo10)

#intercepciones concedidas local pvalor = 0.350166

modelo_ofensivo11 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    toques_area_penal_local+
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo11)

#toques area penal pvalor = 0.304792

modelo_ofensivo12 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo12)

#faltas recibidas U3 local pvalor = 0.290224

modelo_ofensivo13 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo13)

#duelos ganados local pvalor = 0.272148    

modelo_ofensivo14 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo14)

#entradas concedidas local pvalor = 0.289453

modelo_ofensivo15 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    faltas_recibidas_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo15)

#faltas recibidas local pvalor = 0.220788

modelo_ofensivo16 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo16)

#tiros local pvalor = 0.274767

modelo_ofensivo17 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo17)

# tiros libres pvalor = 0.117460

modelo_ofensivo18 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo18)

#ahora son todas significativas, voy a ir depurando para que aumente el pvalor
#ahora el residuo es de 343.02 y AIC es 373.02
#voy a quitar xG local pvalor = 0.084511

modelo_ofensivo19 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_al_U3_local +
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo19)

#ahora es pases al U3 local pvalor = 0.051178

modelo_ofensivo20 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo20)

#pases largos efectivos local pvalor = 0.069209

modelo_ofensivo21 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_U3_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo21)

#centros rematados local pvalor = 0.07882

modelo_ofensivo22 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_U3_local +
    
    # --- Juego por banda ---
    centros_local +
    
    # --- Balón parado ofensivo ---
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo22)

#entradas ganadas concedidas pvalor = 0.069698

modelo_ofensivo23 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_U3_local +
    
    # --- Juego por banda ---
    centros_local +
    
    # --- Balón parado ofensivo ---
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_ofensivo23)

#ahora todas son significativas 1 estrella, pero el intercept me sale muy alto, casi 1
install.packages("car")
library(car)
vif(modelo_ofensivo23)

exp(coef(modelo_ofensivo23))
exp(confint(modelo_ofensivo23))

#modelo con interaccion

modelo_interaccion <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_puerta_local *
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_U3_local +
    
    # --- Juego por banda ---
    centros_local +
    
    # --- Balón parado ofensivo ---
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    recuperaciones_local +
    despejes_local,
  
  data = d,
  family = binomial
)

summary(modelo_interaccion)
#no aporta mucho, con anova comparo
anova(modelo_ofensivo23, modelo_interaccion, test = "Chisq")
#Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#1       370     360.40                     
#2       369     360.38  1 0.018692   0.8913



#ahora montamos el modelo fisico, el ultimo del general

modelo_fisico_exploratorio <- glm(
  win_local ~
    descanso_local +
    descanso_visit +
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
  family = binomial
)
summary(modelo_fisico_exploratorio)

#amarillas local pvalor = 0.917930

modelo_fisico1 <- glm(
  win_local ~
    diff_descanso + 
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_ganadas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_forzadas_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d,
  family = binomial
)
summary(modelo_fisico1)

#entradas ganadas local pvalor = 0.9596

modelo_fisico2 <- glm(
  win_local ~
    diff_descanso + 
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_forzadas_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d,
  family = binomial
)
summary(modelo_fisico2)

#faltas local pvalor = 0.94109

modelo_fisico3 <- glm(
  win_local ~
    diff_descanso + 
    faltas_recibidas_local +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    amarillas_forzadas_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d,
  family = binomial
)
summary(modelo_fisico3)

#amarillas forzadas local pvalor = 0.90485

modelo_fisico4 <- glm(
  win_local ~
    diff_descanso + 
    faltas_recibidas_local +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local,
  data = d,
  family = binomial
)
summary(modelo_fisico4)

#min_ventaja_numerica_local pvalor = 0.81701

modelo_fisico5 <- glm(
  win_local ~
    diff_descanso + 
    faltas_recibidas_local +
    entradas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local 
  ,
  data = d,
  family = binomial
)
summary(modelo_fisico5)

#entradas local pvalor = 0.74901

modelo_fisico6 <- glm(
  win_local ~
    diff_descanso + 
    faltas_recibidas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local 
  ,
  data = d,
  family = binomial
)
summary(modelo_fisico6)

#min roja local pvalor = 0.58539

modelo_fisico7 <- glm(
  win_local ~
    diff_descanso + 
    faltas_recibidas_local +
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local 
  ,
  data = d,
  family = binomial
)
summary(modelo_fisico7)

#faltas recibidas local pvalor = 0.42244

modelo_fisico8 <- glm(
  win_local ~
    diff_descanso + 
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local 
  ,
  data = d,
  family = binomial
)
summary(modelo_fisico8)

#rojas forzadas local pvalor = 0.25793

modelo_fisico9 <- glm(
  win_local ~
    diff_descanso + 
    entradas_concedidas_local +
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local
  ,
  data = d,
  family = binomial
)
summary(modelo_fisico9)

#entradas concedidas local pvalor = 0.29189

modelo_fisico10 <- glm(
  win_local ~
    diff_descanso + 
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local
  ,
  data = d,
  family = binomial
)
summary(modelo_fisico10)

#ya todas significativas, voy a seguir depurando ahora con entradas ganadas concedidas local pvalor = 0.09343

modelo_fisico11 <- glm(
  win_local ~
    diff_descanso + 
    duelos_ganados_local +
    rojas_local
  ,
  data = d,
  family = binomial
)
summary(modelo_fisico11)

#yo creo que mejor el modelo fisico 10, el AIC es mas bajo

#montamos el modelo control del partido

modelo_control_partido <- glm(
  win_local ~
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
  family = binomial
)
summary(modelo_control_partido)

#Montamos el contexto de partido

d$jornada <- as.numeric(as.character(d$jornada))

modelo_contexto_partido <- glm(
  win_local ~
    jornada + 
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    forma_local_5 +
    forma_visitante_5 +
    diff_descanso 
    ,
  data = d,
  family = binomial
)
summary(modelo_contexto_partido)

# Tabla de Odds Ratios con IC 95%
OR_contexto <- tidy(modelo_contexto_partido, conf.int = TRUE, exponentiate = TRUE)

# Mostramos la tabla ordenada por importancia (tamaño del OR)
OR_contexto[order(abs(OR_contexto$estimate), decreasing = TRUE), ]


#Grafico de coeficientes (log-odds)
library(ggplot2)
library(broom)

coef_contexto_df <- tidy(modelo_contexto_partido, conf.int = TRUE)
coef_contexto_df <- coef_contexto_df[coef_contexto_df$term != "(Intercept)", ]

ggplot(coef_contexto_df, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Coeficientes del Modelo Logístico – Modelo A",
    x = "Coeficiente (log-odds)",
    y = "Variables"
  ) +
  theme_minimal(base_size = 13)

#modelo contexto partido de este año (sin posiciones previas)

modelo_contexto_actual <- glm(
  win_local ~
    jornada + 
    diferencia_puntos_local +
    forma_local_5 +
    forma_visitante_5 +
    diff_descanso 
  ,
  data = d,
  family = binomial
)
summary(modelo_contexto_actual)

# Modelo logístico con variables escritas a mano

modelo_logit_completo <- glm(
  win_local ~ 
    jornada +
    equipo_local +
    equipo_visitante +
    formacion_local +
    formacion_visit +
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    forma_local_5 +
    forma_visitante_5 +
    diff_descanso +
    posesion_local +
    tiros_local +
    tiros_concedidos_local +
    tiros_puerta_local +
    tiros_puerta_concedidos_local +
    xG_local +
    xGA_local +
    big_chances_local +
    big_chances_concedidas_local +
    pases_local +
    pases_en_contra_local +
    faltas_local +
    faltas_recibidas_local +
    amarillas_local +
    amarillas_forzadas_local +
    rojas_local +
    rojas_forzadas_local +
    min_roja_local +
    min_ventaja_numerica_local +
    corners_local +
    corners_concedidos_local +
    entradas_local +
    entradas_concedidas_local +
    tiros_libres_local +
    tiros_libres_concedidos_local +
    toques_area_penal_local +
    toques_area_penal_concedidos_local +
    faltas_recibidas_U3_local +
    faltas_cometidas_U3_local +
    pases_al_U3_local +
    pases_al_U3_concedidos_local +
    pases_U3_local +
    pases_U3_efectivos_local +
    pases_U3_concedidos_local +
    pases_U3_efectivos_concedidos_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    pases_largos_concedidos_local +
    pases_largos_efectivos_concedidos_local +
    centros_local +
    centros_rematados_local +
    centros_concedidos_local +
    centros_rematados_concedidos_local +
    duelos_ganados_local +
    regates_local +
    regates_efectivos_local +
    regates_concedidos_local +
    regates_efectivos_concedidos_local +
    entradas_ganadas_local +
    entradas_ganadas_concedidas_local +
    intercepciones_local +
    intercepciones_concedidas_local +
    recuperaciones_local +
    recuperaciones_concedidas_local +
    despejes_local +
    despejes_concedidos_local +
    zona2_pases_pct +
    zona3_pases_pct,
  family = binomial,
  data = d,
)
summary(modelo_logit_completo)

#modelo general con variables mas significativas depuradas a mano

mod_gral <- glm(
  win_local ~
    #fisico
    diff_descanso + 
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    diferencia_puntos_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    pases_U3_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(mod_gral)

#depuro pases_U3_local       -0.001326   0.005296  -0.250 0.802311    
mod_gral1 <- glm(
  win_local ~
    #fisico
    diff_descanso + 
    entradas_ganadas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    diferencia_puntos_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(mod_gral1)
#dupero duelos_ganados_local    -2.029152   2.831362  -0.717 0.473578    
mod_gral2 <- glm(
  win_local ~
    #fisico
    diff_descanso + 
    entradas_ganadas_concedidas_local +
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    diferencia_puntos_local +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(mod_gral2)
#depuro diferencia_puntos_local  0.015851   0.016832   0.942 0.346337    

mod_gral3 <- glm(
  win_local ~
    #fisico
    diff_descanso + 
    entradas_ganadas_concedidas_local +
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(mod_gral3)
#diff_descanso                     -0.063271   0.062454  -1.013 0.311017    
mod_gral4 <- glm(
  win_local ~
    #fisico
    entradas_ganadas_concedidas_local +
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(mod_gral4)
#entradas_ganadas_concedidas_local  1.806707   1.339721   1.349 0.177475    
mod_gral5 <- glm(
  win_local ~
    #fisico
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(mod_gral5)
#pases_U3_concedidos_local       -0.008115   0.005292  -1.533  0.12519    
mod_gral6 <- glm(
  win_local ~
    #fisico
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_U3_concedidos_local	+ 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(mod_gral6)
#ya todas signifcativas cob el modelo 6

modelo_alfa <- glm(
  win_local ~
    #fisico
    rojas_local +
    #contexto
    pos_previa_local_num +
    pos_previa_visitante_num +
    #ofensivo
    tiros_puerta_local +
    big_chances_local +
    centros_local +
    rojas_local + 
    recuperaciones_concedidas_local +
    despejes_concedidos_local +
    recuperaciones_local +
    #defensivo
    xGA_local + 
    corners_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  data = d,
  family = binomial
)
summary(modelo_alfa)


#todas significativas

# Tabla de Odds Ratios con IC 95%
OR_gral <- tidy(modelo_alfa, conf.int = TRUE, exponentiate = TRUE)

# Mostramos la tabla ordenada por importancia (tamaño del OR)
OR_gral[order(abs(OR_gral$estimate), decreasing = TRUE), ]


#Grafico de coeficientes (log-odds)
library(ggplot2)
library(broom)

coef_df_gral <- tidy(modelo_alfa, conf.int = TRUE)
coef_df_gral <- coef_df_gral[coef_df_gral$term != "(Intercept)", ]

ggplot(coef_df_gral, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Coeficientes del Modelo Logístico – Modelo General Depurado",
    x = "Coeficiente (log-odds)",
    y = "Variables"
  ) +
  theme_minimal(base_size = 13)

library(pscl)
pR2(modelo_alfa)

# Probabilidades predichas
pred_prob_alfa <- predict(modelo_alfa, type = "response")

# Clasificación (0/1)
pred_prob_alfa <- ifelse(pred_prob_alfa >= 0.5, 1, 0)

# Matriz de confusión
table(real = d$win_local, pred = pred_prob_alfa)

# Accuracy
mean(pred_prob_alfa == d$win_local)

modelo_contexto_factor <- glm(
  win_local ~
    jornada +
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    forma_local_5 +
    forma_visitante_5 +
    diff_descanso,
  data = d,
  family = binomial
)
summary(modelo_contexto_factor)
d$pos_previa_local_num <- as.numeric(
  as.character(d$pos_previa_local)
)

d$pos_previa_visitante_num <- as.numeric(
  as.character(d$pos_previa_visitante)
)
modelo_contexto_numerico <- glm(
  win_local ~
    jornada +
    pos_previa_local_num +
    pos_previa_visitante_num +
    diferencia_puntos_local +
    forma_local_5 +
    forma_visitante_5 +
    diff_descanso,
  data = d,
  family = binomial
)
summary(modelo_contexto_numerico)

AIC(modelo_contexto_factor, modelo_contexto_numerico)
anova(
  modelo_contexto_numerico,
  modelo_contexto_factor,
  test = "Chisq"
)

modelo_ofensivo_final_d <- modelo_ofensivo23
modelo_defensivo_final_d <- modelo_defensivo23
modelo_fisico_final_d <- modelo_fisico10
modelo_control_final_d <- modelo_control_partido
modelo_contexto_final_d <- modelo_contexto_numerico
modelo_general_final_d <- modelo_alfa
c(
  ofensivo = nobs(modelo_ofensivo_final_d),
  defensivo = nobs(modelo_defensivo_final_d),
  fisico = nobs(modelo_fisico_final_d),
  control = nobs(modelo_control_final_d),
  contexto = nobs(modelo_contexto_final_d),
  general = nobs(modelo_general_final_d)
)
# Solo si algún paquete no está instalado:
# install.packages(c(
#   "ggplot2", "dplyr", "tidyr", "broom",
#   "forcats", "scales", "purrr", "patchwork"
# ))

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(forcats)
library(scales)
library(purrr)
library(patchwork)
# ============================================================
# COMPARACION DE LA CODIFICACION DE LAS POSICIONES
# ============================================================

comparacion_posiciones <- tibble(
  Especificacion = c(
    "Posiciones como factores",
    "Posiciones numéricas"
  ),
  AIC = c(
    AIC(modelo_contexto_factor),
    AIC(modelo_contexto_numerico)
  ),
  Desviacion_residual = c(
    deviance(modelo_contexto_factor),
    deviance(modelo_contexto_numerico)
  ),
  Parametros = c(
    length(coef(modelo_contexto_factor)),
    length(coef(modelo_contexto_numerico))
  )
)

comparacion_posiciones

datos_ajuste_posiciones <- comparacion_posiciones %>%
  select(
    Especificacion,
    AIC,
    Desviacion_residual
  ) %>%
  pivot_longer(
    cols = c(AIC, Desviacion_residual),
    names_to = "Metrica",
    values_to = "Valor"
  ) %>%
  mutate(
    Metrica = recode(
      Metrica,
      "AIC" = "AIC",
      "Desviacion_residual" = "Desviación residual"
    )
  )

grafico_ajuste_posiciones <- ggplot(
  datos_ajuste_posiciones,
  aes(
    x = Especificacion,
    y = Valor,
    fill = Especificacion
  )
) +
  geom_col(
    width = 0.62,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = number(
        Valor,
        accuracy = 0.01,
        decimal.mark = ","
      )
    ),
    vjust = -0.35,
    size = 3.8,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ Metrica,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Posiciones como factores" = "#8C96A3",
      "Posiciones numéricas" = "#1976A3"
    )
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.14)
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    axis.text.x = element_text(
      angle = 12,
      hjust = 1,
      color = "black",
      size = 9.5
    ),
    axis.text.y = element_text(
      color = "black"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
grafico_parametros_posiciones <- ggplot(
  comparacion_posiciones,
  aes(
    x = Especificacion,
    y = Parametros,
    fill = Especificacion
  )
) +
  geom_col(
    width = 0.62,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = Parametros),
    vjust = -0.35,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Posiciones como factores" = "#8C96A3",
      "Posiciones numéricas" = "#1976A3"
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(comparacion_posiciones$Parametros) * 1.18
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Complejidad del modelo",
    x = NULL,
    y = "Número de parámetros",
    fill = NULL
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 11,
      hjust = 0.5
    ),
    axis.text.x = element_text(
      angle = 12,
      hjust = 1,
      color = "black",
      size = 9.5
    ),
    axis.text.y = element_text(
      color = "black"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
figura_comparacion_posiciones <-
  grafico_ajuste_posiciones +
  grafico_parametros_posiciones +
  plot_layout(
    widths = c(2, 1)
  ) +
  plot_annotation(
    title = "Comparación de la codificación de la posición previa",
    subtitle = paste(
      "Modelo contextual binomial, temporada 2024/2025"
    ),
    caption = paste(
      "La especificación factorial consigue una desviación residual menor,",
      "pero la especificación numérica utiliza 36 parámetros menos",
      "y presenta un AIC inferior."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 15
      ),
      plot.subtitle = element_text(
        size = 11.5,
        color = "#555555"
      ),
      plot.caption = element_text(
        size = 9,
        color = "#666666",
        hjust = 0
      )
    )
  )

figura_comparacion_posiciones
modelos_defensivos_d <- list(
  "Completo" = modelo_defensivo_completo,
  "D1" = modelo_defensivo1,
  "D2" = modelo_defensivo2,
  "D3" = modelo_defensivo3,
  "D4" = modelo_defensivo4,
  "D5" = modelo_defensivo5,
  "D6" = modelo_defensivo6,
  "D7" = modelo_defensivo7,
  "D8" = modelo_defensivo8,
  "D9" = modelo_defensivo9,
  "D10" = modelo_defensivo10,
  "D11" = modelo_defensivo11,
  "D12" = modelo_defensivo12,
  "D13" = modelo_defensivo13,
  "D14" = modelo_defensivo14,
  "D15" = modelo_defensivo15,
  "D16" = modelo_defensivo16,
  "D17" = modelo_defensivo17,
  "D18" = modelo_defensivo18,
  "D19" = modelo_defensivo19,
  "D20" = modelo_defensivo20,
  "D21" = modelo_defensivo21,
  "D22" = modelo_defensivo22,
  "D23" = modelo_defensivo23,
  "D24" = modelo_defensivo24
)
modelos_ofensivos_d <- list(
  "Completo" = modelo_ofensivo_completo,
  "O1" = modelo_ofensivo1,
  "O2" = modelo_ofensivo2,
  "O3" = modelo_ofensivo3,
  "O4" = modelo_ofensivo4,
  "O5" = modelo_ofensivo5,
  "O6" = modelo_ofensivo6,
  "O7" = modelo_ofensivo7,
  "O8" = modelo_ofensivo8,
  "O9" = modelo_ofensivo9,
  "O10" = modelo_ofensivo10,
  "O11" = modelo_ofensivo11,
  "O12" = modelo_ofensivo12,
  "O13" = modelo_ofensivo13,
  "O14" = modelo_ofensivo14,
  "O15" = modelo_ofensivo15,
  "O16" = modelo_ofensivo16,
  "O17" = modelo_ofensivo17,
  "O18" = modelo_ofensivo18,
  "O19" = modelo_ofensivo19,
  "O20" = modelo_ofensivo20,
  "O21" = modelo_ofensivo21,
  "O22" = modelo_ofensivo22,
  "O23" = modelo_ofensivo23
)
extraer_evolucion <- function(lista_modelos, bloque) {
  
  imap_dfr(
    lista_modelos,
    function(modelo, nombre) {
      tibble(
        modelo = nombre,
        bloque = bloque,
        AIC = AIC(modelo),
        desviacion = deviance(modelo),
        numero_variables = length(coef(modelo)) - 1,
        n = nobs(modelo)
      )
    }
  ) %>%
    mutate(
      iteracion = row_number()
    )
}
evolucion_ofensivo_d <- extraer_evolucion(
  modelos_ofensivos_d,
  "Bloque ofensivo"
)

evolucion_defensivo_d <- extraer_evolucion(
  modelos_defensivos_d,
  "Bloque defensivo"
)

evolucion_bloques_d <- bind_rows(
  evolucion_ofensivo_d,
  evolucion_defensivo_d
)

evolucion_bloques_d

#######################################
evaluar_modelo_binomial <- function(modelo, nombre) {
  
  datos_modelo <- model.frame(modelo)
  real <- model.response(datos_modelo)
  
  if (is.factor(real)) {
    real_num <- as.integer(real == levels(real)[2])
  } else {
    real_num <- as.integer(real)
  }
  
  probabilidad <- predict(
    modelo,
    type = "response"
  )
  
  predicho <- ifelse(
    probabilidad >= 0.5,
    1,
    0
  )
  
  VN <- sum(real_num == 0 & predicho == 0)
  FP <- sum(real_num == 0 & predicho == 1)
  FN <- sum(real_num == 1 & predicho == 0)
  VP <- sum(real_num == 1 & predicho == 1)
  
  tibble(
    Modelo = nombre,
    n = length(real_num),
    Variables = length(coef(modelo)) - 1,
    AIC = AIC(modelo),
    Desviacion = deviance(modelo),
    
    Exactitud = (VP + VN) /
      length(real_num),
    
    Sensibilidad = ifelse(
      VP + FN == 0,
      NA_real_,
      VP / (VP + FN)
    ),
    
    Especificidad = ifelse(
      VN + FP == 0,
      NA_real_,
      VN / (VN + FP)
    ),
    
    Precision = ifelse(
      VP + FP == 0,
      NA_real_,
      VP / (VP + FP)
    )
  )
}
comparacion_modelos_d <- bind_rows(
  evaluar_modelo_binomial(
    modelo_ofensivo_final_d,
    "Ofensivo"
  ),
  evaluar_modelo_binomial(
    modelo_defensivo_final_d,
    "Defensivo"
  ),
  evaluar_modelo_binomial(
    modelo_fisico_final_d,
    "Físico"
  ),
  evaluar_modelo_binomial(
    modelo_control_final_d,
    "Control"
  ),
  evaluar_modelo_binomial(
    modelo_contexto_final_d,
    "Contexto"
  ),
  evaluar_modelo_binomial(
    modelo_general_final_d,
    "General"
  )
)

comparacion_modelos_d
grafico_AIC_modelos_d <- comparacion_modelos_d %>%
  mutate(
    Modelo = fct_reorder(
      Modelo,
      AIC,
      .desc = TRUE
    ),
    Grupo = ifelse(
      Modelo == "General",
      "Modelo general",
      "Modelos por bloques"
    )
  ) %>%
  ggplot(
    aes(
      x = AIC,
      y = Modelo,
      fill = Grupo
    )
  ) +
  geom_col(
    width = 0.64
  ) +
  geom_text(
    aes(
      label = number(
        AIC,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    hjust = -0.12,
    size = 3.7,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Modelo general" = "#1976A3",
      "Modelos por bloques" = "#8C96A3"
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(0, 0.14)
    )
  ) +
  labs(
    title = "Ajuste de los modelos",
    x = "Criterio de información de Akaike",
    y = NULL,
    fill = NULL
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13
    ),
    axis.text = element_text(
      color = "black"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
metricas_clasificacion_d <- comparacion_modelos_d %>%
  select(
    Modelo,
    Exactitud,
    Sensibilidad,
    Especificidad
  ) %>%
  pivot_longer(
    cols = c(
      Exactitud,
      Sensibilidad,
      Especificidad
    ),
    names_to = "Metrica",
    values_to = "Valor"
  ) %>%
  mutate(
    Metrica = factor(
      Metrica,
      levels = c(
        "Exactitud",
        "Sensibilidad",
        "Especificidad"
      )
    )
  )

grafico_metricas_modelos_d <- ggplot(
  metricas_clasificacion_d,
  aes(
    x = Modelo,
    y = Valor,
    color = Metrica,
    group = Metrica
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Exactitud" = "#1976A3",
      "Sensibilidad" = "#D55E00",
      "Especificidad" = "#009E73"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = percent_format(
      accuracy = 1
    )
  ) +
  labs(
    title = "Capacidad de clasificación",
    x = NULL,
    y = "Porcentaje",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1,
      color = "black"
    ),
    axis.text.y = element_text(
      color = "black"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
figura_comparacion_modelos_d <-
  grafico_AIC_modelos_d +
  grafico_metricas_modelos_d +
  plot_annotation(
    title = "Comparación de los modelos binomiales por bloques",
    subtitle = "Temporada 2024/2025",
    caption = paste(
      "Las métricas de clasificación se han calculado sobre",
      "los mismos datos utilizados para estimar los modelos."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 15
      ),
      plot.subtitle = element_text(
        color = "#555555"
      ),
      plot.caption = element_text(
        size = 9,
        color = "#666666",
        hjust = 0
      )
    )
  )

figura_comparacion_modelos_d
################################################
modelos_forest_d <- list(
  "Ofensivo" = modelo_ofensivo_final_d,
  "Defensivo" = modelo_defensivo_final_d,
  "Físico" = modelo_fisico_final_d,
  "Control" = modelo_control_final_d,
  "Contexto" = modelo_contexto_final_d
)

coef_bloques_d <- imap_dfr(
  modelos_forest_d,
  function(modelo, bloque) {
    
    tidy(
      modelo,
      conf.int = TRUE,
      exponentiate = FALSE
    ) %>%
      mutate(
        Bloque = bloque
      )
  }
) %>%
  filter(term != "(Intercept)") %>%
  
  # Si quedase alguna posición factorial, se excluye
  filter(
    !grepl("^pos_previa_local[0-9]", term),
    !grepl("^pos_previa_visitante[0-9]", term)
  ) %>%
  
  mutate(
    Variable = recode(
      term,
      
      "tiros_puerta_local" =
        "Tiros a puerta locales",
      "big_chances_local" =
        "Grandes ocasiones locales",
      "pases_U3_local" =
        "Pases en el último tercio",
      "centros_local" =
        "Centros locales",
      "rojas_local" =
        "Expulsiones locales",
      "recuperaciones_concedidas_local" =
        "Recuperaciones concedidas",
      "despejes_concedidos_local" =
        "Despejes concedidos",
      "recuperaciones_local" =
        "Recuperaciones locales",
      "despejes_local" =
        "Despejes locales",
      
      "xGA_local" =
        "Goles esperados concedidos",
      "corners_concedidos_local" =
        "Córneres concedidos",
      "pases_U3_concedidos_local" =
        "Pases últimos 30 m concedidos",
      "pases_largos_concedidos_local" =
        "Pases largos concedidos",
      "centros_concedidos_local" =
        "Centros concedidos",
      
      "diff_descanso" =
        "Diferencia de descanso",
      "entradas_ganadas_concedidas_local" =
        "Entradas ganadas concedidas",
      "duelos_ganados_local" =
        "Duelos ganados",
      
      "posesion_local" =
        "Posesión local",
      "pases_local" =
        "Pases locales",
      "pases_en_contra_local" =
        "Pases del rival",
      "zona2_pases_pct" =
        "Pases en zona 2",
      "zona3_pases_pct" =
        "Pases en zona 3",
      "corners_local" =
        "Córneres locales",
      "tiros_libres_local" =
        "Tiros libres locales",
      "tiros_libres_concedidos_local" =
        "Tiros libres concedidos",
      
      "jornada" =
        "Jornada",
      "pos_previa_local_num" =
        "Posición previa local",
      "pos_previa_visitante_num" =
        "Posición previa visitante",
      "diferencia_puntos_local" =
        "Diferencia de puntos",
      "forma_local_5" =
        "Forma reciente local",
      "forma_visitante_5" =
        "Forma reciente visitante"
    ),
    
    Evidencia = case_when(
      conf.low > 0 ~ "Asociación positiva",
      conf.high < 0 ~ "Asociación negativa",
      TRUE ~ "El intervalo incluye cero"
    )
  ) %>%
  group_by(Bloque) %>%
  mutate(
    Variable = reorder(
      Variable,
      estimate
    )
  ) %>%
  ungroup()
figura_coeficientes_bloques_d <- ggplot(
  coef_bloques_d,
  aes(
    x = estimate,
    y = Variable,
    color = Evidencia
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#666666",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    orientation = "y",
    width = 0.14,
    linewidth = 0.65
  ) +
  geom_point(
    size = 2.7
  ) +
  facet_wrap(
    ~ Bloque,
    scales = "free",
    ncol = 2
  ) +
  scale_color_manual(
    values = c(
      "Asociación positiva" = "#1976A3",
      "Asociación negativa" = "#C44E3B",
      "El intervalo incluye cero" = "#8C96A3"
    )
  ) +
  labs(
    title = "Coeficientes de los modelos definitivos por bloques",
    subtitle = "Regresión logística binomial, temporada 2024/2025",
    x = "Coeficiente estimado (log-odds)",
    y = NULL,
    color = NULL,
    caption = paste(
      "Los ejes se adaptan a la escala de cada bloque.",
      "Los coeficientes se interpretan manteniendo constantes",
      "las demás variables del modelo correspondiente."
    )
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      color = "#555555"
    ),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    axis.text.y = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.text.x = element_text(
      color = "black"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

figura_coeficientes_bloques_d
########################################################
# ============================================================
# COEFICIENTES DEL MODELO GENERAL DEFINITIVO
# ============================================================

coef_general_d <- tidy(
  modelo_general_final_d,
  conf.int = TRUE,
  exponentiate = FALSE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = recode(
      term,
      
      "rojas_local" =
        "Expulsiones locales",
      
      "pos_previa_local_num" =
        "Posición previa local",
      
      "pos_previa_visitante_num" =
        "Posición previa visitante",
      
      "tiros_puerta_local" =
        "Tiros a puerta locales",
      
      "big_chances_local" =
        "Grandes ocasiones locales",
      
      "centros_local" =
        "Centros locales",
      
      "recuperaciones_concedidas_local" =
        "Recuperaciones concedidas",
      
      "despejes_concedidos_local" =
        "Despejes concedidos",
      
      "recuperaciones_local" =
        "Recuperaciones locales",
      
      "xGA_local" =
        "Goles esperados concedidos (xGA)",
      
      "corners_concedidos_local" =
        "Córneres concedidos",
      
      "pases_largos_concedidos_local" =
        "Pases largos concedidos",
      
      "centros_concedidos_local" =
        "Centros concedidos",
      
      "despejes_local" =
        "Despejes locales"
    ),
    
    Dimension = case_when(
      term %in% c(
        "pos_previa_local_num",
        "pos_previa_visitante_num"
      ) ~ "Contexto competitivo",
      
      term %in% c(
        "tiros_puerta_local",
        "big_chances_local",
        "centros_local"
      ) ~ "Producción ofensiva",
      
      term %in% c(
        "xGA_local",
        "corners_concedidos_local",
        "pases_largos_concedidos_local",
        "centros_concedidos_local"
      ) ~ "Amenaza concedida",
      
      term == "rojas_local" ~
        "Disciplina",
      
      term %in% c(
        "recuperaciones_concedidas_local",
        "despejes_concedidos_local",
        "recuperaciones_local",
        "despejes_local"
      ) ~ "Recuperaciones y despejes",
      
      TRUE ~ "Otras variables"
    ),
    
    Evidencia = case_when(
      conf.low > 0 ~
        "Asociación positiva",
      
      conf.high < 0 ~
        "Asociación negativa",
      
      TRUE ~
        "El intervalo incluye cero"
    )
  )
coef_general_d %>%
  select(
    term,
    Variable,
    Dimension,
    estimate,
    conf.low,
    conf.high,
    p.value,
    Evidencia
  ) %>%
  print(n = Inf)
orden_variables_general <- c(
  "Expulsiones locales",
  "Posición previa local",
  "Posición previa visitante",
  "Tiros a puerta locales",
  "Grandes ocasiones locales",
  "Centros locales",
  "Recuperaciones locales",
  "Recuperaciones concedidas",
  "Despejes locales",
  "Despejes concedidos",
  "Goles esperados concedidos (xGA)",
  "Córneres concedidos",
  "Pases largos concedidos",
  "Centros concedidos"
)

coef_general_d <- coef_general_d %>%
  mutate(
    Variable = factor(
      Variable,
      levels = rev(orden_variables_general)
    )
  )
figura_modelo_general_d <- ggplot(
  coef_general_d,
  aes(
    x = estimate,
    y = Variable,
    color = Dimension
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.75,
    color = "#666666"
  ) +
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    orientation = "y",
    width = 0.15,
    linewidth = 0.75,
    color = "#454545"
  ) +
  geom_point(
    size = 3.4
  ) +
  scale_color_manual(
    values = c(
      "Contexto competitivo" = "#8E5EA2",
      "Producción ofensiva" = "#1976A3",
      "Amenaza concedida" = "#D55E00",
      "Disciplina" = "#E69F00",
      "Recuperaciones y despejes" = "#009E73",
      "Otras variables" = "#8C96A3"
    )
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 7),
    expand = expansion(
      mult = c(0.06, 0.08)
    )
  ) +
  labs(
    title = "Coeficientes del modelo general depurado",
    subtitle = "Regresión logística binomial, temporada 2024/2025",
    x = "Coeficiente estimado (log-odds)",
    y = NULL,
    color = "Dimensión",
    caption = paste(
      "Los puntos representan los coeficientes estimados",
      "y las barras horizontales sus intervalos de confianza al 95 %.",
      "La línea discontinua señala el valor cero."
    )
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      color = "#1F1F1F"
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(b = 12)
    ),
    
    axis.title.x = element_text(
      size = 11.5,
      margin = margin(t = 10)
    ),
    
    axis.text.y = element_text(
      size = 10.2,
      color = "#2B2B2B"
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "#4D4D4D"
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 9.3
    ),
    
    panel.grid.major.y = element_line(
      color = "#E8E8E8",
      linewidth = 0.45
    ),
    
    panel.grid.major.x = element_line(
      color = "#E0E0E0",
      linewidth = 0.45
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      color = "#666666",
      margin = margin(t = 12)
    ),
    
    plot.margin = margin(
      t = 18,
      r = 25,
      b = 15,
      l = 15
    )
  )

print(figura_modelo_general_d)



#########################################################
preparar_matriz_confusion <- function(
    modelo,
    nombre_modelo
) {
  
  datos_modelo <- model.frame(modelo)
  real <- model.response(datos_modelo)
  
  if (is.factor(real)) {
    
    niveles <- levels(real)
    
    real_etiqueta <- ifelse(
      real == niveles[2],
      "Victoria local",
      "No victoria local"
    )
    
  } else {
    
    real_etiqueta <- ifelse(
      real == 1,
      "Victoria local",
      "No victoria local"
    )
  }
  
  probabilidad <- predict(
    modelo,
    type = "response"
  )
  
  predicho_etiqueta <- ifelse(
    probabilidad >= 0.5,
    "Victoria local",
    "No victoria local"
  )
  
  tibble(
    Real = factor(
      real_etiqueta,
      levels = c(
        "Victoria local",
        "No victoria local"
      )
    ),
    Predicho = factor(
      predicho_etiqueta,
      levels = c(
        "No victoria local",
        "Victoria local"
      )
    )
  ) %>%
    count(
      Real,
      Predicho,
      name = "Frecuencia"
    ) %>%
    complete(
      Real,
      Predicho,
      fill = list(
        Frecuencia = 0
      )
    ) %>%
    group_by(Real) %>%
    mutate(
      Porcentaje = Frecuencia /
        sum(Frecuencia),
      
      Etiqueta = paste0(
        Frecuencia,
        "\n",
        percent(
          Porcentaje,
          accuracy = 0.1,
          decimal.mark = ","
        )
      ),
      
      Modelo = nombre_modelo
    ) %>%
    ungroup()
}
matrices_confusion_d <- bind_rows(
  preparar_matriz_confusion(
    modelo_ofensivo_final_d,
    "Bloque ofensivo"
  ),
  preparar_matriz_confusion(
    modelo_defensivo_final_d,
    "Bloque defensivo"
  ),
  preparar_matriz_confusion(
    modelo_general_final_d,
    "Modelo general"
  )
)

figura_matrices_confusion_d <- ggplot(
  matrices_confusion_d,
  aes(
    x = Predicho,
    y = Real,
    fill = Porcentaje
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1.5
  ) +
  geom_text(
    aes(label = Etiqueta),
    size = 4.3,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ Modelo,
    nrow = 1
  ) +
  scale_fill_gradient(
    low = "#E8F1F8",
    high = "#1976A3",
    labels = percent_format(
      accuracy = 1
    )
  ) +
  coord_equal() +
  labs(
    title = "Matrices de confusión de los principales modelos",
    subtitle = "Temporada 2024/2025",
    x = "Resultado predicho",
    y = "Resultado observado",
    fill = "Porcentaje\npor resultado real",
    caption = paste(
      "Cada celda muestra el número de partidos y el porcentaje",
      "calculado dentro de la categoría observada."
    )
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      color = "#555555"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1,
      color = "black"
    ),
    axis.text.y = element_text(
      color = "black"
    ),
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

figura_matrices_confusion_d
########################################