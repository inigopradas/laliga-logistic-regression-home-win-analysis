# ==============================================================================
# Title: Multi-season binomial block and general model selection
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops, simplifies and evaluates binomial logistic regression
# models for home-team victory using match-level data from the 2022/2023,
# 2023/2024 and 2024/2025 LaLiga seasons.
#
# The script imports and prepares the multi-season dataset, converts the
# relevant categorical variables into factors, cleans tactical formation
# labels and prepares the rest and recent-form variables. It creates an
# indicator for matches without applicable previous-rest information,
# calculates the difference in rest days between the home and visiting teams
# and imputes unavailable recent-form values at the beginning of each season.
#
# The modelling process is organised into defensive, offensive, physical,
# match-control and competitive-context blocks. Each block begins with a
# comparatively broad specification and is progressively simplified through
# the removal of variables with limited statistical contribution. The selected
# specifications balance coefficient significance, model fit and substantive
# interpretability.
#
# The defensive block evaluates variables related to shots and chances
# conceded, expected goals against, opposition passing, disciplinary events,
# set pieces, defensive actions, long passes, crosses, duels and clearances.
#
# The offensive block evaluates attacking production, expected goals, shots,
# passing, wide play, dribbling, duels, set pieces, disciplinary events,
# numerical advantages and defensive actions performed by the opposition.
#
# The physical block evaluates rest differences, fouls, tackles, duels, cards
# and numerical-advantage variables. The match-control block examines
# possession, passing volume, corners, free kicks and duels. The contextual
# block assesses season, matchday, previous league positions, points
# differences, recent form and rest differences.
#
# The predictors retained from the individual blocks are subsequently combined
# in a general binomial logistic regression. The general model is progressively
# simplified to obtain a final specification that integrates contextual,
# physical, offensive and defensive information.
#
# For the complete and selected models, the script calculates predicted
# probabilities and classifies each match using a probability threshold of
# 0.50. Confusion matrices and in-sample accuracy are used to describe the
# classification performance of the different specifications.
#
# The final general model is additionally represented through a coefficient
# plot with 95% confidence intervals. The plot displays the estimated
# log-odds association of each retained predictor with the probability of a
# home-team victory.
#
# Input:
#   LaLiga_22-25_completo_v2 (2).xlsx
#
# Dataset:
#   d1, containing match-level observations from the 2022/2023, 2023/2024 and
#   2024/2025 LaLiga seasons.
#
# Dependent variable:
#   win_local
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory, including draws and away-team victories
#
# Statistical method:
#   Binomial logistic regression estimated with glm() and family = binomial.
#
# Model-selection method:
#   Manual backward reduction based on coefficient significance, AIC, model
#   fit and substantive interpretation.
#
# Main defensive models:
#   modelo_defensivo_completo
#   modelo_defensivo2 to modelo_defensivo18
#
# Selected defensive model:
#   modelo_defensivo17
#
# Main offensive models:
#   modelo_ofensivo_completo
#   modelo_ofensivo2 to modelo_ofensivo15
#
# Selected offensive model:
#   modelo_ofensivo14
#
# Main physical models:
#   modelo_fisico_exploratorio
#   modelo_fisico2 to modelo_fisico10
#
# Selected physical model:
#   modelo_fisico10
#
# Match-control models:
#   modelo_control_partido
#   modelo_control2
#
# Selected match-control model:
#   modelo_control2
#
# Contextual models:
#   modelo_contexto_partido
#   modelo_contexto2 to modelo_contexto5
#
# Selected contextual model:
#   modelo_contexto5
#
# General candidate models:
#   modelo_general
#   modelo_general2 to modelo_general8
#
# Final general model:
#   modelo_general8
#
# Main outputs:
#   Model summaries, predicted probabilities, confusion matrices, in-sample
#   accuracy and a coefficient plot for the selected general model.
# ==============================================================================
library(readxl)   # para leer Excel
library(dplyr)    # para manipular datos

getwd()
d1 <- read_excel("LaLiga_22-25_completo_v2 (2).xlsx")
#de aqui hay muchos valores que salen como NA porque no se recopilaron (de d1)
#voy a calcular el contraste de las formaciones

str(d1)
head(d1)
d1$win_local = factor(d1$win_local)
d1$temporada = factor(d1$temporada)
d1$formacion_local = factor(d1$formacion_local)
d1$formacion_visit = factor(d1$formacion_visit)

# Quitar comillas internas y espacios
d1$formacion_local = gsub('"', '', trimws(as.character(d1$formacion_local)))
d1$formacion_visit = gsub('"', '', trimws(as.character(d1$formacion_visit)))

# Convertir a factor
d1$formacion_local = factor(d1$formacion_local)
d1$formacion_visit = factor(d1$formacion_visit)

# Comprobar niveles
levels(d1$formacion_local)
levels(d1$formacion_visit)
class(d1$jornada)
# Asegurar que equipos son character
d1$equipo_local <- as.character(d1$equipo_local)
d1$equipo_visitante <- as.character(d1$equipo_visitante)

# -------------------------
# DESCANSO
# -------------------------

# 1. Limpiar valores problemáticos
d1$descanso_local[d1$descanso_local %in% c("", "-", "NA")] <- NA
d1$descanso_visit[d1$descanso_visit %in% c("", "-", "NA")] <- NA

# 2. Convertir a numérico
d1$descanso_local <- as.numeric(d1$descanso_local)
d1$descanso_visit <- as.numeric(d1$descanso_visit)

# 3. Indicador de que no aplica descanso previo
# Mejor usar jornada == 1 si eso es lo correcto en tu base
d1$no_descanso_previo <- ifelse(d1$jornada == 1, 1, 0)

# 4. Imputación técnica para poder calcular la diferencia
d1$descanso_local_imp <- d1$descanso_local
d1$descanso_visit_imp <- d1$descanso_visit

d1$descanso_local_imp[is.na(d1$descanso_local_imp)] <- 0
d1$descanso_visit_imp[is.na(d1$descanso_visit_imp)] <- 0

# 5. Diferencia de descanso
d1$diff_descanso <- d1$descanso_local_imp - d1$descanso_visit_imp


# -------------------------
# FORMA RECIENTE
# -------------------------

# 1. Limpiar texto raro
d1$forma_local_5[d1$forma_local_5 %in% c("", "-", "NA")] <- NA
d1$forma_visitante_5[d1$forma_visitante_5 %in% c("", "-", "NA")] <- NA

# 2. Convertir a numérico
d1$forma_local_5 <- as.numeric(d1$forma_local_5)
d1$forma_visitante_5 <- as.numeric(d1$forma_visitante_5)

# 3. Imputar NA a 0 si tu criterio es que al inicio de temporada no hay forma previa
d1$forma_local_5[is.na(d1$forma_local_5)] <- 0
d1$forma_visitante_5[is.na(d1$forma_visitante_5)] <- 0




#################################################################################

#MODELO B: Por bloques sin toques area penal, pases U3, recuperaciones
#Modelo defensivo 
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
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    regates_efectivos_concedidos_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo_completo)

# Probabilidades predichas
pred1_prob = predict(modelo_defensivo_completo, newdata = d1, type = "response")

# Clasificación binaria con umbral 0.5
pred1_y = ifelse(pred1_prob >= 0.5, 1, 0)

# Matriz de confusión
mc1 = table(Real = d1$win_local, Predicho = pred1_y)
mc1
mean(pred1_y== d1$win_local)

#sin regates_efectivos_concedidos_local  pvalor = 0.962630                

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
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    entradas_ganadas_local + 
    intercepciones_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo2)

#amarillas_local   pvalor =  0.852915        

modelo_defensivo3 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    rojas_local + 
    min_roja_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    regates_concedidos_local	+ 
    entradas_ganadas_local + 
    intercepciones_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo3)

#sin min_roja_local   pvalor =  0.674451    

modelo_defensivo4 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    entradas_ganadas_local + 
    regates_concedidos_local	+ 
    intercepciones_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo4)

#sin regates_concedidos_local   pvalor = 0.613774            

modelo_defensivo5 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    entradas_ganadas_local + 
    intercepciones_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo5)

#sin intercepciones_local    pvalor = 0.605604            

modelo_defensivo6 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    entradas_ganadas_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo6)

#sin pases_en_contra_local    pvalor = 0.449442  

modelo_defensivo7 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local +
    faltas_cometidas_U3_local +
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    entradas_ganadas_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo7)

#sin faltas_cometidas_U3_local   pvalor = 0.451420            

modelo_defensivo8 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    centros_rematados_concedidos_local + 
    duelos_ganados_local + 
    entradas_ganadas_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo8)

#sin centros_rematados_concedidos_local  pvalor = 0.387614            

modelo_defensivo9 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    entradas_ganadas_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo9)

#sin entradas_ganadas_local   pvalor = 0.38711            

modelo_defensivo10 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    pases_largos_efectivos_concedidos_local + 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo10)

#sin pases_largos_efectivos_concedidos_local pvalor = 0.254421            

modelo_defensivo11 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    entradas_local + 
    tiros_libres_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo11)

#sin entradas_local pvalor =  0.24673            

modelo_defensivo12 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    tiros_libres_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    duelos_ganados_local + 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo12)

#sin tiros_concedidos_local   pvalor = 0.255364            

modelo_defensivo13 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    tiros_libres_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local +
    duelos_ganados_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo13)

#sin duelos_ganados_local   pvalor =  0.25653            

modelo_defensivo14 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    tiros_libres_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo14)

#sin tiros_libres_concedidos_local pvalor = 0.14218            

modelo_defensivo15 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    faltas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo15)

#sin faltas_local  pvalor = 0.864768            

modelo_defensivo16 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo16)

#sin big_chances_concedidas_local pvalor = 0.103999            

modelo_defensivo17 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	+ 
    despejes_local
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo17)

#sin despejes_local pvalor = 0.09793 (ya significativa en un . )    

modelo_defensivo18 <- glm(
  win_local ~ 
    tiros_puerta_concedidos_local + 
    xGA_local + 
    rojas_local + 
    min_ventaja_numerica_local +
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  
  data = d1,
  family = binomial
)

summary(modelo_defensivo18)
#aqui todas significativas, pero aumenta el AIC

# =========================================================
# MATRIZ DE CONFUSION Y ACCURACY EN TODA LA MUESTRA
# =========================================================

# Probabilidades predichas
pred_prob = predict(modelo_defensivo17, newdata = d1, type = "response")

# Clasificación binaria con umbral 0.5
pred_y = ifelse(pred_prob >= 0.5, 1, 0)

# Matriz de confusión
mc = table(Real = d1$win_local, Predicho = pred_y)
mc
mean(pred_y== d1$win_local)

#########################################################################################

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
    pases_largos_local +
    pases_largos_efectivos_local +
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
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo_completo)

# Probabilidades predichas
pred2_prob = predict(modelo_ofensivo_completo, newdata = d1, type = "response")

# Clasificación binaria con umbral 0.5
pred2_y = ifelse(pred2_prob >= 0.5, 1, 0)

# Matriz de confusión
mc2 = table(Real = d1$win_local, Predicho = pred2_y)
mc2
mean(pred2_y== d1$win_local)

#sin amarillas_forzadas_local pvalor = 0.745527            

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
    pases_largos_local +
    pases_largos_efectivos_local +
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
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    min_roja_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo2)

#sin min_roja_local   pvalor = 0.663306 

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
    pases_largos_local +
    pases_largos_efectivos_local +
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
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    intercepciones_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo3)

#sin intercepciones_concedidas_local pvalor =  0.63467            

modelo_ofensivo4 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
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
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo4)

#sin faltas_recibidas_local   pvalor =  0.62440    

modelo_ofensivo5 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
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
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    entradas_concedidas_local +
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo5)

#sin entradas_concedidas_local pvalor =  0.58552    

modelo_ofensivo6 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
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
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo6)

#sin regates_efectivos_local  pvalor = 0.63031    

modelo_ofensivo7 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    corners_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    entradas_ganadas_concedidas_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo7)

#sin entradas_ganadas_concedidas_local pvalor =  0.54484    

modelo_ofensivo8 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    regates_local +
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    corners_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +

    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo8)

#sin regates_local pvalor = 0.35787        

modelo_ofensivo9 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Regates y duelos ofensivos ---
    duelos_ganados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    corners_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo9)

#duelos_ganados_local  pvalor =  0.34379    

modelo_ofensivo10 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    tiros_libres_local +
    corners_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo10)

#tiros_libres_local  pvalor =  0.50222    

modelo_ofensivo11 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    corners_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo11)

#pases_largos_local  pvalor =  0.29708    

modelo_ofensivo12 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    corners_local +
    faltas_recibidas_U3_local +
    rojas_forzadas_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo12)

#rojas_forzadas_local  pvalor =  0.28317    

modelo_ofensivo13 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    pases_largos_efectivos_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    corners_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo13)

#pases_largos_efectivos_local pvalor =  0.23605    

modelo_ofensivo14 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    corners_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Ventaja numérica ---
    min_ventaja_numerica_local +
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo14)

#ya todas significativas, veo donde esta el valle
#sin min_ventaja_numerica_local pvalor = 0.088420 .  

modelo_ofensivo15 <- glm(
  win_local ~ 
    
    # --- Producción ofensiva del local ---
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    
    # --- Construcción ofensiva ---
    pases_local +
    pases_al_U3_local +
    # --- Juego por banda ---
    centros_local +
    centros_rematados_local +
    
    # --- Balón parado ofensivo ---
    corners_local +
    faltas_recibidas_U3_local +
    rojas_local + 
    # --- Acciones defensivas del rival ante el ataque local ---
    despejes_concedidos_local +
    
    # --- Acciones del local que ayudan a medir recuperación ofensiva ---
    despejes_local,
  
  data = d1,
  family = binomial
)

summary(modelo_ofensivo15)

#ha pegado un rebote el AIC, no me interesa

# Probabilidades predichas
pred3_prob = predict(modelo_ofensivo14, newdata = d1, type = "response")

# Clasificación binaria con umbral 0.5
pred3_y = ifelse(pred3_prob >= 0.5, 1, 0)

# Matriz de confusión
mc3 = table(Real = d1$win_local, Predicho = pred3_y)
mc3
mean(pred3_y== d1$win_local)


####################################################################################

modelo_fisico_exploratorio <- glm(
  win_local ~
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
  family = binomial
)
summary(modelo_fisico_exploratorio)

#Probabilidades predichas
pred4_prob <- predict(modelo_fisico_exploratorio, type = "response")

# Clasificación (0/1)
pred4_y <- ifelse(pred4_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred4_y)

# Accuracy
mean(pred4_y == d1$win_local)

#sin min_roja_local pvalor = 0.84668

modelo_fisico2 <- glm(
  win_local ~
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
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico2)

#sin amarillas_local    pvalor = 0.80304        

modelo_fisico3 <- glm(
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
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico3)

# sin amarillas_forzadas_local pvalor =  0.7310    

modelo_fisico4 <- glm(
  win_local ~
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
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico4)

#entradas_ganadas_concedidas_local pvalor = 0.68634    

modelo_fisico5 <- glm(
  win_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_ganadas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico5)

#entradas_ganadas_local     pvalor = 0.66711    

modelo_fisico6 <- glm(
  win_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    entradas_concedidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico6)

#sin entradas_concedidas_local pvalor = 0.4485        

modelo_fisico7 <- glm(
  win_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    entradas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico7)

#sin entradas_local  pvalor = 0.46864        

modelo_fisico8 <- glm(
  win_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    duelos_ganados_local +
    rojas_local +
    rojas_forzadas_local +
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico8)

#sin rojas_forzadas_local   pvalor = 0.35890        

modelo_fisico9 <- glm(
  win_local ~
    diff_descanso +
    faltas_local +
    faltas_recibidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico9)

#sin faltas_local   pvalor = 0.26886        

modelo_fisico10 <- glm(
  win_local ~
    diff_descanso +
    faltas_recibidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local,
  data = d1,
  family = binomial
)
summary(modelo_fisico10)
#ya todas significativas
#Probabilidades predichas
pred5_prob <- predict(modelo_fisico10, type = "response")

# Clasificación (0/1)
pred5_y <- ifelse(pred5_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred5_y)

# Accuracy
mean(pred5_y == d1$win_local)
#todas significativas

#############################################################################

#sin zonas de pase, es mas explicativo

modelo_control_partido <- glm(
  win_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    corners_local +
    corners_concedidos_local + 
    tiros_libres_local +
    tiros_libres_concedidos_local + 
    duelos_ganados_local,
  data = d1,
  family = binomial
)
summary(modelo_control_partido)

#Probabilidades predichas
pred6_prob <- predict(modelo_control_partido, type = "response")

# Clasificación (0/1)
pred6_y <- ifelse(pred6_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred6_y)

# Accuracy
mean(pred6_y == d1$win_local)

#sin corners_local   pvalor =  0.17112    

modelo_control2 <- glm(
  win_local ~
    posesion_local +
    pases_local +
    pases_en_contra_local +
    corners_concedidos_local + 
    tiros_libres_local +
    tiros_libres_concedidos_local + 
    duelos_ganados_local,
  data = d1,
  family = binomial
)
summary(modelo_control2)

#Probabilidades predichas
pred7_prob <- predict(modelo_control2, type = "response")

# Clasificación (0/1)
pred7_y <- ifelse(pred7_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred7_y)

# Accuracy
mean(pred7_y == d1$win_local)

###########################################################################

modelo_contexto_partido <- glm(
  win_local ~
    temporada +
    jornada + 
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    forma_local_5 +
    forma_visitante_5 +
    diff_descanso 
  ,
  data = d1,
  family = binomial
)
summary(modelo_contexto_partido)

#Probabilidades predichas
pred8_prob <- predict(modelo_contexto_partido, type = "response")

# Clasificación (0/1)
pred8_y <- ifelse(pred8_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred8_y)

# Accuracy
mean(pred8_y == d1$win_local)

#sin temporada

modelo_contexto2 <- glm(
  win_local ~
    jornada + 
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    forma_local_5 +
    forma_visitante_5 +
    diff_descanso 
  ,
  data = d1,
  family = binomial
)
summary(modelo_contexto2)

#forma_local_5   pvalor =  0.3631    

modelo_contexto3 <- glm(
  win_local ~
    jornada + 
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    forma_visitante_5 +
    diff_descanso 
  ,
  data = d1,
  family = binomial
)
summary(modelo_contexto3)

#jornada   pvalor = 0.220959    

modelo_contexto4 <- glm(
  win_local ~
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    forma_visitante_5 +
    diff_descanso 
  ,
  data = d1,
  family = binomial
)
summary(modelo_contexto4)

#forma_visitante_5     pvalor = 0.27765    

modelo_contexto5 <- glm(
  win_local ~
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso 
  ,
  data = d1,
  family = binomial
)
summary(modelo_contexto5)

#Probabilidades predichas
pred9_prob <- predict(modelo_contexto5, type = "response")

# Clasificación (0/1)
pred9_y <- ifelse(pred9_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred9_y)

# Accuracy
mean(pred9_y == d1$win_local)

####################################################################
####################################################################

#MODELO GRAL

modelo_general <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    duelos_ganados_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    pases_local +
    pases_al_U3_local +
    centros_local +
    centros_rematados_local +
    corners_local +
    faltas_recibidas_U3_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general)

#Probabilidades predichas
pred10_prob <- predict(modelo_general, type = "response")

# Clasificación (0/1)
pred10_y <- ifelse(pred10_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred10_y)

# Accuracy
mean(pred10_y == d1$win_local)

#sin duelos_ganados_local   pvalor = 0.516003    

modelo_general2 <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    pases_local +
    pases_al_U3_local +
    centros_local +
    centros_rematados_local +
    corners_local +
    faltas_recibidas_U3_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general2)

#sin faltas_recibidas_U3_local   pvalor = 0.413475    

modelo_general3 <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    big_chances_local +
    pases_local +
    pases_al_U3_local +
    centros_local +
    centros_rematados_local +
    corners_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general3)

#sin big_chances_local  pvalor = 0.277278    

modelo_general4 <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    pases_local +
    pases_al_U3_local +
    centros_local +
    centros_rematados_local +
    corners_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general4)

#sin pases_al_U3_local  pvalor = 0.276885    

modelo_general5 <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    pases_local +
    centros_local +
    centros_rematados_local +
    corners_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general5)

#sin corners_local pvalor = 0.288724    

modelo_general6 <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    pases_local +
    centros_local +
    centros_rematados_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_al_U3_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general6)

#sin pases_al_U3_concedidos_local pvalor = 0.153455    

modelo_general7 <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    pases_local +
    centros_local +
    centros_rematados_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general7)

#sin pases_local  pvalor = 0.112869    
#aunque ha empeorado el AIC

modelo_general8 <- glm(
  win_local ~
    #contexto
    pos_previa_local +
    pos_previa_visitante +
    diferencia_puntos_local +
    diff_descanso +
    #fisico
    faltas_recibidas_local +
    rojas_local +
    min_ventaja_numerica_local +
    #ofensivo
    tiros_local +
    tiros_puerta_local +
    xG_local +
    centros_local +
    centros_rematados_local +
    despejes_concedidos_local +
    despejes_local +
    #defensivo
    tiros_puerta_concedidos_local + 
    xGA_local + 
    corners_concedidos_local + 
    pases_largos_concedidos_local	+ 
    centros_concedidos_local	
  ,
  data = d1,
  family = binomial
)
summary(modelo_general8)

#Probabilidades predichas
pred11_prob <- predict(modelo_general8, type = "response")

# Clasificación (0/1)
pred11_y <- ifelse(pred11_prob >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred11_y)

# Accuracy
mean(pred11_y == d1$win_local)

# =========================
# 2. GRÁFICO DE COEFICIENTES (log-odds)
# =========================
coef_d1 <- tidy(modelo_general8, conf.int = TRUE)
coef_d1 <- coef_d1[coef_d1$term != "(Intercept)", ]

ggplot(coef_d1, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Coeficientes del Modelo Logístico – Victoria local",
    x = "Coeficiente (log-odds)",
    y = "Variables"
  ) +
  theme_minimal(base_size = 13)
