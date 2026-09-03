# ==============================================================================
# Title: Development and validation of multinomial logistic regression Model A
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops and evaluates a multinomial logistic regression model
# for match outcomes during the 2024/2025 LaLiga season.
#
# The dependent variable distinguishes between a home-team victory, a draw and
# a home-team defeat. The draw is used as the reference outcome, so the model
# estimates the log-odds of a home victory versus a draw and a home defeat
# versus a draw.
#
# The script imports the single-season dataset and prepares the variables used
# in the analysis. Data preparation includes cleaning tactical formations,
# converting categorical variables into factors, processing rest information,
# calculating the difference in rest days and preparing the recent-form
# variables.
#
# Model A includes home shots on target, expected goals, expected goals against,
# shots on target conceded, possession, rest difference, absence of previous
# rest information, previous league positions and recent form.
#
# The script extracts coefficient estimates, standard errors, Wald statistics,
# bilateral p-values and odds ratios. The results are organised into structured
# tables identifying coefficients with statistical evidence at the 5% and 10%
# significance levels.
#
# Global likelihood-ratio tests are used to evaluate the overall contribution
# of each explanatory variable. Predicted probabilities are calculated for the
# three possible match outcomes and converted into predicted outcome classes.
#
# In-sample classification performance is evaluated using a confusion matrix,
# overall accuracy, class-specific sensitivity, class-specific precision and
# balanced accuracy.
#
# The dataset is also divided into an 80% training sample and a 20% test sample
# using a reproducible random partition. Model A is re-estimated using the
# training sample and evaluated on the test sample through predicted
# probabilities, predicted classes, a confusion matrix, accuracy, balanced
# accuracy, sensitivity and precision.
#
# A final cleaned specification converts previous league positions into
# numerical ordinal variables before re-estimating the multinomial model.
#
# Input:
#   variables_Estudio (9).xlsx
#
# Additional input:
#   LaLiga_22-25_completo_v2 (2).xlsx
#   logit_funciones.R
#
# Dataset used for model estimation:
#   d, corresponding to the 2024/2025 LaLiga season.
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
# Main explanatory variables:
#   tiros_puerta_local
#   xG_local
#   xGA_local
#   tiros_puerta_concedidos_local
#   posesion_local
#   diff_descanso
#   no_descanso_previo
#   pos_previa_local
#   pos_previa_visitante
#   forma_local_5
#   forma_visitante_5
#
# Statistical method:
#   Multinomial logistic regression estimated with nnet::multinom().
#
# Validation procedure:
#   Random 80% training and 20% test division using set.seed(123).
#
# Main models:
#   modelo_multinom_A
#   modelo_multinom_train
#   modelo_multinom_A_limpio
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, global likelihood-ratio tests,
#   predicted probabilities, confusion matrices, overall accuracy, balanced
#   accuracy, class-specific sensitivity and class-specific precision for the
#   complete and test samples.
# ==============================================================================


# Instalar si no los tienes
install.packages("nnet")
install.packages("dplyr")
install.packages("caret")
install.packages("pscl")

# Cargar paquetes
library(nnet)
library(dplyr)
library(caret)
library(pscl)
library(readxl)   # para leer Excel

getwd()
d1 <- read_excel("LaLiga_22-25_completo_v2 (2).xlsx")
d <- read_excel("variables_Estudio (9).xlsx")
str(d)
head(d)
d$win_local = factor(d$win_local)
d$temporada = factor(d$temporada)
d$formacion_local = factor(d$formacion_local)
d$formacion_visit = factor(d$formacion_visit)

source("logit_funciones.R")


# Quitar comillas internas y espacios
d$formacion_local = gsub('"', '', trimws(as.character(d$formacion_local)))

# Convertir a factor
d$formacion_local = factor(d$formacion_local)

# Comprobar niveles
levels(d$formacion_local)

#Modelo sin interaccion entre las formaciones


table(d$formacion_local)

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

##########################################################

# Ver las primeras filas
head(d$resultado_partido_local)

# Ver categorías
table(d$resultado_partido_local)

# Ver porcentajes
prop.table(table(d$resultado_partido_local))

d$resultado_partido_local <- as.factor(d$resultado_partido_local)


# Comprobar niveles
levels(d$resultado_partido_local)


d$resultado_partido_local <- relevel(d$resultado_partido_local, ref = "Empate")

levels(d$resultado_partido_local)


modelo_multinom_A <- multinom(
  resultado_partido_local ~ 
    tiros_puerta_local +
    xG_local +
    xGA_local +
    tiros_puerta_concedidos_local +
    posesion_local +
    diff_descanso +
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d,
  trace = FALSE
)

summary(modelo_multinom_A)


# Resumen del modelo
resumen_A <- summary(modelo_multinom_A)

# Coeficientes
coeficientes_A <- resumen_A$coefficients

# Errores estándar
errores_A <- resumen_A$standard.errors

# Estadísticos z
z_A <- coeficientes_A / errores_A

# p-valores
p_values_A <- 2 * (1 - pnorm(abs(z_A)))

# Mostrar resultados
coeficientes_A
round(p_values_A, 4)


OR_A <- exp(coeficientes_A)

round(OR_A, 3)


# Convertir matrices a data frame largo
tabla_coef <- as.data.frame(as.table(coeficientes_A))
colnames(tabla_coef) <- c("Resultado", "Variable", "Coeficiente")

tabla_or <- as.data.frame(as.table(OR_A))
colnames(tabla_or) <- c("Resultado", "Variable", "Odds_Ratio")

tabla_p <- as.data.frame(as.table(p_values_A))
colnames(tabla_p) <- c("Resultado", "Variable", "P_valor")

# Unir todo
tabla_resultados_A <- tabla_coef %>%
  left_join(tabla_or, by = c("Resultado", "Variable")) %>%
  left_join(tabla_p, by = c("Resultado", "Variable")) %>%
  mutate(
    Coeficiente = round(Coeficiente, 4),
    Odds_Ratio = round(Odds_Ratio, 4),
    P_valor = round(P_valor, 4)
  )

tabla_resultados_A


probabilidades_A <- predict(modelo_multinom_A, type = "probs")

head(probabilidades_A)

rowSums(probabilidades_A)[1:10]
pred_clase_A <- predict(modelo_multinom_A, type = "class")

accuracy_A <- sum(diag(mc_A)) / sum(mc_A)
accuracy_A

###########################

set.seed(123)

n <- nrow(d)

pos_train <- sample(
  1:n,
  size = round(0.8 * n),
  replace = FALSE
)

d_train <- d[pos_train, ]
d_test  <- d[-pos_train, ]

prop.table(table(d_train$resultado_partido_local))
prop.table(table(d_test$resultado_partido_local))

modelo_multinom_train <- multinom(
  resultado_partido_local ~ 
    tiros_puerta_local +
    xG_local +
    xGA_local +
    tiros_puerta_concedidos_local +
    posesion_local +
    diff_descanso +
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d_train,
  trace = FALSE
)

summary(modelo_multinom_train)


pred_test <- predict(
  modelo_multinom_train,
  newdata = d_test,
  type = "class"
)

prob_test <- predict(
  modelo_multinom_train,
  newdata = d_test,
  type = "probs"
)

###########################################
# Resumen del modelo
resumen_A <- summary(modelo_multinom_A)

# Coeficientes y errores estándar
coeficientes_A <- resumen_A$coefficients
errores_A <- resumen_A$standard.errors

# Estadístico z
z_A <- coeficientes_A / errores_A

# p-valores
p_values_A <- 2 * (1 - pnorm(abs(z_A)))

# Odds ratios
OR_A <- exp(coeficientes_A)

# Ver resultados
round(coeficientes_A, 4)
round(OR_A, 4)
round(p_values_A, 4)
############################################################

tabla_coef <- as.data.frame(as.table(coeficientes_A))
colnames(tabla_coef) <- c("Resultado", "Variable", "Coeficiente")

tabla_or <- as.data.frame(as.table(OR_A))
colnames(tabla_or) <- c("Resultado", "Variable", "Odds_Ratio")

tabla_p <- as.data.frame(as.table(p_values_A))
colnames(tabla_p) <- c("Resultado", "Variable", "P_valor")

tabla_resultados_A <- tabla_coef %>%
  left_join(tabla_or, by = c("Resultado", "Variable")) %>%
  left_join(tabla_p, by = c("Resultado", "Variable")) %>%
  mutate(
    Coeficiente = round(Coeficiente, 4),
    Odds_Ratio = round(Odds_Ratio, 4),
    P_valor = round(P_valor, 4),
    Significativa_5 = ifelse(P_valor < 0.05, "Sí", "No"),
    Significativa_10 = ifelse(P_valor < 0.10, "Sí", "No")
  )

tabla_resultados_A
#significativas al 5%

tabla_resultados_A %>%
  filter(P_valor < 0.05)


test_lr_multinom <- function(modelo_completo) {
  
  vars <- attr(terms(modelo_completo), "term.labels")
  
  resultados <- data.frame(
    Variable = character(),
    LR_stat = numeric(),
    df = numeric(),
    p_value = numeric()
  )
  
  for (v in vars) {
    
    formula_reducida <- update(
      formula(modelo_completo),
      paste(". ~ . -", v)
    )
    
    modelo_reducido <- multinom(
      formula_reducida,
      data = modelo_completo$model,
      trace = FALSE
    )
    
    ll_completo <- as.numeric(logLik(modelo_completo))
    ll_reducido <- as.numeric(logLik(modelo_reducido))
    
    df_completo <- attr(logLik(modelo_completo), "df")
    df_reducido <- attr(logLik(modelo_reducido), "df")
    
    LR <- 2 * (ll_completo - ll_reducido)
    df_diff <- df_completo - df_reducido
    p_val <- pchisq(LR, df = df_diff, lower.tail = FALSE)
    
    resultados <- rbind(
      resultados,
      data.frame(
        Variable = v,
        LR_stat = LR,
        df = df_diff,
        p_value = p_val
      )
    )
  }
  
  resultados <- resultados %>%
    arrange(p_value) %>%
    mutate(
      LR_stat = round(LR_stat, 4),
      p_value = round(p_value, 4),
      Significativa_5 = ifelse(p_value < 0.05, "Sí", "No"),
      Significativa_10 = ifelse(p_value < 0.10, "Sí", "No")
    )
  
  return(resultados)
}

test_global_A <- test_lr_multinom(modelo_multinom_A)
test_global_A
#probabilidades de que sea V, E, D
View(as.data.frame(probabilidades_A))


  #=========================================================
  # MATRIZ DE CONFUSIÓN EN TODA LA MUESTRA
  # =========================================================

# Predicción de clase
pred_clase_A <- predict(
  modelo_multinom_A,
  newdata = d,
  type = "class"
)

# Asegurar que real y predicho tienen los mismos niveles
niveles_resultado <- levels(d$resultado_partido_local)

real_A <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_clase_A <- factor(
  pred_clase_A,
  levels = niveles_resultado
)

# Matriz de confusión
mc_A <- table(
  Real = real_A,
  Predicho = pred_clase_A
)

mc_A

# Matriz con totales
addmargins(mc_A)


accuracy_A <- sum(diag(mc_A)) / sum(mc_A)

sensibilidad_A <- diag(mc_A) / rowSums(mc_A)

precision_A <- diag(mc_A) / colSums(mc_A)
precision_A[is.nan(precision_A)] <- NA

balanced_accuracy_A <- mean(sensibilidad_A, na.rm = TRUE)

metricas_clase_A <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(as.numeric(sensibilidad_A), 4),
  Precision = round(as.numeric(precision_A), 4)
)

accuracy_A
balanced_accuracy_A
metricas_clase_A

# =========================================================
# MATRIZ DE CONFUSIÓN EN TEST
# =========================================================

# Predicción sobre test
pred_test <- predict(
  modelo_multinom_train,
  newdata = d_test,
  type = "class"
)

# Niveles del resultado
niveles_resultado <- levels(d$resultado_partido_local)

# Asegurar mismos niveles
real_test <- factor(
  d_test$resultado_partido_local,
  levels = niveles_resultado
)

pred_test <- factor(
  pred_test,
  levels = niveles_resultado
)

# Matriz de confusión
mc_test <- table(
  Real = real_test,
  Predicho = pred_test
)

mc_test

# Matriz con totales
addmargins(mc_test)

#metricas en test
# Accuracy test
accuracy_test <- sum(diag(mc_test)) / sum(mc_test)
accuracy_test

# Sensibilidad / recall por categoría
sensibilidad_test <- diag(mc_test) / rowSums(mc_test)
sensibilidad_test

# Precisión por categoría
precision_test <- diag(mc_test) / colSums(mc_test)
precision_test

# Evitar NaN si alguna categoría no se predice nunca
precision_test[is.nan(precision_test)] <- NA
precision_test

# Balanced accuracy
balanced_accuracy_test <- mean(sensibilidad_test, na.rm = TRUE)
balanced_accuracy_test

metricas_clase_test <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(as.numeric(sensibilidad_test), 4),
  Precision = round(as.numeric(precision_test), 4)
)

metricas_clase_test

#####################################################
#####################################################
#####################################################
#####################################################

d$pos_previa_local <- as.numeric(as.character(d$pos_previa_local))
d$pos_previa_visitante <- as.numeric(as.character(d$pos_previa_visitante))

modelo_multinom_A_limpio <- multinom(
  resultado_partido_local ~ 
    tiros_puerta_local +
    xG_local +
    xGA_local +
    tiros_puerta_concedidos_local +
    posesion_local +
    diff_descanso +
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d,
  trace = FALSE
)

resumen_limpio <- summary(modelo_multinom_A_limpio)

coef_limpio <- resumen_limpio$coefficients
err_limpio <- resumen_limpio$standard.errors

z_limpio <- coef_limpio / err_limpio
p_limpio <- 2 * (1 - pnorm(abs(z_limpio)))
OR_limpio <- exp(coef_limpio)

round(OR_limpio, 3)
round(p_limpio, 4)

pred_limpio <- predict(modelo_multinom_A_limpio, newdata = d, type = "class")
pred_limpio
