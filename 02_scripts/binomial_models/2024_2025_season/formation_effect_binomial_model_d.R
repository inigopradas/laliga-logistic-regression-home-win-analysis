# ==============================================================================
# Title: Formation effects on the probability of a home-team victory
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script examines the association between the tactical formations used by
# the home and visiting teams and the probability of a home-team victory.
# The analysis is conducted through a binomial logistic regression in which
# win_local is the dependent variable and the home and visiting formations are
# included as categorical explanatory variables.
#
# The script imports both the multi-season dataset and the single-season
# dataset. In its current version, however, the formation analysis is performed
# using the single-season dataset stored in the object d. The multi-season
# dataset stored in d1 is imported but is not subsequently used in the model.
#
# The preparation stage inspects the structure of the single-season dataset,
# converts the relevant variables into factors and removes internal quotation
# marks and unnecessary spaces from the formation labels. Formation categories
# with fewer than ten observations are grouped into a residual category named
# "Otras" to avoid estimating separate coefficients for tactical formations
# represented by very small numbers of matches.
#
# The 1-4-2-3-1 formation is used as the reference category for both the home
# and visiting teams. A binomial logistic regression without an interaction
# term is then estimated to evaluate the additive association of the two
# formations with the probability of a home victory.
#
# A custom Hessian function imported from logit_funciones.R is used to obtain
# the estimated variance-covariance matrix. This matrix is subsequently used
# to calculate the standard error, z statistic, bilateral p-value and 95%
# confidence interval for a selected linear combination of formation
# coefficients.
#
# The script then generates every observed combination of the grouped home and
# visiting formation levels. For each combination, it calculates the linear
# predictor, its standard error, a 95% confidence interval and the corresponding
# estimated probability of a home-team victory. The combinations are ordered
# according to their estimated probability.
#
# In-sample model performance is evaluated using predicted probabilities, a
# classification threshold of 0.50, a confusion matrix, accuracy, sensitivity,
# specificity, precision, a receiver operating characteristic curve and the
# area under the ROC curve.
#
# Finally, the dataset is randomly divided into an 80% training sample and a
# 20% test sample. The formation model is re-estimated using the training
# observations and evaluated on the test observations through a confusion
# matrix, accuracy, sensitivity, specificity, precision, a ROC curve and the
# corresponding area under the curve.
#
# Input files:
#   LaLiga_22-25_completo_v2 (2).xlsx
#   variables_Estudio (9).xlsx
#   logit_funciones.R
#
# Dataset used in the current formation analysis:
#   d, corresponding to the single-season dataset.
#
# Dataset imported but not used in the current analysis:
#   d1, corresponding to the multi-season dataset.
#
# Dependent variable:
#   win_local
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory, including draws and away-team victories
#
# Explanatory variables:
#   formacion_local_dep
#   formacion_visit_dep
#
# Reference formation:
#   1-4-2-3-1 for both the home and visiting teams.
#
# Treatment of infrequent formations:
#   Formation categories with fewer than ten observations are grouped into
#   the category "Otras".
#
# Statistical framework:
#   Binomial logistic regression without an interaction between the home and
#   visiting formations.
#
# Validation procedure:
#   Random 80% training and 20% test split using set.seed(123).
#
# Main objects created:
#   d1, d, freq_local, freq_visit, formacion_local_dep,
#   formacion_visit_dep, m_sin_interaccion, H, M, comb, Xnew,
#   resultados_comb, resultados_comb_orden, pred_prob, pred_y,
#   mc, metricas, roc_obj, d_train, d_test, m_train,
#   pred_prob_test, pred_y_test, mc_test, metricas_test and roc_test.
#
# Main outputs:
#   Formation-frequency tables, regression coefficients, statistical tests
#   for linear combinations of coefficients, predicted probabilities for all
#   formation combinations, confusion matrices, classification metrics, ROC
#   curves and AUC values for the full sample and the test sample.
#
# Important methodological note:
#   The performance measures calculated using the complete dataset describe
#   in-sample classification. The metrics obtained from the test sample provide
#   a more appropriate assessment of predictive performance, although results
#   may vary because they are based on a single random train-test partition.
# ==============================================================================

library(readxl)   # para leer Excel
library(dplyr)    # para manipular datos

getwd()
d1 <- read_excel("LaLiga_22-25_completo_v2 (2).xlsx")
d <- read_excel("variables_Estudio (9).xlsx")
#de aqui hay muchos valores que salen como NA porque no se recopilaron (de d1)
#voy a calcular el contraste de las formaciones

str(d)
head(d)
d$win_local = factor(d$win_local)
d$temporada = factor(d$temporada)
d$formacion_local = factor(d$formacion_local)
d$formacion_visit = factor(d$formacion_visit)

source("logit_funciones.R")


d$formacion_local = relevel(d$formacion_local, ref = "1-4-2-3-1")
d$formacion_visit = relevel(d$formacion_visit, ref = "1-4-2-3-1")
levels(d$formacion_local)


# Quitar comillas internas y espacios
d$formacion_local = gsub('"', '', trimws(as.character(d$formacion_local)))
d$formacion_visit = gsub('"', '', trimws(as.character(d$formacion_visit)))

# Convertir a factor
d$formacion_local = factor(d$formacion_local)
d$formacion_visit = factor(d$formacion_visit)

# Comprobar niveles
levels(d$formacion_local)
levels(d$formacion_visit)

#Modelo sin interaccion entre las formaciones


table(d$formacion_local)
table(d$formacion_visit)


# =========================================================
# FRECUENCIAS
# =========================================================
freq_local = table(d$formacion_local)
freq_visit = table(d$formacion_visit)

sort(freq_local)
sort(freq_visit)

# =========================================================
# AGRUPAR FORMACIONES RARAS EN "Otras"
# =========================================================
umbral = 10

# Crear copias en character para recodificar
d$formacion_local_dep = as.character(d$formacion_local)
d$formacion_visit_dep = as.character(d$formacion_visit)

# Reemplazar niveles raros por "Otras"
d$formacion_local_dep[d$formacion_local_dep %in% names(freq_local[freq_local < umbral])] = "Otras"
d$formacion_visit_dep[d$formacion_visit_dep %in% names(freq_visit[freq_visit < umbral])] = "Otras"

# Volver a factor
d$formacion_local_dep = factor(d$formacion_local_dep)
d$formacion_visit_dep = factor(d$formacion_visit_dep)

# Ver frecuencias nuevas
table(d$formacion_local_dep)
table(d$formacion_visit_dep)

# =========================================================
# REFERENCIAS de variables depuradas
# =========================================================
d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-2-3-1")
d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")

m_sin_interaccion = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                        data = d,
                        family = binomial)

summary(m_sin_interaccion)
names(coef(m_sin_interaccion))



# =========================================================
# HESSIANO Y MATRIZ DE VARIANZAS
# =========================================================
H = logit_hess(coef(m_sin_interaccion), model.matrix(m_sin_interaccion))
M = -solve(H)

# Ver nombres de coeficientes
names(coef(m_sin_interaccion))

# =========================================================
# EJEMPLO: sumar beta local 1-4-3-3 + beta visitante 1-4-4-2
# =========================================================
beta1_name = "formacion_local_dep1-4-3-3"
beta2_name = "formacion_visit_dep1-4-4-2"

# Posiciones
pos1 = which(names(coef(m_sin_interaccion)) == beta1_name)
pos2 = which(names(coef(m_sin_interaccion)) == beta2_name)

# Betas estimadas
beta1_e = coef(m_sin_interaccion)[pos1]
beta2_e = coef(m_sin_interaccion)[pos2]

beta1_e
beta2_e

# Error estándar de la suma
Se = sqrt(M[pos1, pos1] + M[pos2, pos2] + 2 * M[pos1, pos2])
Se

# Estadístico z
z = (beta1_e + beta2_e) / Se
z

# p-valor bilateral
p_valor = 2 * (1 - pnorm(abs(z)))
p_valor

# Intervalo de confianza
alfa = 0.05
LI = beta1_e + beta2_e - qnorm(1 - alfa/2) * Se
LS = beta1_e + beta2_e + qnorm(1 - alfa/2) * Se

IC = c(LI, LS)
IC


# =========================================================
# MATRIZ DE VARIANZAS-COVARIANZAS
# =========================================================
H = logit_hess(coef(m_sin_interaccion), model.matrix(m_sin_interaccion))
M = -solve(H)

# =========================================================
# TODAS LAS COMBINACIONES POSIBLES
# =========================================================
comb = expand.grid(
  formacion_local_dep = levels(d$formacion_local_dep),
  formacion_visit_dep = levels(d$formacion_visit_dep)
)

comb


# Matriz X de las nuevas combinaciones
TT = delete.response(terms(m_sin_interaccion))
Xnew = model.matrix(TT, data = comb)

# Coeficientes
b = coef(m_sin_interaccion)

# Predictor lineal eta = Xnew %*% b
eta = as.numeric(Xnew %*% b)

# Error estándar de cada eta:
# se_i = sqrt(x_i' M x_i)
se_eta = sqrt(diag(Xnew %*% M %*% t(Xnew)))

# Intervalos de confianza en escala logit
alfa = 0.05
z_alfa = qnorm(1 - alfa/2)

LI_eta = eta - z_alfa * se_eta
LS_eta = eta + z_alfa * se_eta

# Pasar a probabilidad
prob = plogis(eta)
LI_prob = plogis(LI_eta)
LS_prob = plogis(LS_eta)

# Tabla final
resultados_comb = data.frame(
  formacion_local = comb$formacion_local_dep,
  formacion_visit = comb$formacion_visit_dep,
  eta = eta,
  se_eta = se_eta,
  LI_eta = LI_eta,
  LS_eta = LS_eta,
  prob = prob,
  LI_prob = LI_prob,
  LS_prob = LS_prob
)

# Ver resultados
resultados_comb

resultados_comb_orden = resultados_comb[order(-resultados_comb$prob), ]
resultados_comb_orden


# =========================================================
# MATRIZ DE CONFUSION Y ACCURACY EN TODA LA MUESTRA
# =========================================================

# Probabilidades predichas
pred_prob = predict(m_sin_interaccion, newdata = d, type = "response")

# Clasificación binaria con umbral 0.5
pred_y = ifelse(pred_prob >= 0.5, 1, 0)

# Matriz de confusión
mc = table(Real = d$win_local, Predicho = pred_y)
mc

# Accuracy
accuracy = sum(diag(mc)) / sum(mc)
accuracy

# Sensibilidad (TPR): proporción de victorias locales reales bien detectadas
sensibilidad = mc["1", "1"] / sum(mc["1", ])
sensibilidad

# Especificidad (TNR): proporción de no-victorias locales reales bien detectadas
especificidad = mc["0", "0"] / sum(mc["0", ])
especificidad

# Precisión (PPV): de los partidos predichos como victoria local, cuántos lo fueron
precision = mc["1", "1"] / sum(mc[, "1"])
precision

# Mostrar todo junto
metricas = c(accuracy = accuracy,
             sensibilidad = sensibilidad,
             especificidad = especificidad,
             precision = precision)

metricas


install.packages("pROC")
library(pROC)

# =========================================================
# CURVA ROC Y AUC EN TODA LA MUESTRA
# =========================================================

roc_obj = roc(response = d$win_local, predictor = pred_prob)

# Mostrar AUC
auc_valor = auc(roc_obj)
auc_valor

# Dibujar curva ROC
plot(roc_obj, col = "blue", lwd = 2,
     main = "Curva ROC - Modelo logístico formaciones")
abline(a = 0, b = 1, lty = 2, col = "gray")



# =========================================================
# VALIDACION TRAIN / TEST
# =========================================================

set.seed(123)

n = nrow(d)

# 80% train, 20% test
pos_train = sample(1:n, size = round(0.8 * n), replace = FALSE)

d_train = d[pos_train, ]
d_test  = d[-pos_train, ]

# Importante:
# NO usar droplevels() aquí, para conservar los niveles del factor

# Ajustar modelo en train
m_train = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
              data = d_train,
              family = binomial)

summary(m_train)

# =========================================================
# PREDICCIONES EN TEST
# =========================================================

pred_prob_test = predict(m_train, newdata = d_test, type = "response")

# Clasificación con umbral 0.5
pred_y_test = ifelse(pred_prob_test >= 0.5, 1, 0)

# Matriz de confusión
mc_test = table(Real = d_test$win_local, Predicho = pred_y_test)
mc_test

# Accuracy test
accuracy_test = sum(diag(mc_test)) / sum(mc_test)
accuracy_test

# Sensibilidad test
sensibilidad_test = mc_test["1", "1"] / sum(mc_test["1", ])
sensibilidad_test

# Especificidad test
especificidad_test = mc_test["0", "0"] / sum(mc_test["0", ])
especificidad_test

# Precisión test
precision_test = mc_test["1", "1"] / sum(mc_test[, "1"])
precision_test

# Métricas juntas
metricas_test = c(accuracy = accuracy_test,
                  sensibilidad = sensibilidad_test,
                  especificidad = especificidad_test,
                  precision = precision_test)

metricas_test

# =========================================================
# ROC Y AUC EN TEST
# =========================================================

roc_test = roc(response = d_test$win_local, predictor = pred_prob_test)

auc_test = auc(roc_test)
auc_test

plot(roc_test, col = "red", lwd = 2,
     main = "Curva ROC - Validación Test")
abline(a = 0, b = 1, lty = 2, col = "gray")
