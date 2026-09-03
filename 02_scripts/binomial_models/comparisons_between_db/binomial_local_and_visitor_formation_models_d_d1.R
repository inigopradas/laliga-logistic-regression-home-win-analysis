# ==============================================================================
# Title: Binary logistic models for home and visiting formations
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script analyses the association between tactical formations and the
# probability of a home-team victory. The analysis compares the 2024/2025
# season with the combined sample covering the 2022/2023, 2023/2024 and
# 2024/2025 LaLiga seasons.
#
# The script imports the single-season dataset d and the multi-season dataset
# d1. The binary dependent variable identifies whether the home team won the
# match, while draws and away-team victories are grouped into the alternative
# outcome of no home-team victory.
#
# Formation labels are cleaned by removing internal quotation marks and
# unnecessary spaces. The frequencies of the home and visiting formations are
# then calculated separately. Formation categories represented by fewer than
# ten matches are grouped into a residual category named "Otras".
#
# Separate binary logistic regression models are estimated for the home
# formation and the visiting formation. The models are fitted independently
# for the single-season and multi-season datasets.
#
# The reference category is changed repeatedly to obtain direct comparisons
# between the different tactical formations. The resulting model summaries
# provide the coefficients, standard errors, z statistics and p-values for the
# relevant formation contrasts.
#
# The 1-4-2-3-1 formation is used as the principal reference category. The
# script also estimates equivalent specifications using the remaining observed
# formations and the grouped "Otras" category as alternative references.
#
# Predicted probabilities are converted into binary classifications using a
# threshold of 0.50. Confusion matrices and in-sample accuracy are calculated
# for the principal home-formation and visiting-formation models.
#
# The script subsequently creates standardised versions of the dependent
# variable and the grouped formation variables. Separate functions are defined
# to extract odds ratios, 95% confidence intervals, p-values and formation
# frequencies from the home-only and visitor-only models.
#
# The final results include the reference formation as an explicit category
# with an odds ratio equal to one. Formation estimates are classified according
# to whether their confidence intervals exclude one and according to the number
# of matches available for each tactical formation.
#
# Forest plots are generated separately for home and visiting formations. The
# plots display adjusted odds ratios, 95% confidence intervals, statistical
# differences from the reference category and the marginal sample size
# associated with each formation.
#
# Datasets:
#   d  = match-level data from the 2024/2025 season
#   d1 = match-level data from the 2022/2023 to 2024/2025 seasons
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
# Principal reference formation:
#   1-4-2-3-1
#
# Treatment of infrequent formations:
#   Formation categories represented by fewer than ten matches are grouped
#   into the category "Otras".
#
# Statistical method:
#   Binary logistic regression estimated with glm() and family = binomial.
#
# Main single-season home-formation models:
#   m_4231, m_343, m_4141, m_433, m_442, m_541 and m_Otras
#
# Main multi-season home-formation models:
#   m1_4231, m1_3421, m1_343, m1_352, m1_4132, m1_4141, m1_4312,
#   m1_433, m1_4411, m1_451, m1_532, m1_541 and m1_Otras
#
# Main single-season visiting-formation models:
#   m2_343, m2_4141, m2_4231, m2_433, m2_442, m2_532, m2_541 and
#   m2_Otras
#
# Main multi-season visiting-formation models:
#   m3_3421, m3_343, m3_352, m3_4132, m3_4141, m3_4231, m3_4312,
#   m3_433, m3_4411, m3_442, m3_451, m3_532, m3_541 and m3_Otras
#
# Final simple models:
#   modelo_solo_local_d
#   modelo_solo_visitante_d
#
# Main function:
#   extraer_or_modelo_simple()
#
# Main outputs:
#   Formation-frequency tables, logistic regression summaries, predicted
#   probabilities, confusion matrices, in-sample accuracy, odds ratios,
#   95% confidence intervals and separate forest plots for home and visiting
#   formations.
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


# Quitar comillas internas y espacios
d$formacion_local = gsub('"', '', trimws(as.character(d$formacion_local)))

# Convertir a factor
d$formacion_local = factor(d$formacion_local)

# Comprobar niveles
levels(d$formacion_local)

#Modelo sin interaccion entre las formaciones


table(d$formacion_local)


# =========================================================
# FRECUENCIAS
# =========================================================
freq_local = table(d$formacion_local)

sort(freq_local)

# =========================================================
# AGRUPAR FORMACIONES RARAS EN "Otras"
# =========================================================
umbral = 10

# Crear copias en character para recodificar
d$formacion_local_dep = as.character(d$formacion_local)

# Reemplazar niveles raros por "Otras"
d$formacion_local_dep[d$formacion_local_dep %in% names(freq_local[freq_local < umbral])] = "Otras"

# Volver a factor
d$formacion_local_dep = factor(d$formacion_local_dep)

# Ver frecuencias nuevas
table(d$formacion_local_dep)

# =========================================================
# REFERENCIAS de variables depuradas
# =========================================================
d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-2-3-1")

m_4231 = glm(win_local ~ formacion_local_dep,
                        data = d,
                        family = binomial)

summary(m_4231)

# Probabilidades predichas
pred_prob_1_a_1 <- predict(m_4231, type = "response")

# Clasificación (0/1)
pred_prob_1_a_1 <- ifelse(pred_prob_1_a_1 >= 0.5, 1, 0)

# Matriz de confusión
table(real = d$win_local, pred = pred_prob_1_a_1)

# Accuracy
mean(pred_prob_1_a_1 == d$win_local)

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-3-4-3")
m_343 = glm(win_local ~ formacion_local_dep,
             data = d,
             family = binomial)

summary(m_343)

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-1-4-1")
m_4141 = glm(win_local ~ formacion_local_dep,
            data = d,
            family = binomial)

summary(m_4141)

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-3-3")
m_433 = glm(win_local ~ formacion_local_dep,
             data = d,
             family = binomial)

summary(m_433)

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-4-2")
m_442 = glm(win_local ~ formacion_local_dep,
            data = d,
            family = binomial)

summary(m_442)

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-5-4-1")
m_541= glm(win_local ~ formacion_local_dep,
            data = d,
            family = binomial)

summary(m_541)

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "Otras")
m_Otras= glm(win_local ~ formacion_local_dep,
           data = d,
           family = binomial)

summary(m_Otras)

###############################################################################

str(d1)
head(d1)
d1$win_local = factor(d1$win_local)
d1$temporada = factor(d1$temporada)
d1$formacion_local = factor(d1$formacion_local)
d1$formacion_visit = factor(d1$formacion_visit)

source("logit_funciones.R")


# Quitar comillas internas y espacios
d1$formacion_local = gsub('"', '', trimws(as.character(d1$formacion_local)))

# Convertir a factor
d1$formacion_local = factor(d1$formacion_local)

# Comprobar niveles
levels(d1$formacion_local)

#Modelo sin interaccion entre las formaciones


table(d1$formacion_local)


# =========================================================
# FRECUENCIAS
# =========================================================
freq1_local = table(d1$formacion_local)

sort(freq1_local)

# =========================================================
# AGRUPAR FORMACIONES RARAS EN "Otras"
# =========================================================
umbral1 = 10

# Crear copias en character para recodificar
d1$formacion_local_dep = as.character(d1$formacion_local)

# Reemplazar niveles raros por "Otras"
d1$formacion_local_dep[d1$formacion_local_dep %in% names(freq1_local[freq1_local < umbral1])] = "Otras"

# Volver a factor
d1$formacion_local_dep = factor(d1$formacion_local_dep)

# Ver frecuencias nuevas
table(d1$formacion_local_dep)

# =========================================================
# REFERENCIAS de variables depuradas
# =========================================================

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-2-3-1")

m1_4231 = glm(win_local ~ formacion_local_dep,
             data = d1,
             family = binomial)

summary(m1_4231)

# Probabilidades predichas
pred_prob_1_a_1_vt <- predict(m1_4231, type = "response")

# Clasificación (0/1)
pred_prob_1_a_1_vt <- ifelse(pred_prob_1_a_1_vt >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred_prob_1_a_1_vt)

# Accuracy
mean(pred_prob_1_a_1_vt == d1$win_local)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-3-4-2-1")

m1_3421 = glm(win_local ~ formacion_local_dep,
              data = d1,
              family = binomial)

summary(m1_3421)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-3-4-3")

m1_343 = glm(win_local ~ formacion_local_dep,
              data = d1,
              family = binomial)

summary(m1_343)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-3-5-2")

m1_352 = glm(win_local ~ formacion_local_dep,
             data = d1,
             family = binomial)

summary(m1_352)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-1-3-2")

m1_4132 = glm(win_local ~ formacion_local_dep,
             data = d1,
             family = binomial)

summary(m1_4132)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-1-4-1")

m1_4141 = glm(win_local ~ formacion_local_dep,
              data = d1,
              family = binomial)

summary(m1_4141)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-3-1-2")

m1_4312 = glm(win_local ~ formacion_local_dep,
              data = d1,
              family = binomial)

summary(m1_4312)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-3-3")

m1_433 = glm(win_local ~ formacion_local_dep,
              data = d1,
              family = binomial)

summary(m1_433)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-4-1-1")

m1_4411 = glm(win_local ~ formacion_local_dep,
             data = d1,
             family = binomial)

summary(m1_4411)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-4-5-1")

m1_451 = glm(win_local ~ formacion_local_dep,
              data = d1,
              family = binomial)

summary(m1_451)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-5-3-2")

m1_532 = glm(win_local ~ formacion_local_dep,
             data = d1,
             family = binomial)

summary(m1_532)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "1-5-4-1")

m1_541 = glm(win_local ~ formacion_local_dep,
             data = d1,
             family = binomial)

summary(m1_541)

d1$formacion_local_dep = relevel(d1$formacion_local_dep, ref = "Otras")

m1_Otras = glm(win_local ~ formacion_local_dep,
             data = d1,
             family = binomial)

summary(m1_Otras)

##############################################################################
##############################################################################
##############################################################################
##############################################################################

# Quitar comillas internas y espacios
d$formacion_visit = gsub('"', '', trimws(as.character(d$formacion_visit)))

# Convertir a factor
d$formacion_visit = factor(d$formacion_visit)

# Comprobar niveles
levels(d$formacion_visit)

#Modelo sin interaccion entre las formaciones


table(d$formacion_visit)


# =========================================================
# FRECUENCIAS
# =========================================================
freq_visit = table(d$formacion_visit)

sort(freq_visit)

# =========================================================
# AGRUPAR FORMACIONES RARAS EN "Otras"
# =========================================================
# Crear copias en character para recodificar
d$formacion_visit_dep = as.character(d$formacion_visit)

# Reemplazar niveles raros por "Otras"
d$formacion_visit_dep[d$formacion_visit_dep %in% names(freq_visit[freq_visit < umbral])] = "Otras"

# Volver a factor
d$formacion_visit_dep = factor(d$formacion_visit_dep)

# Ver frecuencias nuevas
table(d$formacion_visit_dep)

# =========================================================
# REFERENCIAS de variables depuradas
# =========================================================

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m2_343 = glm(win_local ~ formacion_visit_dep,
                   data = d,
                   family = binomial)

summary(m2_343)

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m2_4141 = glm(win_local ~ formacion_visit_dep,
             data = d,
             family = binomial)

summary(m2_4141)

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")
m2_4231 = glm(win_local ~ formacion_visit_dep,
              data = d,
              family = binomial)

summary(m2_4231)

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m2_433 = glm(win_local ~ formacion_visit_dep,
              data = d,
              family = binomial)

summary(m2_433)

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m2_442 = glm(win_local ~ formacion_visit_dep,
             data = d,
             family = binomial)

summary(m2_442)

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m2_532 = glm(win_local ~ formacion_visit_dep,
             data = d,
             family = binomial)

summary(m2_532)

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m2_541 = glm(win_local ~ formacion_visit_dep,
             data = d,
             family = binomial)

summary(m2_541)

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m2_Otras = glm(win_local ~ formacion_visit_dep,
             data = d,
             family = binomial)

summary(m2_Otras)

#########################################################################

# Quitar comillas internas y espacios
d1$formacion_visit = gsub('"', '', trimws(as.character(d1$formacion_visit)))

# Convertir a factor
d1$formacion_visit = factor(d1$formacion_visit)

# Comprobar niveles
levels(d1$formacion_visit)

#Modelo sin interaccion entre las formaciones


table(d1$formacion_visit)


# =========================================================
# FRECUENCIAS
# =========================================================
freq1_visit = table(d1$formacion_visit)

sort(freq1_visit)

# =========================================================
# AGRUPAR FORMACIONES RARAS EN "Otras"
# =========================================================
# Crear copias en character para recodificar
d1$formacion_visit_dep = as.character(d1$formacion_visit)

# Reemplazar niveles raros por "Otras"
d1$formacion_visit_dep[d1$formacion_visit_dep %in% names(freq1_visit[freq1_visit < umbral1])] = "Otras"

# Volver a factor
d1$formacion_visit_dep = factor(d1$formacion_visit_dep)

# Ver frecuencias nuevas
table(d1$formacion_visit_dep)

# =========================================================
# REFERENCIAS de variables depuradas
# =========================================================


d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-2-1")
m3_3421 = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_3421)


d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-4-3")
m3_343 = glm(win_local ~ formacion_visit_dep,
              data = d1,
              family = binomial)

summary(m3_343)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-3-5-2")
m3_352 = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_352)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-3-2")
m3_4132 = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_4132)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-1-4-1")
m3_4141 = glm(win_local ~ formacion_visit_dep,
              data = d1,
              family = binomial)

summary(m3_4141)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-2-3-1")
m3_4231 = glm(win_local ~ formacion_visit_dep,
              data = d1,
              family = binomial)

summary(m3_4231)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-1-2")
m3_4312 = glm(win_local ~ formacion_visit_dep,
              data = d1,
              family = binomial)

summary(m3_4312)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-3-3")
m3_433 = glm(win_local ~ formacion_visit_dep,
              data = d1,
              family = binomial)

summary(m3_433)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-1-1")
m3_4411 = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_4411)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-4-2")
m3_442 = glm(win_local ~ formacion_visit_dep,
              data = d1,
              family = binomial)

summary(m3_442)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-4-5-1")
m3_451 = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_451)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-3-2")
m3_532 = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_532)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "1-5-4-1")
m3_541 = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_541)

d1$formacion_visit_dep = relevel(d1$formacion_visit_dep, ref = "Otras")
m3_Otras = glm(win_local ~ formacion_visit_dep,
             data = d1,
             family = binomial)

summary(m3_Otras)


# Probabilidades predichas
pred_prob_1_a_1_vt_visit <- predict(m3_4231, type = "response")

# Clasificación (0/1)
pred_prob_1_a_1_vt_visit <- ifelse(pred_prob_1_a_1_vt_visit >= 0.5, 1, 0)

# Matriz de confusión
table(real = d1$win_local, pred = pred_prob_1_a_1_vt_visit)

# Accuracy
mean(pred_prob_1_a_1_vt == d1$win_local)




########################################################3
# =========================================================
# 1. PAQUETES
# =========================================================

library(dplyr)
library(ggplot2)
library(readxl)
# =========================================================
# 2. PREPARAR VARIABLE DEPENDIENTE
# =========================================================

# Comprobar valores originales
table(
  d$win_local,
  useNA = "ifany"
)

# Convertir de forma segura a 0/1
win_texto <- trimws(
  as.character(d$win_local)
)

if (!all(na.omit(win_texto) %in% c("0", "1"))) {
  stop(
    "win_local debe contener únicamente los valores 0, 1 o NA. ",
    "Valores encontrados: ",
    paste(
      unique(na.omit(win_texto)),
      collapse = ", "
    )
  )
}

d$win_local_num <- as.integer(
  win_texto
)

table(
  d$win_local_num,
  useNA = "ifany"
)
# =========================================================
# 3. LIMPIAR FORMACIONES
# =========================================================

# Comprobar columnas originales
stopifnot(
  "formacion_local" %in% names(d),
  "formacion_visit" %in% names(d)
)

# Limpiar formación local
d$formacion_local <- gsub(
  '"',
  '',
  trimws(
    as.character(d$formacion_local)
  )
)

# Limpiar formación visitante
d$formacion_visit <- gsub(
  '"',
  '',
  trimws(
    as.character(d$formacion_visit)
  )
)

# Convertir cadenas vacías en NA
d$formacion_local[
  d$formacion_local == ""
] <- NA_character_

d$formacion_visit[
  d$formacion_visit == ""
] <- NA_character_
# =========================================================
# 4. AGRUPAR FORMACIONES POCO FRECUENTES
# =========================================================

umbral <- 10

# Frecuencias originales
freq_local <- table(
  d$formacion_local,
  useNA = "no"
)

freq_visit <- table(
  d$formacion_visit,
  useNA = "no"
)

# Formaciones con frecuencia inferior al umbral
raras_local <- names(
  freq_local[
    freq_local < umbral
  ]
)

raras_visit <- names(
  freq_visit[
    freq_visit < umbral
  ]
)

# Crear formación local depurada
d$formacion_local_dep <- ifelse(
  is.na(d$formacion_local),
  NA_character_,
  ifelse(
    d$formacion_local %in% raras_local,
    "Otras",
    d$formacion_local
  )
)

# Crear formación visitante depurada
d$formacion_visit_dep <- ifelse(
  is.na(d$formacion_visit),
  NA_character_,
  ifelse(
    d$formacion_visit %in% raras_visit,
    "Otras",
    d$formacion_visit
  )
)

# Convertir a factor
d$formacion_local_dep <- droplevels(
  factor(d$formacion_local_dep)
)

d$formacion_visit_dep <- droplevels(
  factor(d$formacion_visit_dep)
)

# Comprobar frecuencias finales
table(
  d$formacion_local_dep,
  useNA = "ifany"
)

table(
  d$formacion_visit_dep,
  useNA = "ifany"
)
# =========================================================
# 5. ESTABLECER REFERENCIAS
# =========================================================

referencia <- "1-4-2-3-1"

if (
  !referencia %in%
  levels(d$formacion_local_dep)
) {
  stop(
    "La referencia 1-4-2-3-1 no existe en ",
    "formacion_local_dep."
  )
}

if (
  !referencia %in%
  levels(d$formacion_visit_dep)
) {
  stop(
    "La referencia 1-4-2-3-1 no existe en ",
    "formacion_visit_dep."
  )
}

d$formacion_local_dep <- relevel(
  d$formacion_local_dep,
  ref = referencia
)

d$formacion_visit_dep <- relevel(
  d$formacion_visit_dep,
  ref = referencia
)

levels(d$formacion_local_dep)
levels(d$formacion_visit_dep)
# =========================================================
# 6. MODELO DE FORMACIONES LOCALES
# =========================================================

modelo_solo_local_d <- glm(
  win_local_num ~ formacion_local_dep,
  data = d,
  family = binomial,
  na.action = na.exclude
)

summary(
  modelo_solo_local_d
)
# =========================================================
# 7. MODELO DE FORMACIONES VISITANTES
# =========================================================

modelo_solo_visitante_d <- glm(
  win_local_num ~ formacion_visit_dep,
  data = d,
  family = binomial,
  na.action = na.exclude
)

summary(
  modelo_solo_visitante_d
)
# =========================================================
# 8. FUNCIÓN PARA EXTRAER OR DE UN MODELO
# =========================================================

extraer_or_modelo_simple <- function(
    modelo,
    datos,
    variable,
    equipo,
    referencia = "1-4-2-3-1"
) {
  
  # Extraer coeficientes
  matriz <- summary(
    modelo
  )$coefficients
  
  tabla <- data.frame(
    termino = rownames(matriz),
    beta = matriz[, "Estimate"],
    error = matriz[, "Std. Error"],
    valor_z = matriz[, "z value"],
    p = matriz[, "Pr(>|z|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  # Eliminar el intercepto
  tabla <- tabla %>%
    filter(
      termino != "(Intercept)"
    )
  
  # Extraer el nombre de la formación
  prefijo <- paste0(
    "^",
    variable
  )
  
  tabla <- tabla %>%
    mutate(
      Formacion = sub(
        prefijo,
        "",
        termino
      ),
      
      OR = exp(beta),
      
      OR_inf = exp(
        beta - 1.96 * error
      ),
      
      OR_sup = exp(
        beta + 1.96 * error
      ),
      
      Equipo = equipo,
      
      es_referencia = FALSE
    )
  
  # Frecuencias de la variable
  frecuencias <- datos %>%
    filter(
      !is.na(
        .data[[variable]]
      ),
      !is.na(win_local_num)
    ) %>%
    count(
      Formacion = .data[[variable]],
      name = "n"
    ) %>%
    mutate(
      Formacion = as.character(
        Formacion
      )
    )
  
  tabla <- tabla %>%
    left_join(
      frecuencias,
      by = "Formacion"
    )
  
  # Frecuencia de la referencia
  n_referencia <- frecuencias %>%
    filter(
      Formacion == referencia
    ) %>%
    pull(n)
  
  if (length(n_referencia) == 0) {
    n_referencia <- NA_integer_
  } else {
    n_referencia <- as.integer(
      n_referencia[1]
    )
  }
  
  # Crear fila de referencia
  fila_referencia <- data.frame(
    termino = "Referencia",
    beta = 0,
    error = 0,
    valor_z = NA_real_,
    p = NA_real_,
    Formacion = referencia,
    OR = 1,
    OR_inf = 1,
    OR_sup = 1,
    Equipo = equipo,
    es_referencia = TRUE,
    n = n_referencia,
    stringsAsFactors = FALSE
  )
  
  # Unir y clasificar
  resultado <- bind_rows(
    tabla,
    fila_referencia
  ) %>%
    mutate(
      diferencia_significativa = case_when(
        es_referencia ~
          "Referencia",
        
        OR_inf > 1 |
          OR_sup < 1 ~
          "Sí",
        
        TRUE ~
          "No"
      ),
      
      respaldo_muestral = case_when(
        es_referencia ~
          "Referencia",
        
        is.na(n) ~
          "Frecuencia desconocida",
        
        n < 20 ~
          "Muestra reducida",
        
        n < 50 ~
          "Muestra moderada",
        
        TRUE ~
          "Mayor respaldo muestral"
      ),
      
      etiqueta = paste0(
        Formacion,
        " (n = ",
        ifelse(
          is.na(n),
          "NA",
          as.character(n)
        ),
        ")"
      ),
      
      OR_IC95 = case_when(
        es_referencia ~
          "1.00 (referencia)",
        
        TRUE ~ paste0(
          sprintf(
            "%.2f",
            OR
          ),
          " [",
          sprintf(
            "%.2f",
            OR_inf
          ),
          ", ",
          sprintf(
            "%.2f",
            OR_sup
          ),
          "]"
        )
      )
    ) %>%
    arrange(
      OR
    )
  
  return(resultado)
}
# =========================================================
# 9. RESULTADOS DE FORMACIONES LOCALES
# =========================================================

or_local_d <- extraer_or_modelo_simple(
  modelo = modelo_solo_local_d,
  datos = d,
  variable = "formacion_local_dep",
  equipo = "Local",
  referencia = referencia
)

print(
  tibble::as_tibble(or_local_d),
  n = Inf,
  width = Inf
)
# =========================================================
# 10. RESULTADOS DE FORMACIONES VISITANTES
# =========================================================

or_visitante_d <- extraer_or_modelo_simple(
  modelo = modelo_solo_visitante_d,
  datos = d,
  variable = "formacion_visit_dep",
  equipo = "Visitante",
  referencia = referencia
)

print(
  tibble::as_tibble(or_visitante_d),
  n = Inf,
  width = Inf
)
# =========================================================
# 11. ORDENAR FORMACIONES LOCALES
# =========================================================

orden_local <- or_local_d %>%
  arrange(OR) %>%
  pull(etiqueta)

or_local_d <- or_local_d %>%
  mutate(
    etiqueta = factor(
      etiqueta,
      levels = unique(
        orden_local
      )
    )
  )
# =========================================================
# 12. ORDENAR FORMACIONES VISITANTES
# =========================================================

orden_visitante <- or_visitante_d %>%
  arrange(OR) %>%
  pull(etiqueta)

or_visitante_d <- or_visitante_d %>%
  mutate(
    etiqueta = factor(
      etiqueta,
      levels = unique(
        orden_visitante
      )
    )
  )
# =========================================================
# 13. LÍMITES COMUNES DEL EJE X
# =========================================================

todos_los_limites <- c(
  or_local_d$OR_inf,
  or_local_d$OR_sup,
  or_visitante_d$OR_inf,
  or_visitante_d$OR_sup
)

todos_los_limites <- todos_los_limites[
  is.finite(todos_los_limites) &
    todos_los_limites > 0
]

limite_inferior <- min(
  todos_los_limites,
  na.rm = TRUE
) / 1.15

limite_superior <- max(
  todos_los_limites,
  na.rm = TRUE
) * 1.15

if (
  !is.finite(limite_inferior) ||
  limite_inferior <= 0
) {
  limite_inferior <- 0.05
}

if (
  !is.finite(limite_superior) ||
  limite_superior <= 1
) {
  limite_superior <- 10
}

cat(
  "Límites del eje:",
  limite_inferior,
  "a",
  limite_superior,
  "\n"
)
# =========================================================
# 14. FOREST PLOT DE FORMACIONES LOCALES
# =========================================================

grafico_OR_local_d <- ggplot(
  or_local_d,
  aes(
    x = OR,
    y = etiqueta
  )
) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey45"
  ) +
  
  geom_errorbar(
    data = or_local_d %>%
      filter(
        !es_referencia
      ),
    aes(
      xmin = OR_inf,
      xmax = OR_sup,
      color = diferencia_significativa
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.75
  ) +
  
  geom_point(
    aes(
      color = diferencia_significativa,
      shape = respaldo_muestral
    ),
    size = 3.6,
    stroke = 0.9
  ) +
  
  scale_x_log10(
    limits = c(
      limite_inferior,
      limite_superior
    ),
    breaks = c(
      0.05,
      0.1,
      0.2,
      0.5,
      1,
      2,
      5,
      10,
      20
    )
  ) +
  
  scale_color_manual(
    values = c(
      "No" = "#666666",
      "Sí" = "#C51B29",
      "Referencia" = "#555555"
    ),
    drop = FALSE
  ) +
  
  scale_shape_manual(
    values = c(
      "Muestra reducida" = 1,
      "Muestra moderada" = 16,
      "Mayor respaldo muestral" = 17,
      "Referencia" = 15,
      "Frecuencia desconocida" = 4
    ),
    drop = FALSE
  ) +
  
  labs(
    title = "Efecto estimado de la formación local",
    subtitle = paste0(
      "Odds ratios e intervalos de confianza del 95% | ",
      "Base d"
    ),
    x = "Odds ratio de victoria local, escala logarítmica",
    y = "Formación local",
    color = "Diferencia significativa",
    shape = "Respaldo muestral",
    caption = paste0(
      "Categoría de referencia: 1-4-2-3-1. ",
      "La línea vertical indica OR = 1. ",
      "Modelo estimado únicamente con la formación local."
    )
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    legend.position = "bottom",
    legend.box = "vertical",
    
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    plot.caption = element_text(
      hjust = 0,
      size = 8.5
    ),
    
    axis.text.y = element_text(
      size = 10
    )
  )

grafico_OR_local_d
# =========================================================
# 15. FOREST PLOT DE FORMACIONES VISITANTES
# =========================================================

grafico_OR_visitante_d <- ggplot(
  or_visitante_d,
  aes(
    x = OR,
    y = etiqueta
  )
) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey45"
  ) +
  
  geom_errorbar(
    data = or_visitante_d %>%
      filter(
        !es_referencia
      ),
    aes(
      xmin = OR_inf,
      xmax = OR_sup,
      color = diferencia_significativa
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.75
  ) +
  
  geom_point(
    aes(
      color = diferencia_significativa,
      shape = respaldo_muestral
    ),
    size = 3.6,
    stroke = 0.9
  ) +
  
  scale_x_log10(
    limits = c(
      limite_inferior,
      limite_superior
    ),
    breaks = c(
      0.05,
      0.1,
      0.2,
      0.5,
      1,
      2,
      5,
      10,
      20
    )
  ) +
  
  scale_color_manual(
    values = c(
      "No" = "#666666",
      "Sí" = "#C51B29",
      "Referencia" = "#555555"
    ),
    drop = FALSE
  ) +
  
  scale_shape_manual(
    values = c(
      "Muestra reducida" = 1,
      "Muestra moderada" = 16,
      "Mayor respaldo muestral" = 17,
      "Referencia" = 15,
      "Frecuencia desconocida" = 4
    ),
    drop = FALSE
  ) +
  
  labs(
    title = "Efecto estimado de la formación visitante",
    subtitle = paste0(
      "Odds ratios e intervalos de confianza del 95% | ",
      "Base d"
    ),
    x = "Odds ratio de victoria local, escala logarítmica",
    y = "Formación visitante",
    color = "Diferencia significativa",
    shape = "Respaldo muestral",
    caption = paste0(
      "Categoría de referencia: 1-4-2-3-1. ",
      "La línea vertical indica OR = 1. ",
      "Modelo estimado únicamente con la formación visitante."
    )
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    legend.position = "bottom",
    legend.box = "vertical",
    
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    plot.caption = element_text(
      hjust = 0,
      size = 8.5
    ),
    
    axis.text.y = element_text(
      size = 10
    )
  )

grafico_OR_visitante_d
