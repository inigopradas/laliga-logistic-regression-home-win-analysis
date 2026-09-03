# ==============================================================================
# Title: Binary logistic comparison of home and visiting formations
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script analyses the association between tactical formations and the
# probability of a home-team victory during the 2024/2025 season. The dependent
# variable is binary and distinguishes between a home victory and no home
# victory.
#
# The script imports the single-season and multi-season datasets, although the
# formation analysis is conducted using the single-season dataset stored as d.
# Formation labels are cleaned by removing internal quotation marks and
# unnecessary spaces before converting the variables into factors.
#
# The frequencies of the home and visiting formations are examined separately.
# Formation categories represented by fewer than ten matches are grouped into
# a residual category named "Otras" to reduce the instability associated with
# sparsely represented tactical systems.
#
# A binary logistic regression without an interaction term is estimated using
# the grouped home and visiting formations as categorical explanatory
# variables. The model therefore estimates the additive association of each
# formation with the probability of a home-team victory.
#
# The reference categories of the home and visiting formation variables are
# changed repeatedly to obtain all relevant pairwise coefficient comparisons.
# Separate models are estimated for combinations involving the 1-4-2-3-1,
# 1-3-4-3, 1-4-1-4-1, 1-4-3-3, 1-4-4-2, 1-5-3-2, 1-5-4-1 and grouped
# alternative formations.
#
# The repeated reference-level specifications allow the coefficients,
# standard errors and p-values associated with different formation contrasts
# to be inspected directly while preserving the same underlying additive
# logistic model.
#
# Input:
#   variables_Estudio (9).xlsx
#   LaLiga_22-25_completo_v2 (2).xlsx
#   logit_funciones.R
#
# Dataset used in the analysis:
#   d, corresponding to the 2024/2025 season.
#
# Dependent variable:
#   win_local
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory
#
# Explanatory variables:
#   formacion_local_dep
#   formacion_visit_dep
#
# Treatment of infrequent formations:
#   Formation categories represented by fewer than ten matches are grouped
#   into the category "Otras".
#
# Statistical method:
#   Binary logistic regression estimated with glm() and family = binomial.
#
# Model specification:
#   Additive model without an interaction between the home and visiting
#   formations.
#
# Main outputs:
#   Formation-frequency tables and logistic regression summaries for the
#   different reference-category combinations of home and visiting formations.
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

#modelo 4231 - 343

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m_sin_interaccion1 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                        data = d,
                        family = binomial)

summary(m_sin_interaccion1)

#4231-4141

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m_sin_interaccion2 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                         data = d,
                         family = binomial)

summary(m_sin_interaccion2)

#4231-433

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m_sin_interaccion3 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                         data = d,
                         family = binomial)

summary(m_sin_interaccion3)

#4231-442

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m_sin_interaccion4 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                         data = d,
                         family = binomial)

summary(m_sin_interaccion4)

#4231-532

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m_sin_interaccion5 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                         data = d,
                         family = binomial)

summary(m_sin_interaccion5)

#4231-541

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m_sin_interaccion6 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                         data = d,
                         family = binomial)

summary(m_sin_interaccion6)

#4231-otras

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m_sin_interaccion6 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                         data = d,
                         family = binomial)

summary(m_sin_interaccion6)

##########################################################################################
##########################################################################################

#343-4231

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-3-4-3")
d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")
m_343_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                         data = d,
                         family = binomial)

summary(m_343_4231)

#343-343

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m_343_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_343_343)

#343-4141

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m_343_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_343_4141)

#343-433

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m_343_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_343_433)

#343-442

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m_343_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_343_442)

#343-532

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m_343_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_343_532)

#343-541

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m_343_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_343_541)

#343-otras

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m_343_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_343_Otras)

###############################################################################
###############################################################################

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-1-4-1")

#4141-4231

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")
m_4141_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_4141_4231)

#4141-343

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m_4141_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_4141_343)

#4141-4141

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m_4141_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_4141_4141)

#4141-433

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m_4141_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_4141_433)

#4141-442

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m_4141_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_4141_442)

#4141-532

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m_4141_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_4141_532)

#4141-541

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m_4141_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_4141_541)

#4141-otras

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m_4141_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d,
                  family = binomial)

summary(m_4141_Otras)

###############################################################################
###############################################################################

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-3-3")

#433-4231

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")
m_433_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d,
                  family = binomial)

summary(m_433_4231)

#433-343

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m_433_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_433_343)

#433-4141

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m_433_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d,
                  family = binomial)

summary(m_433_4141)

#433-433

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m_433_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_433_433)

#433-442

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m_433_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_433_442)

#433-532

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m_433_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_433_532)

#433-541

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m_433_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_433_541)

#433-otras

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m_433_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                   data = d,
                   family = binomial)

summary(m_433_Otras)

###############################################################################
###############################################################################

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-4-4-2")

#442-4231

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")
m_442_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_442_4231)

#442-343

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m_442_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_442_343)

#442-4141

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m_442_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_442_4141)

#442-433

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m_442_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_442_433)

#442-442

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m_442_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_442_442)

#442-532

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m_442_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_442_532)

#442-541

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m_442_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_442_541)

#442-otras

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m_442_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d,
                  family = binomial)

summary(m_442_Otras)

###############################################################################
###############################################################################

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "1-5-4-1")

#541-4231

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")
m_541_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_541_4231)

#541-343

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m_541_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_541_343)

#541-4141

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m_541_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_541_4141)

#541-433

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m_541_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_541_433)

#541-442

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m_541_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_541_442)

#541-532

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m_541_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_541_532)

#541-541

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m_541_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_541_541)

#541-otras

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m_541_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d,
                  family = binomial)

summary(m_541_Otras)

###############################################################################
###############################################################################

d$formacion_local_dep = relevel(d$formacion_local_dep, ref = "Otras")

#Otras-4231

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-2-3-1")
m_Otras_4231 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_Otras_4231)

#Otras-343

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-3-4-3")
m_Otras_343 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_Otras_343)

#Otras-4141

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-1-4-1")
m_Otras_4141 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                 data = d,
                 family = binomial)

summary(m_Otras_4141)

#Otras-433

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-3-3")
m_Otras_433 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_Otras_433)

#Otras-442

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-4-4-2")
m_Otras_442 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_Otras_442)

#Otras-532

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-3-2")
m_Otras_532 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_Otras_532)

#Otras-541

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "1-5-4-1")
m_Otras_541 = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                data = d,
                family = binomial)

summary(m_Otras_541)

#Otras-otras

d$formacion_visit_dep = relevel(d$formacion_visit_dep, ref = "Otras")
m_Otras_Otras = glm(win_local ~ formacion_local_dep + formacion_visit_dep,
                  data = d,
                  family = binomial)

summary(m_Otras_Otras)
