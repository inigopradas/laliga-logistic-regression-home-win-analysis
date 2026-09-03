# ==============================================================================
# Title: Binomial model performance and tactical formation comparison
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script compares the performance of binary logistic regression models
# and analyses the association between tactical formations and home-team
# victory across two study periods.
#
# The first dataset, d, contains match-level observations from the 2024/2025
# LaLiga season. The second dataset, d1, contains observations from the
# 2022/2023, 2023/2024 and 2024/2025 seasons.
#
# The dependent variable distinguishes between a home-team victory and no
# home-team victory. The latter category includes draws and away-team
# victories. The variable is normalised to a numerical binary indicator before
# model estimation and graphical analysis.
#
# The first section compares the selected offensive, defensive, physical,
# match-control, contextual and general models across the single-season and
# multi-season datasets. The comparison includes the number of observations,
# number of explanatory variables, AIC, residual deviance, overall accuracy,
# sensitivity for home victory and specificity for no home victory.
#
# Classification performance is represented through grouped bar charts and
# point-based comparison plots. These figures show how model performance
# changes when the analysis is expanded from one season to three seasons.
# Because the metrics are calculated using the same observations employed for
# model estimation, they should be interpreted as in-sample descriptive
# performance.
#
# The second section analyses the frequency of home and visiting tactical
# formations. Formation labels are cleaned and standardised by removing
# quotation marks, unnecessary spaces, non-standard hyphens and invalid
# values. Infrequent formations may be grouped into the residual category
# "Otras".
#
# Observed home-victory rates are calculated separately for home and visiting
# formations. Wilson 95% confidence intervals are used to represent the
# uncertainty associated with each observed proportion. The overall home-
# victory rate is included as a graphical reference.
#
# A joint binary logistic regression model is estimated using the home and
# visiting formations as simultaneous additive predictors. The 1-4-2-3-1
# formation is used as the reference category for both formation variables.
# The model does not include an interaction between the formations.
#
# Adjusted odds ratios and 95% confidence intervals are extracted from the
# joint model. The estimates describe the association of each formation with
# the odds of a home-team victory while controlling for the formation used by
# the opposing team.
#
# The script also calculates observed results and model-estimated probabilities
# for combinations of home and visiting formations. Heatmaps display the
# frequency and observed home-victory proportion of each tactical pairing,
# while scatter plots compare observed proportions with estimated
# probabilities.
#
# Formation-specific models are additionally estimated for d and d1. These
# models provide predicted home-victory probabilities and 95% confidence
# intervals for home and visiting formations separately.
#
# The results from d and d1 are combined to compare formation-specific
# probabilities across study periods. Difference plots show the change in the
# estimated probability of home victory between the multi-season and
# single-season datasets.
#
# Likelihood-ratio tests compare models with and without interactions between
# formation and dataset. These tests assess whether the association between
# tactical formation and home victory differs significantly between the
# single-season and multi-season samples.
#
# Datasets:
#   d  = match-level data from the 2024/2025 LaLiga season
#   d1 = match-level data from the 2022/2023, 2023/2024 and 2024/2025 seasons
#
# Dependent variable:
#   win_local_num
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory, including draws and away-team victories
#
# Models compared:
#   Offensive model
#   Defensive model
#   Physical model
#   Match-control model
#   Contextual model
#   General model
#
# Formation explanatory variables:
#   formacion_local
#   formacion_visit
#   formacion_local_dep
#   formacion_visit_dep
#
# Formation reference category:
#   1-4-2-3-1 for both home and visiting teams
#
# Statistical methods:
#   Binary logistic regression estimated with glm() and a logit link.
#   Wilson 95% confidence intervals for observed home-victory proportions.
#   Likelihood-ratio tests for comparisons between nested logistic models.
#
# Main functions:
#   convertir_win_local()
#   limpiar_formacion()
#   normalizar_formacion()
#   intervalo_wilson()
#   analizar_formaciones()
#   calcular_diferencias()
#
# Main models:
#   modelo_formaciones_d
#   modelo_formaciones_d1
#   modelo_d1
#   modelo_local_sin_interaccion
#   modelo_local_con_interaccion
#   modelo_visit_sin_interaccion
#   modelo_visit_con_interaccion
#
# Main outputs:
#   Model-performance comparison tables, classification-metric figures,
#   formation-frequency charts, observed home-victory proportions, Wilson
#   confidence intervals, adjusted odds ratios, formation-combination
#   heatmaps, observed-versus-estimated plots and comparisons of formation
#   effects between the single-season and multi-season datasets.
#
# Important methodological notes:
#   The classification metrics represent in-sample performance and should not
#   be interpreted as out-of-sample predictive accuracy.
#
#   Formation-specific victory proportions are descriptive associations and
#   should not be interpreted as causal effects.
#
#   Comparisons between d and d1 are not based on independent samples because
#   the 2024/2025 season included in d is also part of the multi-season dataset
#   d1. This overlap should be considered when interpreting differences between
#   the two study periods.
# ==============================================================================

# =========================================================
# PAQUETES
# =========================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(broom)

# =========================================================
# CARGA DE DATOS
# =========================================================

d <- read_excel("variables_Estudio (9).xlsx")

# =========================================================
# LIMPIEZA DE FORMACIONES
# =========================================================

d <- d %>%
  mutate(
    formacion_local = gsub('"', '', trimws(as.character(formacion_local))),
    formacion_visit = gsub('"', '', trimws(as.character(formacion_visit)))
  )

# Eliminar filas sin información necesaria
d <- d %>%
  filter(
    !is.na(win_local),
    !is.na(formacion_local),
    !is.na(formacion_visit),
    formacion_local != "",
    formacion_visit != ""
  )

# =========================================================
# COMPROBAR LA CODIFICACIÓN DE win_local
# =========================================================

table(d$win_local, useNA = "ifany")
levels(factor(d$win_local))
# =========================================================
# AGRUPACIÓN DE FORMACIONES RARAS
# =========================================================

umbral <- 10

freq_local <- table(d$formacion_local)
freq_visit <- table(d$formacion_visit)

formaciones_raras_local <- names(freq_local[freq_local < umbral])
formaciones_raras_visit <- names(freq_visit[freq_visit < umbral])

d <- d %>%
  mutate(
    formacion_local_dep = ifelse(
      formacion_local %in% formaciones_raras_local,
      "Otras",
      formacion_local
    ),
    formacion_visit_dep = ifelse(
      formacion_visit %in% formaciones_raras_visit,
      "Otras",
      formacion_visit
    ),
    formacion_local_dep = factor(formacion_local_dep),
    formacion_visit_dep = factor(formacion_visit_dep)
  )
orden_formaciones <- c(
  "1-4-2-3-1",
  "1-4-3-3",
  "1-4-4-2",
  "1-4-1-4-1",
  "1-3-4-3",
  "1-5-3-2",
  "1-5-4-1",
  "Otras"
)

niveles_local <- orden_formaciones[
  orden_formaciones %in% levels(d$formacion_local_dep)
]

niveles_visit <- orden_formaciones[
  orden_formaciones %in% levels(d$formacion_visit_dep)
]

d <- d %>%
  mutate(
    formacion_local_dep = factor(
      formacion_local_dep,
      levels = niveles_local
    ),
    formacion_visit_dep = factor(
      formacion_visit_dep,
      levels = niveles_visit
    )
  )
# =========================================================
# FRECUENCIA DE FORMACIONES
# =========================================================

frecuencias <- bind_rows(
  
  d %>%
    count(formacion_local_dep, name = "partidos") %>%
    transmute(
      condicion = "Equipo local",
      formacion = as.character(formacion_local_dep),
      partidos
    ),
  
  d %>%
    count(formacion_visit_dep, name = "partidos") %>%
    transmute(
      condicion = "Equipo visitante",
      formacion = as.character(formacion_visit_dep),
      partidos
    )
)

grafico_frecuencias <- ggplot(
  frecuencias,
  aes(
    x = reorder(formacion, partidos),
    y = partidos,
    fill = condicion
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = partidos),
    position = position_dodge(width = 0.8),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Equipo local" = "#1F4E79",
      "Equipo visitante" = "#E67E22"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Frecuencia de utilización de las formaciones",
    subtitle = "El primer 1 de la formación representa al portero",
    x = "Formación",
    y = "Número de partidos",
    fill = NULL,
    caption = paste0(
      "Las formaciones con menos de ",
      umbral,
      " apariciones se agrupan en 'Otras'."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_frecuencias
# =========================================================
# RESULTADOS OBSERVADOS POR COMBINACIÓN
# =========================================================

observado_combinaciones <- d %>%
  group_by(formacion_local_dep, formacion_visit_dep) %>%
  summarise(
    partidos = n(),
    victorias_local = sum(win_local_num == 1, na.rm = TRUE),
    proporcion_victoria = mean(win_local_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    formacion_local_dep,
    formacion_visit_dep,
    fill = list(
      partidos = 0,
      victorias_local = 0,
      proporcion_victoria = NA
    )
  ) %>%
  mutate(
    etiqueta = ifelse(
      partidos == 0,
      "Sin datos",
      paste0(
        percent(proporcion_victoria, accuracy = 1),
        "\n(n = ",
        partidos,
        ")"
      )
    ),
    color_texto = ifelse(
      !is.na(proporcion_victoria) &
        (proporcion_victoria < 0.25 | proporcion_victoria > 0.70),
      "white",
      "black"
    )
  )

grafico_observado <- ggplot(
  observado_combinaciones,
  aes(
    x = formacion_visit_dep,
    y = formacion_local_dep,
    fill = proporcion_victoria
  )
) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(label = etiqueta, color = color_texto),
    size = 3.1,
    lineheight = 0.9
  ) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "#F7F7F7",
    high = "#2166AC",
    midpoint = 0.5,
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    na.value = "grey90"
  ) +
  labs(
    title = "Porcentaje observado de victoria local",
    subtitle = "Resultados según la combinación de formaciones iniciales",
    x = "Formación del equipo visitante",
    y = "Formación del equipo local",
    fill = "Victorias\nlocales",
    caption = "Entre paréntesis se muestra el número de partidos de cada combinación."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

grafico_observado
################################################
# =========================================================
# COMBINACIONES POSIBLES
# =========================================================
# =========================================================
# MODELO ADITIVO SIN INTERACCIÓN
# =========================================================
d <- d %>%
  mutate(win_local_num = as.numeric(as.character(win_local)))
d <- d %>%
  mutate(
    formacion_local_dep = relevel(
      formacion_local_dep,
      ref = "1-4-2-3-1"
    ),
    formacion_visit_dep = relevel(
      formacion_visit_dep,
      ref = "1-4-2-3-1"
    )
  )

m_sin_interaccion <- glm(
  win_local_num ~ formacion_local_dep + formacion_visit_dep,
  data = d,
  family = binomial(link = "logit")
)

summary(m_sin_interaccion)
nuevos_datos <- expand_grid(
  formacion_local_dep = levels(d$formacion_local_dep),
  formacion_visit_dep = levels(d$formacion_visit_dep)
) %>%
  mutate(
    formacion_local_dep = factor(
      formacion_local_dep,
      levels = levels(d$formacion_local_dep)
    ),
    formacion_visit_dep = factor(
      formacion_visit_dep,
      levels = levels(d$formacion_visit_dep)
    )
  )

# Predicción en escala logit, incluyendo error estándar
predicciones <- predict(
  m_sin_interaccion,
  newdata = nuevos_datos,
  type = "link",
  se.fit = TRUE
)

mapa_predicho <- nuevos_datos %>%
  mutate(
    logit = predicciones$fit,
    error_estandar = predicciones$se.fit,
    probabilidad = plogis(logit),
    limite_inferior = plogis(logit - 1.96 * error_estandar),
    limite_superior = plogis(logit + 1.96 * error_estandar),
    etiqueta = percent(probabilidad, accuracy = 0.1),
    color_texto = ifelse(
      probabilidad < 0.25 | probabilidad > 0.70,
      "white",
      "black"
    )
  )

grafico_predicciones <- ggplot(
  mapa_predicho,
  aes(
    x = formacion_visit_dep,
    y = formacion_local_dep,
    fill = probabilidad
  )
) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(label = etiqueta, color = color_texto),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "#F7F7F7",
    high = "#2166AC",
    midpoint = 0.5,
    limits = c(0, 1),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Probabilidad estimada de victoria local",
    subtitle = "Modelo logístico sin interacción entre las formaciones",
    x = "Formación del equipo visitante",
    y = "Formación del equipo local",
    fill = "Probabilidad\nde victoria",
    caption = paste(
      "Estimaciones obtenidas mediante regresión logística.",
      "El primer 1 representa al portero."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

grafico_predicciones
# =========================================================
# ODDS RATIOS E INTERVALOS DE CONFIANZA
# =========================================================

resultados_or <- tidy(
  m_sin_interaccion,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    condicion = case_when(
      grepl("^formacion_local_dep", term) ~ "Formación local",
      grepl("^formacion_visit_dep", term) ~ "Formación visitante",
      TRUE ~ "Otra"
    ),
    formacion = term,
    formacion = sub(
      "^formacion_local_dep",
      "",
      formacion
    ),
    formacion = sub(
      "^formacion_visit_dep",
      "",
      formacion
    ),
    significativo = ifelse(
      conf.low > 1 | conf.high < 1,
      "Sí",
      "No"
    )
  )
grafico_or <- ggplot(
  resultados_or,
  aes(
    x = estimate,
    y = reorder(formacion, estimate),
    color = significativo
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0.2,
    linewidth = 0.8
  ) +
  geom_point(size = 3) +
  facet_wrap(
    ~ condicion,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "Sí" = "#B2182B",
      "No" = "#636363"
    )
  ) +
  labs(
    title = "Efecto estimado de las formaciones sobre la victoria local",
    subtitle = "Odds ratios e intervalos de confianza del 95 %",
    x = "Odds ratio, escala logarítmica",
    y = "Formación",
    color = "Diferencia\nsignificativa",
    caption = paste(
      "Categoría de referencia: 1-4-2-3-1.",
      "La línea vertical indica OR = 1."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

grafico_or
# =========================================================
# UNIR RESULTADOS OBSERVADOS Y PREDICHOS
# =========================================================

comparacion_modelo <- observado_combinaciones %>%
  filter(partidos > 0) %>%
  left_join(
    mapa_predicho %>%
      select(
        formacion_local_dep,
        formacion_visit_dep,
        probabilidad
      ),
    by = c(
      "formacion_local_dep",
      "formacion_visit_dep"
    )
  )

grafico_comparacion <- ggplot(
  comparacion_modelo,
  aes(
    x = probabilidad,
    y = proporcion_victoria,
    size = partidos,
    color = formacion_local_dep
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey30"
  ) +
  geom_point(alpha = 0.75) +
  scale_x_continuous(
    labels = percent_format(),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1)
  ) +
  scale_size_continuous(
    range = c(2, 10)
  ) +
  labs(
    title = "Resultados observados frente a probabilidades estimadas",
    subtitle = "Cada punto representa una combinación de formaciones",
    x = "Probabilidad estimada de victoria local",
    y = "Proporción observada de victoria local",
    color = "Formación local",
    size = "Partidos",
    caption = "Los puntos próximos a la diagonal presentan mayor concordancia entre el modelo y los datos."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

grafico_comparacion

#########################################################################
# =========================================================
# PAQUETES
# =========================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(broom)

# =========================================================
# CARGAR DATOS
# =========================================================

d  <- read_excel("variables_Estudio (9).xlsx")
d1 <- read_excel("LaLiga_22-25_completo_v2 (2).xlsx")
as.numeric(d$win_local)
convertir_win_local <- function(x) {
  
  x_original <- x
  x <- trimws(tolower(as.character(x)))
  
  resultado <- case_when(
    x %in% c("1", "si", "sí", "yes", "victoria local", "local") ~ 1,
    x %in% c("0", "no", "empate", "victoria visitante",
             "visitante", "no victoria local") ~ 0,
    TRUE ~ NA_real_
  )
  
  # Comprobar valores que no se han podido convertir
  valores_problematicos <- unique(
    as.character(x_original)[is.na(resultado) & !is.na(x_original)]
  )
  
  if (length(valores_problematicos) > 0) {
    warning(
      paste(
        "No se han podido convertir estos valores de win_local:",
        paste(valores_problematicos, collapse = ", ")
      )
    )
  }
  
  return(resultado)
}
d$win_local_num  <- convertir_win_local(d$win_local)
d1$win_local_num <- convertir_win_local(d1$win_local)

table(d$win_local, d$win_local_num, useNA = "ifany")
table(d1$win_local, d1$win_local_num, useNA = "ifany")
limpiar_formacion <- function(x) {
  
  x <- as.character(x)
  x <- gsub('"', "", x)
  x <- trimws(x)
  x[x == ""] <- NA
  
  return(x)
}

d <- d %>%
  mutate(
    formacion_local = limpiar_formacion(formacion_local),
    formacion_visit = limpiar_formacion(formacion_visit)
  )

d1 <- d1 %>%
  mutate(
    formacion_local = limpiar_formacion(formacion_local),
    formacion_visit = limpiar_formacion(formacion_visit)
  )
# =========================================================
# FRECUENCIAS EN CADA CONJUNTO
# =========================================================

umbral <- 10

frecuencias_d <- bind_rows(
  d %>% transmute(formacion = formacion_local),
  d %>% transmute(formacion = formacion_visit)
) %>%
  filter(!is.na(formacion)) %>%
  count(formacion, name = "frecuencia_d")

frecuencias_d1 <- bind_rows(
  d1 %>% transmute(formacion = formacion_local),
  d1 %>% transmute(formacion = formacion_visit)
) %>%
  filter(!is.na(formacion)) %>%
  count(formacion, name = "frecuencia_d1")

tabla_frecuencias <- full_join(
  frecuencias_d,
  frecuencias_d1,
  by = "formacion"
) %>%
  mutate(
    frecuencia_d = replace_na(frecuencia_d, 0),
    frecuencia_d1 = replace_na(frecuencia_d1, 0)
  ) %>%
  arrange(desc(frecuencia_d + frecuencia_d1))

tabla_frecuencias
formaciones_comunes <- tabla_frecuencias %>%
  filter(
    frecuencia_d >= umbral,
    frecuencia_d1 >= umbral
  ) %>%
  pull(formacion)

formaciones_comunes
d <- d %>%
  mutate(
    formacion_local_dep = ifelse(
      formacion_local %in% formaciones_comunes,
      formacion_local,
      "Otras"
    ),
    formacion_visit_dep = ifelse(
      formacion_visit %in% formaciones_comunes,
      formacion_visit,
      "Otras"
    )
  )

d1 <- d1 %>%
  mutate(
    formacion_local_dep = ifelse(
      formacion_local %in% formaciones_comunes,
      formacion_local,
      "Otras"
    ),
    formacion_visit_dep = ifelse(
      formacion_visit %in% formaciones_comunes,
      formacion_visit,
      "Otras"
    )
  )
analizar_formaciones <- function(datos, variable_formacion,
                                 nombre_datos, condicion) {
  
  datos_modelo <- datos %>%
    select(
      win_local_num,
      formacion = all_of(variable_formacion)
    ) %>%
    filter(
      !is.na(win_local_num),
      !is.na(formacion)
    ) %>%
    mutate(
      formacion = droplevels(factor(formacion))
    )
  
  # Modelo univariante
  modelo <- glm(
    win_local_num ~ formacion,
    data = datos_modelo,
    family = binomial(link = "logit")
  )
  
  # Datos descriptivos
  resultados <- datos_modelo %>%
    group_by(formacion) %>%
    summarise(
      partidos = n(),
      victorias_locales = sum(win_local_num == 1),
      derrotas_o_empates = sum(win_local_num == 0),
      proporcion_observada = mean(win_local_num),
      .groups = "drop"
    )
  
  # Predicciones para cada formación
  nuevos_datos <- data.frame(
    formacion = levels(datos_modelo$formacion)
  )
  
  nuevos_datos$formacion <- factor(
    nuevos_datos$formacion,
    levels = levels(datos_modelo$formacion)
  )
  
  prediccion <- predict(
    modelo,
    newdata = nuevos_datos,
    type = "link",
    se.fit = TRUE
  )
  
  predicciones <- nuevos_datos %>%
    mutate(
      logit = as.numeric(prediccion$fit),
      error_estandar = as.numeric(prediccion$se.fit),
      probabilidad_estimada = plogis(logit),
      limite_inferior = plogis(
        logit - qnorm(0.975) * error_estandar
      ),
      limite_superior = plogis(
        logit + qnorm(0.975) * error_estandar
      )
    )
  
  resultados <- resultados %>%
    left_join(predicciones, by = "formacion") %>%
    mutate(
      conjunto = nombre_datos,
      condicion = condicion
    )
  
  return(
    list(
      modelo = modelo,
      resultados = resultados,
      datos_modelo = datos_modelo
    )
  )
}
analisis_d_local <- analizar_formaciones(
  datos = d,
  variable_formacion = "formacion_local_dep",
  nombre_datos = "d",
  condicion = "Formación local"
)

analisis_d_visit <- analizar_formaciones(
  datos = d,
  variable_formacion = "formacion_visit_dep",
  nombre_datos = "d",
  condicion = "Formación visitante"
)

analisis_d1_local <- analizar_formaciones(
  datos = d1,
  variable_formacion = "formacion_local_dep",
  nombre_datos = "d1",
  condicion = "Formación local"
)

analisis_d1_visit <- analizar_formaciones(
  datos = d1,
  variable_formacion = "formacion_visit_dep",
  nombre_datos = "d1",
  condicion = "Formación visitante"
)
analisis_d_local$resultados
analisis_d_visit$resultados

analisis_d1_local$resultados
analisis_d1_visit$resultados
grafico_d_local <- ggplot(
  analisis_d_local$resultados,
  aes(
    x = probabilidad_estimada,
    y = reorder(formacion, probabilidad_estimada)
  )
) +
  geom_vline(
    xintercept = mean(d$win_local_num, na.rm = TRUE),
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_errorbarh(
    aes(
      xmin = limite_inferior,
      xmax = limite_superior
    ),
    height = 0.2,
    color = "#1F4E79",
    linewidth = 0.8
  ) +
  geom_point(
    aes(size = partidos),
    color = "#1F4E79",
    alpha = 0.85
  ) +
  geom_text(
    aes(
      label = percent(
        probabilidad_estimada,
        accuracy = 0.1
      )
    ),
    hjust = -0.2,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      min(
        1,
        max(
          analisis_d_local$resultados$limite_superior,
          na.rm = TRUE
        ) + 0.12
      )
    )
  ) +
  scale_size_continuous(range = c(3, 10)) +
  labs(
    title = "Victoria local según la formación del equipo local",
    subtitle = "Conjunto d: probabilidad estimada e intervalo de confianza del 95 %",
    x = "Probabilidad de victoria local",
    y = "Formación del equipo local",
    size = "Partidos",
    caption = paste(
      "La línea discontinua representa la proporción global",
      "de victorias locales en el conjunto d.",
      "El primer 1 de la formación representa al portero."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_d_local
grafico_d_visit <- ggplot(
  analisis_d_visit$resultados,
  aes(
    x = probabilidad_estimada,
    y = reorder(formacion, probabilidad_estimada)
  )
) +
  geom_vline(
    xintercept = mean(d$win_local_num, na.rm = TRUE),
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_errorbarh(
    aes(
      xmin = limite_inferior,
      xmax = limite_superior
    ),
    height = 0.2,
    color = "#E67E22",
    linewidth = 0.8
  ) +
  geom_point(
    aes(size = partidos),
    color = "#E67E22",
    alpha = 0.85
  ) +
  geom_text(
    aes(
      label = percent(
        probabilidad_estimada,
        accuracy = 0.1
      )
    ),
    hjust = -0.2,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      min(
        1,
        max(
          analisis_d_visit$resultados$limite_superior,
          na.rm = TRUE
        ) + 0.12
      )
    )
  ) +
  scale_size_continuous(range = c(3, 10)) +
  labs(
    title = "Victoria local según la formación del equipo visitante",
    subtitle = "Conjunto d: probabilidad estimada e intervalo de confianza del 95 %",
    x = "Probabilidad de victoria local",
    y = "Formación del equipo visitante",
    size = "Partidos",
    caption = paste(
      "Una probabilidad elevada indica más victorias del equipo local",
      "cuando el visitante utiliza esa formación."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_d_visit
grafico_d1_local <- ggplot(
  analisis_d1_local$resultados,
  aes(
    x = probabilidad_estimada,
    y = reorder(formacion, probabilidad_estimada)
  )
) +
  geom_vline(
    xintercept = mean(d1$win_local_num, na.rm = TRUE),
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_errorbarh(
    aes(
      xmin = limite_inferior,
      xmax = limite_superior
    ),
    height = 0.2,
    color = "#2166AC",
    linewidth = 0.8
  ) +
  geom_point(
    aes(size = partidos),
    color = "#2166AC",
    alpha = 0.85
  ) +
  geom_text(
    aes(
      label = percent(
        probabilidad_estimada,
        accuracy = 0.1
      )
    ),
    hjust = -0.2,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      min(
        1,
        max(
          analisis_d1_local$resultados$limite_superior,
          na.rm = TRUE
        ) + 0.12
      )
    )
  ) +
  scale_size_continuous(range = c(3, 10)) +
  labs(
    title = "Victoria local según la formación del equipo local",
    subtitle = "Conjunto d1: probabilidad estimada e intervalo de confianza del 95 %",
    x = "Probabilidad de victoria local",
    y = "Formación del equipo local",
    size = "Partidos",
    caption = "El primer 1 de la formación representa al portero."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_d1_local
grafico_d1_visit <- ggplot(
  analisis_d1_visit$resultados,
  aes(
    x = probabilidad_estimada,
    y = reorder(formacion, probabilidad_estimada)
  )
) +
  geom_vline(
    xintercept = mean(d1$win_local_num, na.rm = TRUE),
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_errorbarh(
    aes(
      xmin = limite_inferior,
      xmax = limite_superior
    ),
    height = 0.2,
    color = "#D95F02",
    linewidth = 0.8
  ) +
  geom_point(
    aes(size = partidos),
    color = "#D95F02",
    alpha = 0.85
  ) +
  geom_text(
    aes(
      label = percent(
        probabilidad_estimada,
        accuracy = 0.1
      )
    ),
    hjust = -0.2,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      min(
        1,
        max(
          analisis_d1_visit$resultados$limite_superior,
          na.rm = TRUE
        ) + 0.12
      )
    )
  ) +
  scale_size_continuous(range = c(3, 10)) +
  labs(
    title = "Victoria local según la formación del equipo visitante",
    subtitle = "Conjunto d1: probabilidad estimada e intervalo de confianza del 95 %",
    x = "Probabilidad de victoria local",
    y = "Formación del equipo visitante",
    size = "Partidos"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_d1_visit
resultados_completos <- bind_rows(
  analisis_d_local$resultados,
  analisis_d_visit$resultados,
  analisis_d1_local$resultados,
  analisis_d1_visit$resultados
) %>%
  mutate(
    formacion = factor(
      as.character(formacion),
    ),
    conjunto = factor(
      conjunto,
      levels = c("d", "d1")
    )
  )

resultados_completos
comparacion_local <- resultados_completos %>%
  filter(condicion == "Formación local")

grafico_comparacion_local <- ggplot(
  comparacion_local,
  aes(
    x = probabilidad_estimada,
    y = formacion,
    color = conjunto
  )
) +
  geom_errorbarh(
    aes(
      xmin = limite_inferior,
      xmax = limite_superior
    ),
    position = position_dodge(width = 0.55),
    height = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    aes(shape = conjunto),
    position = position_dodge(width = 0.55),
    size = 3.2
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_color_manual(
    values = c(
      "d" = "#1F4E79",
      "d1" = "#E67E22"
    ),
    labels = c(
      "d" = "LaLiga 2024/2025 (d)",
      "d1" = "LaLiga 2022-2025 (d1)"
    )
  ) +
  scale_shape_manual(
    values = c(
      "d" = 16,
      "d1" = 17
    ),
    labels = c(
      "d" = "LaLiga 2024/2025 (d)",
      "d1" = "LaLiga 2022-2025 (d1)"
    )
  ) +
  labs(
    title = "Comparación de las formaciones locales entre d y d1",
    subtitle = "Probabilidades de victoria local e intervalos de confianza del 95 %",
    x = "Probabilidad de victoria local",
    y = "Formación del equipo local",
    color = "Conjunto",
    shape = "Conjunto",
    caption = "Los intervalos amplios indican una mayor incertidumbre en la estimación."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_comparacion_local
calcular_diferencias <- function(resultados, tipo_condicion) {
  
  resultados %>%
    filter(condicion == tipo_condicion) %>%
    select(
      formacion,
      conjunto,
      probabilidad_estimada,
      partidos
    ) %>%
    pivot_wider(
      names_from = conjunto,
      values_from = c(
        probabilidad_estimada,
        partidos
      )
    ) %>%
    filter(
      !is.na(probabilidad_estimada_d),
      !is.na(probabilidad_estimada_d1)
    ) %>%
    mutate(
      diferencia = probabilidad_estimada_d1 -
        probabilidad_estimada_d,
      direccion = case_when(
        diferencia > 0 ~ "Mayor en d1",
        diferencia < 0 ~ "Mayor en d",
        TRUE ~ "Sin diferencia"
      )
    )
}
diferencias_local <- calcular_diferencias(
  resultados_completos,
  "Formación local"
)

grafico_diferencias_local <- ggplot(
  diferencias_local,
  aes(
    x = diferencia,
    y = reorder(formacion, diferencia),
    fill = direccion
  )
) +
  geom_vline(
    xintercept = 0,
    color = "grey30",
    linewidth = 0.8
  ) +
  geom_col(width = 0.65) +
  geom_text(
    aes(
      label = percent(diferencia, accuracy = 0.1)
    ),
    hjust = ifelse(
      diferencias_local$diferencia >= 0,
      -0.15,
      1.15
    ),
    color = "black",
    size = 3.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  scale_fill_manual(
    values = c(
      "Mayor en d1" = "#E67E22",
      "Mayor en d" = "#1F4E79",
      "Sin diferencia" = "grey60"
    )
  ) +
  labs(
    title = "Diferencia entre d1 y d por formación local",
    subtitle = "Diferencia en puntos porcentuales de probabilidad de victoria local",
    x = "Probabilidad en d1 menos probabilidad en d",
    y = "Formación local",
    fill = NULL,
    caption = paste(
      "Los valores positivos indican una probabilidad superior en d1;",
      "los negativos, una probabilidad superior en d."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_diferencias_local
diferencias_visit <- calcular_diferencias(
  resultados_completos,
  "Formación visitante"
)

grafico_diferencias_visit <- ggplot(
  diferencias_visit,
  aes(
    x = diferencia,
    y = reorder(formacion, diferencia),
    fill = direccion
  )
) +
  geom_vline(
    xintercept = 0,
    color = "grey30",
    linewidth = 0.8
  ) +
  geom_col(width = 0.65) +
  geom_text(
    aes(
      label = percent(diferencia, accuracy = 0.1)
    ),
    hjust = ifelse(
      diferencias_visit$diferencia >= 0,
      -0.15,
      1.15
    ),
    color = "black",
    size = 3.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  scale_fill_manual(
    values = c(
      "Mayor en d1" = "#E67E22",
      "Mayor en d" = "#1F4E79",
      "Sin diferencia" = "grey60"
    )
  ) +
  labs(
    title = "Diferencia entre d1 y d por formación visitante",
    subtitle = "Diferencia en la probabilidad de victoria del equipo local",
    x = "Probabilidad en d1 menos probabilidad en d",
    y = "Formación visitante",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

grafico_diferencias_visit
library(patchwork)
panel_individual <- (
  grafico_d_local + grafico_d_visit
) / (
  grafico_d1_local + grafico_d1_visit
) +
  plot_annotation(
    title = "Análisis individual de las formaciones",
    subtitle = paste(
      "Comparación de las probabilidades de victoria local",
      "en los conjuntos d y d1"
    )
  )

panel_individual
panel_comparacion <- (
  grafico_comparacion_local /
    grafico_comparacion_visit
) +
  plot_annotation(
    title = "Comparación de los resultados entre d y d1"
  )

panel_comparacion

datos_unidos_local <- bind_rows(
  d %>%
    transmute(
      win_local_num,
      formacion = formacion_local_dep,
      conjunto = "d"
    ),
  d1 %>%
    transmute(
      win_local_num,
      formacion = formacion_local_dep,
      conjunto = "d1"
    )
) %>%
  filter(
    !is.na(win_local_num),
    !is.na(formacion)
  ) %>%
  mutate(
    conjunto = factor(conjunto),
    formacion = factor(formacion)
  )

modelo_local_sin_interaccion <- glm(
  win_local_num ~ formacion + conjunto,
  data = datos_unidos_local,
  family = binomial
)

modelo_local_con_interaccion <- glm(
  win_local_num ~ formacion * conjunto,
  data = datos_unidos_local,
  family = binomial
)

anova(
  modelo_local_sin_interaccion,
  modelo_local_con_interaccion,
  test = "Chisq"
)
datos_unidos_visit <- bind_rows(
  d %>%
    transmute(
      win_local_num,
      formacion = formacion_visit_dep,
      conjunto = "d"
    ),
  d1 %>%
    transmute(
      win_local_num,
      formacion = formacion_visit_dep,
      conjunto = "d1"
    )
) %>%
  filter(
    !is.na(win_local_num),
    !is.na(formacion)
  ) %>%
  mutate(
    conjunto = factor(conjunto),
    formacion = factor(formacion)
  )

modelo_visit_sin_interaccion <- glm(
  win_local_num ~ formacion + conjunto,
  data = datos_unidos_visit,
  family = binomial
)

modelo_visit_con_interaccion <- glm(
  win_local_num ~ formacion * conjunto,
  data = datos_unidos_visit,
  family = binomial
)

anova(
  modelo_visit_sin_interaccion,
  modelo_visit_con_interaccion,
  test = "Chisq"
)


##############################################################
# =========================================================
# RESULTADOS OBSERVADOS FRENTE A ESTIMADOS EN d1
# =========================================================

library(dplyr)
library(ggplot2)
library(scales)

# ---------------------------------------------------------
# 1. Preparar los datos de d1
# ---------------------------------------------------------
# Ver nombres reales de las columnas de d1
names(d1)

# Ver valores originales de win_local
table(d1$win_local, useNA = "ifany")

# Crear win_local_num de forma robusta
d1 <- d1 %>%
  mutate(
    win_local_num = case_when(
      trimws(tolower(as.character(win_local))) %in%
        c("1", "si", "sí", "yes", "victoria local", "local") ~ 1,
      
      trimws(tolower(as.character(win_local))) %in%
        c(
          "0", "no", "empate", "victoria visitante",
          "visitante", "no victoria local"
        ) ~ 0,
      
      TRUE ~ NA_real_
    )
  )

# Comprobar la conversión
table(
  valor_original = d1$win_local,
  valor_numerico = d1$win_local_num,
  useNA = "ifany"
)
datos_grafico_d1 <- d1 %>%
  transmute(
    win_local_num = win_local_num,
    formacion_local = factor(formacion_local_dep),
    formacion_visit = factor(formacion_visit_dep)
  ) %>%
  filter(
    !is.na(win_local_num),
    !is.na(formacion_local),
    !is.na(formacion_visit)
  ) %>%
  droplevels()

# Comprobar que la variable respuesta contiene únicamente 0 y 1
table(datos_grafico_d1$win_local_num, useNA = "ifany")

# ---------------------------------------------------------
# 2. Modelo logístico aditivo para d1
# ---------------------------------------------------------

modelo_d1 <- glm(
  win_local_num ~ formacion_local + formacion_visit,
  data = datos_grafico_d1,
  family = binomial(link = "logit")
)

summary(modelo_d1)

# ---------------------------------------------------------
# 3. Resultados observados por combinación
# ---------------------------------------------------------

observado_d1 <- datos_grafico_d1 %>%
  group_by(
    formacion_local,
    formacion_visit
  ) %>%
  summarise(
    partidos = n(),
    victorias_locales = sum(win_local_num == 1),
    proporcion_observada = mean(win_local_num),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 4. Probabilidades estimadas por el modelo
# ---------------------------------------------------------

prediccion_d1 <- predict(
  modelo_d1,
  newdata = observado_d1,
  type = "link",
  se.fit = TRUE
)

comparacion_d1 <- observado_d1 %>%
  mutate(
    logit_estimado = as.numeric(prediccion_d1$fit),
    error_estandar = as.numeric(prediccion_d1$se.fit),
    
    probabilidad_estimada = plogis(logit_estimado),
    
    limite_inferior = plogis(
      logit_estimado - qnorm(0.975) * error_estandar
    ),
    
    limite_superior = plogis(
      logit_estimado + qnorm(0.975) * error_estandar
    )
  )

# Revisar la tabla utilizada en el gráfico
comparacion_d1 %>%
  arrange(desc(partidos))

# ---------------------------------------------------------
# 5. Colores de las formaciones
# ---------------------------------------------------------

colores_formaciones <- c(
  "1-4-2-3-1" = "#F8766D",
  "1-4-3-3"   = "#C49A00",
  "1-4-4-2"   = "#53B400",
  "1-4-1-4-1" = "#00C094",
  "1-3-4-3"   = "#00B6EB",
  "1-5-4-1"   = "#A58AFF",
  "1-5-3-2"   = "#7B61FF",
  "Otras"      = "#FB61D7"
)

# Conservar exclusivamente los colores correspondientes a niveles
# realmente presentes en d1
colores_presentes <- colores_formaciones[
  names(colores_formaciones) %in%
    levels(comparacion_d1$formacion_local)
]

# ---------------------------------------------------------
# 6. Gráfico observado frente a estimado
# ---------------------------------------------------------

grafico_comparacion_d1 <- ggplot(
  comparacion_d1,
  aes(
    x = probabilidad_estimada,
    y = proporcion_observada,
    size = partidos,
    color = formacion_local
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.6
  ) +
  geom_point(
    alpha = 0.80
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.25),
    minor_breaks = seq(0, 1, by = 0.125),
    limits = c(-0.04, 1.04),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.25),
    minor_breaks = seq(0, 1, by = 0.125),
    limits = c(-0.04, 1.04),
    expand = expansion(mult = 0)
  ) +
  scale_size_continuous(
    name = "Partidos",
    range = c(2, 10),
    breaks = c(10, 20, 30, 40, 50)
  ) +
  scale_color_manual(
    name = "Formación local",
    values = colores_presentes,
    drop = FALSE
  ) +
  coord_equal() +
  labs(
    title = "Resultados observados frente a probabilidades estimadas",
    subtitle = "Cada punto representa una combinación de formaciones",
    x = "Probabilidad estimada de victoria local",
    y = "Proporción observada de victoria local",
    caption = paste(
      "Los puntos próximos a la diagonal representan mayor",
      "concordancia entre el modelo y los datos."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 12
    ),
    axis.title = element_text(
      face = "bold"
    ),
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.major = element_line(
      color = "grey88",
      linewidth = 0.5
    ),
    panel.grid.minor = element_line(
      color = "grey93",
      linewidth = 0.4
    ),
    plot.caption = element_text(
      hjust = 0.5,
      size = 9
    )
  )

grafico_comparacion_d1
