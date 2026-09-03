# ==============================================================================
# Title: Binomial model comparison and tactical formation analysis
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script compares the classification performance of the offensive,
# defensive, physical, match-control, contextual and general binary logistic
# regression models estimated for two study periods. The first dataset, d,
# corresponds to the 2024/2025 LaLiga season, whereas d1 combines the
# 2022/2023, 2023/2024 and 2024/2025 seasons.
#
# The binary dependent variable distinguishes between a home-team victory and
# no home-team victory. The latter category includes both draws and away-team
# victories.
#
# The first part constructs a common comparison table for the selected
# single-season and multi-season models. The models are compared using the
# Akaike information criterion, residual deviance, overall accuracy,
# sensitivity for home victory and specificity for no home victory.
#
# Classification metrics are represented through grouped bar charts and point
# plots. These figures show whether the predictive behaviour of each analytical
# block remains stable when the study period is expanded from one season to
# three seasons.
#
# The second part examines the frequency with which the different tactical
# formations were used by home and visiting teams. Formation frequencies are
# displayed separately according to match condition to identify tactical
# systems supported by large or small numbers of observations.
#
# Formation labels are standardised by removing quotation marks, unnecessary
# spaces, non-standard hyphens and invalid values. The dependent variable is
# also converted safely into a numerical binary variable before the formation
# analysis is conducted.
#
# Observed home-victory rates are calculated separately for home and visiting
# formations. Wilson 95% confidence intervals are used to represent the
# uncertainty associated with each observed proportion. Only formations used
# in at least ten matches are included in the principal rate plots.
#
# The overall proportion of home victories is displayed as a reference line.
# This allows the observed home-victory rate associated with each tactical
# formation to be compared descriptively with the overall rate for the
# corresponding study period.
#
# A joint binary logistic regression model is subsequently estimated using the
# home and visiting formations as simultaneous additive predictors. This model
# evaluates the association of each tactical system with home-victory odds
# while adjusting for the formation used by the opposing team.
#
# The 1-4-2-3-1 formation is used as the reference category for both the home
# and visiting formation variables. Adjusted odds ratios and 95% confidence
# intervals are extracted from the joint model and classified according to
# whether the confidence interval lies above one, below one or includes one.
#
# The adjusted formation effects are represented through forest plots with a
# logarithmic odds-ratio scale. Formation frequencies are incorporated into
# the labels, and only tactical systems used in at least ten matches are shown.
#
# The script also examines specific combinations of home and visiting
# formations. For every observed pairing, it calculates the number of matches,
# the number of home victories and the observed home-victory proportion.
#
# Heatmaps are generated to display both the frequency of each formation
# pairing and the corresponding observed home-victory rate. Formation
# combinations represented by fewer than five matches are visually
# distinguished because their observed proportions provide limited empirical
# precision.
#
# Datasets:
#   d  = match-level observations from the 2024/2025 LaLiga season
#   d1 = match-level observations from the 2022/2023, 2023/2024 and
#        2024/2025 LaLiga seasons
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
#
# Formation reference category:
#   1-4-2-3-1 for both home and visiting teams
#
# Statistical methods:
#   Binary logistic regression estimated with glm() and a logit link.
#   Wilson 95% confidence intervals for observed home-victory proportions.
#
# Minimum marginal formation frequency:
#   Ten matches for inclusion in the principal formation summaries.
#
# Minimum formation-pair frequency:
#   Five matches for displaying an observed home-victory percentage in the
#   formation-combination heatmap.
#
# Main functions:
#   convertir_win_local()
#   normalizar_formacion()
#   intervalo_wilson()
#
# Main models:
#   modelo_formaciones_d
#   modelo_formaciones_d1
#
# Main outputs:
#   Comparative model-performance tables, classification-metric figures,
#   formation-frequency charts, observed home-victory rates, Wilson confidence
#   intervals, adjusted odds ratios, odds-ratio forest plots and heatmaps of
#   formation-pair frequencies and observed home-victory proportions.
#
# Important methodological note:
#   The model classification metrics are calculated using the same observations
#   employed for model estimation and should therefore be interpreted as
#   in-sample descriptive performance rather than out-of-sample predictive
#   performance. Observed formation-specific victory rates are descriptive and
#   should not be interpreted as causal effects.
# ==============================================================================

# ============================================================
# RESULTADOS DE LOS MODELOS DE UNA TEMPORADA, BASE d
# ============================================================

comparacion_modelos_d <- tibble::tibble(
  Modelo = c(
    "Ofensivo",
    "Defensivo",
    "Físico",
    "Control",
    "Contexto",
    "General"
  ),
  
  n = rep(380L, 6),
  
  Variables = c(
    9,
    6,
    4,
    10,
    7,
    14
  ),
  
  AIC = c(
    380.40,
    448.95,
    505.07,
    515.18,
    476.67,
    306.42
  ),
  
  Desviacion = c(
    360.40,
    434.95,
    495.07,
    493.18,
    460.67,
    276.42
  ),
  
  Exactitud = c(
    0.758,
    0.705,
    0.608,
    0.632,
    0.674,
    0.8394737
  ),
  
  Sensibilidad = c(
    0.698,
    0.651,
    0.444,
    0.444,
    0.592,
    0.8284024
  ),
  
  Especificidad = c(
    0.806,
    0.749,
    0.739,
    0.782,
    0.739,
    0.8483412
  )
)

print(comparacion_modelos_d)
# ============================================================
# RECUPERAR METRICAS NUMERICAS DE d1
# ============================================================

comparacion_modelos_d1_num <- comparacion_modelos_d1 %>%
  mutate(
    Exactitud_num = (VP + VN) /
      (VP + VN + FP + FN),
    
    Sensibilidad_num = VP /
      (VP + FN),
    
    Especificidad_num = VN /
      (VN + FP),
    
    Precision_num = VP /
      (VP + FP)
  )

comparacion_modelos_d1_num %>%
  select(
    Modelo,
    n,
    Variables,
    AIC,
    Exactitud_num,
    Sensibilidad_num,
    Especificidad_num
  )
str(comparacion_modelos_d)
str(comparacion_modelos_d1)
comparacion_d <- comparacion_modelos_d %>%
  mutate(
    Periodo = "Temporada 2024/2025"
  )

comparacion_d1 <- comparacion_modelos_d1 %>%
  mutate(
    Periodo = "Tres temporadas"
  )
modelos_comunes <- c(
  "Ofensivo",
  "Defensivo",
  "Físico",
  "Control",
  "Contexto",
  "General"
)

comparacion_d_d1 <- bind_rows(
  comparacion_d,
  comparacion_d1
) %>%
  filter(
    Modelo %in% modelos_comunes
  ) %>%
  mutate(
    Modelo = factor(
      Modelo,
      levels = modelos_comunes
    ),
    
    Periodo = factor(
      Periodo,
      levels = c(
        "Temporada 2024/2025",
        "Tres temporadas"
      )
    )
  )

comparacion_d_d1
# ============================================================
# PREPARAR METRICAS PARA EL GRAFICO
# ============================================================

metricas_d_d1 <- comparacion_d_d1 %>%
  select(
    Modelo,
    Periodo,
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
      ),
      labels = c(
        "Exactitud",
        "Sensibilidad para la victoria",
        "Especificidad para la no victoria"
      )
    )
  )
# ============================================================
# GRAFICO COMPARATIVO
# ============================================================

figura_metricas_d_d1 <- ggplot(
  metricas_d_d1,
  aes(
    x = Modelo,
    y = Valor,
    fill = Periodo
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.76
    ),
    width = 0.68,
    color = "white",
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = percent(
        Valor,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    position = position_dodge(
      width = 0.76
    ),
    vjust = -0.35,
    size = 2.9,
    fontface = "bold",
    color = "#333333"
  ) +
  facet_wrap(
    ~ Metrica,
    ncol = 3
  ) +
  scale_fill_manual(
    values = c(
      "Temporada 2024/2025" = "#8C96A3",
      "Tres temporadas" = "#1976A3"
    )
  ) +
  scale_y_continuous(
    limits = c(0.35, 0.95),
    breaks = seq(
      0.4,
      0.9,
      0.1
    ),
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  labs(
    title = "Comparación de la capacidad clasificadora",
    subtitle = paste(
      "Temporada 2024/2025 frente al conjunto",
      "de las tres temporadas"
    ),
    x = NULL,
    y = "Porcentaje",
    fill = "Periodo analizado",
    caption = paste(
      "Las métricas corresponden a clasificaciones internas,",
      "realizadas sobre los mismos datos utilizados",
      "para estimar cada modelo."
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
      margin = margin(b = 14)
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    
    axis.text.x = element_text(
      angle = 40,
      hjust = 1,
      size = 8.2,
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "#333333"
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.spacing = unit(
      1.2,
      "lines"
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    )
  )

print(figura_metricas_d_d1)
figura_metricas_d_d1_puntos <- ggplot(
  metricas_d_d1,
  aes(
    x = Valor,
    y = Modelo,
    color = Periodo
  )
) +
  geom_line(
    aes(
      group = interaction(
        Modelo,
        Metrica
      )
    ),
    color = "#D8D8D8",
    linewidth = 0.8
  ) +
  geom_point(
    size = 3.4,
    position = position_dodge(
      width = 0.35
    )
  ) +
  facet_wrap(
    ~ Metrica,
    ncol = 3
  ) +
  scale_color_manual(
    values = c(
      "Temporada 2024/2025" = "#8C96A3",
      "Tres temporadas" = "#1976A3"
    )
  ) +
  scale_x_continuous(
    limits = c(0.4, 0.9),
    breaks = seq(
      0.4,
      0.9,
      0.1
    ),
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Comparación de la capacidad clasificadora",
    subtitle = "Una temporada frente a tres temporadas",
    x = "Porcentaje",
    y = NULL,
    color = "Periodo analizado",
    caption = paste(
      "La proximidad entre los puntos indica estabilidad",
      "de la métrica al ampliar el periodo analizado."
    )
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 14)
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    
    axis.text = element_text(
      color = "#333333"
    ),
    
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

print(figura_metricas_d_d1_puntos)

###############################################
#Formaciones
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

frecuencia_formaciones_d1 <- bind_rows(
  
  d1 %>%
    filter(
      !is.na(formacion_local),
      formacion_local != ""
    ) %>%
    count(
      Formacion = formacion_local,
      name = "Partidos"
    ) %>%
    mutate(
      Condicion = "Equipo local"
    ),
  
  d1 %>%
    filter(
      !is.na(formacion_visit),
      formacion_visit != ""
    ) %>%
    count(
      Formacion = formacion_visit,
      name = "Partidos"
    ) %>%
    mutate(
      Condicion = "Equipo visitante"
    )
) %>%
  group_by(Formacion) %>%
  mutate(
    Total_formacion = sum(Partidos)
  ) %>%
  ungroup() %>%
  mutate(
    Formacion = fct_reorder(
      Formacion,
      Total_formacion
    ),
    
    Condicion = factor(
      Condicion,
      levels = c(
        "Equipo local",
        "Equipo visitante"
      )
    )
  )

print(frecuencia_formaciones_d1, n = Inf)
figura_frecuencia_formaciones_d1 <- ggplot(
  frecuencia_formaciones_d1,
  aes(
    x = Partidos,
    y = Formacion,
    fill = Condicion
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.68,
    color = "white",
    linewidth = 0.35
  ) +
  geom_text(
    aes(label = Partidos),
    position = position_dodge(
      width = 0.75
    ),
    hjust = -0.15,
    size = 3.2,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Equipo local" = "#1976A3",
      "Equipo visitante" = "#8C96A3"
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(0, 0.13)
    )
  ) +
  labs(
    title = "Frecuencia de utilización de las formaciones iniciales",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Número de partidos",
    y = "Formación inicial",
    fill = "Condición",
    caption = paste(
      "Las formaciones con un número reducido de observaciones",
      "deben interpretarse con especial cautela."
    )
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 14)
    ),
    
    axis.text.y = element_text(
      size = 10,
      color = "#333333"
    ),
    
    axis.text.x = element_text(
      color = "#333333"
    ),
    
    legend.position = "bottom",
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

print(figura_frecuencia_formaciones_d1)
# ============================================================
# PAQUETES
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(forcats)
library(scales)
library(stringr)
# ============================================================
# FUNCION PARA NORMALIZAR WIN_LOCAL
# ============================================================

convertir_win_local <- function(x) {
  
  x_texto <- trimws(
    as.character(x)
  )
  
  dplyr::case_when(
    x_texto %in% c(
      "1",
      "1.0",
      "Victoria local",
      "Victoria"
    ) ~ 1L,
    
    x_texto %in% c(
      "0",
      "0.0",
      "No victoria local",
      "No victoria",
      "Empate o derrota"
    ) ~ 0L,
    
    TRUE ~ NA_integer_
  )
}
# ============================================================
# LIMPIEZA DE LAS FORMACIONES EN d1
# ============================================================

d1_formaciones <- d1 %>%
  mutate(
    win_local_num = convertir_win_local(
      win_local
    ),
    
    formacion_local = as.character(
      formacion_local
    ),
    
    formacion_visit = as.character(
      formacion_visit
    ),
    
    # Quitar comillas y espacios
    formacion_local = str_replace_all(
      formacion_local,
      '"',
      ""
    ),
    
    formacion_visit = str_replace_all(
      formacion_visit,
      '"',
      ""
    ),
    
    formacion_local = str_squish(
      formacion_local
    ),
    
    formacion_visit = str_squish(
      formacion_visit
    ),
    
    # Sustituir valores vacíos por NA
    formacion_local = na_if(
      formacion_local,
      ""
    ),
    
    formacion_visit = na_if(
      formacion_visit,
      ""
    )
  ) %>%
  filter(
    !is.na(win_local_num),
    !is.na(formacion_local),
    !is.na(formacion_visit)
  )
table(
  d1_formaciones$win_local_num,
  useNA = "ifany"
)

table(
  d1_formaciones$formacion_local,
  useNA = "ifany"
)

table(
  d1_formaciones$formacion_visit,
  useNA = "ifany"
)

nrow(d1_formaciones)

# ============================================================
# INTERVALO DE CONFIANZA DE WILSON
# ============================================================

intervalo_wilson <- function(
    exitos,
    total,
    nivel = 0.95
) {
  
  z <- qnorm(
    1 - (1 - nivel) / 2
  )
  
  p <- exitos / total
  
  denominador <- 1 + z^2 / total
  
  centro <- (
    p + z^2 / (2 * total)
  ) / denominador
  
  margen <- (
    z *
      sqrt(
        p * (1 - p) / total +
          z^2 / (4 * total^2)
      )
  ) / denominador
  
  tibble(
    estimacion = p,
    inferior = pmax(
      0,
      centro - margen
    ),
    superior = pmin(
      1,
      centro + margen
    )
  )
}
# ============================================================
# TASAS DE VICTORIA POR FORMACION LOCAL
# ============================================================

victoria_formacion_local_d1 <- d1_formaciones %>%
  group_by(formacion_local) %>%
  summarise(
    partidos = n(),
    victorias = sum(
      win_local_num == 1
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intervalo = list(
      intervalo_wilson(
        exitos = victorias,
        total = partidos
      )
    )
  ) %>%
  unnest(intervalo) %>%
  ungroup() %>%
  mutate(
    condicion = "Formación local",
    formacion = formacion_local
  ) %>%
  select(
    formacion,
    condicion,
    partidos,
    victorias,
    estimacion,
    inferior,
    superior
  )
# ============================================================
# TASAS DE VICTORIA SEGUN LA FORMACION VISITANTE
# ============================================================

victoria_formacion_visit_d1 <- d1_formaciones %>%
  group_by(formacion_visit) %>%
  summarise(
    partidos = n(),
    victorias = sum(
      win_local_num == 1
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intervalo = list(
      intervalo_wilson(
        exitos = victorias,
        total = partidos
      )
    )
  ) %>%
  unnest(intervalo) %>%
  ungroup() %>%
  mutate(
    condicion = "Formación visitante",
    formacion = formacion_visit
  ) %>%
  select(
    formacion,
    condicion,
    partidos,
    victorias,
    estimacion,
    inferior,
    superior
  )
# ============================================================
# TASAS DE VICTORIA SEGUN LA FORMACION VISITANTE
# ============================================================

victoria_formacion_visit_d1 <- d1_formaciones %>%
  group_by(formacion_visit) %>%
  summarise(
    partidos = n(),
    victorias = sum(
      win_local_num == 1
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intervalo = list(
      intervalo_wilson(
        exitos = victorias,
        total = partidos
      )
    )
  ) %>%
  unnest(intervalo) %>%
  ungroup() %>%
  mutate(
    condicion = "Formación visitante",
    formacion = formacion_visit
  ) %>%
  select(
    formacion,
    condicion,
    partidos,
    victorias,
    estimacion,
    inferior,
    superior
  )
proporcion_global_d1 <- mean(
  d1_formaciones$win_local_num
)

proporcion_global_d1
figura_tasas_formaciones_d1 <- ggplot(
  tasas_formaciones_d1,
  aes(
    x = estimacion,
    y = fct_reorder(
      formacion,
      estimacion
    )
  )
) +
  
  geom_vline(
    xintercept = proporcion_global_d1,
    linetype = "dashed",
    linewidth = 0.75,
    color = "#D55E00"
  ) +
  
  geom_errorbar(
    aes(
      xmin = inferior,
      xmax = superior
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.7,
    color = "#5A5A5A"
  ) +
  
  geom_point(
    size = 3.3,
    color = "#1976A3"
  ) +
  
  geom_text(
    aes(label = etiqueta),
    hjust = -0.15,
    size = 3.1,
    color = "#333333"
  ) +
  
  facet_wrap(
    ~ condicion,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_x_continuous(
    limits = c(0, 0.85),
    breaks = seq(
      0,
      0.8,
      0.1
    ),
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0.02, 0.14)
    )
  ) +
  
  labs(
    title = "Porcentaje observado de victoria local según la formación",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Porcentaje de victoria local",
    y = "Formación inicial",
    caption = paste(
      "Las barras representan intervalos de confianza de Wilson al 95 %.",
      "La línea discontinua indica el porcentaje global de victorias locales.",
      "Solo se muestran formaciones utilizadas en al menos 10 partidos."
    )
  ) +
  
  theme_minimal(
    base_size = 12.5
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(b = 14)
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
      size = 9.5,
      color = "#333333"
    ),
    
    axis.text.x = element_text(
      color = "#333333"
    ),
    
    panel.grid.major.y = element_line(
      color = "#E8E8E8"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.spacing = unit(
      1.2,
      "lines"
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    )
  )

print(figura_tasas_formaciones_d1)


##################################################3
"4-2-3-1" %in%
  unique(
    d1_formaciones$formacion_local
  )

"4-2-3-1" %in%
  unique(
    d1_formaciones$formacion_visit
  )
d1_modelo_formaciones <- d1_formaciones %>%
  mutate(
    formacion_local = factor(
      formacion_local
    ),
    
    formacion_visit = factor(
      formacion_visit
    ),
    
    formacion_local = relevel(
      formacion_local,
      ref = "4-2-3-1"
    ),
    
    formacion_visit = relevel(
      formacion_visit,
      ref = "4-2-3-1"
    )
  )
modelo_formaciones_d1 <- glm(
  win_local_num ~
    formacion_local +
    formacion_visit,
  data = d1_modelo_formaciones,
  family = binomial(
    link = "logit"
  )
)

summary(modelo_formaciones_d1)

n_local_d1 <- d1_modelo_formaciones %>%
  count(
    formacion_local,
    name = "n"
  ) %>%
  transmute(
    condicion = "Formación local",
    formacion = as.character(
      formacion_local
    ),
    n
  )

n_visit_d1 <- d1_modelo_formaciones %>%
  count(
    formacion_visit,
    name = "n"
  ) %>%
  transmute(
    condicion = "Formación visitante",
    formacion = as.character(
      formacion_visit
    ),
    n
  )

frecuencias_modelo_formaciones_d1 <- bind_rows(
  n_local_d1,
  n_visit_d1
)

OR_formaciones_d1 <- tidy(
  modelo_formaciones_d1,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(
    term != "(Intercept)"
  ) %>%
  mutate(
    condicion = case_when(
      str_starts(
        term,
        "formacion_local"
      ) ~ "Formación local",
      
      str_starts(
        term,
        "formacion_visit"
      ) ~ "Formación visitante",
      
      TRUE ~ NA_character_
    ),
    
    formacion = case_when(
      condicion == "Formación local" ~
        str_remove(
          term,
          "^formacion_local"
        ),
      
      condicion == "Formación visitante" ~
        str_remove(
          term,
          "^formacion_visit"
        ),
      
      TRUE ~ term
    )
  ) %>%
  left_join(
    frecuencias_modelo_formaciones_d1,
    by = c(
      "condicion",
      "formacion"
    )
  ) %>%
  mutate(
    condicion = factor(
      condicion,
      levels = c(
        "Formación local",
        "Formación visitante"
      )
    ),
    
    evidencia = case_when(
      conf.low > 1 ~
        "Odds superiores a la referencia",
      
      conf.high < 1 ~
        "Odds inferiores a la referencia",
      
      TRUE ~
        "El intervalo incluye 1"
    ),
    
    etiqueta = paste0(
      formacion,
      "  (n = ",
      n,
      ")"
    )
  )
# ============================================================
# COMBINACIONES DE FORMACIONES
# ============================================================

combinaciones_formaciones_d1 <- d1_formaciones %>%
  group_by(
    formacion_local,
    formacion_visit
  ) %>%
  summarise(
    partidos = n(),
    victorias_locales = sum(
      win_local_num == 1
    ),
    proporcion_victoria = mean(
      win_local_num
    ),
    .groups = "drop"
  )
formaciones_locales_validas <- d1_formaciones %>%
  count(
    formacion_local
  ) %>%
  filter(
    n >= 10
  ) %>%
  pull(
    formacion_local
  )

formaciones_visit_validas <- d1_formaciones %>%
  count(
    formacion_visit
  ) %>%
  filter(
    n >= 10
  ) %>%
  pull(
    formacion_visit
  )
combinaciones_mapa_d1 <- combinaciones_formaciones_d1 %>%
  filter(
    formacion_local %in%
      formaciones_locales_validas,
    
    formacion_visit %in%
      formaciones_visit_validas
  ) %>%
  mutate(
    # Solo mostrar tasa si hay al menos cinco partidos
    proporcion_mostrada = ifelse(
      partidos >= 5,
      proporcion_victoria,
      NA_real_
    ),
    
    etiqueta = ifelse(
      partidos >= 5,
      
      paste0(
        percent(
          proporcion_victoria,
          accuracy = 1,
          decimal.mark = ","
        ),
        "\n",
        "n = ",
        partidos
      ),
      
      paste0(
        "n = ",
        partidos
      )
    )
  )
combinaciones_mapa_d1 <- combinaciones_mapa_d1 %>%
  mutate(
    formacion_local = factor(
      formacion_local,
      levels = rev(
        orden_local_d1
      )
    ),
    
    formacion_visit = factor(
      formacion_visit,
      levels = orden_visit_d1
    )
  )
figura_mapa_victorias_formaciones_d1 <- ggplot(
  combinaciones_mapa_d1,
  aes(
    x = formacion_visit,
    y = formacion_local,
    fill = proporcion_mostrada
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  
  geom_text(
    aes(label = etiqueta),
    size = 2.6,
    lineheight = 0.95,
    color = "#222222"
  ) +
  
  scale_fill_gradient2(
    low = "#C44E3B",
    mid = "#F4F4F4",
    high = "#1976A3",
    midpoint = proporcion_global_d1,
    limits = c(0, 1),
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    na.value = "#D9D9D9"
  ) +
  
  labs(
    title = "Victoria local según la combinación de formaciones",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Formación visitante",
    y = "Formación local",
    fill = "Porcentaje de\nvictoria local",
    caption = paste(
      "Cada celda muestra el porcentaje observado de victoria local",
      "y el número de partidos. Las combinaciones con menos de cinco",
      "encuentros aparecen en gris y solo muestran su frecuencia."
    )
  ) +
  
  coord_fixed() +
  
  theme_minimal(
    base_size = 11.5
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 14)
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8.5,
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 8.5,
      color = "#333333"
    ),
    
    panel.grid = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    )
  )

print(figura_mapa_victorias_formaciones_d1)
# ============================================================
# MAPA DE FRECUENCIAS POR COMBINACION
# ============================================================

figura_mapa_frecuencias_formaciones_d1 <- ggplot(
  combinaciones_mapa_d1,
  aes(
    x = formacion_visit,
    y = formacion_local,
    fill = partidos
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  
  geom_text(
    aes(label = partidos),
    size = 3,
    fontface = "bold",
    color = "#222222"
  ) +
  
  scale_fill_gradient(
    low = "#E8F1F8",
    high = "#1976A3",
    labels = label_number(
      accuracy = 1
    )
  ) +
  
  labs(
    title = "Número de partidos por combinación de formaciones",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Formación visitante",
    y = "Formación local",
    fill = "Número de\npartidos",
    caption = paste(
      "Las combinaciones con escasa frecuencia proporcionan",
      "estimaciones menos precisas de la proporción de victoria."
    )
  ) +
  
  coord_fixed() +
  
  theme_minimal(
    base_size = 11.5
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 14)
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8.5,
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 8.5,
      color = "#333333"
    ),
    
    panel.grid = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    )
  )

print(figura_mapa_frecuencias_formaciones_d1)
library(patchwork)

figura_mapas_formaciones_d1 <-
  figura_mapa_frecuencias_formaciones_d1 /
  figura_mapa_victorias_formaciones_d1 +
  plot_annotation(
    title = "Combinaciones entre formaciones locales y visitantes",
    caption = paste(
      "El panel superior muestra el número de encuentros",
      "y el inferior la proporción observada de victoria local."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      plot.caption = element_text(
        size = 9,
        color = "#666666",
        hjust = 0
      )
    )
  )

print(figura_mapas_formaciones_d1)
win_local_num ~ formacion_local + formacion_visit
# ============================================================
# LIMPIEZA DE FORMACIONES EN d
# ============================================================

d_formaciones <- d %>%
  mutate(
    win_local_num = convertir_win_local(
      win_local
    ),
    
    formacion_local = str_squish(
      str_replace_all(
        as.character(
          formacion_local
        ),
        '"',
        ""
      )
    ),
    
    formacion_visit = str_squish(
      str_replace_all(
        as.character(
          formacion_visit
        ),
        '"',
        ""
      )
    ),
    
    formacion_local = na_if(
      formacion_local,
      ""
    ),
    
    formacion_visit = na_if(
      formacion_visit,
      ""
    )
  ) %>%
  filter(
    !is.na(win_local_num),
    !is.na(formacion_local),
    !is.na(formacion_visit)
  ) %>%
  mutate(
    formacion_local = factor(
      formacion_local
    ),
    
    formacion_visit = factor(
      formacion_visit
    ),
    
    formacion_local = relevel(
      formacion_local,
      ref = "4-2-3-1"
    ),
    
    formacion_visit = relevel(
      formacion_visit,
      ref = "4-2-3-1"
    )
  )
modelo_formaciones_d <- glm(
  win_local_num ~
    formacion_local +
    formacion_visit,
  data = d_formaciones,
  family = binomial(
    link = "logit"
  )
)

summary(modelo_formaciones_d)
################################################3
# ============================================================
# PAQUETES
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(forcats)
library(scales)
library(stringr)
# ============================================================
# NORMALIZAR WIN_LOCAL
# ============================================================

convertir_win_local <- function(x) {
  
  x <- trimws(as.character(x))
  
  case_when(
    x %in% c(
      "1",
      "1.0",
      "Victoria local",
      "Victoria"
    ) ~ 1L,
    
    x %in% c(
      "0",
      "0.0",
      "No victoria local",
      "No victoria",
      "Empate o derrota"
    ) ~ 0L,
    
    TRUE ~ NA_integer_
  )
}


# ============================================================
# NORMALIZAR NOMBRES DE FORMACIONES
# ============================================================

normalizar_formacion <- function(x) {
  
  x <- as.character(x)
  
  # Quitar comillas
  x <- str_replace_all(
    x,
    c(
      "\"" = "",
      "'" = ""
    )
  )
  
  # Sustituir todas las variantes de guion por "-"
  x <- str_replace_all(
    x,
    "[‐‑‒–—−]",
    "-"
  )
  
  # Quitar espacios alrededor de los guiones
  x <- str_replace_all(
    x,
    "\\s*-\\s*",
    "-"
  )
  
  # Eliminar espacios repetidos y espacios exteriores
  x <- str_squish(x)
  
  # Eliminar posibles espacios invisibles
  x <- str_replace_all(
    x,
    "\u00A0",
    ""
  )
  
  # Valores no válidos
  x[x %in% c(
    "",
    "-",
    "NA",
    "N/A",
    "na",
    "NULL"
  )] <- NA_character_
  
  x
}
# ============================================================
# BASE LIMPIA PARA EL ANALISIS DE FORMACIONES
# ============================================================

d1_formaciones <- d1 %>%
  mutate(
    win_local_num = convertir_win_local(
      win_local
    ),
    
    formacion_local = normalizar_formacion(
      formacion_local
    ),
    
    formacion_visit = normalizar_formacion(
      formacion_visit
    )
  ) %>%
  filter(
    !is.na(win_local_num),
    !is.na(formacion_local),
    !is.na(formacion_visit)
  )
table(
  d1_formaciones$win_local_num,
  useNA = "ifany"
)

sort(
  unique(d1_formaciones$formacion_local)
)

sort(
  unique(d1_formaciones$formacion_visit)
)
"1-4-2-3-1" %in%
  unique(d1_formaciones$formacion_local)

"1-4-2-3-1" %in%
  unique(d1_formaciones$formacion_visit)
intervalo_wilson <- function(
    exitos,
    total,
    nivel = 0.95
) {
  
  z <- qnorm(
    1 - (1 - nivel) / 2
  )
  
  p <- exitos / total
  
  denominador <- 1 + z^2 / total
  
  centro <- (
    p + z^2 / (2 * total)
  ) / denominador
  
  margen <- (
    z *
      sqrt(
        p * (1 - p) / total +
          z^2 / (4 * total^2)
      )
  ) / denominador
  
  tibble(
    estimacion = p,
    inferior = pmax(
      0,
      centro - margen
    ),
    superior = pmin(
      1,
      centro + margen
    )
  )
}
tasas_local_d1 <- d1_formaciones %>%
  group_by(formacion_local) %>%
  summarise(
    partidos = n(),
    victorias = sum(win_local_num),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intervalo = list(
      intervalo_wilson(
        victorias,
        partidos
      )
    )
  ) %>%
  unnest(intervalo) %>%
  ungroup() %>%
  transmute(
    formacion = formacion_local,
    condicion = "Formación local",
    partidos,
    victorias,
    estimacion,
    inferior,
    superior
  )
tasas_visitante_d1 <- d1_formaciones %>%
  group_by(formacion_visit) %>%
  summarise(
    partidos = n(),
    victorias = sum(win_local_num),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intervalo = list(
      intervalo_wilson(
        victorias,
        partidos
      )
    )
  ) %>%
  unnest(intervalo) %>%
  ungroup() %>%
  transmute(
    formacion = formacion_visit,
    condicion = "Formación visitante",
    partidos,
    victorias,
    estimacion,
    inferior,
    superior
  )
tasas_formaciones_d1 <- bind_rows(
  tasas_local_d1,
  tasas_visitante_d1
) %>%
  filter(
    partidos >= 10
  ) %>%
  mutate(
    condicion = factor(
      condicion,
      levels = c(
        "Formación local",
        "Formación visitante"
      )
    ),
    
    etiqueta = paste0(
      percent(
        estimacion,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "  |  n = ",
      partidos
    )
  )

print(
  as_tibble(tasas_formaciones_d1),
  n = Inf
)
proporcion_global_d1 <- mean(
  d1_formaciones$win_local_num
)

figura_tasas_formaciones_d1 <- ggplot(
  tasas_formaciones_d1,
  aes(
    x = estimacion,
    y = reorder(
      formacion,
      estimacion
    )
  )
) +
  geom_vline(
    xintercept = proporcion_global_d1,
    linetype = "dashed",
    linewidth = 0.75,
    color = "#D55E00"
  ) +
  geom_errorbar(
    aes(
      xmin = inferior,
      xmax = superior
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.7,
    color = "#555555"
  ) +
  geom_point(
    size = 3.3,
    color = "#1976A3"
  ) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.12,
    size = 3,
    color = "#333333"
  ) +
  facet_wrap(
    ~ condicion,
    scales = "free_y",
    ncol = 2
  ) +
  scale_x_continuous(
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    breaks = seq(0, 0.8, 0.1),
    expand = expansion(
      mult = c(0.03, 0.25)
    )
  ) +
  coord_cartesian(
    xlim = c(0, 0.85),
    clip = "off"
  ) +
  labs(
    title = "Porcentaje observado de victoria local según la formación",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Porcentaje de victoria local",
    y = "Formación inicial",
    caption = paste(
      "Las barras representan intervalos de confianza de Wilson al 95 %.",
      "La línea discontinua indica el porcentaje global de victorias locales.",
      "Solo se muestran formaciones utilizadas en al menos 10 partidos."
    )
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 14)
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
      size = 9.5,
      color = "#333333"
    ),
    panel.grid.major.y = element_line(
      color = "#E8E8E8"
    ),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    ),
    plot.margin = margin(
      15,
      80,
      15,
      15
    )
  )

print(figura_tasas_formaciones_d1)
# ============================================================
# PREPARACION DEL MODELO AJUSTADO
# ============================================================

d1_modelo_formaciones <- d1_formaciones %>%
  mutate(
    formacion_local = factor(
      formacion_local
    ),
    
    formacion_visit = factor(
      formacion_visit
    )
  )
if (
  !"1-4-2-3-1" %in%
  levels(d1_modelo_formaciones$formacion_local)
) {
  stop(
    "El nivel 1-4-2-3-1 no existe en formacion_local."
  )
}

if (
  !"1-4-2-3-1" %in%
  levels(d1_modelo_formaciones$formacion_visit)
) {
  stop(
    "El nivel 1-4-2-3-1 no existe en formacion_visit."
  )
}

d1_modelo_formaciones <- d1_modelo_formaciones %>%
  mutate(
    formacion_local = relevel(
      formacion_local,
      ref = "1-4-2-3-1"
    ),
    
    formacion_visit = relevel(
      formacion_visit,
      ref = "1-4-2-3-1"
    )
  )
modelo_formaciones_d1 <- glm(
  win_local_num ~
    formacion_local +
    formacion_visit,
  data = d1_modelo_formaciones,
  family = binomial(
    link = "logit"
  )
)

summary(modelo_formaciones_d1)
        
frecuencias_local_d1 <- d1_modelo_formaciones %>%
  count(
    formacion_local,
    name = "n"
  ) %>%
  transmute(
    condicion = "Formación local",
    formacion = as.character(
      formacion_local
    ),
    n
  )

frecuencias_visit_d1 <- d1_modelo_formaciones %>%
  count(
    formacion_visit,
    name = "n"
  ) %>%
  transmute(
    condicion = "Formación visitante",
    formacion = as.character(
      formacion_visit
    ),
    n
  )

frecuencias_formaciones_d1 <- bind_rows(
  frecuencias_local_d1,
  frecuencias_visit_d1
)
OR_formaciones_d1 <- tidy(
  modelo_formaciones_d1,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(
    term != "(Intercept)"
  ) %>%
  mutate(
    condicion = case_when(
      str_starts(
        term,
        "formacion_local"
      ) ~ "Formación local",
      
      str_starts(
        term,
        "formacion_visit"
      ) ~ "Formación visitante",
      
      TRUE ~ NA_character_
    ),
    
    formacion = case_when(
      condicion == "Formación local" ~
        str_remove(
          term,
          "^formacion_local"
        ),
      
      condicion == "Formación visitante" ~
        str_remove(
          term,
          "^formacion_visit"
        ),
      
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    frecuencias_formaciones_d1,
    by = c(
      "condicion",
      "formacion"
    )
  ) %>%
  filter(
    !is.na(n),
    n >= 10
  ) %>%
  mutate(
    evidencia = case_when(
      conf.low > 1 ~
        "Odds superiores a la referencia",
      
      conf.high < 1 ~
        "Odds inferiores a la referencia",
      
      TRUE ~
        "El intervalo incluye 1"
    ),
    
    etiqueta = paste0(
      formacion,
      "  (n = ",
      n,
      ")"
    ),
    
    condicion = factor(
      condicion,
      levels = c(
        "Formación local",
        "Formación visitante"
      )
    )
  )
figura_OR_formaciones_d1 <- ggplot(
  OR_formaciones_d1,
  aes(
    x = estimate,
    y = reorder(
      etiqueta,
      estimate
    ),
    color = evidencia
  )
) +
  geom_vline(
    xintercept = 1,
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
    width = 0.16,
    linewidth = 0.7
  ) +
  geom_point(
    size = 3.3
  ) +
  facet_wrap(
    ~ condicion,
    scales = "free_y",
    ncol = 2
  ) +
  scale_x_log10(
    breaks = c(
      0.25,
      0.5,
      0.75,
      1,
      1.5,
      2,
      3,
      5,
      10
    ),
    labels = label_number(
      accuracy = 0.01,
      decimal.mark = ","
    )
  ) +
  scale_color_manual(
    values = c(
      "Odds superiores a la referencia" = "#1976A3",
      "Odds inferiores a la referencia" = "#C44E3B",
      "El intervalo incluye 1" = "#8C96A3"
    )
  ) +
  labs(
    title = "Odds ratios ajustados asociados con las formaciones",
    subtitle = "4-2-3-1 como categoría de referencia",
    x = "Odds ratio, escala logarítmica",
    y = "Formación inicial",
    color = NULL,
    caption = paste(
      "El modelo incluye simultáneamente la formación local",
      "y la formación visitante. Solo se muestran categorías",
      "utilizadas en al menos 10 encuentros."
    )
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 14)
    ),
    strip.text = element_text(
      face = "bold"
    ),
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    axis.text.y = element_text(
      size = 9,
      color = "#333333"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

print(figura_OR_formaciones_d1)
combinaciones_formaciones_d1 <- d1_formaciones %>%
  group_by(
    formacion_local,
    formacion_visit
  ) %>%
  summarise(
    partidos = n(),
    victorias_locales = sum(
      win_local_num
    ),
    proporcion_victoria = mean(
      win_local_num
    ),
    .groups = "drop"
  )
orden_local_d1 <- d1_formaciones %>%
  count(
    formacion_local,
    sort = TRUE
  ) %>%
  filter(
    n >= 10
  ) %>%
  pull(
    formacion_local
  )

orden_visit_d1 <- d1_formaciones %>%
  count(
    formacion_visit,
    sort = TRUE
  ) %>%
  filter(
    n >= 10
  ) %>%
  pull(
    formacion_visit
  )
combinaciones_mapa_d1 <- combinaciones_formaciones_d1 %>%
  filter(
    formacion_local %in%
      orden_local_d1,
    
    formacion_visit %in%
      orden_visit_d1
  ) %>%
  complete(
    formacion_local = orden_local_d1,
    formacion_visit = orden_visit_d1,
    fill = list(
      partidos = 0,
      victorias_locales = 0,
      proporcion_victoria = NA_real_
    )
  ) %>%
  mutate(
    proporcion_mostrada = ifelse(
      partidos >= 5,
      proporcion_victoria,
      NA_real_
    ),
    
    etiqueta_victoria = case_when(
      partidos >= 5 ~ paste0(
        percent(
          proporcion_victoria,
          accuracy = 1,
          decimal.mark = ","
        ),
        "\nn = ",
        partidos
      ),
      
      partidos > 0 ~ paste0(
        "n = ",
        partidos
      ),
      
      TRUE ~ ""
    ),
    
    formacion_local = factor(
      formacion_local,
      levels = rev(
        orden_local_d1
      )
    ),
    
    formacion_visit = factor(
      formacion_visit,
      levels = orden_visit_d1
    )
  )
figura_mapa_victorias_formaciones_d1 <- ggplot(
  combinaciones_mapa_d1,
  aes(
    x = formacion_visit,
    y = formacion_local,
    fill = proporcion_mostrada
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  geom_text(
    aes(label = etiqueta_victoria),
    size = 2.5,
    lineheight = 0.95
  ) +
  scale_fill_gradient2(
    low = "#C44E3B",
    mid = "#F4F4F4",
    high = "#1976A3",
    midpoint = proporcion_global_d1,
    limits = c(0, 1),
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    na.value = "#D9D9D9"
  ) +
  labs(
    title = "Victoria local según la combinación de formaciones",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Formación visitante",
    y = "Formación local",
    fill = "Porcentaje de\nvictoria local",
    caption = paste(
      "Las combinaciones con menos de cinco partidos aparecen",
      "en gris y únicamente muestran su frecuencia."
    )
  ) +
  coord_fixed() +
  theme_minimal(base_size = 11.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      color = "#555555"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ),
    axis.text.y = element_text(
      size = 8
    ),
    panel.grid = element_blank(),
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

print(figura_mapa_victorias_formaciones_d1)
######################################################
# ============================================================
# PAQUETES
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(forcats)
library(scales)
library(stringr)
# ============================================================
# NORMALIZAR WIN_LOCAL COMO 0/1
# ============================================================

convertir_win_local <- function(x) {
  
  x <- trimws(as.character(x))
  
  case_when(
    x %in% c(
      "1",
      "1.0",
      "Victoria local",
      "Victoria"
    ) ~ 1L,
    
    x %in% c(
      "0",
      "0.0",
      "No victoria local",
      "No victoria",
      "Empate o derrota"
    ) ~ 0L,
    
    TRUE ~ NA_integer_
  )
}
# ============================================================
# NORMALIZAR FORMACIONES MEDIANTE SUS DIGITOS
# ============================================================

normalizar_formacion <- function(x) {
  
  x <- as.character(x)
  
  # Eliminar caracteres que no sean números
  solo_digitos <- str_replace_all(
    x,
    "[^0-9]",
    ""
  )
  
  solo_digitos[
    solo_digitos == ""
  ] <- NA_character_
  
  # Las formaciones utilizadas deben tener
  # entre tres y cinco líneas numéricas
  solo_digitos[
    !is.na(solo_digitos) &
      !nchar(solo_digitos) %in% 3:5
  ] <- NA_character_
  
  resultado <- vapply(
    solo_digitos,
    function(valor) {
      
      if (is.na(valor)) {
        return(NA_character_)
      }
      
      digitos <- str_split(
        valor,
        pattern = "",
        simplify = TRUE
      )
      
      paste(
        digitos,
        collapse = "-"
      )
    },
    FUN.VALUE = character(1)
  )
  
  resultado
}
# ============================================================
# BASE d PARA EL ANALISIS DE FORMACIONES
# ============================================================

d_formaciones <- d %>%
  mutate(
    win_local_num = convertir_win_local(
      win_local
    ),
    
    formacion_local_original = as.character(
      formacion_local
    ),
    
    formacion_visit_original = as.character(
      formacion_visit
    ),
    
    formacion_local = normalizar_formacion(
      formacion_local_original
    ),
    
    formacion_visit = normalizar_formacion(
      formacion_visit_original
    )
  )
d_formaciones %>%
  filter(
    is.na(formacion_local) &
      !is.na(formacion_local_original)
  ) %>%
  count(
    formacion_local_original,
    sort = TRUE
  )
d_formaciones <- d_formaciones %>%
  filter(
    !is.na(win_local_num),
    !is.na(formacion_local),
    !is.na(formacion_visit)
  )
table(
  d_formaciones$win_local_num,
  useNA = "ifany"
)

sort(
  unique(d_formaciones$formacion_local)
)

sort(
  unique(d_formaciones$formacion_visit)
)

nrow(d_formaciones)
# ============================================================
# INTERVALO DE CONFIANZA DE WILSON
# ============================================================

intervalo_wilson <- function(
    exitos,
    total,
    nivel = 0.95
) {
  
  z <- qnorm(
    1 - (1 - nivel) / 2
  )
  
  p <- exitos / total
  
  denominador <- 1 +
    z^2 / total
  
  centro <- (
    p +
      z^2 / (2 * total)
  ) / denominador
  
  margen <- (
    z *
      sqrt(
        p * (1 - p) / total +
          z^2 / (4 * total^2)
      )
  ) / denominador
  
  tibble(
    estimacion = p,
    
    inferior = pmax(
      0,
      centro - margen
    ),
    
    superior = pmin(
      1,
      centro + margen
    )
  )
}
tasas_local_d <- d_formaciones %>%
  group_by(
    formacion_local
  ) %>%
  summarise(
    partidos = n(),
    
    victorias = sum(
      win_local_num == 1
    ),
    
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intervalo = list(
      intervalo_wilson(
        exitos = victorias,
        total = partidos
      )
    )
  ) %>%
  unnest(
    cols = intervalo
  ) %>%
  ungroup() %>%
  transmute(
    formacion = formacion_local,
    condicion = "Formación local",
    partidos,
    victorias,
    estimacion,
    inferior,
    superior
  )
tasas_visitante_d <- d_formaciones %>%
  group_by(
    formacion_visit
  ) %>%
  summarise(
    partidos = n(),
    
    victorias = sum(
      win_local_num == 1
    ),
    
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intervalo = list(
      intervalo_wilson(
        exitos = victorias,
        total = partidos
      )
    )
  ) %>%
  unnest(
    cols = intervalo
  ) %>%
  ungroup() %>%
  transmute(
    formacion = formacion_visit,
    condicion = "Formación visitante",
    partidos,
    victorias,
    estimacion,
    inferior,
    superior
  )
# ============================================================
# FIGURA: TASAS DE VICTORIA POR FORMACION
# ============================================================

figura_tasas_formaciones_d <- ggplot(
  tasas_formaciones_d,
  aes(
    x = estimacion,
    y = fct_reorder(
      formacion,
      estimacion
    )
  )
) +
  
  # Porcentaje global de victorias locales
  geom_vline(
    xintercept = proporcion_global_d,
    linetype = "dashed",
    linewidth = 0.75,
    color = "#D55E00"
  ) +
  
  # Intervalos de confianza de Wilson
  geom_errorbar(
    aes(
      xmin = inferior,
      xmax = superior
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.7,
    color = "#555555"
  ) +
  
  # Estimación puntual
  geom_point(
    size = 3.3,
    color = "#1976A3"
  ) +
  
  # Porcentaje y tamaño muestral
  geom_text(
    aes(label = etiqueta),
    hjust = -0.12,
    size = 3,
    color = "#333333"
  ) +
  
  facet_wrap(
    ~ condicion,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_x_continuous(
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    
    breaks = seq(
      0,
      0.9,
      0.1
    ),
    
    expand = expansion(
      mult = c(
        0.03,
        0.25
      )
    )
  ) +
  
  coord_cartesian(
    xlim = c(
      0,
      0.90
    ),
    clip = "off"
  ) +
  
  labs(
    title = paste(
      "Porcentaje observado de victoria local",
      "según la formación"
    ),
    
    subtitle = "Temporada 2024/2025",
    
    x = "Porcentaje de victoria local",
    y = "Formación inicial",
    
    caption = paste(
      "Las barras representan intervalos de confianza de Wilson al 95 %.",
      "La línea discontinua indica el porcentaje global de victorias locales.",
      "Solo se muestran formaciones utilizadas en al menos 10 partidos."
    )
  ) +
  
  theme_minimal(
    base_size = 12.5
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(
        b = 14
      )
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
      size = 9.5,
      color = "#333333"
    ),
    
    axis.text.x = element_text(
      color = "#333333"
    ),
    
    panel.grid.major.y = element_line(
      color = "#E8E8E8"
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    ),
    
    plot.margin = margin(
      t = 15,
      r = 80,
      b = 15,
      l = 15
    )
  )

print(figura_tasas_formaciones_d)
############################################3
# ============================================================
# FRECUENCIAS DE FORMACIONES LOCALES Y VISITANTES
# ============================================================
# ============================================================
# PREPARAR FACTORES Y CATEGORIA DE REFERENCIA
# ============================================================

d_modelo_formaciones <- d_formaciones %>%
  mutate(
    formacion_local = factor(
      formacion_local
    ),
    
    formacion_visit = factor(
      formacion_visit
    )
  )
d_modelo_formaciones <- d_modelo_formaciones %>%
  mutate(
    formacion_local = relevel(
      formacion_local,
      ref = "1-4-2-3-1"
    ),
    
    formacion_visit = relevel(
      formacion_visit,
      ref = "1-4-2-3-1"
    )
  )
`
frecuencias_local_d <- d_modelo_formaciones %>%
  count(
    formacion_local,
    name = "n"
  ) %>%
  transmute(
    condicion = "Formación local",
    
    formacion = as.character(
      formacion_local
    ),
    
    n
  )


frecuencias_visitante_d <- d_modelo_formaciones %>%
  count(
    formacion_visit,
    name = "n"
  ) %>%
  transmute(
    condicion = "Formación visitante",
    
    formacion = as.character(
      formacion_visit
    ),
    
    n
  )


frecuencias_formaciones_d <- bind_rows(
  frecuencias_local_d,
  frecuencias_visitante_d
)
# ============================================================
# ODDS RATIOS AJUSTADOS
# ============================================================

OR_formaciones_d <- tidy(
  modelo_formaciones_d,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(
    term != "(Intercept)"
  ) %>%
  mutate(
    condicion = case_when(
      str_starts(
        term,
        "formacion_local"
      ) ~ "Formación local",
      
      str_starts(
        term,
        "formacion_visit"
      ) ~ "Formación visitante",
      
      TRUE ~ NA_character_
    ),
    
    formacion = case_when(
      condicion == "Formación local" ~
        str_remove(
          term,
          "^formacion_local"
        ),
      
      condicion == "Formación visitante" ~
        str_remove(
          term,
          "^formacion_visit"
        ),
      
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    frecuencias_formaciones_d,
    
    by = c(
      "condicion",
      "formacion"
    )
  ) %>%
  filter(
    !is.na(n),
    n >= 10
  ) %>%
  mutate(
    evidencia = case_when(
      conf.low > 1 ~
        "Odds superiores a la referencia",
      
      conf.high < 1 ~
        "Odds inferiores a la referencia",
      
      TRUE ~
        "El intervalo incluye 1"
    ),
    
    etiqueta = paste0(
      formacion,
      "  (n = ",
      n,
      ")"
    ),
    
    condicion = factor(
      condicion,
      
      levels = c(
        "Formación local",
        "Formación visitante"
      )
    )
  )
# ============================================================
# ODDS RATIOS AJUSTADOS
# ============================================================

OR_formaciones_d <- tidy(
  modelo_formaciones_d,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(
    term != "(Intercept)"
  ) %>%
  mutate(
    condicion = case_when(
      str_starts(
        term,
        "formacion_local"
      ) ~ "Formación local",
      
      str_starts(
        term,
        "formacion_visit"
      ) ~ "Formación visitante",
      
      TRUE ~ NA_character_
    ),
    
    formacion = case_when(
      condicion == "Formación local" ~
        str_remove(
          term,
          "^formacion_local"
        ),
      
      condicion == "Formación visitante" ~
        str_remove(
          term,
          "^formacion_visit"
        ),
      
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    frecuencias_formaciones_d,
    
    by = c(
      "condicion",
      "formacion"
    )
  ) %>%
  filter(
    !is.na(n),
    n >= 10
  ) %>%
  mutate(
    evidencia = case_when(
      conf.low > 1 ~
        "Odds superiores a la referencia",
      
      conf.high < 1 ~
        "Odds inferiores a la referencia",
      
      TRUE ~
        "El intervalo incluye 1"
    ),
    
    etiqueta = paste0(
      formacion,
      "  (n = ",
      n,
      ")"
    ),
    
    condicion = factor(
      condicion,
      
      levels = c(
        "Formación local",
        "Formación visitante"
      )
    )
  )
as_tibble(
  OR_formaciones_d
) %>%
  select(
    condicion,
    formacion,
    n,
    estimate,
    conf.low,
    conf.high,
    p.value
  ) %>%
  print(n = Inf)
figura_OR_formaciones_d <- ggplot(
  OR_formaciones_d,
  aes(
    x = estimate,
    y = fct_reorder(
      etiqueta,
      estimate
    ),
    color = evidencia
  )
) +
  
  geom_vline(
    xintercept = 1,
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
    width = 0.16,
    linewidth = 0.7
  ) +
  
  geom_point(
    size = 3.3
  ) +
  
  facet_wrap(
    ~ condicion,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_x_log10(
    breaks = c(
      0.20,
      0.25,
      0.50,
      0.75,
      1,
      1.5,
      2,
      3,
      5,
      10
    ),
    
    labels = label_number(
      accuracy = 0.01,
      decimal.mark = ","
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Odds superiores a la referencia" =
        "#1976A3",
      
      "Odds inferiores a la referencia" =
        "#C44E3B",
      
      "El intervalo incluye 1" =
        "#8C96A3"
    )
  ) +
  
  labs(
    title = paste(
      "Odds ratios ajustados asociados",
      "con las formaciones"
    ),
    
    subtitle = paste(
      "1-4-2-3-1 como categoría de referencia,",
      "temporada 2024/2025"
    ),
    
    x = "Odds ratio, escala logarítmica",
    y = "Formación inicial",
    color = NULL,
    
    caption = paste(
      "El modelo incluye simultáneamente la formación local",
      "y la formación visitante. Las barras representan intervalos",
      "de confianza al 95 %. Solo se muestran formaciones",
      "utilizadas en al menos 10 encuentros."
    )
  ) +
  
  theme_minimal(
    base_size = 12.5
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(
        b = 14
      )
    ),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "#333333"
    ),
    
    axis.text.x = element_text(
      color = "#333333"
    ),
    
    legend.position = "bottom",
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

print(figura_OR_formaciones_d)

############################################
