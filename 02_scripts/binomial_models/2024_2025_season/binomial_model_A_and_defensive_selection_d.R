# ==============================================================================
# Title: Data preparation, exploratory analysis and binomial logistic models
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script imports and prepares the match-level dataset used to analyse
# home-team victories during the 2024/2025 season. It inspects the original
# data structure, assigns the appropriate variable classes, cleans the rest
# variables, constructs indicators for matches without previous rest
# information, calculates the difference in rest days between the home and
# visiting teams, and replaces unavailable recent-form observations from the
# opening matchdays with zero.
#
# The script also examines the distribution of the binary dependent variable,
# win_local, using frequency, percentage, circular and horizontal bar charts.
# These figures distinguish between home victories and matches in which the
# home team did not win.
#
# A binomial logistic regression, identified as Model A, is then estimated to
# explain the probability of a home victory. The explanatory variables include
# attacking performance, defensive performance, possession, points difference,
# rest, previous league positions and recent form. Model results are evaluated
# through coefficient estimates, odds ratios, 95% confidence intervals,
# McFadden's pseudo-R-squared, predicted probabilities, a confusion matrix and
# classification accuracy.
#
# The script produces separate coefficient and odds-ratio figures for the main
# predictors and for the categorical previous-position variables. Possession
# is rescaled in the main coefficient figure so that its effect represents an
# increase of ten percentage points.
#
# Finally, the script begins the construction of the block-based binomial
# model by estimating a complete defensive specification. Alternative
# defensive specifications are fitted after removing individual predictors
# to assess their contribution and support the subsequent variable-selection
# process.
#
# Input:
#   variables_Estudio (9).xlsx
#
# Main objects created:
#   d, df_win, modelo_A, OR_A, coef_df, coef_A_principales,
#   coef_A_posiciones, OR_A_posiciones, pred_prob_A, pred_A,
#   accuracy_A, modelo_defensivo_completo and reduced defensive models.
#
# Output:
#   Descriptive tables, diagnostic metrics, coefficient plots, distribution
#   plots and the image file Figura_distribucion_win_local_d.png.
#
# Statistical framework:
#   Binomial logistic regression with home victory as the dependent outcome.
#
# Reference category:
#   win_local = 1, corresponding to home victory.
# ==============================================================================

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

#############################################################

d %>%
  mutate(
    win_local_label = ifelse(win_local == 1, "Victoria local", "No victoria local")
  ) %>%
  ggplot(aes(x = win_local_label, fill = win_local_label)) +
  geom_bar(width = 0.6) +
  labs(
    title = "Distribución de la variable dependiente: victoria local",
    x = "",
    y = "Número de partidos",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )


df_win <- d %>%
  mutate(
    win_local_label = ifelse(win_local == 1, "Victoria local", "No victoria local")
  ) %>%
  count(win_local_label) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )

ggplot(df_win, aes(x = win_local_label, y = porcentaje, fill = win_local_label)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.4,
    size = 5
  ) +
  labs(
    title = "Porcentaje de partidos con victoria local",
    x = "",
    y = "Porcentaje de partidos",
    fill = ""
  ) +
  ylim(0, max(df_win$porcentaje) + 10) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )


df_win <- d %>%
  mutate(
    win_local_label = ifelse(win_local == 1, "Victoria local", "No victoria local")
  ) %>%
  count(win_local_label) %>%
  mutate(
    porcentaje = n / sum(n) * 100,
    etiqueta = paste0(win_local_label, "\n", round(porcentaje, 1), "%")
  )

ggplot(df_win, aes(x = 2, y = porcentaje, fill = win_local_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = "Distribución de la victoria local",
    fill = ""
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#############################################################
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

# ============================================================
# FIGURA PRINCIPAL DEL MODELO A
# VARIABLES CONTINUAS Y BINARIAS
# ============================================================

library(dplyr)
library(ggplot2)
library(broom)
library(forcats)
library(scales)

# Extraer coeficientes e intervalos de confianza
coef_A_principales <- tidy(
  modelo_A,
  conf.int = TRUE,
  exponentiate = FALSE
) %>%
  filter(term != "(Intercept)") %>%
  
  # Las posiciones siguen incluidas en el modelo,
  # pero se representarán en otra figura
  filter(
    !grepl("^pos_previa_local", term),
    !grepl("^pos_previa_visitante", term)
  ) %>%
  
  # Reescalar visualmente la posesión:
  # si está expresada entre 0 y 1, el coeficiente original
  # representa un cambio de 100 puntos porcentuales.
  mutate(
    estimate = if_else(
      term == "posesion_local",
      estimate * 0.10,
      estimate
    ),
    conf.low = if_else(
      term == "posesion_local",
      conf.low * 0.10,
      conf.low
    ),
    conf.high = if_else(
      term == "posesion_local",
      conf.high * 0.10,
      conf.high
    )
  ) %>%
  
  mutate(
    variable = recode(
      term,
      "tiros_puerta_local" =
        "Tiros a puerta locales",
      "xG_local" =
        "Goles esperados locales (xG)",
      "xGA_local" =
        "Goles esperados concedidos (xGA)",
      "tiros_puerta_concedidos_local" =
        "Tiros a puerta concedidos",
      "posesion_local" =
        "Posesión local (+10 puntos porcentuales)",
      "diferencia_puntos_local" =
        "Diferencia de puntos",
      "diff_descanso" =
        "Diferencia de descanso",
      "no_descanso_previo" =
        "Ausencia de descanso previo",
      "forma_local_5" =
        "Forma reciente local",
      "forma_visitante_5" =
        "Forma reciente visitante"
    ),
    
    # Identificar si el intervalo excluye el cero
    evidencia = case_when(
      conf.low > 0 ~ "Asociación positiva",
      conf.high < 0 ~ "Asociación negativa",
      TRUE ~ "Intervalo incluye el cero"
    ),
    
    # Ordenar de menor a mayor coeficiente
    variable = fct_reorder(variable, estimate)
  )

print(coef_A_principales)
grafico_coef_A_principales <- ggplot(
  coef_A_principales,
  aes(
    x = estimate,
    y = variable,
    color = evidencia
  )
) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = "#666666"
  ) +
  
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.75
  ) +
  
  geom_point(
    size = 3.2
  ) +
  
  scale_color_manual(
    values = c(
      "Asociación positiva" = "#1976A3",
      "Asociación negativa" = "#C45A4A",
      "Intervalo incluye el cero" = "#8C96A3"
    )
  ) +
  
  scale_x_continuous(
    breaks = pretty_breaks(n = 7),
    expand = expansion(mult = c(0.06, 0.08))
  ) +
  
  labs(
    title = "Coeficientes estimados del Modelo A",
    subtitle = "Regresión logística binomial, temporada 2024/2025",
    x = "Coeficiente estimado (log-odds)",
    y = NULL,
    color = NULL,
    caption = paste(
      "Las barras representan intervalos de confianza al 95 %.",
      "La posesión se expresa mediante incrementos de 10 puntos porcentuales.",
      "Las posiciones previas se muestran en una figura independiente."
    )
  ) +
  
  theme_minimal(base_size = 12.5) +
  
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = "#1F1F1F"
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(b = 14)
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    ),
    
    axis.title.x = element_text(
      size = 11.5,
      margin = margin(t = 10)
    ),
    
    axis.text.y = element_text(
      size = 10.5,
      color = "#2B2B2B"
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "#4D4D4D"
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
    
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 9.5
    ),
    
    plot.margin = margin(
      t = 18,
      r = 20,
      b = 15,
      l = 15
    )
  )

print(grafico_coef_A_principales)
# ============================================================
# COEFICIENTES DE LAS POSICIONES PREVIAS
# ============================================================

coef_A_posiciones <- tidy(
  modelo_A,
  conf.int = TRUE,
  exponentiate = FALSE
) %>%
  
  filter(
    grepl(
      "^pos_previa_local|^pos_previa_visitante",
      term
    )
  ) %>%
  
  mutate(
    equipo = case_when(
      grepl("^pos_previa_local", term) ~
        "Posición previa local",
      
      grepl("^pos_previa_visitante", term) ~
        "Posición previa visitante"
    ),
    
    posicion = case_when(
      grepl("^pos_previa_local", term) ~
        sub("^pos_previa_local", "", term),
      
      grepl("^pos_previa_visitante", term) ~
        sub("^pos_previa_visitante", "", term)
    ),
    
    posicion = as.numeric(posicion),
    
    evidencia = case_when(
      conf.low > 0 ~ "Asociación positiva",
      conf.high < 0 ~ "Asociación negativa",
      TRUE ~ "Intervalo incluye el cero"
    )
  )

print(coef_A_posiciones)
grafico_coef_A_posiciones <- ggplot(
  coef_A_posiciones,
  aes(
    x = posicion,
    y = estimate,
    color = evidencia
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = "#666666"
  ) +
  
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.22,
    linewidth = 0.65
  ) +
  
  geom_point(
    size = 2.8
  ) +
  
  facet_wrap(
    ~ equipo,
    ncol = 1,
    scales = "free_y"
  ) +
  
  scale_color_manual(
    values = c(
      "Asociación positiva" = "#1976A3",
      "Asociación negativa" = "#C45A4A",
      "Intervalo incluye el cero" = "#8C96A3"
    )
  ) +
  
  scale_x_continuous(
    breaks = 2:20
  ) +
  
  labs(
    title = "Efecto de la posición previa sobre la victoria local",
    subtitle = "Comparación respecto a la primera posición de la clasificación",
    x = "Posición previa",
    y = "Coeficiente estimado (log-odds)",
    color = NULL,
    caption = paste(
      "Las barras representan intervalos de confianza al 95 %.",
      "La primera posición constituye la categoría de referencia y,",
      "por tanto, no dispone de un coeficiente propio."
    )
  ) +
  
  theme_minimal(base_size = 12.5) +
  
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = "#1F1F1F"
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(b = 12)
    ),
    
    strip.text = element_text(
      size = 11.5,
      face = "bold",
      color = "#333333"
    ),
    
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    
    axis.text.x = element_text(
      size = 8.5
    ),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    ),
    
    plot.margin = margin(
      t = 18,
      r = 20,
      b = 15,
      l = 15
    )
  )

print(grafico_coef_A_posiciones)

OR_A_posiciones <- tidy(
  modelo_A,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  
  filter(
    grepl(
      "^pos_previa_local|^pos_previa_visitante",
      term
    )
  ) %>%
  
  mutate(
    equipo = case_when(
      grepl("^pos_previa_local", term) ~
        "Posición previa local",
      
      grepl("^pos_previa_visitante", term) ~
        "Posición previa visitante"
    ),
    
    posicion = case_when(
      grepl("^pos_previa_local", term) ~
        sub("^pos_previa_local", "", term),
      
      grepl("^pos_previa_visitante", term) ~
        sub("^pos_previa_visitante", "", term)
    ),
    
    posicion = as.numeric(posicion),
    
    evidencia = case_when(
      conf.low > 1 ~ "OR superior a 1",
      conf.high < 1 ~ "OR inferior a 1",
      TRUE ~ "El intervalo incluye 1"
    )
  )
grafico_OR_A_posiciones <- ggplot(
  OR_A_posiciones,
  aes(
    x = posicion,
    y = estimate,
    color = evidencia
  )
) +
  
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "#666666"
  ) +
  
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.22,
    linewidth = 0.65
  ) +
  
  geom_point(
    size = 2.8
  ) +
  
  facet_wrap(
    ~ equipo,
    ncol = 1,
    scales = "free_y"
  ) +
  
  scale_y_log10(
    labels = label_number(accuracy = 0.01)
  ) +
  
  scale_x_continuous(
    breaks = 2:20
  ) +
  
  scale_color_manual(
    values = c(
      "OR superior a 1" = "#1976A3",
      "OR inferior a 1" = "#C45A4A",
      "El intervalo incluye 1" = "#8C96A3"
    )
  ) +
  
  labs(
    title = "Odds ratios asociados con la posición previa",
    subtitle = "Primera posición de la clasificación como referencia",
    x = "Posición previa",
    y = "Odds ratio, escala logarítmica",
    color = NULL,
    caption = paste(
      "Las barras representan intervalos de confianza al 95 %.",
      "Los valores superiores a 1 indican mayores odds de victoria local",
      "respecto a la primera posición; los inferiores indican menores odds."
    )
  ) +
  
  theme_minimal(base_size = 12.5) +
  
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(b = 12)
    ),
    
    strip.text = element_text(
      size = 11.5,
      face = "bold"
    ),
    
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    
    axis.text.x = element_text(
      size = 8.5
    ),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    )
  )

print(grafico_OR_A_posiciones)
# ============================================================
# DISTRIBUCION DE WIN_LOCAL
# FRECUENCIA Y PORCENTAJE EN UNA SOLA FIGURA
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)

# Comprobar primero los niveles de la variable
levels(d$win_local)
table(d$win_local)


# Crear la tabla de frecuencias
df_win <- d %>%
  mutate(
    resultado_binario = case_when(
      as.character(win_local) %in% c("1", "Victoria local") ~
        "Victoria local",
      
      as.character(win_local) %in% c("0", "No victoria local") ~
        "No victoria local",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(resultado_binario)) %>%
  count(resultado_binario, name = "partidos") %>%
  mutate(
    porcentaje = partidos / sum(partidos),
    resultado_binario = factor(
      resultado_binario,
      levels = c(
        "No victoria local",
        "Victoria local"
      )
    ),
    etiqueta = paste0(
      partidos,
      " partidos\n",
      number(
        porcentaje * 100,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      " %"
    )
  )


# Comprobar que los resultados son correctos
df_win
sum(df_win$partidos)
sum(df_win$porcentaje)

grafico_distribucion_d <- ggplot(
  df_win,
  aes(
    x = resultado_binario,
    y = partidos,
    fill = resultado_binario
  )
) +
  
  geom_col(
    width = 0.58,
    color = "white",
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(label = etiqueta),
    vjust = -0.35,
    lineheight = 1.05,
    size = 4.2,
    fontface = "bold",
    color = "#2B2B2B"
  ) +
  
  # Paleta sobria y apta para impresión
  scale_fill_manual(
    values = c(
      "No victoria local" = "#8C96A3",
      "Victoria local" = "#1976A3"
    )
  ) +
  
  # Margen superior suficiente para las etiquetas
  scale_y_continuous(
    limits = c(
      0,
      max(df_win$partidos) * 1.20
    ),
    breaks = pretty_breaks(n = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = "Distribución de la victoria local",
    subtitle = "Temporada 2024/2025",
    x = NULL,
    y = "Número de partidos",
    caption = paste0(
      "Nota: la categoría «No victoria local» agrupa ",
      "los empates y las derrotas del equipo local. ",
      "N = ",
      sum(df_win$partidos),
      "."
    )
  ) +
  
  guides(fill = "none") +
  
  theme_minimal(base_size = 12.5) +
  
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = "#1F1F1F",
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(b = 14)
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    ),
    
    axis.title.y = element_text(
      size = 11.5,
      color = "#333333",
      margin = margin(r = 10)
    ),
    
    axis.text.x = element_text(
      size = 11.5,
      color = "#2B2B2B",
      margin = margin(t = 8)
    ),
    
    axis.text.y = element_text(
      size = 10,
      color = "#4D4D4D"
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "#E6E6E6",
      linewidth = 0.45
    ),
    
    plot.margin = margin(
      t = 18,
      r = 20,
      b = 15,
      l = 15
    )
  )

grafico_distribucion_d

ggsave(
  filename = "Figura_distribucion_win_local_d.png",
  plot = grafico_distribucion_d,
  width = 8.5,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# ============================================================
# DISTRIBUCION DE WIN_LOCAL EN LA BASE d
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)

# Comprobación previa
table(d$win_local, useNA = "ifany")
levels(d$win_local)

# Crear de nuevo el objeto df_win
# Funciona si win_local está codificada como 0/1
# o como "No victoria local"/"Victoria local"

df_win <- d %>%
  mutate(
    resultado_binario = case_when(
      as.character(win_local) %in%
        c("1", "Victoria local") ~ "Victoria local",
      
      as.character(win_local) %in%
        c("0", "No victoria local") ~ "No victoria local",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(resultado_binario)) %>%
  count(resultado_binario, name = "partidos") %>%
  mutate(
    porcentaje = partidos / sum(partidos),
    
    resultado_binario = factor(
      resultado_binario,
      levels = c(
        "No victoria local",
        "Victoria local"
      )
    ),
    
    etiqueta = paste0(
      partidos,
      " partidos  |  ",
      number(
        porcentaje * 100,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      " %"
    )
  )

# Verificar el resultado
print(df_win)
grafico_distribucion_horizontal_d <- ggplot(
  data = df_win,
  aes(
    x = partidos,
    y = resultado_binario,
    fill = resultado_binario
  )
) +
  geom_col(
    width = 0.52,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.08,
    size = 4.2,
    fontface = "bold",
    color = "#2B2B2B"
  ) +
  scale_fill_manual(
    values = c(
      "No victoria local" = "#8C96A3",
      "Victoria local" = "#1976A3"
    )
  ) +
  scale_x_continuous(
    limits = c(
      0,
      max(df_win$partidos) * 1.35
    ),
    breaks = pretty_breaks(n = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Distribución de la victoria local",
    subtitle = "Temporada 2024/2025",
    x = "Número de partidos",
    y = NULL,
    caption = paste0(
      "Nota: la categoría «No victoria local» agrupa ",
      "los empates y las derrotas. N = ",
      sum(df_win$partidos),
      "."
    )
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = "#1F1F1F"
    ),
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(b = 14)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11.5,
      color = "#333333",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 11.5,
      color = "#2B2B2B"
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#4D4D4D"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "#E6E6E6",
      linewidth = 0.45
    ),
    plot.margin = margin(
      t = 18,
      r = 40,
      b = 15,
      l = 15
    )
  )

print(grafico_distribucion_horizontal_d)











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

#sin tiros concedidos

modelo_defensivo_completo1 <- glm(
  win_local ~ 
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

summary(modelo_defensivo_completo1)

#sin tiros puerta

modelo_defensivo_completo2 <- glm(
  win_local ~ 
    tiros_concedidos_local +
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

summary(modelo_defensivo_completo2)

#sin xGA

modelo_defensivo_completo3 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
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

summary(modelo_defensivo_completo3)

#sin big chances

modelo_defensivo_completo4 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
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

summary(modelo_defensivo_completo4)

#sin pases

modelo_defensivo_completo5 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
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

summary(modelo_defensivo_completo5)

#sin faltas

modelo_defensivo_completo6 <- glm(
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

summary(modelo_defensivo_completo6)

#sin amarillas

modelo_defensivo_completo7 <- glm(
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

summary(modelo_defensivo_completo7)

#sin rojas

modelo_defensivo_completo8 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    amarillas_local + 
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

summary(modelo_defensivo_completo8)

#sin min roja

modelo_defensivo_completo9 <- glm(
  win_local ~ 
    tiros_concedidos_local +
    tiros_puerta_concedidos_local + 
    xGA_local + 
    big_chances_concedidas_local + 
    pases_en_contra_local + 
    faltas_local + 
    amarillas_local + 
    rojas_local + 
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

summary(modelo_defensivo_completo9)

#sin min ventaja roja

modelo_defensivo_completo10 <- glm(
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

summary(modelo_defensivo_completo10)

#sin corners

modelo_defensivo_completo11 <- glm(
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

summary(modelo_defensivo_completo11)

#sin entradas

modelo_defensivo_completo12 <- glm(
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

summary(modelo_defensivo_completo12)

#sin tiros libres

modelo_defensivo_completo13 <- glm(
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

summary(modelo_defensivo_completo13)
