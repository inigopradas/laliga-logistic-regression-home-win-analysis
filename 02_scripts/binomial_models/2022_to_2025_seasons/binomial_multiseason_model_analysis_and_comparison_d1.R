# ==============================================================================
# Title: Multi-season binomial model analysis and comparison
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops, evaluates and compares binomial logistic regression
# models for home-team victory using match-level data from the 2022/2023,
# 2023/2024 and 2024/2025 LaLiga seasons.
#
# The script imports and prepares the multi-season dataset, converts the
# relevant categorical variables into factors and cleans the rest and recent-
# form variables. It creates an indicator for matches without previous rest
# information, imputes structural missing values for the calculation of rest
# differences and constructs the difference in rest days between the home and
# visiting teams.
#
# The binary dependent variable distinguishes between a home-team victory and
# no home-team victory, with the latter category including draws and away-team
# victories. The response is recoded into both numerical and labelled versions
# to ensure consistent model estimation, evaluation and graphical
# representation.
#
# Model A is estimated using attacking performance, defensive performance,
# possession, rest, previous league positions and recent form. The script
# calculates coefficient estimates, odds ratios, 95% confidence intervals,
# pseudo-R-squared statistics, predicted probabilities and predicted outcome
# classes.
#
# Model A classification performance is evaluated through a confusion matrix,
# overall accuracy, sensitivity, specificity, precision and balanced accuracy.
# The script also produces coefficient and odds-ratio plots in which possession
# is rescaled to represent an increase of ten percentage points.
#
# The distribution of the binary dependent variable is examined using
# horizontal and vertical bar charts. The multi-season distribution is also
# compared graphically with the distribution observed in the single-season
# dataset.
#
# The script assigns definitive names to Model A and to the selected offensive,
# defensive, physical, match-control, contextual and general binomial models.
# A reusable evaluation function extracts the sample size, number of variables,
# AIC, residual deviance, confusion-matrix components and classification
# metrics for each model.
#
# The selected models are compared using AIC, residual deviance, accuracy,
# sensitivity and specificity. Publication-ready figures are generated to
# represent differences in model fit and classification performance.
#
# Finally, the script reconstructs the variable-selection trajectories of the
# offensive, defensive and general models. It records the number of predictors,
# AIC, residual deviance and sample size at each stage and produces graphical
# representations of the evolution of AIC during the model-reduction process.
#
# Input:
#   LaLiga_22-25_completo_v2 (2).xlsx
#
# Dataset:
#   d1, containing match-level observations from the 2022/2023, 2023/2024 and
#   2024/2025 LaLiga seasons.
#
# Dependent variable:
#   win_local_num
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory, including draws and away-team victories
#
# Statistical method:
#   Binomial logistic regression estimated with glm() and a logit link.
#
# Main reference model:
#   modelo_A_d1
#
# Final models compared:
#   modelo_A_final_d1
#   modelo_ofensivo_final_d1
#   modelo_defensivo_final_d1
#   modelo_fisico_final_d1
#   modelo_control_final_d1
#   modelo_contexto_final_d1
#   modelo_general_final_d1
#
# Main outputs:
#   Coefficient and odds-ratio tables, confidence intervals, pseudo-R-squared,
#   predicted probabilities, confusion matrices, classification metrics,
#   dependent-variable distribution charts, model-comparison figures and
#   graphical summaries of the AIC evolution during variable selection.
#
# Important methodological note:
#   The classification metrics are calculated using the same observations
#   employed to estimate each model and therefore represent in-sample
#   performance rather than out-of-sample predictive performance.
# ==============================================================================

library(readxl)   # para leer Excel
library(dplyr)    # para manipular datos

getwd()
d1 <- read_excel("LaLiga_22-25_completo_v2 (2).xlsx")

d1$win_local = factor(d1$win_local)
d1$temporada = factor(d1$temporada)
d1$formacion_local = factor(d1$formacion_local)
d1$formacion_visit = factor(d1$formacion_visit)

levels(d1$temporada)

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


d1_modelo <- d1 %>%
  select(
    win_local,
    tiros_puerta_local,
    xG_local,
    xGA_local,
    tiros_puerta_concedidos_local,
    posesion_local,
    diff_descanso,
    no_descanso_previo,
    pos_previa_local,
    pos_previa_visitante,
    forma_local_5,
    forma_visitante_5
  ) %>%
  na.omit()

modelo_logit <- glm(
  win_local ~ tiros_puerta_local +
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
  data = d1_modelo,
  family = binomial(link = "logit")
)

summary(modelo_logit)



# =========================
# LIBRERÍAS
# =========================
library(broom)
library(ggplot2)
library(pscl)

# =========================
# 1. TABLA DE ODDS RATIOS + IC 95%
# =========================
OR_logit <- tidy(modelo_logit, conf.int = TRUE, exponentiate = TRUE)

# Si no quieres mostrar el intercepto:
OR_logit_sin_intercepto <- OR_logit[OR_logit$term != "(Intercept)", ]

# Ordenamos por importancia (mejor por distancia respecto a 1)
OR_logit_sin_intercepto[order(abs(log(OR_logit_sin_intercepto$estimate)), decreasing = TRUE), ]

# =========================
# 2. GRÁFICO DE COEFICIENTES (log-odds)
# =========================
coef_df <- tidy(modelo_logit, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Coeficientes del Modelo Logístico – Victoria local",
    x = "Coeficiente (log-odds)",
    y = "Variables"
  ) +
  theme_minimal(base_size = 13)

#=========================
# 3. PSEUDO R²
# =========================
pR2(modelo_logit)

# =========================
# 4. PROBABILIDADES PREDICHAS
# =========================
pred_prob_logit <- predict(modelo_logit, type = "response")
pred_prob_logit

# =========================
# 5. CLASIFICACIÓN (0/1)
# =========================
pred_logit <- ifelse(pred_prob_logit >= 0.5, 1, 0)
pred_logit


# =========================
# 6. MATRIZ DE CONFUSIÓN
# =========================
# Usamos exactamente las filas que ha utilizado glm()
datos_eval <- model.frame(modelo_logit)

table(real = datos_eval$win_local, pred = pred_logit)


# =========================
# 7. ACCURACY
# =========================
accuracy_logit <- mean(pred_logit == datos_eval$win_local)
accuracy_logit

modelo_A_d1 <- modelo_logit

modelo_defensivo_final_d1 <- modelo_defensivo17
modelo_ofensivo_final_d1 <- modelo_ofensivo14
modelo_fisico_final_d1 <- modelo_fisico10
modelo_control_final_d1 <- modelo_control2
modelo_contexto_final_d1 <- modelo_contexto5
modelo_general_completo_d1 <- modelo_general
modelo_general_final_d1 <- modelo_general8
datos_eval <- model.frame(modelo_general_final_d1)
real_eval <- model.response(datos_eval)

prob_eval <- predict(
  modelo_general_final_d1,
  type = "response"
)

pred_eval <- ifelse(
  prob_eval >= 0.5,
  1,
  0
)

table(
  Real = real_eval,
  Predicho = pred_eval
)

mean(
  pred_eval ==
    as.numeric(as.character(real_eval))
)
# ============================================================
# DISTRIBUCION DE WIN_LOCAL EN d1
# ============================================================

df_win_d1 <- d1_modelo %>%
  count(
    resultado_binario = win_local,
    name = "partidos"
  ) %>%
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

print(df_win_d1)
sum(df_win_d1$partidos)

grafico_distribucion_horizontal_d1 <- ggplot(
  df_win_d1,
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
      max(df_win_d1$partidos) * 1.31
    ),
    breaks = pretty_breaks(n = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Distribución de la victoria local",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Número de partidos",
    y = NULL,
    caption = paste0(
      "Nota: la categoría «No victoria local» agrupa ",
      "los empates y las derrotas. N = ",
      sum(df_win_d1$partidos),
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
      r = 45,
      b = 15,
      l = 15
    )
  )

print(grafico_distribucion_horizontal_d1)
library(patchwork)

figura_distribucion_d_d1 <-
  grafico_distribucion_horizontal_d +
  grafico_distribucion_horizontal_d1 +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "Distribución de la victoria local según el periodo analizado",
    caption = paste(
      "La categoría «No victoria local» agrupa",
      "los empates y las derrotas."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 15
      ),
      plot.caption = element_text(
        size = 9,
        color = "#666666",
        hjust = 0
      )
    )
  )

print(figura_distribucion_d_d1)

# ============================================================
# COEFICIENTES DEL MODELO A DE TRES TEMPORADAS
# ============================================================

coef_A_d1 <- tidy(
  modelo_A_d1,
  conf.int = TRUE,
  exponentiate = FALSE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # El coeficiente original de posesión representa
    # un incremento de 100 puntos porcentuales.
    # Se transforma para interpretar 10 puntos porcentuales.
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
    ),
    
    Variable = recode(
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
      "diff_descanso" =
        "Diferencia de descanso",
      "no_descanso_previo" =
        "Ausencia de descanso previo",
      "pos_previa_local" =
        "Posición previa local",
      "pos_previa_visitante" =
        "Posición previa visitante",
      "forma_local_5" =
        "Forma reciente local",
      "forma_visitante_5" =
        "Forma reciente visitante"
    ),
    
    Dimension = case_when(
      term %in% c(
        "tiros_puerta_local",
        "xG_local"
      ) ~ "Producción ofensiva",
      
      term %in% c(
        "xGA_local",
        "tiros_puerta_concedidos_local"
      ) ~ "Amenaza concedida",
      
      term == "posesion_local" ~
        "Control del balón",
      
      term %in% c(
        "diff_descanso",
        "no_descanso_previo",
        "pos_previa_local",
        "pos_previa_visitante",
        "forma_local_5",
        "forma_visitante_5"
      ) ~ "Contexto competitivo",
      
      TRUE ~ "Otras variables"
    ),
    
    Evidencia = case_when(
      conf.low > 0 ~ "Asociación positiva",
      conf.high < 0 ~ "Asociación negativa",
      TRUE ~ "El intervalo incluye cero"
    ),
    
    Variable = fct_reorder(
      Variable,
      estimate
    )
  )

print(coef_A_d1, n = Inf)
grafico_coef_A_d1 <- ggplot(
  coef_A_d1,
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
      "Producción ofensiva" = "#1976A3",
      "Amenaza concedida" = "#D55E00",
      "Control del balón" = "#009E73",
      "Contexto competitivo" = "#8E5EA2",
      "Otras variables" = "#8C96A3"
    )
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 7),
    expand = expansion(
      mult = c(0.07, 0.08)
    )
  ) +
  labs(
    title = "Coeficientes estimados del Modelo A",
    subtitle = "Regresión logística binomial, tres temporadas",
    x = "Coeficiente estimado (log-odds)",
    y = NULL,
    color = "Dimensión",
    caption = paste(
      "Los puntos representan los coeficientes estimados",
      "y las barras sus intervalos de confianza al 95 %.",
      "La posesión se expresa mediante incrementos de 10 puntos porcentuales."
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
      color = "#4D4D4D"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
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
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    ),
    plot.margin = margin(
      t = 18,
      r = 25,
      b = 15,
      l = 15
    )
  )

print(grafico_coef_A_d1)
OR_A_d1_grafico <- coef_A_d1 %>%
  mutate(
    odds_ratio = exp(estimate),
    OR_inferior = exp(conf.low),
    OR_superior = exp(conf.high),
    Variable = fct_reorder(
      Variable,
      odds_ratio
    )
  )
grafico_OR_A_d1 <- ggplot(
  OR_A_d1_grafico,
  aes(
    x = odds_ratio,
    y = Variable,
    color = Dimension
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
      xmin = OR_inferior,
      xmax = OR_superior
    ),
    orientation = "y",
    width = 0.15,
    linewidth = 0.75,
    color = "#454545"
  ) +
  geom_point(
    size = 3.4
  ) +
  scale_x_log10(
    breaks = c(
      0.25, 0.5, 0.75, 1,
      1.25, 1.5, 2, 3
    ),
    labels = label_number(
      accuracy = 0.01,
      decimal.mark = ","
    )
  ) +
  scale_color_manual(
    values = c(
      "Producción ofensiva" = "#1976A3",
      "Amenaza concedida" = "#D55E00",
      "Control del balón" = "#009E73",
      "Contexto competitivo" = "#8E5EA2",
      "Otras variables" = "#8C96A3"
    )
  ) +
  labs(
    title = "Odds ratios del Modelo A",
    subtitle = "Regresión logística binomial, tres temporadas",
    x = "Odds ratio, escala logarítmica",
    y = NULL,
    color = "Dimensión",
    caption = paste(
      "La línea discontinua marca OR = 1.",
      "La posesión se expresa mediante incrementos",
      "de 10 puntos porcentuales."
    )
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      color = "#555555"
    ),
    axis.text.y = element_text(
      size = 10.2,
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

print(grafico_OR_A_d1)
# ============================================================
# MATRIZ DE CONFUSION DEL MODELO A
# ============================================================

datos_eval_A_d1 <- model.frame(modelo_A_d1)
real_A_d1 <- model.response(datos_eval_A_d1)

prob_A_d1 <- predict(
  modelo_A_d1,
  type = "response"
)

pred_A_d1 <- ifelse(
  prob_A_d1 >= 0.5,
  "Victoria local",
  "No victoria local"
)

pred_A_d1 <- factor(
  pred_A_d1,
  levels = c(
    "No victoria local",
    "Victoria local"
  )
)

real_A_d1 <- factor(
  as.character(real_A_d1),
  levels = c(
    "No victoria local",
    "Victoria local"
  )
)

matriz_A_d1 <- table(
  Real = real_A_d1,
  Predicho = pred_A_d1
)

print(matriz_A_d1)
VN_A <- matriz_A_d1[
  "No victoria local",
  "No victoria local"
]

FP_A <- matriz_A_d1[
  "No victoria local",
  "Victoria local"
]

FN_A <- matriz_A_d1[
  "Victoria local",
  "No victoria local"
]

VP_A <- matriz_A_d1[
  "Victoria local",
  "Victoria local"
]


exactitud_A_d1 <- (
  VP_A + VN_A
) / sum(matriz_A_d1)

sensibilidad_A_d1 <- VP_A / (
  VP_A + FN_A
)

especificidad_A_d1 <- VN_A / (
  VN_A + FP_A
)

precision_A_d1 <- VP_A / (
  VP_A + FP_A
)

exactitud_equilibrada_A_d1 <- mean(
  c(
    sensibilidad_A_d1,
    especificidad_A_d1
  )
)

metricas_A_d1 <- tibble(
  Metrica = c(
    "Exactitud",
    "Sensibilidad",
    "Especificidad",
    "Precisión",
    "Exactitud equilibrada"
  ),
  Valor = c(
    exactitud_A_d1,
    sensibilidad_A_d1,
    especificidad_A_d1,
    precision_A_d1,
    exactitud_equilibrada_A_d1
  )
) %>%
  mutate(
    Porcentaje = percent(
      Valor,
      accuracy = 0.1,
      decimal.mark = ","
    )
  )

print(metricas_A_d1)
df_matriz_A_d1 <- as.data.frame(
  matriz_A_d1
) %>%
  group_by(Real) %>%
  mutate(
    Porcentaje = Freq / sum(Freq),
    
    Etiqueta = paste0(
      Freq,
      "\n",
      percent(
        Porcentaje,
        accuracy = 0.1,
        decimal.mark = ","
      )
    )
  ) %>%
  ungroup()
grafico_matriz_A_d1 <- ggplot(
  df_matriz_A_d1,
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
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#E8F1F8",
    high = "#1976A3",
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    )
  ) +
  coord_equal() +
  labs(
    title = "Matriz de confusión del Modelo A",
    subtitle = "Regresión logística binomial, tres temporadas",
    x = "Resultado predicho",
    y = "Resultado observado",
    fill = "Porcentaje\npor resultado real",
    caption = paste0(
      "Exactitud = ",
      percent(
        exactitud_A_d1,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "; sensibilidad = ",
      percent(
        sensibilidad_A_d1,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "; especificidad = ",
      percent(
        especificidad_A_d1,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "."
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
      margin = margin(b = 12)
    ),
    panel.grid = element_blank(),
    axis.text = element_text(
      color = "black",
      size = 10.5
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      color = "#666666"
    )
  )

print(grafico_matriz_A_d1)
# Comprobar los valores originales
unique(d1$win_local)
table(d1$win_local, useNA = "ifany")
levels(d1$win_local)
# ============================================================
# RECODIFICACION SEGURA DE WIN_LOCAL
# ============================================================

d1 <- d1 %>%
  mutate(
    win_local_num = case_when(
      
      # Codificación numérica o textual 0/1
      as.character(win_local) %in%
        c("1", "1.0") ~ 1L,
      
      as.character(win_local) %in%
        c("0", "0.0") ~ 0L,
      
      # Por si ya existieran etiquetas
      as.character(win_local) %in%
        c("Victoria local", "Victoria") ~ 1L,
      
      as.character(win_local) %in%
        c(
          "No victoria local",
          "No victoria",
          "Empate o derrota"
        ) ~ 0L,
      
      TRUE ~ NA_integer_
    )
  )

# Comprobación
table(
  original = d1$win_local,
  recodificada = d1$win_local_num,
  useNA = "ifany"
)

table(d1$win_local_num, useNA = "ifany")
d1 <- d1 %>%
  mutate(
    win_local_label = factor(
      win_local_num,
      levels = c(0, 1),
      labels = c(
        "No victoria local",
        "Victoria local"
      )
    )
  )

table(d1$win_local_label, useNA = "ifany")
# ============================================================
# RECONSTRUIR BASE DEL MODELO A
# ============================================================

d1_modelo <- d1 %>%
  select(
    win_local_num,
    win_local_label,
    tiros_puerta_local,
    xG_local,
    xGA_local,
    tiros_puerta_concedidos_local,
    posesion_local,
    diff_descanso,
    no_descanso_previo,
    pos_previa_local,
    pos_previa_visitante,
    forma_local_5,
    forma_visitante_5
  ) %>%
  na.omit()

# Comprobaciones
nrow(d1_modelo)

table(
  d1_modelo$win_local_num,
  useNA = "ifany"
)

table(
  d1_modelo$win_local_label,
  useNA = "ifany"
)
# ============================================================
# MODELO A BINOMIAL DE TRES TEMPORADAS
# ============================================================

modelo_A_d1 <- glm(
  win_local_num ~
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
  data = d1_modelo,
  family = binomial(link = "logit")
)

summary(modelo_A_d1)
nobs(modelo_A_d1)
# ============================================================
# DISTRIBUCION DE WIN_LOCAL
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)

df_win_d1 <- d1_modelo %>%
  count(
    win_local_num,
    name = "partidos"
  ) %>%
  mutate(
    resultado_binario = factor(
      win_local_num,
      levels = c(0, 1),
      labels = c(
        "No victoria local",
        "Victoria local"
      )
    ),
    
    porcentaje = partidos / sum(partidos),
    
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

print(df_win_d1)
grafico_distribucion_horizontal_d1 <- ggplot(
  df_win_d1,
  aes(
    x = partidos,
    y = resultado_binario,
    fill = resultado_binario
  )
) +
  geom_col(
    width = 0.56,
    color = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = etiqueta),
    hjust = 1.08,
    size = 4.2,
    lineheight = 1.05,
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "No victoria local" = "#7F8995",
      "Victoria local" = "#1976A3"
    )
  ) +
  scale_x_continuous(
    limits = c(
      0,
      max(df_win_d1$partidos) * 1.08
    ),
    breaks = pretty_breaks(n = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Distribución de la victoria local",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Número de partidos",
    y = NULL,
    caption = paste0(
      "Nota: «No victoria local» agrupa los empates ",
      "y las derrotas. N = ",
      sum(df_win_d1$partidos),
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
      r = 20,
      b = 15,
      l = 15
    )
  )

print(grafico_distribucion_horizontal_d1)
crear_grafico_distribucion <- function(
    datos,
    variable,
    subtitulo
) {
  
  tabla <- datos %>%
    count(
      respuesta = {{ variable }},
      name = "partidos"
    ) %>%
    mutate(
      resultado_binario = factor(
        respuesta,
        levels = c(0, 1),
        labels = c(
          "No victoria local",
          "Victoria local"
        )
      ),
      porcentaje = partidos / sum(partidos),
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
  
  ggplot(
    tabla,
    aes(
      x = partidos,
      y = resultado_binario,
      fill = resultado_binario
    )
  ) +
    geom_col(
      width = 0.56,
      color = "white",
      linewidth = 0.6
    ) +
    geom_text(
      aes(label = etiqueta),
      hjust = 1.08,
      size = 4,
      fontface = "bold",
      lineheight = 1.05,
      color = "white"
    ) +
    scale_fill_manual(
      values = c(
        "No victoria local" = "#7F8995",
        "Victoria local" = "#1976A3"
      )
    ) +
    scale_x_continuous(
      limits = c(
        0,
        max(tabla$partidos) * 1.08
      ),
      breaks = pretty_breaks(n = 5),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      subtitle = subtitulo,
      x = "Número de partidos",
      y = NULL
    ) +
    guides(fill = "none") +
    theme_minimal(base_size = 12) +
    theme(
      plot.subtitle = element_text(
        size = 11.5,
        face = "bold",
        color = "#333333",
        margin = margin(b = 12)
      ),
      axis.text.y = element_text(
        size = 10.5,
        color = "black"
      ),
      axis.text.x = element_text(
        color = "#4D4D4D"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(
        t = 10,
        r = 20,
        b = 10,
        l = 10
      )
    )
}
library(patchwork)

figura_distribucion_d_d1 <-
  grafico_distribucion_horizontal_d /
  grafico_distribucion_horizontal_d1 +
  plot_annotation(
    title = "Distribución de la victoria local según el periodo analizado",
    caption = paste(
      "La categoría «No victoria local» agrupa los empates",
      "y las derrotas del equipo local."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 15
      ),
      plot.caption = element_text(
        size = 9,
        color = "#666666",
        hjust = 0
      )
    )
  )

print(figura_distribucion_d_d1)
# ============================================================
# EVALUACION SEGURA DEL MODELO A
# ============================================================

datos_eval_A_d1 <- model.frame(modelo_A_d1)

real_A_d1_num <- model.response(
  datos_eval_A_d1
)

real_A_d1_num <- as.integer(
  real_A_d1_num
)

prob_A_d1 <- predict(
  modelo_A_d1,
  type = "response"
)

pred_A_d1_num <- ifelse(
  prob_A_d1 >= 0.5,
  1L,
  0L
)

# Comprobaciones
length(real_A_d1_num)
length(pred_A_d1_num)

table(
  real_A_d1_num,
  useNA = "ifany"
)

table(
  pred_A_d1_num,
  useNA = "ifany"
)
matriz_A_d1 <- table(
  Real = factor(
    real_A_d1_num,
    levels = c(0, 1),
    labels = c(
      "No victoria local",
      "Victoria local"
    )
  ),
  Predicho = factor(
    pred_A_d1_num,
    levels = c(0, 1),
    labels = c(
      "No victoria local",
      "Victoria local"
    )
  )
)

print(matriz_A_d1)
VN_A <- matriz_A_d1[
  "No victoria local",
  "No victoria local"
]

FP_A <- matriz_A_d1[
  "No victoria local",
  "Victoria local"
]

FN_A <- matriz_A_d1[
  "Victoria local",
  "No victoria local"
]

VP_A <- matriz_A_d1[
  "Victoria local",
  "Victoria local"
]

exactitud_A_d1 <- (
  VP_A + VN_A
) / sum(matriz_A_d1)

sensibilidad_A_d1 <- VP_A / (
  VP_A + FN_A
)

especificidad_A_d1 <- VN_A / (
  VN_A + FP_A
)

precision_A_d1 <- VP_A / (
  VP_A + FP_A
)

exactitud_equilibrada_A_d1 <- mean(
  c(
    sensibilidad_A_d1,
    especificidad_A_d1
  )
)

metricas_A_d1 <- tibble(
  Metrica = c(
    "Exactitud",
    "Sensibilidad",
    "Especificidad",
    "Precisión",
    "Exactitud equilibrada"
  ),
  Valor = c(
    exactitud_A_d1,
    sensibilidad_A_d1,
    especificidad_A_d1,
    precision_A_d1,
    exactitud_equilibrada_A_d1
  )
) %>%
  mutate(
    Porcentaje = percent(
      Valor,
      accuracy = 0.1,
      decimal.mark = ","
    )
  )

print(metricas_A_d1)
df_matriz_A_d1 <- as.data.frame(
  matriz_A_d1
) %>%
  group_by(Real) %>%
  mutate(
    Porcentaje = Freq / sum(Freq),
    
    Etiqueta = paste0(
      Freq,
      "\n",
      percent(
        Porcentaje,
        accuracy = 0.1,
        decimal.mark = ","
      )
    )
  ) %>%
  ungroup()
grafico_matriz_A_d1 <- ggplot(
  df_matriz_A_d1,
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
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#E8F1F8",
    high = "#1976A3",
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    )
  ) +
  coord_equal() +
  labs(
    title = "Matriz de confusión del Modelo A",
    subtitle = "Regresión logística binomial, tres temporadas",
    x = "Resultado predicho",
    y = "Resultado observado",
    fill = "Porcentaje\npor resultado real",
    caption = paste0(
      "Exactitud = ",
      percent(
        exactitud_A_d1,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "; sensibilidad = ",
      percent(
        sensibilidad_A_d1,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "; especificidad = ",
      percent(
        especificidad_A_d1,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "."
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
      margin = margin(b = 12)
    ),
    panel.grid = element_blank(),
    axis.text = element_text(
      color = "black",
      size = 10.5
    ),
    axis.text.x = element_text(
      angle = 10,
      hjust = 0.5
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      color = "#666666"
    )
  )

print(grafico_matriz_A_d1)
# ============================================================
# DISTRIBUCION VERTICAL DE WIN_LOCAL EN d1
# ============================================================

library(ggplot2)
library(scales)

grafico_distribucion_vertical_d1 <- ggplot(
  df_win_d1,
  aes(
    x = resultado_binario,
    y = partidos,
    fill = resultado_binario
  )
) +
  geom_col(
    width = 0.58,
    color = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.35,
    size = 4.2,
    lineheight = 1.05,
    fontface = "bold",
    color = "#2B2B2B"
  ) +
  scale_fill_manual(
    values = c(
      "No victoria local" = "#7F8995",
      "Victoria local" = "#1976A3"
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(df_win_d1$partidos) * 1.18
    ),
    breaks = pretty_breaks(n = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Distribución de la victoria local",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = NULL,
    y = "Número de partidos",
    caption = paste0(
      "Nota: «No victoria local» agrupa los empates ",
      "y las derrotas. N = ",
      sum(df_win_d1$partidos),
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
    axis.title.y = element_text(
      size = 11.5,
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

print(grafico_distribucion_vertical_d1)
########################################################
# ============================================================
# COMPARACION DEL AIC ENTRE MODELOS
# ============================================================
# ============================================================
# FUNCION PARA EVALUAR MODELOS BINOMIALES
# ============================================================

evaluar_modelo_binomial_d1 <- function(
    modelo,
    nombre
) {
  
  datos_modelo <- model.frame(modelo)
  real_original <- model.response(datos_modelo)
  
  if (is.factor(real_original)) {
    
    real_texto <- as.character(real_original)
    
    real_num <- case_when(
      real_texto %in% c(
        "1",
        "Victoria local",
        "Victoria"
      ) ~ 1L,
      
      real_texto %in% c(
        "0",
        "No victoria local",
        "No victoria"
      ) ~ 0L,
      
      TRUE ~ NA_integer_
    )
    
  } else {
    
    real_num <- as.integer(real_original)
  }
  
  probabilidades <- predict(
    modelo,
    type = "response"
  )
  
  predicho <- ifelse(
    probabilidades >= 0.5,
    1L,
    0L
  )
  
  if (
    any(is.na(real_num)) ||
    length(real_num) != length(predicho)
  ) {
    stop(
      paste0(
        "No se ha podido evaluar correctamente el modelo: ",
        nombre
      )
    )
  }
  
  VN <- sum(
    real_num == 0 &
      predicho == 0
  )
  
  FP <- sum(
    real_num == 0 &
      predicho == 1
  )
  
  FN <- sum(
    real_num == 1 &
      predicho == 0
  )
  
  VP <- sum(
    real_num == 1 &
      predicho == 1
  )
  
  tibble(
    Modelo = nombre,
    n = length(real_num),
    Variables = length(coef(modelo)) - 1,
    AIC = AIC(modelo),
    Desviacion = deviance(modelo),
    Exactitud = (VP + VN) / length(real_num),
    Sensibilidad = VP / (VP + FN),
    Especificidad = VN / (VN + FP),
    Precision = VP / (VP + FP),
    VN = VN,
    FP = FP,
    FN = FN,
    VP = VP
  )
}
# ============================================================
# NOMBRES DEFINITIVOS DE LOS MODELOS DE d1
# ============================================================

modelo_A_final_d1 <- modelo_A_d1

modelo_defensivo_completo_d1 <- modelo_defensivo_completo
modelo_defensivo_final_d1 <- modelo_defensivo17

modelo_ofensivo_completo_d1 <- modelo_ofensivo_completo
modelo_ofensivo_final_d1 <- modelo_ofensivo14

modelo_fisico_completo_d1 <- modelo_fisico_exploratorio
modelo_fisico_final_d1 <- modelo_fisico10

modelo_control_completo_d1 <- modelo_control_partido
modelo_control_final_d1 <- modelo_control2

modelo_contexto_completo_d1 <- modelo_contexto_partido
modelo_contexto_final_d1 <- modelo_contexto5

modelo_general_completo_d1 <- modelo_general
modelo_general_final_d1 <- modelo_general8
comparacion_modelos_d1 <- bind_rows(
  evaluar_modelo_binomial_d1(
    modelo_A_final_d1,
    "Modelo A"
  ),
  evaluar_modelo_binomial_d1(
    modelo_ofensivo_final_d1,
    "Ofensivo"
  ),
  evaluar_modelo_binomial_d1(
    modelo_defensivo_final_d1,
    "Defensivo"
  ),
  evaluar_modelo_binomial_d1(
    modelo_fisico_final_d1,
    "Físico"
  ),
  evaluar_modelo_binomial_d1(
    modelo_control_final_d1,
    "Control"
  ),
  evaluar_modelo_binomial_d1(
    modelo_contexto_final_d1,
    "Contexto"
  ),
  evaluar_modelo_binomial_d1(
    modelo_general_final_d1,
    "General"
  )
)

comparacion_modelos_d1 %>%
  mutate(
    across(
      c(
        Exactitud,
        Sensibilidad,
        Especificidad,
        Precision
      ),
      ~ percent(
        .x,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    AIC = round(AIC, 2),
    Desviacion = round(
      Desviacion,
      2
    )
  )
grafico_AIC_modelos_d1 <- comparacion_modelos_d1 %>%
  mutate(
    Grupo = ifelse(
      Modelo == "General",
      "Modelo general",
      "Modelos de referencia y por bloques"
    ),
    
    Modelo = fct_reorder(
      Modelo,
      AIC,
      .desc = TRUE
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
    size = 3.8,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Modelo general" = "#1976A3",
      "Modelos de referencia y por bloques" = "#8C96A3"
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(0, 0.13)
    )
  ) +
  labs(
    title = "Comparación del ajuste de los modelos binomiales",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = "Criterio de información de Akaike",
    y = NULL,
    fill = NULL,
    caption = paste(
      "Un menor AIC indica un mejor equilibrio",
      "entre ajuste y complejidad."
    )
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 12)
    ),
    axis.text = element_text(
      color = "black"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0
    )
  )

print(grafico_AIC_modelos_d1)
# ============================================================
# METRICAS DE CLASIFICACION
# ============================================================

metricas_modelos_d1 <- comparacion_modelos_d1 %>%
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
    ),
    
    Modelo = factor(
      Modelo,
      levels = c(
        "Modelo A",
        "Ofensivo",
        "Defensivo",
        "Físico",
        "Control",
        "Contexto",
        "General"
      )
    )
  )
grafico_metricas_modelos_d1 <- ggplot(
  metricas_modelos_d1,
  aes(
    x = Modelo,
    y = Valor,
    color = Metrica,
    group = Metrica
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 3.2
  ) +
  scale_color_manual(
    values = c(
      "Exactitud" = "#1976A3",
      "Sensibilidad" = "#D55E00",
      "Especificidad" = "#009E73"
    )
  ) +
  scale_y_continuous(
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
    title = "Capacidad de clasificación de los modelos",
    subtitle = "Temporadas 2022/2023 a 2024/2025",
    x = NULL,
    y = "Porcentaje",
    color = NULL,
    caption = paste(
      "Las métricas se han calculado sobre los mismos datos",
      "utilizados para estimar los modelos."
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
      margin = margin(b = 12)
    ),
    axis.text.x = element_text(
      angle = 25,
      hjust = 1,
      color = "black"
    ),
    axis.text.y = element_text(
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

print(grafico_metricas_modelos_d1)
# ============================================================
# TRAYECTORIA DE DEPURACION DEL BLOQUE OFENSIVO
# ============================================================

modelos_ofensivos_d1 <- list(
  Completo = modelo_ofensivo_completo,
  O2 = modelo_ofensivo2,
  O3 = modelo_ofensivo3,
  O4 = modelo_ofensivo4,
  O5 = modelo_ofensivo5,
  O6 = modelo_ofensivo6,
  O7 = modelo_ofensivo7,
  O8 = modelo_ofensivo8,
  O9 = modelo_ofensivo9,
  O10 = modelo_ofensivo10,
  O11 = modelo_ofensivo11,
  O12 = modelo_ofensivo12,
  O13 = modelo_ofensivo13,
  O14 = modelo_ofensivo14
)
# ============================================================
# TRAYECTORIA DE DEPURACION DEL BLOQUE DEFENSIVO
# ============================================================

modelos_defensivos_d1 <- list(
  Completo = modelo_defensivo_completo,
  D2 = modelo_defensivo2,
  D3 = modelo_defensivo3,
  D4 = modelo_defensivo4,
  D5 = modelo_defensivo5,
  D6 = modelo_defensivo6,
  D7 = modelo_defensivo7,
  D8 = modelo_defensivo8,
  D9 = modelo_defensivo9,
  D10 = modelo_defensivo10,
  D11 = modelo_defensivo11,
  D12 = modelo_defensivo12,
  D13 = modelo_defensivo13,
  D14 = modelo_defensivo14,
  D15 = modelo_defensivo15,
  D16 = modelo_defensivo16,
  D17 = modelo_defensivo17
)
# ============================================================
# TRAYECTORIA DE DEPURACION DEL MODELO GENERAL
# ============================================================

modelos_generales_d1 <- list(
  Completo = modelo_general,
  G2 = modelo_general2,
  G3 = modelo_general3,
  G4 = modelo_general4,
  G5 = modelo_general5,
  G6 = modelo_general6,
  G7 = modelo_general7,
  G8 = modelo_general8
)
# ============================================================
# FUNCION PARA EXTRAER LA EVOLUCION
# ============================================================

extraer_evolucion_d1 <- function(
    lista_modelos,
    bloque,
    modelo_seleccionado
) {
  
  resultados <- vector(
    mode = "list",
    length = length(lista_modelos)
  )
  
  for (i in seq_along(lista_modelos)) {
    
    modelo_i <- lista_modelos[[i]]
    nombre_i <- names(lista_modelos)[i]
    
    if (!inherits(modelo_i, "glm")) {
      
      stop(
        paste0(
          "El objeto ",
          nombre_i,
          " no es un modelo glm. Clase encontrada: ",
          paste(
            class(modelo_i),
            collapse = ", "
          )
        )
      )
    }
    
    resultados[[i]] <- data.frame(
      Bloque = bloque,
      Modelo = nombre_i,
      
      # El modelo completo corresponde a la iteración 0
      Iteracion = i - 1,
      
      Variables = length(
        coef(modelo_i)
      ) - 1,
      
      AIC = AIC(modelo_i),
      
      Desviacion = deviance(
        modelo_i
      ),
      
      Observaciones = nobs(
        modelo_i
      ),
      
      Seleccionado = nombre_i ==
        modelo_seleccionado
    )
  }
  
  resultado <- bind_rows(resultados)
  
  resultado %>%
    mutate(
      AIC_minimo = AIC ==
        min(AIC),
      
      Tipo = case_when(
        Seleccionado & AIC_minimo ~
          "Seleccionado y AIC mínimo",
        
        Seleccionado ~
          "Modelo seleccionado",
        
        AIC_minimo ~
          "AIC mínimo",
        
        TRUE ~
          "Resto de modelos"
      )
    )
}
evolucion_ofensivo_d1 <- extraer_evolucion_d1(
  lista_modelos = modelos_ofensivos_d1,
  bloque = "Bloque ofensivo",
  modelo_seleccionado = "O14"
)

evolucion_defensivo_d1 <- extraer_evolucion_d1(
  lista_modelos = modelos_defensivos_d1,
  bloque = "Bloque defensivo",
  modelo_seleccionado = "D17"
)

evolucion_general_d1 <- extraer_evolucion_d1(
  lista_modelos = modelos_generales_d1,
  bloque = "Modelo general",
  modelo_seleccionado = "G8"
)
evolucion_depuracion_d1 <- bind_rows(
  evolucion_ofensivo_d1,
  evolucion_defensivo_d1,
  evolucion_general_d1
)

print(
  evolucion_depuracion_d1,
  n = Inf
)
# ============================================================
# FIGURA PRINCIPAL: EVOLUCION DEL AIC DURANTE LA DEPURACION
# ============================================================

figura_evolucion_AIC_d1 <- ggplot(
  evolucion_depuracion_d1,
  aes(
    x = Variables,
    y = AIC,
    group = Bloque
  )
) +
  
  # Línea que conecta las sucesivas especificaciones
  geom_line(
    linewidth = 0.85,
    color = "#A7AFB8"
  ) +
  
  # Puntos correspondientes a los modelos intermedios
  geom_point(
    data = evolucion_depuracion_d1 %>%
      filter(Tipo == "Resto de modelos"),
    size = 2.4,
    color = "#A7AFB8"
  ) +
  
  # Puntos correspondientes al modelo seleccionado
  # y al modelo con AIC mínimo
  geom_point(
    data = evolucion_depuracion_d1 %>%
      filter(Tipo != "Resto de modelos"),
    aes(color = Tipo),
    size = 4.2
  ) +
  
  # Etiqueta del modelo seleccionado
  geom_label(
    data = evolucion_depuracion_d1 %>%
      filter(Seleccionado),
    aes(
      label = paste0(
        Modelo,
        "\n",
        "AIC = ",
        number(
          AIC,
          accuracy = 0.01,
          decimal.mark = ","
        )
      )
    ),
    nudge_y = 4,
    size = 3.2,
    fontface = "bold",
    color = "#1976A3",
    fill = "white",
    label.size = 0.25,
    label.padding = unit(0.18, "lines"),
    show.legend = FALSE
  ) +
  
  # Etiqueta independiente si el AIC mínimo
  # no coincide con el modelo seleccionado
  geom_label(
    data = evolucion_depuracion_d1 %>%
      filter(
        AIC_minimo,
        !Seleccionado
      ),
    aes(
      label = paste0(
        Modelo,
        "\n",
        "AIC mínimo = ",
        number(
          AIC,
          accuracy = 0.01,
          decimal.mark = ","
        )
      )
    ),
    nudge_y = -4,
    size = 3.1,
    fontface = "bold",
    color = "#D55E00",
    fill = "white",
    label.size = 0.25,
    label.padding = unit(0.18, "lines"),
    show.legend = FALSE
  ) +
  
  # Un panel para cada proceso de depuración
  facet_wrap(
    ~ Bloque,
    scales = "free_y",
    ncol = 1
  ) +
  
  # La depuración se muestra desde el modelo con más variables
  # hasta el modelo con menos variables
  scale_x_reverse(
    breaks = function(x) {
      pretty(
        x,
        n = 8
      )
    }
  ) +
  
  scale_color_manual(
    values = c(
      "Seleccionado y AIC mínimo" = "#1976A3",
      "Modelo seleccionado" = "#1976A3",
      "AIC mínimo" = "#D55E00"
    ),
    breaks = c(
      "Seleccionado y AIC mínimo",
      "Modelo seleccionado",
      "AIC mínimo"
    )
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(
      mult = c(0.10, 0.16)
    )
  ) +
  
  labs(
    title = "Evolución del AIC durante la depuración",
    subtitle = paste(
      "Modelos ofensivo, defensivo y general,",
      "temporadas 2022/2023 a 2024/2025"
    ),
    x = "Número de variables explicativas",
    y = "Criterio de información de Akaike",
    color = NULL,
    caption = paste(
      "El eje horizontal se presenta en orden descendente para reflejar",
      "la eliminación sucesiva de variables. Un menor AIC indica un mejor",
      "equilibrio entre ajuste y complejidad."
    )
  ) +
  
  theme_minimal(
    base_size = 12.5
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      color = "#1F1F1F"
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#555555",
      margin = margin(
        b = 14
      )
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(
        t = 14
      )
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11.5,
      color = "#2B2B2B"
    ),
    
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    
    axis.title.x = element_text(
      size = 11,
      margin = margin(
        t = 10
      )
    ),
    
    axis.title.y = element_text(
      size = 11,
      margin = margin(
        r = 10
      )
    ),
    
    axis.text.x = element_text(
      size = 9.5,
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 9.5,
      color = "#333333"
    ),
    
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 9.5
    ),
    
    legend.margin = margin(
      t = 4
    ),
    
    panel.grid.major = element_line(
      color = "#E5E5E5",
      linewidth = 0.45
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.spacing = unit(
      1.4,
      "lines"
    ),
    
    plot.margin = margin(
      t = 18,
      r = 30,
      b = 15,
      l = 18
    )
  )

print(figura_evolucion_AIC_d1)
figura_evolucion_AIC_d1_horizontal <- figura_evolucion_AIC_d1 +
  facet_wrap(
    ~ Bloque,
    scales = "free_y",
    ncol = 3
  ) +
  theme(
    panel.spacing = unit(
      1.2,
      "lines"
    ),
    
    axis.text.x = element_text(
      size = 8.5,
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 8.5,
      color = "#333333"
    )
  )

print(figura_evolucion_AIC_d1_horizontal)
# Comprobar que existe el objeto
exists("evolucion_depuracion_d1")

# Examinar su estructura
str(evolucion_depuracion_d1)

# Comprobar nombres de columnas
names(evolucion_depuracion_d1)

# Mostrar los datos
print(evolucion_depuracion_d1, n = Inf)
# ============================================================
# FIGURA COMPLETA DE LA EVOLUCION DEL AIC
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)
library(grid)

figura_evolucion_AIC_d1 <- ggplot(
  data = evolucion_depuracion_d1,
  mapping = aes(
    x = Variables,
    y = AIC,
    group = Bloque
  )
) +
  
  # Línea de evolución del AIC
  geom_line(
    linewidth = 0.85,
    color = "#A7AFB8"
  ) +
  
  # Modelos intermedios
  geom_point(
    data = evolucion_depuracion_d1 %>%
      filter(Tipo == "Resto de modelos"),
    size = 2.4,
    color = "#A7AFB8"
  ) +
  
  # Modelo seleccionado y modelo con AIC mínimo
  geom_point(
    data = evolucion_depuracion_d1 %>%
      filter(Tipo != "Resto de modelos"),
    mapping = aes(color = Tipo),
    size = 4.2
  ) +
  
  # Etiqueta del modelo seleccionado
  geom_label(
    data = evolucion_depuracion_d1 %>%
      filter(Seleccionado),
    mapping = aes(
      label = paste0(
        Modelo,
        "\nAIC = ",
        number(
          AIC,
          accuracy = 0.01,
          decimal.mark = ","
        )
      )
    ),
    nudge_y = 4,
    size = 3.1,
    fontface = "bold",
    color = "#1976A3",
    fill = "white",
    label.size = 0.25,
    label.padding = unit(0.18, "lines"),
    show.legend = FALSE
  ) +
  
  # Etiqueta del AIC mínimo cuando no coincide
  # con el modelo seleccionado
  geom_label(
    data = evolucion_depuracion_d1 %>%
      filter(
        AIC_minimo,
        !Seleccionado
      ),
    mapping = aes(
      label = paste0(
        Modelo,
        "\nAIC mínimo = ",
        number(
          AIC,
          accuracy = 0.01,
          decimal.mark = ","
        )
      )
    ),
    nudge_y = -4,
    size = 3.1,
    fontface = "bold",
    color = "#D55E00",
    fill = "white",
    label.size = 0.25,
    label.padding = unit(0.18, "lines"),
    show.legend = FALSE
  ) +
  
  # Tres paneles horizontales
  facet_wrap(
    facets = vars(Bloque),
    scales = "free_y",
    ncol = 3
  ) +
  
  # El modelo completo aparece a la izquierda
  # y la depuración avanza hacia la derecha
  scale_x_reverse(
    breaks = function(x) {
      pretty(x, n = 7)
    }
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(
      mult = c(0.13, 0.18)
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Seleccionado y AIC mínimo" = "#1976A3",
      "Modelo seleccionado" = "#1976A3",
      "AIC mínimo" = "#D55E00"
    ),
    drop = FALSE
  ) +
  
  labs(
    title = "Evolución del AIC durante la depuración",
    subtitle = paste(
      "Modelos ofensivo, defensivo y general,",
      "temporadas 2022/2023 a 2024/2025"
    ),
    x = "Número de variables explicativas",
    y = "Criterio de información de Akaike",
    color = NULL,
    caption = paste(
      "La depuración avanza desde las especificaciones con mayor número",
      "de variables hacia modelos más reducidos. Un menor AIC indica",
      "un mejor equilibrio entre ajuste y complejidad."
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
    
    plot.caption = element_text(
      size = 9,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 14)
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11,
      color = "#2B2B2B"
    ),
    
    strip.background = element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 10)
    ),
    
    axis.title.y = element_text(
      size = 11,
      margin = margin(r = 10)
    ),
    
    axis.text.x = element_text(
      size = 8.7,
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 8.7,
      color = "#333333"
    ),
    
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 9.5
    ),
    
    panel.grid.major = element_line(
      color = "#E5E5E5",
      linewidth = 0.45
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.spacing = unit(
      1.2,
      "lines"
    ),
    
    plot.margin = margin(
      t = 18,
      r = 30,
      b = 15,
      l = 18
    )
  )

print(figura_evolucion_AIC_d1)

figura_evolucion_AIC_d1_limpia <- ggplot(
  data = evolucion_depuracion_d1,
  mapping = aes(
    x = Variables,
    y = AIC,
    group = Bloque
  )
) +
  
  geom_line(
    linewidth = 0.85,
    color = "#A7AFB8"
  ) +
  
  geom_point(
    data = evolucion_depuracion_d1 %>%
      filter(Tipo == "Resto de modelos"),
    size = 2.4,
    color = "#A7AFB8"
  ) +
  
  geom_point(
    data = evolucion_depuracion_d1 %>%
      filter(Tipo != "Resto de modelos"),
    mapping = aes(color = Tipo),
    size = 4.2
  ) +
  
  facet_wrap(
    facets = vars(Bloque),
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_x_reverse(
    breaks = function(x) {
      pretty(x, n = 7)
    }
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(
      mult = c(0.08, 0.10)
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Seleccionado y AIC mínimo" = "#1976A3",
      "Modelo seleccionado" = "#1976A3",
      "AIC mínimo" = "#D55E00"
    )
  ) +
  
  labs(
    title = "Evolución del AIC durante la depuración",
    subtitle = paste(
      "Modelos ofensivo, defensivo y general,",
      "temporadas 2022/2023 a 2024/2025"
    ),
    x = "Número de variables explicativas",
    y = "Criterio de información de Akaike",
    color = NULL,
    caption = paste(
      "Los puntos azules identifican los modelos seleccionados",
      "y los puntos naranjas el AIC mínimo cuando no coincide",
      "con la especificación finalmente elegida."
    )
  ) +
  
  theme_minimal(base_size = 12.5) +
  
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
    
    axis.text = element_text(
      color = "#333333"
    ),
    
    legend.position = "bottom",
    
    panel.grid.major = element_line(
      color = "#E5E5E5",
      linewidth = 0.45
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

print(figura_evolucion_AIC_d1_limpia)
