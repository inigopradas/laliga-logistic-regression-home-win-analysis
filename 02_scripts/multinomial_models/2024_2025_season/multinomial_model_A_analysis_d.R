# ==============================================================================
# Title: Multinomial logistic regression Model A for match outcomes
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script estimates and evaluates a multinomial logistic regression model
# for match outcomes during the 2024/2025 season. The dependent variable
# distinguishes between home-team victory, draw and home-team defeat, with the
# draw used as the reference category.
#
# The script imports and prepares the match-level dataset, cleans the rest and
# recent-form variables, calculates the difference in rest days between the
# home and visiting teams, and converts previous league positions into
# numerical ordinal variables.
#
# Model A includes attacking, defensive, possession, rest, league-position and
# recent-form predictors. The analysis calculates coefficients, standard
# errors, p-values, odds ratios and 95% confidence intervals. It also obtains
# predicted probabilities and evaluates classification performance through
# confusion matrices, accuracy, balanced accuracy, class-specific sensitivity
# and class-specific precision.
#
# The model is evaluated both on the complete sample and through an 80% training
# and 20% test division. The script additionally calculates the AIC,
# log-likelihood and pseudo-R-squared statistics.
#
# Finally, the script defines reusable functions for extracting multinomial
# regression results and generating odds-ratio plots, confusion-matrix
# heatmaps, class-specific performance charts and likelihood-ratio test
# figures. An alternative specification rescales possession from a proportion
# to percentage points to facilitate coefficient interpretation.
#
# Input:
#   variables_Estudio (9).xlsx
#
# Main model:
#   modelo_multinom_A_limpio
#
# Dependent variable:
#   resultado_partido_local
#
# Outcome categories:
#   Victoria, Empate and Derrota
#
# Reference category:
#   Empate
#
# Statistical method:
#   Multinomial logistic regression estimated with nnet::multinom().
# ==============================================================================

# =========================================================
# 0. PAQUETES
# =========================================================

# Instalar solo si no los tienes
# install.packages("nnet")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("pscl")
# install.packages("caret")

library(nnet)
library(dplyr)
library(readxl)
library(pscl)
library(caret)


# =========================================================
# 1. LECTURA DE DATOS
# =========================================================

getwd()

d <- read_excel("variables_Estudio (9).xlsx")

str(d)
head(d)


# =========================================================
# 2. LIMPIEZA GENERAL DE VARIABLES
# =========================================================

# Variable dependiente binaria previa, si existe
d$win_local <- factor(d$win_local)

# Temporada y formaciones como factores, si se usan más adelante
d$temporada <- factor(d$temporada)
d$formacion_local <- factor(d$formacion_local)
d$formacion_visit <- factor(d$formacion_visit)

# Equipos como texto
d$equipo_local <- as.character(d$equipo_local)
d$equipo_visitante <- as.character(d$equipo_visitante)

# Limpiar formación local por si tiene comillas o espacios
d$formacion_local <- gsub('"', '', trimws(as.character(d$formacion_local)))
d$formacion_local <- factor(d$formacion_local)

levels(d$formacion_local)
table(d$formacion_local)


# =========================================================
# 3. LIMPIEZA DE DESCANSO
# =========================================================

# Convertir valores raros a NA
d$descanso_local[d$descanso_local %in% c("", "-", "NA")] <- NA
d$descanso_visit[d$descanso_visit %in% c("", "-", "NA")] <- NA

# Convertir a numérico
d$descanso_local <- as.numeric(d$descanso_local)
d$descanso_visit <- as.numeric(d$descanso_visit)

# Indicador de que no hay descanso previo
d$no_descanso_previo <- ifelse(is.na(d$descanso_local), 1, 0)

# Imputar NA como 0 solo para poder calcular diferencia
d$descanso_local[is.na(d$descanso_local)] <- 0
d$descanso_visit[is.na(d$descanso_visit)] <- 0

# Diferencia de descanso
d$diff_descanso <- d$descanso_local - d$descanso_visit


# =========================================================
# 4. LIMPIEZA DE FORMA RECIENTE
# =========================================================

d$forma_local_5[d$forma_local_5 %in% c("", "-", "NA")] <- NA
d$forma_visitante_5[d$forma_visitante_5 %in% c("", "-", "NA")] <- NA

d$forma_local_5 <- as.numeric(d$forma_local_5)
d$forma_visitante_5 <- as.numeric(d$forma_visitante_5)

# Primeras jornadas sin forma previa: imputamos 0
d$forma_local_5[is.na(d$forma_local_5)] <- 0
d$forma_visitante_5[is.na(d$forma_visitante_5)] <- 0


# =========================================================
# 5. PREPARAR VARIABLE MULTINOMIAL
# =========================================================

# La columna resultado_partido_local viene de Excel:
# Victoria / Empate / Derrota

# Limpiar espacios por seguridad
d$resultado_partido_local <- trimws(as.character(d$resultado_partido_local))

# Ver categorías
table(d$resultado_partido_local)
prop.table(table(d$resultado_partido_local))

# Convertir a factor
d$resultado_partido_local <- factor(d$resultado_partido_local)

# Fijar Empate como categoría de referencia
d$resultado_partido_local <- relevel(d$resultado_partido_local, ref = "Empate")

# Comprobar niveles
levels(d$resultado_partido_local)


# =========================================================
# 6. POSICIÓN PREVIA COMO VARIABLE NUMÉRICA ORDINAL
# =========================================================

# Importante:
# 1 = mejor clasificado
# 20 = peor clasificado

d$pos_previa_local <- as.numeric(as.character(d$pos_previa_local))
d$pos_previa_visitante <- as.numeric(as.character(d$pos_previa_visitante))

class(d$pos_previa_local)
class(d$pos_previa_visitante)


# =========================================================
# 7. AJUSTE DEL MODELO MULTINOMIAL LIMPIO
# =========================================================

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

summary(modelo_multinom_A_limpio)


# =========================================================
# 8. COEFICIENTES, P-VALORES Y ODDS RATIOS
# =========================================================

resumen_limpio <- summary(modelo_multinom_A_limpio)

coef_limpio <- resumen_limpio$coefficients
err_limpio <- resumen_limpio$standard.errors

z_limpio <- coef_limpio / err_limpio
p_limpio <- 2 * (1 - pnorm(abs(z_limpio)))

OR_limpio <- exp(coef_limpio)

round(coef_limpio, 4)
round(OR_limpio, 3)
round(p_limpio, 4)


# =========================================================
# 9. TABLA ORDENADA DE RESULTADOS
# =========================================================

tabla_coef <- as.data.frame(as.table(coef_limpio))
colnames(tabla_coef) <- c("Resultado", "Variable", "Coeficiente")

tabla_or <- as.data.frame(as.table(OR_limpio))
colnames(tabla_or) <- c("Resultado", "Variable", "Odds_Ratio")

tabla_p <- as.data.frame(as.table(p_limpio))
colnames(tabla_p) <- c("Resultado", "Variable", "P_valor")

tabla_resultados_limpio <- tabla_coef %>%
  left_join(tabla_or, by = c("Resultado", "Variable")) %>%
  left_join(tabla_p, by = c("Resultado", "Variable")) %>%
  mutate(
    Coeficiente = round(Coeficiente, 4),
    Odds_Ratio = round(Odds_Ratio, 4),
    P_valor = round(P_valor, 4),
    Significativa_5 = ifelse(P_valor < 0.05, "Sí", "No"),
    Significativa_10 = ifelse(P_valor < 0.10, "Sí", "No")
  )

tabla_resultados_limpio

# Variables significativas al 5%
tabla_resultados_limpio %>%
  filter(P_valor < 0.05)

# Variables significativas o marginales al 10%
tabla_resultados_limpio %>%
  filter(P_valor < 0.10)


# =========================================================
# 10. PROBABILIDADES PREDICHAS
# =========================================================

prob_limpio <- predict(
  modelo_multinom_A_limpio,
  newdata = d,
  type = "probs"
)

head(prob_limpio)

# En porcentaje
round(head(prob_limpio * 100), 2)

# Comprobar que las probabilidades suman 1
rowSums(prob_limpio)[1:10]

# Ver todas las probabilidades si quieres
# View(as.data.frame(prob_limpio))


# =========================================================
# 11. PREDICCIÓN DE CLASE EN TODA LA MUESTRA
# =========================================================

pred_limpio <- predict(
  modelo_multinom_A_limpio,
  newdata = d,
  type = "class"
)

head(pred_limpio)


# =========================================================
# 12. MATRIZ DE CONFUSIÓN EN TODA LA MUESTRA
# =========================================================

niveles_resultado <- levels(d$resultado_partido_local)

real_limpio <- factor(
  d$resultado_partido_local,
  levels = niveles_resultado
)

pred_limpio <- factor(
  pred_limpio,
  levels = niveles_resultado
)

mc_limpio <- table(
  Real = real_limpio,
  Predicho = pred_limpio
)

mc_limpio
addmargins(mc_limpio)


# =========================================================
# 13. MÉTRICAS EN TODA LA MUESTRA
# =========================================================

accuracy_limpio <- sum(diag(mc_limpio)) / sum(mc_limpio)

sensibilidad_limpio <- diag(mc_limpio) / rowSums(mc_limpio)

precision_limpio <- diag(mc_limpio) / colSums(mc_limpio)

# Evitar NaN o Inf si alguna categoría no se predice nunca
sensibilidad_limpio[is.nan(sensibilidad_limpio) | is.infinite(sensibilidad_limpio)] <- NA
precision_limpio[is.nan(precision_limpio) | is.infinite(precision_limpio)] <- NA

balanced_accuracy_limpio <- mean(sensibilidad_limpio, na.rm = TRUE)

metricas_limpio <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(as.numeric(sensibilidad_limpio), 4),
  Precision = round(as.numeric(precision_limpio), 4)
)

accuracy_limpio
balanced_accuracy_limpio
metricas_limpio


# =========================================================
# 14. DIVISIÓN TRAIN / TEST
# =========================================================

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


# =========================================================
# 15. MODELO MULTINOMIAL EN TRAIN
# =========================================================

modelo_multinom_train_limpio <- multinom(
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

summary(modelo_multinom_train_limpio)


# =========================================================
# 16. PREDICCIÓN EN TEST
# =========================================================

pred_test_limpio <- predict(
  modelo_multinom_train_limpio,
  newdata = d_test,
  type = "class"
)

prob_test_limpio <- predict(
  modelo_multinom_train_limpio,
  newdata = d_test,
  type = "probs"
)

head(prob_test_limpio)
round(head(prob_test_limpio * 100), 2)


# =========================================================
# 17. MATRIZ DE CONFUSIÓN EN TEST
# =========================================================

real_test <- factor(
  d_test$resultado_partido_local,
  levels = niveles_resultado
)

pred_test_limpio <- factor(
  pred_test_limpio,
  levels = niveles_resultado
)

mc_test_limpio <- table(
  Real = real_test,
  Predicho = pred_test_limpio
)

mc_test_limpio
addmargins(mc_test_limpio)


# =========================================================
# 18. MÉTRICAS EN TEST
# =========================================================

accuracy_test_limpio <- sum(diag(mc_test_limpio)) / sum(mc_test_limpio)

sensibilidad_test_limpio <- diag(mc_test_limpio) / rowSums(mc_test_limpio)

precision_test_limpio <- diag(mc_test_limpio) / colSums(mc_test_limpio)

sensibilidad_test_limpio[is.nan(sensibilidad_test_limpio) | is.infinite(sensibilidad_test_limpio)] <- NA
precision_test_limpio[is.nan(precision_test_limpio) | is.infinite(precision_test_limpio)] <- NA

balanced_accuracy_test_limpio <- mean(sensibilidad_test_limpio, na.rm = TRUE)

metricas_test_limpio <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(as.numeric(sensibilidad_test_limpio), 4),
  Precision = round(as.numeric(precision_test_limpio), 4)
)

accuracy_test_limpio
balanced_accuracy_test_limpio
metricas_test_limpio


# =========================================================
# 19. BONDAD DE AJUSTE
# =========================================================

AIC(modelo_multinom_A_limpio)
logLik(modelo_multinom_A_limpio)

# Pseudo R2
pR2(modelo_multinom_A_limpio)



# =========================================================
# 21. OBJETOS IMPORTANTES QUE DEBES GUARDAR / MIRAR
# =========================================================

tabla_resultados_limpio
mc_limpio
accuracy_limpio
balanced_accuracy_limpio
metricas_limpio

mc_test_limpio
accuracy_test_limpio
balanced_accuracy_test_limpio
metricas_test_limpio

test_global_limpio

########################################
# Instalar una sola vez
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("tidyr")
# install.packages("forcats")
# install.packages("patchwork")

library(ggplot2)
library(scales)
library(tidyr)
library(forcats)
library(patchwork)

dir.create(
  "graficos_multinomial",
  showWarnings = FALSE
)

colores_resultado <- c(
  "Derrota" = "#C44E52",
  "Victoria" = "#4C72B0"
)

colores_categoria <- c(
  "Empate" = "#DD8452",
  "Derrota" = "#C44E52",
  "Victoria" = "#4C72B0"
)
grafico_or_multinom <- function(
    tabla_resultados,
    titulo,
    p_max = 0.10,
    archivo = NULL
) {
  
  datos_grafico <- tabla_resultados %>%
    filter(
      Variable != "(Intercept)",
      P_valor < p_max,
      is.finite(Odds_Ratio),
      Odds_Ratio > 0
    ) %>%
    mutate(
      Resultado = factor(
        Resultado,
        levels = c("Derrota", "Victoria")
      ),
      Variable = gsub("_", " ", Variable),
      Variable = fct_reorder(
        Variable,
        abs(log(Odds_Ratio)),
        .fun = max
      ),
      Significacion = case_when(
        P_valor < 0.05 ~ "p < 0,05",
        P_valor < 0.10 ~ "p < 0,10",
        TRUE ~ "No significativa"
      )
    )
  
  grafico <- ggplot(
    datos_grafico,
    aes(
      x = Odds_Ratio,
      y = Variable,
      color = Resultado,
      shape = Significacion
    )
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = "grey40"
    ) +
    geom_point(size = 3) +
    scale_x_log10(
      breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2, 4),
      labels = label_number(decimal.mark = ",")
    ) +
    scale_color_manual(values = colores_resultado) +
    labs(
      title = titulo,
      subtitle = "Categoría de referencia: Empate",
      x = "Odds ratio, escala logarítmica",
      y = NULL,
      color = "Comparación",
      shape = "Significación"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  if (!is.null(archivo)) {
    ggsave(
      filename = archivo,
      plot = grafico,
      width = 10,
      height = 7,
      dpi = 300
    )
  }
  
  grafico
}
grafico_or_A <- grafico_or_multinom(
  tabla_resultados_limpio,
  "Modelo A multinomial: odds ratios significativos",
  p_max = 0.10,
  archivo = "graficos_multinomial/modelo_A_odds_ratios.png"
)

grafico_or_A
grafico_or_general <- grafico_or_multinom(
  tabla_general_depurado,
  "Modelo general depurado: odds ratios significativos",
  p_max = 0.10,
  archivo = "graficos_multinomial/modelo_general_odds_ratios.png"
)

grafico_or_general
grafico_matriz_confusion <- function(
    matriz,
    titulo,
    archivo = NULL
) {
  
  datos_mc <- as.data.frame(matriz)
  colnames(datos_mc) <- c(
    "Real",
    "Predicho",
    "Frecuencia"
  )
  
  datos_mc <- datos_mc %>%
    group_by(Real) %>%
    mutate(
      Porcentaje_fila = Frecuencia / sum(Frecuencia),
      Etiqueta = paste0(
        Frecuencia,
        "\n",
        percent(
          Porcentaje_fila,
          accuracy = 0.1,
          decimal.mark = ","
        )
      )
    ) %>%
    ungroup()
  
  grafico <- ggplot(
    datos_mc,
    aes(
      x = Predicho,
      y = Real,
      fill = Porcentaje_fila
    )
  ) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(
      aes(label = Etiqueta),
      size = 4
    ) +
    scale_fill_gradient(
      low = "white",
      high = "#4C72B0",
      labels = percent_format(
        accuracy = 1,
        decimal.mark = ","
      )
    ) +
    coord_equal() +
    labs(
      title = titulo,
      subtitle = "Porcentajes calculados dentro de cada resultado real",
      x = "Resultado predicho",
      y = "Resultado real",
      fill = "Porcentaje"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  if (!is.null(archivo)) {
    ggsave(
      filename = archivo,
      plot = grafico,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
  
  grafico
}

grafico_mc_A <- grafico_matriz_confusion(
  mc_limpio,
  "Matriz de confusión del Modelo A",
  "graficos_multinomial/modelo_A_matriz_confusion.png"
)

grafico_mc_A
grafico_mc_general <- grafico_matriz_confusion(
  mc_general_depurado,
  "Matriz de confusión del modelo general depurado",
  "graficos_multinomial/modelo_general_matriz_confusion.png"
)

grafico_mc_general
grafico_metricas_clase <- function(
    metricas,
    titulo,
    archivo = NULL
) {
  
  datos_metricas <- metricas %>%
    pivot_longer(
      cols = c(Sensibilidad, Precision),
      names_to = "Metrica",
      values_to = "Valor"
    ) %>%
    mutate(
      Categoria = factor(
        Categoria,
        levels = c("Empate", "Derrota", "Victoria")
      ),
      Metrica = recode(
        Metrica,
        "Precision" = "Precisión"
      )
    )
  
  grafico <- ggplot(
    datos_metricas,
    aes(
      x = Categoria,
      y = Valor,
      fill = Metrica
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_text(
      aes(
        label = percent(
          Valor,
          accuracy = 0.1,
          decimal.mark = ","
        )
      ),
      position = position_dodge(width = 0.8),
      vjust = -0.35,
      size = 3.5
    ) +
    scale_y_continuous(
      limits = c(0, 1.08),
      labels = percent_format(decimal.mark = ",")
    ) +
    scale_fill_manual(
      values = c(
        "Sensibilidad" = "#4C72B0",
        "Precisión" = "#DD8452"
      )
    ) +
    labs(
      title = titulo,
      x = NULL,
      y = "Proporción",
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
  
  if (!is.null(archivo)) {
    ggsave(
      filename = archivo,
      plot = grafico,
      width = 8,
      height = 5.5,
      dpi = 300
    )
  }
  
  grafico
}
grafico_metricas_A <- grafico_metricas_clase(
  metricas_limpio,
  "Sensibilidad y precisión del Modelo A",
  "graficos_multinomial/modelo_A_metricas.png"
)

grafico_metricas_A
metricas_general_depurado <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = as.numeric(
    sensibilidad_general_depurado
  ),
  Precision = as.numeric(
    precision_general_depurado
  )
)

grafico_metricas_general <- grafico_metricas_clase(
  metricas_general_depurado,
  "Sensibilidad y precisión del modelo general depurado",
  "graficos_multinomial/modelo_general_metricas.png"
)

grafico_metricas_general
comparacion_modelos <- data.frame(
  Modelo = c(
    "Modelo A",
    "Defensivo",
    "Ofensivo",
    "Control",
    "Físico",
    "Contextual",
    "General depurado"
  ),
  Accuracy = c(
    accuracy_limpio,
    accuracy_defensivo_d_1,
    accuracy_ofensivo_d3,
    accuracy_control_d,
    accuracy_fisico_final_d,
    accuracy_contexto_final_d,
    accuracy_general_depurado
  ),
  Balanced_accuracy = c(
    balanced_accuracy_limpio,
    balanced_accuracy_defensivo_d_1,
    sensibilidad_media_ofensivo_d3,
    sensibilidad_media_control_d,
    sensibilidad_media_fisico_final_d,
    sensibilidad_media_contexto_final_d,
    sensibilidad_media_general_depurado
  )
)

grafico_test_lr <- function(
    tabla_lr,
    titulo,
    n_variables = 15,
    archivo = NULL
) {
  
  datos_lr <- tabla_lr %>%
    filter(
      is.finite(LR_stat),
      p_value < 0.10
    ) %>%
    slice_max(
      order_by = LR_stat,
      n = n_variables,
      with_ties = FALSE
    ) %>%
    mutate(
      Variable = gsub("_", " ", Variable),
      Variable = fct_reorder(Variable, LR_stat),
      Significativa = ifelse(
        p_value < 0.05,
        "p < 0,05",
        "0,05 ≤ p < 0,10"
      )
    )
  
  grafico <- ggplot(
    datos_lr,
    aes(
      x = LR_stat,
      y = Variable,
      fill = Significativa
    )
  ) +
    geom_col(width = 0.7) +
    scale_fill_manual(
      values = c(
        "p < 0,05" = "#4C72B0",
        "0,05 ≤ p < 0,10" = "#DD8452"
      )
    ) +
    labs(
      title = titulo,
      subtitle = "Contraste global de razón de verosimilitud",
      x = "Estadístico LR",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
  
  if (!is.null(archivo)) {
    ggsave(
      filename = archivo,
      plot = grafico,
      width = 9,
      height = 6,
      dpi = 300
    )
  }
  
  grafico
}
grafico_lr_general <- grafico_test_lr(
  test_global_general_depurado,
  "Aportación global de las variables del modelo general",
  n_variables = 15,
  archivo = "graficos_multinomial/modelo_general_test_LR.png"
)

grafico_lr_general
figura_general_completa <-
  grafico_or_general +
  grafico_mc_general +
  grafico_metricas_general +
  grafico_lr_general +
  plot_layout(ncol = 2)

figura_general_completa

#################################################################
extraer_resultados_multinom <- function(
    modelo,
    nivel_confianza = 0.95
) {
  
  resumen <- summary(modelo)
  
  coeficientes <- resumen$coefficients
  errores <- resumen$standard.errors
  
  z_critico <- qnorm(
    1 - (1 - nivel_confianza) / 2
  )
  
  estadistico_z <- coeficientes / errores
  
  p_valores <- 2 * (
    1 - pnorm(abs(estadistico_z))
  )
  
  odds_ratios <- exp(coeficientes)
  
  limite_inferior <- exp(
    coeficientes - z_critico * errores
  )
  
  limite_superior <- exp(
    coeficientes + z_critico * errores
  )
  
  tabla_coeficientes <- as.data.frame(
    as.table(coeficientes)
  )
  
  colnames(tabla_coeficientes) <- c(
    "Resultado",
    "Variable",
    "Coeficiente"
  )
  
  tabla_errores <- as.data.frame(
    as.table(errores)
  )
  
  colnames(tabla_errores) <- c(
    "Resultado",
    "Variable",
    "Error_estandar"
  )
  
  tabla_or <- as.data.frame(
    as.table(odds_ratios)
  )
  
  colnames(tabla_or) <- c(
    "Resultado",
    "Variable",
    "Odds_Ratio"
  )
  
  tabla_ic_inferior <- as.data.frame(
    as.table(limite_inferior)
  )
  
  colnames(tabla_ic_inferior) <- c(
    "Resultado",
    "Variable",
    "IC_inferior"
  )
  
  tabla_ic_superior <- as.data.frame(
    as.table(limite_superior)
  )
  
  colnames(tabla_ic_superior) <- c(
    "Resultado",
    "Variable",
    "IC_superior"
  )
  
  tabla_p <- as.data.frame(
    as.table(p_valores)
  )
  
  colnames(tabla_p) <- c(
    "Resultado",
    "Variable",
    "P_valor"
  )
  
  tabla_resultados <- tabla_coeficientes %>%
    left_join(
      tabla_errores,
      by = c("Resultado", "Variable")
    ) %>%
    left_join(
      tabla_or,
      by = c("Resultado", "Variable")
    ) %>%
    left_join(
      tabla_ic_inferior,
      by = c("Resultado", "Variable")
    ) %>%
    left_join(
      tabla_ic_superior,
      by = c("Resultado", "Variable")
    ) %>%
    left_join(
      tabla_p,
      by = c("Resultado", "Variable")
    ) %>%
    mutate(
      Significativa_5 = ifelse(
        P_valor < 0.05,
        "Sí",
        "No"
      ),
      Significativa_10 = ifelse(
        P_valor < 0.10,
        "Sí",
        "No"
      )
    )
  
  return(tabla_resultados)
}
tabla_resultados_limpio %>%
  mutate(
    across(
      c(
        Coeficiente,
        Error_estandar,
        Odds_Ratio,
        IC_inferior,
        IC_superior,
        P_valor
      ),
      ~ round(.x, 4)
    )
  )
tabla_resultados_limpio <- extraer_resultados_multinom(
  modelo_multinom_A_limpio
)
tabla_general_depurado <- extraer_resultados_multinom(
  modelo_general_depurado
)
grafico_or_multinom_ic <- function(
    tabla_resultados,
    titulo,
    p_max = 0.10,
    archivo = NULL
) {
  
  datos_grafico <- tabla_resultados %>%
    filter(
      Variable != "(Intercept)",
      P_valor < p_max,
      is.finite(Odds_Ratio),
      is.finite(IC_inferior),
      is.finite(IC_superior),
      Odds_Ratio > 0,
      IC_inferior > 0,
      IC_superior > 0
    ) %>%
    mutate(
      Resultado = factor(
        Resultado,
        levels = c("Derrota", "Victoria"),
        labels = c(
          "Derrota frente a Empate",
          "Victoria frente a Empate"
        )
      ),
      Variable = gsub("_", " ", Variable),
      Variable = fct_reorder(
        Variable,
        abs(log(Odds_Ratio)),
        .fun = max
      ),
      Significacion = case_when(
        P_valor < 0.05 ~ "p < 0,05",
        P_valor < 0.10 ~ "0,05 ≤ p < 0,10",
        TRUE ~ "p ≥ 0,10"
      )
    )
  
  grafico <- ggplot(
    datos_grafico,
    aes(
      x = Odds_Ratio,
      y = Variable
    )
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.5,
      color = "grey40"
    ) +
    geom_errorbarh(
      aes(
        xmin = IC_inferior,
        xmax = IC_superior
      ),
      height = 0.18,
      linewidth = 0.7,
      color = "grey35"
    ) +
    geom_point(
      aes(
        color = Resultado,
        shape = Significacion
      ),
      size = 3
    ) +
    facet_wrap(
      ~ Resultado,
      ncol = 1,
      scales = "free_y"
    ) +
    scale_x_log10(
      breaks = c(
        0.10,
        0.25,
        0.50,
        0.75,
        1,
        1.50,
        2,
        4,
        10
      ),
      labels = label_number(
        decimal.mark = ",",
        accuracy = 0.01
      )
    ) +
    scale_color_manual(
      values = c(
        "Derrota frente a Empate" = "#C44E52",
        "Victoria frente a Empate" = "#4C72B0"
      )
    ) +
    scale_shape_manual(
      values = c(
        "p < 0,05" = 16,
        "0,05 ≤ p < 0,10" = 17,
        "p ≥ 0,10" = 1
      )
    ) +
    labs(
      title = titulo,
      subtitle = paste0(
        "Odds ratios e intervalos de confianza del 95 %. ",
        "Categoría de referencia: Empate"
      ),
      x = "Odds ratio, escala logarítmica",
      y = NULL,
      color = "Comparación",
      shape = "Significación"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        face = "bold"
      ),
      panel.spacing = unit(
        1,
        "lines"
      )
    )
  
  if (!is.null(archivo)) {
    ggsave(
      filename = archivo,
      plot = grafico,
      width = 10,
      height = 9,
      dpi = 300
    )
  }
  
  return(grafico)
}
grafico_or_A_ic <- grafico_or_multinom_ic(
  tabla_resultados = tabla_resultados_limpio,
  titulo = "Modelo A multinomial",
  p_max = 0.10,
  archivo = paste0(
    "graficos_multinomial/",
    "modelo_A_odds_ratios_IC95.png"
  )
)

grafico_or_A_ic
grafico_or_general_ic <- grafico_or_multinom_ic(
  tabla_resultados = tabla_general_depurado,
  titulo = "Modelo general multinomial depurado",
  p_max = 0.10,
  archivo = paste0(
    "graficos_multinomial/",
    "modelo_general_odds_ratios_IC95.png"
  )
)

grafico_or_general_ic
d$posesion_local_pct <- d$posesion_local * 100
modelo_multinom_A_pct <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    xG_local +
    xGA_local +
    tiros_puerta_concedidos_local +
    posesion_local_pct +
    diff_descanso +
    no_descanso_previo +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d,
  trace = FALSE,
  Hess = TRUE,
  model = TRUE
)
