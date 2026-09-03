# ==============================================================================
# Title: Multi-season multinomial Model A analysis
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script estimates and evaluates a multinomial logistic regression model
# for LaLiga match outcomes across the 2022/2023, 2023/2024 and 2024/2025
# seasons. The dependent variable distinguishes between a home-team victory,
# a draw and a home-team defeat, with the draw used as the reference category.
#
# The script imports and prepares the multi-season match-level dataset. Data
# preparation includes assigning the appropriate variable classes, cleaning
# rest and recent-form variables, calculating the difference in rest days
# between the home and visiting teams, and converting previous league positions
# into numerical ordinal variables.
#
# The multi-season version of Model A includes home shots on target, expected
# goals, expected goals against, shots on target conceded, possession, rest
# difference, previous league positions and recent form. The variable indicating
# the absence of previous rest information is excluded from the final
# specification because it does not provide sufficient variation in the
# multi-season dataset.
#
# The analysis calculates coefficient estimates, standard errors, bilateral
# Wald p-values, odds ratios and 95% confidence intervals. The coefficients
# compare the relative probability of a home defeat or a home victory with the
# probability of a draw, holding the remaining predictors constant.
#
# Predicted probabilities and outcome classes are calculated for the complete
# analytical sample. Model classification is evaluated through a confusion
# matrix, overall accuracy, balanced accuracy, class-specific sensitivity and
# class-specific precision.
#
# The script also performs a reproducible 80% training and 20% test division.
# Model A is re-estimated using the training sample and evaluated on the test
# sample through predicted probabilities, predicted classes, a confusion
# matrix, accuracy, balanced accuracy, sensitivity and precision.
#
# Finally, the script prepares publication-ready graphical summaries of the
# model results. These include an odds-ratio plot with 95% confidence intervals,
# a confusion-matrix heatmap and a grouped bar chart comparing sensitivity and
# precision across the three match-outcome categories.
#
# Input:
#   LaLiga_22-25_completo_v2 (2).xlsx
#
# Dataset:
#   d1, containing match-level observations from multiple LaLiga seasons.
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
#
# Validation procedure:
#   Random 80% training and 20% test division using set.seed(123).
#
# Main models:
#   modelo_multinom_A_limpio
#   modelo_multinom_train_limpio
#
# Main outputs:
#   Coefficient tables, odds ratios, p-values, confidence intervals, predicted
#   probabilities, confusion matrices, accuracy, balanced accuracy,
#   class-specific sensitivity, class-specific precision and graphical
#   summaries of the multi-season Model A results.
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

d1 <- read_excel("LaLiga_22-25_completo_v2 (2).xlsx")

str(d1)
head(d1)


# =========================================================
# 2. LIMPIEZA GENERAL DE VARIABLES
# =========================================================

# Variable dependiente binaria previa, si existe
d1$win_local <- factor(d1$win_local)

# Temporada y formaciones como factores, si se usan más adelante
d1$temporada <- factor(d1$temporada)
d1$formacion_local <- factor(d1$formacion_local)
d1$formacion_visit <- factor(d1$formacion_visit)

# Equipos como texto
d1$equipo_local <- as.character(d1$equipo_local)
d1$equipo_visitante <- as.character(d1$equipo_visitante)

# Limpiar formación local por si tiene comillas o espacios
d1$formacion_local <- gsub(
  '"',
  '',
  trimws(as.character(d1$formacion_local))
)

d1$formacion_local <- factor(d1$formacion_local)

levels(d1$formacion_local)
table(d1$formacion_local)


# =========================================================
# 3. LIMPIEZA DE DESCANSO
# =========================================================

# Convertir valores raros a NA
d1$descanso_local[
  d1$descanso_local %in% c("", "-", "NA")
] <- NA

d1$descanso_visit[
  d1$descanso_visit %in% c("", "-", "NA")
] <- NA

# Convertir a numérico
d1$descanso_local <- as.numeric(d1$descanso_local)
d1$descanso_visit <- as.numeric(d1$descanso_visit)

# Indicador de que no hay descanso previo
d1$no_descanso_previo <- ifelse(
  is.na(d1$descanso_local),
  1,
  0
)

# Imputar NA como 0 solo para poder calcular la diferencia
d1$descanso_local[is.na(d1$descanso_local)] <- 0
d1$descanso_visit[is.na(d1$descanso_visit)] <- 0

# Diferencia de descanso
d1$diff_descanso <- d1$descanso_local - d1$descanso_visit


# =========================================================
# 4. LIMPIEZA DE FORMA RECIENTE
# =========================================================

d1$forma_local_5[
  d1$forma_local_5 %in% c("", "-", "NA")
] <- NA

d1$forma_visitante_5[
  d1$forma_visitante_5 %in% c("", "-", "NA")
] <- NA

d1$forma_local_5 <- as.numeric(d1$forma_local_5)
d1$forma_visitante_5 <- as.numeric(d1$forma_visitante_5)

# Primeras jornadas sin forma previa: imputamos 0
d1$forma_local_5[is.na(d1$forma_local_5)] <- 0
d1$forma_visitante_5[is.na(d1$forma_visitante_5)] <- 0


# =========================================================
# 5. PREPARAR VARIABLE MULTINOMIAL
# =========================================================

# La columna resultado_partido_local viene de Excel:
# Victoria / Empate / Derrota

# Limpiar espacios por seguridad
d1$resultado_partido_local <- trimws(
  as.character(d1$resultado_partido_local)
)

# Ver categorías
table(d1$resultado_partido_local)
prop.table(table(d1$resultado_partido_local))

# Convertir a factor
d1$resultado_partido_local <- factor(
  d1$resultado_partido_local
)

# Fijar Empate como categoría de referencia
d1$resultado_partido_local <- relevel(
  d1$resultado_partido_local,
  ref = "Empate"
)

# Comprobar niveles
levels(d1$resultado_partido_local)


# =========================================================
# 6. POSICIÓN PREVIA COMO VARIABLE NUMÉRICA ORDINAL
# =========================================================

# Importante:
# 1 = mejor clasificado
# 20 = peor clasificado

d1$pos_previa_local <- as.numeric(
  as.character(d1$pos_previa_local)
)

d1$pos_previa_visitante <- as.numeric(
  as.character(d1$pos_previa_visitante)
)

class(d1$pos_previa_local)
class(d1$pos_previa_visitante)


# =========================================================
# 7. AJUSTE DEL MODELO MULTINOMIAL LIMPIO
# =========================================================

#voy a quitar la variable no descanso previo porque para todos es 0
#lo hare dejandola como NA

d1$descanso_local[
  d1$descanso_local %in% c("", "-", "NA")
] <- NA

d1$descanso_visit[
  d1$descanso_visit %in% c("", "-", "NA")
] <- NA

d1$descanso_local <- as.numeric(d1$descanso_local)
d1$descanso_visit <- as.numeric(d1$descanso_visit)

d1$diff_descanso <- (
  d1$descanso_local -
    d1$descanso_visit
)

modelo_multinom_A_limpio <- multinom(
  resultado_partido_local ~
    tiros_puerta_local +
    xG_local +
    xGA_local +
    tiros_puerta_concedidos_local +
    posesion_local +
    diff_descanso +
    pos_previa_local +
    pos_previa_visitante +
    forma_local_5 +
    forma_visitante_5,
  data = d1,
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

colnames(tabla_coef) <- c(
  "Resultado",
  "Variable",
  "Coeficiente"
)

tabla_or <- as.data.frame(as.table(OR_limpio))

colnames(tabla_or) <- c(
  "Resultado",
  "Variable",
  "Odds_Ratio"
)

tabla_p <- as.data.frame(as.table(p_limpio))

colnames(tabla_p) <- c(
  "Resultado",
  "Variable",
  "P_valor"
)

tabla_resultados_limpio <- tabla_coef %>%
  left_join(
    tabla_or,
    by = c("Resultado", "Variable")
  ) %>%
  left_join(
    tabla_p,
    by = c("Resultado", "Variable")
  ) %>%
  mutate(
    Coeficiente = round(Coeficiente, 4),
    Odds_Ratio = round(Odds_Ratio, 4),
    P_valor = round(P_valor, 4),
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
  newdata = d1,
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
  newdata = d1,
  type = "class"
)

head(pred_limpio)


# =========================================================
# 12. MATRIZ DE CONFUSIÓN EN TODA LA MUESTRA
# =========================================================

niveles_resultado <- levels(
  d1$resultado_partido_local
)

real_limpio <- factor(
  d1$resultado_partido_local,
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
sensibilidad_limpio[
  is.nan(sensibilidad_limpio) |
    is.infinite(sensibilidad_limpio)
] <- NA

precision_limpio[
  is.nan(precision_limpio) |
    is.infinite(precision_limpio)
] <- NA

balanced_accuracy_limpio <- mean(
  sensibilidad_limpio,
  na.rm = TRUE
)

metricas_limpio <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_limpio),
    4
  ),
  Precision = round(
    as.numeric(precision_limpio),
    4
  )
)

accuracy_limpio
balanced_accuracy_limpio
metricas_limpio


# =========================================================
# 14. DIVISIÓN TRAIN / TEST
# =========================================================

set.seed(123)

n <- nrow(d1)

pos_train <- sample(
  1:n,
  size = round(0.8 * n),
  replace = FALSE
)

d_train <- d1[pos_train, ]
d_test <- d1[-pos_train, ]

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

accuracy_test_limpio <- sum(
  diag(mc_test_limpio)
) / sum(mc_test_limpio)

sensibilidad_test_limpio <- diag(
  mc_test_limpio
) / rowSums(mc_test_limpio)

precision_test_limpio <- diag(
  mc_test_limpio
) / colSums(mc_test_limpio)

sensibilidad_test_limpio[
  is.nan(sensibilidad_test_limpio) |
    is.infinite(sensibilidad_test_limpio)
] <- NA

precision_test_limpio[
  is.nan(precision_test_limpio) |
    is.infinite(precision_test_limpio)
] <- NA

balanced_accuracy_test_limpio <- mean(
  sensibilidad_test_limpio,
  na.rm = TRUE
)

metricas_test_limpio <- data.frame(
  Categoria = niveles_resultado,
  Sensibilidad = round(
    as.numeric(sensibilidad_test_limpio),
    4
  ),
  Precision = round(
    as.numeric(precision_test_limpio),
    4
  )
)

accuracy_test_limpio
balanced_accuracy_test_limpio
metricas_test_limpio


########################################################################
# =========================================================
# 1. DATOS UTILIZADOS POR EL MODELO A
# =========================================================

datos_modelo_A_d1 <- model.frame(
  modelo_multinom_A_limpio
)

datos_modelo_A_d1 <- droplevels(
  datos_modelo_A_d1
)

# Comprobar número de observaciones
nrow(datos_modelo_A_d1)

# =========================================================
# 2. ODDS RATIOS E INTERVALOS DE CONFIANZA
# =========================================================

resumen_A_d1 <- summary(
  modelo_multinom_A_limpio
)

coef_A_d1 <- resumen_A_d1$coefficients
se_A_d1 <- resumen_A_d1$standard.errors

# Estadísticos de Wald
z_A_d1 <- coef_A_d1 / se_A_d1

# Valores p bilaterales
p_A_d1 <- 2 * (
  1 - pnorm(abs(z_A_d1))
)

# Convertir las matrices a formato largo
tabla_coef_A_d1 <- as.data.frame(
  as.table(coef_A_d1)
)

colnames(tabla_coef_A_d1) <- c(
  "Resultado",
  "Variable",
  "Coeficiente"
)

tabla_se_A_d1 <- as.data.frame(
  as.table(se_A_d1)
)

colnames(tabla_se_A_d1) <- c(
  "Resultado",
  "Variable",
  "Error_estandar"
)

tabla_p_A_d1 <- as.data.frame(
  as.table(p_A_d1)
)

colnames(tabla_p_A_d1) <- c(
  "Resultado",
  "Variable",
  "P_valor"
)

# Reunir resultados
datos_or_A_d1 <- tabla_coef_A_d1 %>%
  left_join(
    tabla_se_A_d1,
    by = c("Resultado", "Variable")
  ) %>%
  left_join(
    tabla_p_A_d1,
    by = c("Resultado", "Variable")
  ) %>%
  mutate(
    Odds_Ratio = exp(Coeficiente),
    
    IC_inferior = exp(
      Coeficiente - 1.96 * Error_estandar
    ),
    
    IC_superior = exp(
      Coeficiente + 1.96 * Error_estandar
    ),
    
    Comparacion = case_when(
      Resultado == "Derrota" ~
        "Derrota frente a Empate",
      
      Resultado == "Victoria" ~
        "Victoria frente a Empate",
      
      TRUE ~ as.character(Resultado)
    ),
    
    Significacion = case_when(
      P_valor < 0.05 ~ "p < 0,05",
      
      P_valor >= 0.05 &
        P_valor < 0.10 ~ "0,05 ≤ p < 0,10",
      
      TRUE ~ "p ≥ 0,10"
    )
  ) %>%
  
  # Eliminar el término independiente
  filter(Variable != "(Intercept)") %>%
  
  # Mostrar solo resultados significativos o marginales
  filter(P_valor < 0.10)
# =========================================================
# 2.1. NOMBRES LEGIBLES
# =========================================================

etiquetas_variables_A <- c(
  "tiros_puerta_local" =
    "tiros puerta local",
  
  "xG_local" =
    "xG local",
  
  "xGA_local" =
    "xGA local",
  
  "tiros_puerta_concedidos_local" =
    "tiros puerta concedidos local",
  
  "posesion_local" =
    "posesión local",
  
  "diff_descanso" =
    "diferencia de descanso",
  
  "pos_previa_local" =
    "posición previa local",
  
  "pos_previa_visitante" =
    "posición previa visitante",
  
  "forma_local_5" =
    "forma local 5",
  
  "forma_visitante_5" =
    "forma visitante 5"
)

datos_or_A_d1 <- datos_or_A_d1 %>%
  mutate(
    Etiqueta = recode(
      Variable,
      !!!etiquetas_variables_A
    ),
    
    Comparacion = factor(
      Comparacion,
      levels = c(
        "Derrota frente a Empate",
        "Victoria frente a Empate"
      )
    ),
    
    Significacion = factor(
      Significacion,
      levels = c(
        "0,05 ≤ p < 0,10",
        "p < 0,05"
      )
    )
  )
# =========================================================
# 2.2. GRÁFICO DE ODDS RATIOS
# =========================================================

grafico_or_A_d1 <- ggplot(
  datos_or_A_d1,
  aes(
    x = Odds_Ratio,
    y = reorder(Etiqueta, Odds_Ratio),
    color = Comparacion,
    shape = Significacion
  )
) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  geom_errorbarh(
    aes(
      xmin = IC_inferior,
      xmax = IC_superior
    ),
    height = 0,
    linewidth = 0.6,
    color = "grey35"
  ) +
  
  geom_point(
    size = 2.8
  ) +
  
  facet_wrap(
    ~ Comparacion,
    ncol = 1,
    scales = "free_y"
  ) +
  
  scale_x_log10(
    breaks = c(
      0.05, 0.10, 0.25, 0.50,
      0.75, 1, 1.50, 2, 4, 10, 20
    ),
    
    labels = label_number(
      decimal.mark = ",",
      accuracy = 0.01
    ),
    
    expand = expansion(
      mult = c(0.05, 0.08)
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
      "0,05 ≤ p < 0,10" = 17,
      "p < 0,05" = 16
    )
  ) +
  
  labs(
    title = "Modelo A multinomial",
    subtitle = paste0(
      "Odds ratios e intervalos de confianza del 95 %. ",
      "Categoría de referencia: Empate"
    ),
    x = "Odds ratio, escala logarítmica",
    y = NULL,
    color = "Comparación",
    shape = "Significación"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      face = "plain",
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "grey92"
    ),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    legend.position = "bottom",
    
    legend.box = "horizontal",
    
    plot.margin = margin(
      10, 15, 10, 10
    )
  )

grafico_or_A_d1
# =========================================================
# 3. PREDICCIONES DEL MODELO A
# =========================================================

pred_modelo_A_d1 <- predict(
  modelo_multinom_A_limpio,
  newdata = datos_modelo_A_d1,
  type = "class"
)

# Orden común de las categorías
orden_categorias <- c(
  "Empate",
  "Derrota",
  "Victoria"
)

real_modelo_A_d1 <- factor(
  datos_modelo_A_d1$resultado_partido_local,
  levels = orden_categorias
)

pred_modelo_A_d1 <- factor(
  pred_modelo_A_d1,
  levels = orden_categorias
)

# Comprobar longitudes
length(real_modelo_A_d1) ==
  length(pred_modelo_A_d1)

# Matriz de confusión
mc_modelo_A_d1 <- table(
  Real = real_modelo_A_d1,
  Predicho = pred_modelo_A_d1
)

mc_modelo_A_d1
addmargins(mc_modelo_A_d1)
# =========================================================
# 3.1. DATOS PARA LA MATRIZ DE CONFUSIÓN
# =========================================================

datos_mc_A_d1 <- as.data.frame(
  mc_modelo_A_d1
)

colnames(datos_mc_A_d1) <- c(
  "Real",
  "Predicho",
  "Frecuencia"
)

datos_mc_A_d1 <- datos_mc_A_d1 %>%
  group_by(Real) %>%
  mutate(
    Porcentaje = Frecuencia /
      sum(Frecuencia),
    
    Etiqueta = paste0(
      Frecuencia,
      "\n",
      sprintf(
        "%.1f%%",
        Porcentaje * 100
      )
    )
  ) %>%
  ungroup()

# Orden de columnas
datos_mc_A_d1$Predicho <- factor(
  datos_mc_A_d1$Predicho,
  levels = c(
    "Empate",
    "Derrota",
    "Victoria"
  )
)

# Orden visual de filas:
# Victoria arriba, Derrota en el centro, Empate abajo
datos_mc_A_d1$Real <- factor(
  datos_mc_A_d1$Real,
  levels = c(
    "Empate",
    "Derrota",
    "Victoria"
  )
)
# =========================================================
# 3.2. GRÁFICO DE MATRIZ DE CONFUSIÓN
# =========================================================

grafico_mc_A_d1 <- ggplot(
  datos_mc_A_d1,
  aes(
    x = Predicho,
    y = Real,
    fill = Porcentaje
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  
  geom_text(
    aes(
      label = Etiqueta
    ),
    size = 4,
    color = "black",
    lineheight = 0.95
  ) +
  
  scale_fill_gradient(
    low = "#F7F8FC",
    high = "#4C72B0",
    
    labels = label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    
    breaks = c(
      0.20, 0.40, 0.60, 0.80
    ),
    
    limits = c(0, 1),
    
    name = "Porcentaje"
  ) +
  
  coord_equal() +
  
  labs(
    title = "Matriz de confusión del Modelo A",
    subtitle = paste0(
      "Porcentajes calculados dentro de ",
      "cada resultado real"
    ),
    x = "Resultado predicho",
    y = "Resultado real"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0
    ),
    
    panel.grid = element_blank(),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 10
    ),
    
    legend.position = "right",
    
    plot.margin = margin(
      10, 15, 10, 10
    )
  )

grafico_mc_A_d1
# =========================================================
# 4. SENSIBILIDAD Y PRECISIÓN
# =========================================================

sensibilidad_A_d1 <- diag(
  mc_modelo_A_d1
) / rowSums(
  mc_modelo_A_d1
)

precision_A_d1 <- diag(
  mc_modelo_A_d1
) / colSums(
  mc_modelo_A_d1
)

# Sustituir valores no finitos por NA
sensibilidad_A_d1[
  !is.finite(sensibilidad_A_d1)
] <- NA

precision_A_d1[
  !is.finite(precision_A_d1)
] <- NA

# Crear tabla
metricas_grafico_A_d1 <- data.frame(
  Categoria = orden_categorias,
  Precision = as.numeric(
    precision_A_d1[orden_categorias]
  ),
  Sensibilidad = as.numeric(
    sensibilidad_A_d1[orden_categorias]
  )
)

metricas_grafico_A_d1
metricas_largas_A_d1 <- metricas_grafico_A_d1 %>%
  pivot_longer(
    cols = c(
      Precision,
      Sensibilidad
    ),
    names_to = "Metrica",
    values_to = "Proporcion"
  ) %>%
  mutate(
    Categoria = factor(
      Categoria,
      levels = c(
        "Empate",
        "Derrota",
        "Victoria"
      )
    ),
    
    Metrica = factor(
      Metrica,
      levels = c(
        "Precision",
        "Sensibilidad"
      ),
      labels = c(
        "Precisión",
        "Sensibilidad"
      )
    )
  )
# =========================================================
# 4.1. GRÁFICO DE BARRAS
# =========================================================

grafico_metricas_A_d1 <- ggplot(
  metricas_largas_A_d1,
  aes(
    x = Categoria,
    y = Proporcion,
    fill = Metrica
  )
) +
  
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.7
  ) +
  
  geom_text(
    aes(
      label = label_percent(
        accuracy = 0.1,
        decimal.mark = ","
      )(Proporcion)
    ),
    
    position = position_dodge(
      width = 0.75
    ),
    
    vjust = -0.5,
    size = 3.8
  ) +
  
  scale_fill_manual(
    values = c(
      "Precisión" = "#DD8452",
      "Sensibilidad" = "#4C72B0"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.05),
    
    breaks = seq(
      0, 1, by = 0.15
    ),
    
    labels = label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  
  labs(
    title = "Sensibilidad y precisión del Modelo A",
    x = NULL,
    y = "Proporción",
    fill = NULL
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      hjust = 0
    ),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      10, 15, 10, 10
    )
  )

grafico_metricas_A_d1
