# ==============================================================================
# Title: Comparison of formation-based binomial models across study periods
# Author: IÑIGO PRADAS NICOLAS
#
# Description:
# This script develops and compares binary logistic regression models designed
# to analyse the association between tactical formations and the probability
# of a home-team victory. The analysis is conducted using two datasets: d,
# corresponding to the 2024/2025 season, and d1, covering the 2022/2023,
# 2023/2024 and 2024/2025 seasons.
#
# The script first standardises the binary dependent variable, ensuring that
# home victory is coded as 1 and no home victory as 0. Tactical formation
# labels are cleaned by removing quotation marks, unnecessary spaces and empty
# values. Formation categories with fewer than ten observations are grouped
# into a residual category named "Otras".
#
# A preferred reference formation is selected for the home and visiting teams.
# The 1-4-2-3-1 formation is used whenever available; otherwise, the most
# frequently observed formation is selected automatically.
#
# Separate binomial logistic regression models are estimated for d and d1.
# Both models include the grouped home and visiting formations as additive
# categorical predictors and do not include an interaction between them.
#
# The script defines reusable functions to estimate the formation models,
# generate all possible combinations of home and visiting formations and
# calculate their predicted log-odds, odds, probabilities and 95% confidence
# intervals. The observed number of matches corresponding to each formation
# combination is also included.
#
# Adjusted odds ratios are calculated relative to the combination in which both
# the home and visiting teams use their respective reference formations. The
# corresponding confidence intervals are used to classify each result as
# favourable, unfavourable or inconclusive with respect to a home-team victory.
#
# Formation frequencies are calculated separately for home and visiting teams.
# These frequencies are used to describe the amount of empirical support
# available for each estimated formation effect and to identify formations
# based on small or moderate samples.
#
# The script creates ordered result tables for formation combinations and
# marginal formation effects. It also produces heatmaps of adjusted odds
# ratios, distinguishing between observed combinations, combinations with
# insufficient sample sizes and combinations not observed in the dataset.
#
# Finally, forest plots display the adjusted odds ratios and 95% confidence
# intervals associated with each home and visiting formation. The plots
# distinguish statistically favourable, statistically unfavourable,
# inconclusive and reference-category results while also representing the
# marginal frequency of each formation.
#
# Datasets:
#   d  = data from the 2024/2025 season
#   d1 = data from the 2022/2023 to 2024/2025 seasons
#
# Dependent variable:
#   win_local_num
#
# Outcome definition:
#   1 = home-team victory
#   0 = no home-team victory, including draws and away-team victories
#
# Explanatory variables:
#   formacion_local_dep
#   formacion_visit_dep
#
# Preferred reference formation:
#   1-4-2-3-1
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
# Main models:
#   modelo_d
#   modelo_d1
#
# Main functions:
#   normalizar_win()
#   normalizar_win_local()
#   elegir_referencia()
#   ajustar_modelo_formaciones()
#   calcular_or_referencia()
#   calcular_frecuencias_formaciones()
#   extraer_or_ajustados()
#
# Main outputs:
#   Formation-frequency tables, predicted probabilities for formation
#   combinations, adjusted odds ratios, 95% confidence intervals, sample-size
#   classifications, heatmaps and forest plots for d and d1.
# ==============================================================================
# =========================================================
# PAQUETES
# =========================================================

library(dplyr)
library(ggplot2)

# =========================================================
# COMPROBACIONES PREVIAS
# =========================================================

# Asegurar que win_local sea realmente binaria 0/1.
# Evita transformar directamente un factor con as.numeric(),
# porque convertiría sus niveles en 1/2.

normalizar_win <- function(x) {
  
  x <- trimws(as.character(x))
  
  if (!all(na.omit(x) %in% c("0", "1"))) {
    stop(
      "win_local debe contener únicamente 0, 1 o NA. ",
      "Valores encontrados: ",
      paste(unique(na.omit(x)), collapse = ", ")
    )
  }
  
  as.integer(x)
}

d$win_local_num  <- normalizar_win(d$win_local)
d1$win_local_num <- normalizar_win(d1$win_local)
# =========================================================
# CREAR VARIABLES DEPURADAS EN d
# =========================================================

# Comprobar que existen las columnas originales
stopifnot(
  "formacion_local" %in% names(d),
  "formacion_visit" %in% names(d)
)

# Limpiar comillas y espacios
d$formacion_local <- gsub(
  '"',
  '',
  trimws(as.character(d$formacion_local))
)

d$formacion_visit <- gsub(
  '"',
  '',
  trimws(as.character(d$formacion_visit))
)

# Convertir cadenas vacías en NA
d$formacion_local[d$formacion_local == ""] <- NA
d$formacion_visit[d$formacion_visit == ""] <- NA

# Calcular frecuencias de las variables originales limpias
freq_local <- table(
  d$formacion_local,
  useNA = "no"
)

freq_visit <- table(
  d$formacion_visit,
  useNA = "no"
)

# Definir umbral
umbral <- 10

# Identificar formaciones con menos de 10 apariciones
raras_local <- names(
  freq_local[freq_local < umbral]
)

raras_visit <- names(
  freq_visit[freq_visit < umbral]
)

# Crear las variables depuradas
d$formacion_local_dep <- ifelse(
  is.na(d$formacion_local),
  NA_character_,
  ifelse(
    d$formacion_local %in% raras_local,
    "Otras",
    d$formacion_local
  )
)

d$formacion_visit_dep <- ifelse(
  is.na(d$formacion_visit),
  NA_character_,
  ifelse(
    d$formacion_visit %in% raras_visit,
    "Otras",
    d$formacion_visit
  )
)

# Convertir las columnas nuevas en factores
d$formacion_local_dep <- factor(
  d$formacion_local_dep
)

d$formacion_visit_dep <- factor(
  d$formacion_visit_dep
)
c(
  filas_d = nrow(d),
  longitud_local_dep = length(d$formacion_local_dep),
  longitud_visit_dep = length(d$formacion_visit_dep)
)
# =========================================================
# CREAR VARIABLES DEPURADAS EN d1
# =========================================================

stopifnot(
  "formacion_local" %in% names(d1),
  "formacion_visit" %in% names(d1)
)

d1$formacion_local <- gsub(
  '"',
  '',
  trimws(as.character(d1$formacion_local))
)

d1$formacion_visit <- gsub(
  '"',
  '',
  trimws(as.character(d1$formacion_visit))
)

d1$formacion_local[d1$formacion_local == ""] <- NA
d1$formacion_visit[d1$formacion_visit == ""] <- NA

freq1_local <- table(
  d1$formacion_local,
  useNA = "no"
)

freq1_visit <- table(
  d1$formacion_visit,
  useNA = "no"
)

raras1_local <- names(
  freq1_local[freq1_local < umbral]
)

raras1_visit <- names(
  freq1_visit[freq1_visit < umbral]
)

d1$formacion_local_dep <- ifelse(
  is.na(d1$formacion_local),
  NA_character_,
  ifelse(
    d1$formacion_local %in% raras1_local,
    "Otras",
    d1$formacion_local
  )
)

d1$formacion_visit_dep <- ifelse(
  is.na(d1$formacion_visit),
  NA_character_,
  ifelse(
    d1$formacion_visit %in% raras1_visit,
    "Otras",
    d1$formacion_visit
  )
)

d1$formacion_local_dep <- droplevels(
  factor(d1$formacion_local_dep)
)

d1$formacion_visit_dep <- droplevels(
  factor(d1$formacion_visit_dep)
)

table(
  d1$formacion_local_dep,
  useNA = "ifany"
)

table(
  d1$formacion_visit_dep,
  useNA = "ifany"
)
# Eliminar niveles vacíos, aunque ahora probablemente no existan
d$formacion_local_dep <- droplevels(
  d$formacion_local_dep
)

d$formacion_visit_dep <- droplevels(
  d$formacion_visit_dep
)
# =========================================================
# 1. PAQUETES
# =========================================================

library(dplyr)
library(ggplot2)

# =========================================================
# 2. COMPROBAR QUE EXISTEN LAS VARIABLES NECESARIAS
# =========================================================

columnas_necesarias <- c(
  "win_local",
  "formacion_local_dep",
  "formacion_visit_dep"
)

faltantes_d <- setdiff(
  columnas_necesarias,
  names(d)
)

faltantes_d1 <- setdiff(
  columnas_necesarias,
  names(d1)
)

if (length(faltantes_d) > 0) {
  stop(
    "En d faltan estas columnas: ",
    paste(faltantes_d, collapse = ", ")
  )
}

if (length(faltantes_d1) > 0) {
  stop(
    "En d1 faltan estas columnas: ",
    paste(faltantes_d1, collapse = ", ")
  )
}

# =========================================================
# 3. CONVERTIR win_local A VARIABLE NUMÉRICA 0/1
# =========================================================

normalizar_win_local <- function(x, nombre_base) {
  
  x_original <- x
  
  x <- trimws(
    as.character(x)
  )
  
  # Mostrar los valores presentes
  mensaje <- paste(
    unique(na.omit(x)),
    collapse = ", "
  )
  
  message(
    "Valores encontrados en win_local de ",
    nombre_base,
    ": ",
    mensaje
  )
  
  # Caso habitual: 0 y 1
  if (all(na.omit(x) %in% c("0", "1"))) {
    return(as.integer(x))
  }
  
  # Caso TRUE/FALSE
  if (all(na.omit(tolower(x)) %in% c("true", "false"))) {
    
    return(
      ifelse(
        is.na(x),
        NA_integer_,
        ifelse(tolower(x) == "true", 1L, 0L)
      )
    )
  }
  
  stop(
    "La variable win_local de ",
    nombre_base,
    " no está codificada como 0/1. ",
    "Valores encontrados: ",
    mensaje
  )
}

d$win_local_num <- normalizar_win_local(
  d$win_local,
  "d"
)

d1$win_local_num <- normalizar_win_local(
  d1$win_local,
  "d1"
)

table(
  d$win_local_num,
  useNA = "ifany"
)

table(
  d1$win_local_num,
  useNA = "ifany"
)

# =========================================================
# 4. ELIMINAR NIVELES VACÍOS
# =========================================================

d$formacion_local_dep <- droplevels(
  factor(d$formacion_local_dep)
)

d$formacion_visit_dep <- droplevels(
  factor(d$formacion_visit_dep)
)

d1$formacion_local_dep <- droplevels(
  factor(d1$formacion_local_dep)
)

d1$formacion_visit_dep <- droplevels(
  factor(d1$formacion_visit_dep)
)

# Comprobar niveles
levels(d$formacion_local_dep)
levels(d$formacion_visit_dep)

levels(d1$formacion_local_dep)
levels(d1$formacion_visit_dep)
# =========================================================
# 5. FUNCIÓN PARA ELEGIR LA REFERENCIA
# =========================================================

elegir_referencia <- function(variable, preferida = "1-4-2-3-1") {
  
  variable <- droplevels(
    factor(variable)
  )
  
  niveles <- levels(variable)
  
  if (length(niveles) == 0) {
    stop("La variable no contiene niveles válidos.")
  }
  
  if (preferida %in% niveles) {
    return(preferida)
  }
  
  frecuencias <- sort(
    table(variable),
    decreasing = TRUE
  )
  
  referencia <- names(frecuencias)[1]
  
  warning(
    "No se encontró la referencia ",
    preferida,
    ". Se utilizará como referencia: ",
    referencia
  )
  
  referencia
}
# =========================================================
# 6. FUNCIÓN PARA AJUSTAR EL MODELO Y PREDECIR
# =========================================================

ajustar_modelo_formaciones <- function(
    datos,
    nombre_base,
    referencia_preferida = "1-4-2-3-1"
) {
  
  # -------------------------------------------------------
  # Seleccionar referencias
  # -------------------------------------------------------
  
  ref_local <- elegir_referencia(
    datos$formacion_local_dep,
    referencia_preferida
  )
  
  ref_visit <- elegir_referencia(
    datos$formacion_visit_dep,
    referencia_preferida
  )
  
  # -------------------------------------------------------
  # Reordenar niveles
  # -------------------------------------------------------
  
  datos$formacion_local_dep <- relevel(
    droplevels(factor(datos$formacion_local_dep)),
    ref = ref_local
  )
  
  datos$formacion_visit_dep <- relevel(
    droplevels(factor(datos$formacion_visit_dep)),
    ref = ref_visit
  )
  
  # -------------------------------------------------------
  # Eliminar solamente filas incompletas para el modelo
  # -------------------------------------------------------
  
  datos_modelo <- datos %>%
    filter(
      !is.na(win_local_num),
      !is.na(formacion_local_dep),
      !is.na(formacion_visit_dep)
    ) %>%
    droplevels()
  
  if (nrow(datos_modelo) == 0) {
    stop(
      "No quedan observaciones completas en la base ",
      nombre_base
    )
  }
  
  # -------------------------------------------------------
  # Ajustar modelo sin interacción
  # -------------------------------------------------------
  
  modelo <- glm(
    win_local_num ~
      formacion_local_dep +
      formacion_visit_dep,
    data = datos_modelo,
    family = binomial,
    na.action = na.exclude
  )
  
  # -------------------------------------------------------
  # Comprobar posibles coeficientes no estimables
  # -------------------------------------------------------
  
  if (any(is.na(coef(modelo)))) {
    
    warning(
      "El modelo de ",
      nombre_base,
      " contiene coeficientes no estimables. ",
      "Revisa tablas con frecuencias muy bajas o niveles vacíos."
    )
  }
  
  # -------------------------------------------------------
  # Crear todas las combinaciones posibles
  # -------------------------------------------------------
  
  rejilla <- expand.grid(
    formacion_local_dep =
      levels(datos_modelo$formacion_local_dep),
    formacion_visit_dep =
      levels(datos_modelo$formacion_visit_dep),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  rejilla$formacion_local_dep <- factor(
    rejilla$formacion_local_dep,
    levels = levels(datos_modelo$formacion_local_dep)
  )
  
  rejilla$formacion_visit_dep <- factor(
    rejilla$formacion_visit_dep,
    levels = levels(datos_modelo$formacion_visit_dep)
  )
  
  # -------------------------------------------------------
  # Obtener predicciones en escala logit
  # -------------------------------------------------------
  
  prediccion <- predict(
    modelo,
    newdata = rejilla,
    type = "link",
    se.fit = TRUE
  )
  
  rejilla$log_odds <- as.numeric(
    prediccion$fit
  )
  
  rejilla$error_log_odds <- as.numeric(
    prediccion$se.fit
  )
  
  z <- qnorm(0.975)
  
  # -------------------------------------------------------
  # Odds e intervalos
  # -------------------------------------------------------
  
  rejilla$odds <- exp(
    rejilla$log_odds
  )
  
  rejilla$odds_inf <- exp(
    rejilla$log_odds -
      z * rejilla$error_log_odds
  )
  
  rejilla$odds_sup <- exp(
    rejilla$log_odds +
      z * rejilla$error_log_odds
  )
  
  # -------------------------------------------------------
  # Probabilidades e intervalos
  # -------------------------------------------------------
  
  rejilla$probabilidad <- plogis(
    rejilla$log_odds
  )
  
  rejilla$prob_inf <- plogis(
    rejilla$log_odds -
      z * rejilla$error_log_odds
  )
  
  rejilla$prob_sup <- plogis(
    rejilla$log_odds +
      z * rejilla$error_log_odds
  )
  
  # -------------------------------------------------------
  # Frecuencia real de cada enfrentamiento
  # -------------------------------------------------------
  
  frecuencias <- datos_modelo %>%
    mutate(
      formacion_local_texto =
        as.character(formacion_local_dep),
      formacion_visit_texto =
        as.character(formacion_visit_dep)
    ) %>%
    count(
      formacion_local_texto,
      formacion_visit_texto,
      name = "n_partidos"
    )
  
  rejilla <- rejilla %>%
    mutate(
      formacion_local_texto =
        as.character(formacion_local_dep),
      formacion_visit_texto =
        as.character(formacion_visit_dep)
    ) %>%
    left_join(
      frecuencias,
      by = c(
        "formacion_local_texto",
        "formacion_visit_texto"
      )
    ) %>%
    mutate(
      n_partidos = ifelse(
        is.na(n_partidos),
        0L,
        n_partidos
      ),
      Base = nombre_base,
      enfrentamiento = paste(
        formacion_local_texto,
        "vs",
        formacion_visit_texto
      ),
      observado = ifelse(
        n_partidos > 0,
        "Observado",
        "Sin partidos observados"
      )
    )
  
  # -------------------------------------------------------
  # Devolver resultados
  # -------------------------------------------------------
  
  list(
    modelo = modelo,
    datos_modelo = datos_modelo,
    resultados = rejilla,
    referencia_local = ref_local,
    referencia_visitante = ref_visit
  )
}
# =========================================================
# 7. AJUSTAR LOS DOS MODELOS
# =========================================================

resultado_d <- ajustar_modelo_formaciones(
  datos = d,
  nombre_base = "d"
)

resultado_d1 <- ajustar_modelo_formaciones(
  datos = d1,
  nombre_base = "d1"
)

# Extraer modelos
modelo_d <- resultado_d$modelo
modelo_d1 <- resultado_d1$modelo

# Extraer tablas de enfrentamientos
enfrentamientos_d <- resultado_d$resultados
enfrentamientos_d1 <- resultado_d1$resultados

# Mostrar referencias utilizadas
resultado_d$referencia_local
resultado_d$referencia_visitante

resultado_d1$referencia_local
resultado_d1$referencia_visitante

# =========================================================
# 7. AJUSTAR LOS MODELOS PARA d Y d1
# =========================================================

# Ajustar modelo para la base d
resultado_d <- ajustar_modelo_formaciones(
  datos = d,
  nombre_base = "d",
  referencia_preferida = "1-4-2-3-1"
)

# Ajustar modelo para la base d1
resultado_d1 <- ajustar_modelo_formaciones(
  datos = d1,
  nombre_base = "d1",
  referencia_preferida = "1-4-2-3-1"
)

# =========================================================
# EXTRAER LOS MODELOS GLM
# =========================================================

modelo_d <- resultado_d$modelo
modelo_d1 <- resultado_d1$modelo

# =========================================================
# EXTRAER LOS DATOS UTILIZADOS EN CADA MODELO
# =========================================================

datos_modelo_d <- resultado_d$datos_modelo
datos_modelo_d1 <- resultado_d1$datos_modelo

# =========================================================
# EXTRAER LAS TABLAS DE ENFRENTAMIENTOS
# =========================================================

enfrentamientos_d <- resultado_d$resultados
enfrentamientos_d1 <- resultado_d1$resultados

# =========================================================
# MOSTRAR LAS REFERENCIAS UTILIZADAS
# =========================================================

cat("\n")
cat("============================================\n")
cat("REFERENCIAS UTILIZADAS EN LA BASE d\n")
cat("============================================\n")

cat(
  "Formación local de referencia:",
  resultado_d$referencia_local,
  "\n"
)

cat(
  "Formación visitante de referencia:",
  resultado_d$referencia_visitante,
  "\n"
)

cat("\n")
cat("============================================\n")
cat("REFERENCIAS UTILIZADAS EN LA BASE d1\n")
cat("============================================\n")

cat(
  "Formación local de referencia:",
  resultado_d1$referencia_local,
  "\n"
)

cat(
  "Formación visitante de referencia:",
  resultado_d1$referencia_visitante,
  "\n"
)

resultado_d <- ajustar_modelo_formaciones(
  datos = d,
  nombre_base = "d",
  referencia_preferida = "1-4-2-3-1"
)

resultado_d1 <- ajustar_modelo_formaciones(
  datos = d1,
  nombre_base = "d1",
  referencia_preferida = "1-4-2-3-1"
)

modelo_d <- resultado_d$modelo
modelo_d1 <- resultado_d1$modelo

enfrentamientos_d <- resultado_d$resultados
enfrentamientos_d1 <- resultado_d1$resultados

summary(modelo_d)
summary(modelo_d1)

head(enfrentamientos_d)
head(enfrentamientos_d1)
# =========================================================
# 8. COMPROBAR LOS RESULTADOS CREADOS
# =========================================================

names(enfrentamientos_d)
names(enfrentamientos_d1)

dim(enfrentamientos_d)
dim(enfrentamientos_d1)

# Comprobar que no haya probabilidades u odds NA
colSums(
  is.na(
    enfrentamientos_d[
      c(
        "log_odds",
        "odds",
        "probabilidad"
      )
    ]
  )
)

colSums(
  is.na(
    enfrentamientos_d1[
      c(
        "log_odds",
        "odds",
        "probabilidad"
      )
    ]
  )
)
# =========================================================
# 8. COMPROBAR LOS RESULTADOS CREADOS
# =========================================================

names(enfrentamientos_d)
names(enfrentamientos_d1)

dim(enfrentamientos_d)
dim(enfrentamientos_d1)

# Comprobar que no haya probabilidades u odds NA
colSums(
  is.na(
    enfrentamientos_d[
      c(
        "log_odds",
        "odds",
        "probabilidad"
      )
    ]
  )
)

colSums(
  is.na(
    enfrentamientos_d1[
      c(
        "log_odds",
        "odds",
        "probabilidad"
      )
    ]
  )
)
# =========================================================
# 9. TABLAS ORDENADAS DE ENFRENTAMIENTOS
# =========================================================

tabla_enfrentamientos_d <- enfrentamientos_d %>%
  arrange(desc(probabilidad)) %>%
  select(
    formacion_local_texto,
    formacion_visit_texto,
    enfrentamiento,
    probabilidad,
    prob_inf,
    prob_sup,
    odds,
    odds_inf,
    odds_sup,
    n_partidos,
    observado
  )

tabla_enfrentamientos_d1 <- enfrentamientos_d1 %>%
  arrange(desc(probabilidad)) %>%
  select(
    formacion_local_texto,
    formacion_visit_texto,
    enfrentamiento,
    probabilidad,
    prob_inf,
    prob_sup,
    odds,
    odds_inf,
    odds_sup,
    n_partidos,
    observado
  )

print(
  tabla_enfrentamientos_d,
  n = Inf
)

print(
  tabla_enfrentamientos_d1,
  n = Inf
)
# =========================================================
# 12. FUNCIÓN PARA CALCULAR ODDS RATIOS
# =========================================================

calcular_or_referencia <- function(resultado) {
  
  modelo <- resultado$modelo
  tabla <- resultado$resultados
  
  ref_local <- resultado$referencia_local
  ref_visit <- resultado$referencia_visitante
  
  # -------------------------------------------------------
  # Crear data frame con todas las combinaciones
  # -------------------------------------------------------
  
  nueva_data <- data.frame(
    formacion_local_dep = factor(
      tabla$formacion_local_texto,
      levels = modelo$xlevels$formacion_local_dep
    ),
    formacion_visit_dep = factor(
      tabla$formacion_visit_texto,
      levels = modelo$xlevels$formacion_visit_dep
    )
  )
  
  # -------------------------------------------------------
  # Crear la combinación de referencia
  # -------------------------------------------------------
  
  datos_referencia <- data.frame(
    formacion_local_dep = factor(
      ref_local,
      levels = modelo$xlevels$formacion_local_dep
    ),
    formacion_visit_dep = factor(
      ref_visit,
      levels = modelo$xlevels$formacion_visit_dep
    )
  )
  
  # -------------------------------------------------------
  # Construir matrices de diseño
  # -------------------------------------------------------
  
  X <- model.matrix(
    delete.response(
      terms(modelo)
    ),
    data = nueva_data
  )
  
  X_ref <- model.matrix(
    delete.response(
      terms(modelo)
    ),
    data = datos_referencia
  )
  
  # -------------------------------------------------------
  # Obtener coeficientes y matriz de covarianzas
  # -------------------------------------------------------
  
  beta <- coef(modelo)
  V <- vcov(modelo)
  
  # Excluir posibles coeficientes no estimables
  validos <- !is.na(beta)
  
  X <- X[
    ,
    validos,
    drop = FALSE
  ]
  
  X_ref <- X_ref[
    ,
    validos,
    drop = FALSE
  ]
  
  beta <- beta[validos]
  
  V <- V[
    validos,
    validos,
    drop = FALSE
  ]
  
  # -------------------------------------------------------
  # Crear el contraste frente a la referencia
  # -------------------------------------------------------
  
  contraste <- sweep(
    X,
    MARGIN = 2,
    STATS = as.numeric(
      X_ref[1, ]
    ),
    FUN = "-"
  )
  
  # -------------------------------------------------------
  # Calcular log(OR)
  # -------------------------------------------------------
  
  log_OR_calculado <- as.vector(
    contraste %*% beta
  )
  
  # -------------------------------------------------------
  # Calcular error estándar de log(OR)
  # -------------------------------------------------------
  
  varianza_log_OR <- diag(
    contraste %*%
      V %*%
      t(contraste)
  )
  
  error_log_OR_calculado <- sqrt(
    pmax(
      0,
      varianza_log_OR
    )
  )
  
  z <- qnorm(0.975)
  
  # -------------------------------------------------------
  # Añadir los resultados a la tabla
  # -------------------------------------------------------
  
  tabla %>%
    mutate(
      log_OR = log_OR_calculado,
      
      error_log_OR =
        error_log_OR_calculado,
      
      OR = exp(log_OR),
      
      OR_inf = exp(
        log_OR -
          z * error_log_OR
      ),
      
      OR_sup = exp(
        log_OR +
          z * error_log_OR
      ),
      
      significativo = case_when(
        OR_inf > 1 ~
          "Significativamente mayor que 1",
        
        OR_sup < 1 ~
          "Significativamente menor que 1",
        
        TRUE ~
          "IC95% incluye 1"
      ),
      
      referencia = paste0(
        ref_local,
        " local vs ",
        ref_visit,
        " visitante"
      )
    )
}
# =========================================================
# 13. CALCULAR OR PARA d Y d1
# =========================================================

or_d <- calcular_or_referencia(
  resultado_d
)

or_d1 <- calcular_or_referencia(
  resultado_d1
)

# Comprobar las primeras filas
head(or_d)
head(or_d1)

# Ver nombres de columnas
names(or_d)
names(or_d1)
# =========================================================
# 14. TABLA DE OR PARA d
# =========================================================

tabla_or_d <- or_d %>%
  select(
    formacion_local_texto,
    formacion_visit_texto,
    enfrentamiento,
    OR,
    OR_inf,
    OR_sup,
    significativo,
    probabilidad,
    n_partidos,
    observado
  ) %>%
  arrange(desc(OR))

print(
  tabla_or_d,
  n = Inf
)
# =========================================================
# 15. TABLA DE OR PARA d1
# =========================================================

tabla_or_d1 <- or_d1 %>%
  select(
    formacion_local_texto,
    formacion_visit_texto,
    enfrentamiento,
    OR,
    OR_inf,
    OR_sup,
    significativo,
    probabilidad,
    n_partidos,
    observado
  ) %>%
  arrange(desc(OR))

print(
  tabla_or_d1,
  n = Inf
)  
# Convertir las tablas a tibble y mostrar todas las filas

print(
  tibble::as_tibble(tabla_enfrentamientos_d),
  n = Inf
)

print(
  tibble::as_tibble(tabla_enfrentamientos_d1),
  n = Inf
)

print(
  tibble::as_tibble(tabla_or_d),
  n = Inf
)

print(
  tibble::as_tibble(tabla_or_d1),
  n = Inf
)
# =========================================================
# CORREGIR EL FORMATO DE LAS CUATRO TABLAS
# =========================================================

tabla_enfrentamientos_d <- tibble::as_tibble(
  tabla_enfrentamientos_d
)

# =========================================================
# HEATMAP DE ODDS RATIOS PARA d
# =========================================================

grafico_or_d <- ggplot(
  or_d,
  aes(
    x = formacion_visit_texto,
    y = formacion_local_texto,
    fill = log_OR
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = paste0(
        sprintf("%.2f", OR),
        "\n",
        "n = ",
        n_partidos
      ),
      fontface = ifelse(
        OR_inf > 1 | OR_sup < 1,
        "bold",
        "plain"
      )
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#1A9850",
    midpoint = 0,
    name = "log(OR)"
  ) +
  labs(
    title = "Odds ratio de victoria local por combinación",
    subtitle = paste0(
      "Base d | Referencia: ",
      resultado_d$referencia_local,
      " local vs ",
      resultado_d$referencia_visitante,
      " visitante"
    ),
    x = "Formación visitante",
    y = "Formación local",
    caption = paste0(
      "OR > 1 indica mayores odds de victoria local ",
      "que el enfrentamiento de referencia. ",
      "Negrita: IC95% no incluye 1."
    )
  ) +
  coord_fixed() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

grafico_or_d
# =========================================================
# HEATMAP DE ODDS RATIOS PARA d1
# =========================================================

grafico_or_d1 <- ggplot(
  or_d1,
  aes(
    x = formacion_visit_texto,
    y = formacion_local_texto,
    fill = log_OR
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = paste0(
        sprintf("%.2f", OR),
        "\n",
        "n = ",
        n_partidos
      ),
      fontface = ifelse(
        OR_inf > 1 | OR_sup < 1,
        "bold",
        "plain"
      )
    ),
    size = 2.7,
    show.legend = FALSE
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#1A9850",
    midpoint = 0,
    name = "log(OR)"
  ) +
  labs(
    title = "Odds ratio de victoria local por combinación",
    subtitle = paste0(
      "Base d1 | Referencia: ",
      resultado_d1$referencia_local,
      " local vs ",
      resultado_d1$referencia_visitante,
      " visitante"
    ),
    x = "Formación visitante",
    y = "Formación local",
    caption = paste0(
      "OR > 1 indica mayores odds de victoria local ",
      "que el enfrentamiento de referencia. ",
      "Negrita: IC95% no incluye 1."
    )
  ) +
  coord_fixed() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

grafico_or_d1
# =========================================================
# CORREGIR LAS TABLAS DE OR SEGÚN EL NÚMERO DE PARTIDOS
# =========================================================

# Umbral mínimo por enfrentamiento
umbral_celda <- 5

or_d_corregido <- or_d %>%
  mutate(
    informacion_celda = case_when(
      n_partidos == 0 ~ "No observado",
      n_partidos < umbral_celda ~ "Muestra insuficiente",
      TRUE ~ "Analizable"
    ),
    
    # Solo mostrar OR si existen al menos 5 partidos
    OR_mostrar = ifelse(
      n_partidos >= umbral_celda,
      OR,
      NA_real_
    ),
    
    log_OR_mostrar = ifelse(
      n_partidos >= umbral_celda,
      log_OR,
      NA_real_
    ),
    
    OR_inf_mostrar = ifelse(
      n_partidos >= umbral_celda,
      OR_inf,
      NA_real_
    ),
    
    OR_sup_mostrar = ifelse(
      n_partidos >= umbral_celda,
      OR_sup,
      NA_real_
    ),
    
    significativo_mostrar = case_when(
      n_partidos < umbral_celda ~ FALSE,
      OR_inf > 1 ~ TRUE,
      OR_sup < 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    etiqueta = case_when(
      n_partidos == 0 ~
        "Sin datos",
      
      n_partidos < umbral_celda ~
        paste0(
          "n = ",
          n_partidos,
          "\nInsuf."
        ),
      
      TRUE ~
        paste0(
          sprintf("%.2f", OR),
          "\nn = ",
          n_partidos
        )
    )
  )

or_d1_corregido <- or_d1 %>%
  mutate(
    informacion_celda = case_when(
      n_partidos == 0 ~ "No observado",
      n_partidos < umbral_celda ~ "Muestra insuficiente",
      TRUE ~ "Analizable"
    ),
    
    OR_mostrar = ifelse(
      n_partidos >= umbral_celda,
      OR,
      NA_real_
    ),
    
    log_OR_mostrar = ifelse(
      n_partidos >= umbral_celda,
      log_OR,
      NA_real_
    ),
    
    OR_inf_mostrar = ifelse(
      n_partidos >= umbral_celda,
      OR_inf,
      NA_real_
    ),
    
    OR_sup_mostrar = ifelse(
      n_partidos >= umbral_celda,
      OR_sup,
      NA_real_
    ),
    
    significativo_mostrar = case_when(
      n_partidos < umbral_celda ~ FALSE,
      OR_inf > 1 ~ TRUE,
      OR_sup < 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    etiqueta = case_when(
      n_partidos == 0 ~
        "Sin datos",
      
      n_partidos < umbral_celda ~
        paste0(
          "n = ",
          n_partidos,
          "\nInsuf."
        ),
      
      TRUE ~
        paste0(
          sprintf("%.2f", OR),
          "\nn = ",
          n_partidos
        )
    )
  )
# =========================================================
# HEATMAP CORREGIDO PARA d
# =========================================================

grafico_or_d_corregido <- ggplot(
  or_d_corregido,
  aes(
    x = formacion_visit_texto,
    y = formacion_local_texto
  )
) +
  
  # Fondo gris para todas las celdas
  geom_tile(
    fill = "grey90",
    color = "white",
    linewidth = 0.5
  ) +
  
  # Color únicamente para celdas analizables
  geom_tile(
    data = or_d_corregido %>%
      filter(
        informacion_celda == "Analizable"
      ),
    aes(
      fill = log_OR_mostrar
    ),
    color = "white",
    linewidth = 0.5
  ) +
  
  # Etiquetas
  geom_text(
    aes(
      label = etiqueta,
      fontface = ifelse(
        significativo_mostrar,
        "bold",
        "plain"
      ),
      color = informacion_celda
    ),
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#1A9850",
    midpoint = 0,
    name = "log(OR)",
    na.value = "grey90"
  ) +
  
  scale_color_manual(
    values = c(
      "No observado" = "grey50",
      "Muestra insuficiente" = "grey35",
      "Analizable" = "black"
    )
  ) +
  
  labs(
    title = "Odds ratio de victoria local por combinación",
    subtitle = paste0(
      "Base d | Solo se muestra el OR cuando n ≥ ",
      umbral_celda
    ),
    x = "Formación visitante",
    y = "Formación local",
    caption = paste0(
      "Referencia: ",
      resultado_d$referencia_local,
      " local vs ",
      resultado_d$referencia_visitante,
      " visitante. ",
      "Gris: combinación no observada o muestra insuficiente. ",
      "Modelo sin interacción."
    )
  ) +
  
  coord_fixed() +
  
  theme_minimal(base_size = 11) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

grafico_or_d_corregido
# =========================================================
# HEATMAP CORREGIDO PARA d1
# =========================================================

grafico_or_d1_corregido <- ggplot(
  or_d1_corregido,
  aes(
    x = formacion_visit_texto,
    y = formacion_local_texto
  )
) +
  
  # Fondo gris para todas las celdas
  geom_tile(
    fill = "grey90",
    color = "white",
    linewidth = 0.5
  ) +
  
  # Color únicamente para celdas analizables
  geom_tile(
    data = or_d1_corregido %>%
      filter(
        informacion_celda == "Analizable"
      ),
    aes(
      fill = log_OR_mostrar
    ),
    color = "white",
    linewidth = 0.5
  ) +
  
  # Etiquetas
  geom_text(
    aes(
      label = etiqueta,
      fontface = ifelse(
        significativo_mostrar,
        "bold",
        "plain"
      ),
      color = informacion_celda
    ),
    size = 2.7,
    show.legend = FALSE
  ) +
  
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#1A9850",
    midpoint = 0,
    name = "log(OR)",
    na.value = "grey90"
  ) +
  
  scale_color_manual(
    values = c(
      "No observado" = "grey50",
      "Muestra insuficiente" = "grey35",
      "Analizable" = "black"
    )
  ) +
  
  labs(
    title = "Odds ratio de victoria local por combinación",
    subtitle = paste0(
      "Base d1 | Solo se muestra el OR cuando n ≥ ",
      umbral_celda
    ),
    x = "Formación visitante",
    y = "Formación local",
    caption = paste0(
      "Referencia: ",
      resultado_d1$referencia_local,
      " local vs ",
      resultado_d1$referencia_visitante,
      " visitante. ",
      "Gris: combinación no observada o muestra insuficiente. ",
      "Modelo sin interacción."
    )
  ) +
  
  coord_fixed() +
  
  theme_minimal(base_size = 11) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

grafico_or_d1_corregido
#############################################################
modelo_d
modelo_d1
resultado_d
resultado_d1
datos_modelo_d
datos_modelo_d1
# =========================================================
# FOREST PLOTS DE OR AJUSTADOS
# =========================================================

library(dplyr)
library(ggplot2)

# Recuperar los datos completos usados en cada modelo
if (!exists("datos_modelo_d")) {
  datos_modelo_d <- resultado_d$datos_modelo
}

if (!exists("datos_modelo_d1")) {
  datos_modelo_d1 <- resultado_d1$datos_modelo
}
# =========================================================
# FUNCIÓN DE FRECUENCIAS POR FORMACIÓN
# =========================================================

calcular_frecuencias_formaciones <- function(datos) {
  
  frecuencias_local <- datos %>%
    filter(
      !is.na(formacion_local_dep)
    ) %>%
    count(
      formacion_local_dep,
      name = "n"
    ) %>%
    transmute(
      Equipo = "Local",
      Formacion = as.character(
        formacion_local_dep
      ),
      n = n
    )
  
  frecuencias_visitante <- datos %>%
    filter(
      !is.na(formacion_visit_dep)
    ) %>%
    count(
      formacion_visit_dep,
      name = "n"
    ) %>%
    transmute(
      Equipo = "Visitante",
      Formacion = as.character(
        formacion_visit_dep
      ),
      n = n
    )
  
  bind_rows(
    frecuencias_local,
    frecuencias_visitante
  )
}
frecuencias_d <- calcular_frecuencias_formaciones(
  datos_modelo_d
)

frecuencias_d1 <- calcular_frecuencias_formaciones(
  datos_modelo_d1
)

print(
  tibble::as_tibble(frecuencias_d),
  n = Inf
)

print(
  tibble::as_tibble(frecuencias_d1),
  n = Inf
)
# =========================================================
# FUNCIÓN PARA CALCULAR FRECUENCIAS MARGINALES
# =========================================================

calcular_frecuencias_formaciones <- function(datos) {
  
  frecuencias_local <- datos %>%
    filter(
      !is.na(formacion_local_dep)
    ) %>%
    count(
      formacion_local_dep,
      name = "n"
    ) %>%
    transmute(
      Equipo = "Local",
      Formacion = as.character(
        formacion_local_dep
      ),
      n = as.integer(n)
    )
  
  frecuencias_visitante <- datos %>%
    filter(
      !is.na(formacion_visit_dep)
    ) %>%
    count(
      formacion_visit_dep,
      name = "n"
    ) %>%
    transmute(
      Equipo = "Visitante",
      Formacion = as.character(
        formacion_visit_dep
      ),
      n = as.integer(n)
    )
  
  bind_rows(
    frecuencias_local,
    frecuencias_visitante
  )
}
# =========================================================
# FUNCIÓN COMPLETA PARA EXTRAER OR AJUSTADOS
# =========================================================

extraer_or_ajustados <- function(
    modelo,
    datos,
    nombre_base,
    referencia_local,
    referencia_visitante
) {
  
  # -------------------------------------------------------
  # Comprobaciones previas
  # -------------------------------------------------------
  
  if (!inherits(modelo, "glm")) {
    stop("El objeto 'modelo' debe ser un modelo de clase glm.")
  }
  
  columnas_necesarias <- c(
    "formacion_local_dep",
    "formacion_visit_dep"
  )
  
  columnas_faltantes <- setdiff(
    columnas_necesarias,
    names(datos)
  )
  
  if (length(columnas_faltantes) > 0) {
    stop(
      "En 'datos' faltan estas columnas: ",
      paste(
        columnas_faltantes,
        collapse = ", "
      )
    )
  }
  
  # -------------------------------------------------------
  # Extraer matriz de coeficientes
  # -------------------------------------------------------
  
  matriz <- summary(modelo)$coefficients
  
  tabla <- data.frame(
    termino = rownames(matriz),
    beta = matriz[, "Estimate"],
    error = matriz[, "Std. Error"],
    valor_z = matriz[, "z value"],
    p = matriz[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  
  # -------------------------------------------------------
  # Conservar únicamente coeficientes de formaciones
  # -------------------------------------------------------
  
  tabla <- tabla %>%
    filter(
      grepl(
        "^formacion_local_dep|^formacion_visit_dep",
        termino
      )
    ) %>%
    mutate(
      Equipo = ifelse(
        grepl(
          "^formacion_local_dep",
          termino
        ),
        "Local",
        "Visitante"
      ),
      
      Formacion = case_when(
        Equipo == "Local" ~ sub(
          "^formacion_local_dep",
          "",
          termino
        ),
        
        Equipo == "Visitante" ~ sub(
          "^formacion_visit_dep",
          "",
          termino
        ),
        
        TRUE ~ NA_character_
      ),
      
      OR = exp(beta),
      
      OR_inf = exp(
        beta - 1.96 * error
      ),
      
      OR_sup = exp(
        beta + 1.96 * error
      ),
      
      Base = nombre_base,
      
      Referencia = ifelse(
        Equipo == "Local",
        referencia_local,
        referencia_visitante
      )
    )
  
  # -------------------------------------------------------
  # Calcular frecuencias marginales
  # -------------------------------------------------------
  
  frecuencias <- calcular_frecuencias_formaciones(
    datos
  )
  
  tabla <- tabla %>%
    left_join(
      frecuencias,
      by = c(
        "Equipo",
        "Formacion"
      )
    )
  
  # -------------------------------------------------------
  # Obtener frecuencia de las referencias
  # -------------------------------------------------------
  
  n_ref_local <- frecuencias %>%
    filter(
      Equipo == "Local",
      Formacion == referencia_local
    ) %>%
    pull(n)
  
  n_ref_visitante <- frecuencias %>%
    filter(
      Equipo == "Visitante",
      Formacion == referencia_visitante
    ) %>%
    pull(n)
  
  if (length(n_ref_local) == 0) {
    n_ref_local <- NA_integer_
  } else {
    n_ref_local <- as.integer(
      n_ref_local[1]
    )
  }
  
  if (length(n_ref_visitante) == 0) {
    n_ref_visitante <- NA_integer_
  } else {
    n_ref_visitante <- as.integer(
      n_ref_visitante[1]
    )
  }
  
  # -------------------------------------------------------
  # Crear filas para las categorías de referencia
  # -------------------------------------------------------
  
  referencias <- data.frame(
    termino = c(
      "Referencia local",
      "Referencia visitante"
    ),
    
    beta = c(
      0,
      0
    ),
    
    error = c(
      0,
      0
    ),
    
    valor_z = c(
      NA_real_,
      NA_real_
    ),
    
    p = c(
      NA_real_,
      NA_real_
    ),
    
    Equipo = c(
      "Local",
      "Visitante"
    ),
    
    Formacion = c(
      referencia_local,
      referencia_visitante
    ),
    
    OR = c(
      1,
      1
    ),
    
    OR_inf = c(
      1,
      1
    ),
    
    OR_sup = c(
      1,
      1
    ),
    
    Base = c(
      nombre_base,
      nombre_base
    ),
    
    Referencia = c(
      referencia_local,
      referencia_visitante
    ),
    
    n = c(
      n_ref_local,
      n_ref_visitante
    ),
    
    stringsAsFactors = FALSE
  )
  
  # -------------------------------------------------------
  # Unir coeficientes y referencias
  # -------------------------------------------------------
  
  resultado <- bind_rows(
    tabla,
    referencias
  ) %>%
    mutate(
      es_referencia = grepl(
        "^Referencia",
        termino
      ),
      
      # El resultado se clasifica según el IC95%
      significacion = case_when(
        es_referencia ~
          "Referencia",
        
        OR_inf > 1 ~
          "Favorable",
        
        OR_sup < 1 ~
          "Desfavorable",
        
        TRUE ~
          "No concluyente"
      ),
      
      # Dirección de la asociación
      asociacion = case_when(
        es_referencia ~
          "Categoría de referencia",
        
        OR > 1 ~
          "Mayores odds de victoria local",
        
        OR < 1 ~
          "Menores odds de victoria local",
        
        TRUE ~
          "Sin diferencia respecto a la referencia"
      ),
      
      # Clasificación descriptiva de la frecuencia
      precision_muestral = case_when(
        es_referencia ~
          "Referencia",
        
        is.na(n) ~
          "Frecuencia desconocida",
        
        n < 10 ~
          "Muy pocas observaciones",
        
        n < 20 ~
          "Muestra reducida",
        
        n < 50 ~
          "Muestra moderada",
        
        TRUE ~
          "Mayor respaldo muestral"
      ),
      
      # Etiqueta que aparecerá en el eje vertical
      etiqueta_formacion = paste0(
        Formacion,
        " (n = ",
        ifelse(
          is.na(n),
          "NA",
          as.character(n)
        ),
        ")"
      ),
      
      # Etiqueta completa del resultado
      etiqueta_resultado = case_when(
        es_referencia ~ paste0(
          "Referencia, n = ",
          ifelse(
            is.na(n),
            "NA",
            as.character(n)
          )
        ),
        
        TRUE ~ paste0(
          "OR ",
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
          "], p = ",
          ifelse(
            is.na(p),
            "NA",
            format.pval(
              p,
              digits = 3,
              eps = 0.001
            )
          ),
          ", n = ",
          ifelse(
            is.na(n),
            "NA",
            as.character(n)
          )
        )
      ),
      
      # Etiqueta abreviada para tablas o gráficos
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
      ),
      
      # Indicador lógico de si el IC95% excluye 1
      IC_excluye_1 = case_when(
        es_referencia ~
          NA,
        
        OR_inf > 1 | OR_sup < 1 ~
          TRUE,
        
        TRUE ~
          FALSE
      ),
      
      # Interpretación específica por equipo
      interpretacion = case_when(
        es_referencia ~
          "Categoría de referencia",
        
        Equipo == "Local" &
          OR_inf > 1 ~
          "Asociación ajustada favorable con la victoria local",
        
        Equipo == "Local" &
          OR_sup < 1 ~
          "Asociación ajustada desfavorable con la victoria local",
        
        Equipo == "Local" &
          OR > 1 ~
          "Tendencia favorable, pero el IC95% incluye 1",
        
        Equipo == "Local" &
          OR < 1 ~
          "Tendencia desfavorable, pero el IC95% incluye 1",
        
        Equipo == "Visitante" &
          OR_inf > 1 ~
          paste0(
            "Asociada con mayores odds de victoria local ",
            "cuando la utiliza el visitante"
          ),
        
        Equipo == "Visitante" &
          OR_sup < 1 ~
          paste0(
            "Asociada con menores odds de victoria local ",
            "cuando la utiliza el visitante"
          ),
        
        Equipo == "Visitante" &
          OR > 1 ~
          paste0(
            "Tendencia hacia mayores odds de victoria local, ",
            "pero el IC95% incluye 1"
          ),
        
        Equipo == "Visitante" &
          OR < 1 ~
          paste0(
            "Tendencia hacia menores odds de victoria local, ",
            "pero el IC95% incluye 1"
          ),
        
        TRUE ~
          "Resultado no clasificable"
      )
    ) %>%
    arrange(
      factor(
        Equipo,
        levels = c(
          "Local",
          "Visitante"
        )
      ),
      OR
    )
  return(resultado)
}
or_ajustados_d1 <- extraer_or_ajustados(
  modelo = modelo_d1,
  datos = datos_modelo_d1,
  nombre_base = "d1",
  referencia_local =
    resultado_d1$referencia_local,
  referencia_visitante =
    resultado_d1$referencia_visitante
)

head(or_ajustados_d1)
or_ajustados_d1 <- extraer_or_ajustados(
  modelo = modelo_d1,
  datos = datos_modelo_d1,
  nombre_base = "d1",
  referencia_local =
    resultado_d1$referencia_local,
  referencia_visitante =
    resultado_d1$referencia_visitante
)

head(or_ajustados_d1)
tabla_texto_d1 <- or_ajustados_d1 %>%
  select(
    Formacion,
    Equipo,
    OR,
    OR_inf,
    OR_sup,
    OR_IC95,
    p,
    n,
    significacion,
    precision_muestral,
    interpretacion
  ) %>%
  arrange(
    factor(
      Equipo,
      levels = c(
        "Local",
        "Visitante"
      )
    ),
    desc(OR)
  )
grafico_forest_d1 <- ggplot(
  or_ajustados_d1,
  aes(
    x = OR,
    y = reorder(
      etiqueta_formacion,
      OR
    ),
    color = significacion
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey35"
  ) +
  geom_errorbar(
    data = or_ajustados_d1 %>%
      filter(
        !es_referencia
      ),
    aes(
      xmin = OR_inf,
      xmax = OR_sup
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.7
  ) +
  geom_point(
    aes(
      shape = precision_muestral
    ),
    size = 3.2,
    stroke = 0.9
  ) +
  scale_x_log10(
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
      "Favorable" = "#1B9E77",
      "Desfavorable" = "#D95F02",
      "No concluyente" = "#7570B3",
      "Referencia" = "grey35"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Muy pocas observaciones" = 1,
      "Muestra reducida" = 2,
      "Muestra moderada" = 16,
      "Mayor respaldo muestral" = 16,
      "Referencia" = 15,
      "Frecuencia desconocida" = 4
    )
  ) +
  facet_grid(
    Equipo ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  labs(
    title = "Odds ratios ajustados de las formaciones",
    subtitle = paste0(
      "Base d1 | Modelo conjunto sin interacción | ",
      "Referencia: ",
      resultado_d1$referencia_local,
      " local y ",
      resultado_d1$referencia_visitante,
      " visitante"
    ),
    x = paste0(
      "Odds ratio ajustado de victoria local ",
      "(escala logarítmica)"
    ),
    y = NULL,
    color = "Resultado del IC95%",
    shape = "Frecuencia",
    caption = paste0(
      "Los puntos representan OR ajustados y las líneas sus IC95%. ",
      "La línea discontinua representa OR = 1. ",
      "La frecuencia entre paréntesis es la frecuencia marginal ",
      "de cada formación."
    )
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    strip.text.y = element_text(
      face = "bold",
      size = 11
    ),
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8.5
    )
  )

grafico_forest_d1


# =========================================================
# RECUPERAR LOS DATOS UTILIZADOS EN EL MODELO d
# =========================================================

if (!exists("datos_modelo_d")) {
  
  if (
    exists("resultado_d") &&
    "datos_modelo" %in% names(resultado_d)
  ) {
    
    datos_modelo_d <- resultado_d$datos_modelo
    
  } else {
    
    stop(
      "No existe 'datos_modelo_d' ni puede recuperarse ",
      "desde resultado_d$datos_modelo."
    )
  }
}

# =========================================================
# COMPROBAR QUE EXISTEN LOS OBJETOS NECESARIOS
# =========================================================

if (!exists("modelo_d")) {
  stop(
    "No existe 'modelo_d'. Debes ejecutar primero ",
    "el ajuste del modelo para la base d."
  )
}

if (!exists("resultado_d")) {
  stop(
    "No existe 'resultado_d'. Debes ejecutar primero ",
    "ajustar_modelo_formaciones() para la base d."
  )
}

if (!exists("extraer_or_ajustados")) {
  stop(
    "No existe la función 'extraer_or_ajustados'. ",
    "Ejecuta primero la definición completa de la función."
  )
}

# =========================================================
# CREAR or_ajustados_d
# =========================================================

or_ajustados_d <- extraer_or_ajustados(
  modelo = modelo_d,
  datos = datos_modelo_d,
  nombre_base = "d",
  referencia_local =
    resultado_d$referencia_local,
  referencia_visitante =
    resultado_d$referencia_visitante
)

# =========================================================
# COMPROBAR QUE SE HA CREADO CORRECTAMENTE
# =========================================================

if (!exists("or_ajustados_d")) {
  stop(
    "No se pudo crear el objeto 'or_ajustados_d'."
  )
}

if (!is.data.frame(or_ajustados_d)) {
  stop(
    "'or_ajustados_d' existe, pero no es un data.frame."
  )
}

columnas_or_necesarias <- c(
  "Formacion",
  "Equipo",
  "OR",
  "OR_inf",
  "OR_sup",
  "OR_IC95",
  "p",
  "n",
  "significacion",
  "precision_muestral",
  "interpretacion",
  "es_referencia",
  "etiqueta_formacion"
)

columnas_or_faltantes <- setdiff(
  columnas_or_necesarias,
  names(or_ajustados_d)
)

if (length(columnas_or_faltantes) > 0) {
  stop(
    "En 'or_ajustados_d' faltan estas columnas: ",
    paste(
      columnas_or_faltantes,
      collapse = ", "
    )
  )
}

cat(
  "Objeto 'or_ajustados_d' creado correctamente con ",
  nrow(or_ajustados_d),
  " filas.\n"
)

head(or_ajustados_d)
# =========================================================
# TABLA RESUMIDA DE OR AJUSTADOS PARA d
# =========================================================

tabla_texto_d <- or_ajustados_d %>%
  select(
    Formacion,
    Equipo,
    OR,
    OR_inf,
    OR_sup,
    OR_IC95,
    p,
    n,
    significacion,
    precision_muestral,
    interpretacion
  ) %>%
  arrange(
    factor(
      Equipo,
      levels = c(
        "Local",
        "Visitante"
      )
    ),
    desc(OR)
  )

print(
  tibble::as_tibble(tabla_texto_d),
  n = Inf,
  width = Inf
)
# =========================================================
# FRECUENCIAS DE LAS FORMACIONES EN d
# =========================================================

frecuencias_d <- calcular_frecuencias_formaciones(
  datos_modelo_d
)

print(
  tibble::as_tibble(frecuencias_d),
  n = Inf,
  width = Inf
)
# Formaciones con menos de 20 partidos

frecuencias_reducidas_d <- frecuencias_d %>%
  filter(
    n < 20
  ) %>%
  arrange(
    Equipo,
    n
  )

print(
  tibble::as_tibble(frecuencias_reducidas_d),
  n = Inf,
  width = Inf
)
# =========================================================
# PREPARAR DATOS PARA EL FOREST PLOT DE d
# =========================================================

or_grafico_d <- or_ajustados_d %>%
  mutate(
    Equipo = factor(
      Equipo,
      levels = c(
        "Local",
        "Visitante"
      )
    ),
    
    # Etiqueta visible
    etiqueta_visible = paste0(
      Formacion,
      " (n = ",
      ifelse(
        is.na(n),
        "NA",
        as.character(n)
      ),
      ")"
    ),
    
    # Identificador interno único
    identificador = paste(
      Equipo,
      Formacion,
      sep = " | "
    )
  )
orden_local_d <- or_grafico_d %>%
  filter(
    Equipo == "Local"
  ) %>%
  arrange(OR) %>%
  pull(etiqueta_visible)

orden_visitante_d <- or_grafico_d %>%
  filter(
    Equipo == "Visitante"
  ) %>%
  arrange(OR) %>%
  pull(etiqueta_visible)

orden_total_d <- unique(
  c(
    orden_local_d,
    orden_visitante_d
  )
)

or_grafico_d <- or_grafico_d %>%
  mutate(
    etiqueta_visible = factor(
      etiqueta_visible,
      levels = orden_total_d
    )
  )
# =========================================================
# LÍMITES DEL EJE X
# =========================================================

limite_inferior_d <- min(
  or_grafico_d$OR_inf[
    is.finite(or_grafico_d$OR_inf) &
      or_grafico_d$OR_inf > 0
  ],
  na.rm = TRUE
)

limite_superior_d <- max(
  or_grafico_d$OR_sup[
    is.finite(or_grafico_d$OR_sup) &
      or_grafico_d$OR_sup > 0
  ],
  na.rm = TRUE
)

# Añadir margen visual
limite_inferior_d <- limite_inferior_d / 1.20
limite_superior_d <- limite_superior_d * 1.20

# Límites de seguridad
if (
  !is.finite(limite_inferior_d) ||
  limite_inferior_d <= 0
) {
  limite_inferior_d <- 0.05
}

if (
  !is.finite(limite_superior_d) ||
  limite_superior_d <= 1
) {
  limite_superior_d <- 10
}

cat(
  "Límite inferior del gráfico:",
  limite_inferior_d,
  "\n"
)

cat(
  "Límite superior del gráfico:",
  limite_superior_d,
  "\n"
)
# =========================================================
# FOREST PLOT DE OR AJUSTADOS PARA d
# =========================================================

grafico_OR_d <- ggplot(
  or_grafico_d,
  aes(
    x = OR,
    y = etiqueta_visible
  )
) +
  
  # Línea de ausencia de asociación
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey35"
  ) +
  
  # Intervalos de confianza
  geom_errorbarh(
    data = or_grafico_d %>%
      filter(
        !es_referencia,
        is.finite(OR_inf),
        is.finite(OR_sup),
        OR_inf > 0,
        OR_sup > 0
      ),
    aes(
      xmin = OR_inf,
      xmax = OR_sup,
      y = etiqueta_visible,
      color = significacion
    ),
    height = 0.16,
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  
  # Puntos de los OR
  geom_point(
    aes(
      color = significacion,
      shape = precision_muestral
    ),
    size = 3.4,
    stroke = 0.9
  ) +
  
  # Escala logarítmica
  scale_x_log10(
    limits = c(
      limite_inferior_d,
      limite_superior_d
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
      20,
      50
    )
  ) +
  
  # Colores según resultado del IC95%
  scale_color_manual(
    values = c(
      "Favorable" = "#1B9E77",
      "Desfavorable" = "#D95F02",
      "No concluyente" = "#7570B3",
      "Referencia" = "grey35"
    ),
    drop = FALSE
  ) +
  
  # Formas según respaldo muestral
  scale_shape_manual(
    values = c(
      "Muy pocas observaciones" = 1,
      "Muestra reducida" = 2,
      "Muestra moderada" = 16,
      "Mayor respaldo muestral" = 17,
      "Referencia" = 15,
      "Frecuencia desconocida" = 4
    ),
    drop = FALSE
  ) +
  
  # Separar local y visitante
  facet_grid(
    Equipo ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  labs(
    title = "Odds ratios ajustados de las formaciones",
    subtitle = paste0(
      "Base d | Modelo conjunto sin interacción | ",
      "Referencia local: ",
      resultado_d$referencia_local,
      " | Referencia visitante: ",
      resultado_d$referencia_visitante
    ),
    x = paste0(
      "Odds ratio ajustado de victoria local ",
      "(escala logarítmica)"
    ),
    y = NULL,
    color = "Resultado del IC95%",
    shape = "Respaldo muestral",
    caption = paste0(
      "Los puntos representan los odds ratios ajustados y ",
      "las líneas sus intervalos de confianza del 95%. ",
      "La línea discontinua representa OR = 1. ",
      "La frecuencia n es marginal para cada formación y posición."
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
    
    strip.text.y = element_text(
      face = "bold",
      size = 11
    ),
    
    strip.background = element_rect(
      fill = "grey95",
      color = NA
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      size = 10.5
    ),
    
    plot.caption = element_text(
      hjust = 0,
      size = 8.5
    ),
    
    axis.text.y = element_text(
      size = 9.5
    )
  )

grafico_OR_d
