# ==============================================================================
# 02_validaciones.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
# Fecha:   2026-09-17
#
# Este script revisa qué tan sólidos son los resultados de
# 01_resultados_principales.R. Cada validación responde a una pregunta que
# probablemente hará el jurado.
#
# Se corre desde la raíz del repositorio (abriendo TESIS_MECA.Rproj), después
# de haber corrido 01_resultados_principales.R al menos una vez.
#
# Resultados: todo se guarda en 4. RESULTADOS/Validaciones/script_validaciones/
#   tablas V01 a V06 y R01 (Word y CSV)
#   gráficos en la subcarpeta "figuras"
#   V00_tablero_validaciones.docx   resumen de todo
#   V00_compendio_validaciones.docx todas las tablas juntas
#
# Las validaciones:
#   V1. ¿Cuánto aguanta el resultado si los grupos no venían exactamente parecidos?
#   V2. ¿Cambia el resultado según con qué años comparamos?
#   V3. ¿El salto salarial de 2023 es efecto del mínimo o recuperación de 2022?
#       (medimos Kaitz en 2019)
#   V4. ¿Qué pasa en 2023 y 2024 por separado? (2024 no tuvo subsidios)
#   V5. ¿Los resultados exploratorios venían con diferencias previas?
#   V6. Placebo: ¿aparece un "efecto" en 2018, cuando no hubo choque?
#   V7. ¿Cambia sin las firmas con salarios extremos?
#   V8. ¿Cambia con más o menos controles?
#   V9. ¿Cambia con solo las firmas presentes todos los años?
#   V10. ¿Cambia si agrupamos los errores por sector?
#   V11. ¿Qué medida de Kaitz usar? (2022, 2019 o promedio 2019-2021-2022)
#        La regla para elegir está escrita en NOTA_DECISIONES.md ANTES de correr.
#   V12. ¿Qué control mueve el resultado del empleo? (sector, tamaño, departamento)
# ==============================================================================


# ==============================================================================
# 0. PREPARACIÓN
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

# Todo lo que genera este script se guarda en su propia subcarpeta dentro de
# Validaciones, para no mezclarlo con lo que guarda el script principal
CARPETA_VALIDACIONES <- file.path("4. RESULTADOS", "Validaciones", "script_validaciones")
dir.create(file.path(CARPETA_VALIDACIONES, "figuras"), recursive = TRUE, showWarnings = FALSE)

# --- Funciones de apoyo (iguales a las de 01_resultados_principales.R) ---------

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 20) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3, carpeta = CARPETA_VALIDACIONES) {
  tabla_word <- flextable(tabla)
  tabla_word <- colformat_double(tabla_word, digits = decimales)
  tabla_word <- set_caption(tabla_word, caption = titulo_tabla)
  tabla_word <- autofit(tabla_word)
  save_as_docx(tabla_word, path = file.path(carpeta, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(carpeta, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tabla_word
  cat("Tabla guardada en", carpeta, ":", nombre_archivo, "\n")
}

graficos <- list()
guardar_grafico <- function(grafico, nombre_archivo, carpeta = CARPETA_VALIDACIONES, ancho = 9, alto = 5.5) {
  print(grafico)
  ggsave(file.path(carpeta, "figuras", paste0(nombre_archivo, ".png")),
         grafico, width = ancho, height = alto, dpi = 200, bg = "white")
  graficos[[nombre_archivo]] <<- grafico
  cat("Gráfico guardado en", file.path(carpeta, "figuras"), ":", nombre_archivo, "\n")
}

estrellas <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
}

# texto_p(): escribe el p-valor como "< 0,001" cuando es muy pequeño
texto_p <- function(p) ifelse(p < 0.001, "< 0,001", format(round(p, 3), nsmall = 3))

tema_tesis <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey35"),
        plot.caption = element_text(color = "grey45", hjust = 0),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"
ANIOS_EJE  <- c(2015:2019, 2021:2024)


# ==============================================================================
# 1. DATOS (misma preparación que 01_resultados_principales.R)
# ==============================================================================
titulo("1. DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)),
         salario_promedio = ifelse(empleo_total > 0, costos_totales_personal_total_c3r10c3 / empleo_total, NA))

cat("Panel de firmas:", nrow(panel), "filas,", n_distinct(panel$NORDEMP), "firmas\n")

# Kaitz 2022, recortado al 1% y 99% y en desviaciones estándar
firmas_2022 <- panel %>%
  filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
  select(NORDEMP, Bite2022_obreros, CIIU4, DPTO, tamano_empresa)

limites <- quantile(firmas_2022$Bite2022_obreros, probs = c(0.01, 0.99))

firmas_2022 <- firmas_2022 %>%
  mutate(
    kaitz       = pmin(pmax(Bite2022_obreros, limites[1]), limites[2]),
    kaitz_de    = kaitz / sd(kaitz),
    exposicion  = factor(ifelse(kaitz > median(kaitz), "Alta exposición", "Baja exposición"),
                         levels = c("Baja exposición", "Alta exposición")),
    sector_2022 = factor(CIIU4),
    depto_2022  = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  ) %>%
  select(NORDEMP, kaitz, kaitz_de, exposicion, sector_2022, depto_2022, tamano_2022)

# Base de análisis
datos <- panel %>%
  inner_join(firmas_2022, by = "NORDEMP") %>%
  mutate(
    ANIO_F      = factor(ANIO),
    post        = as.integer(ANIO >= 2023),
    log_empleo  = log(ifelse(empleo_total > 0, empleo_total, NA)),
    log_salario = log(ifelse(salario_promedio > 0, salario_promedio, NA)),
    log_ventas  = log(ifelse(VALORVEN > 0, VALORVEN, NA)),
    salario_obrero         = ifelse(obreros_permanentes > 0, sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA),
    salario_administrativo = ifelse(administrativos_permanentes > 0, sueldos_permanentes_administrativos_c3r2c2 / administrativos_permanentes, NA),
    salario_profesional    = ifelse(profesionales_permanentes > 0, sueldos_permanentes_profesionales_c3r2pt / profesionales_permanentes, NA),
    log_salario_obrero         = log(ifelse(salario_obrero > 0, salario_obrero, NA)),
    log_salario_administrativo = log(ifelse(salario_administrativo > 0, salario_administrativo, NA)),
    log_salario_profesional    = log(ifelse(salario_profesional > 0, salario_profesional, NA))
  )

# Brechas salariales, recortadas al 1% y 99% como en el script principal
recortar <- function(x) {
  limites_brecha <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  ifelse(x < limites_brecha[1] | x > limites_brecha[2], NA, x)
}
datos <- datos %>%
  mutate(brecha_admin_obrero = recortar(log_salario_administrativo - log_salario_obrero),
         brecha_prof_obrero  = recortar(log_salario_profesional - log_salario_obrero))

cat("Base de análisis:", nrow(datos), "filas,", n_distinct(datos$NORDEMP), "firmas\n")

EFECTOS_FIJOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
NOTA_MODELO <- paste("Controles: firma, año, sector x año, tamaño x año y departamento x año (fijados en 2022).",
                     "\nIntervalos de confianza al 95%, errores agrupados por firma. 2020 no está en la base (pandemia).")


# ==============================================================================
# 2. FUNCIONES DE ESTIMACIÓN
# ==============================================================================

# estimar_did(): diferencias en diferencias. Por defecto compara antes y después
# de 2023 (variable "post"), pero podemos cambiar la variable de tratamiento, la
# medida de exposición, la muestra, los controles y cómo agrupamos los errores.
estimar_did <- function(variable, base = datos, efectos = EFECTOS_FIJOS, etiqueta = variable,
                        tratamiento = "post", medida = "kaitz_de", agrupar = ~NORDEMP) {
  termino <- paste0(tratamiento, ":", medida)
  formula_did <- as.formula(paste0(variable, " ~ ", termino, " | ", efectos))
  modelo <- feols(formula_did, data = base, cluster = agrupar)
  fila <- coeftable(modelo)[termino, ]
  tibble(
    resultado      = etiqueta,
    coeficiente    = fila[["Estimate"]],
    error_estandar = fila[["Std. Error"]],
    p_valor        = fila[["Pr(>|t|)"]],
    significancia  = estrellas(fila[["Pr(>|t|)"]]),
    ic95_inferior  = fila[["Estimate"]] - 1.96 * fila[["Std. Error"]],
    ic95_superior  = fila[["Estimate"]] + 1.96 * fila[["Std. Error"]],
    observaciones  = nobs(modelo),
    firmas         = n_distinct(base$NORDEMP[obs(modelo)])
  )
}

# estudio_evento(): coeficientes año por año frente a 2022, con prueba conjunta
# de los años previos. Devuelve la tabla, el p-valor y el modelo.
estudio_evento <- function(variable, base = datos, medida = "kaitz_de", efectos = EFECTOS_FIJOS) {
  formula_evento <- as.formula(paste0(variable, " ~ i(ANIO_F, ", medida, ", ref = '2022') | ", efectos))
  modelo <- feols(formula_evento, data = base, cluster = ~NORDEMP)
  coeficientes <- as.data.frame(coeftable(modelo))
  
  tabla <- tibble(
    anio           = as.integer(gsub(paste0("ANIO_F::|:", medida), "", rownames(coeficientes))),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]]
  ) %>%
    bind_rows(tibble(anio = 2022L, coeficiente = 0, error_estandar = 0)) %>%
    mutate(ic95_inferior = coeficiente - 1.96 * error_estandar,
           ic95_superior = coeficiente + 1.96 * error_estandar) %>%
    arrange(anio)
  
  p_antes <- wald(modelo, keep = paste0("ANIO_F::(2015|2016|2017|2018|2019|2021):", medida), print = FALSE)$p
  
  list(tabla = tabla, p_antes = p_antes, modelo = modelo, medida = medida)
}

# contraste(): combina coeficientes de un estudio de evento con pesos.
# Ejemplo: c(`2023` = 0.5, `2024` = 0.5) es el promedio de 2023 y 2024 frente a 2022.
contraste <- function(evento, pesos, etiqueta) {
  nombres <- paste0("ANIO_F::", names(pesos), ":", evento$medida)
  b <- coef(evento$modelo)[nombres]
  V <- vcov(evento$modelo)[nombres, nombres, drop = FALSE]
  estimado <- sum(pesos * b)
  error <- sqrt(as.numeric(t(pesos) %*% V %*% pesos))
  p <- 2 * pnorm(-abs(estimado / error))
  tibble(medida = etiqueta, coeficiente = estimado, error_estandar = error,
         p_valor = p, significancia = estrellas(p),
         ic95_inferior = estimado - 1.96 * error, ic95_superior = estimado + 1.96 * error)
}

# Pesos que usamos varias veces
PROMEDIO_POST     <- c(`2023` = 0.5, `2024` = 0.5)
PRE_TODOS         <- c(`2015` = 1, `2016` = 1, `2017` = 1, `2018` = 1, `2019` = 1, `2021` = 1)  # 2022 = 0
POST_MENOS_PRE    <- c(PROMEDIO_POST, -PRE_TODOS / 7)   # promedio 2023-24 menos promedio 2015-2022


# ==============================================================================
# V1. ¿CUÁNTO AGUANTA EL RESULTADO SI LOS GRUPOS NO VENÍAN EXACTAMENTE PARECIDOS?
# ==============================================================================
titulo("V1. SENSIBILIDAD A DIFERENCIAS PREVIAS")

# Pregunta del jurado: "La prueba de años previos no rechaza, pero eso no prueba
# que los grupos venían parecidos. ¿Qué pasa si no?"
#
# Idea (inspirada en Honest DiD, Rambachan y Roth 2023): miramos cuánto se
# movía la diferencia entre grupos de un año a otro ANTES de 2023. Suponemos
# que después de 2023 la diferencia se podría mover por otras razones hasta M
# veces ese movimiento máximo, y ampliamos el intervalo de confianza en esa
# cantidad.
#   M = 0: los grupos habrían seguido exactamente igual (supuesto normal).
#   M = 1: se podrían haber separado tanto como el peor año previo.
#   M = 2: el doble.
# El "M de quiebre" es el M a partir del cual el intervalo incluye el cero.
#
# Es una versión simplificada. La versión formal (paquete HonestDiD) va al final
# de esta sección y solo corre si el paquete está instalado.

sensibilidad_simple <- function(evento, pesos, etiqueta) {
  # Movimiento máximo de un año al siguiente antes de 2023 (incluye 2022 = 0)
  previos <- evento$tabla %>% filter(anio <= 2022) %>% arrange(anio)
  movimiento_maximo <- max(abs(diff(previos$coeficiente)))
  
  # Años después de 2022 que pesa cada coeficiente (2023 = 1, 2024 = 2)
  distancia <- as.numeric(names(pesos)) - 2022
  distancia[distancia < 0] <- 0
  sesgo_por_M <- movimiento_maximo * sum(abs(pesos) * distancia)
  
  base <- contraste(evento, pesos, etiqueta)
  resultado <- tidyr::expand_grid(base, M = c(0, 0.5, 1, 2)) %>%
    mutate(ic_ampliado_inferior = ic95_inferior - M * sesgo_por_M,
           ic_ampliado_superior = ic95_superior + M * sesgo_por_M,
           incluye_cero = ic_ampliado_inferior <= 0 & ic_ampliado_superior >= 0,
           movimiento_maximo_previo = movimiento_maximo)
  
  M_quiebre <- (abs(base$coeficiente) - 1.96 * base$error_estandar) / sesgo_por_M
  resultado %>% mutate(M_de_quiebre = ifelse(M_quiebre <= 0, 0, M_quiebre))
}

evento_salario     <- estudio_evento("log_salario")
evento_empleo      <- estudio_evento("empleo_total")
evento_log_empleo  <- estudio_evento("log_empleo")

v1 <- bind_rows(
  sensibilidad_simple(evento_salario,    c(`2023` = 1), "Salario (log): cambio 2022 -> 2023"),
  sensibilidad_simple(evento_empleo,     PROMEDIO_POST, "Empleo (trabajadores): promedio 2023-24 frente a 2022"),
  sensibilidad_simple(evento_log_empleo, PROMEDIO_POST, "Empleo (log): promedio 2023-24 frente a 2022")
)
ver(v1)
guardar_tabla(v1, "V01_sensibilidad_diferencias_previas",
              "V1. Intervalos ampliados si los grupos se hubieran separado hasta M veces el peor movimiento previo",
              decimales = 4)

cat("\nLECTURA: si el M de quiebre es 0, el resultado ya incluye el cero con el supuesto normal.",
    "\nSi es mayor que 1, el resultado aguanta diferencias tan grandes como las del peor año previo.\n")

# Gráfico: intervalo ampliado según M para el empleo en log
grafico_v1 <- v1 %>%
  filter(medida == "Empleo (log): promedio 2023-24 frente a 2022") %>%
  ggplot(aes(x = factor(M), y = 100 * coeficiente)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_errorbar(aes(ymin = 100 * ic_ampliado_inferior, ymax = 100 * ic_ampliado_superior),
                width = 0.2, color = COLOR_BAJA) +
  geom_point(size = 3, color = COLOR_ALTA) +
  labs(title = "Empleo total (log): ¿cuánto aguanta el resultado?",
       subtitle = "Intervalo de confianza ampliado si los grupos se hubieran separado hasta M veces el peor movimiento previo",
       x = "M", y = "Efecto promedio 2023-2024 (%)", caption = NOTA_MODELO) +
  tema_tesis
guardar_grafico(grafico_v1, "GV01_sensibilidad_empleo_log")

# --- Versión formal con el paquete HonestDiD (opcional) ------------------------------
# Solo corre si el paquete está instalado: renv::install("asheshrambachan/HonestDiD")
# Nota: el método supone años equidistantes; 2020 falta en la base, así que
# 2019 -> 2021 cuenta como un solo paso.
if (requireNamespace("HonestDiD", quietly = TRUE)) {
  cat("\nCorriendo la versión formal de Honest DiD (magnitudes relativas)...\n")
  honest_empleo <- tryCatch({
    modelo <- evento_log_empleo$modelo
    orden  <- paste0("ANIO_F::", c(2015:2019, 2021, 2023, 2024), ":kaitz_de")
    betahat <- coef(modelo)[orden]
    sigma   <- vcov(modelo)[orden, orden]
    sensibilidad <- HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = betahat, sigma = sigma, numPrePeriods = 6, numPostPeriods = 2,
      Mbarvec = c(0.5, 1, 1.5, 2), l_vec = c(0.5, 0.5))
    original <- HonestDiD::constructOriginalCS(
      betahat = betahat, sigma = sigma, numPrePeriods = 6, numPostPeriods = 2, l_vec = c(0.5, 0.5))
    # HonestDiD devuelve los límites (lb y ub) como matrices de una columna;
    # los pasamos a números normales para poder guardar la tabla en Word
    bind_rows(original, sensibilidad) %>%
      mutate(lb = as.numeric(lb), ub = as.numeric(ub)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
      as_tibble()
  }, error = function(e) {
    cat("La versión formal no corrió:", conditionMessage(e), "\n")
    NULL
  })
  if (!is.null(honest_empleo)) {
    ver(honest_empleo)
    guardar_tabla(honest_empleo, "V01b_honest_did_empleo_log",
                  "V1b. Honest DiD (magnitudes relativas): empleo total (log), promedio 2023-2024", decimales = 4)
  }
} else {
  cat("\nPaquete HonestDiD no instalado: se omite la versión formal.",
      "\nPara instalarlo: renv::install('asheshrambachan/HonestDiD')\n")
}


# ==============================================================================
# V2. ¿CAMBIA EL RESULTADO SEGÚN CON QUÉ AÑOS COMPARAMOS?
# ==============================================================================
titulo("V2. SENSIBILIDAD AL PERÍODO DE COMPARACIÓN")

# Pregunta del jurado: "2022 fue un año de alto empleo. Si comparan contra 2022,
# ¿no están exagerando la caída?"
#
# Calculamos el efecto promedio de 2023-2024 contra cuatro referencias:
#   - 2022
#   - el promedio de 2021 y 2022 (pospandemia)
#   - el promedio de 2015 a 2019 (prepandemia)
#   - el promedio de todos los años previos (lo que usa la regresión simple)

referencias <- list(
  "Frente a 2022"               = PROMEDIO_POST,
  "Frente a 2021-2022"          = c(PROMEDIO_POST, `2021` = -0.5),
  "Frente a 2015-2019"          = c(PROMEDIO_POST, `2015` = -0.2, `2016` = -0.2, `2017` = -0.2, `2018` = -0.2, `2019` = -0.2),
  "Frente a todos los previos"  = POST_MENOS_PRE
)

v2 <- tibble()
for (referencia in names(referencias)) {
  v2 <- bind_rows(
    v2,
    contraste(evento_empleo,     referencias[[referencia]], referencia) %>% mutate(resultado = "Empleo (trabajadores)"),
    contraste(evento_log_empleo, referencias[[referencia]], referencia) %>% mutate(resultado = "Empleo (log)"),
    contraste(evento_salario,    referencias[[referencia]], referencia) %>% mutate(resultado = "Salario (log)")
  )
}
v2 <- v2 %>% relocate(resultado) %>% rename(comparacion = medida)
ver(v2)
guardar_tabla(v2, "V02_periodo_de_comparacion",
              "V2. Efecto promedio 2023-2024 según el período de comparación", decimales = 4)

grafico_v2 <- v2 %>%
  filter(resultado != "Empleo (trabajadores)") %>%
  ggplot(aes(x = comparacion, y = 100 * coeficiente, color = resultado)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = 100 * ic95_inferior, ymax = 100 * ic95_superior),
                  position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c(`Salario (log)` = COLOR_BAJA, `Empleo (log)` = COLOR_ALTA)) +
  labs(title = "El resultado según el período de comparación",
       subtitle = "Efecto promedio 2023-2024 por una desviación estándar más de Kaitz (%)",
       x = NULL, y = "Efecto (%)", color = NULL, caption = NOTA_MODELO) +
  tema_tesis +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
guardar_grafico(grafico_v2, "GV02_periodo_de_comparacion")


# ==============================================================================
# V3. ¿EL SALTO SALARIAL DE 2023 ES EFECTO DEL MÍNIMO O RECUPERACIÓN DE 2022?
# ==============================================================================
titulo("V3. KAITZ MEDIDO EN 2019")

# Pregunta del jurado: "Las firmas con Kaitz alto en 2022 tenían salarios bajos
# ese año. Si en 2023 suben, ¿no es simplemente que volvieron a lo normal?"
#
# Si el salto de 2023 es recuperación, debería aparecer solo cuando medimos
# Kaitz en 2022. Si es efecto del mínimo, debería aparecer también midiendo
# Kaitz en 2019, un año lejos del choque.
#
# Kaitz 2019 = salario mínimo 2020 (anual, miles de pesos) / salario promedio
# del obrero permanente en 2019. Misma fórmula que Kaitz 2022.

SM_ANUAL_MILES <- c(`2020` = 877803, `2023` = 1160000) * 12 / 1000

salario_obrero_por_anio <- panel %>%
  filter(ANIO %in% c(2019, 2022)) %>%
  mutate(salario_obrero = ifelse(obreros_permanentes > 0 & sueldos_permanentes_obreros_c3r2c1 > 0,
                                 sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA)) %>%
  select(NORDEMP, ANIO, salario_obrero, Bite2022_obreros)

# Chequeo: al recalcular Kaitz 2022 con el panel ampliado debería dar lo mismo
# que la variable Bite2022_obreros que ya tenemos
chequeo_kaitz <- salario_obrero_por_anio %>%
  filter(ANIO == 2022) %>%
  mutate(kaitz_2022_recalculado = SM_ANUAL_MILES[["2023"]] / salario_obrero)
cat("Chequeo Kaitz 2022 recalculado frente al original:",
    "\n  correlación:", round(cor(chequeo_kaitz$kaitz_2022_recalculado, chequeo_kaitz$Bite2022_obreros, use = "complete.obs"), 4),
    "\n  diferencia absoluta máxima:", round(max(abs(chequeo_kaitz$kaitz_2022_recalculado - chequeo_kaitz$Bite2022_obreros), na.rm = TRUE), 4), "\n")

# Construimos Kaitz 2019, recortado y en desviaciones estándar
kaitz_2019 <- salario_obrero_por_anio %>%
  filter(ANIO == 2019, !is.na(salario_obrero)) %>%
  mutate(kaitz19 = SM_ANUAL_MILES[["2020"]] / salario_obrero) %>%
  select(NORDEMP, kaitz19)
limites19 <- quantile(kaitz_2019$kaitz19, probs = c(0.01, 0.99))
kaitz_2019 <- kaitz_2019 %>%
  mutate(kaitz19 = pmin(pmax(kaitz19, limites19[1]), limites19[2]))

# Usamos las MISMAS firmas para las dos medidas, y cada una en sus propias
# desviaciones estándar dentro de esas firmas
firmas_ambas <- firmas_2022 %>%
  select(NORDEMP, kaitz) %>%
  inner_join(kaitz_2019, by = "NORDEMP") %>%
  mutate(kaitz19_de = kaitz19 / sd(kaitz19),
         kaitz22_de = kaitz / sd(kaitz))

cat("Firmas con Kaitz en 2019 y en 2022:", nrow(firmas_ambas), "\n")
cat("Correlación entre Kaitz 2019 y Kaitz 2022:", round(cor(firmas_ambas$kaitz19, firmas_ambas$kaitz), 3), "\n")

datos_v3 <- datos %>%
  inner_join(select(firmas_ambas, NORDEMP, kaitz19_de, kaitz22_de), by = "NORDEMP")

v3 <- tibble()
eventos_v3 <- list()
for (medida in c("kaitz22_de", "kaitz19_de")) {
  nombre_medida <- ifelse(medida == "kaitz22_de", "Kaitz 2022", "Kaitz 2019")
  
  ev_salario <- estudio_evento("log_salario", base = datos_v3, medida = medida)
  ev_empleo  <- estudio_evento("log_empleo",  base = datos_v3, medida = medida)
  eventos_v3[[nombre_medida]] <- ev_salario
  
  v3 <- bind_rows(
    v3,
    contraste(ev_salario, c(`2023` = 1), "Salario: cambio 2022 -> 2023") %>%
      mutate(medida_exposicion = nombre_medida, p_años_previos = ev_salario$p_antes),
    # Con Kaitz 2019 el salario ya cae en 2018-2019 (su año base). Por eso el
    # cambio típico se mide con 2015-2018 en las dos medidas.
    contraste(ev_salario, c(`2023` = 1, `2015` = 1/3, `2018` = -1/3), "Salario: salto 2023 frente al cambio típico 2015-2018") %>%
      mutate(medida_exposicion = nombre_medida, p_años_previos = ev_salario$p_antes),
    contraste(ev_empleo, POST_MENOS_PRE, "Empleo (log): promedio después menos promedio antes") %>%
      mutate(medida_exposicion = nombre_medida, p_años_previos = ev_empleo$p_antes)
  )
}
v3 <- v3 %>% relocate(medida_exposicion)
ver(v3)
guardar_tabla(v3, "V03_kaitz_2019_frente_a_2022",
              "V3. Salario y empleo con Kaitz medido en 2019 y en 2022 (mismas firmas)", decimales = 4)

# Gráfico: salario año por año con las dos medidas
grafico_v3 <- bind_rows(
  eventos_v3[["Kaitz 2022"]]$tabla %>% mutate(medida = "Kaitz 2022"),
  eventos_v3[["Kaitz 2019"]]$tabla %>% mutate(medida = "Kaitz 2019")
) %>%
  ggplot(aes(x = anio, y = coeficiente, color = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ic95_inferior, ymax = ic95_superior), position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(`Kaitz 2022` = COLOR_ALTA, `Kaitz 2019` = COLOR_BAJA)) +
  scale_x_continuous(breaks = ANIOS_EJE) +
  labs(title = "Salario promedio con Kaitz medido en 2019 y en 2022",
       subtitle = "Si el salto de 2023 aparece con las dos medidas, no es solo recuperación de 2022",
       x = NULL, y = "Diferencia en log del salario (referencia 2022 = 0)", color = NULL,
       caption = NOTA_MODELO) +
  tema_tesis
guardar_grafico(grafico_v3, "GV03_salario_kaitz_2019_vs_2022")


# ==============================================================================
# V4. ¿QUÉ PASA EN 2023 Y 2024 POR SEPARADO?
# ==============================================================================
titulo("V4. 2023 FRENTE A 2024")

# Pregunta del jurado: "En 2023 terminó el incentivo a nuevos empleos (agosto).
# ¿No es eso lo que ven, y no el salario mínimo?"
#
# 2024 no tuvo subsidios. Estimamos el efecto usando solo 2023 como año
# después y solo 2024 como año después. Si el efecto se mantiene en 2024, pesa
# más el salario mínimo que el fin del subsidio.

v4 <- tibble()
for (anio_post in c(2023, 2024)) {
  otro_anio <- setdiff(c(2023, 2024), anio_post)
  base_anio <- datos %>% filter(ANIO != otro_anio)
  etiqueta <- paste("Solo", anio_post)
  v4 <- bind_rows(
    v4,
    estimar_did("log_salario",  base = base_anio, etiqueta = "Salario (log)")         %>% mutate(periodo_despues = etiqueta),
    estimar_did("empleo_total", base = base_anio, etiqueta = "Empleo (trabajadores)") %>% mutate(periodo_despues = etiqueta),
    estimar_did("log_empleo",   base = base_anio, etiqueta = "Empleo (log)")          %>% mutate(periodo_despues = etiqueta)
  )
}

# Cambio entre 2023 y 2024 dentro del estudio de evento
v4 <- bind_rows(
  v4,
  contraste(evento_salario,    c(`2024` = 1, `2023` = -1), "Cambio 2023 -> 2024") %>%
    rename(resultado = medida) %>% mutate(periodo_despues = "Salario (log)"),
  contraste(evento_log_empleo, c(`2024` = 1, `2023` = -1), "Cambio 2023 -> 2024") %>%
    rename(resultado = medida) %>% mutate(periodo_despues = "Empleo (log)")
) %>%
  relocate(periodo_despues)
ver(v4)
guardar_tabla(v4, "V04_2023_frente_a_2024",
              "V4. Efecto usando solo 2023 o solo 2024 como año después (2024 sin subsidios)", decimales = 4)


# ==============================================================================
# V5. ¿LOS RESULTADOS EXPLORATORIOS VENÍAN CON DIFERENCIAS PREVIAS?
# ==============================================================================
titulo("V5. AÑOS PREVIOS DE LOS RESULTADOS EXPLORATORIOS")

# Pregunta del jurado: "Encontraron efectos en ventas, temporales y brechas
# salariales. ¿Esos grupos venían parecidos antes?"
#
# Si la prueba de años previos rechaza (p < 0,05), el resultado exploratorio
# no se puede leer como efecto de 2023.

variables_v5 <- c(
  "Empleo permanente (trabajadores)"        = "empleo_permanente",
  "Empleo temporal (trabajadores)"          = "empleo_temporal",
  "Participación de permanentes (puntos %)" = "participacion_permanente",
  "Ventas (log)"                            = "log_ventas",
  "Salario obrero (log)"                    = "log_salario_obrero",
  "Brecha administrativo - obrero (log)"    = "brecha_admin_obrero",
  "Brecha profesional - obrero (log)"       = "brecha_prof_obrero"
)

v5 <- tibble()
eventos_v5 <- list()
for (etiqueta in names(variables_v5)) {
  ev <- estudio_evento(variables_v5[[etiqueta]])
  eventos_v5[[etiqueta]] <- ev
  fila_2023 <- ev$tabla %>% filter(anio == 2023)
  fila_2024 <- ev$tabla %>% filter(anio == 2024)
  v5 <- bind_rows(v5, tibble(
    resultado = etiqueta,
    p_años_previos = ev$p_antes,
    años_previos_parecidos = ifelse(ev$p_antes >= 0.05, "Sí (no se rechaza)", "No (se rechaza)"),
    coeficiente_2023 = fila_2023$coeficiente,
    ic95_2023 = paste0("[", round(fila_2023$ic95_inferior, 4), "; ", round(fila_2023$ic95_superior, 4), "]"),
    coeficiente_2024 = fila_2024$coeficiente
  ))
}
ver(v5)
guardar_tabla(v5, "V05_años_previos_exploratorios",
              "V5. Prueba de años previos de los resultados exploratorios", decimales = 4)

# Gráficos de ventas y de la brecha administrativo-obrero
for (etiqueta in c("Ventas (log)", "Brecha administrativo - obrero (log)")) {
  ev <- eventos_v5[[etiqueta]]
  grafico <- ggplot(ev$tabla, aes(x = anio, y = coeficiente)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ic95_inferior, ymax = ic95_superior), width = 0.2, color = COLOR_BAJA) +
    geom_point(size = 2.5, color = ifelse(ev$tabla$anio >= 2023, COLOR_ALTA, COLOR_BAJA)) +
    scale_x_continuous(breaks = ANIOS_EJE) +
    labs(title = paste0(etiqueta, ": año por año"),
         subtitle = paste0("Efecto de una desviación estándar más de Kaitz. Referencia: 2022 (= 0)\n",
                           "Prueba conjunta de los años previos: p ", ifelse(ev$p_antes < 0.001, "< 0,001", paste0("= ", round(ev$p_antes, 3)))),
         x = NULL, y = "Coeficiente", caption = NOTA_MODELO) +
    tema_tesis
  nombre <- ifelse(etiqueta == "Ventas (log)", "GV05_evento_ventas", "GV05_evento_brecha_admin_obrero")
  guardar_grafico(grafico, nombre)
}


# ==============================================================================
# V6. PLACEBO: ¿APARECE UN "EFECTO" EN 2018, CUANDO NO HUBO CHOQUE?
# ==============================================================================
titulo("V6. PLACEBO EN 2018")

# Pregunta del jurado: "¿Cómo saben que su método no encuentra 'efectos' en
# cualquier año?"
#
# Usamos solo 2015-2019 y hacemos como si el choque hubiera sido en 2018. Si
# aparece un efecto grande y significativo, el método está captando
# diferencias que ya existían.

datos_placebo <- datos %>%
  filter(ANIO <= 2019) %>%
  mutate(post_placebo = as.integer(ANIO >= 2018),
         ANIO_F = droplevels(ANIO_F))

v6 <- bind_rows(
  estimar_did("log_salario",  base = datos_placebo, tratamiento = "post_placebo", etiqueta = "Salario (log)"),
  estimar_did("empleo_total", base = datos_placebo, tratamiento = "post_placebo", etiqueta = "Empleo (trabajadores)"),
  estimar_did("log_empleo",   base = datos_placebo, tratamiento = "post_placebo", etiqueta = "Empleo (log)"),
  estimar_did("log_ventas",   base = datos_placebo, tratamiento = "post_placebo", etiqueta = "Ventas (log)")
) %>%
  mutate(prueba = "Placebo: choque ficticio en 2018 (datos 2015-2019)") %>%
  relocate(prueba)
ver(v6)
guardar_tabla(v6, "V06_placebo_2018",
              "V6. Placebo: efecto de un choque ficticio en 2018 (datos 2015-2019)", decimales = 4)


# ==============================================================================
# V7. ¿CAMBIA SIN LAS FIRMAS CON SALARIOS EXTREMOS?
# ==============================================================================
titulo("V7. SIN SALARIOS EXTREMOS")

# Pregunta del jurado: "¿Los resultados dependen de unas pocas firmas con datos
# raros?"
#
# Quitamos las firmas-año con costo laboral por trabajador por debajo de 1
# salario mínimo o por encima de 10 (errores de reporte probables).

salario_minimo <- tibble(anio = 2015:2024,
                         valor = c(644350, 689455, 737717, 781242, 828116, 877803, 908526, 1000000, 1160000, 1300000))

datos_sin_extremos <- datos %>%
  left_join(salario_minimo, by = c("ANIO" = "anio")) %>%
  mutate(veces_minimo = (salario_promedio * 1000 / 12) / valor) %>%
  filter(!is.na(veces_minimo), veces_minimo >= 1, veces_minimo <= 10)

cat("Firmas-año que quitamos:", nrow(datos) - nrow(datos_sin_extremos), "de", nrow(datos), "\n")


# ==============================================================================
# V8. ¿CAMBIA CON MÁS O MENOS CONTROLES?
# ==============================================================================
titulo("V8. CON MÁS O MENOS CONTROLES")

# Pregunta del jurado: "¿Por qué esos controles? ¿Qué pasa sin ellos?"
#
# Mostramos el resultado con tres juegos de controles. Si cambia mucho, los
# controles importan y hay que justificarlos (sector, tamaño y departamento
# absorben diferencias entre firmas que no son del salario mínimo).

especificaciones <- c(
  "1. Solo firma y año"                     = "NORDEMP + ANIO_F",
  "2. Firma, año y sector x año"            = "NORDEMP + ANIO_F + sector_2022^ANIO_F",
  "3. Completo (principal)"                 = EFECTOS_FIJOS
)


# ==============================================================================
# V9. ¿CAMBIA CON SOLO LAS FIRMAS PRESENTES TODOS LOS AÑOS?
# ==============================================================================
titulo("V9. PANEL BALANCEADO")

# Pregunta del jurado: "Algunas firmas salen de la encuesta. ¿No será que las
# que salen son las que despidieron más?"
#
# Repetimos con las firmas que aparecen en los 9 años (2015-2019 y 2021-2024).

firmas_todos_los_anios <- datos %>%
  count(NORDEMP) %>%
  filter(n == 9) %>%
  pull(NORDEMP)
datos_balanceado <- datos %>% filter(NORDEMP %in% firmas_todos_los_anios)
cat("Firmas presentes los 9 años:", length(firmas_todos_los_anios), "de", n_distinct(datos$NORDEMP), "\n")


# ==============================================================================
# V7 A V10: TABLA DE ROBUSTEZ
# ==============================================================================
titulo("TABLA DE ROBUSTEZ (V7 A V10)")

# V10. Pregunta del jurado: "¿Y si los errores están correlacionados dentro del
# sector?" Agrupamos los errores por sector en vez de por firma.
#
# Para cada versión reportamos: el salario (cambio 2022 -> 2023, del estudio de
# evento) y el empleo total (regresión simple, en trabajadores y en log).

robustez_una <- function(base, efectos, etiqueta, agrupar = ~NORDEMP) {
  ev_salario <- estudio_evento("log_salario", base = base, efectos = efectos)
  # El cambio 2022 -> 2023 con errores agrupados como pide la versión
  if (!identical(agrupar, ~NORDEMP)) {
    ev_salario$modelo <- summary(ev_salario$modelo, cluster = agrupar)
  }
  bind_rows(
    contraste(ev_salario, c(`2023` = 1), "Salario (log): cambio 2022 -> 2023") %>% rename(resultado = medida),
    estimar_did("empleo_total", base = base, efectos = efectos, etiqueta = "Empleo (trabajadores)", agrupar = agrupar) %>%
      select(-observaciones, -firmas),
    estimar_did("log_empleo", base = base, efectos = efectos, etiqueta = "Empleo (log)", agrupar = agrupar) %>%
      select(-observaciones, -firmas)
  ) %>%
    mutate(version = etiqueta) %>%
    relocate(version)
}

robustez <- bind_rows(
  robustez_una(datos, EFECTOS_FIJOS, "0. Principal"),
  robustez_una(datos_sin_extremos, EFECTOS_FIJOS, "V7. Sin salarios extremos"),
  robustez_una(datos, especificaciones[[1]], paste("V8.", names(especificaciones)[1])),
  robustez_una(datos, especificaciones[[2]], paste("V8.", names(especificaciones)[2])),
  robustez_una(datos_balanceado, EFECTOS_FIJOS, "V9. Panel balanceado"),
  robustez_una(datos, EFECTOS_FIJOS, "V10. Errores agrupados por sector", agrupar = ~sector_2022)
)
ver(robustez, filas = 30)
guardar_tabla(robustez, "R01_tabla_robustez",
              "Robustez: salario y empleo total con distintas muestras, controles y errores", decimales = 4)

grafico_robustez <- robustez %>%
  filter(resultado %in% c("Salario (log): cambio 2022 -> 2023", "Empleo (log)")) %>%
  ggplot(aes(x = 100 * coeficiente, y = reorder(version, desc(version)), color = resultado)) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_pointrange(aes(xmin = 100 * ic95_inferior, xmax = 100 * ic95_superior),
                  position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(`Salario (log): cambio 2022 -> 2023` = COLOR_BAJA, `Empleo (log)` = COLOR_ALTA)) +
  labs(title = "¿Se mantienen los resultados?",
       subtitle = "Efecto por una desviación estándar más de Kaitz (%), con intervalo de confianza al 95%",
       x = "Efecto (%)", y = NULL, color = NULL,
       caption = "Salario: cambio 2022 -> 2023. Empleo: promedio después menos promedio antes.") +
  tema_tesis
guardar_grafico(grafico_robustez, "GR01_robustez", alto = 6)



# ==============================================================================
# V11. ¿QUÉ MEDIDA DE KAITZ USAR? 2022, 2019 O PROMEDIO
# ==============================================================================
titulo("V11. KAITZ 2022, 2019 Y PROMEDIO")

# Pregunta del jurado: "Si Kaitz 2022 mezcla firmas que tuvieron un 2022 malo,
# ¿por qué no usan una medida que no dependa de un solo año?"
#
# Kaitz de cada año = salario mínimo del año siguiente (anual, miles de pesos)
#                     / salario promedio del obrero permanente en ese año
#   2019 -> mínimo 2020;  2021 -> mínimo 2022;  2022 -> mínimo 2023
# Kaitz promedio = promedio de los Kaitz de 2019, 2021 y 2022 (cada año
# recortado al 1% y 99%), para firmas con al menos dos de los tres años.
# Promediar años reduce lo pasajero de un año malo.
#
# REGLA DE DECISIÓN (escrita en NOTA_DECISIONES.md antes de correr esto):
#   1. Una medida pasa el primer eslabón si el salto salarial de 2023 frente al
#      cambio típico 2015-2018 es positivo y significativo al 5%.
#   2. Entre las que pasan, la principal es la de mayor M de quiebre del
#      cambio salarial 2022 -> 2023 (la más robusta a diferencias previas).
#   3. El empleo NO se usa para elegir. Se reporta con las tres medidas.
#   4. Si ninguna pasa el primer eslabón, se declara un problema de diseño.
# Las tres medidas se comparan sobre las MISMAS firmas.

SM_ANIO_SIGUIENTE <- c(`2019` = 877803, `2021` = 1000000, `2022` = 1160000) * 12 / 1000

kaitz_por_anio <- panel %>%
  filter(ANIO %in% c(2019, 2021, 2022)) %>%
  mutate(salario_obrero = ifelse(obreros_permanentes > 0 & sueldos_permanentes_obreros_c3r2c1 > 0,
                                 sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA),
         kaitz_anio = SM_ANIO_SIGUIENTE[as.character(ANIO)] / salario_obrero) %>%
  filter(!is.na(kaitz_anio)) %>%
  group_by(ANIO) %>%
  # Recortamos cada año al 1% y 99% antes de promediar
  mutate(kaitz_anio = pmin(pmax(kaitz_anio, quantile(kaitz_anio, 0.01)), quantile(kaitz_anio, 0.99))) %>%
  ungroup() %>%
  select(NORDEMP, ANIO, kaitz_anio)

kaitz_promedio <- kaitz_por_anio %>%
  group_by(NORDEMP) %>%
  summarise(kaitz_prom = mean(kaitz_anio), anios_con_dato = n(), .groups = "drop") %>%
  filter(anios_con_dato >= 2)

ver(count(kaitz_promedio, anios_con_dato))

# Mismas firmas para las tres medidas, cada una en sus desviaciones estándar
firmas_tres <- firmas_ambas %>%
  select(NORDEMP, kaitz22 = kaitz, kaitz19) %>%
  inner_join(select(kaitz_promedio, NORDEMP, kaitz_prom), by = "NORDEMP") %>%
  mutate(kaitz22_c   = kaitz22 / sd(kaitz22),
         kaitz19_c   = kaitz19 / sd(kaitz19),
         kaitzprom_c = kaitz_prom / sd(kaitz_prom))

cat("Firmas con las tres medidas:", nrow(firmas_tres), "\n")
cat("Correlaciones entre medidas:\n")
print(round(cor(select(firmas_tres, kaitz22, kaitz19, kaitz_prom)), 3))

datos_v11 <- datos %>%
  inner_join(select(firmas_tres, NORDEMP, kaitz22_c, kaitz19_c, kaitzprom_c), by = "NORDEMP")

medidas_v11 <- c("Kaitz 2022" = "kaitz22_c", "Kaitz 2019" = "kaitz19_c", "Kaitz promedio" = "kaitzprom_c")

v11 <- tibble()
decision_v11 <- tibble()
eventos_v11_salario <- list()
eventos_v11_empleo  <- list()

for (nombre_medida in names(medidas_v11)) {
  medida <- medidas_v11[[nombre_medida]]
  ev_salario    <- estudio_evento("log_salario",  base = datos_v11, medida = medida)
  ev_log_empleo <- estudio_evento("log_empleo",   base = datos_v11, medida = medida)
  ev_empleo     <- estudio_evento("empleo_total", base = datos_v11, medida = medida)
  eventos_v11_salario[[nombre_medida]] <- ev_salario
  eventos_v11_empleo[[nombre_medida]]  <- ev_log_empleo
  
  salto_2023  <- contraste(ev_salario, c(`2023` = 1, `2015` = 1/3, `2018` = -1/3),
                           "Salario: salto 2023 frente al cambio típico 2015-2018")
  cambio_2023 <- contraste(ev_salario, c(`2023` = 1), "Salario: cambio 2022 -> 2023")
  M_quiebre   <- sensibilidad_simple(ev_salario, c(`2023` = 1), "x")$M_de_quiebre[1]
  
  filas_medida <- bind_rows(
    salto_2023  %>% mutate(p_años_previos = ev_salario$p_antes),
    cambio_2023 %>% mutate(p_años_previos = ev_salario$p_antes),
    contraste(ev_log_empleo, POST_MENOS_PRE, "Empleo (log): promedio después menos promedio antes") %>%
      mutate(p_años_previos = ev_log_empleo$p_antes),
    contraste(ev_empleo, POST_MENOS_PRE, "Empleo (trabajadores): promedio después menos promedio antes") %>%
      mutate(p_años_previos = ev_empleo$p_antes)
  ) %>%
    mutate(medida_exposicion = nombre_medida)
  v11 <- bind_rows(v11, filas_medida)
  
  decision_v11 <- bind_rows(decision_v11, tibble(
    medida_exposicion = nombre_medida,
    salto_salarial_2023 = salto_2023$coeficiente,
    p_salto = salto_2023$p_valor,
    pasa_primer_eslabon = salto_2023$coeficiente > 0 & salto_2023$p_valor < 0.05,
    M_de_quiebre_salario = M_quiebre,
    p_años_previos_empleo_log = ev_log_empleo$p_antes
  ))
}

v11 <- v11 %>% relocate(medida_exposicion)

# Aplicamos la regla de decisión
decision_v11 <- decision_v11 %>%
  mutate(elegida = pasa_primer_eslabon &
           M_de_quiebre_salario == max(M_de_quiebre_salario[pasa_primer_eslabon], -Inf))

ver(v11)
ver(decision_v11)
guardar_tabla(v11, "V11_kaitz_2022_2019_promedio",
              "V11. Primer eslabón y empleo con Kaitz 2022, 2019 y promedio (mismas firmas)", decimales = 4)
guardar_tabla(decision_v11, "V11b_regla_de_decision_medida",
              "V11b. Regla de decisión: medida de exposición principal", decimales = 4)

if (!any(decision_v11$pasa_primer_eslabon)) {
  cat("\nATENCIÓN: ninguna medida pasa el primer eslabón. Según la regla, es un problema de diseño.\n")
} else {
  cat("\nMedida elegida por la regla:", decision_v11$medida_exposicion[decision_v11$elegida], "\n")
}

# Gráficos: salario y empleo año por año con las tres medidas
colores_medidas <- c(`Kaitz 2022` = COLOR_ALTA, `Kaitz 2019` = COLOR_BAJA, `Kaitz promedio` = "#2E7D32")

grafico_tres <- function(eventos, titulo_grafico, eje_y) {
  bind_rows(lapply(names(eventos), function(n) mutate(eventos[[n]]$tabla, medida = n))) %>%
    ggplot(aes(x = anio, y = coeficiente, color = medida)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = ic95_inferior, ymax = ic95_superior), position = position_dodge(width = 0.6)) +
    scale_color_manual(values = colores_medidas) +
    scale_x_continuous(breaks = ANIOS_EJE) +
    labs(title = titulo_grafico,
         subtitle = "Una desviación estándar más de cada medida. Referencia: 2022 (= 0). Mismas firmas",
         x = NULL, y = eje_y, color = NULL, caption = NOTA_MODELO) +
    tema_tesis
}
guardar_grafico(grafico_tres(eventos_v11_salario, "Salario promedio con tres medidas de Kaitz",
                             "Diferencia en log del salario"), "GV11_salario_tres_medidas")
guardar_grafico(grafico_tres(eventos_v11_empleo, "Empleo total (log) con tres medidas de Kaitz",
                             "Diferencia en log del empleo"), "GV11_empleo_tres_medidas")


# ==============================================================================
# V12. ¿QUÉ CONTROL MUEVE EL RESULTADO DEL EMPLEO?
# ==============================================================================
titulo("V12. QUÉ CONTROL MUEVE EL RESULTADO")

# Pregunta del jurado: "Sin controles el empleo cae 4,7% y con controles casi
# nada. ¿Cuál control hace la diferencia y por qué es correcto incluirlo?"
#
# Agregamos los controles uno por uno y de a dos. Para cada versión mostramos
# el empleo y la prueba de años previos: si un control es necesario para que
# los grupos vengan parecidos antes de 2023, hay una razón para incluirlo.

especificaciones_v12 <- c(
  "1. Firma y año"                        = "NORDEMP + ANIO_F",
  "2. + sector x año"                     = "NORDEMP + ANIO_F + sector_2022^ANIO_F",
  "3. + tamaño x año"                     = "NORDEMP + ANIO_F + tamano_2022^ANIO_F",
  "4. + departamento x año"               = "NORDEMP + ANIO_F + depto_2022^ANIO_F",
  "5. + sector y tamaño x año"            = "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F",
  "6. + sector y departamento x año"      = "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F",
  "7. Completo (principal)"               = EFECTOS_FIJOS
)

v12 <- tibble()
for (nombre_esp in names(especificaciones_v12)) {
  efectos <- especificaciones_v12[[nombre_esp]]
  ev_log_empleo <- estudio_evento("log_empleo", efectos = efectos)
  ev_empleo     <- estudio_evento("empleo_total", efectos = efectos)
  filas_especificacion <- bind_rows(
    estimar_did("log_empleo", efectos = efectos, etiqueta = "Empleo (log)") %>%
      mutate(p_años_previos = ev_log_empleo$p_antes),
    estimar_did("empleo_total", efectos = efectos, etiqueta = "Empleo (trabajadores)") %>%
      mutate(p_años_previos = ev_empleo$p_antes)
  ) %>%
    mutate(controles = nombre_esp)
  v12 <- bind_rows(v12, filas_especificacion)
}
v12 <- v12 %>% relocate(controles) %>% select(-firmas)
ver(v12)
guardar_tabla(v12, "V12_que_control_mueve_el_empleo",
              "V12. Empleo total y prueba de años previos agregando controles uno por uno", decimales = 4)

grafico_v12 <- v12 %>%
  filter(resultado == "Empleo (log)") %>%
  mutate(etiqueta_p = paste0("p previos ", texto_p(p_años_previos))) %>%
  ggplot(aes(x = 100 * coeficiente, y = reorder(controles, desc(controles)))) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_pointrange(aes(xmin = 100 * ic95_inferior, xmax = 100 * ic95_superior), color = COLOR_ALTA) +
  geom_text(aes(label = etiqueta_p), vjust = -1, size = 3, color = "grey35") +
  labs(title = "Empleo total (log) según los controles",
       subtitle = "Efecto promedio después menos antes, por una desviación estándar más de Kaitz 2022 (%)",
       x = "Efecto (%)", y = NULL,
       caption = "p previos: prueba conjunta de los años previos del estudio de evento con los mismos controles.") +
  tema_tesis
guardar_grafico(grafico_v12, "GV12_controles_empleo", alto = 6)


# ==============================================================================
# 11. TABLERO DE VALIDACIONES
# ==============================================================================
titulo("11. TABLERO DE VALIDACIONES")

# Juntamos en una sola tabla la pregunta del jurado, el número que la responde
# y qué mirar. La interpretación final la hacemos nosotros al revisar.

efecto_texto <- function(coef, lim_inf, lim_sup, p, por_cien = TRUE) {
  factor <- ifelse(por_cien, 100, 1)
  unidad <- ifelse(por_cien, "%", "")
  paste0(round(factor * coef, 2), unidad, " [", round(factor * lim_inf, 2), "; ",
         round(factor * lim_sup, 2), "], p ", texto_p(p))
}

fila_v1  <- v1 %>% filter(medida == "Empleo (log): promedio 2023-24 frente a 2022", M == 1)
fila_v1s <- v1 %>% filter(medida == "Salario (log): cambio 2022 -> 2023", M == 1)
fila_v2a <- v2 %>% filter(resultado == "Empleo (log)", comparacion == "Frente a 2022")
fila_v2b <- v2 %>% filter(resultado == "Empleo (log)", comparacion == "Frente a 2015-2019")
fila_v3  <- v3 %>% filter(medida_exposicion == "Kaitz 2019", medida == "Salario: salto 2023 frente al cambio típico 2015-2018")
fila_v4  <- v4 %>% filter(periodo_despues == "Solo 2024", resultado == "Empleo (log)")
fila_v5  <- v5 %>% filter(resultado == "Ventas (log)")
fila_v5b <- v5 %>% filter(resultado == "Brecha administrativo - obrero (log)")
fila_v6  <- v6 %>% filter(resultado == "Empleo (log)")
rob_emp  <- robustez %>% filter(resultado == "Empleo (log)", version != "0. Principal")

tablero <- tribble(
  ~validacion, ~pregunta_del_jurado, ~resultado, ~que_mirar,
  "V1. Sensibilidad (salario)",
  "¿El aumento salarial aguanta si los grupos venían distintos?",
  paste0("Intervalo con M = 1: [", round(100 * fila_v1s$ic_ampliado_inferior, 2), "%; ",
         round(100 * fila_v1s$ic_ampliado_superior, 2), "%]. M de quiebre: ", round(fila_v1s$M_de_quiebre, 2)),
  "M de quiebre mayor que 1: el salto de 2023 supera el peor movimiento previo.",
  "V1. Sensibilidad (empleo)",
  "¿Qué rango de efectos sobre el empleo es compatible con los datos?",
  paste0("Intervalo con M = 1: [", round(100 * fila_v1$ic_ampliado_inferior, 2), "%; ",
         round(100 * fila_v1$ic_ampliado_superior, 2), "%]"),
  "El rango de caídas de empleo que no se pueden descartar.",
  "V2. Período de comparación",
  "¿El resultado depende de que 2022 fue un año alto?",
  paste0("Frente a 2022: ", efecto_texto(fila_v2a$coeficiente, fila_v2a$ic95_inferior, fila_v2a$ic95_superior, fila_v2a$p_valor),
         ". Frente a 2015-2019: ", efecto_texto(fila_v2b$coeficiente, fila_v2b$ic95_inferior, fila_v2b$ic95_superior, fila_v2b$p_valor)),
  "Si cambian de signo o de significancia, el resultado depende de la referencia.",
  "V3. Kaitz 2019",
  "¿El salto salarial es efecto del mínimo o recuperación de 2022?",
  paste0("Salto 2023 con Kaitz 2019: ", efecto_texto(fila_v3$coeficiente, fila_v3$ic95_inferior, fila_v3$ic95_superior, fila_v3$p_valor)),
  "Positivo y significativo: el salto no es solo recuperación del año base.",
  "V4. Solo 2024",
  "¿No es el fin del incentivo al empleo lo que ven?",
  paste0("Empleo (log), solo 2024: ", efecto_texto(fila_v4$coeficiente, fila_v4$ic95_inferior, fila_v4$ic95_superior, fila_v4$p_valor)),
  "Si 2024 se parece a 2023, pesa más el mínimo que el fin del subsidio.",
  "V5. Ventas",
  "¿Las ventas venían parecidas antes?",
  paste0("p años previos ", texto_p(fila_v5$p_años_previos)),
  "p < 0,05: el resultado de ventas no se lee como efecto de 2023.",
  "V5. Brecha salarial",
  "¿La compresión salarial venía de antes?",
  paste0("p años previos ", texto_p(fila_v5b$p_años_previos)),
  "p < 0,05: la compresión no se lee como efecto de 2023.",
  "V6. Placebo 2018",
  "¿El método encuentra 'efectos' en cualquier año?",
  paste0("Empleo (log): ", efecto_texto(fila_v6$coeficiente, fila_v6$ic95_inferior, fila_v6$ic95_superior, fila_v6$p_valor)),
  "No significativo: el método no inventa efectos donde no hubo choque.",
  "V7 a V10. Robustez",
  "¿Cambia con otra muestra, otros controles u otros errores?",
  paste0("Empleo (log) entre ", round(100 * min(rob_emp$coeficiente), 2), "% y ",
         round(100 * max(rob_emp$coeficiente), 2), "%; significativo en ",
         sum(rob_emp$p_valor < 0.05), " de ", nrow(rob_emp), " versiones"),
  "Ver R01: qué versiones cambian el resultado (en especial V8 sin controles)."
)

# Filas de V11 y V12
elegida <- decision_v11 %>% filter(elegida)
fila_v12_sin <- v12 %>% filter(resultado == "Empleo (log)", controles == "1. Firma y año")
fila_v12_tam <- v12 %>% filter(resultado == "Empleo (log)", controles == "3. + tamaño x año")
fila_v12_dep <- v12 %>% filter(resultado == "Empleo (log)", controles == "4. + departamento x año")

tablero <- bind_rows(tablero, tribble(
  ~validacion, ~pregunta_del_jurado, ~resultado, ~que_mirar,
  "V11. Medida de Kaitz",
  "¿Por qué esa medida de exposición y no otra?",
  ifelse(nrow(elegida) == 0, "Ninguna medida pasa el primer eslabón",
         paste0("Elegida por la regla: ", elegida$medida_exposicion,
                " (salto salarial ", round(100 * elegida$salto_salarial_2023, 2), "%, M de quiebre ",
                round(elegida$M_de_quiebre_salario, 2), ")")),
  "La regla está escrita antes de correr; el empleo no se usa para elegir.",
  "V12. Controles",
  "¿Qué control cambia el resultado del empleo?",
  paste0("Sin controles: ", round(100 * fila_v12_sin$coeficiente, 2), "% (p previos ", texto_p(fila_v12_sin$p_años_previos),
         "). + tamaño: ", round(100 * fila_v12_tam$coeficiente, 2), "% (p previos ", texto_p(fila_v12_tam$p_años_previos),
         "). + departamento: ", round(100 * fila_v12_dep$coeficiente, 2), "% (p previos ", texto_p(fila_v12_dep$p_años_previos), ")"),
  "El control que acerca el efecto a cero y limpia los años previos es el que justifica la especificación."
))

ver(tablero)
guardar_tabla(tablero, "V00_tablero_validaciones",
              "Tablero de validaciones: pregunta del jurado, resultado y qué mirar")

# Compendio con todas las tablas de este script
save_as_docx(values = compendio, path = file.path(CARPETA_VALIDACIONES, "V00_compendio_validaciones.docx"))
cat("\nCompendio guardado: V00_compendio_validaciones.docx con", length(compendio), "tablas.\n")

cat("\nGráficos generados:\n")
cat(paste(" -", names(graficos)), sep = "\n")

revisar_graficos <- function() {
  for (nombre in names(graficos)) {
    print(graficos[[nombre]])
    readline(paste0(nombre, " - Enter para el siguiente (Esc para salir)... "))
  }
}
cat("\nPara revisar los gráficos uno por uno, escribe en la consola: revisar_graficos()\n")

titulo("FIN DE LAS VALIDACIONES")

