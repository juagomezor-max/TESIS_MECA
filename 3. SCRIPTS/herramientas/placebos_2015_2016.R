# ==============================================================================
# placebos_2015_2016.R
#
# Objetivo: agregar 2015 y 2016 (aumentos reales del salario mínimo de 0,91%
# y 0,22%, los más bajos de la serie) como placebos adicionales al de 2019,
# con el mismo método de 06_tratamiento_continuo.R (sección C8/C9: las tres
# medidas de exposición A/B/C, salto por quintil, real frente a placebo).
#
# Script independiente, de solo lectura sobre "1. DATOS/" (no escribe ahi).
# Todo lo que se construye a partir del panel completo vive solo en memoria.
# Copia SOLO: la construcción de firmas_2022 (de 06, a partir del panel
# analítico), EFECTOS_FIJOS, EFECTOS_FIJOS_TAMANO, construir_medidas,
# salto_con_medida, salto_con_medida_tamano, y el cálculo de la comparación
# real-frente-a-placebo (C8.3/C9.3) -- NO corre 06 completo, NO lo modifica.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/panel_analitico_firma_eam.rds
#           4. RESULTADOS/06_tratamiento_continuo/T24_tres_medidas_real_vs_placebo.csv (validación)
#           4. RESULTADOS/06_tratamiento_continuo/T24_tres_medidas_real_vs_placebo_tamano.csv (validación)
# Salidas:  4. RESULTADOS/08_respuesta_jurados/T_placebos_2015_2016.csv
#           4. RESULTADOS/08_respuesta_jurados/figuras/G_placebos_costo.png
#           4. RESULTADOS/08_respuesta_jurados/figuras/G_placebos_empleo.png
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)

CARPETA <- file.path("4. RESULTADOS", "08_respuesta_jurados")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}
ver <- function(tabla, filas = 25) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}
estrellas <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                                                      ifelse(p < 0.10, "*", ""))))
}

COLOR_ALTA <- "#1F4E79"   # azul: 2023 (real)
COLOR_GRIS1 <- "grey65"
COLOR_GRIS2 <- "grey45"
COLOR_GRIS3 <- "grey25"

tema_tesis <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())


# ==============================================================================
# 1. DATOS (solo en memoria)
# ==============================================================================
titulo("1. DATOS (panel completo 2008-2024, en memoria)")

panel_completo <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)),
         empleo_total = empleo_total_sin_propietarios)   # renombrado, ver verificación previa

cat("Panel completo:", nrow(panel_completo), "filas,", n_distinct(panel_completo$NORDEMP), "firmas\n")

# --- firmas_2022: copiado literal de 06_tratamiento_continuo.R (líneas 138-167),
#     a partir del panel ANALÍTICO (no del completo) -- Bite2022_obreros,
#     kaitz, quintil, sector_2022, depto_2022 y tamano_2022 (clasificación de
#     tamaño fijada en 2022, igual que en 05 y 06). ---------------------------
panel_analitico <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

firmas_2022 <- panel_analitico %>%
  filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
  select(NORDEMP, Bite2022_obreros, CIIU4, DPTO, tamano_empresa)

limites <- quantile(firmas_2022$Bite2022_obreros, probs = c(0.01, 0.99))

firmas_2022 <- firmas_2022 %>%
  mutate(
    kaitz       = pmin(pmax(Bite2022_obreros, limites[1]), limites[2]),
    kaitz_de    = kaitz / sd(kaitz),
    quintil     = cut(kaitz, breaks = quantile(kaitz, probs = seq(0, 1, 0.2)),
                      labels = c("Q1 (menos expuestas)", "Q2", "Q3", "Q4", "Q5 (más expuestas)"),
                      include.lowest = TRUE),
    sector_2022 = factor(CIIU4),
    depto_2022  = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  ) %>%
  select(NORDEMP, kaitz, kaitz_de, quintil, sector_2022, depto_2022, tamano_2022)

cat("Firmas con Kaitz 2022 válido:", nrow(firmas_2022), "(esperado 5.099 para Bite; aquí son todas con clasificación de tamaño/sector/depto en 2022)\n")

# --- Clasificaciones de tamaño en otros años base, mismo método que
#     tamano_2022 pero a partir del panel COMPLETO (que sí cubre esos años). --
tamano_de_anio <- function(anio) {
  panel_completo %>%
    filter(ANIO == anio) %>%
    transmute(NORDEMP, !!paste0("tamano_", anio) := factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))
}
tamano_2014 <- tamano_de_anio(2014)
tamano_2015 <- tamano_de_anio(2015)
tamano_2018 <- tamano_de_anio(2018)

# --- 'datos': panel completo + clasificación de 2022 (Kaitz, sector, depto,
#     tamano_2022) + las clasificaciones de tamaño de 2014/2015/2018, todo
#     unido por NORDEMP. Años: 2010-2024, excluyendo 2008-2009 (crisis) y
#     2020 (pandemia, excluido en todo el estudio). --------------------------
datos <- panel_completo %>%
  inner_join(firmas_2022, by = "NORDEMP") %>%
  left_join(tamano_2014, by = "NORDEMP") %>%
  left_join(tamano_2015, by = "NORDEMP") %>%
  left_join(tamano_2018, by = "NORDEMP") %>%
  filter(ANIO %in% c(2010:2019, 2021:2024)) %>%
  mutate(
    ANIO_F = factor(ANIO),
    salario_promedio = ifelse(empleo_total > 0,
                              costos_totales_personal_total_c3r10c3 / empleo_total, NA_real_),
    log_empleo  = log(ifelse(empleo_total > 0, empleo_total, NA)),
    log_salario = log(ifelse(salario_promedio > 0, salario_promedio, NA))
  )

cat("\nFirmas-año en 'datos' (2010-2019, 2021-2024):", nrow(datos), "\n")
cat("Firmas distintas:", n_distinct(datos$NORDEMP), "\n")
cat("Años presentes:", paste(sort(unique(datos$ANIO)), collapse = ", "), "\n")

# --- Verificaciones pedidas: sector_2022/depto_2022 sin faltantes en la
#     muestra; % de faltantes de tamano_empresa (tamano_2022) en 2014, 2015 y
#     2018. CIIU4 falta en 2010/2011/2013 en el panel completo, pero no se
#     usa (sector va fijado en 2022 vía el join de firmas_2022). ------------
cat("\nFaltantes de sector_2022 en la muestra:", sum(is.na(datos$sector_2022)), "de", nrow(datos), "\n")
cat("Faltantes de depto_2022 en la muestra:", sum(is.na(datos$depto_2022)), "de", nrow(datos), "\n")
for (a in c(2014, 2015, 2018)) {
  sub <- datos %>% filter(ANIO == a)
  cat("  % faltantes de tamano_empresa (tamano_2022) en", a, ":",
      round(100 * mean(is.na(sub$tamano_2022)), 2), "% (", sum(is.na(sub$tamano_2022)), "de", nrow(sub), ")\n")
}


# ==============================================================================
# 2. FUNCIONES (copiadas literalmente de 06_tratamiento_continuo.R)
# ==============================================================================

EFECTOS_FIJOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F"
efectos_fijos_tamano_de <- function(var_tamano) paste(EFECTOS_FIJOS, "+", var_tamano, "^ANIO_F")
EFECTOS_FIJOS_TAMANO <- efectos_fijos_tamano_de("tamano_2022")   # valor inicial, igual al de 06

# construir_medidas() -- líneas 1033-1087 de 06, LITERAL (usa el global 'datos')
construir_medidas <- function(anio_base, anios_promedio, salario_minimo_siguiente) {
  salarios <- datos %>%
    filter(obreros_permanentes > 0, sueldos_permanentes_obreros_c3r2c1 > 0) %>%
    mutate(salario_obrero = sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes / 12 * 1000) %>%
    select(NORDEMP, ANIO, salario_obrero, sector_2022, depto_2022, tamano_2022)

  medida_a <- salarios %>%
    filter(ANIO == anio_base) %>%
    transmute(NORDEMP, sector_2022, depto_2022, tamano_2022,
              salario_base = salario_obrero)

  medida_b <- salarios %>%
    filter(ANIO %in% anios_promedio) %>%
    group_by(NORDEMP) %>%
    summarise(salario_promediado = mean(salario_obrero, na.rm = TRUE),
              anios = n(), .groups = "drop") %>%
    filter(anios >= 2)

  medida_c <- medida_a %>%
    group_by(sector_2022, depto_2022, tamano_2022) %>%
    mutate(firmas_en_celda = n(),
           suma_celda      = sum(salario_base, na.rm = TRUE),
           salario_ajenas  = (suma_celda - salario_base) / (firmas_en_celda - 1)) %>%
    ungroup() %>%
    filter(firmas_en_celda >= 5, is.finite(salario_ajenas), salario_ajenas > 0) %>%
    select(NORDEMP, salario_ajenas, firmas_en_celda)

  medidas <- medida_a %>%
    select(NORDEMP, salario_base) %>%
    left_join(medida_b, by = "NORDEMP") %>%
    left_join(medida_c, by = "NORDEMP") %>%
    mutate(kaitz_a = salario_minimo_siguiente / salario_base,
           kaitz_b = salario_minimo_siguiente / salario_promediado,
           kaitz_c = salario_minimo_siguiente / salario_ajenas)

  recortar_y_partir <- function(x) {
    limites <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
    x <- pmin(pmax(x, limites[1]), limites[2])
    cut(x, breaks = quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE),
        labels = c("Q1", "Q2", "Q3", "Q4", "Q5"), include.lowest = TRUE)
  }

  medidas %>%
    mutate(qa = recortar_y_partir(kaitz_a),
           qb = recortar_y_partir(kaitz_b),
           qc = recortar_y_partir(kaitz_c)) %>%
    select(NORDEMP, qa, qb, qc, firmas_en_celda)
}

# salto_con_medida() -- líneas 1090-1122 de 06, LITERAL (usa el global EFECTOS_FIJOS)
salto_con_medida <- function(base, columna_quintil, variable, anio_choque, anio_base_ref) {
  base <- base %>%
    filter(!is.na(.data[[columna_quintil]])) %>%
    mutate(g2 = as.integer(.data[[columna_quintil]] == "Q2"),
           g3 = as.integer(.data[[columna_quintil]] == "Q3"),
           g4 = as.integer(.data[[columna_quintil]] == "Q4"),
           g5 = as.integer(.data[[columna_quintil]] == "Q5"))

  modelo <- feols(as.formula(paste0(
    variable, " ~ i(ANIO_F, g2, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g3, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g4, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g5, ref = '", anio_base_ref, "') | ", EFECTOS_FIJOS)),
    data = base, cluster = ~NORDEMP)

  coeficientes <- as.data.frame(coeftable(modelo))
  etiquetas <- c(g2 = "Q2", g3 = "Q3", g4 = "Q4", g5 = "Q5 (más expuestas)")

  tibble(
    nombre         = rownames(coeficientes),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]]
  ) %>%
    mutate(anio    = as.integer(sub("ANIO_F::([0-9]{4}):.*", "\\1", nombre)),
           clave   = sub(".*:(g[2-5])$", "\\1", nombre),
           quintil = etiquetas[clave]) %>%
    filter(anio == anio_choque) %>%
    transmute(quintil, coeficiente, error_estandar, p_valor,
              significancia = estrellas(p_valor),
              efecto_porcentual = 100 * coeficiente,
              firmas = n_distinct(base$NORDEMP))
}

# salto_con_medida_tamano() -- líneas 1283-1318 de 06, LITERAL (usa el global
# EFECTOS_FIJOS_TAMANO, que se reasigna antes de cada llamada)
salto_con_medida_tamano <- function(base, columna_quintil, variable, anio_choque, anio_base_ref) {
  base <- base %>%
    filter(!is.na(.data[[columna_quintil]])) %>%
    mutate(g2 = as.integer(.data[[columna_quintil]] == "Q2"),
           g3 = as.integer(.data[[columna_quintil]] == "Q3"),
           g4 = as.integer(.data[[columna_quintil]] == "Q4"),
           g5 = as.integer(.data[[columna_quintil]] == "Q5"))

  modelo <- feols(as.formula(paste0(
    variable, " ~ i(ANIO_F, g2, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g3, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g4, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g5, ref = '", anio_base_ref, "') | ", EFECTOS_FIJOS_TAMANO)),
    data = base, cluster = ~NORDEMP)

  coeficientes <- as.data.frame(coeftable(modelo))
  etiquetas <- c(g2 = "Q2", g3 = "Q3", g4 = "Q4", g5 = "Q5 (más expuestas)")
  firmas_usadas <- n_distinct(base$NORDEMP[obs(modelo)])

  tibble(
    nombre         = rownames(coeficientes),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]]
  ) %>%
    mutate(anio    = as.integer(sub("ANIO_F::([0-9]{4}):.*", "\\1", nombre)),
           clave   = sub(".*:(g[2-5])$", "\\1", nombre),
           quintil = etiquetas[clave]) %>%
    filter(anio == anio_choque) %>%
    transmute(quintil, coeficiente, error_estandar, p_valor,
              significancia = estrellas(p_valor),
              efecto_porcentual = 100 * coeficiente,
              firmas = firmas_usadas)
}


# ==============================================================================
# 3. VALIDACIÓN: reproducir T24 (sin y con tamaño), medida A, Q5
# ==============================================================================
titulo("3. VALIDACIÓN CONTRA T24 (sin y con tamaño)")

medidas_2022 <- construir_medidas(2022, c(2019, 2021, 2022), 1160000)
base_medidas_2022 <- datos %>% inner_join(medidas_2022, by = "NORDEMP") %>%
  filter(ANIO %in% c(2015:2019, 2021:2024))   # misma ventana efectiva que el 'datos' de 06

medidas_2018 <- construir_medidas(2018, c(2015, 2016, 2017, 2018), 828116)
base_medidas_2018 <- datos %>% inner_join(medidas_2018, by = "NORDEMP") %>%
  filter(ANIO %in% 2015:2019)   # idéntico al filtro de 06

val_real_sin <- salto_con_medida(base_medidas_2022, "qa", "log_salario", 2023, "2022")
val_placebo_sin <- salto_con_medida(base_medidas_2018, "qa", "log_salario", 2019, "2018")
val_real_empleo_sin <- salto_con_medida(base_medidas_2022, "qa", "log_empleo", 2023, "2022")
val_placebo_empleo_sin <- salto_con_medida(base_medidas_2018, "qa", "log_empleo", 2019, "2018")

EFECTOS_FIJOS_TAMANO <- efectos_fijos_tamano_de("tamano_2022")
val_real_con <- salto_con_medida_tamano(base_medidas_2022, "qa", "log_salario", 2023, "2022")
val_placebo_con <- salto_con_medida_tamano(base_medidas_2018, "qa", "log_salario", 2019, "2018")
val_real_empleo_con <- salto_con_medida_tamano(base_medidas_2022, "qa", "log_empleo", 2023, "2022")
val_placebo_empleo_con <- salto_con_medida_tamano(base_medidas_2018, "qa", "log_empleo", 2019, "2018")

q5 <- function(tabla) tabla %>% filter(quintil == "Q5 (más expuestas)") %>% pull(efecto_porcentual)

cat("\n--- Sin control de tamaño (objetivo dado: costo 8,2053 real / 8,3641 placebo; empleo -0,7677 real / 0,2403 placebo) ---\n")
cat("  Real costo:", q5(val_real_sin), " | Placebo costo:", q5(val_placebo_sin), "\n")
cat("  Real empleo:", q5(val_real_empleo_sin), " | Placebo empleo:", q5(val_placebo_empleo_sin), "\n")

cat("\n--- Con tamaño 2022 (T24_tres_medidas_real_vs_placebo_tamano.csv, medida A, Q5) ---\n")
cat("  Real costo:", q5(val_real_con), " | Placebo costo:", q5(val_placebo_con), "\n")
cat("  Real empleo:", q5(val_real_empleo_con), " | Placebo empleo:", q5(val_placebo_empleo_con), "\n")

t24_sin <- read_csv(file.path("4. RESULTADOS", "06_tratamiento_continuo",
                              "T24_tres_medidas_real_vs_placebo.csv"), show_col_types = FALSE)
t24_con <- read_csv(file.path("4. RESULTADOS", "06_tratamiento_continuo",
                              "T24_tres_medidas_real_vs_placebo_tamano.csv"), show_col_types = FALSE)

obtener <- function(tabla, resultado_txt, ejercicio_txt) {
  f <- tabla %>% filter(resultado == resultado_txt,
                        medida == "A. Salario de la firma en el año base",
                        ejercicio == ejercicio_txt, quintil == "Q5 (más expuestas)")
  f$efecto_porcentual[1]
}

TOL <- 1e-8
diffs <- c(
  costo_real_sin    = abs(q5(val_real_sin)         - obtener(t24_sin, "Costo laboral por trabajador (log)", "Real (choque de 2023)")),
  costo_placebo_sin = abs(q5(val_placebo_sin)       - obtener(t24_sin, "Costo laboral por trabajador (log)", "Placebo (año sin choque: 2019)")),
  empleo_real_sin   = abs(q5(val_real_empleo_sin)   - obtener(t24_sin, "Empleo total (log)", "Real (choque de 2023)")),
  empleo_placebo_sin= abs(q5(val_placebo_empleo_sin)- obtener(t24_sin, "Empleo total (log)", "Placebo (año sin choque: 2019)")),
  costo_real_con    = abs(q5(val_real_con)          - obtener(t24_con, "Costo laboral por trabajador (log)", "Real (choque de 2023)")),
  costo_placebo_con = abs(q5(val_placebo_con)       - obtener(t24_con, "Costo laboral por trabajador (log)", "Placebo (año sin choque: 2019)")),
  empleo_real_con   = abs(q5(val_real_empleo_con)   - obtener(t24_con, "Empleo total (log)", "Real (choque de 2023)")),
  empleo_placebo_con= abs(q5(val_placebo_empleo_con)- obtener(t24_con, "Empleo total (log)", "Placebo (año sin choque: 2019)"))
)
cat("\nDiferencias absolutas contra los archivos originales (tolerancia", TOL, "):\n")
print(diffs)

cat("\nNOTA: los números que diste para la validación (8,2053 / 8,3641 / -0,7677 / 0,2403) son los del archivo SIN",
    "\ntamaño (T24_tres_medidas_real_vs_placebo.csv). El archivo CON tamaño (T24_tres_medidas_real_vs_placebo_tamano.csv)",
    "\ntiene valores distintos en el original (costo real 10,0854 / placebo 9,4053; empleo real -3,7153 / placebo 3,4618)",
    "\n-- la comparación 'con tamaño' de arriba se hizo contra ESOS valores, leídos del archivo, no contra los que diste.\n")

if (any(diffs > TOL)) {
  stop("VALIDACIÓN FALLÓ: al menos una diferencia supera la tolerancia ", TOL,
       ". Ver detalle arriba. Deteniendo -- no se continúa con los placebos nuevos.")
}
cat("\nVALIDACIÓN OK: diferencia máxima", max(diffs), "(tolerancia", TOL, ").\n")


# ==============================================================================
# 4-6. PLACEBOS 2015 Y 2016, Y COMPARACIÓN CON 2019 Y 2023
# ==============================================================================
titulo("4-6. PLACEBOS 2015 Y 2016 (y comparación con 2019 y 2023)")

ejercicios <- list(
  real_2023 = list(anio_base = 2022, anios_promedio = c(2019, 2021, 2022),
                   sm_siguiente = 1160000, anio_choque = 2023, anio_base_ref = "2022",
                   ventana = c(2015:2019, 2021:2024), tamano_propio = NULL,
                   etiqueta = "Real (choque de 2023)"),
  placebo_2019 = list(anio_base = 2018, anios_promedio = c(2015, 2016, 2017, 2018),
                      sm_siguiente = 828116, anio_choque = 2019, anio_base_ref = "2018",
                      ventana = 2015:2019, tamano_propio = "tamano_2018",
                      etiqueta = "Placebo (2018 -> 2019)"),
  placebo_2016 = list(anio_base = 2015, anios_promedio = c(2011, 2012, 2013, 2014),
                      sm_siguiente = 689455, anio_choque = 2016, anio_base_ref = "2015",
                      ventana = 2011:2016, tamano_propio = "tamano_2015",
                      etiqueta = "Placebo (2015 -> 2016)"),
  placebo_2015 = list(anio_base = 2014, anios_promedio = c(2010, 2011, 2012, 2013),
                      sm_siguiente = 644350, anio_choque = 2015, anio_base_ref = "2014",
                      ventana = 2010:2015, tamano_propio = "tamano_2014",
                      etiqueta = "Placebo (2014 -> 2015)")
)

estimar_ejercicio <- function(ej) {
  medidas <- construir_medidas(ej$anio_base, ej$anios_promedio, ej$sm_siguiente)
  base_medidas <- datos %>% inner_join(medidas, by = "NORDEMP") %>% filter(ANIO %in% ej$ventana)

  hacer <- function(etiqueta_control, var_tamano) {
    if (is.null(var_tamano)) {
      fn <- function(col, var) salto_con_medida(base_medidas, col, var, ej$anio_choque, ej$anio_base_ref)
    } else {
      EFECTOS_FIJOS_TAMANO <<- efectos_fijos_tamano_de(var_tamano)
      fn <- function(col, var) salto_con_medida_tamano(base_medidas, col, var, ej$anio_choque, ej$anio_base_ref)
    }
    bind_rows(
      fn("qa", "log_salario") %>% mutate(medida = "A. Salario de la firma en el año base", resultado = "Costo laboral por trabajador (log)"),
      fn("qb", "log_salario") %>% mutate(medida = "B. Salario promediado de los años previos", resultado = "Costo laboral por trabajador (log)"),
      fn("qc", "log_salario") %>% mutate(medida = "C. Salario de firmas parecidas", resultado = "Costo laboral por trabajador (log)"),
      fn("qa", "log_empleo") %>% mutate(medida = "A. Salario de la firma en el año base", resultado = "Empleo total (log)"),
      fn("qb", "log_empleo") %>% mutate(medida = "B. Salario promediado de los años previos", resultado = "Empleo total (log)"),
      fn("qc", "log_empleo") %>% mutate(medida = "C. Salario de firmas parecidas", resultado = "Empleo total (log)")
    ) %>% mutate(control_tamano = etiqueta_control)
  }

  resultado <- bind_rows(
    hacer("Sin tamaño", NULL),
    hacer("Con tamaño 2022", "tamano_2022")
  )
  if (!is.null(ej$tamano_propio)) {
    resultado <- bind_rows(resultado,
                           hacer(paste0("Con tamaño ", ej$anio_base, " (año base, contraste)"), ej$tamano_propio))
  }
  resultado %>% mutate(ejercicio = ej$etiqueta)
}

resultados_todos <- bind_rows(lapply(ejercicios, estimar_ejercicio)) %>%
  mutate(error_estandar_pct = 100 * error_estandar)

ver(resultados_todos %>% filter(quintil == "Q5 (más expuestas)") %>%
      select(ejercicio, medida, control_tamano, resultado, efecto_porcentual), filas = 30)

# --- Diferencia frente a 2023 (mismo cálculo que T25/C9.3): "Con tamaño <año
#     base> (contraste)" se compara contra el real de 2023 bajo tamaño 2022
#     (2023 no tiene una versión "tamaño en su propio año base" distinta de
#     tamaño 2022, porque su año base YA es 2022). Supone independencia entre
#     el ejercicio real y cada placebo, aunque comparten firmas. ------------
real_lookup <- resultados_todos %>%
  filter(ejercicio == "Real (choque de 2023)") %>%
  transmute(medida, resultado, quintil, control_tamano,
           efecto_real = efecto_porcentual, ee_real_pct = error_estandar_pct)

resultados_todos <- resultados_todos %>%
  mutate(control_tamano_match = ifelse(grepl("año base, contraste", control_tamano),
                                       "Con tamaño 2022", control_tamano)) %>%
  left_join(real_lookup, by = c("medida", "resultado", "quintil", "control_tamano_match" = "control_tamano")) %>%
  mutate(
    diferencia_frente_2023 = ifelse(ejercicio == "Real (choque de 2023)", NA_real_,
                                    efecto_porcentual - efecto_real),
    ee_diferencia = ifelse(ejercicio == "Real (choque de 2023)", NA_real_,
                          sqrt(error_estandar_pct^2 + ee_real_pct^2)),
    p_diferencia = ifelse(ejercicio == "Real (choque de 2023)", NA_real_,
                         2 * pnorm(-abs(diferencia_frente_2023 / ee_diferencia)))
  )

cat("\nNOTA: p_diferencia supone independencia entre el ejercicio real (2023) y",
    "\ncada placebo, aunque son construidos con las mismas firmas -- es, como en",
    "\nT25_resumen_medidas_q5.csv, un error conservador (más grande que el real).\n")


# ==============================================================================
# 7. SALIDA: T_placebos_2015_2016.csv
# ==============================================================================
titulo("7. GUARDANDO T_placebos_2015_2016.csv")

t_placebos <- resultados_todos %>%
  transmute(ejercicio, medida, control_tamano, resultado, quintil,
           efecto_pct = efecto_porcentual, error_estandar = error_estandar_pct,
           p_valor, significancia, firmas, diferencia_frente_2023, p_diferencia) %>%
  arrange(resultado, medida, control_tamano, quintil, ejercicio)

write_csv(t_placebos, file.path(CARPETA, "T_placebos_2015_2016.csv"))
cat("Guardado:", file.path(CARPETA, "T_placebos_2015_2016.csv"), "(", nrow(t_placebos), "filas )\n")

cat("\nLIMITACIÓN: la muestra de este script (como la del placebo de 2019 en 06)",
    "\nse restringe a firmas con Bite2022_obreros válido -- es decir, firmas que",
    "\nsobreviven hasta 2022 y tienen clasificación de exposición en ese año.",
    "\nLos placebos de 2015 y 2016 heredan ese mismo sesgo de supervivencia: no",
    "\nson una muestra representativa de 2015/2016 por sí sola, sino las mismas",
    "\nfirmas del estudio principal, miradas hacia atrás.\n")


# ==============================================================================
# 8. GRÁFICOS: barras por quintil, costo y empleo (2023 azul; 2015/2016/2019 gris)
# ==============================================================================
titulo("8. GRÁFICOS")

datos_grafico <- resultados_todos %>%
  filter(control_tamano == "Sin tamaño") %>%
  mutate(
    efecto_pct = efecto_porcentual,
    quintil = factor(quintil, levels = c("Q2", "Q3", "Q4", "Q5 (más expuestas)")),
    ejercicio = factor(ejercicio, levels = c("Real (choque de 2023)", "Placebo (2018 -> 2019)",
                                             "Placebo (2015 -> 2016)", "Placebo (2014 -> 2015)"),
                       labels = c("2023 (real)", "2019 (placebo)", "2016 (placebo)", "2015 (placebo)")),
    medida = factor(medida, levels = c("A. Salario de la firma en el año base",
                                       "B. Salario promediado de los años previos",
                                       "C. Salario de firmas parecidas"))
  )

colores_ejercicio <- c("2023 (real)" = COLOR_ALTA, "2019 (placebo)" = COLOR_GRIS1,
                       "2016 (placebo)" = COLOR_GRIS2, "2015 (placebo)" = COLOR_GRIS3)

graficar_barras <- function(resultado_txt, nombre_archivo) {
  g <- ggplot(filter(datos_grafico, resultado == resultado_txt, medida == "A. Salario de la firma en el año base"),
             aes(x = quintil, y = efecto_pct, fill = ejercicio)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_errorbar(aes(ymin = efecto_pct - 1.96 * error_estandar_pct,
                      ymax = efecto_pct + 1.96 * error_estandar_pct),
                 position = position_dodge(width = 0.75), width = 0.2, color = "grey20") +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
    scale_fill_manual(values = colores_ejercicio) +
    labs(x = NULL, y = "Efecto (%)", fill = NULL) +
    tema_tesis
  ggsave(file.path(CARPETA, "figuras", nombre_archivo), g,
         width = 18, height = 11, units = "cm", dpi = 300, bg = "white")
  cat("Gráfico guardado:", nombre_archivo, "\n")
}

graficar_barras("Costo laboral por trabajador (log)", "G_placebos_costo.png")
graficar_barras("Empleo total (log)", "G_placebos_empleo.png")

titulo("FIN DEL SCRIPT")
