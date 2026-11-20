# ==============================================================================
# robustez_tamano_ano_base.R
#
# Objetivo: repetir las lecturas del efecto de 2023 (costo laboral, empleo
# total y empleo permanente) con efectos fijos de tamaño x año, fijando la
# clasificacion de tamaño ANTES del año de exposicion (2022), con dos
# clasificaciones alternativas:
#   - tamano_2019: PRINCIPAL alternativa (ultimo año antes de la pandemia).
#   - tamano_2021: SOLO contraste (año afectado por la pandemia y el PAEF).
#
# Script independiente, de solo lectura sobre "1. DATOS/" (no escribe ahi).
# Copia SOLO las secciones necesarias de 05_resultados_y_mecanismos.R (carga
# de datos, construccion de base/datos, EFECTOS/EFECTOS_TAMANO, y las
# funciones estudio_evento/contraste/tres_lecturas/lecturas_tamano/
# evento_quintiles/evento_quintiles_tamano con sus auxiliares) -- NO corre
# 05 completo, NO toca HonestDiD ni mecanismos, NO modifica 05.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/panel_analitico_firma_eam.rds
#           4. RESULTADOS/05_resultados_y_mecanismos/T07c_lecturas_tamano.csv (validacion)
#           4. RESULTADOS/05_resultados_y_mecanismos/T08_resumen_principal.csv (lectura, sin reestimar)
# Salidas:  4. RESULTADOS/08_respuesta_jurados/T_tamano_ano_base.csv
#           4. RESULTADOS/08_respuesta_jurados/T_tabla_especificaciones.csv
#           4. RESULTADOS/08_respuesta_jurados/figuras/G_evento_empleo_tamano_ano_base.png
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

COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"
COLOR_CONTRASTE <- "grey70"

tema_tesis <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())


# ==============================================================================
# 1. DATOS (copiado de 05_resultados_y_mecanismos.R, lineas 187-416, recortado)
# ==============================================================================
titulo("1. DATOS Y CONSTRUCCIÓN DE VARIABLES (copiado de 05, recortado)")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

# 'alternativas' (golpe_c/golpe_a/golpe_costo) no se copia: este script solo
# usa Bite2022_obreros (igual que T07c/T08), asi que esas medidas no hacen
# falta. 'viejas' SI se copia -- es la fuente de Bite2022_obreros.
viejas <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO))) %>%
  filter(ANIO == 2022) %>%
  select(NORDEMP, any_of(c("Bite2022_obreros", "Exposure2022_obreros")))

base <- panel %>%
  left_join(viejas, by = "NORDEMP")

# --- Outcomes (copiado literal de 05, lineas 208-295) ---------------------------
base <- base %>%
  mutate(
    empleo_total = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo_total > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo_total, NA_real_),
    empleo_permanente = obreros_permanentes + administrativos_permanentes +
      profesional_tecnico_permanentes,
    empleo_temporal_directo = obreros_temporal_directo + administrativos_temporal_directo +
      profesional_tecnico_temporal_directo,
    empleo_temporal_agencias = obreros_temporal_agencias + administrativos_temporal_agencias +
      profesional_tecnico_temporal_agencias,
    empleo_aprendices = obreros_aprendices + administrativos_aprendices +
      profesional_tecnico_aprendices,
    empleo_temporal = empleo_temporal_directo + empleo_temporal_agencias,
    ANIO_F = factor(ANIO),
    post = as.integer(ANIO >= 2023)
  )

# --- Clasificaciones fijadas ANTES del choque: 2022 (igual que 05), y las dos
#     alternativas nuevas, 2019 y 2021. Mismo metodo: join desde 'panel'
#     (panel_firma_eam_expalt_completo.rds), filtrado al año de clasificacion.
#     Sector y departamento SIEMPRE fijados en 2022 (igual que 05 y 06). ------
clasificacion_2022 <- panel %>%
  filter(ANIO == 2022) %>%
  transmute(
    NORDEMP,
    sector_2022 = factor(CIIU4),
    depto_2022  = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

clasificacion_2019_tamano <- panel %>%
  filter(ANIO == 2019) %>%
  transmute(NORDEMP, tamano_2019 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

clasificacion_2021_tamano <- panel %>%
  filter(ANIO == 2021) %>%
  transmute(NORDEMP, tamano_2021 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

n_antes_join <- n_distinct(base$NORDEMP)
base <- base %>%
  select(-any_of(c("sector_2022", "depto_2022", "tamano_2022"))) %>%
  left_join(clasificacion_2022, by = "NORDEMP") %>%
  left_join(clasificacion_2019_tamano, by = "NORDEMP") %>%
  left_join(clasificacion_2021_tamano, by = "NORDEMP")

cat("\nFirmas en el panel:", n_antes_join, "\n")
cat("Firmas con clasificación de tamaño 2022:", sum(!is.na(base$tamano_2022[!duplicated(base$NORDEMP)])), "\n")
cat("Firmas con clasificación de tamaño 2019:", sum(!is.na(base$tamano_2019[!duplicated(base$NORDEMP)])), "\n")
cat("Firmas con clasificación de tamaño 2021:", sum(!is.na(base$tamano_2021[!duplicated(base$NORDEMP)])), "\n")

# --- Logaritmos (copiado de 05, solo los 3 outcomes que usa este script) --------
log_seguro <- function(x) log(ifelse(!is.na(x) & x > 0, x, NA_real_))
base <- base %>%
  mutate(
    log_costo = log_seguro(costo_trabajador),
    log_empleo = log_seguro(empleo_total),
    log_permanente = log_seguro(empleo_permanente)
  )

# --- Winsorizacion y estandarizacion de Bite2022_obreros (copiado de 05) -------
winsorizar <- function(x) {
  lim <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, lim[1]), lim[2])
}
MEDIDAS <- c("Bite2022_obreros")
for (m in MEDIDAS) {
  if (m %in% names(base)) {
    base[[paste0(m, "_de")]] <- {
      w <- winsorizar(base[[m]])
      w / sd(w, na.rm = TRUE)
    }
  }
}

# Ventana de estimación: 2015-2024 sin 2020 (pandemia) -- igual que 05
datos <- base %>% filter(ANIO %in% c(2015:2019, 2021:2024))
datos_completo <- datos   # copia de respaldo: algunas secciones reasignan 'datos' temporalmente

cat("\nFirmas-año en la ventana de estimación:", nrow(datos), "\n")
cat("Firmas distintas:", n_distinct(datos$NORDEMP), "\n")


# ==============================================================================
# 2. ESPECIFICACIÓN (copiado de 05, lineas 424 y 1019, EFECTOS_TAMANO generalizado)
# ==============================================================================
EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F"

# EFECTOS_TAMANO en 05 es fijo ("... + tamano_2022^ANIO_F"); aqui se
# generaliza a una funcion para poder usar tamano_2019/tamano_2021 con las
# MISMAS funciones lecturas_tamano()/evento_quintiles_tamano() de 05 (que
# usan el global EFECTOS_TAMANO), reasignando el global antes de cada
# llamada. La formula en si ("EFECTOS + <tamano_X>^ANIO_F") es identica a la
# de 05 cuando <tamano_X> = tamano_2022.
efectos_tamano_de <- function(var_tamano) paste(EFECTOS, "+", var_tamano, "^ANIO_F")
EFECTOS_TAMANO <- efectos_tamano_de("tamano_2022")   # valor inicial, igual al de 05


# ==============================================================================
# 3. FUNCIONES (copiadas literalmente de 05_resultados_y_mecanismos.R)
# ==============================================================================

# estudio_evento() -- lineas 431-457 de 05
estudio_evento <- function(outcome, tratamiento, base_datos = datos,
                           previos = "2015|2016|2017|2018|2019|2021",
                           efectos = EFECTOS) {
  v <- paste0(tratamiento, "_de")
  if (!v %in% names(base_datos) || !outcome %in% names(base_datos)) return(NULL)

  formula <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", v, ", ref = '2022') | ", efectos))
  modelo <- tryCatch(feols(formula, data = base_datos, cluster = ~NORDEMP),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)

  coefs <- as.data.frame(coeftable(modelo))
  tabla <- tibble(
    anio = as.integer(gsub(paste0("ANIO_F::|:", v), "", rownames(coefs))),
    coeficiente = coefs[["Estimate"]],
    error_estandar = coefs[["Std. Error"]],
    p_valor = coefs[["Pr(>|t|)"]]
  ) %>%
    bind_rows(tibble(anio = 2022L, coeficiente = 0, error_estandar = 0, p_valor = NA_real_)) %>%
    arrange(anio)

  p_previos <- tryCatch(
    wald(modelo, keep = paste0("ANIO_F::(", previos, "):"), print = FALSE)$p,
    error = function(e) NA_real_)

  list(tabla = tabla, p_previos = p_previos, modelo = modelo, variable = v)
}

# contraste() -- lineas 461-478 de 05
contraste <- function(evento, pesos, etiqueta) {
  if (is.null(evento)) return(NULL)
  nombres <- paste0("ANIO_F::", names(pesos), ":", evento$variable)
  disponibles <- nombres %in% names(coef(evento$modelo))
  if (!all(disponibles)) return(NULL)

  b <- coef(evento$modelo)[nombres]
  V <- vcov(evento$modelo)[nombres, nombres]
  estimado <- sum(pesos * b)
  error <- sqrt(as.numeric(t(pesos) %*% V %*% pesos))
  p <- 2 * pnorm(-abs(estimado / error))

  tibble(lectura = etiqueta, coeficiente = estimado, error_estandar = error,
         p_valor = p, significancia = estrellas(p),
         efecto_pct = 100 * estimado,
         ic95_inf_pct = 100 * (estimado - 1.96 * error),
         ic95_sup_pct = 100 * (estimado + 1.96 * error))
}

# tres_lecturas() -- lineas 482-520 de 05 (no se usa para estimar aqui: las
# cifras "sin tamaño" se LEEN de T08/T01 ya calculadas, sin reestimar; se
# copia solo porque se pidió explícitamente y por si se necesita de respaldo)
tres_lecturas <- function(outcome, tratamiento, base_datos = datos, efectos = EFECTOS) {
  evento <- estudio_evento(outcome, tratamiento, base_datos, efectos = efectos)
  if (is.null(evento)) return(NULL)
  a <- contraste(evento, c(`2023` = 1), "A. Cambio 2022 -> 2023")
  b <- contraste(evento, c(`2023` = 1, `2016` = 1/3, `2019` = -1/3),
                 "B. Salto 2023 ajustado por tendencia previa")
  v <- evento$variable
  modelo_c <- tryCatch(
    feols(as.formula(paste0(outcome, " ~ post:", v, " | ", efectos)),
          data = base_datos, cluster = ~NORDEMP),
    error = function(e) NULL)
  c_fila <- if (!is.null(modelo_c)) {
    f <- coeftable(modelo_c)[paste0("post:", v), ]
    tibble(lectura = "C. DiD simple (promedio post - pre)",
           coeficiente = f[["Estimate"]], error_estandar = f[["Std. Error"]],
           p_valor = f[["Pr(>|t|)"]], significancia = estrellas(f[["Pr(>|t|)"]]),
           efecto_pct = 100 * f[["Estimate"]],
           ic95_inf_pct = 100 * (f[["Estimate"]] - 1.96 * f[["Std. Error"]]),
           ic95_sup_pct = 100 * (f[["Estimate"]] + 1.96 * f[["Std. Error"]]))
  } else NULL
  d <- contraste(evento, c(`2023` = 1, `2021` = -1), "D. 2023 frente a 2021")
  bind_rows(a, b, c_fila, d) %>%
    mutate(outcome = outcome, p_previos = evento$p_previos,
           observaciones = nobs(evento$modelo))
}

# lecturas_tamano() -- lineas 1375-1411 de 05, LITERAL (usa los globales
# EFECTOS_TAMANO y 'datos'; se reasignan antes de cada llamada, ver abajo)
lecturas_tamano <- function(outcome) {
  evento <- estudio_evento(outcome, "Bite2022_obreros", efectos = EFECTOS_TAMANO)
  if (is.null(evento)) return(NULL)
  v <- evento$variable

  a <- contraste(evento, c(`2023` = 1), "A. Cambio 2022 -> 2023")
  b <- contraste(evento, c(`2023` = 1, `2016` = 1/3, `2019` = -1/3),
                 "B. Salto 2023 ajustado por tendencia previa")

  modelo_c <- tryCatch(
    feols(as.formula(paste0(outcome, " ~ post:", v, " | ", EFECTOS_TAMANO)),
          data = datos, cluster = ~NORDEMP),
    error = function(e) NULL)
  c_fila <- if (!is.null(modelo_c)) {
    f <- coeftable(modelo_c)[paste0("post:", v), ]
    tibble(lectura = "C. DiD simple (promedio post - pre)",
           coeficiente = f[["Estimate"]], error_estandar = f[["Std. Error"]],
           p_valor = f[["Pr(>|t|)"]], significancia = estrellas(f[["Pr(>|t|)"]]),
           efecto_pct = 100 * f[["Estimate"]],
           ic95_inf_pct = 100 * (f[["Estimate"]] - 1.96 * f[["Std. Error"]]),
           ic95_sup_pct = 100 * (f[["Estimate"]] + 1.96 * f[["Std. Error"]]))
  } else NULL

  d <- contraste(evento, c(`2023` = 1, `2021` = -1), "D. 2023 frente a 2021")

  pp <- -1/7
  pesos_previo <- c(`2015` = pp, `2016` = pp, `2017` = pp, `2018` = pp,
                    `2019` = pp, `2021` = pp)
  e2023 <- contraste(evento, c(`2023` = 1, pesos_previo), "2023 vs promedio previo")
  e2024 <- contraste(evento, c(`2024` = 1, pesos_previo), "2024 vs promedio previo")

  bind_rows(a, b, c_fila, d, e2023, e2024) %>%
    mutate(outcome = outcome, p_previos = evento$p_previos,
           observaciones = nobs(evento$modelo))
}

# evento_quintiles_tamano() -- lineas 1430-1469 de 05, LITERAL (usa los
# globales EFECTOS_TAMANO, 'datos_q' y RESULTADOS_QUINTILES)
evento_quintiles_tamano <- function(outcome) {
  m <- tryCatch(
    feols(as.formula(paste0(outcome, " ~ i(ANIO_F, quintil_kaitz, ref = '2022', ref2 = '1') | ",
                            EFECTOS_TAMANO)), data = datos_q, cluster = ~NORDEMP),
    error = function(e) NULL)
  if (is.null(m)) return(NULL)
  b <- coef(m); V <- vcov(m); nm <- names(b)
  anio <- as.integer(sub("^ANIO_F::(\\d+):.*$", "\\1", nm))
  q <- as.integer(sub("^.*::(\\d+)$", "\\1", nm))

  p_previos <- tryCatch(
    wald(m, keep = "^ANIO_F::(2015|2016|2017|2018|2019|2021):", print = FALSE)$p,
    error = function(e) NA_real_)

  combinar <- function(pesos, etiqueta, qq) {
    w <- setNames(rep(0, length(b)), nm)
    for (a in names(pesos)) w[anio == as.integer(a) & q == qq] <- pesos[[a]]
    est <- sum(w * b); ee <- sqrt(as.numeric(t(w) %*% V %*% w))
    tibble(quintil = qq, lectura = etiqueta, efecto_pct = 100 * est,
           error_estandar_pct = 100 * ee,
           ic95_inf_pct = 100 * (est - 1.96 * ee), ic95_sup_pct = 100 * (est + 1.96 * ee),
           p_valor = 2 * pnorm(-abs(est / ee)))
  }
  pp <- -1/7
  efectos <- bind_rows(lapply(2:5, function(qq) bind_rows(
    combinar(c(`2023` = 1), "2023 vs 2022", qq),
    combinar(c(`2023` = 1, `2015` = pp, `2016` = pp, `2017` = pp, `2018` = pp,
               `2019` = pp, `2021` = pp), "2023 vs promedio previo", qq),
    combinar(c(`2023` = 1, `2021` = -1), "2023 vs 2021", qq),
    combinar(c(`2024` = 1), "2024 vs 2022", qq)
  ))) %>%
    mutate(resultado = RESULTADOS_QUINTILES[[outcome]], significancia = estrellas(p_valor),
           p_previos = p_previos, observaciones = nobs(m))

  trayectoria <- tibble(anio = anio, quintil = q, coeficiente = unname(b),
                        error_estandar = sqrt(diag(V))) %>%
    bind_rows(tibble(anio = 2022L, quintil = 2:5, coeficiente = 0, error_estandar = 0)) %>%
    mutate(resultado = RESULTADOS_QUINTILES[[outcome]])

  list(trayectoria = trayectoria, efectos = efectos)
}


# ==============================================================================
# 4. QUINTILES DE KAITZ (copiado de 05, lineas 1100-1136)
# ==============================================================================
titulo("4. QUINTILES DE KAITZ (copiado de 05)")

quintiles_kaitz <- base %>%
  filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
  distinct(NORDEMP, .keep_all = TRUE) %>%
  transmute(NORDEMP, Bite2022_obreros)

limites_quintiles <- quantile(quintiles_kaitz$Bite2022_obreros, probs = c(0.01, 0.99))

quintiles_kaitz <- quintiles_kaitz %>%
  mutate(
    kaitz_winsorizado = pmin(pmax(Bite2022_obreros, limites_quintiles[1]), limites_quintiles[2]),
    quintil_kaitz = cut(kaitz_winsorizado,
                        breaks = quantile(kaitz_winsorizado, probs = seq(0, 1, 0.2)),
                        labels = as.character(1:5), include.lowest = TRUE)
  )

conteo_quintiles <- as.integer(table(quintiles_kaitz$quintil_kaitz))
esperado_quintiles <- c(1020L, 1022L, 1017L, 1020L, 1020L)
cat("Firmas por quintil (Kaitz 2022, winsorizado):", paste(conteo_quintiles, collapse = " / "),
    "| esperado:", paste(esperado_quintiles, collapse = " / "), "\n")
if (!identical(conteo_quintiles, esperado_quintiles)) {
  stop("Los conteos por quintil NO coinciden con T16_distribucion_exposicion.csv (06). ",
       "Deteniendo -- revisar antes de seguir.")
}

datos_q <- datos %>%
  inner_join(select(quintiles_kaitz, NORDEMP, quintil_kaitz), by = "NORDEMP")
datos_q_completo <- datos_q

RESULTADOS_QUINTILES <- c(log_costo = "Costo laboral por trabajador (log)",
                          log_empleo = "Empleo total (log)",
                          log_permanente = "Empleo permanente (log)")


# ==============================================================================
# 5. VALIDACIÓN: reproducir T07c_lecturas_tamano.csv con tamano_2022
# ==============================================================================
titulo("5. VALIDACIÓN CONTRA T07c_lecturas_tamano.csv")

EFECTOS_TAMANO <- efectos_tamano_de("tamano_2022")
cat("EFECTOS_TAMANO (tamano_2022):", EFECTOS_TAMANO, "\n")

lecturas_tamano_2022 <- bind_rows(
  lapply(c("log_costo", "log_empleo", "log_permanente"), lecturas_tamano)) %>%
  select(outcome, lectura, efecto_pct, ic95_inf_pct, ic95_sup_pct,
         p_valor, significancia, p_previos, observaciones)

ver(lecturas_tamano_2022, filas = 20)

t07c_original <- read_csv(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                                    "T07c_lecturas_tamano.csv"), show_col_types = FALSE)

comparacion_validacion <- lecturas_tamano_2022 %>%
  rename_with(~ paste0(., "_nuevo"), c(efecto_pct, ic95_inf_pct, ic95_sup_pct, p_valor, p_previos)) %>%
  inner_join(
    t07c_original %>%
      rename_with(~ paste0(., "_original"), c(efecto_pct, ic95_inf_pct, ic95_sup_pct, p_valor, p_previos)),
    by = c("outcome", "lectura")
  ) %>%
  mutate(
    dif_efecto = abs(efecto_pct_nuevo - efecto_pct_original),
    dif_p_previos = abs(p_previos_nuevo - p_previos_original)
  )

ver(comparacion_validacion %>% select(outcome, lectura, efecto_pct_nuevo, efecto_pct_original, dif_efecto),
    filas = 20)

TOL <- 1e-8
if (nrow(comparacion_validacion) != nrow(t07c_original)) {
  stop("VALIDACIÓN FALLÓ: el número de filas reproducidas (", nrow(comparacion_validacion),
       ") no coincide con T07c_lecturas_tamano.csv (", nrow(t07c_original), ").")
}
if (any(comparacion_validacion$dif_efecto > TOL, na.rm = TRUE) ||
    any(comparacion_validacion$dif_p_previos > TOL, na.rm = TRUE)) {
  peor_efecto <- max(comparacion_validacion$dif_efecto, na.rm = TRUE)
  peor_p <- max(comparacion_validacion$dif_p_previos, na.rm = TRUE)
  stop("VALIDACIÓN FALLÓ: diferencia máxima en efecto_pct = ", peor_efecto,
       "; diferencia máxima en p_previos = ", peor_p,
       " (tolerancia ", TOL, "). Deteniendo -- no se continúa con el resto del script.")
}
cat("\nVALIDACIÓN OK: lecturas_tamano(tamano_2022) reproduce T07c_lecturas_tamano.csv",
    "con diferencia máxima", max(comparacion_validacion$dif_efecto, na.rm = TRUE),
    "en efecto_pct y", max(comparacion_validacion$dif_p_previos, na.rm = TRUE),
    "en p_previos (tolerancia", TOL, ").\n")


# ==============================================================================
# 6. LECTURAS CON CADA CLASIFICACIÓN DE TAMAÑO (muestra completa)
# ==============================================================================
titulo("6. LECTURAS CON TAMAÑO 2022, 2019 Y 2021 (muestra completa)")

estimar_clasificacion <- function(etiqueta_clasificacion, var_tamano, muestra_datos, etiqueta_muestra) {
  EFECTOS_TAMANO <<- efectos_tamano_de(var_tamano)
  datos <<- muestra_datos
  resultado <- bind_rows(lapply(c("log_costo", "log_empleo", "log_permanente"), lecturas_tamano)) %>%
    mutate(clasificacion = etiqueta_clasificacion, muestra = etiqueta_muestra,
           firmas = n_distinct(muestra_datos$NORDEMP[!is.na(muestra_datos[[var_tamano]])]),
           error_estandar_pct = 100 * error_estandar) %>%
    select(clasificacion, muestra, outcome, lectura, efecto_pct, error_estandar_pct,
           p_valor, significancia, p_previos, firmas, observaciones)
  datos <<- datos_completo   # restaurar el global
  resultado
}

resultados_clasificacion <- bind_rows(
  estimar_clasificacion("2022", "tamano_2022", datos_completo, "completa"),
  estimar_clasificacion("2019", "tamano_2019", datos_completo, "completa"),
  estimar_clasificacion("2021 (contraste: año afectado por la pandemia y el PAEF)",
                        "tamano_2021", datos_completo, "completa")
)

ver(resultados_clasificacion, filas = 20)


# ==============================================================================
# 7. MUESTRA COMÚN
# ==============================================================================
titulo("7. MUESTRA COMÚN (tamano_2022 restringido a firmas con tamano_2019 / tamano_2021)")

reporte_firmas_perdidas <- function(var_tamano) {
  base_2022 <- datos_completo %>% filter(ANIO == 2022, !is.na(tamano_2022)) %>%
    distinct(NORDEMP, .keep_all = TRUE) %>%
    left_join(select(quintiles_kaitz, NORDEMP, quintil_kaitz), by = "NORDEMP")
  perdidas <- base_2022 %>% filter(is.na(.data[[var_tamano]]))
  cat("\nFirmas con tamano_2022 pero SIN", var_tamano, ":", nrow(perdidas),
      "de", nrow(base_2022), "\n")
  cat("Distribución por quintil de exposición de las firmas perdidas:\n")
  print(table(perdidas$quintil_kaitz, useNA = "ifany"))
  invisible(perdidas)
}

perdidas_2019 <- reporte_firmas_perdidas("tamano_2019")
perdidas_2021 <- reporte_firmas_perdidas("tamano_2021")

datos_comun_2019 <- datos_completo %>% filter(!is.na(tamano_2022), !is.na(tamano_2019))
datos_comun_2021 <- datos_completo %>% filter(!is.na(tamano_2022), !is.na(tamano_2021))

resultados_muestra_comun <- bind_rows(
  estimar_clasificacion("2022", "tamano_2022", datos_comun_2019, "común con 2019"),
  estimar_clasificacion("2022", "tamano_2022", datos_comun_2021, "común con 2021")
)

ver(resultados_muestra_comun, filas = 20)


# ==============================================================================
# 8. QUINTILES: Q5 FRENTE A Q1 EN 2023, CON CADA CLASIFICACIÓN
# ==============================================================================
titulo("8. Q5 FRENTE A Q1 EN 2023, CON CADA CLASIFICACIÓN DE TAMAÑO")

estimar_quintil_clasificacion <- function(etiqueta_clasificacion, var_tamano) {
  EFECTOS_TAMANO <<- efectos_tamano_de(var_tamano)
  datos_q <<- datos_q_completo %>% filter(!is.na(.data[[var_tamano]]))
  res <- bind_rows(lapply(c("log_costo", "log_empleo", "log_permanente"), function(oc) {
    r <- evento_quintiles_tamano(oc)
    if (is.null(r)) return(NULL)
    r$efectos %>% filter(quintil == 5, lectura == "2023 vs 2022") %>%
      mutate(outcome = oc, clasificacion = etiqueta_clasificacion)
  }))
  datos_q <<- datos_q_completo
  res
}

q5_vs_q1_2023 <- bind_rows(
  estimar_quintil_clasificacion("2022", "tamano_2022"),
  estimar_quintil_clasificacion("2019", "tamano_2019"),
  estimar_quintil_clasificacion("2021 (contraste: año afectado por la pandemia y el PAEF)", "tamano_2021")
) %>%
  transmute(clasificacion, muestra = "completa", outcome,
            lectura = "Q5 frente a Q1 en 2023 (evento_quintiles, con tamaño)",
            efecto_pct, error_estandar_pct, p_valor, significancia,
            p_previos, firmas = NA_integer_, observaciones)

ver(q5_vs_q1_2023, filas = 20)


# ==============================================================================
# 9. SALIDA: T_tamano_ano_base.csv
# ==============================================================================
titulo("9. GUARDANDO T_tamano_ano_base.csv")

t_tamano_ano_base <- bind_rows(resultados_clasificacion, resultados_muestra_comun, q5_vs_q1_2023) %>%
  rename(error_estandar = error_estandar_pct) %>%
  arrange(clasificacion, muestra, outcome, lectura)

write_csv(t_tamano_ano_base, file.path(CARPETA, "T_tamano_ano_base.csv"))
cat("Guardado:", file.path(CARPETA, "T_tamano_ano_base.csv"), "\n")


# ==============================================================================
# 10. SALIDA: T_tabla_especificaciones.csv
# ==============================================================================
titulo("10. GUARDANDO T_tabla_especificaciones.csv")

# "sin tamaño" se LEE de T08 (costo, empleo), sin reestimar. T08 no tiene
# log_permanente ni la lectura "frente al promedio previo" (esa lectura solo
# existe en lecturas_tamano()) -- quedan NA para esas celdas, como corresponde.
t08 <- read_csv(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                          "T08_resumen_principal.csv"), show_col_types = FALSE)

formatear <- function(efecto, sig) {
  sig[is.na(sig)] <- ""   # read_csv() vuelve NA la significancia vacia (p >= 0,10)
  ifelse(is.na(efecto), NA_character_, paste0(sprintf("%.2f", efecto), sig))
}

fila_desde_t08 <- function(eslabon_txt, lectura_txt) {
  f <- t08 %>% filter(eslabon == eslabon_txt, lectura == lectura_txt)
  if (nrow(f) == 0) return(NA_character_)
  formatear(f$efecto_pct[1], f$significancia[1])
}
p_previos_t08 <- function(eslabon_txt) {
  f <- t08 %>% filter(eslabon == eslabon_txt)
  if (nrow(f) == 0) return(NA_real_)
  f$p_previos[1]
}

fila_desde_tamano <- function(tabla, outcome_txt, lectura_txt) {
  f <- tabla %>% filter(outcome == outcome_txt, lectura == lectura_txt)
  if (nrow(f) == 0) return(NA_character_)
  formatear(f$efecto_pct[1], f$significancia[1])
}
p_previos_tamano <- function(tabla, outcome_txt) {
  f <- tabla %>% filter(outcome == outcome_txt)
  if (nrow(f) == 0) return(NA_real_)
  f$p_previos[1]
}

lecturas_fila <- c("A. Cambio 2022 -> 2023", "B. Salto 2023 ajustado por tendencia previa",
                   "C. DiD simple (promedio post - pre)", "D. 2023 frente a 2021",
                   "2023 vs promedio previo")

lecturas_2022 <- resultados_clasificacion %>% filter(clasificacion == "2022")
lecturas_2019 <- resultados_clasificacion %>% filter(clasificacion == "2019")

tabla_especificaciones <- tibble(
  fila = c(lecturas_fila, "p_previos"),
  costo_sin_tamano = c(sapply(lecturas_fila[1:4], function(l) fila_desde_t08("1. Costo laboral por trabajador", l)),
                       NA_character_, as.character(round(p_previos_t08("1. Costo laboral por trabajador"), 4))),
  costo_tamano_2022 = c(sapply(lecturas_fila, function(l) fila_desde_tamano(lecturas_2022, "log_costo", l)),
                        as.character(round(p_previos_tamano(lecturas_2022, "log_costo"), 4))),
  costo_tamano_2019 = c(sapply(lecturas_fila, function(l) fila_desde_tamano(lecturas_2019, "log_costo", l)),
                        as.character(round(p_previos_tamano(lecturas_2019, "log_costo"), 4))),
  empleo_sin_tamano = c(sapply(lecturas_fila[1:4], function(l) fila_desde_t08("2. Empleo total (log)", l)),
                        NA_character_, as.character(round(p_previos_t08("2. Empleo total (log)"), 4))),
  empleo_tamano_2022 = c(sapply(lecturas_fila, function(l) fila_desde_tamano(lecturas_2022, "log_empleo", l)),
                         as.character(round(p_previos_tamano(lecturas_2022, "log_empleo"), 4))),
  empleo_tamano_2019 = c(sapply(lecturas_fila, function(l) fila_desde_tamano(lecturas_2019, "log_empleo", l)),
                         as.character(round(p_previos_tamano(lecturas_2019, "log_empleo"), 4)))
)

# Rango del empleo: minimo y maximo de efecto_pct (valor numerico, no el
# string con estrellas) entre las lecturas A-D y "2023 vs promedio previo",
# por especificacion de empleo, y para las tres especificaciones juntas.
rango_empleo <- function(tabla_fuente, outcome_txt) {
  if (is.null(tabla_fuente)) return(c(NA_real_, NA_real_))
  v <- tabla_fuente %>% filter(outcome == outcome_txt, lectura %in% lecturas_fila) %>% pull(efecto_pct)
  if (length(v) == 0) return(c(NA_real_, NA_real_))
  c(min(v, na.rm = TRUE), max(v, na.rm = TRUE))
}
rango_sin_tamano <- {
  v <- t08 %>% filter(eslabon == "2. Empleo total (log)", lectura %in% lecturas_fila) %>% pull(efecto_pct)
  c(min(v, na.rm = TRUE), max(v, na.rm = TRUE))
}
rango_2022 <- rango_empleo(lecturas_2022, "log_empleo")
rango_2019 <- rango_empleo(lecturas_2019, "log_empleo")
rango_todas <- range(c(rango_sin_tamano, rango_2022, rango_2019), na.rm = TRUE)

fila_rango_min <- tibble(fila = "Rango empleo (mínimo)", costo_sin_tamano = NA_character_,
                         costo_tamano_2022 = NA_character_, costo_tamano_2019 = NA_character_,
                         empleo_sin_tamano = as.character(round(rango_sin_tamano[1], 2)),
                         empleo_tamano_2022 = as.character(round(rango_2022[1], 2)),
                         empleo_tamano_2019 = as.character(round(rango_2019[1], 2)))
fila_rango_max <- tibble(fila = "Rango empleo (máximo)", costo_sin_tamano = NA_character_,
                         costo_tamano_2022 = NA_character_, costo_tamano_2019 = NA_character_,
                         empleo_sin_tamano = as.character(round(rango_sin_tamano[2], 2)),
                         empleo_tamano_2022 = as.character(round(rango_2022[2], 2)),
                         empleo_tamano_2019 = as.character(round(rango_2019[2], 2)))
fila_rango_todas <- tibble(fila = "Rango empleo (todas las especificaciones)",
                          costo_sin_tamano = NA_character_, costo_tamano_2022 = NA_character_,
                          costo_tamano_2019 = NA_character_,
                          empleo_sin_tamano = paste0("[", round(rango_todas[1], 2), ", ", round(rango_todas[2], 2), "]"),
                          empleo_tamano_2022 = NA_character_, empleo_tamano_2019 = NA_character_)

tabla_especificaciones <- bind_rows(tabla_especificaciones, fila_rango_min, fila_rango_max, fila_rango_todas)

ver(tabla_especificaciones, filas = 20)
write_csv(tabla_especificaciones, file.path(CARPETA, "T_tabla_especificaciones.csv"))
cat("Guardado:", file.path(CARPETA, "T_tabla_especificaciones.csv"), "\n")


# ==============================================================================
# 11. GRÁFICO: estudio de evento lineal del empleo, por clasificación de tamaño
# ==============================================================================
titulo("11. GRÁFICO: EVENTO DE EMPLEO CON TAMAÑO 2022, 2019 Y 2021")

evento_para_grafico <- function(var_tamano) {
  EFECTOS_TAMANO <<- efectos_tamano_de(var_tamano)
  datos <<- datos_completo
  ev <- estudio_evento("log_empleo", "Bite2022_obreros", efectos = EFECTOS_TAMANO)
  datos <<- datos_completo
  if (is.null(ev)) return(NULL)
  ev$tabla %>% mutate(clasificacion = var_tamano)
}

trayectorias_tamano <- bind_rows(
  evento_para_grafico("tamano_2022"),
  evento_para_grafico("tamano_2019"),
  evento_para_grafico("tamano_2021")
) %>%
  mutate(clasificacion = factor(clasificacion,
                                levels = c("tamano_2022", "tamano_2019", "tamano_2021"),
                                labels = c("Tamaño 2022", "Tamaño 2019 (alternativa principal)",
                                          "Tamaño 2021 (contraste)")))

grafico_evento_tamano <- ggplot(trayectorias_tamano,
                                aes(x = anio, y = 100 * coeficiente, color = clasificacion,
                                    group = clasificacion)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                    ymax = 100 * (coeficiente + 1.96 * error_estandar)),
                width = 0.2, position = position_dodge(width = 0.3)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3)) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  scale_color_manual(values = c("Tamaño 2022" = COLOR_BAJA,
                                "Tamaño 2019 (alternativa principal)" = COLOR_BAJA,
                                "Tamaño 2021 (contraste)" = COLOR_CONTRASTE)) +
  labs(x = NULL, y = "Efecto en el empleo (%)", color = NULL) +
  tema_tesis

ggsave(file.path(CARPETA, "figuras", "G_evento_empleo_tamano_ano_base.png"),
       grafico_evento_tamano, width = 18, height = 11, units = "cm", dpi = 300, bg = "white")
cat("Gráfico guardado: G_evento_empleo_tamano_ano_base.png\n")

titulo("FIN DEL SCRIPT")
