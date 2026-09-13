# validar_post_controlando_tendencia_lineal.R
#
# AUDITORIA (2026-09-13), motivada por una discrepancia real encontrada
# por el usuario entre estimar_did_principal_empleo_bite.R (modelo
# estatico, post_2023:bite_1sd) y el event study completo de ese mismo
# script: el modelo estatico da 2 de 4 outcomes significativos con
# controles (empleo_temporal p=0.0124, participacion_permanente
# p=0.00511), pero el event study de esos mismos outcomes no muestra un
# quiebre visible en 2023 -- las series se ven como una continuacion
# suave de la tendencia pre-existente, no un salto puntual.
#
# HIPOTESIS A DESCARTAR: el coeficiente "post_2023:exposicion" del
# modelo estatico podria estar capturando la EXTRAPOLACION de una
# tendencia lineal pre-existente (exposicion x anio), no un quiebre
# causado por el choque de 2023. El modelo estatico, tal como esta
# especificado en estimar_did_principal_empleo*.R, NO incluye un
# termino de tendencia lineal continua -- solo el efecto fijo NORDEST +
# ANIO_F (+ controles), que absorbe niveles pero no pendientes
# diferenciales por exposicion.
#
# METODO: se agrega UN termino adicional a la formula del modelo
# estatico -- anio_lineal:exposicion (tendencia lineal CONTINUA en TODA
# la ventana, pre y post, interactuada con la exposicion) -- y se
# reporta el coeficiente post_2023:exposicion CON y SIN ese termino, en
# la MISMA muestra, mismos controles (sector*anio + tamano*anio +
# departamento*anio) y mismo cluster=~NORDEMP que el modelo original.
# Si el coeficiente post deja de ser significativo (o cae sustancialmente
# en magnitud) al agregar la tendencia lineal, es evidencia de que el
# modelo original estaba capturando la extrapolacion de una tendencia
# pre-existente, no un quiebre en 2023. Si se mantiene significativo, es
# evidencia de un quiebre genuino por encima de la tendencia.
#
# anio_lineal = ANIO - min(PANEL_ANIOS_FINAL): YA construida en
# panel_establecimiento_formal.rds (construccion/construir_panel_
# establecimiento_formal.R linea 245) -- no se re-deriva.
#
# Se corre para AMBAS medidas (Exposure2022_obreros, Bite2022_obreros),
# en pie de igualdad, para las 4 variables de resultado, SOLO con
# controles (la especificacion donde aparecio la discrepancia).
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - auditoria_post_vs_tendencia_lineal_exposure.csv
# - auditoria_post_vs_tendencia_lineal_bite.csv

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
if (!file.exists(panel_formal_path)) {
  stop("Falta panel_establecimiento_formal.rds. Corre construccion/construir_panel_establecimiento_formal.R primero.")
}
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) {
  stop("Falta exposicion_firma_eam.rds. Corre pipeline/02_construir_exposicion.R primero.")
}

outcomes_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total",             "Empleo total",
  "empleo_permanente",        "Empleo permanente",
  "empleo_temporal",          "Empleo temporal (directo + agencia)",
  "participacion_permanente", "Participacion de permanentes en el empleo total (%)"
)

panel_formal <- readr::read_rds(panel_formal_path)
exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros)

FE_CON_CONTROLES <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"

correr_par <- function(data, var_y, var_x) {
  vars_regresion <- c(var_y, "post_2023", "anio_lineal", var_x, "NORDEST", "ANIO_F", "CIIU4", "tamano_empresa", "DPTO_fijo")

  f_original <- stats::as.formula(paste0(var_y, " ~ post_2023:", var_x, " | ", FE_CON_CONTROLES))
  m_original <- fixest::feols(f_original, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  f_con_tendencia <- stats::as.formula(paste0(var_y, " ~ post_2023:", var_x, " + anio_lineal:", var_x, " | ", FE_CON_CONTROLES))
  m_con_tendencia <- fixest::feols(f_con_tendencia, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  ct_orig <- summary(m_original)$coeftable
  ct_tend <- summary(m_con_tendencia)$coeftable
  # fixest no garantiza el orden de los nombres dentro del termino de
  # interaccion (puede salir "post_2023:X" o "X:post_2023" segun el
  # orden en que aparecen las variables en la formula) -- se busca sin
  # anclar al inicio, y explicitamente sin cruzar los dos terminos.
  fila_post_tend <- grep("post_2023", rownames(ct_tend))
  fila_tend_tend <- grep("anio_lineal", rownames(ct_tend))
  if (length(fila_post_tend) != 1) stop("No se encontro (o se encontro mas de 1) coeficiente 'post_2023' en el modelo con tendencia, para ", var_y, "/", var_x, ".")
  if (length(fila_tend_tend) != 1) stop("No se encontro (o se encontro mas de 1) coeficiente 'anio_lineal' en el modelo con tendencia, para ", var_y, "/", var_x, ".")

  tibble::tibble(
    variable = var_y,
    n_obs = stats::nobs(m_original),
    post_estimate_original = round(unname(ct_orig[1, 1]), 6),
    post_se_original = round(unname(ct_orig[1, 2]), 6),
    post_p_original = signif(unname(ct_orig[1, 4]), 4),
    post_estimate_con_tendencia = round(unname(ct_tend[fila_post_tend, 1]), 6),
    post_se_con_tendencia = round(unname(ct_tend[fila_post_tend, 2]), 6),
    post_p_con_tendencia = signif(unname(ct_tend[fila_post_tend, 4]), 4),
    tendencia_estimate = round(unname(ct_tend[fila_tend_tend, 1]), 6),
    tendencia_se = round(unname(ct_tend[fila_tend_tend, 2]), 6),
    tendencia_p = signif(unname(ct_tend[fila_tend_tend, 4]), 4),
    post_sigue_significativo_al_5pct = unname(ct_tend[fila_post_tend, 4]) < 0.05,
    cambio_de_significancia = (unname(ct_orig[1, 4]) < 0.05) != (unname(ct_tend[fila_post_tend, 4]) < 0.05)
  )
}

# ------------------------------------------------------------------
# Exposure2022_obreros
# ------------------------------------------------------------------

datos_exposure <- panel_formal %>%
  dplyr::inner_join(exposicion_firma %>% dplyr::select(NORDEMP, Exposure2022_obreros) %>% dplyr::filter(!is.na(Exposure2022_obreros)), by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023), exposicion_10pp = Exposure2022_obreros / 0.1)

resultado_exposure <- purrr::map_dfr(outcomes_info$var, ~ correr_par(datos_exposure, .x, "exposicion_10pp"))
readr::write_csv(resultado_exposure, file.path(out_dir, "auditoria_post_vs_tendencia_lineal_exposure.csv"))

# ------------------------------------------------------------------
# Bite2022_obreros
# ------------------------------------------------------------------

datos_bite_pre <- panel_formal %>%
  dplyr::inner_join(exposicion_firma %>% dplyr::select(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros)), by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023))
sd_bite_muestra <- stats::sd(datos_bite_pre %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::pull(Bite2022_obreros), na.rm = TRUE)
datos_bite <- datos_bite_pre %>% dplyr::mutate(bite_1sd = Bite2022_obreros / sd_bite_muestra)

resultado_bite <- purrr::map_dfr(outcomes_info$var, ~ correr_par(datos_bite, .x, "bite_1sd"))
readr::write_csv(resultado_bite, file.path(out_dir, "auditoria_post_vs_tendencia_lineal_bite.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("validar_post_controlando_tendencia_lineal.R -- Auditoria: ¿el coeficiente post es un quiebre o la extrapolacion de una tendencia pre-existente?")
message("")
message("=== Exposure2022_obreros (con controles) ===")
print(resultado_exposure %>% dplyr::select(variable, post_estimate_original, post_p_original, post_estimate_con_tendencia, post_p_con_tendencia, tendencia_p, post_sigue_significativo_al_5pct, cambio_de_significancia), n = Inf, width = Inf)
message("")
message("=== Bite2022_obreros (con controles) ===")
print(resultado_bite %>% dplyr::select(variable, post_estimate_original, post_p_original, post_estimate_con_tendencia, post_p_con_tendencia, tendencia_p, post_sigue_significativo_al_5pct, cambio_de_significancia), n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
