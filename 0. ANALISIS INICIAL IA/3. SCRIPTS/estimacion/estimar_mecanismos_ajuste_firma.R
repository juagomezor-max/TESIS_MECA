# estimar_mecanismos_ajuste_firma.R
#
# Paso 3 de la linea "mecanismos de ajuste" (empresa-anio,
# panel_analitico_firma_eam.rds -- panel OFICIAL, ver BORRADOR_RESULTADOS.md
# seccion 0). Las 4 variables pasaron el Paso 2 (pre-tendencias 2015-2019
# limpias CON controles sector*anio+tamano*anio, ver
# validar_pretendencias_mecanismos_firma.R): mantenimiento (C3R23C3),
# subcontratacion (C3R41C3, con advertencia de 72.2% en cero), inversion
# (C7R10C2), ventas (VALORVEN). Delta empleo NO se trata como mecanismo
# (decision explicita del usuario) -- queda solo documentado como
# limitacion.
#
# ESPECIFICACION (misma que la principal, panel de firma):
#   Sin tendencia:  asinh(Y) ~ post_2023:X | NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F
#   Con tendencia:  + anio_lineal:X
#   cluster = ~NORDEMP siempre. SOLO 2 controles (sector*anio + tamano*anio),
#   instruccion explicita del usuario para esta linea -- evita ademas la
#   limitacion conocida de DPTO crudo (seccion 0 de BORRADOR_RESULTADOS.md).
#
# PLACEBO 2022: panel PRE (2015-2019+2021-2022), post_2022 en vez de
# post_2023, con y sin controles (NO se agrega tendencia al placebo,
# mismo criterio que el resto del proyecto).
#
# REGLA DE PARADA: identica al resto del proyecto -- no significativo sin
# tendencia -> nulo; significativo sin tendencia pero no con (o cambia de
# signo) -> "no robusto"; significativo en AMBAS -> bateria completa
# (cuadratica + leave-one-year-out), en script separado, solo si aplica.
#
# DERIVADO de (no reimplementado de memoria):
# - Regla de agregacion NORDEMP-ANIO de las 4 variables: IDENTICA a
#   pipeline/01_construir_base.R y a validar_pretendencias_mecanismos_firma.R
#   (Paso 2 de esta misma linea).
# - asinh(): IDENTICO patron a validar_pretendencias_panel_formal.R /
#   validar_pretendencias_mecanismos_firma.R.
# - Formula sin/con tendencia + bite_1sd: IDENTICO patron a
#   comparar_especificacion_principal_firma_vs_establecimiento.R.
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - mecanismos_ajuste_firma_principal.csv (4 vars x 2 medidas x 2 versiones = 16 filas)
# - mecanismos_ajuste_firma_placebo2022.csv (4 vars x 2 medidas x 2 versiones = 16 filas)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "purrr", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# ------------------------------------------------------------------
# 1) Panel de firma completo (9 anios) + variables de mecanismo
#    agregadas a NORDEMP-ANIO (misma regla auditada).
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path(data_dir, "panel_analitico_firma_eam.rds"))

exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))
bite_firma <- exposicion_firma %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

cols_mecanismo <- c("C3R23C3", "C3R41C3", "C7R10C2", "VALORVEN")
PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO", cols_mecanismo))

mecanismo_firma <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO), ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::select(NORDEMP, ANIO, dplyr::all_of(cols_mecanismo)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_mecanismo), safe_numeric)) %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(cols_mecanismo), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)), .groups = "drop")

panel_built <- panel_firma %>%
  dplyr::left_join(mecanismo_firma, by = c("NORDEMP", "ANIO")) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    asinh_C3R23C3 = asinh(C3R23C3), asinh_C3R41C3 = asinh(C3R41C3),
    asinh_C7R10C2 = asinh(C7R10C2), asinh_VALORVEN = asinh(VALORVEN),
    bite_1sd = Bite2022_obreros / sd_bite_muestra
  )

datos_exposure <- panel_built %>% dplyr::filter(!is.na(Exposure2022_obreros))
datos_bite <- panel_built %>% dplyr::filter(!is.na(bite_1sd))

outcomes_info <- tibble::tribble(
  ~var,             ~label,
  "asinh_C3R23C3",  "asinh(mantenimiento, reparaciones, accesorios y repuestos)",
  "asinh_C3R41C3",  "asinh(outsourcing / servicios contratados con terceros)",
  "asinh_C7R10C2",  "asinh(total inversiones en activos fijos)",
  "asinh_VALORVEN", "asinh(valor de las ventas)"
)

medidas_info <- tibble::tribble(
  ~medida,                    ~var_x,            ~data_ref,
  "Exposure2022_obreros", "exposicion_10pp", "exposure",
  "Bite2022_obreros",     "bite_1sd",        "bite"
)
datasets <- list(exposure = datos_exposure, bite = datos_bite)

FE_CON <- "NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F"

# ------------------------------------------------------------------
# 2) Estimacion PRINCIPAL: sin y con tendencia.
# ------------------------------------------------------------------

correr_modelo <- function(data, var_y, var_x, con_tendencia) {
  terms <- paste0("post_2023:", var_x)
  if (con_tendencia) terms <- c(terms, paste0("anio_lineal:", var_x))
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ ", paste(terms, collapse = " + "), " | ", FE_CON))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  ct <- summary(modelo)$coeftable
  termino <- paste0("post_2023:", var_x)
  fila <- which(rownames(ct) == termino)
  tibble::tibble(
    estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
    p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05,
    n_obs = stats::nobs(modelo)
  )
}

resultado_principal <- purrr::pmap_dfr(medidas_info, function(medida, var_x, data_ref) {
  data <- datasets[[data_ref]]
  purrr::map_dfr(seq_len(nrow(outcomes_info)), function(i) {
    var_y <- outcomes_info$var[i]
    dplyr::bind_rows(
      correr_modelo(data, var_y, var_x, FALSE) %>% dplyr::mutate(con_tendencia = FALSE),
      correr_modelo(data, var_y, var_x, TRUE) %>% dplyr::mutate(con_tendencia = TRUE)
    ) %>% dplyr::mutate(medida = medida, variable = var_y, .before = 1)
  })
})

readr::write_csv(resultado_principal, file.path(out_dir, "mecanismos_ajuste_firma_principal.csv"))

# ------------------------------------------------------------------
# 3) Placebo 2022: panel PRE, post_2022, con/sin controles.
# ------------------------------------------------------------------

ANIOS_PRE <- c(2015:2019, 2021, 2022)
datos_exposure_pre <- datos_exposure %>% dplyr::filter(ANIO %in% ANIOS_PRE) %>% dplyr::mutate(post_2022 = as.integer(ANIO >= 2022))
datos_bite_pre <- datos_bite %>% dplyr::filter(ANIO %in% ANIOS_PRE) %>% dplyr::mutate(post_2022 = as.integer(ANIO >= 2022))
datasets_pre <- list(exposure = datos_exposure_pre, bite = datos_bite_pre)

correr_placebo <- function(data, var_y, var_x, con_controles) {
  fe <- if (con_controles) FE_CON else "NORDEMP + ANIO_F"
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ post_2022:", var_x, " | ", fe))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  ct <- summary(modelo)$coeftable
  termino <- paste0("post_2022:", var_x)
  fila <- which(rownames(ct) == termino)
  tibble::tibble(
    estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
    p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05,
    n_obs = stats::nobs(modelo)
  )
}

resultado_placebo <- purrr::pmap_dfr(medidas_info, function(medida, var_x, data_ref) {
  data <- datasets_pre[[data_ref]]
  purrr::map_dfr(seq_len(nrow(outcomes_info)), function(i) {
    var_y <- outcomes_info$var[i]
    dplyr::bind_rows(
      correr_placebo(data, var_y, var_x, FALSE) %>% dplyr::mutate(con_controles = FALSE),
      correr_placebo(data, var_y, var_x, TRUE) %>% dplyr::mutate(con_controles = TRUE)
    ) %>% dplyr::mutate(medida = medida, variable = var_y, .before = 1)
  })
})

readr::write_csv(resultado_placebo, file.path(out_dir, "mecanismos_ajuste_firma_placebo2022.csv"))

script_header("estimar_mecanismos_ajuste_firma.R -- Paso 3: estimacion mecanismos de ajuste, panel de FIRMA")
message("")
message("=== Principal: sin y con tendencia ===")
print(resultado_principal, n = Inf, width = Inf)
message("")
message("=== Placebo 2022: sin y con controles ===")
print(resultado_placebo, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
