# escrutinio_mecanismos_bite_firma.R
#
# Bateria de escrutinio completo, activada por la regla de parada de
# estimar_mecanismos_ajuste_firma.R: 2 celdas resultaron significativas
# TANTO sin como con tendencia lineal, ambas CAMBIANDO DE SIGNO:
#   - Bite2022_obreros x asinh(mantenimiento, C3R23C3): sin -0.0552 (p=0.0250), con +0.0701 (p=0.0276)
#   - Bite2022_obreros x asinh(ventas, VALORVEN): sin -0.0368 (p=0.000432), con +0.0462 (p=0.000280)
#
# Bateria (identica a la ya usada en las rondas anteriores de escrutinio):
# 1) Tendencia CUADRATICA: agrega anio_cuadratico (=anio_lineal^2)
#    interactuado con bite_1sd, ADEMAS del termino lineal ya incluido.
# 2) Leave-one-year-out: excluye un anio del panel a la vez (9 anios:
#    2015-2019, 2021-2024), reestima la version CON tendencia lineal.
#
# DERIVADO de estimar_mecanismos_ajuste_firma.R (misma reconstruccion de
# datos_bite, mismo FE_CON con NORDEMP, mismo bite_1sd).
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - escrutinio_mecanismos_bite_firma_cuadratica.csv (2 filas)
# - escrutinio_mecanismos_bite_firma_leaveoneyearout.csv (2 vars x 9 anios = 18 filas)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "purrr", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

panel_firma <- readr::read_rds(file.path(data_dir, "panel_analitico_firma_eam.rds")) %>%
  dplyr::mutate(anio_cuadratico = anio_lineal^2)

exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))
bite_firma <- exposicion_firma %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

cols_mecanismo <- c("C3R23C3", "VALORVEN")
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

datos_bite <- panel_firma %>%
  dplyr::left_join(mecanismo_firma, by = c("NORDEMP", "ANIO")) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(tamano_empresa), !is.na(Bite2022_obreros)) %>%
  dplyr::mutate(
    asinh_C3R23C3 = asinh(C3R23C3), asinh_VALORVEN = asinh(VALORVEN),
    bite_1sd = Bite2022_obreros / sd_bite_muestra
  )

FE_CON <- "NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F"

extraer_beta <- function(modelo, termino = "post_2023:bite_1sd") {
  ct <- summary(modelo)$coeftable
  fila <- which(rownames(ct) == termino)
  tibble::tibble(
    estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
    p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05,
    n_obs = stats::nobs(modelo)
  )
}

variables_activadas <- c("asinh_C3R23C3", "asinh_VALORVEN")

# ------------------------------------------------------------------
# 1) Tendencia cuadratica
# ------------------------------------------------------------------

resultado_cuadratica <- purrr::map_dfr(variables_activadas, function(var_y) {
  f_cuadratica <- stats::as.formula(paste0(
    var_y, " ~ post_2023:bite_1sd + anio_lineal:bite_1sd + anio_cuadratico:bite_1sd | ", FE_CON
  ))
  modelo <- fixest::feols(f_cuadratica, data = datos_bite, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  extraer_beta(modelo) %>% dplyr::mutate(variable = var_y, prueba = "tendencia_cuadratica", .before = 1)
})

# ------------------------------------------------------------------
# 2) Leave-one-year-out (version CON tendencia lineal)
# ------------------------------------------------------------------

anios_panel <- sort(unique(datos_bite$ANIO))
resultado_loyo <- purrr::map_dfr(variables_activadas, function(var_y) {
  f_con_tendencia <- stats::as.formula(paste0(var_y, " ~ post_2023:bite_1sd + anio_lineal:bite_1sd | ", FE_CON))
  purrr::map_dfr(anios_panel, function(anio_excluido) {
    data_sub <- datos_bite %>% dplyr::filter(ANIO != anio_excluido)
    modelo <- tryCatch(
      fixest::feols(f_con_tendencia, data = data_sub, cluster = ~NORDEMP, warn = FALSE, notes = FALSE),
      error = function(e) NULL
    )
    if (is.null(modelo)) {
      return(tibble::tibble(variable = var_y, anio_excluido = anio_excluido, estimate = NA_real_, std_error = NA_real_, p_value = NA_real_, significativo = NA, n_obs = NA_integer_))
    }
    extraer_beta(modelo) %>% dplyr::mutate(variable = var_y, anio_excluido = anio_excluido, .before = 1)
  })
})

readr::write_csv(resultado_cuadratica, file.path(out_dir, "escrutinio_mecanismos_bite_firma_cuadratica.csv"))
readr::write_csv(resultado_loyo, file.path(out_dir, "escrutinio_mecanismos_bite_firma_leaveoneyearout.csv"))

script_header("escrutinio_mecanismos_bite_firma.R -- bateria de escrutinio completo (Bite x mantenimiento, Bite x ventas)")
message("")
message("=== Tendencia cuadratica ===")
print(resultado_cuadratica, width = Inf)
message("")
message("=== Leave-one-year-out (con tendencia lineal) ===")
print(resultado_loyo, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
