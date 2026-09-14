# escrutinio_bite_participacion_firma.R
#
# Bateria de escrutinio completo, activada por la regla de escalacion tras
# comparar_especificacion_principal_firma_vs_establecimiento.R: en el
# panel de FIRMA genuino (panel_analitico_firma_eam.rds, empresa-anio),
# Bite2022_obreros x participacion_permanente resulto significativo TANTO
# sin como con tendencia lineal (sin: p=0.00490, con: p=0.0297; ademas
# CAMBIA DE SIGNO: +1.18 -> -0.950). En el panel de establecimiento (el
# que sustenta la Tabla 1 actual) esta misma celda NO escala (con
# tendencia p=0.0523, apenas por encima del umbral) -- por eso la
# diferencia de panel importa y se corre esta bateria solo sobre el panel
# correcto.
#
# Bateria (identica a la ya usada para el caso analogo de establecimiento,
# escrutinio_bite_participacion_espA.R):
# 1) Tendencia CUADRATICA: agrega anio_cuadratico (=anio_lineal^2)
#    interactuado con bite_1sd, ADEMAS del termino lineal ya incluido.
# 2) Leave-one-year-out: excluye un anio del panel a la vez (9 anios:
#    2015-2019, 2021-2024), reestima la version CON tendencia lineal.
#
# DERIVADO de comparar_especificacion_principal_firma_vs_establecimiento.R
# (misma reconstruccion de datos_firma_bite, mismo FE_CON con NORDEMP,
# mismo bite_1sd).
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - escrutinio_bite_participacion_firma_cuadratica.csv
# - escrutinio_bite_participacion_firma_leaveoneyearout.csv

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "purrr", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")

panel_firma <- readr::read_rds(file.path(data_dir, "panel_analitico_firma_eam.rds")) %>%
  dplyr::mutate(anio_cuadratico = anio_lineal^2)

exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))
bite_firma <- exposicion_firma %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

datos_firma_bite <- panel_firma %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO), !is.na(tamano_empresa)) %>%
  dplyr::inner_join(bite_firma, by = "NORDEMP", suffix = c("", "_dup")) %>%
  dplyr::mutate(bite_1sd = Bite2022_obreros / sd_bite_muestra)

FE_CON <- "NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO^ANIO_F"

extraer_beta <- function(modelo, termino = "post_2023:bite_1sd") {
  ct <- summary(modelo)$coeftable
  fila <- which(rownames(ct) == termino)
  tibble::tibble(
    estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
    p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05,
    n_obs = stats::nobs(modelo)
  )
}

# ------------------------------------------------------------------
# 1) Tendencia cuadratica
# ------------------------------------------------------------------

f_cuadratica <- stats::as.formula(paste0(
  "participacion_permanente ~ post_2023:bite_1sd + anio_lineal:bite_1sd + anio_cuadratico:bite_1sd | ", FE_CON
))
m_cuadratica <- fixest::feols(f_cuadratica, data = datos_firma_bite, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
resultado_cuadratica <- extraer_beta(m_cuadratica) %>% dplyr::mutate(prueba = "tendencia_cuadratica", .before = 1)

# ------------------------------------------------------------------
# 2) Leave-one-year-out (version CON tendencia lineal)
# ------------------------------------------------------------------

f_con_tendencia <- stats::as.formula(paste0(
  "participacion_permanente ~ post_2023:bite_1sd + anio_lineal:bite_1sd | ", FE_CON
))

anios_panel <- sort(unique(datos_firma_bite$ANIO))
resultado_loyo <- purrr::map_dfr(anios_panel, function(anio_excluido) {
  data_sub <- datos_firma_bite %>% dplyr::filter(ANIO != anio_excluido)
  modelo <- tryCatch(
    fixest::feols(f_con_tendencia, data = data_sub, cluster = ~NORDEMP, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(modelo)) {
    return(tibble::tibble(anio_excluido = anio_excluido, estimate = NA_real_, std_error = NA_real_, p_value = NA_real_, significativo = NA, n_obs = NA_integer_))
  }
  extraer_beta(modelo) %>% dplyr::mutate(anio_excluido = anio_excluido, .before = 1)
})

readr::write_csv(resultado_cuadratica, file.path(out_dir, "escrutinio_bite_participacion_firma_cuadratica.csv"))
readr::write_csv(resultado_loyo, file.path(out_dir, "escrutinio_bite_participacion_firma_leaveoneyearout.csv"))

script_header("escrutinio_bite_participacion_firma.R -- bateria de escrutinio completo (Bite x participacion_permanente, panel de FIRMA)")
message("")
message("=== Tendencia cuadratica (beta = post_2023:bite_1sd) ===")
print(resultado_cuadratica, width = Inf)
message("")
message("=== Leave-one-year-out (con tendencia lineal) ===")
print(resultado_loyo, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
