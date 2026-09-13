# escrutinio_bite_participacion_espA.R
#
# Bateria de escrutinio completo, activada por la regla de parada de
# estimar_especificacion_a_establecimiento.R: el UNICO coeficiente que
# resulto significativo tanto SIN como CON tendencia lineal fue
# Bite2022_obreros x participacion_permanente, Especificacion A (muestra
# completa: mono + multiplanta), beta1 = post_2023:bite_1sd
#   sin tendencia: estimate=?, p=0.0124 (ver especificacion_a_establecimiento_principal.csv)
#   con tendencia: p=0.0459
#
# Bateria (identica a la ya usada en la ronda de robustez anterior a
# nivel firma):
# 1) Tendencia CUADRATICA: agrega anio_cuadratico (= anio_lineal^2)
#    interactuado con bite_1sd, bite_1sd:Multi_f y Multi_f, ADEMAS de
#    los terminos lineales ya incluidos -- si beta1 sigue significativo
#    con una tendencia pre-existente mas flexible, es mas dificil
#    explicarlo por una tendencia no lineal preexistente.
# 2) Leave-one-year-out: se excluye un anio del panel a la vez (de los
#    9 anios: 2015-2019, 2021-2024) y se reestima la especificacion CON
#    tendencia lineal (la version que goza de significancia marginal),
#    para ver si el resultado depende de un solo anio.
#
# DERIVADO de estimar_especificacion_a_establecimiento.R (misma
# reconstruccion de Exposure2022_obreros_est, Multi_f, bite_1sd, FE_CON).

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "purrr", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")

ANIO_BASE <- 2022

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))

base_establecimiento <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP), ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::distinct(NORDEST, NORDEMP, ANIO)

base_2022_multi <- base_establecimiento %>% dplyr::filter(ANIO == ANIO_BASE) %>% dplyr::distinct(NORDEST, NORDEMP)
n_est_por_firma_2022 <- base_2022_multi %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_por_firma_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)
message("Multi_f: ", length(firmas_262), " firmas (esperado 262).")

panel_formal <- readr::read_rds(panel_formal_path)
bite_firma <- readr::read_rds(exposicion_firma_path) %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

panel_base <- panel_formal %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    Multi_f = as.integer(NORDEMP %in% firmas_262),
    post_2023 = as.integer(ANIO >= 2023),
    anio_cuadratico = anio_lineal^2
  )

datos_bite <- panel_base %>%
  dplyr::inner_join(bite_firma, by = "NORDEMP") %>%
  dplyr::mutate(bite_1sd = Bite2022_obreros / sd_bite_muestra)

FE_CON <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"

extraer_beta1 <- function(modelo, termino = "post_2023:bite_1sd") {
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

terms_post <- c("post_2023:bite_1sd", "post_2023:bite_1sd:Multi_f", "post_2023:Multi_f")
terms_lineal <- c("anio_lineal:bite_1sd", "anio_lineal:bite_1sd:Multi_f", "anio_lineal:Multi_f")
terms_cuadratico <- c("anio_cuadratico:bite_1sd", "anio_cuadratico:bite_1sd:Multi_f", "anio_cuadratico:Multi_f")

f_cuadratica <- stats::as.formula(paste0(
  "participacion_permanente ~ ", paste(c(terms_post, terms_lineal, terms_cuadratico), collapse = " + "), " | ", FE_CON
))
m_cuadratica <- fixest::feols(f_cuadratica, data = datos_bite, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
resultado_cuadratica <- extraer_beta1(m_cuadratica) %>% dplyr::mutate(prueba = "tendencia_cuadratica", .before = 1)

# ------------------------------------------------------------------
# 2) Leave-one-year-out (version CON tendencia lineal)
# ------------------------------------------------------------------

f_con_tendencia <- stats::as.formula(paste0(
  "participacion_permanente ~ ", paste(c(terms_post, terms_lineal), collapse = " + "), " | ", FE_CON
))

anios_panel <- sort(unique(datos_bite$ANIO))
resultado_loyo <- purrr::map_dfr(anios_panel, function(anio_excluido) {
  data_sub <- datos_bite %>% dplyr::filter(ANIO != anio_excluido)
  modelo <- tryCatch(
    fixest::feols(f_con_tendencia, data = data_sub, cluster = ~NORDEMP, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(modelo)) {
    return(tibble::tibble(anio_excluido = anio_excluido, estimate = NA_real_, std_error = NA_real_, p_value = NA_real_, significativo = NA, n_obs = NA_integer_))
  }
  extraer_beta1(modelo) %>% dplyr::mutate(anio_excluido = anio_excluido, .before = 1)
})

readr::write_csv(resultado_cuadratica, file.path(out_dir, "escrutinio_bite_participacion_espA_cuadratica.csv"))
readr::write_csv(resultado_loyo, file.path(out_dir, "escrutinio_bite_participacion_espA_leaveoneyearout.csv"))

script_header("escrutinio_bite_participacion_espA.R -- bateria de escrutinio completo (Bite x participacion_permanente, Especificacion A)")
message("")
message("=== Tendencia cuadratica (beta1 = post_2023:bite_1sd) ===")
print(resultado_cuadratica, width = Inf)
message("")
message("=== Leave-one-year-out (con tendencia lineal) ===")
print(resultado_loyo, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
