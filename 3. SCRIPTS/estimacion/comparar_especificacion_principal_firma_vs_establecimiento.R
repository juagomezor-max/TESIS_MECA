# comparar_especificacion_principal_firma_vs_establecimiento.R
#
# HALLAZGO QUE MOTIVA ESTE SCRIPT (2026-09-15): la especificacion
# "principal"/"confirmada" de la seccion 4.4 (estimar_did_principal_
# empleo.R, generar_tablas_resultados.R -> Tabla 1 de INDICE_RESULTADOS.md)
# en realidad corre sobre panel_establecimiento_formal.rds -- granularidad
# NORDEST-ANIO (establecimiento), con Exposure2022_obreros/Bite2022_obreros
# HEREDADOS de la firma duena, no un panel de empresa-anio genuino. La
# tesis (segun la instruccion del usuario) documenta el modelo base como
# "a nivel empresa-anio". Existe panel_analitico_firma_eam.rds
# (pipeline/03_construir_panel.R), genuinamente NORDEMP-ANIO (62,816
# filas, 9,087 firmas), que es el panel correcto de empresa-anio.
#
# Este script corre la MISMA especificacion (modelo estatico: post_2023 x
# exposicion, sin y con tendencia lineal pre-existente interactuada,
# controles sector*anio + tamano*anio + departamento*anio, cluster
# ~NORDEMP) en AMBOS paneles, con formula IDENTICA salvo el FE de unidad
# (NORDEMP vs. NORDEST) y la variable de departamento (DPTO en el panel
# de firma -- crudo, puede variar anio a anio -- vs. DPTO_fijo en el de
# establecimiento -- version estabilizada/recodificada, ver
# construccion/construir_panel_establecimiento_formal.R lineas 185-201).
# Esa diferencia de insumo de DPTO es una limitacion conocida de esta
# comparacion, documentada aqui, no resuelta (reconstruir un DPTO_fijo a
# nivel firma esta fuera del alcance de esta comparacion urgente).
#
# NO SE REESTIMAN LAS 4 VARIABLES DE MECANISMO (mantenimiento,
# outsourcing, inversion, ventas) EN ESTE SCRIPT -- esa linea de trabajo
# esta pausada hasta resolver cual panel es "el principal" (instruccion
# explicita del usuario).
#
# DERIVADO de (no reimplementado de memoria):
# - Formula/controles/cluster del modelo estatico: IDENTICOS a
#   estimacion/estimar_did_principal_empleo.R (Exposure) y
#   estimacion/estimar_did_principal_empleo_bite.R (Bite).
# - Patron sin/con tendencia lineal interactuada: IDENTICO al ya usado en
#   estimacion/estimar_especificacion_a_establecimiento.R (seccion 4.5),
#   aqui sin los terminos de Multi_f (este es el modelo estatico simple,
#   no la Especificacion A/B de heterogeneidad mono/multiplanta).
# - anio_lineal: ya viene construido en panel_analitico_firma_eam.rds
#   (pipeline/03_construir_panel.R) y en panel_establecimiento_formal.rds
#   (construccion/construir_panel_establecimiento_formal.R), misma formula
#   (ANIO - min(PANEL_ANIOS_FINAL)) en ambos -- no se recalcula.
# - bite_1sd: SD de Bite2022_obreros recalculada fresca sobre la muestra
#   distinta de firmas de exposicion_firma_eam.rds, IDENTICO patron ya
#   usado en toda estimacion de Bite en este repositorio.
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - comparacion_principal_firma_vs_establecimiento.csv (8 celdas x 2 paneles x 2 versiones = 32 filas)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "purrr", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")

# ------------------------------------------------------------------
# 1) Cargar los 2 paneles + Bite escalado (misma SD para ambos, viene de
#    la misma fuente de exposicion_firma_eam.rds).
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path(data_dir, "panel_analitico_firma_eam.rds"))
panel_establecimiento <- readr::read_rds(file.path(data_dir, "panel_establecimiento_formal.rds"))

exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))
bite_firma <- exposicion_firma %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

# --- Panel de FIRMA (empresa-anio genuino) ---
datos_firma_exposure <- panel_firma %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO), !is.na(tamano_empresa), !is.na(Exposure2022_obreros))
datos_firma_bite <- panel_firma %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO), !is.na(tamano_empresa)) %>%
  dplyr::inner_join(bite_firma, by = "NORDEMP", suffix = c("", "_dup")) %>%
  dplyr::mutate(bite_1sd = Bite2022_obreros / sd_bite_muestra)

# --- Panel de ESTABLECIMIENTO (el que sustenta la Tabla 1 actual) ---
datos_est_exposure <- panel_establecimiento %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::inner_join(exposicion_firma %>% dplyr::distinct(NORDEMP, Exposure2022_obreros) %>% dplyr::filter(!is.na(Exposure2022_obreros)), by = "NORDEMP") %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023), exposicion_10pp = Exposure2022_obreros / 0.1)
datos_est_bite <- panel_establecimiento %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::inner_join(bite_firma, by = "NORDEMP") %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023), bite_1sd = Bite2022_obreros / sd_bite_muestra)

outcomes_info <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")

# ------------------------------------------------------------------
# 2) Estimacion generica: modelo estatico, sin/con tendencia, FE de
#    unidad parametrizable (NORDEMP o NORDEST), var. de dpto parametrizable.
# ------------------------------------------------------------------

correr_modelo <- function(data, var_y, var_x, fe_unidad, fe_dpto, con_tendencia) {
  fe_con <- paste0(fe_unidad, " + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + ", fe_dpto, "^ANIO_F")
  terms <- paste0("post_2023:", var_x)
  if (con_tendencia) terms <- c(terms, paste0("anio_lineal:", var_x))
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ ", paste(terms, collapse = " + "), " | ", fe_con))
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

combinaciones <- tibble::tribble(
  ~panel,             ~medida,                    ~data_ref,       ~var_x,      ~fe_unidad, ~fe_dpto,
  "Firma (empresa-anio)",        "Exposure2022_obreros", "firma_exposure", "exposicion_10pp", "NORDEMP", "DPTO",
  "Firma (empresa-anio)",        "Bite2022_obreros",     "firma_bite",     "bite_1sd",        "NORDEMP", "DPTO",
  "Establecimiento (Tabla 1 actual)", "Exposure2022_obreros", "est_exposure",   "exposicion_10pp", "NORDEST", "DPTO_fijo",
  "Establecimiento (Tabla 1 actual)", "Bite2022_obreros",     "est_bite",       "bite_1sd",        "NORDEST", "DPTO_fijo"
)

datasets <- list(
  firma_exposure = datos_firma_exposure, firma_bite = datos_firma_bite,
  est_exposure = datos_est_exposure, est_bite = datos_est_bite
)

resultado <- purrr::pmap_dfr(combinaciones, function(panel, medida, data_ref, var_x, fe_unidad, fe_dpto) {
  data <- datasets[[data_ref]]
  purrr::map_dfr(outcomes_info, function(var_y) {
    dplyr::bind_rows(
      correr_modelo(data, var_y, var_x, fe_unidad, fe_dpto, FALSE) %>% dplyr::mutate(con_tendencia = FALSE),
      correr_modelo(data, var_y, var_x, fe_unidad, fe_dpto, TRUE) %>% dplyr::mutate(con_tendencia = TRUE)
    ) %>% dplyr::mutate(panel = panel, medida = medida, variable = var_y, .before = 1)
  })
})

readr::write_csv(resultado, file.path(out_dir, "comparacion_principal_firma_vs_establecimiento.csv"))

script_header("comparar_especificacion_principal_firma_vs_establecimiento.R -- Tabla 1: firma-anio genuino vs. establecimiento-anio")
message("")
print(resultado, n = Inf, width = Inf)
message("")
message("Tabla exportada en: ", out_dir)
