# validar_robustez_forma_funcional_tendencia_exposure.R
#
# Robustece el hallazgo de validar_post_controlando_tendencia_lineal.R
# para los 2 outcomes de Exposure2022_obreros que cambiaron de signo Y
# de significancia al agregar una tendencia lineal pre-existente
# (empleo_temporal, participacion_permanente): un cambio de signo al
# agregar UN control es consistente tanto con inestabilidad de
# especificacion como con un efecto real -- esta motivado
# especificamente porque Exposure2022_obreros ya tiene evidencia
# independiente de ser una medida debil (primer eslabon, p=0.771 con
# controles, ver validar_primer_eslabon_costo_laboral.R).
#
# TRES CHEQUEOS, SOLO para Exposure2022_obreros, SOLO para
# empleo_temporal y participacion_permanente, SOLO con los 3 controles
# (sector*anio + tamano*anio + departamento*anio), mismo cluster=~NORDEMP:
#
# A. TENDENCIA CUADRATICA: agrega anio_lineal:exposicion Y
#    anio_lineal_sq:exposicion (anio_lineal_sq = anio_lineal^2) a la
#    especificacion, en vez de solo la lineal. Si el coeficiente post
#    cambia de signo/significancia otra vez, es evidencia de que ni
#    siquiera una tendencia lineal es la forma funcional correcta.
#
# B. EVENT STUDY AÑO A AÑO (inspeccion de forma, no una regresion
#    nueva): se leen los coeficientes YA calculados en
#    estimar_did_principal_empleo.R (event_study_completo_coeficientes.csv,
#    ref=2022, CON controles, ventana completa) para estos 2 outcomes,
#    y se reporta la secuencia completa 2015-2024 para inspeccionar si
#    el patron PRE (2015-2021 relativo a 2022) ya es marcadamente no
#    lineal o con quiebres -- lo que explicaria que una tendencia
#    lineal este mal especificada.
#
# C. LEAVE-ONE-YEAR-OUT: repite la especificacion CON tendencia lineal
#    (identica a validar_post_controlando_tendencia_lineal.R) excluyendo
#    UN anio PRE-tratamiento a la vez (2015,2016,2017,2018,2019,2021,2022),
#    para ver si el resultado depende de un anio especifico.
#
# Si el signo y la significancia de estos 2 coeficientes NO son
# estables entre las 3 formas funcionales (lineal ya calculada en
# validar_post_controlando_tendencia_lineal.R, cuadratica aqui, y el
# patron del event-study), o si el resultado depende de excluir un solo
# anio, se reporta EXPLICITAMENTE como evidencia de fragilidad a la
# forma funcional del control -- no como una discrepancia menor. No se
# decide aqui si la especificacion con tendencia se vuelve estandar.
#
# DERIVADO de (no reimplementado de memoria):
# - Panel, controles, cluster, escala de exposicion_10pp: identico a
#   validar_post_controlando_tendencia_lineal.R / estimar_did_principal_
#   empleo.R.
# - anio_lineal: YA construida en panel_establecimiento_formal.rds.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - robustez_tendencia_cuadratica_exposure.csv (chequeo A)
# - robustez_event_study_forma_pretendencia_exposure.csv (chequeo B, lectura de datos ya existentes)
# - robustez_leave_one_year_out_exposure.csv (chequeo C)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
event_study_exposure_path <- file.path(out_dir, "..", "Estimacion_DiD", "event_study_completo_coeficientes.csv")
event_study_exposure_path <- file.path(paths$resultados, "Estimacion_DiD", "event_study_completo_coeficientes.csv")

if (!file.exists(panel_formal_path)) stop("Falta panel_establecimiento_formal.rds.")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_firma_eam.rds.")
if (!file.exists(event_study_exposure_path)) stop("Falta event_study_completo_coeficientes.csv. Corre estimacion/estimar_did_principal_empleo.R primero.")

OUTCOMES_FLIP <- c("empleo_temporal", "participacion_permanente")
FE_CON_CONTROLES <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"
ANIOS_PRE <- c(2015:2019, 2021, 2022)

panel_formal <- readr::read_rds(panel_formal_path)
exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

panel_built <- panel_formal %>%
  dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    post_2023 = as.integer(ANIO >= 2023),
    exposicion_10pp = Exposure2022_obreros / 0.1,
    anio_lineal_sq = anio_lineal^2
  )

extraer_post <- function(modelo) {
  ct <- summary(modelo)$coeftable
  fila_post <- grep("post_2023", rownames(ct))
  if (length(fila_post) != 1) stop("No se encontro (o se encontro mas de 1) coeficiente 'post_2023'.")
  list(estimate = unname(ct[fila_post, 1]), se = unname(ct[fila_post, 2]), p = unname(ct[fila_post, 4]))
}

# ------------------------------------------------------------------
# A) Tendencia CUADRATICA.
# ------------------------------------------------------------------

resultado_cuadratica <- purrr::map_dfr(OUTCOMES_FLIP, function(var_y) {
  f_cuad <- stats::as.formula(paste0(
    var_y, " ~ post_2023:exposicion_10pp + anio_lineal:exposicion_10pp + anio_lineal_sq:exposicion_10pp | ", FE_CON_CONTROLES
  ))
  m_cuad <- fixest::feols(f_cuad, data = panel_built, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  post <- extraer_post(m_cuad)
  tibble::tibble(
    variable = var_y,
    n_obs = stats::nobs(m_cuad),
    post_estimate = round(post$estimate, 6),
    post_se = round(post$se, 6),
    post_p = signif(post$p, 4),
    post_significativo_al_5pct = post$p < 0.05,
    signo = ifelse(post$estimate >= 0, "+", "-")
  )
})

readr::write_csv(resultado_cuadratica, file.path(out_dir, "robustez_tendencia_cuadratica_exposure.csv"))

# ------------------------------------------------------------------
# B) Forma del event-study PRE (lectura de datos ya existentes, ref=2022,
#    con controles, ventana completa) -- inspeccion de linealidad.
# ------------------------------------------------------------------

event_study <- readr::read_csv(event_study_exposure_path, show_col_types = FALSE) %>%
  dplyr::filter(variable %in% OUTCOMES_FLIP) %>%
  dplyr::arrange(variable, anio) %>%
  dplyr::select(variable, anio, estimate, std.error, p.value, conf.low, conf.high)

readr::write_csv(event_study, file.path(out_dir, "robustez_event_study_forma_pretendencia_exposure.csv"))

# Chequeo simple de no-linealidad: para el tramo PRE (2015-2021, ref=2022),
# compara el coeficiente observado en cada anio contra el que predeciria
# una linea recta ajustada a esos mismos puntos (minimos cuadrados
# simple) -- reporta si hay un anio con residuo grande (posible quiebre).
chequeo_linealidad <- purrr::map_dfr(OUTCOMES_FLIP, function(var_y) {
  datos_pre <- event_study %>% dplyr::filter(variable == var_y, anio %in% c(2015:2019, 2021))
  ajuste <- stats::lm(estimate ~ anio, data = datos_pre)
  datos_pre$prediccion_lineal <- stats::predict(ajuste)
  datos_pre$residuo <- datos_pre$estimate - datos_pre$prediccion_lineal
  datos_pre$variable <- var_y
  datos_pre %>% dplyr::select(variable, anio, estimate, prediccion_lineal, residuo)
})

readr::write_csv(chequeo_linealidad, file.path(out_dir, "robustez_chequeo_linealidad_pretendencia_exposure.csv"))

# ------------------------------------------------------------------
# C) LEAVE-ONE-YEAR-OUT, especificacion CON tendencia lineal.
# ------------------------------------------------------------------

resultado_loo <- purrr::map_dfr(OUTCOMES_FLIP, function(var_y) {
  purrr::map_dfr(ANIOS_PRE, function(anio_excluido) {
    datos_sin_anio <- panel_built %>% dplyr::filter(ANIO != anio_excluido)
    f_tend <- stats::as.formula(paste0(
      var_y, " ~ post_2023:exposicion_10pp + anio_lineal:exposicion_10pp | ", FE_CON_CONTROLES
    ))
    modelo <- tryCatch(
      fixest::feols(f_tend, data = datos_sin_anio, cluster = ~NORDEMP, warn = FALSE, notes = FALSE),
      error = function(e) NULL
    )
    if (is.null(modelo)) {
      return(tibble::tibble(variable = var_y, anio_excluido = anio_excluido, n_obs = NA_integer_,
                             post_estimate = NA_real_, post_se = NA_real_, post_p = NA_real_,
                             post_significativo_al_5pct = NA, nota = "modelo no convergio"))
    }
    post <- extraer_post(modelo)
    tibble::tibble(
      variable = var_y,
      anio_excluido = anio_excluido,
      n_obs = stats::nobs(modelo),
      post_estimate = round(post$estimate, 6),
      post_se = round(post$se, 6),
      post_p = signif(post$p, 4),
      post_significativo_al_5pct = post$p < 0.05,
      nota = NA_character_
    )
  })
})

readr::write_csv(resultado_loo, file.path(out_dir, "robustez_leave_one_year_out_exposure.csv"))

# ------------------------------------------------------------------
# Reporte en consola (solo numeros, sin interpretar)
# ------------------------------------------------------------------

script_header("validar_robustez_forma_funcional_tendencia_exposure.R -- Robustez de Exposure para empleo_temporal y participacion_permanente")
message("")
message("=== A) Tendencia CUADRATICA (coeficiente post_2023) ===")
print(resultado_cuadratica, n = Inf, width = Inf)
message("")
message("=== B) Secuencia del event-study (ref=2022, con controles) -- inspeccion de forma ===")
print(event_study, n = Inf, width = Inf)
message("")
message("=== B) Residuos frente a una linea recta ajustada al tramo PRE (2015-2021) ===")
print(chequeo_linealidad, n = Inf, width = Inf)
message("")
message("=== C) Leave-one-year-out (especificacion CON tendencia lineal) ===")
print(resultado_loo %>% dplyr::select(variable, anio_excluido, post_estimate, post_p, post_significativo_al_5pct), n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
