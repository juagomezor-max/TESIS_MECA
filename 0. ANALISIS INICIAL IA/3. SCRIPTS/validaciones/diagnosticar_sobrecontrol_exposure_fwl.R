# diagnosticar_sobrecontrol_exposure_fwl.R
#
# Diagnostico de SOBRE-CONTROL (bad control), motivado por un hallazgo
# sospechoso de validar_primer_eslabon_costo_laboral.R: el vinculo entre
# Exposure2022_obreros y el choque de costo laboral 2023 desaparece POR
# COMPLETO al agregar sector(CIIU4)*anio + tamano_empresa*anio +
# departamento(DPTO_fijo)*anio -- pese a ser un vinculo casi MECANICO
# (mas obreros expuestos al salario minimo = mas nomina cuando el
# minimo sube, si no hay despidos). Es sospechoso que un vinculo tan
# directo se anule del todo. HIPOTESIS A PROBAR (no confirmada aqui,
# solo se reportan los numeros): los 3 controles no estan absorbiendo
# un confusor externo a Exposure2022_obreros, estan absorbiendo la
# EXPOSICION MISMA, porque Exposure2022_obreros esta fuertemente
# correlacionada con sector y tamano (un "bad control": condicionar
# sobre una variable que es en si misma parte del mecanismo o esta
# co-determinada con el tratamiento).
#
# Este script NO responde la pregunta de si el resultado del primer
# eslabon queda invalidado -- solo cuantifica cuanta varianza de
# Exposure2022_obreros (y de Exposure2022_obreros_est) sobrevive
# despues de los 3 controles, via Frisch-Waugh-Lovell: si se remueve la
# parte de Exposure explicada por sector+tamano+departamento (residuo
# de regresionar Exposure sobre esos FE), la varianza restante (1-R2)
# es la que realmente identifica los coeficientes de exposicion en las
# especificaciones "con controles" de este proyecto. La interpretacion
# (si esto invalida o no el resultado del primer eslabon) se hace en
# una conversacion posterior con estos numeros en mano -- no se asume
# aqui.
#
# METODO: sobre el panel formal, corte transversal 2022 (Exposure2022_obreros
# y Exposure2022_obreros_est son invariantes en el tiempo -- no varian
# por ANIO, asi que no tiene sentido interactuar con ANIO_F ni usar el
# panel completo; un corte de 2022 basta y evita pesar el resultado por
# cuantos anios aparece cada establecimiento).
#   1. feols(Exposure ~ 1 | CIIU4 + tamano_empresa + DPTO_fijo) -- R2 y 1-R2.
#   2. Lo mismo con cada control por separado (solo CIIU4, solo
#      tamano_empresa, solo DPTO_fijo).
#   3. Exposure2022_obreros promedio por CIIU4, ordenado de mayor a
#      menor, para ver si la concentracion sectorial es extrema.
#   4. Se repiten 1-3 para Exposure2022_obreros_est.
#
# DERIVADO de (no reimplementado de memoria):
# - Exposure2022_obreros (firma) y su union por NORDEMP: identico a
#   validaciones/validar_primer_eslabon_costo_laboral.R.
# - Exposure2022_obreros_est (recalculada, no persistida) y su union
#   directa por NORDEST: identico a
#   validaciones/validar_primer_eslabon_costo_laboral_exposure_est.R.
# - CIIU4/tamano_empresa/DPTO_fijo: columnas ya construidas en
#   panel_establecimiento_formal.rds (Paso A).
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - diagnostico_sobrecontrol_r2.csv (R2/1-R2, 4 modelos x 2 medidas de exposicion)
# - diagnostico_sobrecontrol_exposure_por_ciiu4.csv (Exposure2022_obreros promedio por sector)
# - diagnostico_sobrecontrol_exposure_est_por_ciiu4.csv (idem, Exposure2022_obreros_est)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "fixest", "purrr")
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

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

# ------------------------------------------------------------------
# 1) Corte 2022 del panel formal (CIIU4, tamano_empresa, DPTO_fijo).
# ------------------------------------------------------------------

corte_2022 <- readr::read_rds(panel_formal_path) %>%
  dplyr::filter(ANIO == 2022) %>%
  dplyr::distinct(NORDEST, NORDEMP, CIIU4, tamano_empresa, DPTO_fijo)

# ------------------------------------------------------------------
# 2) Exposure2022_obreros (firma, ya construida) y Exposure2022_obreros_est
#    (establecimiento, recalculada -- no persistida, mismo patron ya
#    usado en validar_primer_eslabon_costo_laboral_exposure_est.R).
# ------------------------------------------------------------------

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_necesarias <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "ANIO", cols_necesarias))

exposicion_est <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", ANIO == 2022) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_necesarias), safe_numeric)) %>%
  dplyr::mutate(
    total_obreros = rowSums(dplyr::across(dplyr::all_of(cols_obreros)), na.rm = TRUE),
    total_administrativos = rowSums(dplyr::across(dplyr::all_of(cols_administrativos)), na.rm = TRUE),
    total_prof_tecnico = rowSums(dplyr::across(dplyr::all_of(cols_prof_tecnico)), na.rm = TRUE),
    empleo_total_categorias = total_obreros + total_administrativos + total_prof_tecnico,
    participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias)
  ) %>%
  dplyr::transmute(NORDEST, Exposure2022_obreros_est = winsorize(participacion_obreros_raw)) %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est))

datos_firma <- corte_2022 %>%
  dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(tamano_empresa), !is.na(DPTO_fijo))

datos_est <- corte_2022 %>%
  dplyr::inner_join(exposicion_est, by = "NORDEST") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(tamano_empresa), !is.na(DPTO_fijo))

# ------------------------------------------------------------------
# 3) R2 / 1-R2 de Exposure ~ 1 | controles (los 3 juntos, y cada uno
#    por separado). Sin interaccion con ANIO_F: Exposure es invariante
#    en el tiempo, es un corte 2022.
# ------------------------------------------------------------------

correr_r2 <- function(data, var_exposicion, medida) {
  especificaciones <- list(
    "CIIU4 + tamano_empresa + DPTO_fijo (los 3 juntos)" = "CIIU4 + tamano_empresa + DPTO_fijo",
    "Solo CIIU4" = "CIIU4",
    "Solo tamano_empresa" = "tamano_empresa",
    "Solo DPTO_fijo" = "DPTO_fijo"
  )
  purrr::map_dfr(names(especificaciones), function(nombre_espec) {
    fe <- especificaciones[[nombre_espec]]
    formula_modelo <- stats::as.formula(paste0(var_exposicion, " ~ 1 | ", fe))
    modelo <- fixest::feols(formula_modelo, data = data, warn = FALSE, notes = FALSE)
    r2_valor <- unname(fixest::r2(modelo, type = "r2"))
    tibble::tibble(
      medida_exposicion = medida,
      controles = nombre_espec,
      n_obs = stats::nobs(modelo),
      r2 = round(r2_valor, 4),
      uno_menos_r2 = round(1 - r2_valor, 4)
    )
  })
}

resultado_r2 <- dplyr::bind_rows(
  correr_r2(datos_firma, "Exposure2022_obreros", "Exposure2022_obreros (firma)"),
  correr_r2(datos_est, "Exposure2022_obreros_est", "Exposure2022_obreros_est (establecimiento)")
)

readr::write_csv(resultado_r2, file.path(out_dir, "diagnostico_sobrecontrol_r2.csv"))

# ------------------------------------------------------------------
# 4) Exposure promedio por CIIU4, ordenado de mayor a menor.
# ------------------------------------------------------------------

exposure_por_ciiu4 <- datos_firma %>%
  dplyr::group_by(CIIU4) %>%
  dplyr::summarise(
    n_firmas = dplyr::n_distinct(NORDEMP),
    exposure_promedio = round(mean(Exposure2022_obreros, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(exposure_promedio))

exposure_est_por_ciiu4 <- datos_est %>%
  dplyr::group_by(CIIU4) %>%
  dplyr::summarise(
    n_establecimientos = dplyr::n_distinct(NORDEST),
    exposure_est_promedio = round(mean(Exposure2022_obreros_est, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(exposure_est_promedio))

readr::write_csv(exposure_por_ciiu4, file.path(out_dir, "diagnostico_sobrecontrol_exposure_por_ciiu4.csv"))
readr::write_csv(exposure_est_por_ciiu4, file.path(out_dir, "diagnostico_sobrecontrol_exposure_est_por_ciiu4.csv"))

# ------------------------------------------------------------------
# Reporte en consola (solo numeros, sin interpretar)
# ------------------------------------------------------------------

script_header("diagnosticar_sobrecontrol_exposure_fwl.R -- R2 de Exposure ~ controles (Frisch-Waugh-Lovell)")
message("")
message("Corte 2022. Firmas (Exposure2022_obreros): ", dplyr::n_distinct(datos_firma$NORDEMP),
        " | Establecimientos (Exposure2022_obreros_est): ", dplyr::n_distinct(datos_est$NORDEST))
message("")
message("=== R2 / 1-R2 ===")
print(resultado_r2, n = Inf, width = Inf)
message("")
message("=== Exposure2022_obreros promedio por CIIU4 (top 10 y bottom 10 de ", nrow(exposure_por_ciiu4), " sectores) ===")
print(dplyr::bind_rows(head(exposure_por_ciiu4, 10), tail(exposure_por_ciiu4, 10)), n = Inf, width = Inf)
message("")
message("=== Exposure2022_obreros_est promedio por CIIU4 (top 10 y bottom 10 de ", nrow(exposure_est_por_ciiu4), " sectores) ===")
print(dplyr::bind_rows(head(exposure_est_por_ciiu4, 10), tail(exposure_est_por_ciiu4, 10)), n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
