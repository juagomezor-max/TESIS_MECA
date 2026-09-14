# exportar_paneles_manuales_1datos.R
#
# Genera las copias de trabajo de los 2 paneles principales que viven en
# la nueva "1. DATOS/" (raiz del repositorio, fuera de esta carpeta
# archivada, gitignorada) para el analisis manual de Julio -- ver
# "1. DATOS/README.md" y "1. DATOS/CODEBOOK.md" en la raiz del repo.
#
# NO MODIFICA los .rds originales dentro de esta carpeta archivada (los
# lee, los enriquece con columnas unidas de otras fuentes ya validadas, y
# escribe COPIAS en la nueva "1. DATOS/"). Formatos de salida: .rds, .csv,
# .dta (via haven).
#
# Enriquecimiento respecto a los .rds originales:
# - panel_analitico_firma_eam.rds: agrega Multi_f y las 4 variables de
#   mecanismo (C3R23C3, C3R41C3, C7R10C2, VALORVEN), agregadas NORDEMP-ANIO
#   con la regla ya auditada de pipeline/01_construir_base.R (suma si hay
#   al menos 1 establecimiento no-NA, si no NA) -- ya tenia Exposure2022_
#   obreros/Bite2022_obreros/quintiles desde 03_construir_panel.R.
# - panel_establecimiento_formal.rds: agrega Exposure2022_obreros/
#   Bite2022_obreros (firma, heredados por NORDEMP), Exposure2022_obreros_est
#   (recalculada fresca a nivel establecimiento), Multi_f, y las 4
#   variables de mecanismo DIRECTAS (ya son NORDEST-ANIO, sin agregar).
#
# DERIVADO de (no reimplementado de memoria):
# - Multi_f (262 firmas, corte 2022): pipeline/opcional_establecimiento.R,
#   re-derivado identico en estimacion/estimar_especificacion_a_establecimiento.R.
# - Exposure2022_obreros_est: construccion/construir_exposicion_obreros_
#   establecimiento_eam.R, re-derivado identico en estimacion/estimar_
#   especificacion_a_establecimiento.R.
# - Agregacion NORDEMP-ANIO de variables de mecanismo: estimacion/
#   estimar_mecanismos_ajuste_firma.R.
#
# Ejecutar con cwd = esta carpeta ("0. ANALISIS INICIAL IA/") -- ver nota
# tecnica de renv en "0. ANALISIS INICIAL IA/README.md".

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))
load_project_packages(c("dplyr", "readr", "tibble", "haven"))
paths <- ensure_project_structure()

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

destino <- "../1. DATOS"
if (!dir.exists(destino)) stop("No existe la carpeta destino ../1. DATOS -- crearla primero.")

# ------------------------------------------------------------------
# 0) Multi_f (262 firmas, corte 2022).
# ------------------------------------------------------------------

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))

base_2022_multi <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(ANIO == 2022, !is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "") %>%
  dplyr::distinct(NORDEST, NORDEMP)
n_est_por_firma_2022 <- base_2022_multi %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_por_firma_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)
multi_f_tabla <- tibble::tibble(NORDEMP = unique(macro_base$NORDEMP) %>% as.character()) %>%
  dplyr::mutate(Multi_f = as.integer(NORDEMP %in% firmas_262))
message("Multi_f: ", length(firmas_262), " firmas (esperado 262).")

# ------------------------------------------------------------------
# 1) Variables de mecanismo, crudas (sin asinh).
# ------------------------------------------------------------------

cols_mecanismo <- c("C3R23C3", "C3R41C3", "C7R10C2", "VALORVEN")
PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)

mecanismo_base <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO), ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, dplyr::all_of(cols_mecanismo)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_mecanismo), safe_numeric))

mecanismo_establecimiento <- mecanismo_base %>% dplyr::distinct(NORDEST, ANIO, .keep_all = TRUE) %>% dplyr::select(-NORDEMP)

mecanismo_firma <- mecanismo_base %>%
  dplyr::select(-NORDEST) %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(cols_mecanismo), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)), .groups = "drop")

# ------------------------------------------------------------------
# 2) Exposure2022_obreros_est.
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_necesarias <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico))

exposure_est_2022 <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", ANIO == 2022) %>%
  dplyr::select(NORDEST, dplyr::all_of(cols_necesarias)) %>%
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

# ------------------------------------------------------------------
# 3) Exposure2022_obreros / Bite2022_obreros a nivel firma.
# ------------------------------------------------------------------

exposicion_firma <- readr::read_rds(file.path(paths$bases_derivadas_exposicion, "exposicion_firma_eam.rds")) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros)

# ------------------------------------------------------------------
# 4) Panel de FIRMA -- enriquecido, exportado en 3 formatos.
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path(paths$bases_derivadas_exposicion, "panel_analitico_firma_eam.rds")) %>%
  dplyr::left_join(multi_f_tabla, by = "NORDEMP") %>%
  dplyr::left_join(mecanismo_firma, by = c("NORDEMP", "ANIO"))

readr::write_rds(panel_firma, file.path(destino, "panel_analitico_firma_eam.rds"))
readr::write_csv(panel_firma, file.path(destino, "panel_analitico_firma_eam.csv"))
haven::write_dta(panel_firma %>% dplyr::mutate(dplyr::across(where(is.factor), as.character)), file.path(destino, "panel_analitico_firma_eam.dta"))

# ------------------------------------------------------------------
# 5) Panel de ESTABLECIMIENTO -- enriquecido, exportado en 3 formatos.
# ------------------------------------------------------------------

panel_est <- readr::read_rds(file.path(paths$bases_derivadas_exposicion, "panel_establecimiento_formal.rds")) %>%
  dplyr::left_join(exposicion_firma, by = "NORDEMP") %>%
  dplyr::left_join(exposure_est_2022, by = "NORDEST") %>%
  dplyr::left_join(multi_f_tabla, by = "NORDEMP") %>%
  dplyr::left_join(mecanismo_establecimiento, by = c("NORDEST", "ANIO"))

readr::write_rds(panel_est, file.path(destino, "panel_establecimiento_formal.rds"))
readr::write_csv(panel_est, file.path(destino, "panel_establecimiento_formal.csv"))
haven::write_dta(panel_est %>% dplyr::mutate(dplyr::across(where(is.factor), as.character)), file.path(destino, "panel_establecimiento_formal.dta"))

script_header("exportar_paneles_manuales_1datos.R -- paneles enriquecidos exportados a 1. DATOS/")
message("")
message("panel_analitico_firma_eam: ", nrow(panel_firma), " filas, ", ncol(panel_firma), " columnas")
message("panel_establecimiento_formal: ", nrow(panel_est), " filas, ", ncol(panel_est), " columnas")
message("")
message("Exportado en: ", normalizePath(destino))
