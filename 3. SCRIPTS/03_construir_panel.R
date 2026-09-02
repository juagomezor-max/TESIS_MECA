# 03_construir_panel.R -- Pipeline simplificado (rama `simplificacion`).
#
# Ensambla el panel analitico de FIRMA-ANIO: ventana 2015-2019 + 2021-2022
# (pre-choque) + 2023-2024 (post-choque), 2020 excluido, con las 4
# variables de resultado y las medidas de exposicion de 02 unidas.
#
# DERIVADO de (no reimplementado de memoria):
# - Ventana del panel: 2015-2019+2021-2022 (pre) + 2023-2024 (post), 2020
#   excluido -- confirmada en notas_panel_establecimiento.md Paso 2.6,
#   consolidando notas_exposicion_obreros_eam.md (pendiente Paso 7) y
#   diagnostico_preliminar_tendencias_2015_2019.R (exclusion de 2020 por
#   pandemia).
# - Variables de resultado (empleo_total/permanente/temporal/participacion_permanente):
#   misma definicion que validar_tendencias_paralelas_empleo_exposure_grafico.R
#   y validar_tendencias_paralelas_empleo_bite.R.
# - tamano_empresa (Pequena<50, Mediana<200, Grande>=200 por empleo_total):
#   mismos umbrales que investigar_validez_test_pretendencias.R.
#
# Salidas (no versionadas, 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/):
# - panel_analitico_firma_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

panel_path <- file.path(data_dir, "panel_firma_eam.rds")
exposicion_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(panel_path)) stop("Falta panel_firma_eam.rds. Corre 01_construir_base.R primero.")
if (!file.exists(exposicion_path)) stop("Falta exposicion_firma_eam.rds. Corre 02_construir_exposicion.R primero.")

panel_firma <- readr::read_rds(panel_path)
exposicion_firma <- readr::read_rds(exposicion_path)

PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)

safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

tamano_de <- function(empleo) {
  dplyr::case_when(
    is.na(empleo) ~ NA_character_,
    empleo < 50 ~ "Pequena",
    empleo < 200 ~ "Mediana",
    TRUE ~ "Grande"
  )
}

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_permanente <- c("C4R2C1", "C4R2C2", "C4R2C3", "C4R2C4", "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E")
cols_temporal <- c(
  "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E", "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"
)

panel_ventana <- panel_firma %>%
  dplyr::filter(ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::mutate(
    empleo_total = rowSums(dplyr::across(dplyr::all_of(c(cols_obreros, cols_administrativos, cols_prof_tecnico))), na.rm = TRUE),
    empleo_permanente = rowSums(dplyr::across(dplyr::all_of(cols_permanente)), na.rm = TRUE),
    empleo_temporal = rowSums(dplyr::across(dplyr::all_of(cols_temporal)), na.rm = TRUE),
    participacion_permanente = safe_divide(empleo_permanente, empleo_total) * 100,
    tamano_empresa = tamano_de(empleo_total)
  ) %>%
  dplyr::select(NORDEMP, ANIO, CIIU4, DPTO, tamano_empresa, empleo_total, empleo_permanente, empleo_temporal, participacion_permanente)

panel_analitico <- panel_ventana %>%
  dplyr::left_join(
    exposicion_firma %>% dplyr::select(NORDEMP, Exposure2022_obreros, quintil_exposure2022_obreros, Bite2022_obreros, quintil_bite2022_obreros),
    by = "NORDEMP"
  ) %>%
  dplyr::mutate(
    post_2023 = as.integer(ANIO >= 2023),
    anio_lineal = ANIO - min(PANEL_ANIOS_FINAL),
    ANIO_F = factor(ANIO),
    CIIU4 = factor(CIIU4),
    DPTO = factor(DPTO),
    tamano_empresa = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")),
    exposicion_10pp = Exposure2022_obreros / 0.1
  )

readr::write_rds(panel_analitico, file.path(data_dir, "panel_analitico_firma_eam.rds"))
readr::write_csv(panel_analitico, file.path(data_dir, "panel_analitico_firma_eam.csv"))

script_header("03_construir_panel.R -- Panel analitico de firma (2015-2019+2021-2022+2023-2024)")
message("")
message("Filas NORDEMP-ANIO: ", nrow(panel_analitico))
message("Firmas unicas: ", dplyr::n_distinct(panel_analitico$NORDEMP))
message("")
print(panel_analitico %>% dplyr::count(ANIO), n = Inf)
message("")
message("Panel exportado en: ", file.path(data_dir, "panel_analitico_firma_eam.rds"))
