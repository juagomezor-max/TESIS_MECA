# 05_descriptivos.R -- Pipeline simplificado (rama `simplificacion`).
#
# Denominadores y descriptivos a nivel FIRMA que sustentan las cifras
# reportadas (para CIFRAS_CLAVE.csv). Los denominadores a nivel
# ESTABLECIMIENTO (6,775 / 6,761) se calculan en opcional_establecimiento.R.
#
# DERIVADO de (no reimplementado de memoria):
# - descriptivos_multiplanta_2022_peso_firmas_empleo.csv (firmas totales
#   2022, ya reproducido identico en 03_construir_panel.R: 6,186).
# - descriptivos_comparacion_exposure_establecimiento_vs_firma_2022.csv
#   (firmas con Exposure2022_obreros valida en 2022: 6,180).
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - simplificado_denominadores_firma.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_path <- file.path(data_dir, "panel_analitico_firma_eam.rds")
exposicion_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(panel_path)) stop("Falta panel_analitico_firma_eam.rds. Corre 03_construir_panel.R primero.")
if (!file.exists(exposicion_path)) stop("Falta exposicion_firma_eam.rds. Corre 02_construir_exposicion.R primero.")

panel_analitico <- readr::read_rds(panel_path)
exposicion_firma <- readr::read_rds(exposicion_path)

n_firmas_2022 <- panel_analitico %>% dplyr::filter(ANIO == 2022) %>% dplyr::summarise(n = dplyr::n_distinct(NORDEMP)) %>% dplyr::pull(n)
n_firmas_exposicion_valida <- sum(!is.na(exposicion_firma$Exposure2022_obreros))

denominadores <- tibble::tibble(
  metrica = c(
    "Firmas del panel 2022 (todas)",
    "Firmas con Exposure2022_obreros valida en 2022"
  ),
  valor = c(n_firmas_2022, n_firmas_exposicion_valida)
)

readr::write_csv(denominadores, file.path(out_dir, "simplificado_denominadores_firma.csv"))

script_header("05_descriptivos.R -- Denominadores a nivel firma")
message("")
print(denominadores, width = Inf)
message("")
message("(Los denominadores a nivel ESTABLECIMIENTO -- 6,775 / 6,761 -- se calculan en opcional_establecimiento.R)")
message("")
message("Tabla exportada en: ", out_dir)
