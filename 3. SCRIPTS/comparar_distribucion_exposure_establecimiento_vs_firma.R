# Paso 3 (cierre) de feature/panel-establecimiento: distribucion
# comparativa de Exposure2022_obreros_est (nivel establecimiento) vs
# Exposure2022_obreros (nivel firma), mismo anio base (2022), en una
# sola tabla.
#
# Objetivo: responder si la dispersion de la exposicion es mayor a
# nivel planta que a nivel firma, y por cuanto -- insumo directo para
# decidir si vale la pena el diseño "dentro de firma" (ya se establecio
# en el Paso 3.2 que hay variacion sustancial ENTRE plantas de una
# misma firma; esta tabla mide la dispersion GENERAL de cada medida,
# no solo dentro de firmas multiplanta).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - descriptivos_comparacion_exposure_establecimiento_vs_firma_2022.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

ANIO_BASE <- 2022

exposicion_est_path <- file.path(data_dir, "exposicion_obreros_establecimiento_eam.rds")
exposicion_firma_path <- file.path(data_dir, "exposicion_obreros_eam.rds")

if (!file.exists(exposicion_est_path)) stop("Falta exposicion_obreros_establecimiento_eam.rds. Corre construir_exposicion_obreros_establecimiento_eam.R primero.")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_obreros_eam.rds. Corre construir_exposicion_obreros_eam.R primero.")

exposure_est <- readr::read_rds(exposicion_est_path) %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, Exposure2022_obreros_est) %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est)) %>%
  dplyr::pull(Exposure2022_obreros_est)

exposure_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros)) %>%
  dplyr::pull(Exposure2022_obreros)

describir <- function(x, etiqueta) {
  tibble::tibble(
    nivel = etiqueta,
    N = length(x),
    media = round(mean(x), 4),
    mediana = round(median(x), 4),
    DE = round(sd(x), 4),
    p10 = round(quantile(x, 0.10), 4),
    p25 = round(quantile(x, 0.25), 4),
    p50 = round(quantile(x, 0.50), 4),
    p75 = round(quantile(x, 0.75), 4),
    p90 = round(quantile(x, 0.90), 4),
    minimo = round(min(x), 4),
    maximo = round(max(x), 4),
    pct_exactamente_0 = round(100 * mean(x == 0), 3),
    pct_exactamente_1 = round(100 * mean(x == 1), 3)
  )
}

comparacion <- dplyr::bind_rows(
  describir(exposure_est, "Establecimiento (Exposure2022_obreros_est)"),
  describir(exposure_firma, "Firma (Exposure2022_obreros)")
)

# Razon de dispersion (establecimiento / firma), para responder
# explicitamente "por cuanto" es mayor la dispersion a nivel planta.
razon_DE <- round(comparacion$DE[1] / comparacion$DE[2], 3)
razon_IQR <- round((comparacion$p75[1] - comparacion$p25[1]) / (comparacion$p75[2] - comparacion$p25[2]), 3)
razon_rango_p10_p90 <- round((comparacion$p90[1] - comparacion$p10[1]) / (comparacion$p90[2] - comparacion$p10[2]), 3)

readr::write_csv(comparacion, file.path(out_dir, "descriptivos_comparacion_exposure_establecimiento_vs_firma_2022.csv"))

script_header("Distribucion comparativa: Exposure2022_obreros_est (establecimiento) vs Exposure2022_obreros (firma), 2022")

message("")
print(comparacion, n = Inf, width = Inf)

message("")
message("Razon de dispersion establecimiento/firma:")
message("  DE:            ", razon_DE, "x")
message("  IQR (p75-p25): ", razon_IQR, "x")
message("  Rango p90-p10: ", razon_rango_p10_p90, "x")

message("")
message("Tabla exportada en: ", out_dir)
