# Paso 3 (cierre) de feature/panel-establecimiento: compara la
# distribucion de Exposure2022_obreros_est ENTRE establecimientos de
# firmas multiplanta vs. establecimientos de firmas monoplanta (2022).
#
# Distinto de descriptivos_estructura_multiplanta_2022.R (que mide
# variacion INTERNA dentro de cada firma multiplanta, es decir
# heterogeneidad DENTRO del grupo). Aqui la pregunta es si el NIVEL de
# exposicion de las plantas de firmas multiplanta es sistematicamente
# distinto (mas alto o mas bajo en promedio) que el de plantas de firmas
# monoplanta -- heterogeneidad ENTRE los dos grupos, no dentro de uno.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - descriptivos_comparacion_exposure_multiplanta_vs_monoplanta_2022.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

ANIO_BASE <- 2022

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO"))

base_2022 <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP)

n_est_por_firma <- base_2022 %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")

firmas_multiplanta <- n_est_por_firma %>% dplyr::filter(n_establecimientos > 1) %>% dplyr::pull(NORDEMP)

exposicion_est_path <- file.path(data_dir, "exposicion_obreros_establecimiento_eam.rds")
if (!file.exists(exposicion_est_path)) stop("Falta exposicion_obreros_establecimiento_eam.rds.")

exposicion_est_2022 <- readr::read_rds(exposicion_est_path) %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP, Exposure2022_obreros_est) %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est)) %>%
  dplyr::mutate(grupo = ifelse(NORDEMP %in% firmas_multiplanta, "Multiplanta (planta de firma con 2+ establecimientos)", "Monoplanta (unica planta de la firma)"))

describir <- function(x, etiqueta, n_firmas) {
  tibble::tibble(
    grupo = etiqueta,
    n_establecimientos = length(x),
    n_firmas = n_firmas,
    media = round(mean(x), 4),
    mediana = round(median(x), 4),
    DE = round(sd(x), 4),
    p10 = round(quantile(x, 0.10), 4),
    p25 = round(quantile(x, 0.25), 4),
    p50 = round(quantile(x, 0.50), 4),
    p75 = round(quantile(x, 0.75), 4),
    p90 = round(quantile(x, 0.90), 4)
  )
}

grupo_multi <- exposicion_est_2022 %>% dplyr::filter(grupo == "Multiplanta (planta de firma con 2+ establecimientos)")
grupo_mono <- exposicion_est_2022 %>% dplyr::filter(grupo == "Monoplanta (unica planta de la firma)")

comparacion <- dplyr::bind_rows(
  describir(grupo_multi$Exposure2022_obreros_est, "Multiplanta", dplyr::n_distinct(grupo_multi$NORDEMP)),
  describir(grupo_mono$Exposure2022_obreros_est, "Monoplanta", dplyr::n_distinct(grupo_mono$NORDEMP))
)

# Diferencia de medias con prueba t (Welch, no asume varianzas iguales)
prueba_t <- t.test(grupo_multi$Exposure2022_obreros_est, grupo_mono$Exposure2022_obreros_est)

readr::write_csv(comparacion, file.path(out_dir, "descriptivos_comparacion_exposure_multiplanta_vs_monoplanta_2022.csv"))

script_header("Exposure2022_obreros_est: plantas de firmas multiplanta vs. monoplanta (2022)")

message("")
print(comparacion, n = Inf, width = Inf)

message("")
message("Diferencia de medias (multiplanta - monoplanta): ", round(comparacion$media[1] - comparacion$media[2], 4))
message("Prueba t (Welch): t = ", round(prueba_t$statistic, 3), ", df = ", round(prueba_t$parameter, 1), ", p-valor = ", signif(prueba_t$p.value, 4))
message("Intervalo de confianza 95% de la diferencia: [", round(prueba_t$conf.int[1], 4), ", ", round(prueba_t$conf.int[2], 4), "]")

message("")
message("Tabla exportada en: ", out_dir)
