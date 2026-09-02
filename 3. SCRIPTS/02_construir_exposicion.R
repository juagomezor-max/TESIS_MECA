# 02_construir_exposicion.R -- Pipeline simplificado (rama `simplificacion`).
#
# Construye, a nivel FIRMA (NORDEMP), las 2 medidas de exposicion pre-choque
# 2022: Exposure2022_obreros (composicion ocupacional) y Bite2022_obreros
# (indice de Kaitz). Formula de cada una documentada explicitamente abajo.
#
# DERIVADO de (no reimplementado de memoria):
# - construir_conteo_personal_categoria_eam.R: definicion de
#   total_obreros/administrativos/prof_tecnico y empleo_total_categorias
#   (sin propietarios).
# - construir_exposicion_obreros_eam.R: formula, winsorizacion (1%-99%) y
#   quintiles (ntile) de Exposure2022_obreros.
# - construir_salarios_promedio_categoria_eam.R + construir_bite_obreros_eam.R:
#   formula de Bite2022_obreros y verificacion de escala de C3R2C1 (anual,
#   miles de pesos -- confirmado empiricamente contra SM_2023 mensual vs.
#   anualizado, ver notas_exposicion_obreros_eam.md).
# - diagnosticos_validacion_bite_obreros_eam.R: correlacion Exposure-Bite
#   (Pearson/Spearman), para CIFRAS_CLAVE.csv.
#
# FORMULAS:
#   Exposure2022_obreros_f = total_obreros_f_2022 / empleo_total_categorias_f_2022
#     (participacion de obreros y operarios en el empleo total de la firma,
#     sin propietarios; winsorizada 1%-99%)
#   Bite2022_obreros_f = (SM_2023_mensual x 12 / 1000) / salario_promedio_obrero_f_2022
#     (SM_2023 = $1.160.000 COP/mes, Decreto 2613 de 2022; salario_promedio_obrero_f_2022
#     = C3R2C1 de esa firma en 2022, ya en miles de pesos anuales)
#
# Salidas (no versionadas, 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/):
# - exposicion_firma_eam.rds/.csv: NORDEMP, Exposure2022_obreros,
#   quintil_exposure2022_obreros, Bite2022_obreros, quintil_bite2022_obreros
#   (atributo pre-choque, constante, sin ANIO -- se une al panel en 03).
# - correlacion_exposure_bite.csv: Pearson/Spearman, para CIFRAS_CLAVE.csv.

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

panel_path <- file.path(data_dir, "panel_firma_eam.rds")
if (!file.exists(panel_path)) stop("Falta panel_firma_eam.rds. Corre 01_construir_base.R primero.")
panel_firma <- readr::read_rds(panel_path)

ANIO_BASE <- 2022
SM_2023_MENSUAL_COP <- 1160000  # Decreto 2613 de 2022, Ministerio del Trabajo
SM_2023_ANUAL_MILES <- SM_2023_MENSUAL_COP * 12 / 1000

safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

make_quintiles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))
  if (length(valid) < 5 || dplyr::n_distinct(x[valid]) < 5) return(out)
  quint <- dplyr::ntile(x[valid], 5)
  labels <- c("Q1 - Muy baja", "Q2 - Baja", "Q3 - Media", "Q4 - Alta", "Q5 - Muy alta")
  out[valid] <- labels[quint]
  factor(out, levels = labels, ordered = TRUE)
}

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)

baseline_2022 <- panel_firma %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::mutate(
    total_obreros = rowSums(dplyr::across(dplyr::all_of(cols_obreros)), na.rm = TRUE),
    total_administrativos = rowSums(dplyr::across(dplyr::all_of(cols_administrativos)), na.rm = TRUE),
    total_prof_tecnico = rowSums(dplyr::across(dplyr::all_of(cols_prof_tecnico)), na.rm = TRUE),
    empleo_total_categorias = total_obreros + total_administrativos + total_prof_tecnico,
    participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias),
    # salario_promedio_obrero = costo permanente (C3R2C1) / CONTEO PERMANENTE
    # de obreros (C4R2C1+C4R2C2) -- NO el total_obreros de arriba (que
    # incluye temporales/aprendices). Formula exacta de
    # construir_salarios_promedio_categoria_eam.R: numerador y denominador
    # deben medir al mismo grupo (personal permanente) o el salario
    # promedio queda subestimado.
    personal_permanente_obrero = C4R2C1 + C4R2C2,
    salario_promedio_obrero = safe_divide(C3R2C1, personal_permanente_obrero)
  ) %>%
  dplyr::transmute(
    NORDEMP,
    Exposure2022_obreros = winsorize(participacion_obreros_raw),
    Bite2022_obreros = safe_divide(SM_2023_ANUAL_MILES, salario_promedio_obrero)
  ) %>%
  dplyr::mutate(
    quintil_exposure2022_obreros = make_quintiles(Exposure2022_obreros),
    quintil_bite2022_obreros = make_quintiles(Bite2022_obreros)
  )

readr::write_rds(baseline_2022, file.path(data_dir, "exposicion_firma_eam.rds"))
readr::write_csv(baseline_2022, file.path(data_dir, "exposicion_firma_eam.csv"))

# ------------------------------------------------------------------
# Correlacion Exposure-Bite (para CIFRAS_CLAVE.csv).
# ------------------------------------------------------------------

datos_correlacion <- baseline_2022 %>%
  dplyr::filter(!is.na(Exposure2022_obreros), !is.na(Bite2022_obreros))

correlacion <- tibble::tibble(
  n = nrow(datos_correlacion),
  correlacion_pearson = round(cor(datos_correlacion$Exposure2022_obreros, datos_correlacion$Bite2022_obreros, method = "pearson"), 3),
  correlacion_spearman = round(cor(datos_correlacion$Exposure2022_obreros, datos_correlacion$Bite2022_obreros, method = "spearman"), 3)
)

readr::write_csv(correlacion, file.path(data_dir, "correlacion_exposure_bite.csv"))

script_header("02_construir_exposicion.R -- Exposure2022_obreros y Bite2022_obreros (firma)")
message("")
message("Firmas con Exposure2022_obreros valida en 2022: ", sum(!is.na(baseline_2022$Exposure2022_obreros)))
message("Firmas con Bite2022_obreros valida en 2022: ", sum(!is.na(baseline_2022$Bite2022_obreros)))
message("")
message("Correlacion Exposure-Bite:")
print(correlacion, width = Inf)
message("")
message("Bases exportadas en: ", data_dir)
