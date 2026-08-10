# Construye Exposure2022_obreros: proxy de exposicion al choque de salario
# minimo de 2023 basada en la participacion de obreros y operarios en el
# empleo total de la firma, medida en la linea base pre-choque.
#
# NO reemplaza a Exposure2022 (definida en descriptivo_exposicion_eam.R
# como el inverso del salario promedio general de la firma en 2022): esa
# columna se conserva intacta en su propio script/salida. Esta es una
# medida alternativa, pensada especificamente para capturar exposicion via
# composicion ocupacional (cuanto pesan los obreros, el grupo peor pagado
# segun el Paso 4) en vez de via nivel salarial agregado de la firma.
#
# El anio base se parametriza explicitamente: 2022 es el ultimo anio
# completo antes del fuerte incremento del salario minimo de 2023 (mismo
# criterio de linea base pre-choque que usa descriptivo_exposicion_eam.R
# para su Exposure2022).
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/exposicion_obreros_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

conteo_path <- file.path(data_output_dir, "conteo_personal_categoria_eam.rds")
if (!file.exists(conteo_path)) {
  stop(
    "No se encontro el conteo de personal por categoria. ",
    "Corre primero 3. SCRIPTS/construir_conteo_personal_categoria_eam.R (Paso 3)."
  )
}

# Parametro explicito del anio base: cambiar aqui, no en el cuerpo del script.
ANIO_BASE_EXPOSICION <- 2022

# Evita divisiones por cero y deja NA cuando el denominador no es usable
# (misma logica que descriptivo_exposicion_eam.R).
safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) {
    return(x)
  }
  limits <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limits[[1]]), limits[[2]])
}

make_quintiles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))

  if (length(valid) < 5 || dplyr::n_distinct(x[valid]) < 5) {
    message("No hay suficiente variacion para construir quintiles. Se devolvera NA.")
    return(out)
  }

  quint <- dplyr::ntile(x[valid], 5)
  labels <- c("Q1 - Muy baja", "Q2 - Baja", "Q3 - Media", "Q4 - Alta", "Q5 - Muy alta")
  out[valid] <- labels[quint]
  factor(out, levels = labels, ordered = TRUE)
}

conteo <- readr::read_rds(conteo_path)
check_required_vars(conteo, c("NORDEMP", "ANIO", "total_obreros", "empleo_total_categorias"))

if (!ANIO_BASE_EXPOSICION %in% unique(conteo$ANIO)) {
  stop("El anio base ", ANIO_BASE_EXPOSICION, " no esta presente en el conteo de personal.")
}

# ------------------------------------------------------------------
# Exposure2022_obreros = participacion de obreros en el empleo total de
# las 3 categorias (sin propietarios, ver Paso 3), medida en el anio base.
# ------------------------------------------------------------------

baseline <- conteo %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::mutate(
    participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias)
  ) %>%
  dplyr::transmute(
    NORDEMP,
    Exposure2022_obreros = winsorize(participacion_obreros_raw),
    quintil_exposure2022_obreros = make_quintiles(Exposure2022_obreros)
  )

# La exposicion es un atributo pre-choque de la firma: se asigna al panel
# completo (todos los anios) via left_join por NORDEMP, igual que
# baseline_2022/baseline_2011 en descriptivo_exposicion_eam.R.
exposicion_obreros <- conteo %>%
  dplyr::select(NORDEMP, ANIO) %>%
  dplyr::left_join(baseline, by = "NORDEMP") %>%
  dplyr::mutate(
    periodo_2023 = dplyr::case_when(
      ANIO %in% 2020:2022 ~ "Pre (2020-2022)",
      ANIO %in% 2023:2024 ~ "Post (2023-2024)",
      TRUE ~ NA_character_
    )
  )

readr::write_rds(exposicion_obreros, file.path(data_output_dir, "exposicion_obreros_eam.rds"))
readr::write_csv(exposicion_obreros, file.path(data_output_dir, "exposicion_obreros_eam.csv"))

# ------------------------------------------------------------------
# Chequeo rapido: correlacion con la Exposure2022 original (1/salario
# promedio), generada por descriptivo_exposicion_eam.R. Solo lectura; el
# diagnostico completo (histogramas, percentiles, por sector/tamano/DPTO)
# es el Paso 6.
# ------------------------------------------------------------------

script_header("Exposure2022_obreros construida")
message("Empresas en linea base ", ANIO_BASE_EXPOSICION, ": ", sum(!is.na(baseline$Exposure2022_obreros)))
message("Filas NORDEMP-ANIO en el panel: ", nrow(exposicion_obreros))

base_original_path <- file.path(data_output_dir, "base_reducida_exposicion_eam.rds")
if (file.exists(base_original_path)) {
  original <- readr::read_rds(base_original_path) %>%
    dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
    dplyr::distinct(NORDEMP, Exposure2022)

  comparacion <- baseline %>%
    dplyr::inner_join(original, by = "NORDEMP") %>%
    dplyr::filter(!is.na(Exposure2022_obreros), !is.na(Exposure2022))

  if (nrow(comparacion) >= 3) {
    correlacion <- cor(comparacion$Exposure2022_obreros, comparacion$Exposure2022, use = "complete.obs")
    message("")
    message(
      "Correlacion Exposure2022_obreros vs Exposure2022 (original, n=",
      nrow(comparacion), "): ", round(correlacion, 3)
    )
  }
} else {
  message("")
  message(
    "Nota: no se encontro base_reducida_exposicion_eam.rds todavia; ",
    "corre el flujo completo para poder comparar contra la Exposure2022 original."
  )
}

message("")
message("Base exportada en: ", file.path(data_output_dir, "exposicion_obreros_eam.rds"))
