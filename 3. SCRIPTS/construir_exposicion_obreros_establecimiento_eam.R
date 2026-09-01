# Construye Exposure2022_obreros_est: version a nivel de ESTABLECIMIENTO
# de Exposure2022_obreros (participacion de obreros y operarios en el
# empleo total de la firma, ver construir_exposicion_obreros_eam.R).
#
# Diferencia clave frente a la version a nivel empresa: usa la
# composicion ocupacional PROPIA de cada establecimiento en el anio base
# 2022 (total_obreros del NORDEST / empleo_total_categorias del NORDEST),
# NO la exposicion heredada de la firma (NORDEMP) a la que pertenece.
# Relevante especificamente para las 447 firmas multiplanta identificadas
# en el Paso 1.5 de esta rama (auditar_recodificacion_multiplanta_nordest.R):
# sus establecimientos pueden tener composiciones ocupacionales muy
# distintas entre si, que la version a nivel empresa promediaria/sumaria
# y por lo tanto ocultaria.
#
# Se llama `Exposure2022_obreros_est` (sufijo `_est`) para no confundirla
# con `Exposure2022_obreros` (nivel empresa, ya en
# exposicion_obreros_eam.rds). Misma formula, mismo anio base, mismo
# criterio de winsorizacion y quintiles que la version a nivel empresa,
# para que ambas sean comparables metodologicamente.
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/exposicion_obreros_establecimiento_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

conteo_path <- file.path(data_output_dir, "conteo_personal_categoria_establecimiento_eam.rds")
if (!file.exists(conteo_path)) {
  stop(
    "No se encontro el conteo de personal por categoria a nivel establecimiento. ",
    "Corre primero 3. SCRIPTS/construir_conteo_personal_categoria_establecimiento_eam.R."
  )
}

# Parametro explicito del anio base: igual al usado a nivel empresa.
ANIO_BASE_EXPOSICION <- 2022

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
check_required_vars(conteo, c("NORDEST", "NORDEMP", "ANIO", "total_obreros", "empleo_total_categorias"))

if (!ANIO_BASE_EXPOSICION %in% unique(conteo$ANIO)) {
  stop("El anio base ", ANIO_BASE_EXPOSICION, " no esta presente en el conteo de personal por establecimiento.")
}

# ------------------------------------------------------------------
# Exposure2022_obreros_est = participacion de obreros en el empleo total
# de las 3 categorias del ESTABLECIMIENTO (sin propietarios), medida en
# el anio base.
# ------------------------------------------------------------------

baseline <- conteo %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::mutate(
    participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias)
  ) %>%
  dplyr::transmute(
    NORDEST,
    Exposure2022_obreros_est = winsorize(participacion_obreros_raw),
    quintil_exposure2022_obreros_est = make_quintiles(Exposure2022_obreros_est)
  )

# Atributo pre-choque del establecimiento: se asigna al panel completo
# (todos los anios) via left_join por NORDEST, igual que la version a
# nivel empresa.
exposicion_establecimiento <- conteo %>%
  dplyr::select(NORDEST, NORDEMP, ANIO) %>%
  dplyr::left_join(baseline, by = "NORDEST") %>%
  dplyr::mutate(
    periodo_2023 = dplyr::case_when(
      ANIO %in% 2020:2022 ~ "Pre (2020-2022)",
      ANIO %in% 2023:2024 ~ "Post (2023-2024)",
      TRUE ~ NA_character_
    )
  )

readr::write_rds(exposicion_establecimiento, file.path(data_output_dir, "exposicion_obreros_establecimiento_eam.rds"))
readr::write_csv(exposicion_establecimiento, file.path(data_output_dir, "exposicion_obreros_establecimiento_eam.csv"))

# ------------------------------------------------------------------
# Chequeo rapido: correlacion con Exposure2022_obreros a nivel empresa
# (heredada por todos los establecimientos de esa firma). Solo lectura;
# el descriptivo completo de estructura multiplanta es el resto del
# Paso 3.
# ------------------------------------------------------------------

script_header("Exposure2022_obreros_est construida (nivel establecimiento)")
message("Establecimientos con Exposure2022_obreros_est valida en ", ANIO_BASE_EXPOSICION, ": ", sum(!is.na(baseline$Exposure2022_obreros_est)))
message("Filas NORDEST-ANIO en el panel: ", nrow(exposicion_establecimiento))

exposicion_empresa_path <- file.path(data_output_dir, "exposicion_obreros_eam.rds")
if (file.exists(exposicion_empresa_path)) {
  exposicion_empresa <- readr::read_rds(exposicion_empresa_path) %>%
    dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
    dplyr::distinct(NORDEMP, Exposure2022_obreros)

  comparacion <- exposicion_establecimiento %>%
    dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
    dplyr::distinct(NORDEST, NORDEMP, Exposure2022_obreros_est) %>%
    dplyr::inner_join(exposicion_empresa, by = "NORDEMP") %>%
    dplyr::filter(!is.na(Exposure2022_obreros_est), !is.na(Exposure2022_obreros))

  if (nrow(comparacion) >= 3) {
    correlacion <- cor(comparacion$Exposure2022_obreros_est, comparacion$Exposure2022_obreros, use = "complete.obs")
    message("")
    message(
      "Correlacion Exposure2022_obreros_est (establecimiento) vs Exposure2022_obreros (empresa, heredada), n=",
      nrow(comparacion), ": ", round(correlacion, 3)
    )
  }
} else {
  message("")
  message(
    "Nota: no se encontro exposicion_obreros_eam.rds todavia; ",
    "corre construir_exposicion_obreros_eam.R (rama feature/exposicion-obreros-operarios, ya en main) para poder comparar."
  )
}

message("")
message("Base exportada en: ", file.path(data_output_dir, "exposicion_obreros_establecimiento_eam.rds"))
