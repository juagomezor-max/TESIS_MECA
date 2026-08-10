# Escanea TODAS las columnas de la macrobase EAM (no solo las de personal)
# buscando pares de nombres que probablemente representen la misma
# variable con variaciones de escritura entre anios (guion bajo,
# mayusculas/minusculas, sufijos distintos), y variables con cobertura
# parcial que podrian ser continuacion/discontinuacion de otra.
#
# Motivacion: al construir los diagnosticos del Paso 6 se detecto que
# "CIIU3" (sector, CIIU Revision 3) tiene datos SOLO 2008-2011 y quedo en
# 0% desde 2012, mientras que el sector de 2012 en adelante vive en una
# columna distinta ("CIIU4", y en 2013 especificamente como "CIIU_4", con
# guion bajo). Este script generaliza esa verificacion a las 398 columnas
# de la macrobase para detectar si hay mas casos parecidos sin corregirlos
# todavia (eso se evalua caso por caso, no automaticamente).
#
# IMPORTANTE: CIIU3 y CIIU4 NO son directamente comparables entre si. No
# es un simple cambio de nombre de la misma variable: son dos revisiones
# distintas de la Clasificacion Industrial Internacional Uniforme, con
# categorias y codigos que no tienen una correspondencia 1 a 1 exacta. Si
# en el futuro se necesita analizar sector de forma longitudinal en todo
# el panel 2008-2024, CIIU3 (2008-2011) y CIIU4 (2012-2024) deben tratarse
# como dos variables categoricas distintas, no concatenarse como si fueran
# una sola serie continua.
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/cobertura_todas_variables_macrobase.csv
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/candidatos_columnas_renombradas.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "purrr", "tibble", "readr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

macro_path <- paths$macro_base_eam
if (!file.exists(macro_path)) {
  stop("No se encontro la macrobase EAM en: ", macro_path)
}

macro_base <- readr::read_rds(macro_path)
names(macro_base) <- toupper(names(macro_base))
macro_base <- macro_base %>% dplyr::mutate(ANIO = suppressWarnings(as.integer(ANIO)))

meta_cols <- c("FUENTE", "ANIO", "ARCHIVO_ORIGEN", "NORDEMP")
data_cols <- setdiff(names(macro_base), meta_cols)

# ------------------------------------------------------------------
# 1) Cobertura (% no faltante) de cada variable, por anio, y resumen de
#    primer/ultimo anio con datos.
# ------------------------------------------------------------------

presencia <- purrr::map_dfr(data_cols, function(v) {
  tibble::tibble(variable = v, anio = macro_base$ANIO, no_na = !is.na(macro_base[[v]]))
}) %>%
  dplyr::group_by(variable, anio) %>%
  dplyr::summarise(pct_no_na = round(100 * mean(no_na), 1), .groups = "drop")

cobertura_var <- presencia %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    anios_con_datos = sum(pct_no_na > 0),
    primer_anio = if (any(pct_no_na > 0)) min(anio[pct_no_na > 0]) else NA_integer_,
    ultimo_anio = if (any(pct_no_na > 0)) max(anio[pct_no_na > 0]) else NA_integer_,
    .groups = "drop"
  )

readr::write_csv(cobertura_var, file.path(data_output_dir, "cobertura_todas_variables_macrobase.csv"))

# ------------------------------------------------------------------
# 2) Candidatos a "misma variable, nombre distinto": columnas que
#    colapsan al mismo nombre al quitar guiones bajos.
# ------------------------------------------------------------------

norm_sin_guion <- toupper(gsub("_", "", data_cols))

candidatos_guion_bajo <- tibble::tibble(variable = data_cols, norm = norm_sin_guion) %>%
  dplyr::group_by(norm) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(cobertura_var, by = "variable") %>%
  dplyr::arrange(norm) %>%
  dplyr::mutate(tipo_candidato = "colapsa al mismo nombre sin guion bajo")

# ------------------------------------------------------------------
# 3) Candidatos adicionales: variables con cobertura PARCIAL que
#    comparten raiz alfabetica (mismo prefijo antes de digitos/guion
#    final) con otra variable tambien parcial. Se reportan para revision
#    manual, NO se asumen equivalentes automaticamente.
# ------------------------------------------------------------------

raiz <- function(x) toupper(gsub("[0-9_]+$", "", x))

candidatos_raiz <- cobertura_var %>%
  dplyr::mutate(raiz = raiz(variable)) %>%
  dplyr::filter(anios_con_datos > 0, anios_con_datos < length(unique(macro_base$ANIO))) %>%
  dplyr::group_by(raiz) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(raiz, primer_anio) %>%
  dplyr::mutate(tipo_candidato = "comparte raiz alfabetica, cobertura parcial")

candidatos <- dplyr::bind_rows(
  candidatos_guion_bajo %>% dplyr::select(variable, tipo_candidato, anios_con_datos, primer_anio, ultimo_anio),
  candidatos_raiz %>% dplyr::select(variable, tipo_candidato, anios_con_datos, primer_anio, ultimo_anio)
) %>%
  dplyr::distinct()

readr::write_csv(candidatos, file.path(data_output_dir, "candidatos_columnas_renombradas.csv"))

script_header("Candidatos a columnas renombradas entre anios (macrobase completa)")
message("Columnas evaluadas: ", length(data_cols))
message("")
message("Candidatos por colapso de guion bajo:")
print(candidatos_guion_bajo %>% dplyr::select(variable, norm, anios_con_datos, primer_anio, ultimo_anio), n = Inf)
message("")
message("Candidatos por raiz alfabetica compartida (cobertura parcial):")
print(candidatos_raiz %>% dplyr::select(variable, raiz, anios_con_datos, primer_anio, ultimo_anio), n = Inf)
message("")
message("Nota interpretativa:")
message("- CIIU3 (2008-2011) y CIIU4/CIIU_4 (2012-2024) son la misma variable conceptual")
message("  (sector) pero en DOS REVISIONES DISTINTAS de la clasificacion CIIU, no")
message("  directamente comparables entre si. CIIU_4 en 2013 es solo una variacion de")
message("  escritura de CIIU4 (mismo concepto, mismo anio de la revision 4).")
message("- C3R1* (Salario Integral, 2008-2019) ya se documento como discontinuado en el")
message("  Paso 2; no es una variable renombrada, sino un concepto de costo retirado.")
message("- C3R20* (2013-2024) es una fila de costo distinta (no personal), con cobertura")
message("  parcial propia; no esta relacionada con C3R1 ni con las categorias ocupacionales")
message("  usadas en este trabajo.")
message("")
message("Tablas exportadas en: ", data_output_dir)
