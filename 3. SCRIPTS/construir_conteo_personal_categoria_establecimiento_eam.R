# Construye conteos de personal por categoria ocupacional (obreros y
# operarios, profesional-tecnico-tecnologo, directivos y administracion/
# ventas) a nivel NORDEST-ANIO (establecimiento), version espejo de
# construir_conteo_personal_categoria_eam.R (que opera a nivel NORDEMP).
#
# Mismas columnas C4R confirmadas estables 2008-2024 en
# verificar_estabilidad_columnas_c3r_c4r.R, misma definicion de
# categorias (propietarios excluidos de empleo_total_categorias, ver esa
# nota en la version a nivel empresa para el razonamiento completo).
#
# Diferencia clave frente a la version NORDEMP: NO hace falta
# group_by/summarise para deduplicar, porque el Paso 1 de
# feature/panel-establecimiento (auditar_confiabilidad_nordest.R) ya
# confirmo que NORDEST-ANIO es unico en el 100% de la macrobase
# (0 combinaciones duplicadas en los 17 anios). Cada fila de la
# macrobase filtrada por NORDEST/ANIO validos ya es un
# establecimiento-anio unico.
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/conteo_personal_categoria_establecimiento_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

macro_path <- paths$macro_base_eam
if (!file.exists(macro_path)) {
  stop("No se encontro la macrobase EAM en: ", macro_path)
}

sum_if_exists <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  safe_numeric <- function(x) suppressWarnings(as.numeric(x))

  out <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), safe_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(.sum = rowSums(dplyr::across(dplyr::everything()))) %>%
    dplyr::pull(.sum)

  all_missing <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), ~is.na(safe_numeric(.x)))) %>%
    dplyr::mutate(.all_missing = dplyr::if_all(dplyr::everything(), identity)) %>%
    dplyr::pull(.all_missing)

  out[all_missing] <- NA_real_
  out
}

macro_base <- readr::read_rds(macro_path)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO"))

# ------------------------------------------------------------------
# 1) Columnas por categoria ocupacional y tipo de vinculacion, EXCLUYENDO
#    la fila de propietarios (C4R1 sin sufijo N/E). Identicas a
#    construir_conteo_personal_categoria_eam.R.
# ------------------------------------------------------------------

cols_obreros <- c(
  "C4R2C1", "C4R2C2",   # Personal permanente
  "C4R3C1", "C4R3C2",   # Temporal directo
  "C4R4C1", "C4R4C2",   # Temporal agencia
  "C4R6OM", "C4R6OH"    # Aprendices y pasantes
)

cols_administrativos <- c(
  "C4R2C3", "C4R2C4",
  "C4R3C3", "C4R3C4",
  "C4R4C3", "C4R4C4",
  "C4R6DM", "C4R6DH"
)

cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",   # Personal permanente (nac/ext)
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",   # Temporal directo (nac/ext)
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",   # Temporal agencia (nac/ext)
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"        # Aprendices y pasantes (nac/ext)
)

cols_propietarios <- c(
  "C4R1C1", "C4R1C2",                             # Propietarios - Obreros
  "C4R1C3", "C4R1C4",                             # Propietarios - Directivos/Admin
  "C4R1C1N", "C4R1C2N", "C4R2C1E", "C4R2C2E"      # Propietarios - PT (nac/ext)
)

todas_las_cols <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico, cols_propietarios))
cols_presentes <- todas_las_cols[todas_las_cols %in% names(macro_base)]

if (length(cols_presentes) < length(todas_las_cols)) {
  faltantes <- setdiff(todas_las_cols, cols_presentes)
  stop(
    "Faltan columnas esperadas en la macrobase (revisar Paso 2 de feature/exposicion-obreros-operarios): ",
    paste(faltantes, collapse = ", ")
  )
}

# ------------------------------------------------------------------
# 2) Filtrado a NORDEST-ANIO validos. Sin group_by/summarise: NORDEST-ANIO
#    ya es unico (confirmado en el Paso 1 de esta rama), asi que cada
#    fila es directamente un establecimiento-anio.
# ------------------------------------------------------------------

panel <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, dplyr::all_of(cols_presentes)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_presentes), ~suppressWarnings(as.numeric(.x))))

n_dup <- panel %>% dplyr::count(NORDEST, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup > 0) {
  stop(
    "NORDEST-ANIO ya no es unico (", n_dup, " grupos duplicados). ",
    "Esto contradice el hallazgo del Paso 1 (auditar_confiabilidad_nordest.R) -- revisar antes de continuar."
  )
}

# ------------------------------------------------------------------
# 3) Conteos por categoria
# ------------------------------------------------------------------

panel_conteo <- panel %>%
  dplyr::mutate(
    total_obreros = sum_if_exists(., cols_obreros),
    total_administrativos = sum_if_exists(., cols_administrativos),
    total_prof_tecnico = sum_if_exists(., cols_prof_tecnico),
    total_propietarios = sum_if_exists(., cols_propietarios),
    empleo_total_categorias = total_obreros + total_administrativos + total_prof_tecnico,
    empleo_total_categorias_con_propietarios = empleo_total_categorias + total_propietarios
  ) %>%
  dplyr::select(
    NORDEST, NORDEMP, ANIO,
    total_obreros, total_prof_tecnico, total_administrativos, total_propietarios,
    empleo_total_categorias, empleo_total_categorias_con_propietarios
  )

readr::write_rds(panel_conteo, file.path(data_output_dir, "conteo_personal_categoria_establecimiento_eam.rds"))
readr::write_csv(panel_conteo, file.path(data_output_dir, "conteo_personal_categoria_establecimiento_eam.csv"))

# ------------------------------------------------------------------
# 4) Diagnostico de cobertura por anio
# ------------------------------------------------------------------

cobertura_por_anio <- panel_conteo %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    establecimientos = dplyr::n(),
    obreros_promedio = round(mean(total_obreros, na.rm = TRUE), 1),
    prof_tecnico_promedio = round(mean(total_prof_tecnico, na.rm = TRUE), 1),
    administrativos_promedio = round(mean(total_administrativos, na.rm = TRUE), 1),
    propietarios_promedio = round(mean(total_propietarios, na.rm = TRUE), 1),
    pct_con_las_3_categorias = round(
      100 * mean(!is.na(total_obreros) & !is.na(total_prof_tecnico) & !is.na(total_administrativos)),
      2
    ),
    .groups = "drop"
  )

readr::write_csv(cobertura_por_anio, file.path(data_output_dir, "cobertura_conteo_personal_categoria_establecimiento_eam.csv"))

script_header("Conteo de personal por categoria ocupacional, nivel ESTABLECIMIENTO (EAM)")
message("Filas NORDEST-ANIO: ", nrow(panel_conteo))
message("")
message("Cobertura y promedios por anio:")
print(cobertura_por_anio, n = Inf, width = Inf)
message("")
message("Base exportada en: ", file.path(data_output_dir, "conteo_personal_categoria_establecimiento_eam.rds"))
