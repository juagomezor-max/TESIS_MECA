# Construye conteos de personal por categoria ocupacional (obreros y
# operarios, profesional-tecnico-tecnologo, directivos y administracion/
# ventas) a nivel NORDEMP-ANIO, a partir de las columnas C4R confirmadas
# como estables en verificar_estabilidad_columnas_c3r_c4r.R.
#
# Los propietarios/socios sin remuneracion fija se excluyen de las tres
# categorias principales y se dejan en una columna aparte
# (total_propietarios), porque el diccionario oficial de DANE no indica
# que deban sumarse dentro de Obreros/PT/Administrativos: son una fila de
# tipo de vinculacion (C4R1) que cruza las tres categorias, no una cuarta
# categoria ocupacional en si misma. `empleo_total_categorias` se calcula
# SIN propietarios por consistencia con esa decision; se deja tambien
# `empleo_total_categorias_con_propietarios` como alternativa para poder
# comparar (ver nota de trade-off mas abajo).
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/conteo_personal_categoria_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

macro_path <- paths$macro_base_eam
if (!file.exists(macro_path)) {
  stop("No se encontro la macrobase EAM en: ", macro_path)
}

# Misma logica de manejo de NA que descriptivo_exposicion_eam.R: si todas
# las columnas de un grupo faltan en una fila, se conserva NA en vez de
# devolver un cero artificial.
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
check_required_vars(macro_base, c("NORDEMP", "ANIO"))

# ------------------------------------------------------------------
# 1) Columnas por categoria ocupacional y tipo de vinculacion, EXCLUYENDO
#    la fila de propietarios (C4R1 sin sufijo N/E). Confirmadas estables
#    2008-2024 en el Paso 2 (verificar_estabilidad_columnas_c3r_c4r.R).
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

# ------------------------------------------------------------------
# 2) Consolidacion a NORDEMP-ANIO (misma logica que
#    descriptivo_exposicion_eam.R: si una empresa-anio aparece duplicada,
#    se suman los componentes numericos antes de construir las categorias).
# ------------------------------------------------------------------

todas_las_cols <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico, cols_propietarios))
cols_presentes <- todas_las_cols[todas_las_cols %in% names(macro_base)]

if (length(cols_presentes) < length(todas_las_cols)) {
  faltantes <- setdiff(todas_las_cols, cols_presentes)
  stop(
    "Faltan columnas esperadas en la macrobase (revisar Paso 2): ",
    paste(faltantes, collapse = ", ")
  )
}

panel_raw <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEMP, ANIO, dplyr::all_of(cols_presentes)) %>%
  dplyr::mutate(dplyr::across(-c(NORDEMP, ANIO), ~suppressWarnings(as.numeric(.x))))

panel <- panel_raw %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(cols_presentes),
      ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# 3) Conteos por categoria
# ------------------------------------------------------------------

panel_conteo <- panel %>%
  dplyr::mutate(
    total_obreros = sum_if_exists(., cols_obreros),
    total_administrativos = sum_if_exists(., cols_administrativos),
    total_prof_tecnico = sum_if_exists(., cols_prof_tecnico),
    total_propietarios = sum_if_exists(., cols_propietarios),
    # Sin propietarios: la exposicion busca capturar trabajadores con
    # relacion de dependencia salarial, que son quienes estan sujetos a la
    # normativa de salario minimo. Los propietarios/socios sin remuneracion
    # fija no perciben un salario fijo por definicion, asi que mezclarlos
    # en el denominador de empleo diluiria una medida pensada para
    # trabajadores asalariados.
    empleo_total_categorias = total_obreros + total_administrativos + total_prof_tecnico,
    # Alternativa para comparar el efecto de incluirlos.
    empleo_total_categorias_con_propietarios = empleo_total_categorias + total_propietarios
  ) %>%
  dplyr::select(
    NORDEMP, ANIO,
    total_obreros, total_prof_tecnico, total_administrativos, total_propietarios,
    empleo_total_categorias, empleo_total_categorias_con_propietarios
  )

readr::write_rds(panel_conteo, file.path(data_output_dir, "conteo_personal_categoria_eam.rds"))
readr::write_csv(panel_conteo, file.path(data_output_dir, "conteo_personal_categoria_eam.csv"))

# ------------------------------------------------------------------
# 4) Diagnostico de cobertura por anio
# ------------------------------------------------------------------

cobertura_por_anio <- panel_conteo %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    empresas = dplyr::n(),
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

readr::write_csv(cobertura_por_anio, file.path(data_output_dir, "cobertura_conteo_personal_categoria_eam.csv"))

script_header("Conteo de personal por categoria ocupacional (EAM)")
message("Filas NORDEMP-ANIO: ", nrow(panel_conteo))
message("")
message("Cobertura y promedios por anio:")
print(cobertura_por_anio, n = Inf, width = Inf)
message("")
message("Base exportada en: ", file.path(data_output_dir, "conteo_personal_categoria_eam.rds"))
