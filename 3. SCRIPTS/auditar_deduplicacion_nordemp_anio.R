# Auditoria de la regla de deduplicacion NORDEMP-ANIO (Paso 2 de la
# auditoria de reproducibilidad de la macrobase).
#
# construir_macro_base_eam.R NO deduplica: simplemente apila (bind_rows)
# las filas crudas de cada DTA anual. La macrobase resultante
# (macro_base_eam.rds) tiene 140,835 filas, con NORDEMP-ANIO repetido en
# 4,402 combinaciones (14,378 filas involucradas). La reduccion a
# NORDEMP-ANIO unico (130,859 filas) ocurre DESPUES, en los scripts que
# construyen paneles derivados (construir_conteo_personal_categoria_eam.R,
# construir_salarios_promedio_categoria_eam.R, descriptivo_exposicion_eam.R
# y su version archivada de 2012), todos con el MISMO patron:
#
#   panel_raw %>%
#     group_by(NORDEMP, ANIO) %>%
#     summarise(across(<columnas numericas>, ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)))
#
# Regla: SUMA los valores de todas las filas duplicadas (si TODAS son NA,
# el resultado es NA, no 0). No es "se queda el primero", ni "el mas
# reciente", ni un promedio.
#
# Hipotesis a verificar: la macrobase tiene una columna NORDEST
# ("Numero de identificacion del establecimiento"), distinta de NORDEMP
# ("Numero de identificacion de la empresa"). Si una empresa (NORDEMP)
# tiene varios establecimientos (NORDEST) reportando por separado en el
# mismo anio, sumar sus columnas numericas para obtener el total a nivel
# EMPRESA es la agregacion correcta. Si, en cambio, los "duplicados"
# comparten el mismo NORDEST (mismo establecimiento reportado dos veces),
# sumarlos duplicaria indebidamente el dato.
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/auditoria_dedup_muestra_20.csv
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/auditoria_dedup_resumen.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO"))

if (!"NORDEST" %in% names(macro_base)) {
  stop("La columna NORDEST no existe en la macrobase; no se puede verificar la hipotesis de multi-establecimiento.")
}

# ------------------------------------------------------------------
# 1) Ubicar los grupos duplicados NORDEMP-ANIO
# ------------------------------------------------------------------

grupos_dup <- macro_base %>%
  dplyr::count(NORDEMP, ANIO, name = "n_filas") %>%
  dplyr::filter(n_filas > 1)

message("Combinaciones NORDEMP-ANIO con mas de 1 fila: ", nrow(grupos_dup))
message("Filas totales involucradas: ", sum(grupos_dup$n_filas))
message("Filas tras deduplicar (una por NORDEMP-ANIO): ", dplyr::n_distinct(macro_base$NORDEMP, macro_base$ANIO))

# ------------------------------------------------------------------
# 2) Distintos NORDEST dentro de cada grupo duplicado: confirma o
#    descarta la hipotesis de multi-establecimiento.
# ------------------------------------------------------------------

nordest_por_grupo <- macro_base %>%
  dplyr::semi_join(grupos_dup, by = c("NORDEMP", "ANIO")) %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    n_filas = dplyr::n(),
    n_nordest_distintos = dplyr::n_distinct(NORDEST),
    todos_nordest_distintos = n_nordest_distintos == n_filas,
    .groups = "drop"
  )

resumen_nordest <- nordest_por_grupo %>%
  dplyr::summarise(
    grupos_totales = dplyr::n(),
    grupos_con_nordest_todos_distintos = sum(todos_nordest_distintos),
    pct_con_nordest_todos_distintos = round(100 * mean(todos_nordest_distintos), 2),
    grupos_con_algun_nordest_repetido = sum(!todos_nordest_distintos)
  )

readr::write_csv(resumen_nordest, file.path(data_dir, "auditoria_dedup_resumen.csv"))

# ------------------------------------------------------------------
# 3) Muestra de 20 grupos duplicados: filas originales + resultado de
#    aplicar la regla de suma, para revision manual.
# ------------------------------------------------------------------

set.seed(20260810)
muestra_grupos <- grupos_dup %>% dplyr::slice_sample(n = 20)

cols_ilustrativas <- intersect(
  c("PERTOTAL", "C3R10C3", "C4R5C1", "C4R5C2", "C4R5C3", "C4R5C4"),
  names(macro_base)
)

detalle_muestra <- macro_base %>%
  dplyr::inner_join(muestra_grupos %>% dplyr::select(NORDEMP, ANIO), by = c("NORDEMP", "ANIO")) %>%
  dplyr::select(NORDEMP, ANIO, NORDEST, dplyr::all_of(cols_ilustrativas)) %>%
  dplyr::arrange(NORDEMP, ANIO, NORDEST) %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), tipo_fila = "original")

resultado_suma <- detalle_muestra %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    NORDEST = paste(NORDEST, collapse = "+"),
    dplyr::across(dplyr::all_of(cols_ilustrativas), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(tipo_fila = "SUMA (resultado usado en los paneles derivados)")

muestra_20_completa <- dplyr::bind_rows(detalle_muestra, resultado_suma) %>%
  dplyr::arrange(NORDEMP, ANIO, tipo_fila != "SUMA (resultado usado en los paneles derivados)")

readr::write_csv(muestra_20_completa, file.path(data_dir, "auditoria_dedup_muestra_20.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Auditoria de deduplicacion NORDEMP-ANIO")

message("")
message("Verificacion de la hipotesis de multi-establecimiento (NORDEST distinto dentro del grupo):")
print(resumen_nordest, width = Inf)

message("")
message("Muestra de 20 grupos duplicados (filas originales + resultado de la suma):")
print(muestra_20_completa, n = Inf, width = Inf)

message("")
message("Tablas exportadas en: ", data_dir)
