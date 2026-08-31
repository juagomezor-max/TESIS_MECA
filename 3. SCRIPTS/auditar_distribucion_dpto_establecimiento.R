# Distribucion de establecimientos por departamento (Paso 2.4 de
# feature/panel-establecimiento). Objetivo: entender cuantos
# departamentos distintos estan representados y que tan concentrada o
# dispersa esta la distribucion, porque celdas departamento*anio
# pequeñas corren riesgo de quedarse sin variacion suficiente para
# identificar ese efecto fijo en la especificacion a nivel
# establecimiento.
#
# DPTO representativo por establecimiento: se usa el DPTO MODAL (el mas
# frecuente a lo largo del panel de cada NORDEST), robusto a la
# inestabilidad ya documentada en el Paso 2.3 (3.68% de establecimientos
# con mas de un DPTO, mayoritariamente el par Bogota/Cundinamarca). No
# depende de que decision se tome sobre el tratamiento final de esa
# inestabilidad -- es solo para dimensionar el problema de celdas
# pequeñas, no para fijar la especificacion final.
#
# Ademas de la distribucion cross-seccional (cuantos establecimientos
# por departamento en total), se reporta la dimension que realmente
# importa para `departamento*anio`: cuantas celdas departamento-anio
# tienen pocos establecimientos en la ventana de estimacion relevante
# (2015-2019 + 2023, el diseño pre/post del DiD).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - auditoria_dpto_distribucion_establecimientos.csv
# - auditoria_dpto_celdas_departamento_anio.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "ANIO", "DPTO"))

base <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO), !is.na(DPTO)) %>%
  dplyr::distinct(NORDEST, ANIO, DPTO)

# ------------------------------------------------------------------
# 1) DPTO modal por establecimiento (representante cross-seccional).
# ------------------------------------------------------------------

moda <- function(x) {
  tabla <- table(x)
  as.numeric(names(tabla)[which.max(tabla)])
}

dpto_modal <- base %>%
  dplyr::group_by(NORDEST) %>%
  dplyr::summarise(DPTO_modal = moda(DPTO), .groups = "drop")

distribucion <- dpto_modal %>%
  dplyr::count(DPTO_modal, name = "n_establecimientos") %>%
  dplyr::arrange(dplyr::desc(n_establecimientos)) %>%
  dplyr::mutate(
    pct = round(100 * n_establecimientos / sum(n_establecimientos), 2),
    pct_acumulado = round(cumsum(pct), 2)
  )

readr::write_csv(distribucion, file.path(out_dir, "auditoria_dpto_distribucion_establecimientos.csv"))

n_departamentos <- nrow(distribucion)
umbral_pequeno <- 30
departamentos_pequenos <- distribucion %>% dplyr::filter(n_establecimientos < umbral_pequeno)

# ------------------------------------------------------------------
# 2) Celdas departamento-anio en la ventana relevante del DiD
#    (2015-2019 pre-choque + 2023 post-choque), usando el DPTO
#    reportado ESE anio (no el modal), porque es la celda real que
#    usaria el efecto fijo departamento*anio.
# ------------------------------------------------------------------

anios_relevantes <- c(2015:2019, 2023)

celdas <- base %>%
  dplyr::filter(ANIO %in% anios_relevantes) %>%
  dplyr::count(DPTO, ANIO, name = "n_establecimientos") %>%
  tidyr::complete(DPTO = unique(base$DPTO), ANIO = anios_relevantes, fill = list(n_establecimientos = 0)) %>%
  dplyr::arrange(DPTO, ANIO)

readr::write_csv(celdas, file.path(out_dir, "auditoria_dpto_celdas_departamento_anio.csv"))

umbral_celda_pequena <- 10
celdas_pequenas <- celdas %>% dplyr::filter(n_establecimientos < umbral_celda_pequena)
celdas_vacias <- celdas %>% dplyr::filter(n_establecimientos == 0)

resumen_celdas_por_dpto <- celdas %>%
  dplyr::group_by(DPTO) %>%
  dplyr::summarise(
    min_establecimientos_anio = min(n_establecimientos),
    max_establecimientos_anio = max(n_establecimientos),
    algun_anio_bajo_umbral = any(n_establecimientos < umbral_celda_pequena),
    .groups = "drop"
  ) %>%
  dplyr::arrange(min_establecimientos_anio)

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Distribucion de establecimientos por departamento")

message("")
message("Departamentos distintos representados: ", n_departamentos)
message("")
message("Distribucion (DPTO modal por establecimiento, cross-seccional, 2008-2024):")
print(distribucion, n = Inf, width = Inf)

message("")
message("Departamentos con menos de ", umbral_pequeno, " establecimientos (riesgo de celda pequeña): ", nrow(departamentos_pequenos))
print(departamentos_pequenos, n = Inf, width = Inf)

message("")
message("Celdas departamento-anio en la ventana relevante (2015-2019 + 2023), por DPTO (ordenado por el minimo anual):")
print(resumen_celdas_por_dpto, n = Inf, width = Inf)

message("")
message("Celdas departamento-anio con menos de ", umbral_celda_pequena, " establecimientos: ", nrow(celdas_pequenas), " de ", nrow(celdas))
message("Celdas departamento-anio COMPLETAMENTE VACIAS (0 establecimientos): ", nrow(celdas_vacias))
if (nrow(celdas_vacias) > 0) print(celdas_vacias, n = Inf, width = Inf)

message("")
message("Tablas exportadas en: ", out_dir)
