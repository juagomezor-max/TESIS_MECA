# Cruce departamento (DPTO) x sector (CIIU4), Paso 2.5 de
# feature/panel-establecimiento. Objetivo: verificar si hay
# departamentos donde la actividad manufacturera esta muy concentrada en
# uno o dos sectores -- riesgo de colinealidad entre `sector*anio` y
# `departamento*anio` si ambos efectos fijos se incluyen juntos en la
# especificacion a nivel establecimiento (si un departamento es casi
# monosectorial, departamento*anio y sector*anio para ESE departamento
# absorben esencialmente la misma variacion).
#
# NO se construye el panel de establecimiento-anio ni se corre ninguna
# regresion en este script. Es exclusivamente validacion de la variable
# de ubicacion (continuacion del Paso 2).
#
# Ventana y granularidad: establecimiento-anio en 2015-2019 + 2023 (la
# ventana pre/post relevante del diseño DiD), misma que
# auditar_distribucion_dpto_establecimiento.R (Paso 2.4), para
# comparabilidad directa. CIIU4 es la variable de sector correcta en
# este periodo (CIIU3 solo tiene datos 2008-2011, ver notas de la rama
# feature/exposicion-obreros-operarios).
#
# Metricas de concentracion por departamento:
# - n_sectores_distintos: cuantos CIIU4 distintos aparecen en ese DPTO.
# - pct_top1_sector: % de establecimientos-anio del DPTO que caen en su
#   sector CIIU4 mas frecuente.
# - pct_top2_sectores: % acumulado en los 2 sectores mas frecuentes.
# - HHI (Herfindahl-Hirschman, base participaciones 0-1, rango 0-1):
#   suma de las participaciones al cuadrado de cada CIIU4 dentro del
#   DPTO. HHI cercano a 1 = practicamente monosectorial; HHI bajo =
#   diversificado.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - auditoria_dpto_ciiu4_concentracion.csv (una fila por DPTO)
# - auditoria_dpto_ciiu4_detalle.csv (una fila por DPTO x CIIU4, con conteos)

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "ANIO", "DPTO", "CIIU4"))

ANIOS_RELEVANTES <- c(2015:2019, 2023)

base <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(
    !is.na(NORDEST), NORDEST != "", !is.na(ANIO), ANIO %in% ANIOS_RELEVANTES,
    !is.na(DPTO), !is.na(CIIU4)
  ) %>%
  dplyr::distinct(NORDEST, ANIO, DPTO, CIIU4)

message("Establecimiento-anio en la ventana 2015-2019+2023, con DPTO y CIIU4 validos: ", nrow(base))

# ------------------------------------------------------------------
# 1) Detalle DPTO x CIIU4 (conteos).
# ------------------------------------------------------------------

detalle <- base %>%
  dplyr::count(DPTO, CIIU4, name = "n_establecimientos_anio") %>%
  dplyr::group_by(DPTO) %>%
  dplyr::mutate(pct_dentro_dpto = round(100 * n_establecimientos_anio / sum(n_establecimientos_anio), 2)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(DPTO, dplyr::desc(n_establecimientos_anio))

readr::write_csv(detalle, file.path(out_dir, "auditoria_dpto_ciiu4_detalle.csv"))

# ------------------------------------------------------------------
# 2) Concentracion por departamento.
# ------------------------------------------------------------------

concentracion <- detalle %>%
  dplyr::group_by(DPTO) %>%
  dplyr::summarise(
    total_establecimientos_anio = sum(n_establecimientos_anio),
    n_sectores_distintos = dplyr::n_distinct(CIIU4),
    sector_top1 = CIIU4[which.max(n_establecimientos_anio)],
    pct_top1_sector = max(pct_dentro_dpto),
    pct_top2_sectores = sum(sort(pct_dentro_dpto, decreasing = TRUE)[1:min(2, dplyr::n())]),
    HHI = round(sum((pct_dentro_dpto / 100)^2), 4),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(pct_top1_sector))

readr::write_csv(concentracion, file.path(out_dir, "auditoria_dpto_ciiu4_concentracion.csv"))

umbral_alto <- 50
departamentos_alta_concentracion <- concentracion %>% dplyr::filter(pct_top1_sector >= umbral_alto)

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Cruce departamento x sector (CIIU4): concentracion sectorial")

message("")
message("Concentracion por departamento (ordenado por % en el sector top1, ventana 2015-2019+2023):")
print(concentracion, n = Inf, width = Inf)

message("")
message("Departamentos con >= ", umbral_alto, "% de sus establecimiento-anio en UN SOLO sector CIIU4: ",
        nrow(departamentos_alta_concentracion), " de ", nrow(concentracion))
if (nrow(departamentos_alta_concentracion) > 0) {
  print(departamentos_alta_concentracion, n = Inf, width = Inf)
}

message("")
message("Tablas exportadas en: ", out_dir)
