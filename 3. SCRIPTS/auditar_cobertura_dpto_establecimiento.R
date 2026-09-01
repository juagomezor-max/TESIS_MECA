# Auditoria de cobertura y consistencia de DPTO (departamento) por
# establecimiento-anio, Paso 2 de feature/panel-establecimiento. Objetivo:
# confirmar que DPTO se puede usar para construir departamento*anio como
# efecto fijo en la especificacion a nivel establecimiento, sin huecos ni
# cambios de codificacion que distorsionen ese control.
#
# Preguntas (Paso 2.2):
# - Que proporcion de establecimiento-anio tiene DPTO no faltante, por anio.
# - Si hay anios con cobertura notablemente peor que el resto (posible
#   cambio de formulario o metodologia de captura).
#
# Chequeo adicional (barato, mismo espiritu que el Paso 1.5): se compara
# el CONJUNTO de codigos DPTO presentes cada anio, no solo el % de
# cobertura -- un cambio de metodologia de captura podria mantener 100%
# de cobertura pero alterar que codigos se usan (ej. reagrupar
# departamentos, introducir un codigo nuevo de "no clasificado").
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - auditoria_dpto_cobertura_por_anio.csv
# - auditoria_dpto_codigos_por_anio.csv

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
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO))

# ------------------------------------------------------------------
# 1) Cobertura de DPTO por anio (establecimiento-anio; NORDEST-ANIO ya
#    confirmado unico en el 100% del panel en el Paso 1, asi que cada
#    fila de `base` es un establecimiento-anio).
# ------------------------------------------------------------------

cobertura_por_anio <- base %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    establecimientos = dplyr::n(),
    dpto_no_faltante = sum(!is.na(DPTO)),
    pct_cobertura = round(100 * mean(!is.na(DPTO)), 3),
    n_dptos_distintos = dplyr::n_distinct(DPTO[!is.na(DPTO)]),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ANIO)

readr::write_csv(cobertura_por_anio, file.path(out_dir, "auditoria_dpto_cobertura_por_anio.csv"))

cobertura_minima <- min(cobertura_por_anio$pct_cobertura)
anios_cobertura_baja <- cobertura_por_anio %>% dplyr::filter(pct_cobertura < 99) %>% dplyr::pull(ANIO)

# ------------------------------------------------------------------
# 2) Conjunto de codigos DPTO presentes cada anio (deteccion de cambios
#    de codificacion aunque la cobertura se mantenga en 100%).
# ------------------------------------------------------------------

codigos_por_anio <- base %>%
  dplyr::filter(!is.na(DPTO)) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(codigos_dpto = paste(sort(unique(DPTO)), collapse = ","), .groups = "drop") %>%
  dplyr::arrange(ANIO)

readr::write_csv(codigos_por_anio, file.path(out_dir, "auditoria_dpto_codigos_por_anio.csv"))

conjunto_referencia <- codigos_por_anio$codigos_dpto[1]
anios_con_conjunto_distinto <- codigos_por_anio %>%
  dplyr::filter(codigos_dpto != conjunto_referencia) %>%
  dplyr::pull(ANIO)

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Cobertura y consistencia de DPTO por establecimiento-anio")

message("")
message("1) Cobertura de DPTO por anio:")
print(cobertura_por_anio, n = Inf, width = Inf)
message("")
message("Cobertura minima observada: ", cobertura_minima, "%")
if (length(anios_cobertura_baja) == 0) {
  message("Ningun anio tiene cobertura por debajo del 99%.")
} else {
  message("Anios con cobertura < 99%: ", paste(anios_cobertura_baja, collapse = ", "))
}

message("")
message("2) Conjunto de codigos DPTO por anio (referencia = ", codigos_por_anio$ANIO[1], "): ", conjunto_referencia)
if (length(anios_con_conjunto_distinto) == 0) {
  message("El conjunto de codigos DPTO es IDENTICO en los 17 anios -- sin evidencia de cambio de")
  message("metodologia de captura o recodificacion del departamento.")
} else {
  message("Anios con conjunto de codigos DISTINTO al de referencia: ", paste(anios_con_conjunto_distinto, collapse = ", "))
}

message("")
message("Tablas exportadas en: ", out_dir)
