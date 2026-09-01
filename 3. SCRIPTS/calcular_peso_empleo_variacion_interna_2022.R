# Peso en empleo de las firmas multiplanta con variacion interna
# sustancial de exposicion (Paso 3, pendiente). El 21.44% reportado en
# descriptivos_estructura_multiplanta_2022_parte2.R cubre las 262 firmas
# multiplanta COMPLETAS, incluyendo firmas cuyas plantas tienen
# exposicion casi identica (no aportan variacion a delta_f,t). Esta
# cifra separa el peso en empleo de solo las firmas que SI superan el
# umbral de variacion sustancial (15pp y 20pp, definido en
# descriptivos_estructura_multiplanta_2022.R).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - descriptivos_peso_empleo_variacion_interna_2022.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

ANIO_BASE <- 2022

variacion_path <- file.path(out_dir, "descriptivos_multiplanta_2022_variacion_interna.csv")
if (!file.exists(variacion_path)) stop("Falta descriptivos_multiplanta_2022_variacion_interna.csv. Corre descriptivos_estructura_multiplanta_2022.R primero.")

variacion_interna <- readr::read_csv(variacion_path, show_col_types = FALSE) %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", "PERTOTAL"))

base_2022 <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO))),
    PERTOTAL = suppressWarnings(as.numeric(PERTOTAL))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP, PERTOTAL)

empleo_total_muestra <- sum(base_2022$PERTOTAL, na.rm = TRUE)

empleo_por_firma <- base_2022 %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(empleo_firma = sum(PERTOTAL, na.rm = TRUE), .groups = "drop")

UMBRAL_15PP <- 0.15
UMBRAL_20PP <- 0.20

firmas_15pp <- variacion_interna %>% dplyr::filter(rango_max_menos_min >= UMBRAL_15PP) %>% dplyr::pull(NORDEMP)
firmas_20pp <- variacion_interna %>% dplyr::filter(rango_max_menos_min >= UMBRAL_20PP) %>% dplyr::pull(NORDEMP)
firmas_262 <- variacion_interna %>% dplyr::pull(NORDEMP)  # de referencia: universo Multi_f completo (262 o 260 con dato)

n_firmas_total_muestra <- dplyr::n_distinct(base_2022$NORDEMP)
empleo_promedio_muestra <- round(empleo_total_muestra / n_firmas_total_muestra, 1)

calcular_peso <- function(firmas, etiqueta) {
  empleo_grupo <- empleo_por_firma %>% dplyr::filter(NORDEMP %in% firmas) %>% dplyr::summarise(e = sum(empleo_firma)) %>% dplyr::pull(e)
  n_firmas <- length(firmas)
  tibble::tibble(
    grupo = etiqueta,
    n_firmas = n_firmas,
    pct_n_firmas_de_262 = round(100 * n_firmas / length(firmas_262), 2),
    empleo_grupo = empleo_grupo,
    pct_empleo_de_262 = round(100 * empleo_grupo / (empleo_por_firma %>% dplyr::filter(NORDEMP %in% firmas_262) %>% dplyr::summarise(e = sum(empleo_firma)) %>% dplyr::pull(e)), 2),
    pct_empleo_de_muestra_total = round(100 * empleo_grupo / empleo_total_muestra, 3),
    empleo_promedio_por_firma = round(empleo_grupo / n_firmas, 1),
    razon_vs_empleo_promedio_muestra = round((empleo_grupo / n_firmas) / empleo_promedio_muestra, 2)
  )
}

resultado <- dplyr::bind_rows(
  calcular_peso(firmas_262, "Todas las firmas con exposicion valida en 2+ plantas (base variacion interna, referencia)"),
  calcular_peso(firmas_15pp, "Firmas con brecha >=15pp (variacion sustancial)"),
  calcular_peso(firmas_20pp, "Firmas con brecha >=20pp (variacion sustancial, umbral conservador)")
)

readr::write_csv(resultado, file.path(out_dir, "descriptivos_peso_empleo_variacion_interna_2022.csv"))

script_header("Peso en empleo de firmas con variacion interna sustancial (2022)")
message("")
message("Empleo promedio por firma, TODA la muestra 2022 (", n_firmas_total_muestra, " firmas): ", empleo_promedio_muestra)
print(resultado, n = Inf, width = Inf)
message("")
message("Tabla exportada en: ", out_dir)
