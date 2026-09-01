# Paso 3 (cierre, 2b) de feature/panel-establecimiento: estructura
# multiplanta 2022, complemento de descriptivos_estructura_multiplanta_2022.R.
#
# - Frecuencia de establecimientos por firma en 2022 (TODAS las firmas,
#   no solo multiplanta): 1, 2, 3, 4, 5+.
# - Entre las 262 firmas multiplanta: frecuencia de N departamentos
#   distintos por firma, y cuantas tienen TODAS sus plantas en el mismo
#   departamento.
# - Peso de las 262 firmas multiplanta: % sobre el total de firmas del
#   panel 2022, y % del empleo total de la muestra 2022 que concentran
#   (usando PERTOTAL, la variable oficial DANE de personal ocupado total,
#   ya validada contra cifras oficiales en auditar_empleo_total_vs_dane.R).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - descriptivos_multiplanta_2022_frecuencia_n_establecimientos_todas_firmas.csv
# - descriptivos_multiplanta_2022_frecuencia_n_departamentos.csv
# - descriptivos_multiplanta_2022_peso_firmas_empleo.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

ANIO_BASE <- 2022

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", "DPTO", "PERTOTAL"))

base_2022 <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO))),
    PERTOTAL = suppressWarnings(as.numeric(PERTOTAL))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP, DPTO, PERTOTAL)

# ------------------------------------------------------------------
# 1) Frecuencia de establecimientos por firma en 2022, TODAS las firmas
#    (1, 2, 3, 4, 5+).
# ------------------------------------------------------------------

n_est_por_firma <- base_2022 %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")

frecuencia_n_establecimientos <- n_est_por_firma %>%
  dplyr::mutate(categoria_n = dplyr::case_when(
    n_establecimientos == 1 ~ "1",
    n_establecimientos == 2 ~ "2",
    n_establecimientos == 3 ~ "3",
    n_establecimientos == 4 ~ "4",
    n_establecimientos >= 5 ~ "5+",
    TRUE ~ NA_character_
  )) %>%
  dplyr::count(categoria_n, name = "n_firmas") %>%
  dplyr::mutate(pct = round(100 * n_firmas / sum(n_firmas), 3)) %>%
  dplyr::arrange(factor(categoria_n, levels = c("1", "2", "3", "4", "5+")))

readr::write_csv(frecuencia_n_establecimientos, file.path(out_dir, "descriptivos_multiplanta_2022_frecuencia_n_establecimientos_todas_firmas.csv"))

n_firmas_total_2022 <- nrow(n_est_por_firma)
firmas_multiplanta_2022 <- n_est_por_firma %>% dplyr::filter(n_establecimientos > 1) %>% dplyr::pull(NORDEMP)
n_multiplanta_2022 <- length(firmas_multiplanta_2022)

# ------------------------------------------------------------------
# 2) Entre las 262 firmas multiplanta: frecuencia de N departamentos
#    distintos, y cuantas tienen TODAS sus plantas en un mismo
#    departamento.
# ------------------------------------------------------------------

n_dptos_por_firma <- base_2022 %>%
  dplyr::filter(NORDEMP %in% firmas_multiplanta_2022, !is.na(DPTO)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(n_departamentos_distintos = dplyr::n_distinct(DPTO), .groups = "drop")

frecuencia_n_departamentos <- n_dptos_por_firma %>%
  dplyr::count(n_departamentos_distintos, name = "n_firmas") %>%
  dplyr::mutate(pct = round(100 * n_firmas / sum(n_firmas), 2)) %>%
  dplyr::arrange(n_departamentos_distintos)

readr::write_csv(frecuencia_n_departamentos, file.path(out_dir, "descriptivos_multiplanta_2022_frecuencia_n_departamentos.csv"))

n_todas_mismo_dpto <- n_dptos_por_firma %>% dplyr::filter(n_departamentos_distintos == 1) %>% nrow()

# ------------------------------------------------------------------
# 3) Peso de las 262 firmas multiplanta: % de firmas y % de empleo
#    (PERTOTAL, variable oficial DANE ya validada contra cifras
#    publicadas).
# ------------------------------------------------------------------

empleo_total_muestra <- sum(base_2022$PERTOTAL, na.rm = TRUE)
empleo_firmas_multiplanta <- base_2022 %>%
  dplyr::filter(NORDEMP %in% firmas_multiplanta_2022) %>%
  dplyr::summarise(empleo = sum(PERTOTAL, na.rm = TRUE)) %>%
  dplyr::pull(empleo)

pct_firmas <- round(100 * n_multiplanta_2022 / n_firmas_total_2022, 3)
pct_empleo <- round(100 * empleo_firmas_multiplanta / empleo_total_muestra, 3)

peso_firmas_empleo <- tibble::tibble(
  metrica = c(
    "Firmas totales en el panel 2022",
    "Firmas multiplanta en 2022 (Multi_f)",
    "% de firmas que son multiplanta",
    "Empleo total de la muestra 2022 (PERTOTAL, suma)",
    "Empleo en firmas multiplanta 2022 (PERTOTAL, suma)",
    "% del empleo total que concentran las firmas multiplanta"
  ),
  valor = c(
    n_firmas_total_2022,
    n_multiplanta_2022,
    pct_firmas,
    empleo_total_muestra,
    empleo_firmas_multiplanta,
    pct_empleo
  )
)

readr::write_csv(peso_firmas_empleo, file.path(out_dir, "descriptivos_multiplanta_2022_peso_firmas_empleo.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Estructura multiplanta 2022 (parte 2): frecuencias y peso economico")

message("")
message("1) Frecuencia de establecimientos por firma (TODAS las firmas, 2022):")
print(frecuencia_n_establecimientos, n = Inf, width = Inf)

message("")
message("2) Frecuencia de N departamentos distintos, entre las ", n_multiplanta_2022, " firmas multiplanta:")
print(frecuencia_n_departamentos, n = Inf, width = Inf)
message("Firmas con TODAS sus plantas en el mismo departamento: ", n_todas_mismo_dpto, " de ", nrow(n_dptos_por_firma))

message("")
message("3) Peso de las firmas multiplanta:")
print(peso_firmas_empleo, n = Inf, width = Inf)
message("")
message(">>> Las ", n_multiplanta_2022, " firmas multiplanta (", pct_firmas, "% de las firmas) concentran el ", pct_empleo, "% del empleo total de la muestra 2022. <<<")

message("")
message("Tablas exportadas en: ", out_dir)
