# Verificacion cruzada de consistencia entre los scripts del Paso 3 de
# feature/panel-establecimiento (auditar_confiabilidad_nordest.R,
# descriptivos_estructura_multiplanta_2022.R,
# descriptivos_estructura_multiplanta_2022_parte2.R,
# construir_exposicion_obreros_establecimiento_eam.R). Objetivo: confirmar
# con codigo -- no de memoria ni combinando cifras entre mensajes -- que
# los totales de establecimientos/firmas 2022 usados en distintos scripts
# coinciden, y verificar por conteo DIRECTO (no inferido) el % de
# establecimientos monoplanta y la cobertura de Exposure2022_obreros_est.
#
# Chequeos:
# A vs B: total establecimientos 2022 segun el filtro de
#   auditar_confiabilidad_nordest.R vs. el filtro de
#   descriptivos_estructura_multiplanta_2022.R.
# C vs D: firmas monoplanta (n_establecimientos==1) vs. establecimientos
#   pertenecientes a firmas monoplanta contados DIRECTAMENTE (no
#   inferidos de C -- deben coincidir por definicion, pero se verifica).
# E: % de establecimientos monoplanta = D / B.
# F vs B: total establecimientos 2022 en
#   exposicion_obreros_establecimiento_eam.rds vs. B.
# H: cobertura de Exposure2022_obreros_est en 2022 = G / F.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - verificacion_consistencia_cruzada_multiplanta_2022.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

ANIO_BASE <- 2022

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO"))

# --- A: filtro de auditar_confiabilidad_nordest.R (solo exige NORDEST/ANIO validos) ---
base_A <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP), NORDEST = as.character(NORDEST),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO))
n_A <- base_A %>% dplyr::filter(ANIO == ANIO_BASE) %>% dplyr::summarise(n = dplyr::n_distinct(NORDEST)) %>% dplyr::pull(n)

# --- B: filtro de descriptivos_estructura_multiplanta_2022.R (exige ademas ANIO == 2022 desde el inicio) ---
base_B <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP)
n_B <- dplyr::n_distinct(base_B$NORDEST)

# --- C vs D: firmas monoplanta vs establecimientos monoplanta (conteo directo) ---
n_est_por_firma <- base_B %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_monoplanta <- n_est_por_firma %>% dplyr::filter(n_establecimientos == 1) %>% dplyr::pull(NORDEMP)
n_C <- length(firmas_monoplanta)
n_D <- base_B %>% dplyr::filter(NORDEMP %in% firmas_monoplanta) %>% dplyr::summarise(n = dplyr::n_distinct(NORDEST)) %>% dplyr::pull(n)

pct_E <- round(100 * n_D / n_B, 4)

# --- F vs G vs H: cobertura de Exposure2022_obreros_est ---
exposicion_est_path <- file.path(data_dir, "exposicion_obreros_establecimiento_eam.rds")
if (!file.exists(exposicion_est_path)) stop("Falta exposicion_obreros_establecimiento_eam.rds.")
exposicion_est_2022 <- readr::read_rds(exposicion_est_path) %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, Exposure2022_obreros_est)
n_F <- nrow(exposicion_est_2022)
n_G <- sum(!is.na(exposicion_est_2022$Exposure2022_obreros_est))
pct_H_cobertura <- round(100 * n_G / n_F, 4)
pct_H_faltante <- round(100 * (n_F - n_G) / n_F, 4)

resultado <- tibble::tibble(
  chequeo = c(
    "A: total establecimientos 2022 (filtro auditar_confiabilidad_nordest.R)",
    "B: total establecimientos 2022 (filtro descriptivos_estructura_multiplanta_2022.R)",
    "A == B",
    "C: firmas monoplanta (n_establecimientos == 1)",
    "D: establecimientos en firmas monoplanta (conteo directo, NO inferido de C)",
    "C == D",
    "E: % establecimientos monoplanta (D / B)",
    "F: total establecimientos 2022 en exposicion_obreros_establecimiento_eam.rds",
    "F == B",
    "G: establecimientos con Exposure2022_obreros_est valida",
    "H: cobertura Exposure2022_obreros_est en 2022 (G / F, %)",
    "H: faltante Exposure2022_obreros_est en 2022 (%)"
  ),
  valor = c(
    n_A, n_B, as.character(n_A == n_B), n_C, n_D, as.character(n_C == n_D),
    pct_E, n_F, as.character(n_F == n_B), n_G, pct_H_cobertura, pct_H_faltante
  )
)

readr::write_csv(resultado, file.path(out_dir, "verificacion_consistencia_cruzada_multiplanta_2022.csv"))

script_header("Verificacion cruzada de consistencia: estructura multiplanta 2022")
message("")
print(resultado, n = Inf, width = Inf)

if (n_A != n_B) message("*** DISCREPANCIA REAL: A (", n_A, ") != B (", n_B, ") ***")
if (n_C != n_D) message("*** DISCREPANCIA REAL: C (", n_C, ") != D (", n_D, ") ***")
if (n_F != n_B) message("*** DISCREPANCIA REAL: F (", n_F, ") != B (", n_B, ") ***")

message("")
message("Tabla exportada en: ", out_dir)
