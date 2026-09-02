# opcional_establecimiento.R -- Pipeline simplificado (rama `simplificacion`).
#
# MODULO OPCIONAL, fuera de la ruta principal (run_all.R NO lo llama). El
# nucleo minimo del pipeline es a nivel de FIRMA; este modulo extiende al
# nivel de ESTABLECIMIENTO (NORDEST): panel completo 2008-2024,
# Exposure2022_obreros_est propia de cada planta, estructura multiplanta
# 2022, y la cohorte balanceada para la especificacion "dentro de firma".
#
# Requiere que 01_construir_base.R y 02_construir_exposicion.R ya se
# hayan corrido (usa panel_firma_eam.rds solo para el join de
# comparacion planta-firma; el resto se recalcula desde la macrobase
# porque necesita TODO el panel 2008-2024, no solo la ventana de 03).
#
# DERIVADO de (no reimplementado de memoria):
# - construir_conteo_personal_categoria_establecimiento_eam.R +
#   construir_exposicion_obreros_establecimiento_eam.R: formula y
#   winsorizacion de Exposure2022_obreros_est.
# - auditar_confiabilidad_nordest.R: NORDEST-ANIO unico en el 100% del
#   panel 2008-2024 (12,621 establecimientos unicos).
# - descriptivos_estructura_multiplanta_2022.R +
#   descriptivos_estructura_multiplanta_2022_parte2.R: definicion de
#   Multi_f (firmas con >1 establecimiento EN 2022), variacion interna
#   (rango max-min de Exposure2022_obreros_est entre plantas de una
#   firma), umbral de variacion sustancial (15pp/20pp).
# - calcular_peso_empleo_variacion_interna_2022.R: peso en empleo
#   (PERTOTAL) de las firmas multiplanta.
# - construir_panel_efectivo_especificacion_b_por_anio.R: cohorte
#   balanceada (>=2 plantas en los 9 años de la ventana del panel formal).
# - verificar_consistencia_cruzada_multiplanta_2022.R: denominadores
#   6,775/6,761 y verificacion cruzada.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - simplificado_establecimiento_denominadores.csv
# - simplificado_establecimiento_multiplanta.csv
# - simplificado_establecimiento_umbral_variacion.csv
# - simplificado_establecimiento_cohorte_balanceada.csv
# - simplificado_establecimiento_correlacion_planta_firma.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)
ANIO_BASE <- 2022

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_numericas <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico, "PERTOTAL"))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", "CIIU4", "DPTO", cols_numericas))

# ------------------------------------------------------------------
# 1) Panel de establecimiento COMPLETO 2008-2024 (NORDEST-ANIO ya
#    unico -- confirmado en auditar_confiabilidad_nordest.R, verificado
#    aqui explicitamente antes de continuar).
# ------------------------------------------------------------------

base_establecimiento <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, CIIU4, DPTO, dplyr::all_of(cols_numericas)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_numericas), safe_numeric))

n_dup <- base_establecimiento %>% dplyr::count(NORDEST, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup > 0) stop("NORDEST-ANIO no es unico (", n_dup, " grupos). Contradice auditar_confiabilidad_nordest.R -- revisar.")

n_establecimientos_2008_2024 <- dplyr::n_distinct(base_establecimiento$NORDEST)

base_establecimiento <- base_establecimiento %>%
  dplyr::mutate(
    total_obreros = rowSums(dplyr::across(dplyr::all_of(cols_obreros)), na.rm = TRUE),
    total_administrativos = rowSums(dplyr::across(dplyr::all_of(cols_administrativos)), na.rm = TRUE),
    total_prof_tecnico = rowSums(dplyr::across(dplyr::all_of(cols_prof_tecnico)), na.rm = TRUE),
    empleo_total = total_obreros + total_administrativos + total_prof_tecnico
  )

# ------------------------------------------------------------------
# 2) Exposure2022_obreros_est (linea base 2022, propia del establecimiento).
# ------------------------------------------------------------------

baseline_est_2022 <- base_establecimiento %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::mutate(participacion_obreros_raw = safe_divide(total_obreros, empleo_total)) %>%
  dplyr::transmute(NORDEST, NORDEMP, DPTO, Exposure2022_obreros_est = winsorize(participacion_obreros_raw))

n_establecimientos_2022 <- base_establecimiento %>% dplyr::filter(ANIO == ANIO_BASE) %>% dplyr::summarise(n = dplyr::n_distinct(NORDEST)) %>% dplyr::pull(n)
n_establecimientos_2022_exp_valida <- sum(!is.na(baseline_est_2022$Exposure2022_obreros_est))

denominadores_est <- tibble::tibble(
  metrica = c("Establecimientos activos 2022", "Establecimientos con Exposure2022_obreros_est valida en 2022", "Establecimientos unicos 2008-2024 (panel completo)"),
  valor = c(n_establecimientos_2022, n_establecimientos_2022_exp_valida, n_establecimientos_2008_2024)
)
readr::write_csv(denominadores_est, file.path(out_dir, "simplificado_establecimiento_denominadores.csv"))

# ------------------------------------------------------------------
# 3) Correlacion Exposure2022_obreros_est (planta) vs Exposure2022_obreros
#    (firma, heredada) -- requiere exposicion_firma_eam.rds de 02.
# ------------------------------------------------------------------

exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_firma_eam.rds. Corre 02_construir_exposicion.R primero.")
exposicion_firma <- readr::read_rds(exposicion_firma_path)

comparacion_planta_firma <- baseline_est_2022 %>%
  dplyr::inner_join(exposicion_firma %>% dplyr::select(NORDEMP, Exposure2022_obreros), by = "NORDEMP") %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est), !is.na(Exposure2022_obreros))

correlacion_planta_firma <- tibble::tibble(
  n = nrow(comparacion_planta_firma),
  correlacion_pearson = round(cor(comparacion_planta_firma$Exposure2022_obreros_est, comparacion_planta_firma$Exposure2022_obreros), 3)
)
readr::write_csv(correlacion_planta_firma, file.path(out_dir, "simplificado_establecimiento_correlacion_planta_firma.csv"))

# ------------------------------------------------------------------
# 4) Estructura multiplanta 2022: Multi_f oficial, variacion interna,
#    umbral de variacion sustancial, peso en empleo.
# ------------------------------------------------------------------

base_2022_multi <- base_establecimiento %>% dplyr::filter(ANIO == ANIO_BASE) %>% dplyr::distinct(NORDEST, NORDEMP)
n_est_por_firma_2022 <- base_2022_multi %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_por_firma_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)

variacion_interna <- baseline_est_2022 %>%
  dplyr::filter(NORDEMP %in% firmas_262, !is.na(Exposure2022_obreros_est)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(
    n_establecimientos_con_exposicion = dplyr::n(),
    rango_max_menos_min = round(max(Exposure2022_obreros_est) - min(Exposure2022_obreros_est), 4),
    .groups = "drop"
  )

n_260 <- variacion_interna %>% dplyr::filter(n_establecimientos_con_exposicion >= 2) %>% nrow()
n_supera_15pp <- variacion_interna %>% dplyr::filter(n_establecimientos_con_exposicion >= 2, rango_max_menos_min >= 0.15) %>% nrow()
n_supera_20pp <- variacion_interna %>% dplyr::filter(n_establecimientos_con_exposicion >= 2, rango_max_menos_min >= 0.20) %>% nrow()

pertotal_2022 <- base_establecimiento %>% dplyr::filter(ANIO == ANIO_BASE) %>% dplyr::distinct(NORDEST, NORDEMP, PERTOTAL)
empleo_total_muestra <- sum(pertotal_2022$PERTOTAL, na.rm = TRUE)
empleo_multiplanta <- pertotal_2022 %>% dplyr::filter(NORDEMP %in% firmas_262) %>% dplyr::summarise(e = sum(PERTOTAL, na.rm = TRUE)) %>% dplyr::pull(e)
pct_empleo_multiplanta <- round(100 * empleo_multiplanta / empleo_total_muestra, 2)

tabla_multiplanta <- tibble::tibble(
  metrica = c(
    "Firmas multiplanta EN 2022 (Multi_f)",
    "Firmas con 2+ establecimientos con exposicion valida (base variacion interna)",
    "% del empleo total 2022 que concentran las firmas multiplanta"
  ),
  valor = c(length(firmas_262), n_260, pct_empleo_multiplanta)
)
readr::write_csv(tabla_multiplanta, file.path(out_dir, "simplificado_establecimiento_multiplanta.csv"))

tabla_umbral <- tibble::tibble(
  umbral_pp = c(15, 20),
  n_firmas_supera_umbral = c(n_supera_15pp, n_supera_20pp),
  n_firmas_base = n_260,
  pct_supera_umbral = round(100 * c(n_supera_15pp, n_supera_20pp) / n_260, 2)
)
readr::write_csv(tabla_umbral, file.path(out_dir, "simplificado_establecimiento_umbral_variacion.csv"))

# ------------------------------------------------------------------
# 5) Cohorte balanceada: firmas de las 262 con >=2 plantas en LOS 9
#    anios de la ventana del panel formal.
# ------------------------------------------------------------------

base_ventana_est <- base_establecimiento %>% dplyr::filter(ANIO %in% PANEL_ANIOS_FINAL)
n_est_por_firma_anio <- base_ventana_est %>% dplyr::group_by(NORDEMP, ANIO) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")

persistencia <- n_est_por_firma_anio %>%
  dplyr::filter(NORDEMP %in% firmas_262) %>%
  tidyr::complete(NORDEMP = firmas_262, ANIO = PANEL_ANIOS_FINAL, fill = list(n_est = 0)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(mantiene_2plus_todos_los_anios = all(n_est >= 2), .groups = "drop")

n_cohorte_balanceada <- sum(persistencia$mantiene_2plus_todos_los_anios)

tabla_cohorte <- tibble::tibble(
  metrica = c("Firmas multiplanta 2022 (Multi_f)", "Firmas que mantienen >=2 plantas en los 9 anios (cohorte balanceada)"),
  valor = c(length(firmas_262), n_cohorte_balanceada)
)
readr::write_csv(tabla_cohorte, file.path(out_dir, "simplificado_establecimiento_cohorte_balanceada.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("opcional_establecimiento.R -- Panel NORDEST, Exposure_est, multiplanta, cohorte")
message("")
message("Denominadores:")
print(denominadores_est, width = Inf)
message("")
message("Correlacion planta-firma:")
print(correlacion_planta_firma, width = Inf)
message("")
message("Estructura multiplanta:")
print(tabla_multiplanta, width = Inf)
message("")
message("Umbral de variacion:")
print(tabla_umbral, width = Inf)
message("")
message("Cohorte balanceada:")
print(tabla_cohorte, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
