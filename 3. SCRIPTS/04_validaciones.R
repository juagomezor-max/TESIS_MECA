# 04_validaciones.R -- Pipeline simplificado (rama `simplificacion`).
#
# Valida los 2 supuestos de identificacion del diseño DiD:
# (A) Tendencias paralelas 2015-2019, para Exposure2022_obreros y
#     Bite2022_obreros, en las 4 dimensiones de empleo.
# (B) Atricion diferencial 2022->2023/2024, con PLACEBO pre-choque
#     2017->2018/2019.
#
# REGLA INNEGOCIABLE: SIEMPRE cluster = ~NORDEMP en toda especificacion
# de regresion de este script, sin excepcion -- la inconsistencia entre
# inferencia IID y clusterizada en la version anterior de la validacion
# de Bite2022_obreros produjo una conclusion erronea ("rechaza en 3 de 4
# dimensiones") que hubo que retractar (ver INDICE_RESULTADOS.md, fila 29
# de la rama feature/estimacion-preliminar, commit 397e349). Los chequeos
# de atricion (C) NO son regresiones -- son proporciones binomiales por
# celda quintil-anio, con error estandar analitico sqrt(p(1-p)/n); no hay
# variable de cluster que aplicar ahi (celdas independientes por
# construccion), pero se documenta explicitamente para que quede claro
# que no es una omision de la regla.
#
# DERIVADO de (no reimplementado de memoria):
# - (A) Exposure: validar_tendencias_paralelas_empleo_exposure_grafico.R
#   (formula, FE, cluster -- SIN cambios, esa version ya clusterizaba).
# - (A) Bite: validar_tendencias_paralelas_empleo_bite.R **version
#   CORREGIDA** (rama feature/estimacion-preliminar, commit 397e349, NO
#   la version de main que aun tiene IID) -- unico cambio respecto a la
#   version de main fue agregar cluster=~NORDEMP, documentado y
#   verificado ese mismo dia (comparar_inferencia_iid_vs_cluster_bite.R).
# - (B) diagnostico_atricion_diferencial_exposicion_eam.R (formula de
#   tasa de salida y brecha Q5-Q1) + extender_diagnostico_atricion_diferencial.R
#   partes (a) y (c) (error estandar analitico, IC 95%, placebo 2017-2018/2019).
#   NO se incluyen las partes (b) [especificacion continua] ni (d)
#   [descomposicion por umbral] de la extension -- no forman parte de
#   ninguna cifra de CIFRAS_CLAVE.csv y quedan fuera del pipeline MINIMO;
#   siguen disponibles en el historial (tag panel-establecimiento-v1 y
#   rama feature/estimacion-preliminar).
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - simplificado_pretendencias_exposure.csv
# - simplificado_pretendencias_bite.csv
# - simplificado_atricion_real_2022_2023_2024.csv
# - simplificado_atricion_placebo_2017_2018_2019.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_path <- file.path(data_dir, "panel_analitico_firma_eam.rds")
panel_firma_path <- file.path(data_dir, "panel_firma_eam.rds")
if (!file.exists(panel_path)) stop("Falta panel_analitico_firma_eam.rds. Corre 03_construir_panel.R primero.")
if (!file.exists(panel_firma_path)) stop("Falta panel_firma_eam.rds. Corre 01_construir_base.R primero.")

panel_analitico <- readr::read_rds(panel_path)
panel_firma <- readr::read_rds(panel_firma_path)

metrics_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total", "Empleo total",
  "empleo_permanente", "Empleo permanente",
  "empleo_temporal", "Empleo temporal (directo + agencia)",
  "participacion_permanente", "Participacion de permanentes (%)"
)

# ====================================================================
# (A) TENDENCIAS PARALELAS 2015-2019
# ====================================================================

panel_pre_2019 <- panel_analitico %>%
  dplyr::filter(ANIO %in% 2015:2019) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(2015:2019)))

# --- Exposure2022_obreros: event study continuo ---

panel_exposure <- panel_pre_2019 %>%
  dplyr::filter(!is.na(exposicion_10pp), !is.na(CIIU4), !is.na(DPTO))

correr_evento_exposure <- function(data, var_y) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, exposicion_10pp, ref = '2015') | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F"
  ))
  fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}

resultado_exposure <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  modelo <- correr_evento_exposure(panel_exposure, metric$var)
  prueba_f <- fixest::wald(modelo, keep = "ANIO_F::(2016|2017|2018|2019)", print = FALSE)
  tibble::tibble(
    variable = metric$var,
    medida = "Exposure2022_obreros",
    n_obs = stats::nobs(modelo),
    n_firmas = dplyr::n_distinct(panel_exposure$NORDEMP[!is.na(panel_exposure[[metric$var]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4)
  )
})

readr::write_csv(resultado_exposure, file.path(out_dir, "simplificado_pretendencias_exposure.csv"))

# --- Bite2022_obreros: quintiles x anio_lineal, CON controles (misma
#     especificacion que la columna "Con sector*anio + departamento*anio"
#     de la version corregida, para comparar con Exposure en igualdad de
#     condiciones -- Exposure siempre incluye esos controles). ---

panel_bite <- panel_pre_2019 %>%
  dplyr::filter(!is.na(quintil_bite2022_obreros), !is.na(CIIU4), !is.na(DPTO)) %>%
  dplyr::mutate(quintil_bite2022_obreros = factor(quintil_bite2022_obreros))

correr_evento_bite <- function(data, var_y) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ anio_lineal + i(quintil_bite2022_obreros, anio_lineal, ref = 'Q1 - Muy baja') | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F"
  ))
  fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}

resultado_bite <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  modelo <- correr_evento_bite(panel_bite, metric$var)
  prueba_f <- fixest::wald(modelo, keep = "quintil_bite2022_obreros", print = FALSE)
  tibble::tibble(
    variable = metric$var,
    medida = "Bite2022_obreros",
    n_obs = stats::nobs(modelo),
    n_firmas = dplyr::n_distinct(panel_bite$NORDEMP[!is.na(panel_bite[[metric$var]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4)
  )
})

readr::write_csv(resultado_bite, file.path(out_dir, "simplificado_pretendencias_bite.csv"))

# ====================================================================
# (B) ATRICION DIFERENCIAL: real (2022->2023/2024) y placebo (2017->2018/2019)
# ====================================================================

exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_firma_eam.rds. Corre 02_construir_exposicion.R primero.")
exposicion_firma <- readr::read_rds(exposicion_firma_path)

winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}
make_quintiles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))
  if (length(valid) < 5 || dplyr::n_distinct(x[valid]) < 5) return(out)
  quint <- dplyr::ntile(x[valid], 5)
  labels <- c("Q1 - Muy baja", "Q2 - Baja", "Q3 - Media", "Q4 - Alta", "Q5 - Muy alta")
  out[valid] <- labels[quint]
  factor(out, levels = labels, ordered = TRUE)
}
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)

exposicion_en_anio_base <- function(anio_base) {
  if (anio_base == 2022) {
    return(exposicion_firma %>% dplyr::select(NORDEMP, Exposure2022_obreros, quintil_exposure2022_obreros))
  }
  panel_firma %>%
    dplyr::filter(ANIO == anio_base) %>%
    dplyr::mutate(
      total_obreros = rowSums(dplyr::across(dplyr::all_of(cols_obreros)), na.rm = TRUE),
      total_administrativos = rowSums(dplyr::across(dplyr::all_of(cols_administrativos)), na.rm = TRUE),
      total_prof_tecnico = rowSums(dplyr::across(dplyr::all_of(cols_prof_tecnico)), na.rm = TRUE),
      empleo_total_categorias = total_obreros + total_administrativos + total_prof_tecnico,
      participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias)
    ) %>%
    dplyr::transmute(NORDEMP, Exposure2022_obreros = winsorize(participacion_obreros_raw)) %>%
    dplyr::mutate(quintil_exposure2022_obreros = make_quintiles(Exposure2022_obreros))
}

calcular_atricion <- function(anio_base, anios_seguimiento, etiqueta_anio_base) {
  cohorte <- exposicion_en_anio_base(anio_base) %>%
    dplyr::filter(!is.na(quintil_exposure2022_obreros))

  purrr::map_dfr(anios_seguimiento, function(anio_seg) {
    nordemp_ese_anio <- panel_firma %>% dplyr::filter(ANIO == anio_seg) %>% dplyr::pull(NORDEMP) %>% unique()
    datos <- cohorte %>% dplyr::mutate(sale = as.integer(!NORDEMP %in% nordemp_ese_anio))

    tabla <- datos %>%
      dplyr::group_by(quintil = as.character(quintil_exposure2022_obreros)) %>%
      dplyr::summarise(
        n_firmas = dplyr::n(),
        tasa_salida_pct = round(100 * mean(sale), 3),
        se_pct = round(100 * sqrt(mean(sale) * (1 - mean(sale)) / dplyr::n()), 3),
        .groups = "drop"
      ) %>%
      dplyr::mutate(anio_seguimiento = anio_seg, anio_base = etiqueta_anio_base)

    q1 <- tabla %>% dplyr::filter(quintil == "Q1 - Muy baja")
    q5 <- tabla %>% dplyr::filter(quintil == "Q5 - Muy alta")
    if (nrow(q1) == 1 && nrow(q5) == 1) {
      brecha_pp <- round(q5$tasa_salida_pct - q1$tasa_salida_pct, 3)
      se_brecha <- round(sqrt(q5$se_pct^2 + q1$se_pct^2), 3)
      tabla$brecha_q5_q1_pp <- brecha_pp
      tabla$se_brecha_pp <- se_brecha
      tabla$ic95_bajo <- round(brecha_pp - 1.96 * se_brecha, 3)
      tabla$ic95_alto <- round(brecha_pp + 1.96 * se_brecha, 3)
    }
    tabla
  })
}

resultado_real <- calcular_atricion(2022, c(2023, 2024), "2022 (real, post-choque)")
resultado_placebo <- calcular_atricion(2017, c(2018, 2019), "2017 (placebo, pre-choque)")

readr::write_csv(resultado_real, file.path(out_dir, "simplificado_atricion_real_2022_2023_2024.csv"))
readr::write_csv(resultado_placebo, file.path(out_dir, "simplificado_atricion_placebo_2017_2018_2019.csv"))

# ====================================================================
# Reporte en consola
# ====================================================================

script_header("04_validaciones.R -- Tendencias paralelas y atricion diferencial (cluster=~NORDEMP siempre)")

message("\n=== (A) Tendencias paralelas 2015-2019: Exposure2022_obreros ===")
print(resultado_exposure, n = Inf, width = Inf)
message("\n=== (A) Tendencias paralelas 2015-2019: Bite2022_obreros (con controles) ===")
print(resultado_bite, n = Inf, width = Inf)
message("\n=== (B) Atricion real 2022->2023/2024 (brecha Q5-Q1) ===")
print(resultado_real %>% dplyr::distinct(anio_seguimiento, brecha_q5_q1_pp, ic95_bajo, ic95_alto), n = Inf, width = Inf)
message("\n=== (B) Atricion placebo 2017->2018/2019 (brecha Q5-Q1) ===")
print(resultado_placebo %>% dplyr::distinct(anio_seguimiento, brecha_q5_q1_pp, ic95_bajo, ic95_alto), n = Inf, width = Inf)

message("\nTablas exportadas en: ", out_dir)
