# validar_primer_eslabon_costo_laboral_exposure_est.R
#
# COPIA de validar_primer_eslabon_costo_laboral.R con UN SOLO CAMBIO: la
# exposicion usada es `Exposure2022_obreros_est` (propia de cada
# ESTABLECIMIENTO, union DIRECTA por NORDEST) en vez de
# `Exposure2022_obreros` (de la FIRMA duena, union por NORDEMP) --
# mismo patron de robustez ya aplicado en
# validar_pretendencias_panel_formal_exposure_est.R. Misma ventana
# 2015-2024, mismos 3 controles con/sin, mismo cluster (=~NORDEMP),
# mismo outcome (asinh(salario_promedio)), mismo contraste del salto
# atipico (Paso 3). Guardado como script SEPARADO -- no sobreescribe la
# version de firma.
#
# *** ESTO NO ES EL RESULTADO PRINCIPAL DEL DiD ***, ver cabecera de
# validar_primer_eslabon_costo_laboral.R.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - validacion_primer_eslabon_costo_laboral_exposure_est.csv
# - validacion_primer_eslabon_costo_laboral_exposure_est_coeficientes.csv
# - validacion_primer_eslabon_costo_laboral_exposure_est_metadatos.csv
# - validacion_primer_eslabon_contraste_salto_atipico_exposure_est.csv

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
if (!file.exists(panel_formal_path)) {
  stop("Falta panel_establecimiento_formal.rds. Corre construccion/construir_panel_establecimiento_formal.R primero.")
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

ANIO_BASE <- 2022
ANIOS_PANEL <- sort(c(2015:2019, 2021:2024))

# ------------------------------------------------------------------
# 1) Exposure2022_obreros_est (propia del establecimiento, ANIO_BASE=2022),
#    recalculada desde la macrobase -- IDENTICO a
#    validar_pretendencias_panel_formal_exposure_est.R.
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_necesarias <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", cols_necesarias))

base_establecimiento <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, dplyr::all_of(cols_necesarias)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_necesarias), safe_numeric))

baseline_est_2022 <- base_establecimiento %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::mutate(
    total_obreros = rowSums(dplyr::across(dplyr::all_of(cols_obreros)), na.rm = TRUE),
    total_administrativos = rowSums(dplyr::across(dplyr::all_of(cols_administrativos)), na.rm = TRUE),
    total_prof_tecnico = rowSums(dplyr::across(dplyr::all_of(cols_prof_tecnico)), na.rm = TRUE),
    empleo_total_categorias = total_obreros + total_administrativos + total_prof_tecnico,
    participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias)
  ) %>%
  dplyr::transmute(NORDEST, Exposure2022_obreros_est = winsorize(participacion_obreros_raw)) %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est))

# ------------------------------------------------------------------
# 2) Panel formal (Paso A) 2015-2024 + Exposure2022_obreros_est (union
#    DIRECTA por NORDEST).
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(ANIOS_PANEL)))

panel_built <- panel_formal %>%
  dplyr::inner_join(baseline_est_2022, by = "NORDEST") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa), !is.na(salario_promedio)) %>%
  dplyr::mutate(
    exposicion_10pp = Exposure2022_obreros_est / 0.1,
    asinh_salario_promedio = asinh(salario_promedio)
  )

# ------------------------------------------------------------------
# 3) Estudio de evento COMPLETO 2015-2024, sin y con controles.
# ------------------------------------------------------------------

coeficientes_list <- list()
metadatos_list <- list()

correr_evento <- function(data, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEST" else paste("NORDEST", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(
    "asinh_salario_promedio ~ i(ANIO_F, exposicion_10pp, ref = '2015') | ", fe
  ))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  anios_no_ref <- setdiff(ANIOS_PANEL, 2015)
  patron_f <- paste0("ANIO_F::(", paste(anios_no_ref, collapse = "|"), ")")
  prueba_f <- fixest::wald(modelo, keep = patron_f, print = FALSE)

  coeficientes_list[[etiqueta]] <<- extraer_coeficientes_tidy_fixest(modelo, "asinh_salario_promedio", paste0("2015 (ref.); especificacion: ", etiqueta)) %>%
    dplyr::mutate(especificacion = etiqueta, .after = variable)

  vars_regresion <- c("asinh_salario_promedio", "ANIO_F", "exposicion_10pp", "NORDEST")
  if (!is.null(fe_adicionales)) vars_regresion <- c(vars_regresion, "CIIU4", "tamano_empresa", "DPTO_fijo")
  clusters_info <- contar_clusters_fixest(modelo, data, "NORDEMP", vars_regresion)

  metadatos_list[[etiqueta]] <<- tibble::tibble(
    especificacion = etiqueta,
    efectos_fijos = fe,
    controles = if (is.null(fe_adicionales)) "Ninguno" else "sector (CIIU4) x anio, tamano_empresa x anio, departamento (DPTO_fijo) x anio",
    variable_cluster = "NORDEMP",
    n_clusters = clusters_info$n_clusters,
    n_obs = stats::nobs(modelo),
    n_obs_reconstruido_coincide = clusters_info$coincide_con_modelo,
    ventana = "2015-2024 (2020 excluido) -- COMPLETA, no solo pre-choque",
    filtro_muestra = "Exposure2022_obreros_est (establecimiento) no NA, CIIU4 no NA, DPTO_fijo no NA, tamano_empresa no NA, salario_promedio no NA",
    variable_exposicion = "Exposure2022_obreros_est (ESTABLECIMIENTO, no firma), continua, escalada a 10pp, unida DIRECTO por NORDEST",
    outcome = "asinh(salario_promedio) = asinh(costo_laboral_total / empleo_total), SIN deflactar (ANIO_F absorbe tendencia de precios)"
  )

  list(
    modelo = modelo,
    resumen_f = tibble::tibble(
      especificacion = etiqueta,
      n_obs = stats::nobs(modelo),
      n_establecimientos = dplyr::n_distinct(data$NORDEST),
      n_firmas = dplyr::n_distinct(data$NORDEMP),
      f_stat = round(prueba_f$stat, 3),
      df1 = prueba_f$df1,
      df2 = round(prueba_f$df2, 1),
      p_value = signif(prueba_f$p, 4)
    )
  )
}

resultado_sin <- correr_evento(panel_built, "Sin controles")
resultado_con <- correr_evento(panel_built, "Con sector*anio + tamano*anio + departamento*anio",
                                "CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F")

resultado_f <- dplyr::bind_rows(resultado_sin$resumen_f, resultado_con$resumen_f)
readr::write_csv(resultado_f, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_exposure_est.csv"))

coeficientes_tabla <- dplyr::bind_rows(coeficientes_list)
metadatos_tabla <- dplyr::bind_rows(metadatos_list)
readr::write_csv(coeficientes_tabla, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_exposure_est_coeficientes.csv"))
readr::write_csv(metadatos_tabla, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_exposure_est_metadatos.csv"))

# ------------------------------------------------------------------
# 4) Prueba formal del salto atipico (Paso 3, identica logica).
# ------------------------------------------------------------------

contraste_salto_atipico <- function(modelo, etiqueta) {
  nombres <- names(stats::coef(modelo))
  buscar <- function(anio) {
    hallado <- grep(paste0("^ANIO_F::", anio, ":"), nombres, value = TRUE)
    if (length(hallado) != 1) stop("No se encontro (o se encontro mas de 1) coeficiente para ANIO_F::", anio, " en '", etiqueta, "'.")
    hallado
  }
  n2016 <- buscar(2016); n2019 <- buscar(2019); n2022 <- buscar(2022); n2023 <- buscar(2023)

  b <- stats::coef(modelo)
  V <- stats::vcov(modelo)

  w <- setNames(rep(0, length(b)), nombres)
  w[n2016] <- 1/3
  w[n2019] <- -1/3
  w[n2022] <- -1
  w[n2023] <- 1

  estimate <- as.numeric(sum(w * b))
  se <- as.numeric(sqrt(t(w) %*% V %*% w))
  t_stat <- estimate / se
  df <- fixest::degrees_freedom(modelo, type = "t")
  p_value <- 2 * stats::pt(-abs(t_stat), df = df)

  tibble::tibble(
    especificacion = etiqueta,
    incremento_2022_2023 = round(unname(b[n2023]) - unname(b[n2022]), 6),
    incremento_tipico_2016_2019 = round((unname(b[n2019]) - unname(b[n2016])) / 3, 6),
    contraste = round(estimate, 6),
    se_contraste = round(se, 6),
    t_stat = round(t_stat, 3),
    df = round(df, 1),
    p_value = signif(p_value, 4),
    atipico_al_5pct = p_value < 0.05
  )
}

contraste_sin <- contraste_salto_atipico(resultado_sin$modelo, "Sin controles")
contraste_con <- contraste_salto_atipico(resultado_con$modelo, "Con sector*anio + tamano*anio + departamento*anio")
contraste_tabla <- dplyr::bind_rows(contraste_sin, contraste_con)

readr::write_csv(contraste_tabla, file.path(out_dir, "validacion_primer_eslabon_contraste_salto_atipico_exposure_est.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("validar_primer_eslabon_costo_laboral_exposure_est.R -- Robustez con Exposure2022_obreros_est")
message("")
message("Panel (2015-2024, muestra filtrada): ", nrow(panel_built), " filas NORDEST-ANIO, ",
        dplyr::n_distinct(panel_built$NORDEST), " establecimientos, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("=== F conjunto 2016-2024 (ambas especificaciones) ===")
print(resultado_f, n = Inf, width = Inf)
message("")
message("=== Prueba del salto atipico 2022->2023 (Paso 3) ===")
print(contraste_tabla, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
