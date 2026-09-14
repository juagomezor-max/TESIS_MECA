# validar_robustez_conjunta_grupos_exposicion.R
#
# Dos verificaciones adicionales sobre el analisis por grupos de
# exposicion (estimacion/estimar_did_por_grupos_exposicion.R), pedidas
# tras confirmar que Bite x participacion_permanente NO es robusto a
# tendencia lineal pre-existente por grupo (validar_grupos_bite_
# participacion_con_tendencia.R).
#
# PARTE 1 -- MISMO ESTANDAR YA APLICADO A BITE: para Exposure2022_obreros
# x empleo_total y x empleo_permanente (terciles y quintiles, las 2
# celdas donde Q3/Q5 o T2/T3 aparecian significativos, p~0.04), se
# repite EXACTAMENTE el mismo ejercicio que para Bite x participacion_
# permanente: agregar i(grupo, anio_lineal, ref=grupo mas bajo) junto a
# i(grupo, post_2023, ref=grupo mas bajo), mismos controles/cluster.
#
# PARTE 2 -- PRUEBA CONJUNTA (no celda por celda), las 8 combinaciones
# (4 outcomes x 2 medidas) x 2 particiones = 16 casos:
#  (a) F conjunto de TODOS los coeficientes de grupo (excluyendo la
#      referencia) = 0 -- via fixest::wald(), keep="post_2023:" sobre
#      contrastes polinomiales ortogonales (ver mas abajo).
#  (b) Test de si un termino LINEAL en el rango del grupo es suficiente:
#      se descompone el efecto de grupo en contrastes polinomiales
#      ortogonales (contr.poly(), L=lineal, Q=cuadratico, C=cubico,
#      X4=cuartico -- terciles solo tienen L y Q, k-1=2 grados de
#      libertad; quintiles tienen L,Q,C,X4, k-1=4), interactuados con
#      post_2023 en la MISMA regresion, y se hace wald() sobre los
#      terminos NO lineales (Q,C,X4) conjuntamente = 0. Si NO se
#      rechaza, el patron es consistente con una tendencia monotonica
#      lineal; si SE rechaza, hay curvatura/no-monotonicidad
#      significativa mas alla de una linea recta -- IMPORTANTE:
#      fixest NO aplica contrastes polinomiales automaticamente a
#      factores ordenados (verificado empiricamente, usa codificacion
#      dummy por defecto) -- se construyen a mano con contr.poly() y se
#      interactuan explicitamente, no se asume el comportamiento de
#      fixest.
#
# DERIVADO de (no reimplementado de memoria):
# - Grupos (terciles/quintiles), panel, controles, cluster: identico a
#   estimacion/estimar_did_por_grupos_exposicion.R.
# - Logica de agregar tendencia (Parte 1): identica a validaciones/
#   validar_grupos_bite_participacion_con_tendencia.R, aplicada aqui a
#   Exposure en vez de Bite.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - verificacion_grupos_exposure_empleo_con_tendencia.csv (Parte 1)
# - prueba_conjunta_y_linealidad_por_grupo.csv (Parte 2, las 16 filas)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal <- readr::read_rds(file.path(data_dir, "panel_establecimiento_formal.rds"))
exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))

make_terciles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))
  if (length(valid) < 3 || dplyr::n_distinct(x[valid]) < 3) return(out)
  terc <- dplyr::ntile(x[valid], 3)
  labels <- c("T1 - Baja", "T2 - Media", "T3 - Alta")
  out[valid] <- labels[terc]
  factor(out, levels = labels, ordered = TRUE)
}

exposicion_firma <- exposicion_firma %>%
  dplyr::mutate(tercil_exposure2022_obreros = make_terciles(Exposure2022_obreros),
                tercil_bite2022_obreros = make_terciles(Bite2022_obreros))

datos_exposure <- panel_formal %>%
  dplyr::inner_join(
    exposicion_firma %>% dplyr::select(NORDEMP, tercil_exposure2022_obreros, quintil_exposure2022_obreros) %>%
      dplyr::filter(!is.na(quintil_exposure2022_obreros)),
    by = "NORDEMP"
  ) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023))

datos_bite <- panel_formal %>%
  dplyr::inner_join(
    exposicion_firma %>% dplyr::select(NORDEMP, tercil_bite2022_obreros, quintil_bite2022_obreros) %>%
      dplyr::filter(!is.na(quintil_bite2022_obreros)),
    by = "NORDEMP"
  ) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023))

FE_CON <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"

# ====================================================================
# PARTE 1: Exposure x empleo_total / empleo_permanente, con tendencia.
# ====================================================================

correr_par_tendencia <- function(data, var_grupo, ref_label, var_y, particion) {
  f_original <- stats::as.formula(paste0(var_y, " ~ i(", var_grupo, ", post_2023, ref = '", ref_label, "') | ", FE_CON))
  m_original <- fixest::feols(f_original, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  f_tendencia <- stats::as.formula(paste0(
    var_y, " ~ i(", var_grupo, ", post_2023, ref = '", ref_label, "') + i(", var_grupo, ", anio_lineal, ref = '", ref_label, "') | ", FE_CON
  ))
  m_tendencia <- fixest::feols(f_tendencia, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  ct_orig <- summary(m_original)$coeftable
  ct_tend <- summary(m_tendencia)$coeftable
  filas_post_tend <- grep(":post_2023$", rownames(ct_tend))
  filas_tend_tend <- grep(":anio_lineal$", rownames(ct_tend))

  tibble::tibble(
    variable = var_y,
    particion = particion,
    grupo = sub(paste0("^", var_grupo, "::"), "", sub(":post_2023$", "", rownames(ct_orig))),
    post_estimate_original = round(unname(ct_orig[, 1]), 6),
    post_p_original = signif(unname(ct_orig[, 4]), 4),
    post_estimate_con_tendencia = round(unname(ct_tend[filas_post_tend, 1]), 6),
    post_p_con_tendencia = signif(unname(ct_tend[filas_post_tend, 4]), 4),
    tendencia_p = signif(unname(ct_tend[filas_tend_tend, 4]), 4),
    post_sig_original = unname(ct_orig[, 4]) < 0.05,
    post_sig_con_tendencia = unname(ct_tend[filas_post_tend, 4]) < 0.05
  )
}

parte1 <- dplyr::bind_rows(
  correr_par_tendencia(datos_exposure, "tercil_exposure2022_obreros", "T1 - Baja", "empleo_total", "Terciles"),
  correr_par_tendencia(datos_exposure, "quintil_exposure2022_obreros", "Q1 - Muy baja", "empleo_total", "Quintiles"),
  correr_par_tendencia(datos_exposure, "tercil_exposure2022_obreros", "T1 - Baja", "empleo_permanente", "Terciles"),
  correr_par_tendencia(datos_exposure, "quintil_exposure2022_obreros", "Q1 - Muy baja", "empleo_permanente", "Quintiles")
) %>%
  dplyr::mutate(medida = "Exposure2022_obreros", .before = 1)

readr::write_csv(parte1, file.path(out_dir, "verificacion_grupos_exposure_empleo_con_tendencia.csv"))

# ====================================================================
# PARTE 2: prueba conjunta + linealidad, 8 combinaciones x 2 particiones.
# ====================================================================

outcomes_info <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")

construir_contrastes <- function(niveles) {
  k <- length(niveles)
  cp <- stats::contr.poly(k)
  nombres_grado <- c("L", "Q", "C", paste0("X", 4:10))[1:(k - 1)]
  colnames(cp) <- nombres_grado
  df <- as.data.frame(cp)
  df$grupo <- niveles
  list(df = df, nombres_grado = nombres_grado)
}

correr_conjunta_linealidad <- function(data, var_grupo, medida, particion, var_y) {
  niveles <- levels(data[[var_grupo]])
  contrastes <- construir_contrastes(niveles)
  data_con_contrastes <- data %>%
    dplyr::mutate(.grupo_chr = as.character(.data[[var_grupo]])) %>%
    dplyr::left_join(contrastes$df %>% dplyr::rename(.grupo_chr = grupo), by = ".grupo_chr")

  terminos <- paste0("post_2023:", contrastes$nombres_grado)
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ ", paste(terminos, collapse = " + "), " | ", FE_CON))
  modelo <- fixest::feols(formula_modelo, data = data_con_contrastes, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  prueba_conjunta <- fixest::wald(modelo, keep = "post_2023:", print = FALSE)

  terminos_no_lineales <- contrastes$nombres_grado[-1]  # todos menos "L"
  if (length(terminos_no_lineales) == 0) {
    prueba_linealidad <- list(stat = NA_real_, p = NA_real_, df1 = NA_integer_, df2 = NA_real_)
  } else {
    patron_no_lineal <- paste0("post_2023:(", paste(terminos_no_lineales, collapse = "|"), ")$")
    prueba_linealidad <- fixest::wald(modelo, keep = patron_no_lineal, print = FALSE)
  }

  tibble::tibble(
    medida = medida, particion = particion, variable = var_y,
    n_grupos = length(niveles), n_obs = stats::nobs(modelo),
    f_conjunto = round(prueba_conjunta$stat, 3), df1_conjunto = prueba_conjunta$df1, df2_conjunto = round(prueba_conjunta$df2, 1),
    p_conjunto = signif(prueba_conjunta$p, 4), rechaza_conjunto_al_5pct = prueba_conjunta$p < 0.05,
    f_no_lineal = round(prueba_linealidad$stat, 3), df1_no_lineal = prueba_linealidad$df1, df2_no_lineal = round(prueba_linealidad$df2, 1),
    p_no_lineal = signif(prueba_linealidad$p, 4),
    lineal_es_suficiente = ifelse(is.na(prueba_linealidad$p), NA, prueba_linealidad$p >= 0.05)
  )
}

combinaciones2 <- tibble::tribble(
  ~medida, ~particion, ~data_ref, ~var_grupo,
  "Exposure2022_obreros", "Terciles", "exposure", "tercil_exposure2022_obreros",
  "Exposure2022_obreros", "Quintiles", "exposure", "quintil_exposure2022_obreros",
  "Bite2022_obreros", "Terciles", "bite", "tercil_bite2022_obreros",
  "Bite2022_obreros", "Quintiles", "bite", "quintil_bite2022_obreros"
)

parte2 <- purrr::pmap_dfr(combinaciones2, function(medida, particion, data_ref, var_grupo) {
  data <- if (data_ref == "exposure") datos_exposure else datos_bite
  purrr::map_dfr(outcomes_info, function(var_y) {
    correr_conjunta_linealidad(data, var_grupo, medida, particion, var_y)
  })
})

readr::write_csv(parte2, file.path(out_dir, "prueba_conjunta_y_linealidad_por_grupo.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("validar_robustez_conjunta_grupos_exposicion.R -- Parte 1 (Exposure+tendencia) y Parte 2 (prueba conjunta + linealidad, 16 casos)")
message("")
message("=== PARTE 1: Exposure x empleo_total/empleo_permanente, con y sin tendencia ===")
print(parte1 %>% dplyr::select(variable, particion, grupo, post_estimate_original, post_p_original, post_estimate_con_tendencia, post_p_con_tendencia, tendencia_p), n = Inf, width = Inf)
message("")
message("=== PARTE 2: prueba F conjunta y test de linealidad, las 16 combinaciones ===")
print(parte2 %>% dplyr::select(medida, particion, variable, f_conjunto, p_conjunto, rechaza_conjunto_al_5pct, f_no_lineal, p_no_lineal, lineal_es_suficiente), n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
