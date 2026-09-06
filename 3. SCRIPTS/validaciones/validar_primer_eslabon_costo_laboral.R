# validar_primer_eslabon_costo_laboral.R
#
# "Primer eslabon" / manipulation check del diseño DiD: verifica si
# Exposure2022_obreros predice el aumento diferencial del COSTO LABORAL
# REAL por trabajador (salario_promedio = costo_laboral_total /
# empleo_total) en 2023, usando el panel formal YA VALIDADO
# (panel_establecimiento_formal.rds, Paso A), no una muestra nueva.
#
# *** ESTO NO ES EL RESULTADO PRINCIPAL DEL DiD ***. Es un chequeo de
# PRIMERA ETAPA: valida que la exposicion efectivamente predice el
# choque de costo que el diseño asume, antes de estimar efectos sobre
# empleo. costo_laboral_total NO se usa como outcome estandar del DiD
# por la contaminacion del pre-periodo ya documentada (ver marca
# explicita en 4. RESULTADOS/Validaciones/README.md).
#
# DERIVADO de (no reimplementado de memoria):
# - Forma funcional del estudio de evento (i(ANIO_F, exposicion_10pp,
#   ref='2015')), patron sin/con controles, helpers de exportacion:
#   IDENTICO a validaciones/validar_pretendencias_panel_formal.R --
#   unica diferencia real: ventana COMPLETA 2015-2024 (no solo
#   2015-2019) y outcome unico (asinh(salario_promedio), no las 8
#   dimensiones de esa validacion).
# - asinh() sin deflactor: MISMA logica ya aplicada a las 4 variables
#   de mecanismo/extension de validar_pretendencias_panel_formal.R --
#   el efecto fijo ANIO_F absorbe la tendencia de precios agregada
#   comun a todas las firmas en un anio dado. NO se usa el deflactor de
#   exploratorio_nicolas/construir_base_analitica_nicolas.R (nunca
#   validado).
# - Exposure2022_obreros (FIRMA), union por NORDEMP: misma decision ya
#   tomada (y verificada como equivalente en el 15/16 de las celdas) en
#   validar_pretendencias_panel_formal.R -- especificacion principal.
#
# ESPECIFICACION:
#   Sin controles:  asinh(salario_promedio) ~ i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST
#   Con controles:  asinh(salario_promedio) ~ i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F
#   cluster = ~NORDEMP en ambas. Ventana: 2015-2019+2021-2024 (2020
#   excluido, panel formal Paso A), 8 anios de coeficientes vs. 2015.
#
# PRUEBA DEL SALTO ATIPICO (Paso 3): contraste lineal manual (coef/vcov
# del propio modelo, no reimplementado con un paquete nuevo) entre el
# incremento 2022->2023 y el incremento tipico anio-a-anio 2016-2019:
#   contraste = (b_2023 - b_2022) - promedio(b_2017-b_2016, b_2018-b_2017, b_2019-b_2018)
#             = (b_2023 - b_2022) - (b_2019 - b_2016)/3   [suma telescopica]
#   SE = sqrt(w' V w), t = contraste/SE, df = fixest::degrees_freedom(modelo, "t")
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - validacion_primer_eslabon_costo_laboral.csv (F-test conjunto 2016-2024, ambas especificaciones)
# - validacion_primer_eslabon_costo_laboral_coeficientes.csv (coeficientes tidy, SE, IC95%)
# - validacion_primer_eslabon_costo_laboral_metadatos.csv (FE/controles/cluster/N)
# - validacion_primer_eslabon_contraste_salto_atipico.csv (Paso 3)
# - evento_primer_eslabon_costo_laboral.png (grafico, ambas especificaciones)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr", "ggplot2")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
if (!file.exists(panel_formal_path)) {
  stop("Falta panel_establecimiento_formal.rds. Corre construccion/construir_panel_establecimiento_formal.R primero.")
}
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) {
  stop("Falta exposicion_firma_eam.rds. Corre pipeline/02_construir_exposicion.R primero.")
}

# ------------------------------------------------------------------
# 1) Panel COMPLETO 2015-2024 (2020 excluido, ya lo esta en el panel
#    formal) + Exposure2022_obreros (firma) + asinh(salario_promedio).
# ------------------------------------------------------------------

ANIOS_PANEL <- sort(c(2015:2019, 2021:2024))

panel_formal <- readr::read_rds(panel_formal_path) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(ANIOS_PANEL)))

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

panel_built <- panel_formal %>%
  dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa), !is.na(salario_promedio)) %>%
  dplyr::mutate(
    exposicion_10pp = Exposure2022_obreros / 0.1,
    asinh_salario_promedio = asinh(salario_promedio)
  )

# ------------------------------------------------------------------
# 2) Estudio de evento COMPLETO 2015-2024, sin y con controles.
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
    filtro_muestra = "Exposure2022_obreros (firma) no NA, CIIU4 no NA, DPTO_fijo no NA, tamano_empresa no NA, salario_promedio no NA",
    variable_exposicion = "Exposure2022_obreros (FIRMA), continua, escalada a 10pp, unida por NORDEMP",
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
readr::write_csv(resultado_f, file.path(out_dir, "validacion_primer_eslabon_costo_laboral.csv"))

coeficientes_tabla <- dplyr::bind_rows(coeficientes_list)
metadatos_tabla <- dplyr::bind_rows(metadatos_list)
readr::write_csv(coeficientes_tabla, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_coeficientes.csv"))
readr::write_csv(metadatos_tabla, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_metadatos.csv"))

# ------------------------------------------------------------------
# 3) Grafico: coeficiente por anio, ambas especificaciones, para
#    inspeccion visual del salto 2022->2023.
# ------------------------------------------------------------------

p <- ggplot2::ggplot(coeficientes_tabla, ggplot2::aes(x = as.integer(sub("ANIO_F::(\\d+):.*", "\\1", term)), y = estimate, color = especificacion)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::geom_vline(xintercept = 2022.5, linetype = "dotted", color = "grey40") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  ggplot2::scale_x_continuous(breaks = ANIOS_PANEL) +
  ggplot2::labs(
    title = "Primer eslabon: efecto de +10pp de Exposure2022_obreros en asinh(salario_promedio)",
    subtitle = "Ref. 2015 | linea punteada vertical: 2022->2023 (choque de salario minimo) | NO es el resultado principal del DiD",
    x = "Anio", y = "Coeficiente (ref. 2015)", color = "Especificacion"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(file.path(out_dir, "evento_primer_eslabon_costo_laboral.png"), p, width = 11, height = 6, dpi = 180)

# ------------------------------------------------------------------
# 4) Prueba formal del salto atipico (Paso 3): contraste lineal manual.
#    contraste = (b2023 - b2022) - (b2019 - b2016)/3
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
  V <- stats::vcov(modelo)  # ya clusterizada (cluster=~NORDEMP en el feols)

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

readr::write_csv(contraste_tabla, file.path(out_dir, "validacion_primer_eslabon_contraste_salto_atipico.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("validar_primer_eslabon_costo_laboral.R -- Primer eslabon / manipulation check (NO es el resultado principal del DiD)")
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
message("Tablas y grafico exportados en: ", out_dir)
