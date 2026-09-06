# validar_primer_eslabon_costo_laboral_bite.R
#
# COPIA de validar_primer_eslabon_costo_laboral.R con UN CAMBIO
# principal: la exposicion usada es `Bite2022_obreros` (indice de
# Kaitz: SM_2023 anualizado / salario promedio de obreros en 2022) en
# vez de `Exposure2022_obreros` (composicion ocupacional). Mismo panel
# formal, misma ventana COMPLETA 2015-2024, mismo outcome
# (asinh(salario_promedio)), mismo cluster=~NORDEMP, mismos 3 controles
# con/sin, mismo test de contraste formal del Paso 3 (incremento
# 2022->2023 vs. incremento tipico 2016-2019). Guardado como script
# SEPARADO -- no sobreescribe la version de Exposure.
#
# *** ESTO NO ES EL RESULTADO PRINCIPAL DEL DiD ***, ver cabecera de
# validar_primer_eslabon_costo_laboral.R.
#
# ESCALA DE Bite2022_obreros (decision explicita, documentada porque
# NO es la misma transformacion que Exposure2022_obreros y forzarla
# no tendria sentido): Exposure2022_obreros se escalo a "+10pp"
# (Exposure/0.1) porque es una PARTICIPACION (0-1, interpretable en
# puntos porcentuales). Bite2022_obreros es un indice de Kaitz (SM /
# salario promedio, tipicamente >0, sin techo natural en 1 -- ya
# documentado en el proyecto que hay firmas con Bite muy por encima de
# 1) -- "+10pp de Bite" no tiene lectura economica clara. Se usa en su
# lugar bite_1sd = Bite2022_obreros / sd(Bite2022_obreros) (escalado
# por su propia desviacion estandar EN LA MUESTRA analitica de este
# script, SIN centrar -- el FE de NORDEST ya absorbe el nivel), para
# que cada coeficiente se lea como "efecto de +1 desviacion estandar de
# Bite2022_obreros". El estadistico F, el p-valor y la conclusion de
# atipico/no atipico del Paso 3 son INVARIANTES a esta eleccion de
# escala (un reescalamiento lineal de la variable continua reescala
# los coeficientes pero no el test conjunto ni el contraste
# estandarizado por su propio error estandar) -- la eleccion de escala
# es solo para que la MAGNITUD del coeficiente sea interpretable, no
# afecta ninguna conclusion estadistica.
#
# DERIVADO de (no reimplementado de memoria):
# - Todo lo ya citado en validar_primer_eslabon_costo_laboral.R (forma
#   funcional, patron sin/con controles, helpers, formula de
#   costo_laboral_total/salario_promedio, asinh() sin deflactor).
# - Bite2022_obreros (firma), union por NORDEMP: ya construida en
#   pipeline/02_construir_exposicion.R -- misma fuente que Exposure2022_obreros
#   en el mismo archivo exposicion_firma_eam.rds.
#
# ESPECIFICACION:
#   Sin controles:  asinh(salario_promedio) ~ i(ANIO_F, bite_1sd, ref='2015') | NORDEST
#   Con controles:  asinh(salario_promedio) ~ i(ANIO_F, bite_1sd, ref='2015') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F
#   cluster = ~NORDEMP en ambas. Ventana: 2015-2019+2021-2024 (2020
#   excluido, panel formal Paso A).
#
# PRUEBA DEL SALTO ATIPICO (Paso 3): identica logica de contraste lineal
# manual que validar_primer_eslabon_costo_laboral.R.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - validacion_primer_eslabon_costo_laboral_bite.csv
# - validacion_primer_eslabon_costo_laboral_bite_coeficientes.csv
# - validacion_primer_eslabon_costo_laboral_bite_metadatos.csv
# - validacion_primer_eslabon_contraste_salto_atipico_bite.csv
# - evento_primer_eslabon_costo_laboral_bite.png
# - primer_eslabon_comparacion_exposure_vs_bite.csv (tabla lado a lado)

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
resultado_exposure_path <- file.path(out_dir, "validacion_primer_eslabon_contraste_salto_atipico.csv")
if (!file.exists(resultado_exposure_path)) {
  stop("Falta validacion_primer_eslabon_contraste_salto_atipico.csv. Corre validaciones/validar_primer_eslabon_costo_laboral.R primero (version de Exposure).")
}

# ------------------------------------------------------------------
# 1) Panel COMPLETO 2015-2024 + Bite2022_obreros (firma) + asinh(salario_promedio).
# ------------------------------------------------------------------

ANIOS_PANEL <- sort(c(2015:2019, 2021:2024))

panel_formal <- readr::read_rds(panel_formal_path) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(ANIOS_PANEL)))

bite_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Bite2022_obreros) %>%
  dplyr::filter(!is.na(Bite2022_obreros))

panel_built_pre_escala <- panel_formal %>%
  dplyr::inner_join(bite_firma, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa), !is.na(salario_promedio))

sd_bite_muestra <- stats::sd(
  panel_built_pre_escala %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::pull(Bite2022_obreros),
  na.rm = TRUE
)

panel_built <- panel_built_pre_escala %>%
  dplyr::mutate(
    bite_1sd = Bite2022_obreros / sd_bite_muestra,
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
    "asinh_salario_promedio ~ i(ANIO_F, bite_1sd, ref = '2015') | ", fe
  ))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  anios_no_ref <- setdiff(ANIOS_PANEL, 2015)
  patron_f <- paste0("ANIO_F::(", paste(anios_no_ref, collapse = "|"), ")")
  prueba_f <- fixest::wald(modelo, keep = patron_f, print = FALSE)

  coeficientes_list[[etiqueta]] <<- extraer_coeficientes_tidy_fixest(modelo, "asinh_salario_promedio", paste0("2015 (ref.); especificacion: ", etiqueta)) %>%
    dplyr::mutate(especificacion = etiqueta, .after = variable)

  vars_regresion <- c("asinh_salario_promedio", "ANIO_F", "bite_1sd", "NORDEST")
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
    filtro_muestra = "Bite2022_obreros (firma) no NA, CIIU4 no NA, DPTO_fijo no NA, tamano_empresa no NA, salario_promedio no NA",
    variable_exposicion = paste0("Bite2022_obreros (FIRMA), continua, escalada por su propia SD en la muestra (sd=", round(sd_bite_muestra, 4), "), unida por NORDEMP"),
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
readr::write_csv(resultado_f, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_bite.csv"))

coeficientes_tabla <- dplyr::bind_rows(coeficientes_list)
metadatos_tabla <- dplyr::bind_rows(metadatos_list)
readr::write_csv(coeficientes_tabla, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_bite_coeficientes.csv"))
readr::write_csv(metadatos_tabla, file.path(out_dir, "validacion_primer_eslabon_costo_laboral_bite_metadatos.csv"))

# ------------------------------------------------------------------
# 3) Grafico.
# ------------------------------------------------------------------

p <- ggplot2::ggplot(coeficientes_tabla, ggplot2::aes(x = as.integer(sub("ANIO_F::(\\d+):.*", "\\1", term)), y = estimate, color = especificacion)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::geom_vline(xintercept = 2022.5, linetype = "dotted", color = "grey40") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  ggplot2::scale_x_continuous(breaks = ANIOS_PANEL) +
  ggplot2::labs(
    title = "Primer eslabon (Bite2022_obreros): efecto de +1 SD de Bite en asinh(salario_promedio)",
    subtitle = "Ref. 2015 | linea punteada vertical: 2022->2023 (choque de salario minimo) | NO es el resultado principal del DiD",
    x = "Anio", y = "Coeficiente (ref. 2015)", color = "Especificacion"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(file.path(out_dir, "evento_primer_eslabon_costo_laboral_bite.png"), p, width = 11, height = 6, dpi = 180)

# ------------------------------------------------------------------
# 4) Prueba formal del salto atipico (Paso 3): contraste lineal manual.
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

readr::write_csv(contraste_tabla, file.path(out_dir, "validacion_primer_eslabon_contraste_salto_atipico_bite.csv"))

# ------------------------------------------------------------------
# 5) Tabla comparativa lado a lado, Exposure vs. Bite (Paso 3, ambas
#    especificaciones) -- solo se reportan los numeros, sin interpretar.
# ------------------------------------------------------------------

contraste_exposure <- readr::read_csv(resultado_exposure_path, show_col_types = FALSE) %>%
  dplyr::rename(
    incremento_2022_2023_exposure = incremento_2022_2023,
    incremento_tipico_2016_2019_exposure = incremento_tipico_2016_2019,
    contraste_exposure = contraste,
    se_contraste_exposure = se_contraste,
    t_stat_exposure = t_stat,
    df_exposure = df,
    p_value_exposure = p_value,
    atipico_al_5pct_exposure = atipico_al_5pct
  )

contraste_bite_renombrado <- contraste_tabla %>%
  dplyr::rename(
    incremento_2022_2023_bite = incremento_2022_2023,
    incremento_tipico_2016_2019_bite = incremento_tipico_2016_2019,
    contraste_bite = contraste,
    se_contraste_bite = se_contraste,
    t_stat_bite = t_stat,
    df_bite = df,
    p_value_bite = p_value,
    atipico_al_5pct_bite = atipico_al_5pct
  )

comparacion_exposure_vs_bite <- dplyr::inner_join(contraste_exposure, contraste_bite_renombrado, by = "especificacion")

if (nrow(comparacion_exposure_vs_bite) != 2) {
  stop("La tabla de comparacion Exposure vs. Bite no tiene 2 filas (tiene ", nrow(comparacion_exposure_vs_bite), "). Revisar el join por especificacion.")
}

readr::write_csv(comparacion_exposure_vs_bite, file.path(out_dir, "primer_eslabon_comparacion_exposure_vs_bite.csv"))

# ------------------------------------------------------------------
# Reporte en consola (solo numeros, sin interpretar)
# ------------------------------------------------------------------

script_header("validar_primer_eslabon_costo_laboral_bite.R -- Primer eslabon con Bite2022_obreros (NO es el resultado principal del DiD)")
message("")
message("Escala: bite_1sd = Bite2022_obreros / sd_muestra (sd=", round(sd_bite_muestra, 4), ", n=", dplyr::n_distinct(panel_built$NORDEMP), " firmas).")
message("Panel (2015-2024, muestra filtrada): ", nrow(panel_built), " filas NORDEST-ANIO, ",
        dplyr::n_distinct(panel_built$NORDEST), " establecimientos, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("=== F conjunto 2016-2024 (ambas especificaciones) ===")
print(resultado_f, n = Inf, width = Inf)
message("")
message("=== Prueba del salto atipico 2022->2023, Bite2022_obreros (Paso 3) ===")
print(contraste_tabla, n = Inf, width = Inf)
message("")
message("=== Comparacion lado a lado, Exposure vs. Bite (solo numeros, sin interpretar) ===")
print(comparacion_exposure_vs_bite %>% dplyr::select(especificacion, contraste_exposure, p_value_exposure, atipico_al_5pct_exposure, contraste_bite, p_value_bite, atipico_al_5pct_bite), n = Inf, width = Inf)
message("")
message("Tablas y grafico exportados en: ", out_dir)
