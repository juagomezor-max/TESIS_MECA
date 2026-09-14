# validar_pretendencias_panel_formal_ventana_completa.R
#
# Re-corre la validacion de pre-tendencias de validar_pretendencias_
# panel_formal.R, pero con la ventana PRE COMPLETA (2015-2019 + 2021-2022,
# excluyendo solo 2020 y el post-periodo 2023-2024) en vez de solo
# 2015-2019. NO se reinterpreta ni se ajusta nada del panel ni de la
# especificacion -- es UNICAMENTE ampliar la ventana temporal del test
# F conjunto ya existente. Los numeros se reportan crudos; la
# interpretacion se hace en una conversacion posterior.
#
# MOTIVO (2026-09-06, no omitido): el event study completo (ref=2022)
# de la estimacion principal (4. RESULTADOS/Estimacion_DiD/event_study_
# completo_coeficientes.csv) revela que, para participacion_permanente
# CON controles, la brecha respecto a 2015 (coef(t) - coef(2015), ya
# que ese event study usa ref=2022, no ref=2015) crece de -0.167 en
# 2019 a -0.443 en 2021 -- casi se triplica entre el fin de la ventana
# de pre-tendencias YA VALIDADA (2015-2019) y el resto del periodo pre
# (2021-2022) que NUNCA se incluyo en esa prueba F conjunta original.
# Esto es sospechoso porque el test de pre-tendencias ya hecho
# (validar_pretendencias_panel_formal.R) solo cubrio 2016-2019 -- si la
# divergencia real esta concentrada en 2021-2022, ese test corto pudo
# no detectarla.
#
# DERIVADO de (no reimplementado de memoria):
# - Forma funcional, patron sin/con controles, helpers de exportacion,
#   panel formal, Exposure2022_obreros (firma): IDENTICO a
#   validaciones/validar_pretendencias_panel_formal.R -- UNICO cambio
#   real es la ventana temporal del filtro y del F-test conjunto (6
#   anios pre en vez de 4).
# - Resultado de la ventana CORTA (2015-2019), usado como comparacion:
#   LEIDO del archivo ya versionado
#   4. RESULTADOS/Validaciones/validacion_pretendencias_panel_formal.csv
#   (filas tipo_dimension=="principal"), NO recalculado de memoria.
#
# ESPECIFICACION (identica a validar_pretendencias_panel_formal.R,
# salvo la ventana):
#   Sin controles:  Y ~ i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST
#   Con controles:  Y ~ i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F
#   cluster = ~NORDEMP en ambas. Ventana PRE COMPLETA: 2015-2019+2021-2022
#   (6 anios no-referencia: 2016,2017,2018,2019,2021,2022 -- NO 2023-2024,
#   que es post-tratamiento). F conjunto sobre los 6, no solo 4.
#
# Si la conclusion (rechaza/no rechaza al 5%) cambia para alguna
# dimension al ampliar la ventana, se repite el MISMO ejercicio
# (ventana corta vs. completa, sin/con controles) con Bite2022_obreros
# para esa(s) dimension(es) especifica(s) -- calculado fresco sobre el
# MISMO panel formal y los MISMOS 3 controles (no se reusa el resultado
# de matriz_comparacion_funcional_exposure_bite.csv, que corrio sobre
# el panel de FIRMA con solo 2 controles -- no es directamente
# comparable a esta especificacion de 3 controles sobre el panel
# formal de establecimiento).
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - comparacion_pretendencias_ventana_corta_vs_completa.csv (Exposure, 4 outcomes x 2 especificaciones)
# - validacion_pretendencias_ventana_completa_bite.csv (SOLO si hay flip, outcomes especificos)

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
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) {
  stop("Falta exposicion_firma_eam.rds. Corre pipeline/02_construir_exposicion.R primero.")
}
resultado_corto_path <- file.path(out_dir, "validacion_pretendencias_panel_formal.csv")
if (!file.exists(resultado_corto_path)) {
  stop("Falta validacion_pretendencias_panel_formal.csv. Corre validaciones/validar_pretendencias_panel_formal.R primero (ventana corta).")
}

ANIOS_PRE_COMPLETO <- c(2015:2019, 2021, 2022)  # excluye 2020 (pandemia) y 2023-2024 (post)

# ------------------------------------------------------------------
# 1) Panel PRE completo + Exposure2022_obreros (firma) + Bite2022_obreros
#    (firma, ya en el mismo archivo, se usa solo si hace falta).
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path) %>%
  dplyr::filter(ANIO %in% ANIOS_PRE_COMPLETO) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(ANIOS_PRE_COMPLETO)))

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

panel_built <- panel_formal %>%
  dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(exposicion_10pp = Exposure2022_obreros / 0.1)

metrics_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total",             "Empleo total",
  "empleo_permanente",        "Empleo permanente",
  "empleo_temporal",          "Empleo temporal (directo + agencia)",
  "participacion_permanente", "Participacion de permanentes en el empleo total (%)"
)

# ------------------------------------------------------------------
# 2) Estudio de evento, ventana PRE completa, sin y con controles,
#    F conjunto sobre los 6 anios no-referencia.
# ------------------------------------------------------------------

ANIOS_NO_REF <- setdiff(ANIOS_PRE_COMPLETO, 2015)
PATRON_F <- paste0("ANIO_F::(", paste(ANIOS_NO_REF, collapse = "|"), ")")

correr_evento_ventana <- function(data, var_y, var_x, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEST" else paste("NORDEST", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, ", var_x, ", ref = '2015') | ", fe
  ))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  prueba_f <- fixest::wald(modelo, keep = PATRON_F, print = FALSE)

  tibble::tibble(
    variable = var_y,
    especificacion = etiqueta,
    n_obs = stats::nobs(modelo),
    n_establecimientos = dplyr::n_distinct(data$NORDEST[!is.na(data[[var_y]])]),
    n_firmas = dplyr::n_distinct(data$NORDEMP[!is.na(data[[var_y]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4)
  )
}

resultado_ventana_completa_exposure <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  dplyr::bind_rows(
    correr_evento_ventana(panel_built, metric$var, "exposicion_10pp", "Sin controles"),
    correr_evento_ventana(panel_built, metric$var, "exposicion_10pp", "Con sector*anio + tamano*anio + departamento*anio",
                           "CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F")
  )
})

# ------------------------------------------------------------------
# 3) Comparacion lado a lado contra la ventana CORTA ya documentada.
# ------------------------------------------------------------------

ALFA <- 0.05

resultado_corto <- readr::read_csv(resultado_corto_path, show_col_types = FALSE) %>%
  dplyr::filter(tipo_dimension == "principal") %>%
  dplyr::select(variable, especificacion, f_stat_ventana_corta = f_stat, p_value_ventana_corta = p_value)

comparacion <- resultado_corto %>%
  dplyr::inner_join(
    resultado_ventana_completa_exposure %>% dplyr::select(variable, especificacion, f_stat_ventana_completa = f_stat, p_value_ventana_completa = p_value),
    by = c("variable", "especificacion")
  ) %>%
  dplyr::mutate(
    rechaza_ventana_corta = p_value_ventana_corta < ALFA,
    rechaza_ventana_completa = p_value_ventana_completa < ALFA,
    cambia_conclusion = rechaza_ventana_corta != rechaza_ventana_completa
  )

if (nrow(comparacion) != 8) {
  stop("La tabla de comparacion no tiene 8 filas (tiene ", nrow(comparacion), "). Revisar el join ventana corta vs. completa.")
}

readr::write_csv(comparacion, file.path(out_dir, "comparacion_pretendencias_ventana_corta_vs_completa.csv"))

outcomes_con_flip <- comparacion %>% dplyr::filter(cambia_conclusion) %>% dplyr::distinct(variable) %>% dplyr::pull(variable)

# ------------------------------------------------------------------
# 4) Si algun outcome cambia de conclusion, repetir el MISMO ejercicio
#    (ventana corta vs. completa, sin/con controles) con Bite2022_obreros,
#    fresco sobre el MISMO panel formal y los MISMOS 3 controles.
# ------------------------------------------------------------------

resultado_bite <- NULL
if (length(outcomes_con_flip) > 0) {
  panel_bite <- panel_formal %>%
    dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
    dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa), !is.na(Bite2022_obreros))

  ventanas <- list(
    "Ventana corta (2015-2019)" = 2015:2019,
    "Ventana PRE completa (2015-2019+2021-2022)" = ANIOS_PRE_COMPLETO
  )

  resultado_bite <- purrr::map_dfr(outcomes_con_flip, function(var_y) {
    purrr::map_dfr(names(ventanas), function(nombre_ventana) {
      anios_v <- ventanas[[nombre_ventana]]
      datos_v <- panel_bite %>%
        dplyr::filter(ANIO %in% anios_v) %>%
        dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(sort(anios_v))))
      anios_no_ref_v <- setdiff(sort(anios_v), 2015)
      patron_f_v <- paste0("ANIO_F::(", paste(anios_no_ref_v, collapse = "|"), ")")

      correr_evento_bite_v <- function(etiqueta, fe_adicionales = NULL) {
        fe <- if (is.null(fe_adicionales)) "NORDEST" else paste("NORDEST", fe_adicionales, sep = " + ")
        formula_modelo <- stats::as.formula(paste0(var_y, " ~ i(ANIO_F, Bite2022_obreros, ref = '2015') | ", fe))
        modelo <- fixest::feols(formula_modelo, data = datos_v, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
        prueba_f <- fixest::wald(modelo, keep = patron_f_v, print = FALSE)
        tibble::tibble(
          variable = var_y,
          ventana = nombre_ventana,
          especificacion = etiqueta,
          n_obs = stats::nobs(modelo),
          n_firmas = dplyr::n_distinct(datos_v$NORDEMP[!is.na(datos_v[[var_y]])]),
          f_stat = round(prueba_f$stat, 3),
          df1 = prueba_f$df1,
          df2 = round(prueba_f$df2, 1),
          p_value = signif(prueba_f$p, 4)
        )
      }

      dplyr::bind_rows(
        correr_evento_bite_v("Sin controles"),
        correr_evento_bite_v("Con sector*anio + tamano*anio + departamento*anio",
                              "CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F")
      )
    })
  })

  readr::write_csv(resultado_bite, file.path(out_dir, "validacion_pretendencias_ventana_completa_bite.csv"))
}

# ------------------------------------------------------------------
# Reporte en consola (numeros crudos, sin interpretar)
# ------------------------------------------------------------------

script_header("validar_pretendencias_panel_formal_ventana_completa.R -- ventana PRE completa (2015-2019+2021-2022) vs. ventana corta")
message("")
message("Panel (ventana PRE completa, muestra filtrada): ", nrow(panel_built), " filas NORDEST-ANIO, ",
        dplyr::n_distinct(panel_built$NORDEST), " establecimientos, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("=== Comparacion ventana corta (2015-2019) vs. ventana PRE completa (2015-2019+2021-2022), Exposure2022_obreros ===")
print(comparacion %>% dplyr::select(variable, especificacion, f_stat_ventana_corta, p_value_ventana_corta, f_stat_ventana_completa, p_value_ventana_completa, cambia_conclusion), n = Inf, width = Inf)
message("")
if (length(outcomes_con_flip) == 0) {
  message(">>> NINGUN outcome cambia de conclusion (rechaza vs. no rechaza al 5%) al ampliar la ventana. <<<")
} else {
  message(">>> ", length(outcomes_con_flip), " outcome(s) CAMBIAN de conclusion al ampliar la ventana: ", paste(outcomes_con_flip, collapse = ", "), " <<<")
  message("")
  message("=== Repeticion con Bite2022_obreros para ", paste(outcomes_con_flip, collapse = ", "), " (ventana corta vs. completa) ===")
  print(resultado_bite, n = Inf, width = Inf)
}
message("")
message("Tablas exportadas en: ", out_dir)
