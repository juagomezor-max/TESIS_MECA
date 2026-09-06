# estimar_did_principal_empleo.R
#
# *** ESPECIFICACION PRINCIPAL DE LA TESIS ***. No es un mecanismo, no
# es una validacion -- es el resultado central del diseño DiD: efecto
# del choque de salario minimo 2023 sobre 4 dimensiones de empleo, en
# funcion de Exposure2022_obreros (composicion ocupacional pre-choque,
# a nivel FIRMA), sobre el panel formal ya validado
# (panel_establecimiento_formal.rds, Paso A).
#
# Bite2022_obreros queda DELIBERADAMENTE fuera de esta ronda -- no
# descartada, solo pospuesta para evaluar en el futuro con otras
# medidas de exposicion alternativas. Ver README.md de
# 4. RESULTADOS/Estimacion_DiD/ para la marca completa.
#
# LIMITACION CONOCIDA A CITAR EN CUALQUIER INTERPRETACION DE UN
# RESULTADO NULO: el "primer eslabon" (validaciones/validar_primer_
# eslabon_costo_laboral.R) encontro que Exposure2022_obreros NO predice
# de forma robusta el choque real de costo laboral en 2023 bajo la
# especificacion CON controles (p=0.771), aunque Bite2022_obreros SI lo
# predice (p=5.8e-9, ver validaciones/validar_primer_eslabon_costo_
# laboral_bite.R). Esto significa que un coeficiente NO significativo
# en este script es AMBIGUO entre "no hay efecto de empleo" y "la
# medida Exposure2022_obreros no tiene la potencia para detectar el
# mecanismo que el diseño asume" -- no se puede distinguir con los
# datos de este script solo. Diagnostico de sobre-control relacionado
# (R2=0.218 de Exposure explicado por sector+tamano+depto): ver
# validaciones/diagnosticar_sobrecontrol_exposure_fwl.R.
#
# DERIVADO de (no reimplementado de memoria):
# - post_2023 = as.integer(ANIO >= 2023): identico a
#   pipeline/03_construir_panel.R.
# - exposicion_10pp = Exposure2022_obreros / 0.1: identico a
#   pipeline/03_construir_panel.R y validaciones/validar_pretendencias_
#   panel_formal.R.
# - Ventana 2015-2019+2021-2022(pre)+2023-2024(post), 2020 excluido:
#   YA es la ventana completa de panel_establecimiento_formal.rds
#   (Paso A) -- no se refiltra.
# - Los 3 controles (sector*anio + tamano*anio + departamento*anio) y
#   cluster=~NORDEMP: identico a validaciones/validar_pretendencias_
#   panel_formal.R y validaciones/validar_primer_eslabon_costo_laboral.R.
# - Forma funcional del event-study (i(ANIO_F, exposicion_10pp, ref)):
#   identica a las validaciones anteriores, unico cambio real es
#   ref='2022' (ultimo anio pre-tratamiento) en vez de '2015' -- aqui
#   el objetivo es la imagen completa pre+post, no solo el chequeo de
#   pre-tendencias (que ya se hizo con ref='2015').
#
# ESPECIFICACION A -- DiD estatico (modelo base del proyecto):
#   Y_ft = a + b*(post_2023 x exposicion_10pp) + FE_NORDEST + FE_ANIO [+ controles]
#   Sin controles:  outcome ~ post_2023:exposicion_10pp | NORDEST + ANIO_F
#   Con controles:  outcome ~ post_2023:exposicion_10pp | NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F
#   cluster = ~NORDEMP siempre. 4 outcomes x 2 especificaciones = 8 modelos.
#
# ESPECIFICACION B -- event study completo (pre+post en una sola imagen):
#   outcome ~ i(ANIO_F, exposicion_10pp, ref='2022') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F
#   cluster = ~NORDEMP, SOLO con controles (la especificacion recomendada).
#   4 outcomes.
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - tabla_did_estatico_empleo.csv / .html (Especificacion A, 8 modelos, formato academico)
# - did_estatico_empleo_coeficientes.csv (Especificacion A, tidy)
# - event_study_completo_coeficientes.csv / _metadatos.csv (Especificacion B, tidy)
# - evento_did_completo_<outcome>.png (4 archivos, Especificacion B)
# - evento_did_completo_panel_2x2.png (las 4 en un solo panel, para referencia rapida)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr", "ggplot2")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
if (!file.exists(panel_formal_path)) {
  stop("Falta panel_establecimiento_formal.rds. Corre construccion/construir_panel_establecimiento_formal.R primero.")
}
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) {
  stop("Falta exposicion_firma_eam.rds. Corre pipeline/02_construir_exposicion.R primero.")
}

# ------------------------------------------------------------------
# 1) Panel formal completo (9 anios, ya es la ventana final) +
#    Exposure2022_obreros (firma) + post_2023 + exposicion_10pp.
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path)

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

panel_built <- panel_formal %>%
  dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    post_2023 = as.integer(ANIO >= 2023),
    exposicion_10pp = Exposure2022_obreros / 0.1
  )

outcomes_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total", "Empleo total",
  "empleo_permanente", "Empleo permanente",
  "empleo_temporal", "Empleo temporal (directo + agencia)",
  "participacion_permanente", "Participacion de permanentes en el empleo total (%)"
)

# ------------------------------------------------------------------
# 2) ESPECIFICACION A: DiD estatico, sin y con controles, 4 outcomes.
# ------------------------------------------------------------------

modelos_did <- list()
coef_did_list <- list()

for (i in seq_len(nrow(outcomes_info))) {
  var_y <- outcomes_info$var[i]

  f_sin <- stats::as.formula(paste0(var_y, " ~ post_2023:exposicion_10pp | NORDEST + ANIO_F"))
  m_sin <- fixest::feols(f_sin, data = panel_built, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  f_con <- stats::as.formula(paste0(
    var_y, " ~ post_2023:exposicion_10pp | NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"
  ))
  m_con <- fixest::feols(f_con, data = panel_built, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  modelos_did[[paste0(var_y, "_sin")]] <- m_sin
  modelos_did[[paste0(var_y, "_con")]] <- m_con

  for (spec in list(list(m = m_sin, etiqueta = "Sin controles", fe_adic = NULL),
                     list(m = m_con, etiqueta = "Con sector*anio + tamano*anio + departamento*anio",
                          fe_adic = c("CIIU4", "tamano_empresa", "DPTO_fijo")))) {
    modelo <- spec$m
    vars_regresion <- c(var_y, "post_2023", "exposicion_10pp", "NORDEST", "ANIO_F")
    if (!is.null(spec$fe_adic)) vars_regresion <- c(vars_regresion, spec$fe_adic)
    clusters_info <- contar_clusters_fixest(modelo, panel_built, "NORDEMP", vars_regresion)
    ct <- summary(modelo)$coeftable
    ci <- stats::confint(modelo, level = 0.95)
    coef_did_list[[paste(var_y, spec$etiqueta)]] <- tibble::tibble(
      outcome = var_y,
      especificacion = spec$etiqueta,
      estimate = round(unname(ct[1, 1]), 6),
      std.error = round(unname(ct[1, 2]), 6),
      statistic = round(unname(ct[1, 3]), 4),
      p.value = signif(unname(ct[1, 4]), 4),
      conf.low = round(unname(ci[1, 1]), 6),
      conf.high = round(unname(ci[1, 2]), 6),
      n_obs = stats::nobs(modelo),
      n_clusters = clusters_info$n_clusters
    )
  }
}

coef_did_tabla <- dplyr::bind_rows(coef_did_list)
readr::write_csv(coef_did_tabla, file.path(out_dir, "did_estatico_empleo_coeficientes.csv"))

# ------------------------------------------------------------------
# 3) Tabla formato academico (fixest::etable): 8 columnas, orden
#    outcome x (sin, con), con fila "Controles" explicita.
# ------------------------------------------------------------------

orden_modelos <- as.vector(rbind(
  paste0(outcomes_info$var, "_sin"),
  paste0(outcomes_info$var, "_con")
))
modelos_ordenados <- modelos_did[orden_modelos]

nombres_dict <- c(
  "post_2023:exposicion_10pp" = "Post2023 x Exposure (10pp)",
  empleo_total = "Empleo total", empleo_permanente = "Empleo permanente",
  empleo_temporal = "Empleo temporal", participacion_permanente = "Participacion permanente (%)"
)

etable_did <- fixest::etable(
  modelos_ordenados,
  tex = FALSE,
  dict = nombres_dict,
  headers = list(
    "Outcome" = as.list(rep(outcomes_info$label, each = 2)),
    "Controles" = as.list(rep(c("No", "Si"), times = nrow(outcomes_info)))
  ),
  fitstat = ~ n + r2
)

readr::write_csv(as.data.frame(etable_did), file.path(out_dir, "tabla_did_estatico_empleo.csv"))

ancho_original <- getOption("width")
options(width = 300)
texto_tabla <- utils::capture.output(print(etable_did))
options(width = ancho_original)
html_tabla <- c(
  "<!doctype html><html><head><meta charset='utf-8'>",
  "<title>DiD estatico -- empleo (Especificacion A)</title>",
  "<style>body{font-family:Consolas,Menlo,monospace;background:#fff;color:#111;padding:24px;}",
  "h1{font-family:sans-serif;font-size:18px;} pre{font-size:13px;line-height:1.4;white-space:pre;overflow-x:auto;}</style>",
  "</head><body>",
  "<h1>Estimacion DiD principal -- empleo (Especificacion A: DiD estatico, 8 modelos)</h1>",
  "<pre>", paste(texto_tabla, collapse = "\n"), "</pre>",
  "</body></html>"
)
writeLines(html_tabla, file.path(out_dir, "tabla_did_estatico_empleo.html"))

# ------------------------------------------------------------------
# 4) ESPECIFICACION B: event study completo, ref='2022', CON controles,
#    4 outcomes.
# ------------------------------------------------------------------

ANIOS_PANEL <- sort(unique(panel_built$ANIO))
panel_built <- panel_built %>% dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(ANIOS_PANEL)))

coeficientes_event_list <- list()
metadatos_event_list <- list()

for (i in seq_len(nrow(outcomes_info))) {
  var_y <- outcomes_info$var[i]
  formula_evento <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, exposicion_10pp, ref = '2022') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"
  ))
  modelo <- fixest::feols(formula_evento, data = panel_built, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  coeficientes_event_list[[var_y]] <- extraer_coeficientes_tidy_fixest(modelo, var_y, "2022 (ref.)") %>%
    dplyr::mutate(anio = as.integer(sub("ANIO_F::(\\d+):.*", "\\1", term)), .after = term)

  vars_regresion <- c(var_y, "ANIO_F", "exposicion_10pp", "NORDEST", "CIIU4", "tamano_empresa", "DPTO_fijo")
  clusters_info <- contar_clusters_fixest(modelo, panel_built, "NORDEMP", vars_regresion)
  metadatos_event_list[[var_y]] <- tibble::tibble(
    outcome = var_y,
    efectos_fijos = "NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F",
    controles = "sector (CIIU4) x anio, tamano_empresa x anio, departamento (DPTO_fijo) x anio",
    variable_cluster = "NORDEMP",
    n_clusters = clusters_info$n_clusters,
    n_obs = stats::nobs(modelo),
    n_obs_reconstruido_coincide = clusters_info$coincide_con_modelo,
    ventana = "2015-2019+2021-2022 (pre) + 2023-2024 (post), 2020 excluido -- panel formal completo",
    variable_exposicion = "Exposure2022_obreros (FIRMA), continua, escalada a 10pp, unida por NORDEMP",
    referencia = "2022 (ultimo anio pre-tratamiento)"
  )
}

coef_event_tabla <- dplyr::bind_rows(coeficientes_event_list)
metadatos_event_tabla <- dplyr::bind_rows(metadatos_event_list)
readr::write_csv(coef_event_tabla, file.path(out_dir, "event_study_completo_coeficientes.csv"))
readr::write_csv(metadatos_event_tabla, file.path(out_dir, "event_study_completo_metadatos.csv"))

# ------------------------------------------------------------------
# 5) Graficos: coeficiente por anio, IC95%, linea vertical en 2023,
#    linea horizontal en 0. Un archivo por outcome + un panel 2x2.
# ------------------------------------------------------------------

graficar_evento <- function(data_outcome, titulo) {
  ggplot2::ggplot(data_outcome, ggplot2::aes(x = anio, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_vline(xintercept = 2022.5, linetype = "dotted", color = "firebrick") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "#1F77B4") +
    ggplot2::geom_line(color = "#1F77B4", linewidth = 1) +
    ggplot2::geom_point(color = "#1F77B4", size = 2) +
    ggplot2::scale_x_continuous(breaks = ANIOS_PANEL) +
    ggplot2::labs(
      title = titulo,
      subtitle = "Efecto de +10pp de Exposure2022_obreros | Ref. 2022 | IC95% | linea roja: inicio tratamiento (2023)",
      x = "Anio", y = "Coeficiente (ref. 2022)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

for (i in seq_len(nrow(outcomes_info))) {
  var_y <- outcomes_info$var[i]
  datos_outcome <- coef_event_tabla %>% dplyr::filter(variable == var_y)
  p <- graficar_evento(datos_outcome, outcomes_info$label[i])
  ggplot2::ggsave(file.path(out_dir, paste0("evento_did_completo_", var_y, ".png")), p, width = 9, height = 6, dpi = 180)
}

p_panel <- ggplot2::ggplot(coef_event_tabla %>% dplyr::left_join(outcomes_info, by = c("variable" = "var")),
                            ggplot2::aes(x = anio, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::geom_vline(xintercept = 2022.5, linetype = "dotted", color = "firebrick") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "#1F77B4") +
  ggplot2::geom_line(color = "#1F77B4", linewidth = 1) +
  ggplot2::geom_point(color = "#1F77B4", size = 1.6) +
  ggplot2::facet_wrap(~label, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = ANIOS_PANEL) +
  ggplot2::labs(
    title = "Estimacion DiD principal -- event study completo, 4 outcomes (Especificacion B)",
    subtitle = "Efecto de +10pp de Exposure2022_obreros | Ref. 2022 | IC95% | linea roja: inicio tratamiento (2023)",
    x = "Anio", y = "Coeficiente (ref. 2022)"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(file.path(out_dir, "evento_did_completo_panel_2x2.png"), p_panel, width = 12, height = 8, dpi = 180)

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("estimar_did_principal_empleo.R -- ESPECIFICACION PRINCIPAL de la tesis (empleo, Exposure2022_obreros)")
message("")
message("Panel: ", nrow(panel_built), " filas NORDEST-ANIO, ", dplyr::n_distinct(panel_built$NORDEST), " establecimientos, ",
        dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("=== Especificacion A: DiD estatico (coeficiente post_2023 x exposicion_10pp) ===")
print(coef_did_tabla %>% dplyr::select(outcome, especificacion, estimate, std.error, p.value, n_obs, n_clusters), n = Inf, width = Inf)
message("")
message("=== Especificacion B: event study completo, ref=2022 (resumen: coeficientes 2023 y 2024) ===")
print(coef_event_tabla %>% dplyr::filter(anio %in% c(2023, 2024)) %>% dplyr::select(variable, anio, estimate, std.error, p.value, conf.low, conf.high), n = Inf, width = Inf)
message("")
message("Tablas y graficos exportados en: ", out_dir)
