# generar_tablas_resultados.R
#
# Paquete de 4 tablas para el capitulo de Resultados, formato listo
# para pegar en Word/LaTeX (fixest::etable para las tablas de modelos,
# tabla HTML simple para las tablas de comparacion ya calculadas). Cada
# tabla cita explicitamente su script y archivo fuente -- ninguna cifra
# se recalcula "a mano", todas vienen de un feols() fresco (Tabla 1) o
# de un CSV ya versionado y verificado en commits anteriores (Tablas
# 2-4).
#
# DERIVADO de (no reimplementado de memoria):
# - Tabla 1: misma especificacion "con controles" de estimacion/
#   estimar_did_principal_empleo.R y _bite.R (post_2023:exposicion |
#   NORDEST+ANIO_F+CIIU4^ANIO_F+tamano_empresa^ANIO_F+DPTO_fijo^ANIO_F,
#   cluster=~NORDEMP) -- refiteada aqui (mismos datos, mismo codigo) para
#   tener los objetos feols que necesita fixest::etable con las 8
#   columnas juntas (el script original las separa por medida).
# - Tabla 2: validaciones/validar_primer_eslabon_costo_laboral.R /
#   _bite.R -- validacion_primer_eslabon_contraste_salto_atipico.csv / _bite.csv.
# - Tabla 3: validaciones/validar_post_controlando_tendencia_lineal.R
#   (auditoria_post_vs_tendencia_lineal_exposure.csv / _bite.csv) +
#   validar_robustez_forma_funcional_tendencia_exposure.R
#   (robustez_tendencia_cuadratica_exposure.csv). Estado "no robusto"
#   tomado literal de BORRADOR_RESULTADOS.md seccion 3.2 (cerrado
#   2026-09-13) -- no se re-decide aqui.
# - Tabla 4: validaciones/validar_placebo_2022_empleo.R --
#   validacion_placebo_2022_contraste_salto_atipico_exposure.csv / _bite.csv.
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - tabla1_resultados_principales.csv / .html
# - tabla2_manipulation_check.csv / .html
# - tabla3_robustez_tendencia.csv / .html
# - tabla4_placebo_2022.csv / .html

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")
val_dir <- paths$resultados_validaciones

outcomes_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total", "Empleo total",
  "empleo_permanente", "Empleo permanente",
  "empleo_temporal", "Empleo temporal",
  "participacion_permanente", "Participacion permanente (%)"
)

escribir_tabla_html <- function(df, path_out, titulo, nota_pie = NULL) {
  filas <- apply(df, 1, function(r) paste0("<tr><td>", paste(r, collapse = "</td><td>"), "</td></tr>"))
  encabezado <- paste0("<tr><th>", paste(names(df), collapse = "</th><th>"), "</th></tr>")
  nota_html <- if (!is.null(nota_pie)) paste0("<p style='font-size:12px;color:#444;max-width:900px;'>", nota_pie, "</p>") else ""
  html <- c(
    "<!doctype html><html><head><meta charset='utf-8'>",
    paste0("<title>", titulo, "</title>"),
    "<style>body{font-family:Arial,sans-serif;padding:24px;} table{border-collapse:collapse;font-size:13px;} th,td{border:1px solid #999;padding:5px 8px;text-align:left;} th{background:#eee;} h1{font-size:18px;}</style>",
    "</head><body>",
    paste0("<h1>", titulo, "</h1>"),
    "<table>", encabezado, paste(filas, collapse = "\n"), "</table>",
    nota_html,
    "</body></html>"
  )
  writeLines(html, path_out)
}

# ====================================================================
# TABLA 1: resultados principales (modelo estatico, con controles),
# Exposure y Bite, 4 outcomes -- 8 columnas en una sola tabla etable.
# ====================================================================

panel_formal <- readr::read_rds(file.path(data_dir, "panel_establecimiento_formal.rds"))
exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds")) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros)

datos_exposure <- panel_formal %>%
  dplyr::inner_join(exposicion_firma %>% dplyr::select(NORDEMP, Exposure2022_obreros) %>% dplyr::filter(!is.na(Exposure2022_obreros)), by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023), exposicion_10pp = Exposure2022_obreros / 0.1)

datos_bite_pre <- panel_formal %>%
  dplyr::inner_join(exposicion_firma %>% dplyr::select(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros)), by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023))
sd_bite <- stats::sd(datos_bite_pre %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::pull(Bite2022_obreros), na.rm = TRUE)
datos_bite <- datos_bite_pre %>% dplyr::mutate(bite_1sd = Bite2022_obreros / sd_bite)

FE_CON <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"

modelos_tabla1 <- list()
for (i in seq_len(nrow(outcomes_info))) {
  var_y <- outcomes_info$var[i]
  f_exp <- stats::as.formula(paste0(var_y, " ~ post_2023:exposicion_10pp | ", FE_CON))
  modelos_tabla1[[paste0(var_y, "_exp")]] <- fixest::feols(f_exp, data = datos_exposure, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  f_bite <- stats::as.formula(paste0(var_y, " ~ post_2023:bite_1sd | ", FE_CON))
  modelos_tabla1[[paste0(var_y, "_bite")]] <- fixest::feols(f_bite, data = datos_bite, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}
orden <- as.vector(rbind(paste0(outcomes_info$var, "_exp"), paste0(outcomes_info$var, "_bite")))
modelos_tabla1 <- modelos_tabla1[orden]

dict_tabla1 <- c(
  "post_2023:exposicion_10pp" = "Post2023 x Exposure (10pp)",
  "post_2023:bite_1sd" = "Post2023 x Bite (1 SD)"
)

etable1 <- fixest::etable(
  modelos_tabla1, tex = FALSE, dict = dict_tabla1,
  headers = list(
    "Outcome" = as.list(rep(outcomes_info$label, each = 2)),
    "Medida" = as.list(rep(c("Exposure2022_obreros", "Bite2022_obreros"), times = nrow(outcomes_info)))
  ),
  fitstat = ~ n + r2
)
readr::write_csv(as.data.frame(etable1), file.path(out_dir, "tabla1_resultados_principales.csv"))

ancho <- getOption("width"); options(width = 300)
texto1 <- utils::capture.output(print(etable1))
options(width = ancho)
writeLines(c(
  "<!doctype html><html><head><meta charset='utf-8'><title>Tabla 1 -- Resultados principales</title>",
  "<style>body{font-family:Consolas,Menlo,monospace;padding:24px;} pre{font-size:12px;overflow-x:auto;}</style></head><body>",
  "<h1 style='font-family:sans-serif;'>Tabla 1 -- Modelo estatico con controles, Exposure2022_obreros y Bite2022_obreros, 4 outcomes</h1>",
  "<p style='font-family:sans-serif;font-size:12px;'>Especificacion: post_2023:exposicion | NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F, cluster=~NORDEMP. Muestra completa de establecimientos (no restringida a multiplanta). Fuente: estimacion/generar_tablas_resultados.R, misma especificacion que estimacion/estimar_did_principal_empleo.R / _bite.R.</p>",
  "<pre>", paste(texto1, collapse = "\n"), "</pre></body></html>"
), file.path(out_dir, "tabla1_resultados_principales.html"))

# ====================================================================
# TABLA 2: manipulation check (primer eslabon), con y sin controles.
# ====================================================================

contraste_exp <- readr::read_csv(file.path(val_dir, "validacion_primer_eslabon_contraste_salto_atipico.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(Medida = "Exposure2022_obreros", Especificacion = especificacion, Contraste = contraste, t = t_stat, p_valor = p_value, Atipico = ifelse(atipico_al_5pct, "SI", "NO"))
contraste_bite <- readr::read_csv(file.path(val_dir, "validacion_primer_eslabon_contraste_salto_atipico_bite.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(Medida = "Bite2022_obreros", Especificacion = especificacion, Contraste = contraste, t = t_stat, p_valor = p_value, Atipico = ifelse(atipico_al_5pct, "SI", "NO"))

tabla2 <- dplyr::bind_rows(contraste_exp, contraste_bite)
readr::write_csv(tabla2, file.path(out_dir, "tabla2_manipulation_check.csv"))
escribir_tabla_html(
  tabla2, file.path(out_dir, "tabla2_manipulation_check.html"),
  "Tabla 2 -- Manipulation check (primer eslabon): contraste del salto 2022-&gt;2023 en costo laboral vs. incremento tipico 2016-2019",
  "Fuente: validaciones/validar_primer_eslabon_costo_laboral.R (Exposure) y _bite.R (Bite). \"Atipico\" = SI si p&lt;0.05."
)

# ====================================================================
# TABLA 3: robustez a tendencia (8 celdas + cuadratica para los 2
# outcomes de Exposure), con columna de estado ("no robusto" segun
# BORRADOR_RESULTADOS.md seccion 3.2, cerrado 2026-09-13).
# ====================================================================

tend_exp <- readr::read_csv(file.path(val_dir, "auditoria_post_vs_tendencia_lineal_exposure.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(Medida = "Exposure2022_obreros")
tend_bite <- readr::read_csv(file.path(val_dir, "auditoria_post_vs_tendencia_lineal_bite.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(Medida = "Bite2022_obreros")
tend <- dplyr::bind_rows(tend_exp, tend_bite)

cuad_exp <- readr::read_csv(file.path(val_dir, "robustez_tendencia_cuadratica_exposure.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(variable, Medida = "Exposure2022_obreros", post_estimate_cuadratica = post_estimate, post_p_cuadratica = post_p)

tabla3 <- tend %>%
  dplyr::left_join(cuad_exp, by = c("variable", "Medida")) %>%
  dplyr::mutate(
    Estado = dplyr::case_when(
      Medida == "Exposure2022_obreros" & variable %in% c("empleo_temporal", "participacion_permanente") ~ "NO ROBUSTO (cerrado 2026-09-13, ver BORRADOR_RESULTADOS.md seccion 3.2)",
      Medida == "Bite2022_obreros" & variable %in% c("empleo_temporal", "participacion_permanente") ~ "Efecto original NO se sostiene -- desaparece con tendencia (resuelto, ver seccion 4.2)",
      TRUE ~ "Nulo estable (no cambio en ninguna prueba)"
    )
  ) %>%
  dplyr::select(
    variable, Medida,
    p_original = post_p_original, p_con_tendencia_lineal = post_p_con_tendencia,
    p_con_tendencia_cuadratica = post_p_cuadratica,
    signo_original = post_estimate_original, signo_con_tendencia = post_estimate_con_tendencia,
    Estado
  )

readr::write_csv(tabla3, file.path(out_dir, "tabla3_robustez_tendencia.csv"))
escribir_tabla_html(
  tabla3, file.path(out_dir, "tabla3_robustez_tendencia.html"),
  "Tabla 3 -- Robustez a tendencia lineal pre-existente (8 celdas) + cuadratica para Exposure",
  "Fuente: validaciones/validar_post_controlando_tendencia_lineal.R (columnas lineal) y validar_robustez_forma_funcional_tendencia_exposure.R (columna cuadratica, solo Exposure/empleo_temporal/participacion_permanente). La columna Estado refleja la conclusion YA CERRADA en BORRADOR_RESULTADOS.md (no se re-decide en esta tabla)."
)

# ====================================================================
# TABLA 4: placebo 2022 (8 celdas).
# ====================================================================

placebo_exp <- readr::read_csv(file.path(val_dir, "validacion_placebo_2022_contraste_salto_atipico_exposure.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(Medida = "Exposure2022_obreros", variable, especificacion, contraste, t_stat, p_value, Atipico = ifelse(atipico_al_5pct, "SI", "NO"))
placebo_bite <- readr::read_csv(file.path(val_dir, "validacion_placebo_2022_contraste_salto_atipico_bite.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(Medida = "Bite2022_obreros", variable, especificacion, contraste, t_stat, p_value, Atipico = ifelse(atipico_al_5pct, "SI", "NO"))

tabla4 <- dplyr::bind_rows(placebo_exp, placebo_bite)
readr::write_csv(tabla4, file.path(out_dir, "tabla4_placebo_2022.csv"))
escribir_tabla_html(
  tabla4, file.path(out_dir, "tabla4_placebo_2022.html"),
  "Tabla 4 -- Placebo 2022 (salario minimo real cayo -3,05pp): contraste 2021-&gt;2022 vs. incremento tipico 2016-2019",
  "Fuente: validaciones/validar_placebo_2022_empleo.R. \"Atipico\" = SI si p&lt;0.05."
)

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("generar_tablas_resultados.R -- Paquete de 4 tablas para el capitulo de Resultados")
message("")
message("Tabla 1 (resultados principales): tabla1_resultados_principales.csv/.html")
message("Tabla 2 (manipulation check): tabla2_manipulation_check.csv/.html")
message("Tabla 3 (robustez a tendencia): tabla3_robustez_tendencia.csv/.html")
message("Tabla 4 (placebo 2022): tabla4_placebo_2022.csv/.html")
message("")
message("Exportadas en: ", out_dir)
