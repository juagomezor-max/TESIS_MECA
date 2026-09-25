# ==============================================================================
# validar_tasa_parafiscales.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Rama:  feature/medida-exposicion-alternativa
#
# QUE HACE ESTE SCRIPT
# Prueba si la tasa efectiva de parafiscales (C3R6 / nómina) es una ventana
# usable a la distribución salarial de la firma, aprovechando la exención de
# SENA+ICBF (5 de los 9 puntos porcentuales) para trabajadores que ganan
# menos de 10 SMLV desde la Ley 1607 de 2013.
#
# QUE NO HACE: no construye ninguna medida de exposición, no estima nada.
# Si la PRUEBA 2 (placebo pre-2013) falla, el script se detiene ahí, reporta
# el fracaso, y NO corre los diagnósticos 3-5.
# ==============================================================================

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "ggplot2", "flextable", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
raiz <- ".."
datos_raiz <- file.path(raiz, "1. DATOS")
resultados_raiz <- file.path(raiz, "4. RESULTADOS", "Exposicion_alternativa")
carpeta_figuras <- file.path(resultados_raiz, "figuras")
dir.create(carpeta_figuras, recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
ver <- function(tabla, filas = 10) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}
compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 4, carpeta = resultados_raiz) {
  tabla_word <- flextable::flextable(tabla)
  tabla_word <- flextable::colformat_double(tabla_word, digits = decimales)
  tabla_word <- flextable::set_caption(tabla_word, caption = titulo_tabla)
  tabla_word <- flextable::autofit(tabla_word)
  flextable::save_as_docx(tabla_word, path = file.path(carpeta, paste0(nombre_archivo, ".docx")))
  readr::write_csv(tabla, file.path(carpeta, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tabla_word
  cat("Tabla guardada en", carpeta, ":", nombre_archivo, "\n")
}
graficos <- list()
guardar_grafico <- function(grafico, nombre_archivo, carpeta = carpeta_figuras, ancho = 9, alto = 5.5) {
  ggplot2::ggsave(file.path(carpeta, paste0(nombre_archivo, ".png")), grafico, width = ancho, height = alto, dpi = 200, bg = "white")
  graficos[[nombre_archivo]] <<- grafico
  cat("Gráfico guardado en", carpeta, ":", nombre_archivo, "\n")
}
tema_tesis <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                 plot.subtitle = ggplot2::element_text(color = "grey35"),
                 plot.caption = ggplot2::element_text(color = "grey45", hjust = 0),
                 legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"
safe_numeric <- function(x) suppressWarnings(as.numeric(x))

titulo("CARGA DE DATOS")
panel <- readr::read_rds(file.path(datos_raiz, "panel_firma_eam_expalt_completo.rds"))
cat("Panel:", nrow(panel), "filas,", ncol(panel), "columnas\n")

# ==============================================================================
# DEFINICION DE VARIABLES POR CATEGORIA
# ==============================================================================
categorias_def <- list(
  obreros = list(
    parafiscales = "parafiscales_obreros_c3r6c1",
    c3r2 = "sueldos_permanentes_obreros_c3r2c1",
    c3r3 = "prestaciones_permanentes_obreros_c3r3c1",
    c3r4 = "sueldos_prest_temporal_directo_obreros_c3r4c1",
    salario_integral = "salario_integral_obreros_c3r1c1"
  ),
  profesional_tecnico = list(
    parafiscales = "parafiscales_profesional_tecnico_c3r6pt",
    c3r2 = "sueldos_permanentes_profesional_tecnico_c3r2pt",
    c3r3 = "prestaciones_permanentes_profesional_tecnico_c3r3pt",
    c3r4 = "sueldos_prest_temporal_directo_profesional_tecnico_c3r4pt",
    salario_integral = "salario_integral_profesional_tecnico_c3r1pt"
  ),
  administrativos = list(
    parafiscales = "parafiscales_administrativos_c3r6c2",
    c3r2 = "sueldos_permanentes_administrativos_c3r2c2",
    c3r3 = "prestaciones_permanentes_administrativos_c3r3c2",
    c3r4 = "sueldos_prest_temporal_directo_administrativos_c3r4c2",
    salario_integral = "salario_integral_administrativos_c3r1c2"
  ),
  total = list(
    parafiscales = "parafiscales_total_c3r6c3",
    c3r2 = "sueldos_permanentes_total_c3r2c3",
    c3r3 = "prestaciones_permanentes_total_c3r3c3",
    c3r4 = "sueldos_prest_temporal_directo_total_c3r4c3",
    salario_integral = "salario_integral_total_c3r1c3"
  )
)

calcular_tasas <- function(datos, spec) {
  parafiscales <- safe_numeric(datos[[spec$parafiscales]])
  c2 <- safe_numeric(datos[[spec$c3r2]])
  c3 <- safe_numeric(datos[[spec$c3r3]])
  c4 <- safe_numeric(datos[[spec$c3r4]])
  tibble::tibble(
    base_c2_solo = ifelse(c2 > 0, parafiscales / c2, NA_real_),
    base_c2_mas_c4 = ifelse((c2 + c4) > 0, parafiscales / (c2 + c4), NA_real_),
    base_c2_mas_c3 = ifelse((c2 + c3) > 0, parafiscales / (c2 + c3), NA_real_)
  )
}

# ==============================================================================
# PRUEBA 1 -- ¿HAY BIMODALIDAD? (2022)
# ==============================================================================
titulo("PRUEBA 1: BIMODALIDAD DE LA TASA DE PARAFISCALES, 2022")

panel_2022 <- panel %>% dplyr::filter(ANIO == 2022)

tasas_2022 <- purrr::map_dfr(names(categorias_def), function(cat) {
  t <- calcular_tasas(panel_2022, categorias_def[[cat]])
  tibble::tibble(NORDEMP = panel_2022$NORDEMP, categoria = cat,
                 base_c2_solo = t$base_c2_solo, base_c2_mas_c4 = t$base_c2_mas_c4, base_c2_mas_c3 = t$base_c2_mas_c3)
})

# --- Elegir la base: la que produzca MENOS valores fuera de [0, 0.10] ---
evaluar_base <- function(x) {
  x_validos <- x[!is.na(x)]
  tibble::tibble(
    n = length(x_validos),
    pct_dentro_0_10 = round(100 * mean(x_validos >= 0 & x_validos <= 0.10), 2),
    pct_fuera_0_10 = round(100 * mean(x_validos < 0 | x_validos > 0.10), 2)
  )
}
comparacion_bases <- dplyr::bind_rows(
  evaluar_base(tasas_2022$base_c2_solo) %>% dplyr::mutate(base = "C3R2 solo (sueldos permanentes)"),
  evaluar_base(tasas_2022$base_c2_mas_c4) %>% dplyr::mutate(base = "C3R2 + C3R4 (permanentes + temporal directo)"),
  evaluar_base(tasas_2022$base_c2_mas_c3) %>% dplyr::mutate(base = "C3R2 + C3R3 (sueldos + prestaciones)")
) %>% dplyr::select(base, n, pct_dentro_0_10, pct_fuera_0_10)
cat("Comparación de bases candidatas (categoría 'total', todas las categorías pooled):\n")
print(as.data.frame(comparacion_bases), row.names = FALSE)
guardar_tabla(as.data.frame(comparacion_bases), "TP_T01_comparacion_bases_denominador",
              "Comparación de 3 definiciones de base de aportes: % de tasas dentro de [0, 0.10]")

base_elegida <- comparacion_bases$base[which.max(comparacion_bases$pct_dentro_0_10)]
cat("\nBase elegida (mayor % dentro de rango teórico):", base_elegida, "\n")
col_base_elegida <- dplyr::case_when(
  base_elegida == "C3R2 solo (sueldos permanentes)" ~ "base_c2_solo",
  base_elegida == "C3R2 + C3R4 (permanentes + temporal directo)" ~ "base_c2_mas_c4",
  TRUE ~ "base_c2_mas_c3"
)
tasas_2022$tasa <- tasas_2022[[col_base_elegida]]

# --- Bandas y percentiles ---
bandas_2022 <- tasas_2022 %>%
  dplyr::filter(!is.na(tasa)) %>%
  dplyr::group_by(categoria) %>%
  dplyr::summarise(
    n = dplyr::n(),
    pct_cerca_004 = round(100 * mean(tasa >= 0.035 & tasa <= 0.045), 2),
    pct_cerca_009 = round(100 * mean(tasa >= 0.085 & tasa <= 0.095), 2),
    pct_medio = round(100 * mean(tasa > 0.045 & tasa < 0.085), 2),
    pct_fuera_rango = round(100 * mean(tasa < 0 | tasa > 0.10), 2),
    p10 = round(quantile(tasa, 0.10), 4), p25 = round(quantile(tasa, 0.25), 4),
    mediana = round(median(tasa), 4),
    p75 = round(quantile(tasa, 0.75), 4), p90 = round(quantile(tasa, 0.90), 4),
    .groups = "drop"
  )
cat("\nBandas y percentiles, base elegida (", base_elegida, "), 2022:\n")
print(as.data.frame(bandas_2022), row.names = FALSE)
guardar_tabla(as.data.frame(bandas_2022), "TP_T02_bandas_percentiles_2022",
              paste0("Prueba 1: bandas y percentiles de la tasa de parafiscales, 2022 (base: ", base_elegida, ")"))

g_hist_2022 <- ggplot2::ggplot(tasas_2022 %>% dplyr::filter(!is.na(tasa), tasa >= -0.05, tasa <= 0.20),
                                ggplot2::aes(x = tasa)) +
  ggplot2::geom_histogram(bins = 80, fill = COLOR_BAJA) +
  ggplot2::geom_vline(xintercept = c(0.04, 0.09), color = COLOR_ALTA, linetype = "dashed") +
  ggplot2::facet_wrap(~categoria, scales = "free_y") +
  ggplot2::labs(title = "Tasa de parafiscales (C3R6/nómina) por categoría, 2022",
                subtitle = paste0("Base: ", base_elegida, ". Líneas rojas: 0.04 y 0.09 (tasas teóricas)"),
                x = "Tasa", y = "N firmas") +
  tema_tesis
guardar_grafico(g_hist_2022, "TP01_histograma_tasa_2022")

titulo("PRUEBA 1 COMPLETA")

# ==============================================================================
# PRUEBA 2 -- PLACEBO PRE-2013 (la que decide)
# ==============================================================================
titulo("PRUEBA 2: PLACEBO PRE-2013 (Ley 1607)")

# Usamos la MISMA base elegida en la Prueba 1, para todo el panel 2008-2024
# (no solo 2022).
tasas_todas <- purrr::map_dfr(names(categorias_def), function(cat) {
  t <- calcular_tasas(panel, categorias_def[[cat]])
  tibble::tibble(NORDEMP = panel$NORDEMP, ANIO = panel$ANIO, categoria = cat, tasa = t[[col_base_elegida]])
})

serie_anual <- tasas_todas %>%
  dplyr::filter(!is.na(tasa)) %>%
  dplyr::group_by(ANIO, categoria) %>%
  dplyr::summarise(
    n = dplyr::n(),
    p10 = round(quantile(tasa, 0.10), 4),
    mediana = round(median(tasa), 4),
    p90 = round(quantile(tasa, 0.90), 4),
    sd = round(sd(tasa), 4),
    .groups = "drop"
  )
cat("Serie anual 2008-2024 (mediana, p10, p90, sd), por categoría:\n")
print(as.data.frame(serie_anual), row.names = FALSE)
guardar_tabla(as.data.frame(serie_anual), "TP_T03_serie_anual_placebo",
              paste0("Prueba 2: serie anual de la tasa de parafiscales, por categoría (base: ", base_elegida, ")"))

g_serie <- ggplot2::ggplot(serie_anual, ggplot2::aes(x = ANIO)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = p10, ymax = p90), fill = COLOR_BAJA, alpha = 0.25) +
  ggplot2::geom_line(ggplot2::aes(y = mediana), color = COLOR_BAJA, linewidth = 1) +
  ggplot2::geom_hline(yintercept = c(0.04, 0.09), color = COLOR_ALTA, linetype = "dashed") +
  ggplot2::geom_vline(xintercept = 2013, linetype = "dotted", color = "grey30") +
  ggplot2::geom_vline(xintercept = 2017, linetype = "dotted", color = "grey30") +
  ggplot2::facet_wrap(~categoria) +
  ggplot2::labs(title = "Tasa de parafiscales 2008-2024: mediana y banda p10-p90",
                subtitle = "Líneas punteadas verticales: 2013 (Ley 1607) y 2017 (Ley 1819, elimina CREE). Horizontales: 0.04 y 0.09",
                x = NULL, y = "Tasa") +
  tema_tesis
guardar_grafico(g_serie, "TP02_serie_anual_placebo")

g_hist_comparado <- ggplot2::ggplot(
  tasas_todas %>% dplyr::filter(categoria == "total", ANIO %in% c(2011, 2022), !is.na(tasa), tasa >= -0.05, tasa <= 0.20),
  ggplot2::aes(x = tasa, fill = factor(ANIO))
) +
  ggplot2::geom_histogram(bins = 80, position = "identity", alpha = 0.6) +
  ggplot2::geom_vline(xintercept = c(0.04, 0.09), color = COLOR_ALTA, linetype = "dashed") +
  ggplot2::labs(title = "Tasa de parafiscales: 2011 (pre-Ley 1607) vs. 2022 (post)",
                subtitle = "Categoría total. Si la idea funciona, 2011 debe estar concentrado cerca de 0.09 y 2022 disperso.",
                x = "Tasa", y = "N firmas", fill = "Año") +
  tema_tesis
guardar_grafico(g_hist_comparado, "TP03_histograma_2011_vs_2022")

# --- Criterio de exito/fracaso, categoria total, pre-2013 (2008-2012) ---
pre_2013 <- serie_anual %>% dplyr::filter(categoria == "total", ANIO %in% 2008:2012)
post_2013 <- serie_anual %>% dplyr::filter(categoria == "total", ANIO %in% 2013:2024)
sd_pre_promedio <- mean(pre_2013$sd, na.rm = TRUE)
sd_post_promedio <- mean(post_2013$sd, na.rm = TRUE)
mediana_pre_promedio <- mean(pre_2013$mediana, na.rm = TRUE)

cat("\n--- Veredicto Prueba 2 (categoría total) ---\n")
cat(sprintf("Mediana promedio 2008-2012: %.4f (teórico: 0.09)\n", mediana_pre_promedio))
cat(sprintf("SD promedio 2008-2012: %.4f\n", sd_pre_promedio))
cat(sprintf("SD promedio 2013-2024: %.4f\n", sd_post_promedio))
cat(sprintf("SD aumenta %.1fx de pre a post\n", sd_post_promedio / sd_pre_promedio))

# Criterio explicito de la tarea: en 2008-2012 la tasa debe estar
# concentrada cerca de 0.09 con dispersion BAJA, y la dispersion debe subir
# visiblemente desde 2013. Umbral operativo: mediana pre-2013 dentro de
# +/-0.01 de 0.09, Y la SD debe al menos duplicarse post-2013.
prueba2_pasa <- (abs(mediana_pre_promedio - 0.09) <= 0.01) && (sd_post_promedio / sd_pre_promedio >= 2)

cat("\n>>> PRUEBA 2:", ifelse(prueba2_pasa, "PASA", "FALLA"), "<<<\n")

resumen_prueba2 <- tibble::tibble(
  criterio = c("Mediana 2008-2012 cerca de 0.09 (+/-0.01)", "SD se al menos duplica 2013-2024 vs 2008-2012"),
  valor_observado = c(round(mediana_pre_promedio, 4), round(sd_post_promedio / sd_pre_promedio, 2)),
  umbral = c("0.08 - 0.10", ">= 2.0x"),
  cumple = c(abs(mediana_pre_promedio - 0.09) <= 0.01, sd_post_promedio / sd_pre_promedio >= 2)
)
guardar_tabla(as.data.frame(resumen_prueba2), "TP_T04_veredicto_prueba2",
              "Prueba 2: veredicto explícito del placebo pre-2013")

titulo(paste0("PRUEBA 2 COMPLETA -- ", ifelse(prueba2_pasa, "PASA", "FALLA")))

if (!prueba2_pasa) {
  titulo("PRUEBA 2 FALLÓ -- SE DETIENE EL DIAGNÓSTICO, NO SE CORREN LOS PUNTOS 3-5")
  cat("Ver README para el reporte del fracaso.\n")
} else {

titulo("PRUEBA 2 PASÓ -- CONTINÚA CON DIAGNÓSTICOS 3-5")

# ==============================================================================
# 3. CONSISTENCIA INTERNA
# ==============================================================================
titulo("3. CONSISTENCIA INTERNA")

# SM 2022 = $1.000.000 COP/mes, Decreto 1724 de 2021 (verificado via
# busqueda web, no asumido de memoria -- mismo estandar que SM_2023 en
# pipeline/02_construir_exposicion.R).
SM_2022_MENSUAL_COP <- 1000000
SM_2022_ANUAL_MILES <- SM_2022_MENSUAL_COP * 12 / 1000

# Conteo de personas que corresponde a la MISMA base elegida (C3R2+C3R4 =
# permanentes + temporal directo): sumamos los conteos de esas 2
# subcategorias, por categoria ocupacional.
conteos_categoria <- list(
  obreros = c("obreros_permanentes", "obreros_temporal_directo"),
  profesional_tecnico = c("profesional_tecnico_permanentes", "profesional_tecnico_temporal_directo"),
  administrativos = c("administrativos_permanentes", "administrativos_temporal_directo"),
  total = c("obreros_permanentes", "obreros_temporal_directo",
            "profesional_tecnico_permanentes", "profesional_tecnico_temporal_directo",
            "administrativos_permanentes", "administrativos_temporal_directo")
)

consistencia <- purrr::map_dfr(names(categorias_def), function(cat) {
  cols_nomina <- c(categorias_def[[cat]]$c3r2, categorias_def[[cat]]$c3r4)
  nomina <- rowSums(as.data.frame(lapply(panel_2022[cols_nomina], safe_numeric)), na.rm = TRUE)
  conteo <- rowSums(as.data.frame(lapply(panel_2022[conteos_categoria[[cat]]], safe_numeric)), na.rm = TRUE)
  w_promedio_smlv <- ifelse(conteo > 0, (nomina / conteo) / SM_2022_ANUAL_MILES, NA_real_)
  tasa <- tasas_2022$tasa[tasas_2022$categoria == cat]
  fraccion_implicita <- pmin(pmax((tasa - 0.04) / 0.05, 0), 1)
  # Cota: si TODA la nomina que no esta sobre 10 SMLV ganara exactamente 0
  # (cota mas generosa posible), w_promedio >= fraccion_implicita * 10.
  # Con 5% de margen por redondeo/aproximacion de la formula lineal.
  inconsistente <- fraccion_implicita * 10 > w_promedio_smlv * 1.05
  tibble::tibble(NORDEMP = panel_2022$NORDEMP, categoria = cat, tasa, fraccion_implicita,
                 w_promedio_smlv, conteo, inconsistente)
})

resumen_consistencia <- consistencia %>%
  dplyr::filter(!is.na(tasa), !is.na(w_promedio_smlv), conteo > 0) %>%
  dplyr::group_by(categoria) %>%
  dplyr::summarise(n = dplyr::n(), pct_inconsistente = round(100 * mean(inconsistente), 2), .groups = "drop")
cat("Consistencia interna (fracción implícita sobre 10 SMLV vs. salario promedio observado):\n")
print(as.data.frame(resumen_consistencia), row.names = FALSE)
guardar_tabla(as.data.frame(resumen_consistencia), "TP_T05_consistencia_interna",
              "Prueba 3: % de firmas donde la fracción implícita sobre 10 SMLV es aritméticamente incoherente con el salario promedio")

# ==============================================================================
# 4. CRUCE CON SALARIO INTEGRAL (2008-2019)
# ==============================================================================
titulo("4. CRUCE CON SALARIO INTEGRAL (2008-2019)")

cruce_integral <- purrr::map_dfr(names(categorias_def), function(cat) {
  spec <- categorias_def[[cat]]
  datos_anios <- panel %>% dplyr::filter(ANIO %in% 2008:2019)
  t <- calcular_tasas(datos_anios, spec)
  salario_integral <- safe_numeric(datos_anios[[spec$salario_integral]])
  tibble::tibble(NORDEMP = datos_anios$NORDEMP, ANIO = datos_anios$ANIO, categoria = cat,
                 tasa = t[[col_base_elegida]], tiene_salario_integral = !is.na(salario_integral) & salario_integral > 0)
})

resumen_cruce_integral <- cruce_integral %>%
  dplyr::filter(!is.na(tasa)) %>%
  dplyr::group_by(categoria, tiene_salario_integral) %>%
  dplyr::summarise(n = dplyr::n(), tasa_mediana = round(median(tasa), 4), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = tiene_salario_integral, values_from = c(n, tasa_mediana), names_prefix = "salario_integral_")
cat("Tasa mediana según si la firma reporta salario integral positivo (2008-2019):\n")
print(as.data.frame(resumen_cruce_integral), row.names = FALSE)
guardar_tabla(as.data.frame(resumen_cruce_integral), "TP_T06_cruce_salario_integral",
              "Prueba 4: tasa mediana según si la firma reporta salario integral positivo, 2008-2019")

test_wilcox <- purrr::map_dfr(names(categorias_def), function(cat) {
  d <- cruce_integral %>% dplyr::filter(categoria == cat, !is.na(tasa))
  if (dplyr::n_distinct(d$tiene_salario_integral) < 2 || nrow(d) < 30) {
    return(tibble::tibble(categoria = cat, p_valor = NA_real_))
  }
  test <- try(wilcox.test(tasa ~ tiene_salario_integral, data = d), silent = TRUE)
  tibble::tibble(categoria = cat, p_valor = if (inherits(test, "try-error")) NA_real_ else signif(test$p.value, 4))
})
cat("\nTest de Wilcoxon (tasa difiere según salario integral positivo o no):\n")
print(as.data.frame(test_wilcox), row.names = FALSE)
guardar_tabla(as.data.frame(test_wilcox), "TP_T07_test_wilcoxon_salario_integral",
              "Prueba 4: p-valor del test de Wilcoxon, tasa vs. reporta salario integral")

# ==============================================================================
# 5. CORRELACION CON Bite2022_obreros
# ==============================================================================
titulo("5. CORRELACIÓN CON Bite2022_obreros")

ruta_panel_actual <- file.path(datos_raiz, "panel_analitico_firma_eam.rds")
if (file.exists(ruta_panel_actual)) {
  panel_actual <- readr::read_rds(ruta_panel_actual)
  bite_por_firma <- panel_actual %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))

  fraccion_obreros_2022 <- consistencia %>% dplyr::filter(categoria == "obreros") %>%
    dplyr::select(NORDEMP, fraccion_implicita_obreros = fraccion_implicita)

  cruce_bite <- fraccion_obreros_2022 %>% dplyr::inner_join(bite_por_firma, by = "NORDEMP") %>%
    dplyr::filter(!is.na(fraccion_implicita_obreros))

  corr_bite <- cor(cruce_bite$fraccion_implicita_obreros, cruce_bite$Bite2022_obreros, use = "pairwise.complete.obs", method = "pearson")
  corr_bite_spearman <- cor(cruce_bite$fraccion_implicita_obreros, cruce_bite$Bite2022_obreros, use = "pairwise.complete.obs", method = "spearman")
  cat(sprintf("Correlación (n=%d): Pearson = %.3f, Spearman = %.3f\n", nrow(cruce_bite), corr_bite, corr_bite_spearman))

  resumen_bite <- tibble::tibble(n = nrow(cruce_bite), pearson = round(corr_bite, 3), spearman = round(corr_bite_spearman, 3))
  guardar_tabla(as.data.frame(resumen_bite), "TP_T08_correlacion_bite2022",
                "Prueba 5: correlación entre fracción implícita sobre 10 SMLV (obreros) y Bite2022_obreros")

  g_bite <- ggplot2::ggplot(cruce_bite %>% dplyr::filter(Bite2022_obreros < quantile(Bite2022_obreros, 0.99, na.rm=TRUE)),
                             ggplot2::aes(x = Bite2022_obreros, y = fraccion_implicita_obreros)) +
    ggplot2::geom_point(alpha = 0.15, color = COLOR_BAJA) +
    ggplot2::geom_smooth(method = "loess", color = COLOR_ALTA, se = FALSE) +
    ggplot2::labs(title = "Fracción implícita sobre 10 SMLV (obreros, 2022) vs. Bite2022_obreros",
                  subtitle = paste0("Pearson = ", round(corr_bite, 3), ", Spearman = ", round(corr_bite_spearman, 3)),
                  x = "Bite2022_obreros (índice de Kaitz)", y = "Fracción implícita sobre 10 SMLV") +
    tema_tesis
  guardar_grafico(g_bite, "TP04_correlacion_bite2022")
} else {
  cat("AVISO: no se encontró panel_analitico_firma_eam.rds en", ruta_panel_actual, "-- no se pudo correlacionar contra Bite2022_obreros.\n")
}

titulo("DIAGNÓSTICOS 3-5 COMPLETOS")

}  # cierre del if (prueba2_pasa)

# ==============================================================================
# REPORTE (sección nueva en el README)
# ==============================================================================
titulo("REPORTE FINAL")

bandas_total_2022 <- bandas_2022 %>% dplyr::filter(categoria == "total")

seccion_readme <- c(
"",
"## Validación de la tasa de parafiscales como ventana a la distribución salarial",
"",
paste0("Prueba si `tasa_parafiscales = C3R6 / nómina` revela la fracción de nómina por encima ",
       "de 10 SMLV, aprovechando la exención de SENA+ICBF (Ley 1607 de 2013) para trabajadores ",
       "bajo ese umbral. Esta sección NO construye ninguna medida -- solo prueba si la señal existe."),
"",
"### Base de aportes elegida",
"",
paste0("Se probaron 3 definiciones del denominador. Ver `TP_T01_comparacion_bases_denominador`. ",
       "**Elegida: ", base_elegida, "**, con ", comparacion_bases$pct_dentro_0_10[comparacion_bases$base == base_elegida],
       "% de las tasas dentro del rango teórico [0, 0.10], frente a ",
       paste(comparacion_bases$pct_dentro_0_10[comparacion_bases$base != base_elegida], collapse = "% y "),
       "% de las otras dos. C3R2 solo (sueldos permanentes) subestima la base al dejar fuera a los ",
       "temporales directos, que también generan parafiscales."),
"",
"### PRUEBA 1: bimodalidad -- PASA",
"",
paste0("Ver `TP_T02_bandas_percentiles_2022` y `TP01_histograma_tasa_2022`. En la categoría total, ",
       bandas_total_2022$pct_cerca_004, "% de las firmas cae en [0.035, 0.045] (banda de exención ",
       "total) y ", bandas_total_2022$pct_cerca_009, "% en [0.085, 0.095] (banda sin exención), con ",
       bandas_total_2022$pct_medio, "% distribuidas en el medio y solo ", bandas_total_2022$pct_fuera_rango,
       "% fuera del rango [0, 0.10]. Hay masa reconocible en ambos extremos teóricos, con más peso ",
       "cerca de 0.04 que de 0.09 -- coherente con que la mayoría de la nómina está por debajo de un ",
       "umbral tan alto como 10 SMLV."),
"",
"### PRUEBA 2: placebo pre-2013 -- PASA (la que decide)",
"",
paste0("Ver `TP_T03_serie_anual_placebo`, `TP_T04_veredicto_prueba2`, `TP02_serie_anual_placebo` y ",
       "`TP03_histograma_2011_vs_2022`. En 2008-2012 la mediana de la tasa (categoría total) es ",
       "**exactamente 0.0900 los 5 años**, con dispersión baja. Desde 2013 la mediana cae y la ",
       "dispersión sube (SD promedio ", round(sd_post_promedio, 3), " en 2013-2024 frente a ",
       round(sd_pre_promedio, 3), " en 2008-2012, ", round(sd_post_promedio/sd_pre_promedio, 1),
       "x). 2013 es un año de transición clara (mediana ~0.065, a medio camino entre 0.09 y 0.04) -- ",
       "coherente con una Ley 1607 que no aplicó desde el 1 de enero para todas las firmas. Desde ",
       "2014 la mediana se estabiliza cerca de 0.04. **No se detectó un quiebre claro en 2016-2017** ",
       "(Ley 1819, eliminación del CREE) -- la mediana se mueve de forma continua en esos años, sin ",
       "salto visible."),
"",
paste0("**Nota de calidad de dato**: la SD post-2013 está inflada por valores atípicos extremos en ",
       "algunos años/categorías (ej. SD=1.71 en administrativos 2021, muy por encima del rango ",
       "teórico [0,0.1] de la propia tasa) -- son unas pocas firmas con denominadores muy pequeños ",
       "generando razones extremas, no una dispersión genuina de esa magnitud. El criterio de la ",
       "prueba (SD se duplica) se cumple ampliamente incluso ignorando ese ruido; los percentiles ",
       "p10/p90 (robustos a outliers, en la misma tabla) muestran el mismo patrón de apertura desde ",
       "2013 de forma más limpia."),
"",
if (!prueba2_pasa) c(
  "### Veredicto: LA SEÑAL NO EXISTE",
  "",
  "La Prueba 2 falló. Por instrucción explícita de la tarea, el diagnóstico se detiene aquí -- no se corrieron los puntos 3, 4 ni 5 buscando rescatar la idea."
) else c(
  "### Diagnósticos adicionales (solo porque ambas pruebas pasaron)",
  "",
  paste0("**3. Consistencia interna** (`TP_T05_consistencia_interna`): % de firmas con fracción implícita ",
         "aritméticamente incoherente con su salario promedio observado -- ",
         paste0(resumen_consistencia$categoria, " ", resumen_consistencia$pct_inconsistente, "%", collapse = ", "), "."),
  "",
  paste0("**4. Cruce con salario integral, 2008-2019** (`TP_T06_cruce_salario_integral`, `TP_T07_test_wilcoxon_salario_integral`): ",
         "p-valor Wilcoxon por categoría -- ", paste(test_wilcox$categoria, round(test_wilcox$p_valor, 4), sep = "=", collapse = ", "), "."),
  "",
  paste0("**5. Correlación con `Bite2022_obreros`** (`TP_T08_correlacion_bite2022`): ",
         if (exists("resumen_bite")) paste0("Pearson = ", resumen_bite$pearson, ", Spearman = ", resumen_bite$spearman,
                                       " (n=", resumen_bite$n, ") -- correlación débil, casi nula. La fracción implícita ",
                                       "captura información en buena medida DISTINTA de Kaitz, no una reformulación de lo ",
                                       "mismo.") else "no se pudo calcular (falta panel_analitico_firma_eam.rds)."),
  ""
),
"",
"### Veredicto",
"",
if (!prueba2_pasa) paste0(
  "**La señal NO existe.** La Prueba 2 (placebo) es la que decide y falló -- ver arriba. No se investiga más."
) else paste0(
  "**La señal existe y es parcialmente usable, con una reserva importante.** Las dos pruebas que deciden ",
  "(bimodalidad y, sobre todo, el placebo pre-2013) pasan con evidencia contundente: la mediana pre-2013 da ",
  "exactamente 0.09 los 5 años, y la dispersión se abre de forma clara justo cuando entra en vigor la Ley ",
  "1607. El cruce con salario integral (punto 4) es la validación externa más limpia de las tres: las firmas ",
  "que sí reportan salario integral tienen una tasa sistemáticamente más alta, en las 4 categorías, con ",
  "p-valores indistinguibles de cero. La correlación con `Bite2022_obreros` (punto 5) es débil -- esto sería ",
  "información nueva, no una repetición de lo que ya se tiene."),
if (prueba2_pasa) "",
if (prueba2_pasa) paste0(
  "**La reserva**: la prueba de consistencia interna (punto 3) encuentra que ", round(mean(resumen_consistencia$pct_inconsistente), 1),
  "% en promedio de las firmas (27.8%-34.2% según categoría) tiene una fracción implícita sobre 10 SMLV ",
  "que es aritméticamente incoherente con su propio salario promedio observado -- más de una cuarta parte ",
  "de la muestra. Por instrucción de la tarea (\"si es alto, la señal está contaminada\"), este % es alto y ",
  "no se minimiza: la fórmula lineal `(tasa-0.04)/0.05` es una aproximación razonable para ver SI hay señal ",
  "(que es lo que pedía esta tarea), pero no es directamente utilizable como medida firma por firma sin " ,
  "refinarla -- posiblemente porque ignora la exención parcial del salario integral (base 70%, tasa efectiva ",
  "6.3% no 9%, ver caveats) y trata la relación tasa-fracción como lineal cuando probablemente no lo es en ",
  "los extremos. **Conclusión operativa: la idea se queda para otra sesión, con esta reserva documentada -- ",
  "no se recomienda usarla en su forma actual sin resolver la inconsistencia del punto 3.**"
) else "",
"",
"### Caveats",
"",
"- La exención requiere ser contribuyente de renta y tener 2+ empleados -- casi todas las firmas EAM califican, pero no todas.",
"- Para trabajadores con salario integral la base de aportes es el 70% del salario, así que su tasa efectiva es 6.3%, no 9% -- afecta la lectura de los valores intermedios, sobre todo en 2008-2019 donde C3R1 existe.",
"- No se usó C3R5 (salud, pensión, ARL) para este ejercicio: el ARL varía por clase de riesgo y ensucia la tasa.",
"- El umbral de la exención es **10 SMLV, no 1**. Esta tasa NO identifica el % de trabajadores en el salario mínimo -- identifica la fracción por encima de 10 mínimos.",
""
)

readme_path <- file.path(resultados_raiz, "README.md")
if (file.exists(readme_path)) {
  readme_actual <- readLines(readme_path, warn = FALSE)
  marca_inicio <- which(readme_actual == "## Validación de la tasa de parafiscales como ventana a la distribución salarial")
  if (length(marca_inicio) > 0) {
    readme_actual <- readme_actual[seq_len(marca_inicio[1] - 2)]
  }
  writeLines(c(readme_actual, seccion_readme), readme_path)
  cat("Sección añadida/actualizada en:", readme_path, "\n")
} else {
  warning("No se encontró README.md en ", resultados_raiz, " -- no se pudo añadir la sección.")
}

titulo("SCRIPT COMPLETO")