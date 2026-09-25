# ==============================================================================
# diagnostico_incoherencia_parafiscales.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Rama:  feature/medida-exposicion-alternativa
#
# QUE HACE ESTE SCRIPT
# La validacion de la tasa de parafiscales (validar_tasa_parafiscales.R) paso
# las 2 pruebas decisivas, pero encontro que 28%-34% de las firmas tienen una
# fraccion implicita sobre 10 SMLV incoherente con su salario promedio
# observado. Este script atribuye esas firmas incoherentes a 4 causas
# candidatas: (1) base reducida del salario integral, (2) mezcla de
# poblaciones en el denominador C3R2+C3R4, (3) firmas no contribuyentes de
# renta (sin exencion), (4) ruido de reporte puro.
#
# QUE NO HACE: no construye medidas de exposicion, no estima nada, no decide
# cual de los 3 usos posibles de la tasa (tratamiento/control/particion de
# muestra) se adopta -- solo deja dicho cuales quedan habilitados.
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
compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3, carpeta = resultados_raiz) {
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

# SMLV historico, verificado via busqueda web (2022 y 2023 ya confirmados
# independientemente en scripts previos -- Decreto 1724/2021 y Decreto
# 2613/2022 -- coinciden exactamente con esta tabla, usada como control de
# calidad de la fuente para el resto de años).
SM_MENSUAL_COP <- c(
  `2008` = 461500, `2009` = 496900, `2010` = 515000, `2011` = 535600, `2012` = 566700,
  `2013` = 589500, `2014` = 616000, `2015` = 644350, `2016` = 689455, `2017` = 737717,
  `2018` = 781242, `2019` = 828116, `2020` = 877803, `2021` = 908526, `2022` = 1000000,
  `2023` = 1160000, `2024` = 1300000
)
SM_ANUAL_MILES <- SM_MENSUAL_COP * 12 / 1000

# ==============================================================================
# RECONSTRUCCION DE LA TASA, FRACCION IMPLICITA E INCOHERENCIA (categoria
# "total", TODOS los años -- misma base elegida en validar_tasa_
# parafiscales.R: C3R2+C3R4, "permanentes + temporal directo").
# ==============================================================================
titulo("RECONSTRUCCIÓN DE TASA/FRACCIÓN/INCOHERENCIA")

base <- panel %>%
  dplyr::transmute(
    NORDEMP, ANIO, tamano_empresa, CIIU4,
    parafiscales = safe_numeric(parafiscales_total_c3r6c3),
    c2 = safe_numeric(sueldos_permanentes_total_c3r2c3),
    c4 = safe_numeric(sueldos_prest_temporal_directo_total_c3r4c3),
    c1 = safe_numeric(salario_integral_total_c3r1c3),
    c20 = safe_numeric(impuesto_renta_equidad_total_c3r20c3),
    conteo_perm = obreros_permanentes + profesional_tecnico_permanentes + administrativos_permanentes,
    conteo_tempdir = obreros_temporal_directo + profesional_tecnico_temporal_directo + administrativos_temporal_directo
  ) %>%
  dplyr::mutate(
    conteo = conteo_perm + conteo_tempdir,
    nomina = c2 + c4,
    tasa = ifelse(nomina > 0, parafiscales / nomina, NA_real_),
    w_promedio_smlv = ifelse(conteo > 0, (nomina / conteo) / SM_ANUAL_MILES[as.character(ANIO)], NA_real_),
    fraccion_implicita = pmin(pmax((tasa - 0.04) / 0.05, 0), 1),
    inconsistente = (fraccion_implicita * 10) > (w_promedio_smlv * 1.05)
  )

n_incoherentes_2022 <- sum(base$inconsistente & base$ANIO == 2022, na.rm = TRUE)
n_total_2022 <- sum(!is.na(base$inconsistente) & base$ANIO == 2022 & !is.na(base$tasa) & !is.na(base$w_promedio_smlv) & base$conteo > 0)
cat("2022: ", n_incoherentes_2022, " firmas incoherentes de ", n_total_2022,
    " (", round(100 * n_incoherentes_2022 / n_total_2022, 1), "%)\n", sep = "")

# ==============================================================================
# CAUSA 1 -- BASE REDUCIDA DEL SALARIO INTEGRAL (2008-2019, donde existe C3R1)
# ==============================================================================
titulo("CAUSA 1: BASE REDUCIDA DEL SALARIO INTEGRAL (2008-2019)")

base_c1 <- base %>% dplyr::filter(ANIO %in% 2008:2019, !is.na(tasa), !is.na(w_promedio_smlv), conteo > 0)

# --- 1a. Tabla de contingencia: incoherente vs reporta C3R1 > 0 ---
base_c1 <- base_c1 %>% dplyr::mutate(reporta_integral = !is.na(c1) & c1 > 0)
tabla_contingencia_c1 <- table(incoherente = base_c1$inconsistente, reporta_integral = base_c1$reporta_integral)
cat("Tabla de contingencia (incoherente x reporta salario integral > 0), 2008-2019:\n")
print(tabla_contingencia_c1)
test_chi_c1 <- chisq.test(tabla_contingencia_c1)
cat("Chi-cuadrado:", round(test_chi_c1$statistic, 2), ", p-valor:", signif(test_chi_c1$p.value, 4), "\n")
pct_incoherente_con_integral <- round(100 * mean(base_c1$inconsistente[base_c1$reporta_integral]), 2)
pct_incoherente_sin_integral <- round(100 * mean(base_c1$inconsistente[!base_c1$reporta_integral]), 2)
cat("% incoherentes CON salario integral:", pct_incoherente_con_integral, "%\n")
cat("% incoherentes SIN salario integral:", pct_incoherente_sin_integral, "%\n")

tabla_c1a <- tibble::tibble(
  grupo = c("Reporta salario integral > 0", "No reporta salario integral"),
  n = c(sum(base_c1$reporta_integral), sum(!base_c1$reporta_integral)),
  pct_incoherente = c(pct_incoherente_con_integral, pct_incoherente_sin_integral)
)
guardar_tabla(as.data.frame(tabla_c1a), "IP_T01_contingencia_causa1",
              paste0("Causa 1: % incoherente según si reporta salario integral (chi2=", round(test_chi_c1$statistic,1), ", p=", signif(test_chi_c1$p.value,3), ")"))

# --- 1b. Entre las que reportan C3R1, ¿el peso predice la magnitud de la incoherencia? ---
base_c1 <- base_c1 %>%
  dplyr::mutate(
    magnitud_incoherencia = pmax(0, fraccion_implicita * 10 - w_promedio_smlv),
    peso_integral = ifelse((c1 + c2) > 0, c1 / (c1 + c2), NA_real_)
  )
con_integral <- base_c1 %>% dplyr::filter(reporta_integral, !is.na(peso_integral))
corr_peso_magnitud <- cor(con_integral$peso_integral, con_integral$magnitud_incoherencia, use = "pairwise.complete.obs")
cat("\nEntre firmas con salario integral: correlación peso_integral vs. magnitud de incoherencia:", round(corr_peso_magnitud, 3), "(n=", nrow(con_integral), ")\n")

g_peso_integral <- ggplot2::ggplot(con_integral %>% dplyr::filter(magnitud_incoherencia < quantile(magnitud_incoherencia, 0.99, na.rm=TRUE)),
                                    ggplot2::aes(x = peso_integral, y = magnitud_incoherencia)) +
  ggplot2::geom_point(alpha = 0.15, color = COLOR_BAJA) +
  ggplot2::geom_smooth(method = "loess", color = COLOR_ALTA, se = FALSE) +
  ggplot2::labs(title = "Magnitud de la incoherencia vs. peso del salario integral en la nómina",
                subtitle = paste0("Solo firmas con C3R1>0, 2008-2019. Correlación = ", round(corr_peso_magnitud, 3)),
                x = "Peso del salario integral (C3R1/(C3R1+C3R2))", y = "Magnitud de incoherencia (SMLV implícitos de más)") +
  tema_tesis
guardar_grafico(g_peso_integral, "IP01_magnitud_vs_peso_integral")

# --- 1c. Formula corregida: divisor efectivo mezclando 0.05 (tasa plena) y
#     0.023 (tasa reducida, base 70% para integral) segun el peso de C3R1
#     en la nomina total (C1+C2+C4) -- aproximacion, documentada como tal:
#     no se observa el headcount de integral por separado, se usa el peso
#     en pesos como proxy del peso en personas. ---
base_c1 <- base_c1 %>%
  dplyr::mutate(
    peso_integral_nomina = ifelse((c1 + c2 + c4) > 0, c1 / (c1 + c2 + c4), 0),
    divisor_corregido = 0.05 * (1 - peso_integral_nomina) + 0.023 * peso_integral_nomina,
    fraccion_corregida = pmin(pmax((tasa - 0.04) / divisor_corregido, 0), 1),
    inconsistente_corregido = (fraccion_corregida * 10) > (w_promedio_smlv * 1.05)
  )

pct_incoherente_original_2008_2019 <- round(100 * mean(base_c1$inconsistente), 2)
pct_incoherente_corregido_2008_2019 <- round(100 * mean(base_c1$inconsistente_corregido), 2)
cat("\n% incoherente (fórmula original), 2008-2019:", pct_incoherente_original_2008_2019, "%\n")
cat("% incoherente (fórmula corregida por salario integral), 2008-2019:", pct_incoherente_corregido_2008_2019, "%\n")

tabla_c1c <- tibble::tibble(
  version = c("Fórmula original (divisor fijo 0.05)", "Fórmula corregida (divisor mezclado según peso de C3R1)"),
  pct_incoherente = c(pct_incoherente_original_2008_2019, pct_incoherente_corregido_2008_2019)
)
guardar_tabla(as.data.frame(tabla_c1c), "IP_T02_correccion_causa1",
              "Causa 1: % de incoherencia antes y después de corregir por la base reducida del salario integral, 2008-2019")

titulo("CAUSA 1 COMPLETA")

# ==============================================================================
# CAUSA 2 -- EL DENOMINADOR MEZCLA DOS POBLACIONES (todos los años)
# ==============================================================================
titulo("CAUSA 2: MEZCLA DE POBLACIONES EN EL DENOMINADOR")

base <- base %>%
  dplyr::mutate(
    intensidad_temporal_directo = ifelse(nomina > 0, c4 / nomina, NA_real_),
    # Version con poblaciones emparejadas: SOLO C3R2 como base, SOLO
    # permanentes en el conteo -- mismo criterio de coherencia.
    tasa_c2_solo = ifelse(c2 > 0, parafiscales / c2, NA_real_),
    w_promedio_smlv_perm = ifelse(conteo_perm > 0, (c2 / conteo_perm) / SM_ANUAL_MILES[as.character(ANIO)], NA_real_),
    fraccion_implicita_c2_solo = pmin(pmax((tasa_c2_solo - 0.04) / 0.05, 0), 1),
    inconsistente_c2_solo = (fraccion_implicita_c2_solo * 10) > (w_promedio_smlv_perm * 1.05)
  )

base_2022 <- base %>% dplyr::filter(ANIO == 2022, !is.na(tasa), !is.na(w_promedio_smlv), conteo > 0)

corr_intensidad_incoherencia <- cor(as.numeric(base_2022$inconsistente), base_2022$intensidad_temporal_directo, use = "pairwise.complete.obs")
cat("Correlación (2022) entre incoherencia (0/1) e intensidad de temporal directo:", round(corr_intensidad_incoherencia, 3), "\n")

tabla_intensidad <- base_2022 %>%
  dplyr::filter(!is.na(intensidad_temporal_directo)) %>%
  dplyr::mutate(banda_intensidad = cut(intensidad_temporal_directo, breaks = c(-0.01, 0, 0.1, 0.3, 1.01),
                                        labels = c("0% (solo permanentes)", "0-10%", "10-30%", ">30%"))) %>%
  dplyr::group_by(banda_intensidad) %>%
  dplyr::summarise(n = dplyr::n(), pct_incoherente = round(100 * mean(inconsistente, na.rm = TRUE), 2), .groups = "drop")
cat("\nIncoherencia por banda de intensidad de temporal directo, 2022:\n")
print(as.data.frame(tabla_intensidad), row.names = FALSE)
guardar_tabla(as.data.frame(tabla_intensidad), "IP_T03_incoherencia_por_intensidad_temporal",
              paste0("Causa 2: % incoherente por banda de intensidad de temporal directo, 2022 (correlación=", round(corr_intensidad_incoherencia,3), ")"))

g_intensidad <- ggplot2::ggplot(base_2022 %>% dplyr::filter(!is.na(intensidad_temporal_directo)),
                                 ggplot2::aes(x = intensidad_temporal_directo, fill = inconsistente)) +
  ggplot2::geom_histogram(bins = 50, position = "fill") +
  ggplot2::scale_fill_manual(values = c("FALSE" = COLOR_BAJA, "TRUE" = COLOR_ALTA), labels = c("Coherente", "Incoherente"), name = NULL) +
  ggplot2::labs(title = "Incoherencia vs. intensidad de temporal directo en la nómina, 2022",
                x = "C3R4 / (C3R2+C3R4)", y = "Proporción de firmas") +
  tema_tesis
guardar_grafico(g_intensidad, "IP02_incoherencia_vs_intensidad_temporal")

# --- Comparacion directa: % incoherente con poblaciones emparejadas (solo
#     permanentes) vs. con la base mixta ---
comparables <- base_2022 %>% dplyr::filter(!is.na(inconsistente), !is.na(inconsistente_c2_solo))
pct_incoherente_mixto <- round(100 * mean(comparables$inconsistente), 2)
pct_incoherente_emparejado <- round(100 * mean(comparables$inconsistente_c2_solo), 2)
cat("\n% incoherente con base mixta (C2+C4):", pct_incoherente_mixto, "%\n")
cat("% incoherente con poblaciones emparejadas (solo C2 y solo permanentes):", pct_incoherente_emparejado, "%\n")

# De las firmas incoherentes bajo la base mixta, ¿cuántas se vuelven
# coherentes bajo la base emparejada? -- esta es la atribucion directa a
# Causa 2.
incoherentes_mixto <- comparables %>% dplyr::filter(inconsistente)
pct_se_resuelve_con_c2_solo <- round(100 * mean(!incoherentes_mixto$inconsistente_c2_solo, na.rm = TRUE), 2)
cat("De las firmas incoherentes (base mixta), % que se vuelve coherente con poblaciones emparejadas:", pct_se_resuelve_con_c2_solo, "%\n")

tabla_c2 <- tibble::tibble(
  metrica = c("% incoherente, base mixta (C2+C4)", "% incoherente, poblaciones emparejadas (solo C2/permanentes)",
              "De las incoherentes en base mixta, % que se resuelve con poblaciones emparejadas"),
  valor = c(pct_incoherente_mixto, pct_incoherente_emparejado, pct_se_resuelve_con_c2_solo)
)
guardar_tabla(as.data.frame(tabla_c2), "IP_T04_causa2_poblaciones_emparejadas",
              "Causa 2: efecto de emparejar poblaciones (solo permanentes) sobre la tasa de incoherencia, 2022")

titulo("CAUSA 2 COMPLETA")

# ==============================================================================
# CAUSA 3 -- FIRMAS NO CONTRIBUYENTES DE RENTA (2013-2024, donde existe C3R20)
# ==============================================================================
titulo("CAUSA 3: FIRMAS NO CONTRIBUYENTES DE RENTA")

# IMPORTANTE: C3R20 (Impuesto de Renta para la Equidad, CREE) existe en la
# macrobase 2013-2024, pero el CREE fue ELIMINADO por la Ley 1819 de 2016,
# vigente desde 2017 -- confirmado empiricamente: 64%-78% de las firmas lo
# reportan positivo en 2013-2016, y cae a ~0% desde 2017 (ver log de
# ejecucion). Usar 2022 (como en el resto del script) haria esta prueba
# inutil -- todas las firmas reportarian 0 sin importar si son o no
# contribuyentes de renta. Se usa 2013-2016 (pool), el ultimo periodo donde
# el CREE fue un impuesto real y discriminante.
base_c3 <- base %>% dplyr::filter(ANIO %in% 2013:2016, !is.na(tasa), !is.na(w_promedio_smlv), conteo > 0) %>%
  dplyr::mutate(reporta_c3r20 = !is.na(c20) & c20 > 0, ciiu2 = substr(as.character(CIIU4), 1, 2))

tabla_contingencia_c3 <- table(incoherente = base_c3$inconsistente, reporta_c3r20 = base_c3$reporta_c3r20)
cat("Tabla de contingencia (incoherente x reporta C3R20 > 0), 2013-2016:\n")
print(tabla_contingencia_c3)
test_chi_c3 <- chisq.test(tabla_contingencia_c3)
cat("Chi-cuadrado:", round(test_chi_c3$statistic, 2), ", p-valor:", signif(test_chi_c3$p.value, 4), "\n")

pct_c3r20_incoherentes <- round(100 * mean(base_c3$reporta_c3r20[base_c3$inconsistente]), 2)
pct_c3r20_coherentes <- round(100 * mean(base_c3$reporta_c3r20[!base_c3$inconsistente]), 2)
cat("% que reporta C3R20>0 entre INCOHERENTES:", pct_c3r20_incoherentes, "%\n")
cat("% que reporta C3R20>0 entre COHERENTES:  ", pct_c3r20_coherentes, "%\n")

# Comparacion DENTRO del mismo sector (CIIU a 2 digitos), para no confundir
# composicion sectorial con el efecto de interes.
por_sector_c3 <- base_c3 %>%
  dplyr::filter(!is.na(ciiu2), ciiu2 != "") %>%
  dplyr::group_by(ciiu2, inconsistente) %>%
  dplyr::summarise(n = dplyr::n(), pct_reporta_c3r20 = round(100 * mean(reporta_c3r20), 2), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = inconsistente, values_from = c(n, pct_reporta_c3r20), names_prefix = "incoherente_") %>%
  dplyr::filter(!is.na(pct_reporta_c3r20_incoherente_TRUE), !is.na(pct_reporta_c3r20_incoherente_FALSE)) %>%
  dplyr::mutate(diferencia_pp = pct_reporta_c3r20_incoherente_FALSE - pct_reporta_c3r20_incoherente_TRUE) %>%
  dplyr::arrange(dplyr::desc(diferencia_pp))
cat("\nPor sector (CIIU 2 dígitos): diferencia en % que reporta C3R20, coherentes vs. incoherentes:\n")
print(as.data.frame(por_sector_c3), row.names = FALSE)
guardar_tabla(as.data.frame(por_sector_c3), "IP_T05_causa3_c3r20_por_sector",
              "Causa 3: % que reporta C3R20>0, coherentes vs. incoherentes, por sector (CIIU 2 dígitos)")

diferencia_pp_promedio <- round(mean(por_sector_c3$diferencia_pp, na.rm = TRUE), 2)
cat("\nDiferencia promedio entre sectores (coherentes reportan C3R20 más que incoherentes, en pp):", diferencia_pp_promedio, "\n")

tabla_c3_resumen <- tibble::tibble(
  metrica = c("% reporta C3R20>0 entre incoherentes", "% reporta C3R20>0 entre coherentes", "Diferencia promedio por sector (pp)"),
  valor = c(pct_c3r20_incoherentes, pct_c3r20_coherentes, diferencia_pp_promedio)
)
guardar_tabla(as.data.frame(tabla_c3_resumen), "IP_T06_causa3_resumen",
              paste0("Causa 3: resumen (chi2=", round(test_chi_c3$statistic,1), ", p=", signif(test_chi_c3$p.value,3), ")"))

titulo("CAUSA 3 COMPLETA")

# ==============================================================================
# CAUSA 4 -- PERSISTENCIA (todos los años, se corre siempre)
# ==============================================================================
titulo("CAUSA 4: PERSISTENCIA AÑO A AÑO")

matriz_incoherencia <- base %>%
  dplyr::filter(!is.na(inconsistente)) %>%
  dplyr::select(NORDEMP, ANIO, inconsistente) %>%
  tidyr::pivot_wider(names_from = ANIO, values_from = inconsistente, names_prefix = "y")

# --- Persistencia consecutiva: P(incoherente en t | incoherente en t-1) vs.
#     tasa base P(incoherente) ---
anios_panel <- sort(unique(base$ANIO))
pares_consecutivos <- purrr::map_dfr(seq_along(anios_panel)[-1], function(i) {
  a0 <- anios_panel[i - 1]; a1 <- anios_panel[i]
  d <- base %>% dplyr::filter(ANIO %in% c(a0, a1), !is.na(inconsistente)) %>%
    dplyr::select(NORDEMP, ANIO, inconsistente) %>%
    tidyr::pivot_wider(names_from = ANIO, values_from = inconsistente, names_prefix = "y")
  col0 <- paste0("y", a0); col1 <- paste0("y", a1)
  if (!(col0 %in% names(d)) || !(col1 %in% names(d))) return(NULL)
  d <- d %>% dplyr::filter(!is.na(.data[[col0]]), !is.na(.data[[col1]]))
  if (nrow(d) == 0) return(NULL)
  tasa_base_t1 <- mean(d[[col1]])
  d_prev_incoh <- d %>% dplyr::filter(.data[[col0]])
  tasa_condicional <- if (nrow(d_prev_incoh) > 0) mean(d_prev_incoh[[col1]]) else NA_real_
  tibble::tibble(anio_desde = a0, anio_hasta = a1, n = nrow(d),
                 pct_incoherente_base = round(100 * tasa_base_t1, 2),
                 pct_incoherente_si_lo_era_antes = round(100 * tasa_condicional, 2))
})
cat("Persistencia año a año: P(incoherente en t | incoherente en t-1) vs. tasa base:\n")
print(as.data.frame(pares_consecutivos), row.names = FALSE)
guardar_tabla(as.data.frame(pares_consecutivos), "IP_T07_persistencia_anual",
              "Causa 4: persistencia de la incoherencia, año a año (condicional vs. tasa base)")

g_persistencia <- ggplot2::ggplot(pares_consecutivos, ggplot2::aes(x = anio_hasta)) +
  ggplot2::geom_line(ggplot2::aes(y = pct_incoherente_base, color = "Tasa base (todas las firmas)"), linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(y = pct_incoherente_si_lo_era_antes, color = "Firmas incoherentes el año anterior"), linewidth = 1) +
  ggplot2::labs(title = "Persistencia de la incoherencia: condicional vs. tasa base",
                x = NULL, y = "% incoherente", color = NULL) +
  tema_tesis
guardar_grafico(g_persistencia, "IP03_persistencia_condicional")

# --- Matriz 2019-2021-2022, como pide la tarea explicitamente ---
if (all(c("y2019", "y2021", "y2022") %in% names(matriz_incoherencia))) {
  matriz_3anios <- matriz_incoherencia %>%
    dplyr::filter(!is.na(y2019), !is.na(y2021), !is.na(y2022)) %>%
    dplyr::count(y2019, y2021, y2022) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 2)) %>%
    dplyr::arrange(dplyr::desc(n))
  cat("\nMatriz de persistencia 2019-2021-2022 (TRUE = incoherente):\n")
  print(as.data.frame(matriz_3anios), row.names = FALSE)
  guardar_tabla(as.data.frame(matriz_3anios), "IP_T08_matriz_persistencia_2019_2021_2022",
                "Causa 4: patrón conjunto de incoherencia en 2019, 2021 y 2022")

  siempre_incoherente <- matriz_3anios$pct[matriz_3anios$y2019 & matriz_3anios$y2021 & matriz_3anios$y2022]
  nunca_incoherente <- matriz_3anios$pct[!matriz_3anios$y2019 & !matriz_3anios$y2021 & !matriz_3anios$y2022]
  cat("\n% de firmas incoherentes en LOS 3 años (2019,2021,2022):", ifelse(length(siempre_incoherente)==0, 0, siempre_incoherente), "%\n")
  cat("% de firmas coherentes en LOS 3 años:", ifelse(length(nunca_incoherente)==0, 0, nunca_incoherente), "%\n")
}

titulo("CAUSA 4 COMPLETA")

# ==============================================================================
# TABLA DE ATRIBUCION FINAL
# ==============================================================================
titulo("TABLA DE ATRIBUCIÓN FINAL")

# AVISO IMPORTANTE: las 4 causas se prueban en ventanas de años distintas
# (Causa 1 solo es observable 2008-2019, donde existe C3R1; Causa 3 solo es
# observable 2013-2016, donde el CREE era un impuesto vigente; Causa 2 y 4
# si son observables en 2022). Por eso NO se puede construir un "waterfall"
# estricto que sume a 100% sobre las mismas firmas de 2022 -- se reporta la
# fuerza de la evidencia de cada causa en su propia ventana valida, sin
# forzar una precision que los datos no permiten.

tabla_atribucion <- tibble::tribble(
  ~causa, ~ventana_valida, ~evidencia_principal, ~pct_incoherencia_resuelto, ~veredicto,
  "1. Base reducida salario integral", "2008-2019",
    "80.95% incoherentes CON integral vs 61.14% SIN (chi2=2789, p≈0). Pero la corrección probada (divisor mezclado) NO reduce la incoherencia: 65.27% -> 65.51%.",
    "0% (la corrección probada no funciona)", "Asociada, NO resuelta con esta corrección",
  "2. Mezcla de poblaciones (C2+C4)", "todos los años",
    "Correlación incoherencia vs. intensidad temporal directo ≈ 0 (-0.019). Poblaciones emparejadas EMPEORAN: 34.9% -> 51.24% incoherente.",
    "-16.3pp (empeora)", "Rechazada",
  "3. Firmas no contribuyentes de renta", "2013-2016 (único período con C3R20 informativo)",
    "66.8% de incoherentes reporta C3R20>0 vs. 77.82% de coherentes (11pp, chi2=506, p=4.6e-112), consistente en 23 sectores (dif. promedio 9.98pp).",
    "No medible directamente en 2022 (C3R20=0 universal desde 2017, Ley 1819)", "Evidencia más fuerte de las 4, pero no verificable en el año de interés",
  "4. Ruido de reporte", "todos los años",
    "Persistencia condicional post-2013: 57%-70% vs. tasa base 25%-35%. En 2019-2021-2022: 10.46% SIEMPRE incoherente, 40.42% NUNCA, 49.12% intermitente.",
    "~40% del patrón es compatible con ruido puro (nunca incoherente); ~10% persistente (estructural, sin resolver)", "Parcial: mayoritariamente ruido, con un núcleo estructural no resuelto"
)
cat("Tabla de atribución final:\n")
print(as.data.frame(tabla_atribucion), row.names = FALSE)
guardar_tabla(as.data.frame(tabla_atribucion), "IP_T09_atribucion_final",
              "Tabla de atribución: evidencia por causa (no es un waterfall estricto -- ver aviso en el script)")

cat("\n% de incoherencia en 2022 (34.2%) que queda SIN una corrección medible y aplicable en ese año: prácticamente el total -- ",
    "ninguna de las 4 causas produjo una corrección que se pueda aplicar y verificar en 2022 específicamente.\n")

# ==============================================================================
# REPORTE (sección nueva en el README)
# ==============================================================================
titulo("REPORTE FINAL")

seccion_readme <- c(
"",
"## Diagnóstico: por qué 28-34% de firmas tienen fracción implícita incoherente",
"",
paste0("Atribuye la incoherencia encontrada en la validación de la tasa de parafiscales (28%-34% de ",
       "firmas, según categoría) a 4 causas candidatas. Ver `IP_T09_atribucion_final` para la síntesis. ",
       "**Aviso metodológico**: las 4 causas se prueban en ventanas de años distintas -- Causa 1 solo es ",
       "observable 2008-2019 (donde existe `C3R1`), Causa 3 solo 2013-2016 (único período donde el CREE ",
       "fue un impuesto real y `C3R20` es informativo), Causa 2 y 4 en todos los años. Por eso esta ",
       "sección NO es un *waterfall* que sume 100% sobre las firmas incoherentes de 2022 -- se reporta la ",
       "fuerza de la evidencia de cada causa en su propia ventana válida."),
"",
"### Causa 1: base reducida del salario integral -- asociada, NO resuelta",
"",
paste0("Ver `IP_T01_contingencia_causa1`, `IP01_magnitud_vs_peso_integral`, `IP_T02_correccion_causa1`. La ",
       "asociación es fuerte: 80.95% de las firmas con salario integral positivo son incoherentes, frente a ",
       "61.14% de las que no reportan salario integral (chi²=2789, p≈0, 2008-2019). Pero la corrección ",
       "probada -- un divisor mezclado entre 0.05 (tasa plena) y 0.023 (tasa reducida al 70% de base) según ",
       "el peso de `C3R1` en la nómina -- **no reduce la incoherencia**: pasa de 65.27% a 65.51%, ",
       "prácticamente sin cambio. La corrección, si acaso, empeora ligeramente. Hipótesis más plausible: la ",
       "corrección solo ajusta la fórmula tasa→fracción, pero `w_promedio` (el salario promedio contra el ",
       "que se contrasta) probablemente también está distorsionado -- `C3R2` (sueldos permanentes) parece ",
       "EXCLUIR a los trabajadores con salario integral (son filas separadas del cuadro 3), pero el conteo ",
       "de personal permanente (`C4R2`) probablemente SÍ los incluye, así que `w_promedio` se calcula ",
       "dividiendo una nómina que excluye a los integral-salariados entre un conteo que sí los incluye -- ",
       "subestimándolo. No se pudo corregir esto sin un conteo específico de trabajadores con salario ",
       "integral, que la EAM no reporta por separado. **Veredicto: asociada, pero no se logró una ",
       "corrección funcional con los datos disponibles.**"),
"",
"### Causa 2: mezcla de poblaciones en el denominador -- rechazada",
"",
paste0("Ver `IP_T03_incoherencia_por_intensidad_temporal`, `IP02_incoherencia_vs_intensidad_temporal`, ",
       "`IP_T04_causa2_poblaciones_emparejadas`. La correlación entre incoherencia e intensidad de temporal ",
       "directo es prácticamente nula (-0.019), y por bandas no hay un patrón monótono (31.7% / 42.3% / ",
       "44.7% / 33.4% de incoherencia según la intensidad crece). La prueba decisiva: usar poblaciones ",
       "perfectamente emparejadas (solo `C3R2`, solo conteo de permanentes) **empeora** la incoherencia, de ",
       "34.9% a 51.24% -- de las firmas incoherentes bajo la base mixta, solo 0.75% se vuelve coherente al ",
       "emparejar poblaciones. **Veredicto: rechazada.** La base C2+C4 no solo es mejor para la ",
       "bimodalidad (ya establecido en la validación anterior, 98% vs. 85.67% dentro de rango), también es ",
       "mejor para la coherencia -- no es la causa del problema."),
"",
"### Causa 3: firmas no contribuyentes de renta -- la evidencia más fuerte, no verificable en 2022",
"",
paste0("Ver `IP_T05_causa3_c3r20_por_sector`, `IP_T06_causa3_resumen`. **Hallazgo previo al diagnóstico ",
       "mismo**: `C3R20` (Impuesto de Renta para la Equidad, CREE) existe en la macrobase 2013-2024, pero ",
       "el CREE fue eliminado por la Ley 1819 de 2016 -- confirmado empíricamente, 64%-78% de firmas lo ",
       "reportan positivo en 2013-2016, cae a ~0% desde 2017. Probar esta causa en 2022 (como el resto del ",
       "diagnóstico) habría sido inútil. Se usó 2013-2016, el último período donde `C3R20` discrimina. ",
       "Resultado: 66.8% de las firmas incoherentes reporta `C3R20`>0, frente a 77.82% de las coherentes -- ",
       "una brecha de 11 puntos porcentuales (chi²=506, p=4.6e-112), **consistente en dirección en los 23 ",
       "sectores con datos suficientes** (diferencia promedio 9.98pp, coherentes siempre reportan más). Es ",
       "la evidencia más limpia y consistente de las 4 causas. **Pero no se puede verificar directamente en ",
       "2022**, el año de interés, porque `C3R20` ya no discrimina ahí. Es plausible que el estatus de ",
       "contribuyente de renta sea una característica relativamente estable de la firma en el tiempo, pero ",
       "eso no se comprobó -- es una extrapolación, no un hallazgo directo para 2022."),
"",
"### Causa 4: ruido de reporte -- mayoritariamente sí, con un núcleo estructural",
"",
paste0("Ver `IP_T07_persistencia_anual`, `IP03_persistencia_condicional`, `IP_T08_matriz_persistencia_2019_2021_2022`. ",
       "Antes de 2013 la incoherencia es casi universal (~98%, esperado: el concepto de \"fracción sobre 10 ",
       "SMLV\" no aplica sin la exención). Desde 2013, la persistencia condicional (57%-70%, dado que la ",
       "firma fue incoherente el año anterior) es sistemáticamente más alta que la tasa base (25%-35%) -- ",
       "**no es ruido puro**, hay un componente estructural. La matriz 2019-2021-2022 lo cuantifica: 40.42% ",
       "de las firmas nunca es incoherente en esos 3 años (compatible con ruido en los casos aislados que sí ",
       "aparecen), pero 10.46% es incoherente **los 3 años** -- un núcleo persistente y estructural, sin ",
       "explicación identificada por las causas 1-3. El resto (49.12%) es intermitente. **Veredicto: la ",
       "incoherencia NO es ruido puro** -- hay un subconjunto persistente de firmas (~10%) con una causa ",
       "estructural todavía sin identificar."),
"",
"### Qué usos quedan habilitados (no se decide aquí)",
"",
paste0("**Uso 1 (medida de tratamiento)**: NO habilitado con la evidencia actual. Ninguna corrección probada ",
       "resuelve la incoherencia en el año de interés (2022); usar la fracción implícita firma por firma ",
       "como medida de exposición heredaría ese ~34% de incoherencia sin explicación aplicable."),
"",
paste0("**Uso 2 (control en la especificación principal)**: sigue habilitado. Este uso solo requiere que la ",
       "tasa ORDENE bien la exposición relativa entre firmas, no que sea exacta firma por firma. La ",
       "correlación de -0.05 con `Bite2022_obreros` (validación anterior) y la validación externa fuerte ",
       "contra salario integral (p<1e-40) sostienen que hay señal real y en buena medida ortogonal a Kaitz, ",
       "incluso con el ~34% de casos individualmente incoherentes -- mientras la incoherencia no esté ",
       "sistemáticamente correlacionada con el propio Kaitz de forma que invierta el ordenamiento (no se ",
       "probó esto explícitamente, queda pendiente antes de usar la tasa como control)."),
"",
paste0("**Uso 3 (partición de muestra)**: sigue habilitado, y es el menos exigente de los tres. Mostrar que ",
       "el resultado principal se sostiene en el subconjunto de firmas con fracción baja sobre 10 SMLV no ",
       "requiere que la fracción sea exacta para las firmas incoherentes -- solo que las firmas de fracción ",
       "claramente baja (cerca de 0.04) estén correctamente clasificadas, y esas son precisamente las menos ",
       "propensas a la incoherencia (la incoherencia ocurre cuando la fracción implícita es ",
       "inverosímilmente ALTA, no baja)."),
"",
paste0("**Nota sobre persistencia (Causa 4)**: los usos 2 y 3 sobreviven aunque la Causa 4 sea dominante en ",
       "parte de la muestra, siempre que el ordenamiento entre firmas sea estable -- pero el núcleo del ",
       "10.46% con incoherencia persistente en los 3 años (2019, 2021, 2022) es señal de un riesgo real: ",
       "si esas firmas también tienen Kaitz sistemáticamente distinto, podrían sesgar el uso 2. No se probó ",
       "esto tampoco -- queda pendiente."),
"",
paste0("**Nota para la tesis**: la Ley 1607 de 2013 aparece dos veces en este proyecto -- se descartó en su ",
       "momento como choque de interés (no era el foco de la tesis), y reaparece aquí como instrumento de ",
       "medición de la distribución salarial, aprovechando su propia estructura de exención para inferir la ",
       "composición de la nómina. Es un uso más interesante que el original y vale la pena contarlo así."),
""
)

readme_path <- file.path(resultados_raiz, "README.md")
if (file.exists(readme_path)) {
  readme_actual <- readLines(readme_path, warn = FALSE)
  marca_inicio <- which(readme_actual == "## Diagnóstico: por qué 28-34% de firmas tienen fracción implícita incoherente")
  if (length(marca_inicio) > 0) {
    readme_actual <- readme_actual[seq_len(marca_inicio[1] - 2)]
  }
  writeLines(c(readme_actual, seccion_readme), readme_path)
  cat("Sección añadida/actualizada en:", readme_path, "\n")
} else {
  warning("No se encontró README.md en ", resultados_raiz, " -- no se pudo añadir la sección.")
}

titulo("SCRIPT COMPLETO")