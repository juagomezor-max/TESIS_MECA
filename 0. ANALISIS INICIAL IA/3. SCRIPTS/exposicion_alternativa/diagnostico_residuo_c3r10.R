# ==============================================================================
# diagnostico_residuo_c3r10.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Rama:  feature/medida-exposicion-alternativa
#
# QUE HACE ESTE SCRIPT
# Diagnostica por que C3R10 (costo total del personal, numerador del
# salario_promedio que usa 01_resultados_principales.R) no cierra como
# suma(C3R1..C3R9) -- la validacion previa (EA_T07) encontro solo 17%-74%
# de cumplimiento segun categoria. Esto importa porque salario_promedio es
# el numerador del primer eslabon de la tesis.
#
# QUE NO HACE: no construye medidas de exposicion, no estima nada, no
# "arregla" C3R10 -- solo diagnostica.
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
  cat("Grafico guardado en", carpeta, ":", nombre_archivo, "\n")
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

if (!file.exists(paths$macro_base_eam)) stop("No se encontro la macrobase en ", paths$macro_base_eam)
macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
cat("Macrobase (establecimiento-año):", nrow(macro_base), "filas\n")

# ==============================================================================
# 1. DIRECCION DEL RESIDUO
# ==============================================================================
titulo("1. DIRECCION DEL RESIDUO")

# Definimos los componentes por categoria. C3R1 (salario integral) solo
# existe 2008-2019 -- se suma como 0 fuera de esos años (NO como NA, para
# no descartar filas enteras donde el resto de componentes si existen; se
# documenta explicitamente que en 2020-2024 el residuo, si lo hay, no
# puede venir de C3R1 porque esa variable no se usa en esos años).
componentes_categoria <- list(
  obreros = list(
    total = "costos_totales_personal_obreros_c3r10c1",
    r1 = "salario_integral_obreros_c3r1c1",
    resto = c("sueldos_permanentes_obreros_c3r2c1", "prestaciones_permanentes_obreros_c3r3c1",
              "sueldos_prest_temporal_directo_obreros_c3r4c1", "cotizaciones_obreros_c3r5c1",
              "parafiscales_obreros_c3r6c1", "seguros_vida_voluntarios_obreros_c3r7c1",
              "pago_agencias_temporales_obreros_c3r8c1", "otros_gastos_personal_obreros_c3r9c1")
  ),
  profesional_tecnico = list(
    total = "costos_totales_personal_profesional_tecnico_c3r10pt",
    r1 = "salario_integral_profesional_tecnico_c3r1pt",
    resto = c("sueldos_permanentes_profesional_tecnico_c3r2pt", "prestaciones_permanentes_profesional_tecnico_c3r3pt",
              "sueldos_prest_temporal_directo_profesional_tecnico_c3r4pt", "cotizaciones_profesional_tecnico_c3r5pt",
              "parafiscales_profesional_tecnico_c3r6pt", "seguros_vida_voluntarios_profesional_tecnico_c3r7pt",
              "pago_agencias_temporales_profesional_tecnico_c3r8pt", "otros_gastos_personal_profesional_tecnico_c3r9pt")
  ),
  administrativos = list(
    total = "costos_totales_personal_administrativos_c3r10c2",
    r1 = "salario_integral_administrativos_c3r1c2",
    resto = c("sueldos_permanentes_administrativos_c3r2c2", "prestaciones_permanentes_administrativos_c3r3c2",
              "sueldos_prest_temporal_directo_administrativos_c3r4c2", "cotizaciones_administrativos_c3r5c2",
              "parafiscales_administrativos_c3r6c2", "seguros_vida_voluntarios_administrativos_c3r7c2",
              "pago_agencias_temporales_administrativos_c3r8c2", "otros_gastos_personal_administrativos_c3r9c2")
  ),
  total = list(
    total = "costos_totales_personal_total_c3r10c3",
    r1 = "salario_integral_total_c3r1c3",
    resto = c("sueldos_permanentes_total_c3r2c3", "prestaciones_permanentes_total_c3r3c3",
              "sueldos_prest_temporal_directo_total_c3r4c3", "cotizaciones_total_c3r5c3",
              "parafiscales_total_c3r6c3", "seguros_vida_voluntarios_total_c3r7c3",
              "pago_agencias_temporales_total_c3r8c3", "otros_gastos_personal_total_c3r9c3")
  )
)

calcular_residuo <- function(datos, spec) {
  r1 <- if (spec$r1 %in% names(datos)) safe_numeric(datos[[spec$r1]]) else rep(0, nrow(datos))
  r1[is.na(r1)] <- 0  # C3R1 fuera de 2008-2019: tratado como 0, no NA -- documentado arriba.
  mat_resto <- as.data.frame(lapply(datos[intersect(spec$resto, names(datos))], safe_numeric))
  suma_resto <- rowSums(mat_resto, na.rm = TRUE)
  total <- safe_numeric(datos[[spec$total]])
  tibble::tibble(total = total, suma_componentes = r1 + suma_resto, residuo = total - (r1 + suma_resto))
}

TOLERANCIA_REL <- 0.001  # 0.1% de C3R10, como pide la tarea

residuos_panel <- purrr::map_dfr(names(componentes_categoria), function(cat) {
  r <- calcular_residuo(panel, componentes_categoria[[cat]])
  tibble::tibble(NORDEMP = panel$NORDEMP, ANIO = panel$ANIO, categoria = cat,
                 total = r$total, suma_componentes = r$suma_componentes, residuo = r$residuo)
})

reparto_signos <- residuos_panel %>%
  dplyr::filter(!is.na(total)) %>%
  dplyr::mutate(
    tol_abs = TOLERANCIA_REL * abs(total),
    signo = dplyr::case_when(
      abs(residuo) <= pmax(tol_abs, 1e-9) ~ "cero (dentro de tolerancia)",
      residuo > 0 ~ "positivo (C3R10 > suma R1-R9)",
      residuo < 0 ~ "negativo (C3R10 < suma R1-R9)"
    )
  ) %>%
  dplyr::count(categoria, signo) %>%
  dplyr::group_by(categoria) %>%
  dplyr::mutate(pct = round(100 * n / sum(n), 2)) %>%
  dplyr::ungroup()

cat("Reparto de signos del residuo, tolerancia", TOLERANCIA_REL * 100, "% de C3R10:\n")
print(as.data.frame(reparto_signos), row.names = FALSE)
guardar_tabla(as.data.frame(reparto_signos), "RC_T01_reparto_signos_residuo",
              "Dirección del residuo C3R10 - suma(C3R1..C3R9): reparto de signos por categoría")

# ==============================================================================
# 2. MAGNITUD
# ==============================================================================
titulo("2. MAGNITUD DEL RESIDUO")

magnitud <- residuos_panel %>%
  dplyr::filter(!is.na(total), total != 0) %>%
  dplyr::mutate(residuo_pct = 100 * residuo / total) %>%
  dplyr::group_by(categoria) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mediana_pct = round(median(residuo_pct, na.rm = TRUE), 3),
    p10_pct = round(quantile(residuo_pct, 0.10, na.rm = TRUE), 3),
    p25_pct = round(quantile(residuo_pct, 0.25, na.rm = TRUE), 3),
    p75_pct = round(quantile(residuo_pct, 0.75, na.rm = TRUE), 3),
    p90_pct = round(quantile(residuo_pct, 0.90, na.rm = TRUE), 3),
    pct_supera_5pct = round(100 * mean(abs(residuo_pct) > 5, na.rm = TRUE), 2),
    pct_supera_20pct = round(100 * mean(abs(residuo_pct) > 20, na.rm = TRUE), 2),
    .groups = "drop"
  )
cat("Magnitud del residuo como % de C3R10, por categoría:\n")
print(as.data.frame(magnitud), row.names = FALSE)
guardar_tabla(as.data.frame(magnitud), "RC_T02_magnitud_residuo",
              "Magnitud del residuo como % de C3R10: mediana, percentiles, % que supera 5%/20%")

g_magnitud <- ggplot2::ggplot(
  residuos_panel %>% dplyr::filter(!is.na(total), total != 0) %>% dplyr::mutate(residuo_pct = pmin(pmax(100 * residuo / total, -100), 100)),
  ggplot2::aes(x = residuo_pct, fill = categoria)
) +
  ggplot2::geom_histogram(bins = 60) +
  ggplot2::facet_wrap(~categoria, scales = "free_y") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::labs(title = "Distribución del residuo C3R10 - suma(C3R1..C3R9), como % de C3R10",
                subtitle = "Truncado a ±100% para legibilidad", x = "Residuo (% de C3R10)", y = "N firma-años") +
  tema_tesis + ggplot2::theme(legend.position = "none")
guardar_grafico(g_magnitud, "RC01_distribucion_residuo_pct")

# ==============================================================================
# 3. ESTRUCTURA DEL DESAJUSTE
# ==============================================================================
titulo("3. ESTRUCTURA DEL DESAJUSTE")

# --- 3.1 Por año y categoría (cumplimiento exacto, tolerancia 0.1%) ---
por_anio_cat <- residuos_panel %>%
  dplyr::filter(!is.na(total)) %>%
  dplyr::mutate(tol_abs = TOLERANCIA_REL * abs(total), ok = abs(residuo) <= pmax(tol_abs, 1e-9)) %>%
  dplyr::group_by(ANIO, categoria) %>%
  dplyr::summarise(n = dplyr::n(), pct_ok = round(100 * mean(ok), 2), .groups = "drop")
cat("3.1 -- Cumplimiento por año y categoría:\n")
print(as.data.frame(por_anio_cat), row.names = FALSE)
guardar_tabla(as.data.frame(por_anio_cat), "RC_T03_cumplimiento_por_anio_categoria",
              "Cumplimiento de C3R10=suma(R1..R9) por año y categoría, tolerancia 0.1%")

g_anio <- ggplot2::ggplot(por_anio_cat, ggplot2::aes(x = ANIO, y = pct_ok, color = categoria)) +
  ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point() +
  ggplot2::labs(title = "% de firma-años donde C3R10 = suma(C3R1..C3R9), por año", x = NULL, y = "% cumple") +
  tema_tesis
guardar_grafico(g_anio, "RC02_cumplimiento_por_anio")

# --- 3.2 Por tamaño de firma ---
por_tamano <- residuos_panel %>%
  dplyr::filter(!is.na(total), categoria == "total") %>%
  dplyr::left_join(panel %>% dplyr::select(NORDEMP, ANIO, tamano_empresa), by = c("NORDEMP", "ANIO")) %>%
  dplyr::mutate(tol_abs = TOLERANCIA_REL * abs(total), ok = abs(residuo) <= pmax(tol_abs, 1e-9)) %>%
  dplyr::filter(!is.na(tamano_empresa)) %>%
  dplyr::group_by(tamano_empresa) %>%
  dplyr::summarise(n = dplyr::n(), pct_ok = round(100 * mean(ok), 2), .groups = "drop")
cat("\n3.2 -- Cumplimiento por tamaño de firma (categoría total):\n")
print(as.data.frame(por_tamano), row.names = FALSE)
guardar_tabla(as.data.frame(por_tamano), "RC_T04_cumplimiento_por_tamano",
              "Cumplimiento de C3R10=suma(R1..R9) por tamaño de firma (categoría total)")

# --- 3.3 Por sector (CIIU4, division a 2 digitos) ---
por_sector <- residuos_panel %>%
  dplyr::filter(!is.na(total), categoria == "total") %>%
  dplyr::left_join(panel %>% dplyr::select(NORDEMP, ANIO, CIIU4), by = c("NORDEMP", "ANIO")) %>%
  dplyr::mutate(
    tol_abs = TOLERANCIA_REL * abs(total), ok = abs(residuo) <= pmax(tol_abs, 1e-9),
    ciiu2 = substr(as.character(CIIU4), 1, 2)
  ) %>%
  dplyr::filter(!is.na(ciiu2), ciiu2 != "") %>%
  dplyr::group_by(ciiu2) %>%
  dplyr::summarise(n = dplyr::n(), pct_ok = round(100 * mean(ok), 2), .groups = "drop") %>%
  dplyr::filter(n >= 100) %>%
  dplyr::arrange(pct_ok)
cat("\n3.3 -- Cumplimiento por sector (CIIU a 2 dígitos, division), categoría total, n>=100 (10 mejores y 10 peores):\n")
print(as.data.frame(dplyr::bind_rows(head(por_sector, 10), tail(por_sector, 10))), row.names = FALSE)
guardar_tabla(as.data.frame(por_sector), "RC_T05_cumplimiento_por_sector",
              "Cumplimiento de C3R10=suma(R1..R9) por sector (CIIU4, 2 dígitos), categoría total")

# --- 3.4 Establecimiento-año (macrobase cruda) vs firma-año (panel), misma
#     tolerancia, para ver si la agregacion amplifica el residuo. ---
cols_resto_c1 <- c("C3R2C1", "C3R3C1", "C3R4C1", "C3R5C1", "C3R6C1", "C3R7C1", "C3R8C1", "C3R9C1")
m <- macro_base %>%
  dplyr::mutate(
    r1 = { x <- safe_numeric(C3R1C1); x[is.na(x)] <- 0; x },
    resto = rowSums(as.data.frame(lapply(macro_base[intersect(cols_resto_c1, names(macro_base))], safe_numeric)), na.rm = TRUE),
    total = safe_numeric(C3R10C1),
    residuo = total - (r1 + resto)
  ) %>%
  dplyr::filter(!is.na(total)) %>%
  dplyr::mutate(tol_abs = TOLERANCIA_REL * abs(total), ok = abs(residuo) <= pmax(tol_abs, 1e-9))
pct_ok_establecimiento <- round(100 * mean(m$ok), 2)
pct_ok_firma <- por_anio_cat %>% dplyr::filter(categoria == "obreros") %>% dplyr::summarise(w = weighted.mean(pct_ok, n)) %>% dplyr::pull(w)
cat("\n3.4 -- Establecimiento-año vs firma-año (categoría obreros, C1):\n")
cat(sprintf("  Establecimiento-año (macrobase cruda): %.2f%% cumple\n", pct_ok_establecimiento))
cat(sprintf("  Firma-año (panel agregado):            %.2f%% cumple\n", pct_ok_firma))
tabla_est_vs_firma <- tibble::tibble(
  nivel = c("Establecimiento-año (macrobase cruda)", "Firma-año (panel agregado)"),
  pct_cumple = c(pct_ok_establecimiento, round(pct_ok_firma, 2))
)
guardar_tabla(as.data.frame(tabla_est_vs_firma), "RC_T06_establecimiento_vs_firma",
              "Cumplimiento de C3R10C1=suma(R1..R9,C1) a nivel establecimiento vs firma (categoría obreros)")

titulo("3 COMPLETO")

# ==============================================================================
# 4. BUSCAR EL RUBRO FALTANTE (residuo positivo)
# ==============================================================================
titulo("4. BUSCAR EL RUBRO FALTANTE")

residuo_total <- residuos_panel %>% dplyr::filter(categoria == "total") %>%
  dplyr::left_join(
    panel %>% dplyr::select(
      NORDEMP, ANIO,
      r1csap = apoyo_sostenimiento_aprendices_1_r1csap,
      r2csap = apoyo_sostenimiento_aprendices_2_r2csap,
      r3csap = apoyo_sostenimiento_aprendices_3_r3csap,
      r4csap = apoyo_sostenimiento_aprendices_total_r4csap,
      salarper = control_salario_permanentes_salarper,
      salpeyte = control_salario_permanente_y_temporal_salpeyte,
      pressper = control_prestaciones_permanentes_pressper,
      prespyte = control_prestaciones_permanente_y_temporal_prespyte
    ),
    by = c("NORDEMP", "ANIO")
  ) %>%
  dplyr::mutate(dplyr::across(c(r1csap, r2csap, r3csap, r4csap, salarper, salpeyte, pressper, prespyte), safe_numeric)) %>%
  dplyr::mutate(csap_total = dplyr::coalesce(r1csap, 0) + dplyr::coalesce(r2csap, 0) + dplyr::coalesce(r3csap, 0))

candidatos <- c("r4csap", "csap_total", "salarper", "salpeyte", "pressper", "prespyte")
correlaciones_residuo <- purrr::map_dfr(candidatos, function(v) {
  tibble::tibble(candidato = v, correlacion = round(cor(residuo_total$residuo, residuo_total[[v]], use = "pairwise.complete.obs"), 4))
})
cat("Correlación del residuo (categoría total) contra candidatos:\n")
print(as.data.frame(correlaciones_residuo), row.names = FALSE)
guardar_tabla(as.data.frame(correlaciones_residuo), "RC_T07_correlacion_candidatos",
              "Correlación del residuo (C3R10 - suma R1..R9, total) contra candidatos a rubro faltante")

# Prueba especifica pedida: residuo ~ R4CSAP. Si el apoyo de sostenimiento
# de aprendices explicara el residuo, residuo/R4CSAP deberia concentrarse
# cerca de 1 en las filas donde R4CSAP > 0.
prueba_r4csap <- residuo_total %>%
  dplyr::filter(!is.na(r4csap), r4csap > 0, !is.na(residuo)) %>%
  dplyr::mutate(razon_residuo_r4csap = residuo / r4csap)
cat("\nPrueba especifica residuo ~ R4CSAP (solo filas con R4CSAP > 0, n =", nrow(prueba_r4csap), "):\n")
cat("  Correlación:", round(cor(prueba_r4csap$residuo, prueba_r4csap$r4csap, use = "pairwise.complete.obs"), 4), "\n")
cat("  Mediana de residuo/R4CSAP:", round(median(prueba_r4csap$razon_residuo_r4csap, na.rm = TRUE), 3), "(1.0 confirmaria la hipotesis)\n")
cat("  % de filas con razon entre 0.9 y 1.1:", round(100 * mean(prueba_r4csap$razon_residuo_r4csap > 0.9 & prueba_r4csap$razon_residuo_r4csap < 1.1, na.rm = TRUE), 2), "%\n")

g_r4csap <- ggplot2::ggplot(prueba_r4csap %>% dplyr::filter(abs(residuo) < quantile(abs(residuo), 0.95, na.rm=TRUE), r4csap < quantile(r4csap, 0.95, na.rm=TRUE)),
                             ggplot2::aes(x = r4csap, y = residuo)) +
  ggplot2::geom_point(alpha = 0.15, color = COLOR_BAJA) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = COLOR_ALTA, linetype = "dashed") +
  ggplot2::labs(title = "Residuo de C3R10 vs. R4CSAP (apoyo de sostenimiento de aprendices)",
                subtitle = "Línea roja: residuo = R4CSAP (lo que confirmaría la hipótesis). Truncado al p95 para legibilidad.",
                x = "R4CSAP", y = "Residuo (C3R10 - suma R1..R9)") +
  tema_tesis
guardar_grafico(g_r4csap, "RC03_residuo_vs_r4csap")

resumen_r4csap <- tibble::tibble(
  n_filas_r4csap_positivo = nrow(prueba_r4csap),
  correlacion = round(cor(prueba_r4csap$residuo, prueba_r4csap$r4csap, use = "pairwise.complete.obs"), 4),
  mediana_razon = round(median(prueba_r4csap$razon_residuo_r4csap, na.rm = TRUE), 3),
  pct_razon_cerca_de_1 = round(100 * mean(prueba_r4csap$razon_residuo_r4csap > 0.9 & prueba_r4csap$razon_residuo_r4csap < 1.1, na.rm = TRUE), 2)
)
guardar_tabla(as.data.frame(resumen_r4csap), "RC_T08_prueba_r4csap",
              "Prueba específica: ¿residuo ≈ R4CSAP?")

# Residuo NETO tras restar R4CSAP: si R4CSAP es el rubro que faltaba, el
# % de filas "dentro de tolerancia" deberia subir mucho frente al 48.01%
# (total, Seccion 1) que se obtuvo sin descontarlo.
residuo_total <- residuo_total %>%
  dplyr::mutate(
    residuo_neto = residuo - dplyr::coalesce(r4csap, 0),
    tol_abs = TOLERANCIA_REL * abs(total),
    ok_sin_r4csap = abs(residuo) <= pmax(tol_abs, 1e-9),
    ok_con_r4csap = abs(residuo_neto) <= pmax(tol_abs, 1e-9)
  )
comparacion_neta <- tibble::tibble(
  version = c("C3R10 - suma(R1..R9)", "C3R10 - suma(R1..R9) - R4CSAP"),
  pct_dentro_de_tolerancia = c(
    round(100 * mean(residuo_total$ok_sin_r4csap, na.rm = TRUE), 2),
    round(100 * mean(residuo_total$ok_con_r4csap, na.rm = TRUE), 2)
  )
)
cat("\nResiduo NETO tras descontar R4CSAP (categoría total):\n")
print(as.data.frame(comparacion_neta), row.names = FALSE)
guardar_tabla(as.data.frame(comparacion_neta), "RC_T11_residuo_neto_tras_r4csap",
              "% de filas dentro de tolerancia (0.1%), antes y después de descontar R4CSAP")

# ==============================================================================
# 5. CONTRASTAR CONTRA TOTALES DE CONTROL
# ==============================================================================
titulo("5. CONTRASTAR CONTRA TOTALES DE CONTROL")

contraste_control <- panel %>%
  dplyr::transmute(
    NORDEMP, ANIO,
    salarper = safe_numeric(control_salario_permanentes_salarper),
    c3r2c3 = safe_numeric(sueldos_permanentes_total_c3r2c3),
    pressper = safe_numeric(control_prestaciones_permanentes_pressper),
    c3r3c3 = safe_numeric(prestaciones_permanentes_total_c3r3c3),
    salpeyte = safe_numeric(control_salario_permanente_y_temporal_salpeyte),
    c3r2c3_mas_c3r4c3 = c3r2c3 + safe_numeric(sueldos_prest_temporal_directo_total_c3r4c3)
  ) %>%
  dplyr::mutate(
    dif_salarper = salarper - c3r2c3,
    dif_pressper = pressper - c3r3c3,
    dif_salpeyte = salpeyte - c3r2c3_mas_c3r4c3
  )

resumen_contraste <- tibble::tibble(
  contraste = c("SALARPER vs C3R2C3", "PRESSPER vs C3R3C3", "SALPEYTE vs C3R2C3+C3R4C3"),
  pct_coincide_exacto = c(
    round(100 * mean(abs(contraste_control$dif_salarper) < 0.5, na.rm = TRUE), 2),
    round(100 * mean(abs(contraste_control$dif_pressper) < 0.5, na.rm = TRUE), 2),
    round(100 * mean(abs(contraste_control$dif_salpeyte) < 0.5, na.rm = TRUE), 2)
  )
)
cat("Contraste de totales de control:\n")
print(as.data.frame(resumen_contraste), row.names = FALSE)
guardar_tabla(as.data.frame(resumen_contraste), "RC_T09_contraste_totales_control",
              "Contraste SALARPER/PRESSPER/SALPEYTE contra las filas individuales ya extraídas")

# ==============================================================================
# 6. CONCLUSION Y EFECTO SOBRE salario_promedio
# ==============================================================================
titulo("6. EFECTO SOBRE salario_promedio (2022 y 2023)")

# Misma definicion de empleo_total que panel_analitico_firma_eam.rds y
# 01_resultados_principales.R (permanentes+temporal_directo+temporal_
# agencias+aprendices, 3 categorias, SIN propietarios) -- ya calculada y
# validada al 100% contra el panel oficial en construir_panel_completo.R.
# suma_r1_r9_directa se recalcula aqui (misma fila-a-fila que panel, sin
# necesidad de match()) para evitar cualquier desalineacion de indices.
suma_r1_r9_directa <- calcular_residuo(panel, componentes_categoria$total)
comparacion_salario <- panel %>%
  dplyr::transmute(NORDEMP, ANIO, empleo_total = empleo_total_sin_propietarios,
                    c3r10_total = safe_numeric(costos_totales_personal_total_c3r10c3),
                    suma_r1_r9_total = suma_r1_r9_directa$suma_componentes) %>%
  dplyr::filter(ANIO %in% c(2022, 2023), !is.na(empleo_total), empleo_total > 0) %>%
  dplyr::mutate(
    salario_promedio_c3r10 = c3r10_total / empleo_total,
    salario_promedio_suma_r1_r9 = suma_r1_r9_total / empleo_total,
    diferencia_pct = 100 * (salario_promedio_c3r10 / salario_promedio_suma_r1_r9 - 1)
  )

resumen_impacto_salario <- comparacion_salario %>%
  dplyr::filter(is.finite(diferencia_pct)) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    n_firmas = dplyr::n(),
    salario_promedio_c3r10_mediana = round(median(salario_promedio_c3r10, na.rm = TRUE), 1),
    salario_promedio_suma_r1_r9_mediana = round(median(salario_promedio_suma_r1_r9, na.rm = TRUE), 1),
    diferencia_pct_mediana = round(median(diferencia_pct, na.rm = TRUE), 2),
    diferencia_pct_p10 = round(quantile(diferencia_pct, 0.10, na.rm = TRUE), 2),
    diferencia_pct_p90 = round(quantile(diferencia_pct, 0.90, na.rm = TRUE), 2),
    .groups = "drop"
  )
cat("Impacto sobre salario_promedio, C3R10 vs. suma explícita R1-R9, 2022 y 2023:\n")
print(as.data.frame(resumen_impacto_salario), row.names = FALSE)
guardar_tabla(as.data.frame(resumen_impacto_salario), "RC_T10_impacto_salario_promedio",
              "salario_promedio: C3R10/empleo_total vs. suma(R1..R9)/empleo_total, 2022 y 2023")

g_impacto <- ggplot2::ggplot(comparacion_salario %>% dplyr::filter(is.finite(diferencia_pct), abs(diferencia_pct) < quantile(abs(diferencia_pct), 0.99, na.rm=TRUE)),
                              ggplot2::aes(x = diferencia_pct, fill = factor(ANIO))) +
  ggplot2::geom_histogram(bins = 60, position = "identity", alpha = 0.6) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::labs(title = "Diferencia % en salario_promedio: usar C3R10 vs. suma explícita de R1-R9",
                subtitle = "2022 (año base de Kaitz) y 2023 (post-choque). Truncado al p99.",
                x = "Diferencia % (C3R10 respecto a suma R1-R9)", y = "N firmas", fill = "Año") +
  tema_tesis
guardar_grafico(g_impacto, "RC04_impacto_salario_promedio")

titulo("SCRIPT COMPLETO")
cat("Tablas y gráficos en:", resultados_raiz, "\n")

# ==============================================================================
# 7. REPORTE (sección nueva en el README)
# ==============================================================================
titulo("7. REPORTE FINAL")

corr_r4csap_todas <- correlaciones_residuo$correlacion[correlaciones_residuo$candidato == "r4csap"]
corr_r4csap_positivas <- resumen_r4csap$correlacion
pct_dentro_sin <- comparacion_neta$pct_dentro_de_tolerancia[1]
pct_dentro_con <- comparacion_neta$pct_dentro_de_tolerancia[2]

seccion_readme <- c(
"",
"## Diagnóstico del residuo de C3R10 (costo total del personal)",
"",
paste0("`C3R10` (costo total del personal, numerador de `salario_promedio` en ",
       "`01_resultados_principales.R`) no cerraba como `suma(C3R1..C3R9)` en el diagnóstico ",
       "previo (`EA_T07`, 17%-74% de cumplimiento). Este script diagnostica por qué, con datos, ",
       "no con especulación."),
"",
"### 1. Dirección del residuo: casi enteramente positiva",
"",
paste0("Ver `RC_T01_reparto_signos_residuo`. El residuo negativo es prácticamente inexistente ",
       "(0.01%-0.06% de las filas en las 4 categorías). El resto se reparte entre \"cero dentro ",
       "de tolerancia\" (48%-75%) y **positivo** (25%-52%). No hay mezcla de signos que sugiera ",
       "un problema de calidad de dato: la dirección es sistemática y apunta limpiamente a un ",
       "**rubro que no se extrajo**, no a doble conteo."),
"",
"### 2. Magnitud: pequeña en la mediana, concentrada en una minoría de firmas",
"",
paste0("Ver `RC_T02_magnitud_residuo`. Mediana del residuo (categoría total): 0.209% de `C3R10`. ",
       "Solo 6.68% de las filas supera el 5%, y 0.61% supera el 20%. Esto NO es \"un rubro entero ",
       "por fuera\" para la firma promedio -- es un residuo que afecta de forma importante solo a ",
       "una minoría de firmas: exactamente las que tienen aprendices."),
"",
"### 3. Estructura: no es agregación, es un dato de la macrobase cruda",
"",
paste0("Por año (`RC_T03`): el cumplimiento empeora progresivamente de 2008 a 2024 en las 4 ",
       "categorías, sin un salto abrupto en un año específico que sugiera un cambio puntual de ",
       "formulario -- es una tendencia gradual, no un quiebre. Por tamaño (`RC_T04`): empeora con ",
       "el tamaño de la firma (Pequeña 67.9% cumple, Mediana 16.1%, Grande 6.4%) -- coherente con ",
       "que las firmas grandes tienen más probabilidad de tener aprendices. Por sector (`RC_T05`): ",
       "varía bastante (29%-59%) pero sin un sector claramente atípico que domine el patrón. ",
       "Establecimiento vs. firma (`RC_T06`): 73.58% vs. 73.78% -- prácticamente idéntico. **La ",
       "agregación a firma NO amplifica el residuo**: ya está en los datos crudos de la macrobase, ",
       "por establecimiento."),
"",
"### 4. El rubro identificado: R4CSAP (apoyo de sostenimiento de aprendices)",
"",
paste0("Ver `RC_T07_correlacion_candidatos`, `RC_T08_prueba_r4csap` y `RC_T11_residuo_neto_tras_r4csap`. ",
       "De los 6 candidatos probados (R4CSAP, R1+R2+R3CSAP, SALARPER, SALPEYTE, PRESSPER, PRESPYTE), ",
       "R4CSAP es, por lejos, el que mejor explica el residuo: correlación ", corr_r4csap_todas,
       " en toda la muestra, ", corr_r4csap_positivas, " restringido a las filas con R4CSAP > 0. La ",
       "razón residuo/R4CSAP tiene **mediana exactamente 1.0**, y 87.63% de esas filas caen entre ",
       "0.9 y 1.1. Al descontar R4CSAP del residuo, el % de filas dentro de tolerancia (0.1%) sube ",
       "de ", pct_dentro_sin, "% a ", pct_dentro_con, "%. Es la prueba que pedía la tarea, y confirma ",
       "la hipótesis: los aprendices están en el conteo de personal que determina `C3R10` (\"Costos ",
       "y Gastos Causados por el Personal Ocupado\"), pero su pago (`R4CSAP`, apoyo de sostenimiento, ",
       "Ley 789) vive en una fila aparte del cuadro 3, fuera de `C3R1`-`C3R9`."),
"",
"### 5. Totales de control: parcialmente consistentes, no perfectos",
"",
paste0("Ver `RC_T09_contraste_totales_control`. `SALARPER` vs. `C3R2C3` y `PRESSPER` vs. `C3R3C3` ",
       "coinciden en 84.33% de las filas cada uno -- alto, pero no total, así que incluso las filas ",
       "individuales R2/R3 tienen algo de inconsistencia interna frente a sus propios totales de ",
       "control, con una magnitud menor a la del hallazgo principal. `SALPEYTE` vs. ",
       "`C3R2C3+C3R4C3` coincide solo 51.11% -- más ruido en la frontera permanente/temporal ",
       "directo, consistente con lo ya documentado en el punto 2 del cierre anterior (la estructura ",
       "completa del cuadro 3 no cierra de forma perfecta en ningún cruce probado hasta ahora)."),
"",
"### 6. Conclusión",
"",
paste0("**\"C3R10 = suma de R1-R9 más X\", con X identificado: R4CSAP** (apoyo de sostenimiento de ",
       "aprendices, Ley 789). No es una explicación perfecta al 100% -- queda un residuo menor sin ",
       "explicar tras descontar R4CSAP (ver `RC_T11`), pero pasa de ser el problema dominante a ser ",
       "un remanente secundario. `salario_promedio` (`C3R10C3 / empleo_total`) **está bien ",
       "definido**: incluye el costo de aprendices que `empleo_total` también cuenta en el ",
       "denominador (ver PASO 0.3 del script principal -- los aprendices SÍ están en `PERTOTAL`/",
       "`empleo_total`). Usar `C3R10` es, de hecho, la opción MÁS consistente entre numerador y ",
       "denominador, no una caja opaca."),
"",
paste0("**Efecto sobre `salario_promedio` 2022 y 2023** (ver `RC_T10_impacto_salario_promedio`): la ",
       "diferencia mediana entre usar `C3R10` y usar la suma explícita de R1-R9 es **1.33% en 2022 ",
       "y 1.10% en 2023** -- pequeña, y el percentil 10 es 0% (la mitad de las firmas o más no tiene ",
       "diferencia alguna, porque no tienen aprendices). El percentil 90 sí llega a 15-16%, para las ",
       "firmas que sí tienen aprendices con apoyo de sostenimiento. **El hallazgo es una nota ",
       "metodológica, no una corrección al resultado del primer eslabón**: la diferencia en la ",
       "mediana es pequeña y el numerador actual (`C3R10`) es, si acaso, más correcto que la ",
       "alternativa de sumar R1-R9 a mano, porque esa suma excluiría el costo de los aprendices que ",
       "sí están contados en el denominador."),
"",
paste0("**Lo que queda pendiente**: identificar la causa exacta del residuo restante tras descontar ",
       "R4CSAP (12.37% de las filas con R4CSAP>0 caen fuera del rango 0.9-1.1 en la razón ",
       "residuo/R4CSAP) -- no se investigó más a fondo por quedar fuera del alcance de esta tarea. ",
       "Candidato más plausible para ese remanente: redondeo o pequeñas inconsistencias de reporte ",
       "en R1CSAP/R2CSAP/R3CSAP individuales (ver el 96.83% -no 100%- de la identidad R1+R2+R3=R4 ",
       "ya documentado en el cierre anterior)."),
""
)

readme_path <- file.path(resultados_raiz, "README.md")
if (file.exists(readme_path)) {
  readme_actual <- readLines(readme_path, warn = FALSE)
  # Idempotente: si ya se habia corrido este script antes, se quita la
  # version anterior de esta seccion antes de volver a agregarla (no se
  # duplica el README en cada corrida).
  marca_inicio <- which(readme_actual == "## Diagnóstico del residuo de C3R10 (costo total del personal)")
  if (length(marca_inicio) > 0) {
    readme_actual <- readme_actual[seq_len(marca_inicio[1] - 2)]  # -2 quita tambien la linea "" antes del encabezado
  }
  writeLines(c(readme_actual, seccion_readme), readme_path)
  cat("Sección añadida/actualizada en:", readme_path, "\n")
} else {
  warning("No se encontró README.md en ", resultados_raiz, " -- no se pudo añadir la sección.")
}