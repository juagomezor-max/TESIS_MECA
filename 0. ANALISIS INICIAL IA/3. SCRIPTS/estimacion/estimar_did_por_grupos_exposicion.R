# estimar_did_por_grupos_exposicion.R
#
# Parte C: efectos por NIVEL de exposicion (no la especificacion
# continua lineal ya reportada) -- para las 4 variables de resultado y
# las 2 medidas de exposicion, bajo DOS particiones (terciles y
# quintiles) para comparar sensibilidad a la particion. Objetivo:
# detectar si la relacion exposicion-efecto es monotonica (como
# asumiria la especificacion lineal continua) o no -- algo que esa
# especificacion no puede ver si la relacion no es lineal.
#
# DERIVADO de (no reimplementado de memoria):
# - Exposure2022_obreros, Bite2022_obreros y sus QUINTILES ya
#   construidos (quintil_exposure2022_obreros, quintil_bite2022_obreros):
#   pipeline/02_construir_exposicion.R, funcion make_quintiles()
#   (ntile(x,5), labels "Q1 - Muy baja".."Q5 - Muy alta"). Se REUSAN
#   tal cual, no se recalculan.
# - TERCILES (nuevos, mismo patron que make_quintiles pero ntile(x,3),
#   labels "T1 - Baja"/"T2 - Media"/"T3 - Alta") -- misma logica,
#   grado distinto.
# - post_2023, panel formal, los 3 controles, cluster=~NORDEMP: idem
#   estimacion/estimar_did_principal_empleo.R.
# - Forma funcional i(grupo, post_2023, ref=<grupo mas bajo>): mismo
#   patron fixest::i() ya usado en todo el proyecto para efectos por
#   grupo/quintil (ej. 04_validaciones.R).
#
# ESPECIFICACION: outcome ~ i(grupo_exposicion, post_2023, ref=<grupo
# de menor exposicion>) | NORDEST + ANIO_F + CIIU4^ANIO_F +
# tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F, cluster=~NORDEMP -- MISMOS
# controles y cluster que la especificacion principal continua.
#
# Salidas (versionadas):
# - 4. RESULTADOS/Validaciones/grupos_exposicion_n_por_grupo.csv (Paso b, ANTES de estimar)
# - 4. RESULTADOS/Estimacion_DiD/histograma_exposure2022_obreros.png
# - 4. RESULTADOS/Estimacion_DiD/histograma_bite2022_obreros.png
# - 4. RESULTADOS/Estimacion_DiD/coeficientes_por_grupo_exposicion.csv (todas las celdas, tidy)
# - 4. RESULTADOS/Estimacion_DiD/efecto_por_grupo_<medida>_terciles.png
# - 4. RESULTADOS/Estimacion_DiD/efecto_por_grupo_<medida>_quintiles.png
# - 4. RESULTADOS/Validaciones/chequeo_monotonicidad_por_grupo.csv (Paso e)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr", "ggplot2")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir_est <- file.path(paths$resultados, "Estimacion_DiD")
out_dir_val <- paths$resultados_validaciones

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(panel_formal_path)) stop("Falta panel_establecimiento_formal.rds.")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_firma_eam.rds.")

outcomes_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total", "Empleo total",
  "empleo_permanente", "Empleo permanente",
  "empleo_temporal", "Empleo temporal",
  "participacion_permanente", "Participacion permanente (%)"
)

# ------------------------------------------------------------------
# 1) Terciles (nuevos, mismo patron que make_quintiles).
# ------------------------------------------------------------------

make_terciles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))
  if (length(valid) < 3 || dplyr::n_distinct(x[valid]) < 3) return(out)
  terc <- dplyr::ntile(x[valid], 3)
  labels <- c("T1 - Baja", "T2 - Media", "T3 - Alta")
  out[valid] <- labels[terc]
  factor(out, levels = labels, ordered = TRUE)
}

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::mutate(
    tercil_exposure2022_obreros = make_terciles(Exposure2022_obreros),
    tercil_bite2022_obreros = make_terciles(Bite2022_obreros)
  )

# ------------------------------------------------------------------
# 2a) Histogramas, marcando masa en los puntos limite documentados.
# ------------------------------------------------------------------

n_exp_0 <- sum(exposicion_firma$Exposure2022_obreros == 0, na.rm = TRUE)
n_exp_1 <- sum(exposicion_firma$Exposure2022_obreros == 1, na.rm = TRUE)
n_exp_total <- sum(!is.na(exposicion_firma$Exposure2022_obreros))

p_hist_exp <- ggplot2::ggplot(exposicion_firma %>% dplyr::filter(!is.na(Exposure2022_obreros)), ggplot2::aes(x = Exposure2022_obreros)) +
  ggplot2::geom_histogram(bins = 50, fill = "#1F77B4", color = "white") +
  ggplot2::geom_vline(xintercept = c(0, 1), linetype = "dashed", color = "firebrick") +
  ggplot2::labs(
    title = "Distribucion de Exposure2022_obreros (firma)",
    subtitle = paste0("n=", n_exp_total, " | Masa en 0: ", n_exp_0, " firmas (", round(100*n_exp_0/n_exp_total,1),
                       "%) | Masa en 1 (maximo, winsorizado): ", n_exp_1, " firmas (", round(100*n_exp_1/n_exp_total,1), "%)"),
    x = "Exposure2022_obreros", y = "N firmas"
  ) +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(out_dir_est, "histograma_exposure2022_obreros.png"), p_hist_exp, width = 9, height = 6, dpi = 180)

n_bite_1 <- sum(exposicion_firma$Bite2022_obreros == 1, na.rm = TRUE)
n_bite_total <- sum(!is.na(exposicion_firma$Bite2022_obreros))
n_bite_gt1 <- sum(exposicion_firma$Bite2022_obreros > 1, na.rm = TRUE)

p_hist_bite <- ggplot2::ggplot(exposicion_firma %>% dplyr::filter(!is.na(Bite2022_obreros)), ggplot2::aes(x = Bite2022_obreros)) +
  ggplot2::geom_histogram(bins = 50, fill = "#D62728", color = "white") +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "Distribucion de Bite2022_obreros (firma)",
    subtitle = paste0("n=", n_bite_total, " | Bite2022_obreros NO tiene masa natural en 0 (indice de Kaitz, minimo observado=0.092) | ",
                       "Bite=1 (SM=salario promedio): ", n_bite_1, " firmas | Bite>1 (SM>salario promedio): ", n_bite_gt1,
                       " firmas (", round(100*n_bite_gt1/n_bite_total,1), "%) | cola larga hasta max=6"),
    x = "Bite2022_obreros", y = "N firmas"
  ) +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(out_dir_est, "histograma_bite2022_obreros.png"), p_hist_bite, width = 9, height = 6, dpi = 180)

# ------------------------------------------------------------------
# 2b) N por grupo, AMBAS particiones, ANTES de estimar nada.
# ------------------------------------------------------------------

n_por_grupo <- dplyr::bind_rows(
  exposicion_firma %>% dplyr::filter(!is.na(tercil_exposure2022_obreros)) %>% dplyr::count(grupo = as.character(tercil_exposure2022_obreros)) %>% dplyr::mutate(medida = "Exposure2022_obreros", particion = "Terciles"),
  exposicion_firma %>% dplyr::filter(!is.na(quintil_exposure2022_obreros)) %>% dplyr::count(grupo = as.character(quintil_exposure2022_obreros)) %>% dplyr::mutate(medida = "Exposure2022_obreros", particion = "Quintiles"),
  exposicion_firma %>% dplyr::filter(!is.na(tercil_bite2022_obreros)) %>% dplyr::count(grupo = as.character(tercil_bite2022_obreros)) %>% dplyr::mutate(medida = "Bite2022_obreros", particion = "Terciles"),
  exposicion_firma %>% dplyr::filter(!is.na(quintil_bite2022_obreros)) %>% dplyr::count(grupo = as.character(quintil_bite2022_obreros)) %>% dplyr::mutate(medida = "Bite2022_obreros", particion = "Quintiles")
) %>%
  dplyr::mutate(baja_potencia_menor_30 = n < 30) %>%
  dplyr::select(medida, particion, grupo, n, baja_potencia_menor_30)

readr::write_csv(n_por_grupo, file.path(out_dir_val, "grupos_exposicion_n_por_grupo.csv"))

script_header("estimar_did_por_grupos_exposicion.R -- Paso b: N por grupo (ANTES de estimar)")
message("")
print(n_por_grupo, n = Inf, width = Inf)
if (any(n_por_grupo$baja_potencia_menor_30)) {
  message("\n>>> ALERTA: hay grupos con menos de 30 firmas -- ver columna baja_potencia_menor_30. <<<")
} else {
  message("\n>>> Ningun grupo por debajo de 30 firmas en ninguna medida/particion. <<<")
}

# ------------------------------------------------------------------
# 3) Panel formal + grupos, 2 medidas.
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path)

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

# ------------------------------------------------------------------
# 4) Estimacion por grupo: i(grupo, post_2023, ref=<grupo mas bajo>).
# ------------------------------------------------------------------

correr_por_grupo <- function(data, var_grupo, ref_label, var_y) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(", var_grupo, ", post_2023, ref = '", ref_label, "') | ", FE_CON
  ))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  extraer_coeficientes_tidy_fixest(modelo, var_y, paste0(ref_label, " (ref.)")) %>%
    dplyr::mutate(grupo = sub(paste0("^", var_grupo, "::"), "", term), grupo = sub(":post_2023$", "", grupo), .after = term)
}

combinaciones <- tibble::tribble(
  ~medida, ~particion, ~data_ref, ~var_grupo, ~ref_label,
  "Exposure2022_obreros", "Terciles", "exposure", "tercil_exposure2022_obreros", "T1 - Baja",
  "Exposure2022_obreros", "Quintiles", "exposure", "quintil_exposure2022_obreros", "Q1 - Muy baja",
  "Bite2022_obreros", "Terciles", "bite", "tercil_bite2022_obreros", "T1 - Baja",
  "Bite2022_obreros", "Quintiles", "bite", "quintil_bite2022_obreros", "Q1 - Muy baja"
)

resultados <- purrr::pmap_dfr(combinaciones, function(medida, particion, data_ref, var_grupo, ref_label) {
  data <- if (data_ref == "exposure") datos_exposure else datos_bite
  purrr::map_dfr(outcomes_info$var, function(var_y) {
    correr_por_grupo(data, var_grupo, ref_label, var_y) %>%
      dplyr::mutate(medida = medida, particion = particion, .before = 1)
  })
})

readr::write_csv(resultados, file.path(out_dir_est, "coeficientes_por_grupo_exposicion.csv"))

# ------------------------------------------------------------------
# 5) Coefplots: 1 figura por (medida, particion), facet por outcome.
# ------------------------------------------------------------------

graficar_por_grupo <- function(medida_sel, particion_sel) {
  datos_fig <- resultados %>%
    dplyr::filter(medida == medida_sel, particion == particion_sel) %>%
    dplyr::left_join(outcomes_info, by = c("variable" = "var"))

  orden_grupos <- if (particion_sel == "Terciles") c("T1 - Baja", "T2 - Media", "T3 - Alta") else c("Q1 - Muy baja", "Q2 - Baja", "Q3 - Media", "Q4 - Alta", "Q5 - Muy alta")
  datos_fig <- datos_fig %>% dplyr::mutate(grupo = factor(grupo, levels = orden_grupos))

  ggplot2::ggplot(datos_fig, ggplot2::aes(x = grupo, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_point(size = 2.2, color = "#1F77B4") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.15, color = "#1F77B4") +
    ggplot2::facet_wrap(~label, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Efecto DiD por grupo de exposicion (", particion_sel, ") -- ", medida_sel),
      subtitle = paste0("Post2023 x grupo, ref.=grupo de MENOR exposicion | CON controles (sector*anio+tamano*anio+depto*anio) | cluster=~NORDEMP | IC95%"),
      x = "Grupo de exposicion (2022)", y = "Coeficiente (ref. grupo mas bajo)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}

for (i in seq_len(nrow(combinaciones))) {
  medida_sel <- combinaciones$medida[i]
  particion_sel <- combinaciones$particion[i]
  sufijo_medida <- ifelse(medida_sel == "Exposure2022_obreros", "exposure", "bite")
  sufijo_particion <- ifelse(particion_sel == "Terciles", "terciles", "quintiles")
  p <- graficar_por_grupo(medida_sel, particion_sel)
  ggplot2::ggsave(file.path(out_dir_est, paste0("efecto_por_grupo_", sufijo_medida, "_", sufijo_particion, ".png")), p, width = 11, height = 8, dpi = 180)
}

# ------------------------------------------------------------------
# 6) Chequeo de monotonicidad: para cada (medida, particion, outcome),
#    ¿los coeficientes estan en orden monotonico (todo creciente o todo
#    decreciente) segun el orden de los grupos?
# ------------------------------------------------------------------

es_monotonica <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA)
  d <- diff(x)
  all(d >= 0) || all(d <= 0)
}

chequeo_monotonicidad <- resultados %>%
  dplyr::group_by(medida, particion, variable) %>%
  dplyr::summarise(
    secuencia_estimate = paste(round(estimate, 3), collapse = " -> "),
    monotonica = es_monotonica(estimate),
    .groups = "drop"
  )

readr::write_csv(chequeo_monotonicidad, file.path(out_dir_val, "chequeo_monotonicidad_por_grupo.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

message("")
message("=== Coeficientes por grupo (todas las celdas) ===")
print(resultados %>% dplyr::select(medida, particion, variable, grupo, estimate, std.error, p.value), n = Inf, width = Inf)
message("")
message("=== Chequeo de monotonicidad ===")
print(chequeo_monotonicidad, n = Inf, width = Inf)
message("")
message("Salidas exportadas en: ", out_dir_est, " y ", out_dir_val)
