# graficar_participacion_permanente_por_quintil_bite_vs_exposure.R
#
# Contrasta la hipotesis planteada al interpretar la matriz 2x2
# (comparar_matriz_funcional_exposure_bite.R): que Bite2022_obreros
# rechaza tendencias paralelas con mas fuerza que Exposure2022_obreros
# en participacion_permanente porque la desviacion pre-choque 2015-2019
# es MONOTONICA por quintil de Bite (donde el test de tendencia lineal
# tiene mas potencia) y NO lo es por quintil de Exposure (donde un F
# sobre dummies de anio es la forma apropiada).
#
# Grafica el promedio de participacion_permanente 2015-2019 por
# quintil, una vez para Bite2022_obreros y una vez para
# Exposure2022_obreros, en el mismo panel (2015-2019, sin 2020) para
# poder comparar visualmente si la divergencia entre quintiles es
# monotonica en un caso y no en el otro.
#
# DERIVADO del patron de graficos de series por quintil de
# diagnostico_preliminar_tendencias_2015_2019.R (group_by(ANIO, quintil)
# + summarise(mean) + ggplot2::geom_line/geom_point + theme_minimal),
# aplicado aqui a participacion_permanente con AMBAS medidas de
# exposicion en paneles separados (facet) para comparacion directa.
#
# IMPORTANTE: esto es evidencia descriptiva (medias por celda), NO un
# test estadistico -- se reporta como consistente o inconsistente con
# la hipotesis, no como confirmacion. La matriz 2x2 (F-tests) sigue
# siendo la evidencia inferencial; este grafico es diagnostico visual
# complementario.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - participacion_permanente_por_quintil_bite_vs_exposure.png
# - participacion_permanente_por_quintil_bite_vs_exposure.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "ggplot2", "scales")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_path <- file.path(data_dir, "panel_analitico_firma_eam.rds")
if (!file.exists(panel_path)) stop("Falta panel_analitico_firma_eam.rds. Corre 03_construir_panel.R primero.")
panel_analitico <- readr::read_rds(panel_path)

panel_pre_2019 <- panel_analitico %>%
  dplyr::filter(ANIO %in% 2015:2019)

serie_por_quintil <- function(data, var_quintil, medida) {
  data %>%
    dplyr::filter(!is.na(.data[[var_quintil]])) %>%
    dplyr::group_by(ANIO, quintil = .data[[var_quintil]]) %>%
    dplyr::summarise(
      participacion_permanente_media = mean(participacion_permanente, na.rm = TRUE),
      n_firmas = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(medida = medida)
}

serie_exposure <- serie_por_quintil(panel_pre_2019, "quintil_exposure2022_obreros", "Exposure2022_obreros")
serie_bite <- serie_por_quintil(panel_pre_2019, "quintil_bite2022_obreros", "Bite2022_obreros")
serie_combinada <- dplyr::bind_rows(serie_exposure, serie_bite)

readr::write_csv(serie_combinada, file.path(out_dir, "participacion_permanente_por_quintil_bite_vs_exposure.csv"))

p <- ggplot2::ggplot(serie_combinada, ggplot2::aes(x = ANIO, y = participacion_permanente_media, color = quintil)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~medida, nrow = 1) +
  ggplot2::scale_x_continuous(breaks = 2015:2019) +
  ggplot2::scale_y_continuous(labels = scales::label_number(decimal.mark = ",", suffix = "%")) +
  ggplot2::labs(
    title = "Participacion de permanentes 2015-2019, por quintil de exposicion",
    subtitle = "Evidencia descriptiva (medias por celda), no un test estadistico -- ver matriz 2x2 para inferencia",
    x = "Anio", y = "Participacion de permanentes (%), promedio", color = "Quintil"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(out_dir, "participacion_permanente_por_quintil_bite_vs_exposure.png"),
  plot = p, width = 12, height = 6, dpi = 180
)

# ------------------------------------------------------------------
# Chequeo de monotonicidad: en 2019 (ultimo anio pre-choque), ¿el orden
# de los quintiles Q1..Q5 es monotonico (todo creciente o todo
# decreciente) en cada medida?
# ------------------------------------------------------------------

es_monotonica <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA)
  diffs <- diff(x)
  all(diffs >= 0) || all(diffs <= 0)
}

chequeo_monotonicidad <- serie_combinada %>%
  dplyr::filter(ANIO == 2019) %>%
  dplyr::arrange(medida, quintil) %>%
  dplyr::group_by(medida) %>%
  dplyr::summarise(
    orden_participacion_2019 = paste(round(participacion_permanente_media, 2), collapse = " -> "),
    monotonica = es_monotonica(participacion_permanente_media),
    .groups = "drop"
  )

script_header("graficar_participacion_permanente_por_quintil_bite_vs_exposure.R -- evidencia descriptiva, no test")
message("")
print(serie_combinada %>% dplyr::arrange(medida, quintil, ANIO), n = Inf, width = Inf)
message("")
message("=== Chequeo de monotonicidad en 2019 (Q1 -> Q5) ===")
print(chequeo_monotonicidad, n = Inf, width = Inf)
message("")
message("Grafico: ", file.path(out_dir, "participacion_permanente_por_quintil_bite_vs_exposure.png"))
message("Tabla: ", file.path(out_dir, "participacion_permanente_por_quintil_bite_vs_exposure.csv"))
