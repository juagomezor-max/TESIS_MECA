# Paso 5: diagnostica si la correlacion debil entre Bite2022_obreros y
# Exposure2022_obreros (Pearson=0.124, Spearman=0.159, Paso 3) es un hallazgo
# real o un artefacto de outliers/heterogeneidad no tratada. NO cambia la
# formula ni el anio base de Bite2022_obreros (eso quedo fijo en el Paso 2);
# esto es solo diagnostico.
#
# 1) Winsorizacion: salario_promedio_obrero_f (fuente de Bite2022_obreros) NO
#    paso por el mismo criterio de winsorizacion que usa
#    descriptivo_exposicion_eam.R Seccion 4 para Exposure2022 (winsorize,
#    probs 1%-99%) -- confirmado leyendo construir_salarios_promedio_categoria_eam.R,
#    que usa safe_divide() sin winsorize(). Exposure2022_obreros SI fue
#    winsorizada (construir_exposicion_obreros_eam.R, Paso 5 de la rama
#    anterior). Aqui se aplica el mismo criterio a salario_promedio_obrero_f,
#    se recalcula Bite2022_obreros_winsorizado, y se reportan AMBAS
#    correlaciones (con y sin winsorizar), sin reemplazar la version original.
# 2) Scatter Bite (Y) vs Exposure (X), con linea de tendencia lineal y curva
#    suavizada (loess), para detectar no linealidad que el coeficiente lineal
#    no capture.
# 3) Correlacion Bite-Exposure dentro de cada uno de los sectores (CIIU4) con
#    mas observaciones, para detectar si heterogeneidad sectorial esconde una
#    relacion mas fuerte a nivel firma.
#
# Salidas en 4. RESULTADOS/descriptivos_exposicion/ (versionadas).

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "ggplot2", "scales", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
plot_dir <- paths$resultados_exposicion

ANIO_BASE_EXPOSICION <- 2022
SM_2023_MENSUAL_COP <- 1160000
SM_2023_ANUAL_MILES <- SM_2023_MENSUAL_COP * 12 / 1000

leer_o_fallar <- function(path, mensaje) {
  if (!file.exists(path)) stop(mensaje)
  readr::read_rds(path)
}

bite_obreros <- leer_o_fallar(file.path(data_dir, "bite_obreros_eam.rds"), "Falta bite_obreros_eam.rds (Paso 2).")
salarios <- leer_o_fallar(file.path(data_dir, "salarios_promedio_categoria_eam.rds"), "Falta salarios_promedio_categoria_eam.rds.")

# Misma funcion y mismos parametros que descriptivo_exposicion_eam.R Seccion 4.
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limits <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limits[[1]]), limits[[2]])
}

safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

# ------------------------------------------------------------------
# 1) Bite winsorizado vs original
# ------------------------------------------------------------------

salario_2022 <- salarios %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, salario_promedio_obrero) %>%
  dplyr::mutate(
    salario_promedio_obrero_winsorizado = winsorize(salario_promedio_obrero),
    Bite2022_obreros_winsorizado = safe_divide(SM_2023_ANUAL_MILES, salario_promedio_obrero_winsorizado)
  )

base_comparacion <- bite_obreros %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros) %>%
  dplyr::left_join(salario_2022 %>% dplyr::select(NORDEMP, Bite2022_obreros_winsorizado), by = "NORDEMP") %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

cor_sin_winsorizar <- base_comparacion %>%
  dplyr::filter(!is.na(Bite2022_obreros)) %>%
  dplyr::summarise(
    version = "Bite2022_obreros (original, sin winsorizar salario)",
    n = dplyr::n(),
    pearson = round(cor(Bite2022_obreros, Exposure2022_obreros, method = "pearson"), 3),
    spearman = round(cor(Bite2022_obreros, Exposure2022_obreros, method = "spearman"), 3)
  )

cor_con_winsorizar <- base_comparacion %>%
  dplyr::filter(!is.na(Bite2022_obreros_winsorizado)) %>%
  dplyr::summarise(
    version = "Bite2022_obreros_winsorizado (salario winsorizado 1%-99%)",
    n = dplyr::n(),
    pearson = round(cor(Bite2022_obreros_winsorizado, Exposure2022_obreros, method = "pearson"), 3),
    spearman = round(cor(Bite2022_obreros_winsorizado, Exposure2022_obreros, method = "spearman"), 3)
  )

tabla_winsorizacion <- dplyr::bind_rows(cor_sin_winsorizar, cor_con_winsorizar)
readr::write_csv(tabla_winsorizacion, file.path(plot_dir, "tabla_correlacion_bite_exposure_winsorizado.csv"))

# ------------------------------------------------------------------
# 2) Scatter plot Bite (Y, version original) vs Exposure (X), con linea
#    de tendencia lineal y curva loess.
# ------------------------------------------------------------------

scatter_data <- base_comparacion %>% dplyr::filter(!is.na(Bite2022_obreros))

scatter_bite_exposure <- ggplot2::ggplot(scatter_data, ggplot2::aes(x = Exposure2022_obreros, y = Bite2022_obreros)) +
  ggplot2::geom_point(alpha = 0.25, size = 1, color = "#377EB8") +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#D95F02", linewidth = 0.8, linetype = "dashed") +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    title = "Bite2022_obreros vs Exposure2022_obreros",
    subtitle = paste0(
      "n = ", nrow(scatter_data), " | linea negra = tendencia lineal, linea punteada naranja = loess | ",
      "Pearson=", cor_sin_winsorizar$pearson, ", Spearman=", cor_sin_winsorizar$spearman
    ),
    x = "Exposure2022_obreros (participacion de obreros en el empleo)",
    y = "Bite2022_obreros (indice de Kaitz)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(plot_dir, "scatter_bite_vs_exposure.png"),
  plot = scatter_bite_exposure, width = 9, height = 7, dpi = 180
)

# ------------------------------------------------------------------
# 3) Correlacion Bite-Exposure dentro de cada sector (CIIU4), los 6
#    sectores con mas observaciones.
# ------------------------------------------------------------------

macro_path <- paths$macro_base_eam
macro_atributos <- readr::read_rds(macro_path)
names(macro_atributos) <- toupper(names(macro_atributos))
macro_atributos <- macro_atributos %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = suppressWarnings(as.integer(ANIO))) %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, .keep_all = TRUE) %>%
  dplyr::select(NORDEMP, CIIU4)

datos_con_sector <- scatter_data %>%
  dplyr::left_join(macro_atributos, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4))

top_sectores <- datos_con_sector %>%
  dplyr::count(CIIU4, sort = TRUE) %>%
  dplyr::slice_head(n = 6) %>%
  dplyr::pull(CIIU4)

correlacion_por_sector <- purrr::map_dfr(top_sectores, function(sec) {
  d <- datos_con_sector %>% dplyr::filter(CIIU4 == sec)
  if (nrow(d) < 10 || dplyr::n_distinct(d$Exposure2022_obreros) < 3) {
    return(tibble::tibble(CIIU4 = sec, n = nrow(d), pearson = NA_real_, spearman = NA_real_))
  }
  tibble::tibble(
    CIIU4 = sec,
    n = nrow(d),
    pearson = round(cor(d$Bite2022_obreros, d$Exposure2022_obreros, method = "pearson"), 3),
    spearman = round(cor(d$Bite2022_obreros, d$Exposure2022_obreros, method = "spearman"), 3)
  )
}) %>%
  dplyr::arrange(dplyr::desc(n))

readr::write_csv(correlacion_por_sector, file.path(plot_dir, "correlacion_bite_exposure_por_sector.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Paso 5: robustez de la correlacion Bite-Exposure")

message("1) Correlacion con y sin winsorizar salario_promedio_obrero_f:")
print(tabla_winsorizacion, width = Inf)

message("")
message("2) Scatter guardado en: ", file.path(plot_dir, "scatter_bite_vs_exposure.png"))

message("")
message("3) Correlacion Bite-Exposure dentro de cada sector (top 6 por n):")
print(correlacion_por_sector, n = Inf, width = Inf)
message("")
message("Correlacion agregada de referencia (Paso 3): Pearson=0.124, Spearman=0.159")

message("")
message("Tablas exportadas en: ", plot_dir)
