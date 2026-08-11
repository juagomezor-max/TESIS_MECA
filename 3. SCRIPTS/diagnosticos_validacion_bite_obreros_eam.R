# Diagnosticos de validacion para Bite2022_obreros (Paso 3):
# - Percentiles y histograma.
# - Correlacion (Pearson y Spearman) con Exposure2022_obreros.
# - Relacion con sector (CIIU4), tamano de empresa y DPTO (mismo formato
#   R2/Spearman usado para Exposure2022_obreros, para poder comparar).
# - Fraccion de firmas con Bite2022_obreros > 1 vs < 1.
#
# No modifica ningun script previo: solo lee salidas ya generadas.
# Salidas en 4. RESULTADOS/descriptivos_exposicion/ (versionadas).

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "ggplot2", "scales")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
plot_dir <- paths$resultados_exposicion

ANIO_BASE_EXPOSICION <- 2022

leer_o_fallar <- function(path, mensaje) {
  if (!file.exists(path)) stop(mensaje)
  readr::read_rds(path)
}

bite_obreros <- leer_o_fallar(
  file.path(data_dir, "bite_obreros_eam.rds"),
  "Falta bite_obreros_eam.rds. Corre construir_bite_obreros_eam.R (Paso 2) primero."
)
base_original <- leer_o_fallar(
  file.path(data_dir, "base_reducida_exposicion_eam.rds"),
  "Falta base_reducida_exposicion_eam.rds. Corre el flujo original primero."
)

save_plot <- function(plot_obj, filename, width = 9, height = 6) {
  ggplot2::ggsave(
    filename = file.path(plot_dir, filename),
    plot = plot_obj, width = width, height = height, dpi = 180
  )
}

bite_baseline <- bite_obreros %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros) %>%
  dplyr::filter(!is.na(Bite2022_obreros))

# ------------------------------------------------------------------
# 1) Percentiles y histograma
# ------------------------------------------------------------------

percentiles_bite <- tibble::tibble(
  percentil = c("p1", "p10", "p25", "p50", "p75", "p90", "p99"),
  Bite2022_obreros = quantile(bite_baseline$Bite2022_obreros, c(.01, .10, .25, .50, .75, .90, .99), na.rm = TRUE)
)

readr::write_csv(percentiles_bite, file.path(plot_dir, "tabla_percentiles_bite_obreros.csv"))

hist_bite <- ggplot2::ggplot(bite_baseline, ggplot2::aes(x = Bite2022_obreros)) +
  ggplot2::geom_histogram(bins = 40, fill = "#D95F02", color = "white", alpha = 0.9) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    title = "Distribucion de Bite2022_obreros",
    subtitle = paste0(
      "Indice de Kaitz: SM_2023 anualizado / salario promedio obrero ", ANIO_BASE_EXPOSICION,
      " | n = ", nrow(bite_baseline), " | linea vertical en Bite = 1"
    ),
    x = "Bite2022_obreros", y = "Frecuencia"
  ) +
  ggplot2::theme_minimal(base_size = 12)

save_plot(hist_bite, "histograma_bite_obreros.png")

# ------------------------------------------------------------------
# 2) Correlacion con Exposure2022_obreros (Pearson y Spearman, ambas
#    reportadas explicitamente, sin asumir que sean iguales)
# ------------------------------------------------------------------

comparables <- bite_baseline %>% dplyr::filter(!is.na(Exposure2022_obreros), !is.na(Bite2022_obreros))
cor_pearson <- round(cor(comparables$Bite2022_obreros, comparables$Exposure2022_obreros, method = "pearson"), 3)
cor_spearman <- round(cor(comparables$Bite2022_obreros, comparables$Exposure2022_obreros, method = "spearman"), 3)

tabla_correlacion <- tibble::tibble(
  n = nrow(comparables),
  correlacion_pearson = cor_pearson,
  correlacion_spearman = cor_spearman
)

readr::write_csv(tabla_correlacion, file.path(plot_dir, "tabla_correlacion_bite_exposure_obreros.csv"))

# ------------------------------------------------------------------
# 3) Relacion con sector (CIIU4), tamano de empresa y DPTO
# ------------------------------------------------------------------

macro_path <- paths$macro_base_eam
macro_atributos <- readr::read_rds(macro_path)
names(macro_atributos) <- toupper(names(macro_atributos))
macro_atributos <- macro_atributos %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = suppressWarnings(as.integer(ANIO))) %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, .keep_all = TRUE) %>%
  dplyr::select(NORDEMP, CIIU4, DPTO) %>%
  dplyr::rename(SECTOR_CIIU = CIIU4)

tamano_original <- base_original %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, tamano_empresa)

bite_con_atributos <- bite_baseline %>%
  dplyr::left_join(macro_atributos, by = "NORDEMP") %>%
  dplyr::left_join(tamano_original, by = "NORDEMP")

r2_anova <- function(data, var_dependiente, var_categorica) {
  formula_txt <- paste0(var_dependiente, " ~ factor(", var_categorica, ")")
  datos_validos <- data %>% dplyr::filter(!is.na(.data[[var_categorica]]), !is.na(.data[[var_dependiente]]))
  if (dplyr::n_distinct(datos_validos[[var_categorica]]) < 2) return(NA_real_)
  modelo <- stats::aov(stats::as.formula(formula_txt), data = datos_validos)
  resumen <- summary(modelo)[[1]]
  round(resumen[1, "Sum Sq"] / sum(resumen[, "Sum Sq"]), 4)
}

r2_sector <- r2_anova(bite_con_atributos, "Bite2022_obreros", "SECTOR_CIIU")
r2_dpto <- r2_anova(bite_con_atributos, "Bite2022_obreros", "DPTO")
r2_tamano <- r2_anova(bite_con_atributos, "Bite2022_obreros", "tamano_empresa")

tamano_ordenado <- bite_con_atributos %>%
  dplyr::filter(!is.na(tamano_empresa)) %>%
  dplyr::mutate(tamano_num = as.integer(factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"), ordered = TRUE)))

spearman_tamano <- if (nrow(tamano_ordenado) >= 3) {
  round(cor(tamano_ordenado$tamano_num, tamano_ordenado$Bite2022_obreros, method = "spearman", use = "complete.obs"), 3)
} else {
  NA_real_
}

tabla_relacion_atributos <- tibble::tibble(
  atributo = c("Sector (CIIU4)", "DPTO (region)", "Tamano de empresa"),
  tipo_medida = c("R2 (ANOVA, % varianza explicada)", "R2 (ANOVA, % varianza explicada)", "R2 (ANOVA) y Spearman (ordinal)"),
  r2 = c(r2_sector, r2_dpto, r2_tamano),
  spearman = c(NA_real_, NA_real_, spearman_tamano)
)

readr::write_csv(tabla_relacion_atributos, file.path(plot_dir, "tabla_relacion_bite_atributos.csv"))

# ------------------------------------------------------------------
# 4) Fraccion de firmas con Bite > 1 vs < 1
# ------------------------------------------------------------------

tabla_bite_mayor_menor_1 <- bite_baseline %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_bite_mayor_1 = sum(Bite2022_obreros > 1),
    n_bite_menor_1 = sum(Bite2022_obreros < 1),
    n_bite_igual_1 = sum(Bite2022_obreros == 1),
    pct_bite_mayor_1 = round(100 * mean(Bite2022_obreros > 1), 2),
    pct_bite_menor_1 = round(100 * mean(Bite2022_obreros < 1), 2)
  )

readr::write_csv(tabla_bite_mayor_menor_1, file.path(plot_dir, "tabla_bite_mayor_menor_1.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Diagnosticos de validacion: Bite2022_obreros")

message("1) Percentiles:")
print(percentiles_bite, n = Inf)
message("Histograma guardado en: ", file.path(plot_dir, "histograma_bite_obreros.png"))

message("")
message("2) Correlacion Bite2022_obreros vs Exposure2022_obreros (n=", nrow(comparables), "):")
print(tabla_correlacion, width = Inf)

message("")
message("3) Relacion con sector/tamano/DPTO:")
print(tabla_relacion_atributos, n = Inf, width = Inf)

message("")
message("4) Fraccion de firmas con Bite > 1 vs < 1:")
print(tabla_bite_mayor_menor_1, width = Inf)

message("")
message("Todas las salidas quedaron en: ", plot_dir)
