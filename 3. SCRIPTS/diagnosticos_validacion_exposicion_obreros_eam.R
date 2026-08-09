# Diagnosticos de validacion para Exposure2022_obreros (Paso 6):
# 1) Cobertura de las 3 categorias ocupacionales por anio.
# 2) Distribucion de Exposure2022_obreros vs Exposure2022 (original), mismo panel.
# 3) Relacion de Exposure2022_obreros con sector (CIIU3), tamano de empresa y DPTO.
# 4) Tabla de salarios promedio por categoria (obrero vs prof-tecnico vs administrativo).
#
# No modifica ningun script previo: solo lee las salidas ya generadas por
# los Pasos 3, 4 y 5, y por el pipeline original (base_reducida_exposicion_eam.rds).
#
# Salidas en 4. RESULTADOS/descriptivos_exposicion/ (segun lo pedido para
# este paso; ver 4. RESULTADOS/README.md para la convencion general del
# resto del pipeline).

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "ggplot2", "scales", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
plot_dir <- paths$resultados_exposicion

ANIO_BASE_EXPOSICION <- 2022

leer_o_fallar <- function(path, mensaje) {
  if (!file.exists(path)) stop(mensaje)
  readr::read_rds(path)
}

conteo <- leer_o_fallar(
  file.path(data_dir, "conteo_personal_categoria_eam.rds"),
  "Falta conteo_personal_categoria_eam.rds. Corre el Paso 3 primero."
)
salarios <- leer_o_fallar(
  file.path(data_dir, "salarios_promedio_categoria_eam.rds"),
  "Falta salarios_promedio_categoria_eam.rds. Corre el Paso 4 primero."
)
exposicion_obreros <- leer_o_fallar(
  file.path(data_dir, "exposicion_obreros_eam.rds"),
  "Falta exposicion_obreros_eam.rds. Corre el Paso 5 primero."
)
base_original <- leer_o_fallar(
  file.path(data_dir, "base_reducida_exposicion_eam.rds"),
  "Falta base_reducida_exposicion_eam.rds. Corre el flujo original (Exposure2022) primero."
)

save_plot <- function(plot_obj, filename, width = 10, height = 6) {
  ggplot2::ggsave(
    filename = file.path(plot_dir, filename),
    plot = plot_obj,
    width = width, height = height, dpi = 180
  )
}

# ------------------------------------------------------------------
# 1) Cobertura de las 3 categorias por anio
# ------------------------------------------------------------------

cobertura_categorias <- conteo %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    n_empresas = dplyr::n(),
    pct_con_las_3_categorias = round(
      100 * mean(!is.na(total_obreros) & !is.na(total_prof_tecnico) & !is.na(total_administrativos)),
      2
    ),
    .groups = "drop"
  )

readr::write_csv(cobertura_categorias, file.path(plot_dir, "tabla_cobertura_categorias_ocupacionales.csv"))

# ------------------------------------------------------------------
# 2) Exposure2022_obreros (nueva) vs Exposure2022 (original): histograma
#    conjunto + percentiles
# ------------------------------------------------------------------

nueva_baseline <- exposicion_obreros %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros)

original_baseline <- base_original %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, Exposure2022, tamano_empresa)

comparacion_exposiciones <- nueva_baseline %>%
  dplyr::inner_join(original_baseline, by = "NORDEMP")

percentiles_comparacion <- tibble::tibble(
  percentil = c("p1", "p10", "p25", "p50", "p75", "p90", "p99"),
  Exposure2022_obreros = quantile(comparacion_exposiciones$Exposure2022_obreros, c(.01, .10, .25, .50, .75, .90, .99), na.rm = TRUE),
  Exposure2022_original = quantile(comparacion_exposiciones$Exposure2022, c(.01, .10, .25, .50, .75, .90, .99), na.rm = TRUE)
)

readr::write_csv(percentiles_comparacion, file.path(plot_dir, "tabla_percentiles_exposure_nueva_vs_original.csv"))

comparacion_larga <- comparacion_exposiciones %>%
  dplyr::select(NORDEMP, Exposure2022_obreros, Exposure2022) %>%
  tidyr::pivot_longer(
    cols = c(Exposure2022_obreros, Exposure2022),
    names_to = "medida",
    values_to = "valor"
  ) %>%
  dplyr::mutate(
    medida = dplyr::recode(
      medida,
      Exposure2022_obreros = "Exposure2022_obreros (nueva, participacion de obreros)",
      Exposure2022 = "Exposure2022 (original, 1/salario promedio)"
    )
  ) %>%
  dplyr::filter(is.finite(valor))

hist_comparado <- ggplot2::ggplot(comparacion_larga, ggplot2::aes(x = valor, fill = medida)) +
  ggplot2::geom_histogram(bins = 30, alpha = 0.6, position = "identity", color = "white") +
  ggplot2::facet_wrap(~medida, scales = "free", ncol = 1) +
  ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    title = "Exposure2022_obreros (nueva) vs Exposure2022 (original)",
    subtitle = paste0("Linea base ", ANIO_BASE_EXPOSICION, " | n = ", nrow(comparacion_exposiciones)),
    x = "Valor de exposicion", y = "Frecuencia", fill = "Medida"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "none")

save_plot(hist_comparado, "histograma_exposure_nueva_vs_original.png", width = 9, height = 8)

# ------------------------------------------------------------------
# 3) Relacion de Exposure2022_obreros con sector, tamano de empresa y
#    DPTO. Para variables categoricas de alta cardinalidad (sector, DPTO)
#    se reporta R2 de un ANOVA de un factor (% de varianza explicada),
#    NO una correlacion de Pearson, porque no son variables ordinales.
#    Para tamano_empresa (Pequena < Mediana < Grande, ordinal) se reporta
#    ademas una correlacion de Spearman.
#
# Nota sobre la variable de sector: "CIIU3" en la macrobase es CIIU
# Revision 3 y SOLO tiene datos 2008-2011 (0% desde 2012). DANE paso a
# CIIU Revision 4 en 2012; esa columna se llama "CIIU4" casi todos los
# anios, excepto 2013 donde aparece como "CIIU_4" (con guion bajo). Para
# la linea base 2022 se usa CIIU4, que es la que realmente tiene datos.
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

comparacion_con_atributos <- comparacion_exposiciones %>%
  dplyr::left_join(macro_atributos, by = "NORDEMP") %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

r2_anova <- function(data, var_categorica) {
  formula_txt <- paste0("Exposure2022_obreros ~ factor(", var_categorica, ")")
  datos_validos <- data %>% dplyr::filter(!is.na(.data[[var_categorica]]))
  if (dplyr::n_distinct(datos_validos[[var_categorica]]) < 2) return(NA_real_)
  modelo <- stats::aov(stats::as.formula(formula_txt), data = datos_validos)
  resumen <- summary(modelo)[[1]]
  ss_efecto <- resumen[1, "Sum Sq"]
  ss_total <- sum(resumen[, "Sum Sq"])
  round(ss_efecto / ss_total, 4)
}

r2_sector <- r2_anova(comparacion_con_atributos, "SECTOR_CIIU")
r2_dpto <- r2_anova(comparacion_con_atributos, "DPTO")

tamano_ordenado <- comparacion_con_atributos %>%
  dplyr::filter(!is.na(tamano_empresa)) %>%
  dplyr::mutate(tamano_num = as.integer(factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"), ordered = TRUE)))

r2_tamano <- r2_anova(comparacion_con_atributos, "tamano_empresa")
spearman_tamano <- if (nrow(tamano_ordenado) >= 3) {
  round(cor(tamano_ordenado$tamano_num, tamano_ordenado$Exposure2022_obreros, method = "spearman", use = "complete.obs"), 3)
} else {
  NA_real_
}

tabla_relacion_atributos <- tibble::tibble(
  atributo = c("Sector (CIIU4)", "DPTO (region)", "Tamano de empresa"),
  tipo_medida = c("R2 (ANOVA, % varianza explicada)", "R2 (ANOVA, % varianza explicada)", "R2 (ANOVA) y Spearman (ordinal)"),
  r2 = c(r2_sector, r2_dpto, r2_tamano),
  spearman = c(NA_real_, NA_real_, spearman_tamano)
)

readr::write_csv(tabla_relacion_atributos, file.path(plot_dir, "tabla_relacion_exposure_atributos.csv"))

resumen_por_sector <- comparacion_con_atributos %>%
  dplyr::filter(!is.na(SECTOR_CIIU)) %>%
  dplyr::group_by(SECTOR_CIIU) %>%
  dplyr::summarise(n_empresas = dplyr::n(), exposure_obreros_media = round(mean(Exposure2022_obreros, na.rm = TRUE), 3), .groups = "drop") %>%
  dplyr::filter(n_empresas >= 10) %>%
  dplyr::arrange(dplyr::desc(exposure_obreros_media))

resumen_por_dpto <- comparacion_con_atributos %>%
  dplyr::filter(!is.na(DPTO)) %>%
  dplyr::group_by(DPTO) %>%
  dplyr::summarise(n_empresas = dplyr::n(), exposure_obreros_media = round(mean(Exposure2022_obreros, na.rm = TRUE), 3), .groups = "drop") %>%
  dplyr::filter(n_empresas >= 10) %>%
  dplyr::arrange(dplyr::desc(exposure_obreros_media))

resumen_por_tamano <- comparacion_con_atributos %>%
  dplyr::filter(!is.na(tamano_empresa)) %>%
  dplyr::group_by(tamano_empresa) %>%
  dplyr::summarise(n_empresas = dplyr::n(), exposure_obreros_media = round(mean(Exposure2022_obreros, na.rm = TRUE), 3), .groups = "drop")

readr::write_csv(resumen_por_sector, file.path(plot_dir, "exposure_obreros_por_sector_ciiu4.csv"))
readr::write_csv(resumen_por_dpto, file.path(plot_dir, "exposure_obreros_por_dpto.csv"))
readr::write_csv(resumen_por_tamano, file.path(plot_dir, "exposure_obreros_por_tamano.csv"))

# ------------------------------------------------------------------
# 4) Salarios promedio por categoria: obrero vs prof-tecnico vs
#    administrativo, por anio (retoma el Paso 4, guardado ahora en
#    4. RESULTADOS/ como pide este paso).
# ------------------------------------------------------------------

tabla_salarios_categoria <- salarios %>%
  dplyr::filter(
    !is.na(salario_promedio_obrero) | !is.na(salario_promedio_administrativo) | !is.na(salario_promedio_prof_tecnico)
  ) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    n_empresas = dplyr::n(),
    salario_obrero_mediana = round(median(salario_promedio_obrero, na.rm = TRUE), 1),
    salario_admin_mediana = round(median(salario_promedio_administrativo, na.rm = TRUE), 1),
    salario_pt_mediana = round(median(salario_promedio_prof_tecnico, na.rm = TRUE), 1),
    obrero_es_el_mas_bajo = salario_obrero_mediana <= pmin(salario_admin_mediana, salario_pt_mediana, na.rm = TRUE),
    ratio_admin_sobre_obrero = round(salario_admin_mediana / salario_obrero_mediana, 2),
    ratio_pt_sobre_obrero = round(salario_pt_mediana / salario_obrero_mediana, 2),
    .groups = "drop"
  )

readr::write_csv(tabla_salarios_categoria, file.path(plot_dir, "tabla_salarios_promedio_por_categoria.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Diagnosticos de validacion: Exposure2022_obreros")

message("1) Cobertura de las 3 categorias por anio: exportada en tabla_cobertura_categorias_ocupacionales.csv")
print(cobertura_categorias, n = Inf)

message("")
message("2) Percentiles Exposure2022_obreros vs Exposure2022 (n=", nrow(comparacion_exposiciones), "):")
print(percentiles_comparacion, n = Inf)
message("Histograma comparado guardado en: ", file.path(plot_dir, "histograma_exposure_nueva_vs_original.png"))

message("")
message("3) Relacion de Exposure2022_obreros con sector/tamano/DPTO:")
print(tabla_relacion_atributos, n = Inf, width = Inf)

message("")
message("4) Salario promedio por categoria (mediana), obrero vs prof-tecnico vs administrativo:")
print(tabla_salarios_categoria, n = Inf, width = Inf)

message("")
message("Todas las salidas quedaron en: ", plot_dir)
