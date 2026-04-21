suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(here)
  library(scales)
})

# ============================================================
# Analisis descriptivo preliminar de exposicion a choques laborales
# Macrobase EAM
# ============================================================
# Este script:
# 1. Carga la macrobase EAM desde here::here().
# 2. Agrega la base a nivel NORDEMP-ANIO.
# 3. Construye variables laborales y de desempeno.
# 4. Define exposiciones para:
#    - Reforma tributaria de 2012 (baseline 2011)
#    - Salario minimo 2023 (baseline 2022)
# 5. Genera series, histogramas, boxplots y tablas resumen.
# 6. Exporta una base reducida con las variables construidas.
#
# Nota metodologica:
# - Exposure2012 se define como intensidad laboral en 2011.
# - Exposure2022 se define como una proxy de cercania al salario minimo
#   usando el inverso del salario promedio en 2022.
# - Si algunas variables no existen, el script usa candidatos alternativos
#   o deja el resultado en NA con un mensaje informativo.

# -----------------------------
# 1) Rutas
# -----------------------------

plot_dir <- here::here("4. RESULTADOS", "descriptivos_exposicion")
data_output_dir <- here::here("1. DATOS", "6. BASES_DERIVADAS", "descriptivos_exposicion")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)

macro_candidates <- c(
  here::here("1. DATOS", "5. MACROBASE", "macro_base_eam.rds"),
  here::here("4. RESULTADOS", "macro_base_eam.rds")
)

macro_path <- macro_candidates[file.exists(macro_candidates)][1]

if (is.na(macro_path) || !nzchar(macro_path)) {
  stop(
    "No se encontro macro_base_eam.rds en rutas candidatas:\n",
    paste0("- ", macro_candidates, collapse = "\n")
  )
}

message("Leyendo macrobase desde: ", macro_path)

# -----------------------------
# 2) Utilidades
# -----------------------------

# Valida columnas estrictamente necesarias antes de correr el analisis.
check_required_vars <- function(data, vars) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Faltan columnas requeridas: ", paste(missing_vars, collapse = ", "))
  }
}

first_existing_var <- function(data, candidates, label = NULL) {
  match <- candidates[candidates %in% names(data)][1]

  if (is.na(match) || !nzchar(match)) {
    if (!is.null(label)) {
      message("No se encontro variable candidata para ", label, ". Se devolvera NA.")
    }
    return(NA_character_)
  }

  match
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Evita divisiones por cero y deja NA cuando el denominador no es usable.
safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

# Suma un conjunto de variables si existen en la base. Cuando todas faltan
# en una fila, conserva NA en lugar de devolver cero artificial.
sum_if_exists <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  out <- data %>%
    transmute(across(all_of(present), safe_numeric)) %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    mutate(.sum = rowSums(across(everything()))) %>%
    pull(.sum)

  all_missing <- data %>%
    transmute(across(all_of(present), ~is.na(safe_numeric(.x)))) %>%
    mutate(.all_missing = if_all(everything(), identity)) %>%
    pull(.all_missing)

  out[all_missing] <- NA_real_
  out
}

coalesce_positive <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  out <- rep(NA_real_, nrow(data))
  for (var in present) {
    current <- safe_numeric(data[[var]])
    current[current <= 0] <- NA_real_
    out <- dplyr::coalesce(out, current)
  }
  out
}

winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) {
    return(x)
  }

  limits <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limits[[1]]), limits[[2]])
}

make_quintiles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))

  if (length(valid) < 5 || dplyr::n_distinct(x[valid]) < 5) {
    message("No hay suficiente variacion para construir quintiles. Se devolvera NA.")
    return(out)
  }

  quint <- dplyr::ntile(x[valid], 5)
  labels <- c(
    "Q1 - Muy baja",
    "Q2 - Baja",
    "Q3 - Media",
    "Q4 - Alta",
    "Q5 - Muy alta"
  )

  out[valid] <- labels[quint]
  factor(out, levels = labels, ordered = TRUE)
}

save_plot <- function(plot_obj, filename, width = 10, height = 6) {
  ggsave(
    filename = file.path(plot_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 180
  )
}

build_series_plot <- function(data, value_var, title, subtitle, shock_year, y_label) {
  ggplot(data, aes(x = ANIO, y = .data[[value_var]], color = quintil)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    geom_vline(xintercept = shock_year, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Anio",
      y = y_label,
      color = "Quintil de exposicion"
    ) +
    theme_minimal(base_size = 12)
}

build_histogram <- function(data, exposure_var, title, subtitle, x_label) {
  # Se filtran valores no finitos para evitar advertencias en el histograma.
  plot_data <- data %>%
    filter(is.finite(.data[[exposure_var]]))

  ggplot(plot_data, aes(x = .data[[exposure_var]])) +
    geom_histogram(bins = 30, fill = "#2C7FB8", color = "white", alpha = 0.9) +
    scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = "Frecuencia"
    ) +
    theme_minimal(base_size = 12)
}

build_boxplot <- function(data, title, subtitle) {
  # Lo mismo para boxplots: se descartan infinitos/NA generados por ratios.
  plot_data <- data %>%
    filter(is.finite(valor))

  ggplot(plot_data, aes(x = periodo, y = valor, fill = grupo_exposicion)) +
    geom_boxplot(outlier.alpha = 0.2) +
    facet_wrap(~ indicador, scales = "free_y") +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Periodo",
      y = "Distribucion",
      fill = "Grupo"
    ) +
    theme_minimal(base_size = 12)
}

# -----------------------------
# 3) Carga y preparacion de datos
# -----------------------------

macro_base <- readr::read_rds(macro_path)
check_required_vars(macro_base, c("NORDEMP", "ANIO"))

component_candidates <- c(
  "PERTOTAL", "PERSOCU", "PERSOESC", "PERTEM3", "PPERYTEM",
  "SALARPER", "PRESSPER", "PRESPYTE", "SALPEYTE", "REMUTEMP",
  "C3R10C3", "VALAGRI", "PRODBIND", "VALORVEN", "VALVFAB"
)

present_components <- component_candidates[component_candidates %in% names(macro_base)]

if (length(present_components) == 0) {
  stop("Ninguna de las variables candidatas para construir indicadores esta disponible.")
}

panel_raw <- macro_base %>%
  mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  select(NORDEMP, ANIO, all_of(present_components)) %>%
  mutate(across(-c(NORDEMP, ANIO), safe_numeric))

panel <- panel_raw %>%
  # Si una empresa-anio aparece duplicada, se consolida sumando componentes
  # numericos para trabajar con un unico registro por NORDEMP-ANIO.
  group_by(NORDEMP, ANIO) %>%
  summarise(
    across(
      all_of(present_components),
      ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# -----------------------------
# 4) Variables basicas construidas
# -----------------------------

empleo_var <- first_existing_var(panel, c("PERTOTAL", "PPERYTEM", "PERSOESC"), "empleo_total")
permanentes_var <- first_existing_var(panel, c("PERSOCU", "PPERYTEM"), "trabajadores permanentes")
perm_mas_prop_var <- first_existing_var(panel, c("PERSOESC"), "permanentes + propietarios")
temporales_directos_var <- first_existing_var(panel, c("PERTEM3"), "temporales directos")

panel_built <- panel %>%
  mutate(
    # Las definiciones priorizan variables resumen ya construidas en EAM
    # y usan alternativas cuando la variable preferida no esta disponible.
    empleo_total = if (!is.na(empleo_var)) safe_numeric(.data[[empleo_var]]) else NA_real_,
    trabajadores_permanentes = if (!is.na(permanentes_var)) safe_numeric(.data[[permanentes_var]]) else NA_real_,
    trabajadores_temporales = if (!is.na(perm_mas_prop_var)) {
      pmax(empleo_total - safe_numeric(.data[[perm_mas_prop_var]]), 0)
    } else if (!is.na(temporales_directos_var)) {
      safe_numeric(.data[[temporales_directos_var]])
    } else {
      NA_real_
    },
    costo_laboral_total = if ("C3R10C3" %in% names(.)) {
      safe_numeric(C3R10C3)
    } else if (all(c("SALPEYTE", "PRESPYTE") %in% names(.))) {
      sum_if_exists(., c("SALPEYTE", "PRESPYTE"))
    } else {
      sum_if_exists(., c("SALARPER", "PRESSPER", "REMUTEMP"))
    },
    # Se usa una jerarquia de variables de resultado para productividad e
    # intensidad laboral, priorizando valor agregado y luego produccion/ventas.
    base_resultado = coalesce_positive(., c("VALAGRI", "PRODBIND", "VALORVEN", "VALVFAB")),
    salario_promedio = safe_divide(costo_laboral_total, empleo_total),
    productividad = safe_divide(base_resultado, empleo_total),
    intensidad_laboral = safe_divide(costo_laboral_total, base_resultado),
    participacion_permanentes = safe_divide(trabajadores_permanentes, empleo_total),
    participacion_temporales = safe_divide(trabajadores_temporales, empleo_total),
    tamano_empresa = case_when(
      is.na(empleo_total) ~ NA_character_,
      empleo_total < 50 ~ "Pequena",
      empleo_total < 200 ~ "Mediana",
      empleo_total >= 200 ~ "Grande",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Pequena", "Mediana", "Grande"), ordered = TRUE)
  )

core_vars <- c(
  "NORDEMP", "ANIO", "empleo_total", "costo_laboral_total", "salario_promedio",
  "productividad", "intensidad_laboral", "tamano_empresa",
  "trabajadores_permanentes", "trabajadores_temporales",
  "participacion_permanentes", "participacion_temporales"
)

# -----------------------------
# 5) Exposicion al shock 2012
# -----------------------------

baseline_2011 <- panel_built %>%
  filter(ANIO == 2011) %>%
  transmute(
    NORDEMP,
    # Exposure2012 captura que tan intensiva en trabajo era la firma justo
    # antes de la reforma tributaria.
    Exposure2012 = winsorize(intensidad_laboral),
    quintil_exposure2012 = make_quintiles(Exposure2012)
  )

panel_2012 <- panel_built %>%
  left_join(baseline_2011, by = "NORDEMP") %>%
  mutate(
    periodo_2012 = case_when(
      ANIO <= 2012 ~ "Pre (<=2012)",
      ANIO >= 2013 ~ "Post (>=2013)",
      TRUE ~ NA_character_
    )
  )

# -----------------------------
# 6) Exposicion al shock 2023
# -----------------------------

baseline_2022 <- panel_built %>%
  filter(ANIO == 2022) %>%
  # Menor salario promedio implica mayor exposicion potencial a un alza fuerte
  # del salario minimo; por eso se usa su inverso.
  mutate(exposure_low_wage = if_else(salario_promedio > 0, 1 / salario_promedio, NA_real_)) %>%
  transmute(
    NORDEMP,
    Exposure2022 = winsorize(exposure_low_wage),
    quintil_exposure2022 = make_quintiles(Exposure2022)
  )

panel_2022 <- panel_2012 %>%
  left_join(baseline_2022, by = "NORDEMP") %>%
  mutate(
    periodo_2023 = case_when(
      ANIO %in% 2020:2022 ~ "Pre (2020-2022)",
      ANIO %in% 2023:2024 ~ "Post (2023-2024)",
      TRUE ~ NA_character_
    )
  )

# -----------------------------
# 7) Base reducida y chequeos
# -----------------------------

base_reducida <- panel_2022 %>%
  select(
    all_of(core_vars),
    Exposure2012, quintil_exposure2012, periodo_2012,
    Exposure2022, quintil_exposure2022, periodo_2023
  )

readr::write_rds(base_reducida, file.path(data_output_dir, "base_reducida_exposicion_eam.rds"))
readr::write_csv(base_reducida, file.path(data_output_dir, "base_reducida_exposicion_eam.csv"))

# -----------------------------
# 8) Graficos de series 2012
# -----------------------------

metrics_info <- tribble(
  ~var, ~label, ~filename_stub,
  "empleo_total", "Empleo total promedio", "empleo_total",
  "costo_laboral_total", "Costo laboral total promedio", "costo_laboral_total",
  "productividad", "Productividad promedio", "productividad",
  "participacion_permanentes", "Participacion de permanentes", "participacion_permanentes",
  "intensidad_laboral", "Intensidad laboral promedio", "intensidad_laboral"
)

series_2012 <- panel_2022 %>%
  # Cada punto es el promedio simple por anio dentro de cada quintil.
  filter(!is.na(quintil_exposure2012)) %>%
  group_by(ANIO, quintil = quintil_exposure2012) %>%
  summarise(
    across(
      all_of(metrics_info$var),
      ~mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

for (i in seq_len(nrow(metrics_info))) {
  metric <- metrics_info[i, ]
  plot_i <- build_series_plot(
    data = series_2012,
    value_var = metric$var,
    title = paste0(metric$label, " por quintil de exposicion"),
    subtitle = "Shock: reforma tributaria 2012 | Exposicion medida en 2011 | Linea vertical en 2013",
    shock_year = 2013,
    y_label = metric$label
  )

  save_plot(plot_i, paste0("serie_2012_", metric$filename_stub, ".png"))
}

# -----------------------------
# 9) Graficos de series 2023
# -----------------------------

series_2023 <- panel_2022 %>%
  filter(ANIO %in% 2020:2024, !is.na(quintil_exposure2022)) %>%
  group_by(ANIO, quintil = quintil_exposure2022) %>%
  summarise(
    across(
      all_of(metrics_info$var),
      ~mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

for (i in seq_len(nrow(metrics_info))) {
  metric <- metrics_info[i, ]
  plot_i <- build_series_plot(
    data = series_2023,
    value_var = metric$var,
    title = paste0(metric$label, " por quintil de exposicion"),
    subtitle = "Shock: salario minimo 2023 | Exposicion medida en 2022 | Ventana 2020-2024",
    shock_year = 2023,
    y_label = metric$label
  )

  save_plot(plot_i, paste0("serie_2023_", metric$filename_stub, ".png"))
}

# -----------------------------
# 10) Histogramas de exposicion
# -----------------------------

hist_2012 <- build_histogram(
  data = baseline_2011,
  exposure_var = "Exposure2012",
  title = "Distribucion de Exposure2012",
  subtitle = "Proxy de intensidad laboral medida en 2011",
  x_label = "Exposure2012"
)

hist_2022 <- build_histogram(
  data = baseline_2022,
  exposure_var = "Exposure2022",
  title = "Distribucion de Exposure2022",
  subtitle = "Proxy de cercania al salario minimo medida en 2022",
  x_label = "Exposure2022"
)

save_plot(hist_2012, "histograma_exposure2012.png", width = 9, height = 6)
save_plot(hist_2022, "histograma_exposure2022.png", width = 9, height = 6)

# -----------------------------
# 11) Boxplots alta vs baja exposicion
# -----------------------------

boxplot_vars <- c(
  "empleo_total",
  "costo_laboral_total",
  "productividad",
  "participacion_permanentes",
  "intensidad_laboral"
)

boxplot_labels <- c(
  empleo_total = "Empleo total",
  costo_laboral_total = "Costo laboral total",
  productividad = "Productividad",
  participacion_permanentes = "Participacion permanentes",
  intensidad_laboral = "Intensidad laboral"
)

box_2012 <- panel_2022 %>%
  filter(quintil_exposure2012 %in% c("Q1 - Muy baja", "Q5 - Muy alta")) %>%
  mutate(grupo_exposicion = if_else(quintil_exposure2012 == "Q5 - Muy alta", "Alta exposicion", "Baja exposicion")) %>%
  select(NORDEMP, ANIO, periodo = periodo_2012, grupo_exposicion, all_of(boxplot_vars)) %>%
  filter(!is.na(periodo)) %>%
  pivot_longer(cols = all_of(boxplot_vars), names_to = "indicador", values_to = "valor") %>%
  mutate(indicador = recode(indicador, !!!boxplot_labels))

box_2023 <- panel_2022 %>%
  filter(ANIO %in% 2020:2024, quintil_exposure2022 %in% c("Q1 - Muy baja", "Q5 - Muy alta")) %>%
  mutate(grupo_exposicion = if_else(quintil_exposure2022 == "Q5 - Muy alta", "Alta exposicion", "Baja exposicion")) %>%
  select(NORDEMP, ANIO, periodo = periodo_2023, grupo_exposicion, all_of(boxplot_vars)) %>%
  filter(!is.na(periodo)) %>%
  pivot_longer(cols = all_of(boxplot_vars), names_to = "indicador", values_to = "valor") %>%
  mutate(indicador = recode(indicador, !!!boxplot_labels))

plot_box_2012 <- build_boxplot(
  box_2012,
  title = "Empresas de alta y baja exposicion antes y despues de la reforma de 2012",
  subtitle = "Comparacion entre Q1 y Q5 de Exposure2012"
)

plot_box_2023 <- build_boxplot(
  box_2023,
  title = "Empresas de alta y baja exposicion antes y despues del shock de salario minimo 2023",
  subtitle = "Comparacion entre Q1 y Q5 de Exposure2022"
)

save_plot(plot_box_2012, "boxplots_alta_baja_exposicion_2012.png", width = 13, height = 8)
save_plot(plot_box_2023, "boxplots_alta_baja_exposicion_2023.png", width = 13, height = 8)

# -----------------------------
# 12) Tablas resumen por quintil y periodo
# -----------------------------

summary_2012 <- panel_2022 %>%
  filter(!is.na(quintil_exposure2012), !is.na(periodo_2012)) %>%
  group_by(quintil_exposure2012, periodo_2012) %>%
  summarise(
    n_empresas = n_distinct(NORDEMP),
    n_obs = n(),
    empleo_total_promedio = mean(empleo_total, na.rm = TRUE),
    costo_laboral_total_promedio = mean(costo_laboral_total, na.rm = TRUE),
    salario_promedio_promedio = mean(salario_promedio, na.rm = TRUE),
    productividad_promedio = mean(productividad, na.rm = TRUE),
    participacion_permanentes_promedio = mean(participacion_permanentes, na.rm = TRUE),
    participacion_temporales_promedio = mean(participacion_temporales, na.rm = TRUE),
    intensidad_laboral_promedio = mean(intensidad_laboral, na.rm = TRUE),
    .groups = "drop"
  )

summary_2023 <- panel_2022 %>%
  filter(ANIO %in% 2020:2024, !is.na(quintil_exposure2022), !is.na(periodo_2023)) %>%
  group_by(quintil_exposure2022, periodo_2023) %>%
  summarise(
    n_empresas = n_distinct(NORDEMP),
    n_obs = n(),
    empleo_total_promedio = mean(empleo_total, na.rm = TRUE),
    costo_laboral_total_promedio = mean(costo_laboral_total, na.rm = TRUE),
    salario_promedio_promedio = mean(salario_promedio, na.rm = TRUE),
    productividad_promedio = mean(productividad, na.rm = TRUE),
    participacion_permanentes_promedio = mean(participacion_permanentes, na.rm = TRUE),
    participacion_temporales_promedio = mean(participacion_temporales, na.rm = TRUE),
    intensidad_laboral_promedio = mean(intensidad_laboral, na.rm = TRUE),
    .groups = "drop"
  )

summary_combined <- bind_rows(
  summary_2012 %>%
    rename(quintil = quintil_exposure2012, periodo = periodo_2012) %>%
    mutate(shock = "Reforma 2012"),
  summary_2023 %>%
    rename(quintil = quintil_exposure2022, periodo = periodo_2023) %>%
    mutate(shock = "Salario minimo 2023")
) %>%
  select(shock, quintil, periodo, everything())

readr::write_csv(summary_2012, file.path(data_output_dir, "tabla_resumen_quintiles_2012.csv"))
readr::write_csv(summary_2023, file.path(data_output_dir, "tabla_resumen_quintiles_2023.csv"))
readr::write_csv(summary_combined, file.path(data_output_dir, "tabla_resumen_quintiles_consolidada.csv"))

# -----------------------------
# 13) Mensajes finales
# -----------------------------

message("Script completado.")
message("Base reducida exportada en: ", file.path(data_output_dir, "base_reducida_exposicion_eam.rds"))
message("Tablas resumen exportadas en: ", data_output_dir)
message("Graficos exportados en: ", plot_dir)
