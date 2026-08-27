# ARCHIVADO el 2026-08-10: logica de exposicion al choque de la reforma
# tributaria de 2012 (Ley 1607), extraida de
# 3. SCRIPTS/descriptivo_exposicion_eam.R para que ese script quede
# exclusivamente con la logica del choque de salario minimo 2023.
# Ver README.md en esta misma carpeta para el motivo del archivado.
#
# Este script es AUTOCONTENIDO (no depende de 3. SCRIPTS/descriptivo_exposicion_eam.R):
# duplica la carga de datos y las utilidades que necesita, para poder
# correr de forma independiente si esta linea se retoma en el futuro.
# Se ejecuta desde la raiz del repositorio, igual que el resto de scripts
# del proyecto:
#
#   Rscript "descartado/2012_reforma_tributaria/descriptivo_exposicion_2012_eam.R"
#
# Nota metodologica (igual que en el script original):
# - Exposure2012 se define como intensidad laboral en 2011 (linea base
#   pre-choque de la reforma tributaria de 2012).
# - Si algunas variables no existen, el script usa candidatos alternativos
#   o deja el resultado en NA con un mensaje informativo.

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(here)
  library(scales)
})

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

# -----------------------------
# 1) Rutas (las salidas de esta linea archivada quedan en esta misma
#    carpeta, no en 4. RESULTADOS/ ni en 1. DATOS/, para no mezclarse con
#    las salidas activas del choque de 2023).
# -----------------------------

paths <- ensure_project_structure()
archivo_dir <- here::here("descartado", "2012_reforma_tributaria")
plot_dir <- archivo_dir
data_output_dir <- archivo_dir

macro_candidates <- c(
  paths$macro_base_eam,
  here::here("4. RESULTADOS", "macro_base_eam.rds")
)

macro_path <- find_existing_path(macro_candidates, "macro_base_eam.rds")

message("Leyendo macrobase desde: ", macro_path)

# -----------------------------
# 2) Utilidades (duplicadas de descriptivo_exposicion_eam.R)
# -----------------------------

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

safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

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
# 6) Base reducida (solo 2012)
# -----------------------------

base_reducida_2012 <- panel_2012 %>%
  select(
    all_of(core_vars),
    Exposure2012, quintil_exposure2012, periodo_2012
  )

readr::write_rds(base_reducida_2012, file.path(data_output_dir, "base_reducida_exposicion_2012_eam.rds"))
readr::write_csv(base_reducida_2012, file.path(data_output_dir, "base_reducida_exposicion_2012_eam.csv"))

# -----------------------------
# 7) Graficos de series 2012
# -----------------------------

metrics_info <- tribble(
  ~var, ~label, ~filename_stub,
  "empleo_total", "Empleo total promedio", "empleo_total",
  "costo_laboral_total", "Costo laboral total promedio", "costo_laboral_total",
  "productividad", "Productividad promedio", "productividad",
  "participacion_permanentes", "Participacion de permanentes", "participacion_permanentes",
  "intensidad_laboral", "Intensidad laboral promedio", "intensidad_laboral"
)

series_2012 <- panel_2012 %>%
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
# 8) Histograma de exposicion 2012
# -----------------------------

hist_2012 <- build_histogram(
  data = baseline_2011,
  exposure_var = "Exposure2012",
  title = "Distribucion de Exposure2012",
  subtitle = "Proxy de intensidad laboral medida en 2011",
  x_label = "Exposure2012"
)

save_plot(hist_2012, "histograma_exposure2012.png", width = 9, height = 6)

# -----------------------------
# 9) Boxplot alta vs baja exposicion 2012
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

box_2012 <- panel_2012 %>%
  filter(quintil_exposure2012 %in% c("Q1 - Muy baja", "Q5 - Muy alta")) %>%
  mutate(grupo_exposicion = if_else(quintil_exposure2012 == "Q5 - Muy alta", "Alta exposicion", "Baja exposicion")) %>%
  select(NORDEMP, ANIO, periodo = periodo_2012, grupo_exposicion, all_of(boxplot_vars)) %>%
  filter(!is.na(periodo)) %>%
  pivot_longer(cols = all_of(boxplot_vars), names_to = "indicador", values_to = "valor") %>%
  mutate(indicador = recode(indicador, !!!boxplot_labels))

plot_box_2012 <- build_boxplot(
  box_2012,
  title = "Empresas de alta y baja exposicion antes y despues de la reforma de 2012",
  subtitle = "Comparacion entre Q1 y Q5 de Exposure2012"
)

save_plot(plot_box_2012, "boxplots_alta_baja_exposicion_2012.png", width = 13, height = 8)

# -----------------------------
# 10) Tabla resumen por quintil y periodo (2012)
# -----------------------------

summary_2012 <- panel_2012 %>%
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

readr::write_csv(summary_2012, file.path(data_output_dir, "tabla_resumen_quintiles_2012.csv"))

# -----------------------------
# 11) Mensajes finales
# -----------------------------

message("Script completado (linea archivada: reforma tributaria 2012).")
message("Base reducida exportada en: ", file.path(data_output_dir, "base_reducida_exposicion_2012_eam.rds"))
message("Tabla resumen exportada en: ", file.path(data_output_dir, "tabla_resumen_quintiles_2012.csv"))
message("Graficos exportados en: ", plot_dir)
