# Diagnostico visual RAPIDO y PRELIMINAR de pre-tendencias 2015-2019 por
# quintil de Exposure2022_obreros, para decidir si vale la pena invertir en
# el panel formal del event study (con chequeo de balance, winsorizacion
# consistente, etc.) antes de construirlo.
#
# Este script es exploratorio, no la version final:
# - NO winsoriza las variables de resultado (se acepta ese riesgo para esta
#   inspeccion rapida; el panel formal si lo hara).
# - NO corre ninguna regresion de event study, solo grafica promedios.
#
# Ventana 2015-2019: EXCLUYE 2020 explicitamente porque ese anio arranca el
# choque de la pandemia (COVID-19), que introduciria una discontinuidad
# ajena al diseño de pre-tendencias y contaminaria la lectura visual de si
# los quintiles se mueven paralelos antes del choque de salario minimo 2023.
#
# Variables de resultado: se reusan exactamente las ya definidas en
# 3. SCRIPTS/descriptivo_exposicion_eam.R (empleo_total, costo_laboral_total,
# base_resultado, intensidad_laboral) para consistencia entre scripts. Nota:
# ese script no separa "produccion" y "ventas" en dos variables -- las
# combina en una sola jerarquia de disponibilidad (base_resultado, prioriza
# VALAGRI > PRODBIND > VALORVEN > VALVFAB); aqui se usa esa misma variable
# combinada, etiquetada como "Produccion/ventas (proxy)".
#
# Salidas (versionadas):
# - 4. RESULTADOS/descriptivos_exposicion/preliminar_tendencias_2015_2019_*.png

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "ggplot2", "scales")
load_project_packages(required_packages)

paths <- ensure_project_structure()
plot_dir <- paths$resultados_exposicion
data_dir <- paths$bases_derivadas_exposicion

exposicion_path <- file.path(data_dir, "exposicion_obreros_eam.rds")
if (!file.exists(exposicion_path)) {
  stop(
    "Falta exposicion_obreros_eam.rds (quintiles de Exposure2022_obreros). ",
    "Corre construir_exposicion_obreros_eam.R (rama feature/exposicion-obreros-operarios) primero."
  )
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

sum_if_exists <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) return(rep(NA_real_, nrow(data)))

  out <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), safe_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::coalesce(.x, 0))) %>%
    dplyr::mutate(.sum = rowSums(dplyr::across(dplyr::everything()))) %>%
    dplyr::pull(.sum)

  all_missing <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), ~is.na(safe_numeric(.x)))) %>%
    dplyr::mutate(.all_missing = dplyr::if_all(dplyr::everything(), identity)) %>%
    dplyr::pull(.all_missing)

  out[all_missing] <- NA_real_
  out
}

coalesce_positive <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) return(rep(NA_real_, nrow(data)))

  out <- rep(NA_real_, nrow(data))
  for (var in present) {
    current <- safe_numeric(data[[var]])
    current[current <= 0] <- NA_real_
    out <- dplyr::coalesce(out, current)
  }
  out
}

first_existing_var <- function(data, candidates) {
  match <- candidates[candidates %in% names(data)][1]
  if (is.na(match) || !nzchar(match)) return(NA_character_)
  match
}

# ------------------------------------------------------------------
# 1) Quintiles de Exposure2022_obreros (ya calculados en la rama
#    feature/exposicion-obreros-operarios).
# ------------------------------------------------------------------

quintiles <- readr::read_rds(exposicion_path) %>%
  dplyr::distinct(NORDEMP, quintil_exposure2022_obreros) %>%
  dplyr::filter(!is.na(quintil_exposure2022_obreros))

# ------------------------------------------------------------------
# 2) Macrobase 2015-2019 (excluye 2020, ver nota arriba), solo firmas con
#    exposicion valida en 2022.
# ------------------------------------------------------------------

macro_path <- paths$macro_base_eam
macro_base <- readr::read_rds(macro_path)
names(macro_base) <- toupper(names(macro_base))

component_candidates <- c(
  "PERTOTAL", "PERSOCU", "PERSOESC", "PERTEM3", "PPERYTEM",
  "SALARPER", "PRESSPER", "PRESPYTE", "SALPEYTE", "REMUTEMP",
  "C3R10C3", "VALAGRI", "PRODBIND", "VALORVEN", "VALVFAB"
)
present_components <- component_candidates[component_candidates %in% names(macro_base)]

panel_raw <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  dplyr::filter(
    !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO),
    ANIO %in% 2015:2019,  # excluye 2020 explicitamente (arranque choque COVID-19)
    NORDEMP %in% quintiles$NORDEMP
  ) %>%
  dplyr::select(NORDEMP, ANIO, dplyr::all_of(present_components)) %>%
  dplyr::mutate(dplyr::across(-c(NORDEMP, ANIO), safe_numeric))

panel <- panel_raw %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(present_components), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

empleo_var <- first_existing_var(panel, c("PERTOTAL", "PPERYTEM", "PERSOESC"))

panel_built <- panel %>%
  dplyr::mutate(
    empleo_total = if (!is.na(empleo_var)) safe_numeric(.data[[empleo_var]]) else NA_real_,
    costo_laboral_total = if ("C3R10C3" %in% names(.)) {
      safe_numeric(C3R10C3)
    } else if (all(c("SALPEYTE", "PRESPYTE") %in% names(.))) {
      sum_if_exists(., c("SALPEYTE", "PRESPYTE"))
    } else {
      sum_if_exists(., c("SALARPER", "PRESSPER", "REMUTEMP"))
    },
    base_resultado = coalesce_positive(., c("VALAGRI", "PRODBIND", "VALORVEN", "VALVFAB")),
    intensidad_laboral = safe_divide(costo_laboral_total, base_resultado)
  ) %>%
  dplyr::left_join(quintiles, by = "NORDEMP")

# ------------------------------------------------------------------
# 3) Promedio por anio y quintil, 2015-2019, SIN winsorizar (exploratorio).
# ------------------------------------------------------------------

metrics_info <- tibble::tribble(
  ~var, ~label, ~filename_stub,
  "empleo_total", "Empleo total (promedio)", "empleo_total",
  "costo_laboral_total", "Costo laboral total (promedio)", "costo_laboral_total",
  "base_resultado", "Produccion/ventas (proxy, promedio)", "produccion_ventas",
  "intensidad_laboral", "Intensidad laboral (promedio)", "intensidad_laboral"
)

series_preliminar <- panel_built %>%
  dplyr::group_by(ANIO, quintil = quintil_exposure2022_obreros) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(metrics_info$var), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

build_series_plot <- function(data, value_var, title, y_label) {
  ggplot2::ggplot(data, ggplot2::aes(x = ANIO, y = .data[[value_var]], color = quintil)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = 2015:2019) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    ggplot2::labs(
      title = title,
      subtitle = "Diagnostico PRELIMINAR, sin winsorizar | 2015-2019 (2020 excluido) | por quintil de Exposure2022_obreros",
      x = "Anio", y = y_label, color = "Quintil de exposicion"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

for (i in seq_len(nrow(metrics_info))) {
  metric <- metrics_info[i, ]
  p <- build_series_plot(series_preliminar, metric$var, metric$label, metric$label)
  ggplot2::ggsave(
    filename = file.path(plot_dir, paste0("preliminar_tendencias_2015_2019_", metric$filename_stub, ".png")),
    plot = p, width = 9, height = 6, dpi = 180
  )
}

script_header("Diagnostico preliminar de pre-tendencias 2015-2019")
message("Firmas con quintil valido y datos 2015-2019: ", dplyr::n_distinct(panel_built$NORDEMP))
message("Graficos guardados en: ", plot_dir)
message("Archivos: ", paste0("preliminar_tendencias_2015_2019_", metrics_info$filename_stub, ".png", collapse = ", "))
