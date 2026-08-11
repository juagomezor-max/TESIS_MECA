# Investiga la causa de la divergencia 2018-2019 detectada en el diagnostico
# preliminar (diagnostico_preliminar_tendencias_2015_2019.R) en empleo_total
# y produccion/ventas por quintil de Exposure2022_obreros, antes de invertir
# en el panel formal del event study.
#
# Paso 1: identifica las firmas de Q4 (el quintil que mas diverge) con mayor
#         cambio absoluto 2017->2019 en empleo_total y produccion, y prueba
#         si la divergencia agregada sobrevive al excluirlas.
# Paso 2: revisa concentracion sectorial (CIIU4) entre esas firmas, sin
#         asumir causalidad con la Ley de Financiamiento de 2019 (Ley 1943).
# Paso 3: test F formal de pre-tendencias (Y ~ quintil:anio_lineal + FE
#         firma), con y sin las firmas atipicas del Paso 1.
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/divergencia_firmas_q4_top.csv
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/divergencia_sector_q4.csv
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/divergencia_test_f_pretendencias.csv
# Salidas versionadas:
# - 4. RESULTADOS/descriptivos_exposicion/preliminar_tendencias_q4_con_sin_atipicas_*.png

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "ggplot2", "scales", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
plot_dir <- paths$resultados_exposicion

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

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
# 0) Reconstruir el mismo panel 2015-2019 (con quintiles y atributos de
#    sector/tamano) del diagnostico preliminar.
# ------------------------------------------------------------------

exposicion_path <- file.path(data_dir, "exposicion_obreros_eam.rds")
if (!file.exists(exposicion_path)) stop("Falta exposicion_obreros_eam.rds.")

quintiles <- readr::read_rds(exposicion_path) %>%
  dplyr::distinct(NORDEMP, quintil_exposure2022_obreros) %>%
  dplyr::filter(!is.na(quintil_exposure2022_obreros))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))

component_candidates <- c(
  "PERTOTAL", "PERSOCU", "PERSOESC", "PERTEM3", "PPERYTEM",
  "SALARPER", "PRESSPER", "PRESPYTE", "SALPEYTE", "REMUTEMP",
  "C3R10C3", "VALAGRI", "PRODBIND", "VALORVEN", "VALVFAB"
)
present_components <- component_candidates[component_candidates %in% names(macro_base)]

panel_raw <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = suppressWarnings(as.integer(ANIO))) %>%
  dplyr::filter(
    !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO),
    ANIO %in% 2015:2019,
    NORDEMP %in% quintiles$NORDEMP
  ) %>%
  dplyr::select(NORDEMP, ANIO, CIIU4, dplyr::all_of(present_components)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(present_components), safe_numeric))

panel <- panel_raw %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(present_components), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    CIIU4 = dplyr::first(CIIU4),
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
    tamano_empresa = dplyr::case_when(
      is.na(empleo_total) ~ NA_character_,
      empleo_total < 50 ~ "Pequena",
      empleo_total < 200 ~ "Mediana",
      empleo_total >= 200 ~ "Grande",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::left_join(quintiles, by = "NORDEMP")

# ------------------------------------------------------------------
# PASO 1: firmas de Q4 con mayor cambio absoluto 2017 -> 2019
# ------------------------------------------------------------------

q4_2017 <- panel_built %>%
  dplyr::filter(quintil_exposure2022_obreros == "Q4 - Alta", ANIO == 2017) %>%
  dplyr::select(NORDEMP, CIIU4_2017 = CIIU4, tamano_2017 = tamano_empresa,
                empleo_2017 = empleo_total, produccion_2017 = base_resultado)

q4_2019 <- panel_built %>%
  dplyr::filter(quintil_exposure2022_obreros == "Q4 - Alta", ANIO == 2019) %>%
  dplyr::select(NORDEMP, empleo_2019 = empleo_total, produccion_2019 = base_resultado)

presencia_q4 <- panel_built %>%
  dplyr::filter(quintil_exposure2022_obreros == "Q4 - Alta") %>%
  dplyr::distinct(NORDEMP, ANIO) %>%
  dplyr::mutate(presente = TRUE) %>%
  tidyr::pivot_wider(names_from = ANIO, values_from = presente, values_fill = FALSE, names_prefix = "presente_")

q4_cambio <- q4_2017 %>%
  dplyr::inner_join(q4_2019, by = "NORDEMP") %>%
  dplyr::left_join(presencia_q4, by = "NORDEMP") %>%
  dplyr::mutate(
    delta_empleo = empleo_2019 - empleo_2017,
    delta_produccion = produccion_2019 - produccion_2017
  )

top_empleo <- q4_cambio %>%
  dplyr::filter(!is.na(delta_empleo)) %>%
  dplyr::arrange(dplyr::desc(abs(delta_empleo))) %>%
  dplyr::slice_head(n = 15) %>%
  dplyr::mutate(criterio = "top_delta_empleo")

top_produccion <- q4_cambio %>%
  dplyr::filter(!is.na(delta_produccion)) %>%
  dplyr::arrange(dplyr::desc(abs(delta_produccion))) %>%
  dplyr::slice_head(n = 15) %>%
  dplyr::mutate(criterio = "top_delta_produccion")

top_firmas <- dplyr::bind_rows(top_empleo, top_produccion) %>%
  dplyr::select(
    NORDEMP, criterio, CIIU4_2017, tamano_2017,
    presente_2015, presente_2016, presente_2017, presente_2018, presente_2019,
    empleo_2017, empleo_2019, delta_empleo,
    produccion_2017, produccion_2019, delta_produccion
  )

readr::write_csv(top_firmas, file.path(data_dir, "divergencia_firmas_q4_top.csv"))

nordemp_atipicas <- unique(c(top_empleo$NORDEMP, top_produccion$NORDEMP))

# Recalcula la serie de Q4 (2015-2019) excluyendo las firmas atipicas, y la
# compara contra la serie completa.
serie_q4_completa <- panel_built %>%
  dplyr::filter(quintil_exposure2022_obreros == "Q4 - Alta") %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    empleo_total = mean(empleo_total, na.rm = TRUE),
    base_resultado = mean(base_resultado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(muestra = "Q4 completo")

serie_q4_sin_atipicas <- panel_built %>%
  dplyr::filter(quintil_exposure2022_obreros == "Q4 - Alta", !NORDEMP %in% nordemp_atipicas) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    empleo_total = mean(empleo_total, na.rm = TRUE),
    base_resultado = mean(base_resultado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(muestra = "Q4 sin las 15+15 firmas mas atipicas")

serie_q4_comparada <- dplyr::bind_rows(serie_q4_completa, serie_q4_sin_atipicas)

plot_empleo <- ggplot2::ggplot(serie_q4_comparada, ggplot2::aes(x = ANIO, y = empleo_total, color = muestra)) +
  ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = 2015:2019) +
  ggplot2::labs(title = "Q4: empleo total, con vs. sin firmas atipicas", x = "Anio", y = "Empleo total (promedio)", color = NULL) +
  ggplot2::theme_minimal(base_size = 12)

plot_produccion <- ggplot2::ggplot(serie_q4_comparada, ggplot2::aes(x = ANIO, y = base_resultado, color = muestra)) +
  ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = 2015:2019) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(title = "Q4: produccion/ventas, con vs. sin firmas atipicas", x = "Anio", y = "Produccion/ventas (promedio)", color = NULL) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(file.path(plot_dir, "preliminar_tendencias_q4_con_sin_atipicas_empleo.png"), plot_empleo, width = 9, height = 6, dpi = 180)
ggplot2::ggsave(file.path(plot_dir, "preliminar_tendencias_q4_con_sin_atipicas_produccion.png"), plot_produccion, width = 9, height = 6, dpi = 180)

# ------------------------------------------------------------------
# PASO 2: concentracion sectorial (CIIU4) entre las firmas atipicas
# ------------------------------------------------------------------

sector_atipicas <- panel_built %>%
  dplyr::filter(NORDEMP %in% nordemp_atipicas, ANIO == 2017) %>%
  dplyr::count(CIIU4, name = "n_atipicas") %>%
  dplyr::mutate(pct_atipicas = round(100 * n_atipicas / sum(n_atipicas), 1))

sector_q4_total <- panel_built %>%
  dplyr::filter(quintil_exposure2022_obreros == "Q4 - Alta", ANIO == 2017) %>%
  dplyr::count(CIIU4, name = "n_q4_total") %>%
  dplyr::mutate(pct_q4_total = round(100 * n_q4_total / sum(n_q4_total), 1))

comparacion_sectorial <- sector_atipicas %>%
  dplyr::full_join(sector_q4_total, by = "CIIU4") %>%
  dplyr::mutate(dplyr::across(c(n_atipicas, n_q4_total, pct_atipicas, pct_q4_total), ~tidyr::replace_na(.x, 0))) %>%
  dplyr::arrange(dplyr::desc(pct_atipicas))

readr::write_csv(comparacion_sectorial, file.path(data_dir, "divergencia_sector_q4.csv"))

# ------------------------------------------------------------------
# PASO 3: test F formal de pre-tendencias, 2015-2019, con efectos fijos
# de firma: Y_ft ~ quintil:anio_lineal + FE_firma (quintil principal se
# absorbe en el FE de firma por ser invariante en el tiempo).
# ------------------------------------------------------------------

panel_regresion <- panel_built %>%
  dplyr::filter(!is.na(quintil_exposure2022_obreros)) %>%
  dplyr::mutate(
    anio_lineal = ANIO - 2015,
    quintil_exposure2022_obreros = factor(quintil_exposure2022_obreros)
  )

QUINTIL_REFERENCIA <- "Q1 - Muy baja"

correr_test_f <- function(data, var_y, etiqueta_muestra) {
  # anio_lineal (termino principal) = pendiente del quintil de referencia
  # (Q1). i(quintil, anio_lineal, ref=Q1) = DIFERENCIA de pendiente de cada
  # quintil respecto a Q1. El FE de firma absorbe el efecto principal de
  # quintil (invariante en el tiempo), no su interaccion con anio_lineal.
  # La hipotesis nula de pre-tendencias paralelas es que TODAS las
  # diferencias de pendiente (los coeficientes i()) son cero conjuntamente.
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ anio_lineal + i(quintil_exposure2022_obreros, anio_lineal, ref = '", QUINTIL_REFERENCIA, "') | NORDEMP"
  ))
  modelo <- fixest::feols(formula_modelo, data = data, warn = FALSE, notes = FALSE)

  # keep = nombre de la variable de quintil: matchea SOLO los coeficientes
  # de interaccion generados por i() (su nombre incluye "quintil_..."),
  # sin incluir el termino principal "anio_lineal" (pendiente de Q1), que
  # no debe ser parte de la hipotesis nula.
  comparacion <- fixest::wald(modelo, keep = "quintil_exposure2022_obreros", print = FALSE)

  tibble::tibble(
    variable = var_y,
    muestra = etiqueta_muestra,
    n_obs = stats::nobs(modelo),
    n_firmas = dplyr::n_distinct(data$NORDEMP[!is.na(data[[var_y]])]),
    f_stat = round(comparacion$stat, 3),
    df1 = comparacion$df1,
    df2 = comparacion$df2,
    p_value = signif(comparacion$p, 4)
  )
}

datos_completos <- panel_regresion
datos_sin_atipicas <- panel_regresion %>% dplyr::filter(!NORDEMP %in% nordemp_atipicas)

variables_test <- c("empleo_total", "base_resultado", "costo_laboral_total")

resultados_test_f <- dplyr::bind_rows(
  purrr::map_dfr(variables_test, ~correr_test_f(datos_completos, .x, "Todas las firmas")),
  purrr::map_dfr(variables_test, ~correr_test_f(datos_sin_atipicas, .x, "Excluyendo firmas atipicas del Paso 1"))
)

readr::write_csv(resultados_test_f, file.path(data_dir, "divergencia_test_f_pretendencias.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Investigacion de divergencia de pre-tendencias 2018-2019")

message("PASO 1: top firmas Q4 por cambio absoluto 2017->2019 exportadas en divergencia_firmas_q4_top.csv")
message("Firmas atipicas identificadas (union empleo+produccion): ", length(nordemp_atipicas))
message("")
message("Serie Q4 completa vs. sin atipicas:")
print(serie_q4_comparada, n = Inf, width = Inf)

message("")
message("PASO 2: distribucion sectorial (CIIU4) de firmas atipicas vs. Q4 total:")
print(comparacion_sectorial, n = Inf, width = Inf)

message("")
message("PASO 3: test F conjunto de pre-tendencias diferenciales por quintil:")
print(resultados_test_f, n = Inf, width = Inf)

message("")
message("Tablas exportadas en: ", data_dir)
message("Graficos exportados en: ", plot_dir)
