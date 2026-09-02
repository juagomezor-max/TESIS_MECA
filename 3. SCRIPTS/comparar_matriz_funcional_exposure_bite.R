# comparar_matriz_funcional_exposure_bite.R -- Pipeline simplificado (rama `main`, post-consolidacion).
#
# Responde una objecion metodologica concreta: las 2 validaciones de
# tendencias paralelas 2015-2019 de 04_validaciones.R comparan
# Exposure2022_obreros (continua, event-study) contra Bite2022_obreros
# (quintiles, tendencia lineal) -- formas funcionales Y estadisticos de
# prueba DISTINTOS. Un F conjunto sobre dummies de anio detecta
# desviaciones no lineales que un test de tendencia lineal no ve; un
# test de tendencia lineal tiene mas potencia contra desviaciones
# monotonicas que un F sobre dummies. Esa asimetria por si sola podria
# explicar por que Bite "rechaza mas" que Exposure en participacion_permanente,
# sin que sea una propiedad real de la medida.
#
# Este script corre la MATRIZ 2x2 completa (2 medidas x 2 formas
# funcionales), con los MISMOS efectos fijos y cluster que
# 04_validaciones.R (NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F, cluster=~NORDEMP)
# y el MISMO panel 2015-2019, para que la comparacion Exposure-vs-Bite
# sea posible tambien controlando por forma funcional/estadistico:
#
# 1. Exposure, event-study anio-a-anio        (YA EXISTE, 04_validaciones.R)
# 2. Exposure, quintiles + tendencia lineal    (NUEVO en este script)
# 3. Bite, event-study anio-a-anio             (NUEVO en este script)
# 4. Bite, quintiles + tendencia lineal        (YA EXISTE, 04_validaciones.R)
#
# DERIVADO de 04_validaciones.R (mismo panel, mismas funciones
# `correr_evento_exposure`/`correr_evento_bite`, mismo filtro
# 2015-2019, misma regla de cluster=~NORDEMP sin excepcion) -- las 2
# celdas "NUEVAS" son la MISMA funcion de 04_validaciones.R aplicada a
# la variable-forma-funcional que le faltaba a cada medida, no una
# especificacion nueva.
#
# Nota sobre escala de Bite2022_obreros en la celda 3 (continua): a
# diferencia de Exposure2022_obreros (reescalada a exposicion_10pp para
# interpretabilidad, ver 03_construir_panel.R), Bite2022_obreros se usa
# SIN reescalar -- es un indice de Kaitz (salario minimo / salario
# promedio obrero), no tiene una escala natural de "10pp". Esto NO
# afecta el estadistico F ni el p-valor del test conjunto: un
# reescalamiento lineal de la variable continua reescala los
# coeficientes anio-a-anio pero no cambia el test conjunto de que sean
# cero, que es invariante a la escala.
#
# Salida (versionada, 4. RESULTADOS/Validaciones/):
# - matriz_comparacion_funcional_exposure_bite.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "purrr", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_path <- file.path(data_dir, "panel_analitico_firma_eam.rds")
if (!file.exists(panel_path)) stop("Falta panel_analitico_firma_eam.rds. Corre 03_construir_panel.R primero.")
panel_analitico <- readr::read_rds(panel_path)

metrics_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total", "Empleo total",
  "empleo_permanente", "Empleo permanente",
  "empleo_temporal", "Empleo temporal (directo + agencia)",
  "participacion_permanente", "Participacion de permanentes (%)"
)

panel_pre_2019 <- panel_analitico %>%
  dplyr::filter(ANIO %in% 2015:2019) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(2015:2019)))

# ------------------------------------------------------------------
# Forma funcional 1: event-study continuo anio-a-anio (identica a la
# celda Exposure de 04_validaciones.R, aplicada aqui tambien a Bite).
# ------------------------------------------------------------------

correr_evento_continuo <- function(data, var_y, var_x) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, ", var_x, ", ref = '2015') | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F"
  ))
  fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}

correr_matriz_event_study <- function(data, var_x, medida) {
  data_f <- data %>% dplyr::filter(!is.na(.data[[var_x]]), !is.na(CIIU4), !is.na(DPTO))
  purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
    metric <- metrics_info[i, ]
    modelo <- correr_evento_continuo(data_f, metric$var, var_x)
    prueba_f <- fixest::wald(modelo, keep = "ANIO_F::(2016|2017|2018|2019)", print = FALSE)
    tibble::tibble(
      medida = medida,
      forma_funcional = "continua, event-study anio-a-anio",
      tipo_test = "F conjunto: coeficientes ANIO_F x exposicion (2016-2019) = 0",
      variable = metric$var,
      n_obs = stats::nobs(modelo),
      n_firmas = dplyr::n_distinct(data_f$NORDEMP[!is.na(data_f[[metric$var]])]),
      f_stat = round(prueba_f$stat, 3),
      df1 = prueba_f$df1,
      df2 = round(prueba_f$df2, 1),
      p_value = signif(prueba_f$p, 4)
    )
  })
}

# ------------------------------------------------------------------
# Forma funcional 2: quintiles + tendencia lineal, con controles
# (identica a la celda Bite de 04_validaciones.R, aplicada aqui tambien
# a Exposure).
# ------------------------------------------------------------------

correr_evento_quintil_lineal <- function(data, var_y, var_quintil) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ anio_lineal + i(", var_quintil, ", anio_lineal, ref = 'Q1 - Muy baja') | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F"
  ))
  fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}

correr_matriz_quintil_lineal <- function(data, var_quintil, medida) {
  data_f <- data %>%
    dplyr::filter(!is.na(.data[[var_quintil]]), !is.na(CIIU4), !is.na(DPTO)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(var_quintil), ~ factor(.x)))
  purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
    metric <- metrics_info[i, ]
    modelo <- correr_evento_quintil_lineal(data_f, metric$var, var_quintil)
    prueba_f <- fixest::wald(modelo, keep = var_quintil, print = FALSE)
    tibble::tibble(
      medida = medida,
      forma_funcional = "quintiles, tendencia lineal (anio_lineal)",
      tipo_test = paste0("F conjunto: coeficientes ", var_quintil, " x anio_lineal (Q2-Q5 vs Q1) = 0"),
      variable = metric$var,
      n_obs = stats::nobs(modelo),
      n_firmas = dplyr::n_distinct(data_f$NORDEMP[!is.na(data_f[[metric$var]])]),
      f_stat = round(prueba_f$stat, 3),
      df1 = prueba_f$df1,
      df2 = round(prueba_f$df2, 1),
      p_value = signif(prueba_f$p, 4)
    )
  })
}

# ------------------------------------------------------------------
# Las 4 celdas de la matriz.
# ------------------------------------------------------------------

celda_1_exposure_event <- correr_matriz_event_study(panel_pre_2019, "exposicion_10pp", "Exposure2022_obreros")
celda_2_exposure_quintil <- correr_matriz_quintil_lineal(panel_pre_2019, "quintil_exposure2022_obreros", "Exposure2022_obreros")
celda_3_bite_event <- correr_matriz_event_study(panel_pre_2019, "Bite2022_obreros", "Bite2022_obreros")
celda_4_bite_quintil <- correr_matriz_quintil_lineal(panel_pre_2019, "quintil_bite2022_obreros", "Bite2022_obreros")

matriz <- dplyr::bind_rows(celda_1_exposure_event, celda_2_exposure_quintil, celda_3_bite_event, celda_4_bite_quintil)

readr::write_csv(matriz, file.path(out_dir, "matriz_comparacion_funcional_exposure_bite.csv"))

script_header("comparar_matriz_funcional_exposure_bite.R -- Matriz 2x2 Exposure/Bite x forma funcional (FE y cluster identicos)")
message("")
print(matriz %>% dplyr::select(medida, forma_funcional, variable, f_stat, df1, df2, p_value), n = Inf, width = Inf)
message("")

# ------------------------------------------------------------------
# Pregunta explicita: la divergencia en participacion_permanente,
# ¿sobrevive con la MISMA forma funcional y el MISMO estadistico?
# ------------------------------------------------------------------

pp <- matriz %>% dplyr::filter(variable == "participacion_permanente")
message("=== participacion_permanente, misma forma funcional, ambas medidas ===")
print(pp %>% dplyr::select(medida, forma_funcional, f_stat, p_value), n = Inf, width = Inf)
message("")
message("Tabla exportada en: ", file.path(out_dir, "matriz_comparacion_funcional_exposure_bite.csv"))
