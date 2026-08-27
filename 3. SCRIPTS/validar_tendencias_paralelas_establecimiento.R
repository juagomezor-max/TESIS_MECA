# Validacion de tendencias paralelas 2015-2019 A NIVEL DE ESTABLECIMIENTO
# (NORDEST), no de empresa (NORDEMP) como en
# validar_tendencias_paralelas_empleo_exposure_grafico.R.
#
# Diferencia de diseño, no solo de efecto fijo: TODA la construccion
# (conteo de personal por categoria, empleo_permanente/temporal,
# Exposure2022_obreros) se recalcula al nivel de NORDEST, en vez de
# reusar los artefactos ya construidos a nivel NORDEMP. Motivo: 336
# empresas del panel tienen mas de un establecimiento (confirmado en
# auditar_deduplicacion_nordemp_anio.R); agregarlas a nivel NORDEMP suma
# sus establecimientos, lo que puede enmascarar o mezclar tendencias que
# son en realidad propias de una planta especifica, no de la empresa como
# un todo. Verificado antes de construir: NORDEST-ANIO ya es unico en la
# macrobase para 2015-2019 (0 combinaciones duplicadas) y para 2022, asi
# que NO hace falta agregar/sumar por grupo -- cada fila del panel
# establecimiento-anio es unica de por si.
#
# Exposure2022_obreros_establecimiento = participacion de obreros en el
# empleo total DEL ESTABLECIMIENTO (no de la empresa), medida en 2022,
# winsorizada 1%-99% (mismo criterio que la version a nivel empresa).
#
# Dimensiones (Y), definidas a nivel de establecimiento:
# 1. empleo_total (obreros + administrativos + PT, sin propietarios)
# 2. empleo_permanente (fila C4R2 del diccionario DANE)
# 3. empleo_temporal (C4R3 directo + C4R4 agencia)
# 4. participacion_permanente = 100 * empleo_permanente / empleo_total
#
# Especificacion (estudio de evento, exposicion continua):
#   Y ~ i(ANIO_F, exposicion_10pp, ref = "2015") | NORDEST + CIIU4^ANIO_F + DPTO^ANIO_F
#   cluster = ~NORDEMP  -- no ~NORDEST, porque varios establecimientos de
#   una misma empresa pueden compartir shocks correlacionados (misma
#   gerencia, mismas decisiones salariales); clusterizar por NORDEMP es
#   mas conservador y evita subestimar errores estandar en esos casos.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - evento_tendencias_establecimiento_2015_2019_empleo_total.png
# - evento_tendencias_establecimiento_2015_2019_empleo_permanente.png
# - evento_tendencias_establecimiento_2015_2019_empleo_temporal.png
# - evento_tendencias_establecimiento_2015_2019_participacion_permanente.png
# - tabla_evento_tendencias_establecimiento_2015_2019.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

sum_if_exists <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) return(rep(NA_real_, nrow(data)))
  out <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), safe_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(.sum = rowSums(dplyr::across(dplyr::everything()))) %>%
    dplyr::pull(.sum)
  all_missing <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), ~is.na(safe_numeric(.x)))) %>%
    dplyr::mutate(.all_missing = dplyr::if_all(dplyr::everything(), identity)) %>%
    dplyr::pull(.all_missing)
  out[all_missing] <- NA_real_
  out
}

winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limits <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limits[[1]]), limits[[2]])
}

# ------------------------------------------------------------------
# 1) Columnas C4R por tipo de vinculacion y categoria ocupacional
#    (identicas a construir_conteo_personal_categoria_eam.R y
#    validar_tendencias_paralelas_empleo_bite.R, confirmadas estables
#    2008-2024 en verificar_estabilidad_columnas_c3r_c4r.R).
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)

cols_permanente <- c(
  "C4R2C1", "C4R2C2", "C4R2C3", "C4R2C4",
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E"
)
cols_temporal <- c(
  "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2",
  "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"
)

cols_necesarias <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico, cols_permanente, cols_temporal))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "NORDEST", "ANIO", "CIIU4", "DPTO", cols_necesarias))

base_establecimiento <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    NORDEST = as.character(NORDEST),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO), ANIO %in% c(2015:2019, 2022)) %>%
  dplyr::select(NORDEMP, NORDEST, ANIO, CIIU4, DPTO, dplyr::all_of(cols_necesarias)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_necesarias), safe_numeric))

# Confirmacion: NORDEST-ANIO ya es unico (verificado antes de construir el
# script); si esto alguna vez deja de cumplirse, el analisis debe
# revisarse porque implicaria doble conteo.
n_dup <- base_establecimiento %>% dplyr::count(NORDEST, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup > 0) stop("NORDEST-ANIO ya no es unico (", n_dup, " grupos duplicados). Revisar antes de continuar.")

base_establecimiento <- base_establecimiento %>%
  dplyr::mutate(
    total_obreros = sum_if_exists(., cols_obreros),
    total_administrativos = sum_if_exists(., cols_administrativos),
    total_prof_tecnico = sum_if_exists(., cols_prof_tecnico),
    empleo_total = total_obreros + total_administrativos + total_prof_tecnico,
    empleo_permanente = sum_if_exists(., cols_permanente),
    empleo_temporal = sum_if_exists(., cols_temporal),
    participacion_permanente = safe_divide(empleo_permanente, empleo_total) * 100
  )

# ------------------------------------------------------------------
# 2) Exposure2022_obreros a nivel de ESTABLECIMIENTO (linea base 2022).
# ------------------------------------------------------------------

baseline_2022 <- base_establecimiento %>%
  dplyr::filter(ANIO == 2022) %>%
  dplyr::mutate(participacion_obreros_raw = safe_divide(total_obreros, empleo_total)) %>%
  dplyr::transmute(NORDEST, Exposure2022_obreros_est = winsorize(participacion_obreros_raw))

message("Establecimientos con Exposure2022_obreros_est valida en 2022: ", sum(!is.na(baseline_2022$Exposure2022_obreros_est)))

# ------------------------------------------------------------------
# 3) Panel final 2015-2019.
# ------------------------------------------------------------------

panel_built <- base_establecimiento %>%
  dplyr::filter(ANIO %in% 2015:2019) %>%
  dplyr::inner_join(baseline_2022, by = "NORDEST") %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est), !is.na(CIIU4), !is.na(DPTO)) %>%
  dplyr::mutate(
    exposicion_10pp = Exposure2022_obreros_est / 0.1,
    ANIO_F = factor(ANIO),
    CIIU4 = factor(CIIU4),
    DPTO = factor(DPTO)
  )

# ------------------------------------------------------------------
# 4) Estudio de evento por dimension, con NORDEST como efecto fijo
#    principal (no NORDEMP) y clustering por NORDEMP.
# ------------------------------------------------------------------

metrics_info <- tibble::tribble(
  ~var, ~label, ~filename_stub,
  "empleo_total", "Empleo total", "empleo_total",
  "empleo_permanente", "Empleo permanente", "empleo_permanente",
  "empleo_temporal", "Empleo temporal (directo + agencia)", "empleo_temporal",
  "participacion_permanente", "Participacion de permanentes (%)", "participacion_permanente"
)

correr_evento <- function(data, var_y) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, exposicion_10pp, ref = '2015') | NORDEST + CIIU4^ANIO_F + DPTO^ANIO_F"
  ))
  fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}

resultados_f <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  modelo <- correr_evento(panel_built, metric$var)

  png(
    file.path(out_dir, paste0("evento_tendencias_establecimiento_2015_2019_", metric$filename_stub, ".png")),
    width = 1100, height = 650, res = 130
  )
  par(cex.main = 0.95)
  fixest::iplot(
    modelo,
    ref.line = 0,
    main = metric$label,
    xlab = "Anio",
    ylab = "Efecto de +10pp de exposicion (ref. 2015)",
    ci_level = 0.95
  )
  dev.off()

  prueba_f <- fixest::wald(modelo, keep = "ANIO_F::(2016|2017|2018|2019)", print = FALSE)
  tibble::tibble(
    variable = metric$var,
    n_obs = stats::nobs(modelo),
    n_establecimientos = dplyr::n_distinct(panel_built$NORDEST[!is.na(panel_built[[metric$var]])]),
    n_firmas = dplyr::n_distinct(panel_built$NORDEMP[!is.na(panel_built[[metric$var]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4)
  )
})

readr::write_csv(resultados_f, file.path(out_dir, "tabla_evento_tendencias_establecimiento_2015_2019.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Validacion de tendencias paralelas 2015-2019, nivel ESTABLECIMIENTO")
message("Panel: ", nrow(panel_built), " filas NORDEST-ANIO, ", dplyr::n_distinct(panel_built$NORDEST),
        " establecimientos, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("Prueba F conjunta de los coeficientes 2016-2019 (relativo a 2015):")
print(resultados_f, n = Inf, width = Inf)
message("")
message("Graficos y tabla exportados en: ", out_dir)
