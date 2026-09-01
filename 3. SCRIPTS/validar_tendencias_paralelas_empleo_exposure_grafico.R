# Version GRAFICA de la validacion de tendencias paralelas 2015-2019, en
# las mismas 4 dimensiones de empleo que
# validar_tendencias_paralelas_empleo_bite.R, pero usando
# Exposure2022_obreros (composicion ocupacional, ya winsorizada 1%-99% en
# construir_exposicion_obreros_eam.R) en vez de Bite2022_obreros, con los
# mismos controles de sector (CIIU4) y departamento (DPTO).
#
# Especificacion (estudio de evento, exposicion CONTINUA en vez de
# quintiles, para poder graficar un coeficiente por anio con
# fixest::iplot -- misma logica que usa el compañero en
# "3. SCRIPTS/3. SCRIPTS/construir_base analitica.R", paso 50/51/52):
#
#   Y ~ i(ANIO_F, exposicion_10pp, ref = "2015") | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F
#   cluster = ~NORDEMP
#
# exposicion_10pp = Exposure2022_obreros / 0.1 (cada coeficiente se lee
# como el efecto de 10 puntos porcentuales adicionales de participacion de
# obreros en el empleo total, relativo a 2015). Como el panel es 100%
# pre-choque (2015-2019), TODOS los coeficientes graficados son chequeos
# de pre-tendencia: si son planos y no significativos en 2016-2018,
# soporta el supuesto de tendencias paralelas.
#
# Dimensiones (Y): empleo_total, empleo_permanente, empleo_temporal,
# participacion_permanente -- mismas definiciones y mismo panel base
# (2015-2019, sin exclusion de firmas atipicas) que
# validar_tendencias_paralelas_empleo_bite.R.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - evento_tendencias_2015_2019_empleo_total.png
# - evento_tendencias_2015_2019_empleo_permanente.png
# - evento_tendencias_2015_2019_empleo_temporal.png
# - evento_tendencias_2015_2019_participacion_permanente.png
# - tabla_evento_tendencias_2015_2019_exposure.csv (prueba F conjunta 2016-2019 por dimension)
# - tabla_evento_tendencias_2015_2019_exposure_coeficientes.csv (2026-09-01: coeficientes tidy con SE e IC 95%)
# - tabla_evento_tendencias_2015_2019_exposure_metadatos.csv (2026-09-01: FE/controles/cluster explicitos)

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
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

# ------------------------------------------------------------------
# 1) empleo_permanente / empleo_temporal (identico a
#    validar_tendencias_paralelas_empleo_bite.R).
# ------------------------------------------------------------------

cols_permanente <- c(
  "C4R2C1", "C4R2C2",
  "C4R2C3", "C4R2C4",
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E"
)

cols_temporal <- c(
  "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2",
  "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"
)

cols_necesarias <- unique(c(cols_permanente, cols_temporal))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO", "CIIU4", "DPTO", cols_necesarias))

panel_raw <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = safe_numeric(ANIO)) %>%
  dplyr::mutate(ANIO = as.integer(ANIO)) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO), ANIO %in% 2015:2019) %>%
  dplyr::select(NORDEMP, ANIO, CIIU4, DPTO, dplyr::all_of(cols_necesarias)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_necesarias), safe_numeric))

panel_vinculacion <- panel_raw %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(cols_necesarias), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    CIIU4 = dplyr::first(CIIU4),
    DPTO = dplyr::first(DPTO),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    empleo_permanente = sum_if_exists(., cols_permanente),
    empleo_temporal = sum_if_exists(., cols_temporal)
  ) %>%
  dplyr::select(NORDEMP, ANIO, CIIU4, DPTO, empleo_permanente, empleo_temporal)

# ------------------------------------------------------------------
# 2) empleo_total: reusa empleo_total_categorias.
# ------------------------------------------------------------------

conteo_path <- file.path(data_dir, "conteo_personal_categoria_eam.rds")
if (!file.exists(conteo_path)) stop("Falta conteo_personal_categoria_eam.rds. Corre construir_conteo_personal_categoria_eam.R primero.")

conteo <- readr::read_rds(conteo_path) %>%
  dplyr::select(NORDEMP, ANIO, empleo_total = empleo_total_categorias)

# ------------------------------------------------------------------
# 3) Exposure2022_obreros (continua, ya winsorizada).
# ------------------------------------------------------------------

exposicion_path <- file.path(data_dir, "exposicion_obreros_eam.rds")
if (!file.exists(exposicion_path)) stop("Falta exposicion_obreros_eam.rds. Corre construir_exposicion_obreros_eam.R primero.")

exposicion_baseline <- readr::read_rds(exposicion_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

# ------------------------------------------------------------------
# 4) Panel final.
# ------------------------------------------------------------------

panel_built <- panel_vinculacion %>%
  dplyr::inner_join(conteo, by = c("NORDEMP", "ANIO")) %>%
  dplyr::inner_join(exposicion_baseline, by = "NORDEMP") %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO)) %>%
  dplyr::mutate(
    participacion_permanente = safe_divide(empleo_permanente, empleo_total) * 100,
    exposicion_10pp = Exposure2022_obreros / 0.1,
    ANIO_F = factor(ANIO),
    CIIU4 = factor(CIIU4),
    DPTO = factor(DPTO)
  )

# ------------------------------------------------------------------
# 5) Estudio de evento por dimension: modelo, grafico (iplot) y prueba F
#    conjunta de los coeficientes 2016-2019 (pre-tendencias).
# ------------------------------------------------------------------

metrics_info <- tibble::tribble(
  ~var, ~label, ~filename_stub,
  "empleo_total", "Empleo total", "empleo_total",
  "empleo_permanente", "Empleo permanente", "empleo_permanente",
  "empleo_temporal", "Empleo temporal (directo + agencia)", "empleo_temporal",
  "participacion_permanente", "Participacion de permanentes en el empleo total (%)", "participacion_permanente"
)

correr_evento <- function(data, var_y) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, exposicion_10pp, ref = '2015') | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F"
  ))
  fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}

coeficientes_list <- list()
metadatos_list <- list()

resultados_f <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  modelo <- correr_evento(panel_built, metric$var)

  png(
    file.path(out_dir, paste0("evento_tendencias_2015_2019_", metric$filename_stub, ".png")),
    width = 900, height = 650, res = 130
  )
  fixest::iplot(
    modelo,
    ref.line = 0,
    main = paste0("Estudio de evento (pre-tendencias): ", metric$label),
    xlab = "Anio",
    ylab = "Efecto de +10pp de Exposure2022_obreros (ref. 2015)",
    ci_level = 0.95
  )
  dev.off()

  prueba_f <- fixest::wald(modelo, keep = "ANIO_F::(2016|2017|2018|2019)", print = FALSE)

  # --- Exportacion adicional (NO cambia la especificacion): tabla tidy
  # de coeficientes con SE e IC 95%, y conteo explicito de clusters. ---
  coeficientes_list[[metric$var]] <<- extraer_coeficientes_tidy_fixest(modelo, metric$var, "2015 (ref.)")
  clusters_info <- contar_clusters_fixest(
    modelo, panel_built, "NORDEMP",
    c(metric$var, "ANIO_F", "exposicion_10pp", "CIIU4", "DPTO")
  )
  metadatos_list[[metric$var]] <<- tibble::tibble(
    variable = metric$var,
    efectos_fijos = "NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F",
    controles = "sector (CIIU4) x anio, departamento (DPTO) x anio",
    variable_cluster = "NORDEMP",
    n_clusters = clusters_info$n_clusters,
    n_obs = stats::nobs(modelo),
    n_obs_reconstruido_coincide = clusters_info$coincide_con_modelo,
    ventana = "2015-2019 (pre-choque, NO ampliada)",
    filtro_muestra = "CIIU4 no NA, DPTO no NA; sin exclusion de firmas atipicas",
    variable_exposicion = "Exposure2022_obreros (firma), continua, escalada a 10pp"
  )

  tibble::tibble(
    variable = metric$var,
    n_obs = stats::nobs(modelo),
    n_firmas = dplyr::n_distinct(panel_built$NORDEMP[!is.na(panel_built[[metric$var]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4),
    coeficientes_en_test = paste(grep("ANIO_F::(2016|2017|2018|2019)", names(stats::coef(modelo)), value = TRUE), collapse = "; ")
  )
})

readr::write_csv(resultados_f, file.path(out_dir, "tabla_evento_tendencias_2015_2019_exposure.csv"))

coeficientes_tabla <- dplyr::bind_rows(coeficientes_list)
metadatos_tabla <- dplyr::bind_rows(metadatos_list)
readr::write_csv(coeficientes_tabla, file.path(out_dir, "tabla_evento_tendencias_2015_2019_exposure_coeficientes.csv"))
readr::write_csv(metadatos_tabla, file.path(out_dir, "tabla_evento_tendencias_2015_2019_exposure_metadatos.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Estudio de evento grafico: tendencias paralelas 2015-2019 (Exposure2022_obreros)")
message("Panel: ", nrow(panel_built), " filas NORDEMP-ANIO, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("Prueba F conjunta de los coeficientes 2016-2019 (relativo a 2015):")
print(resultados_f, n = Inf, width = Inf)
message("")
message("Graficos y tabla exportados en: ", out_dir)
