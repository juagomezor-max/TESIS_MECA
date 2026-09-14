# validar_pretendencias_panel_formal_exposure_est.R
#
# COPIA de validar_pretendencias_panel_formal.R con UN SOLO CAMBIO: la
# exposicion usada es `Exposure2022_obreros_est` (propia de cada
# ESTABLECIMIENTO, union directa por NORDEST) en vez de
# `Exposure2022_obreros` (de la FIRMA duena, union por NORDEMP). Misma
# ventana (2015-2019), mismos 3 controles con/sin, mismo cluster
# (=~NORDEMP), mismas 8 dimensiones, misma logica de muestra filtrada.
# Se guarda como script SEPARADO -- no sobreescribe la version de firma.
#
# DERIVADO de (no reimplementado de memoria):
# - Todo lo ya citado en validar_pretendencias_panel_formal.R (misma
#   forma funcional, mismo patron sin/con controles, mismos helpers).
# - Formula de Exposure2022_obreros_est (participacion de obreros en el
#   empleo total del establecimiento, ANIO_BASE=2022, winsorizada
#   1%-99%): pipeline/opcional_establecimiento.R, bloque 2 -- ESE
#   script NO persiste `baseline_est_2022` en disco (solo usa el valor
#   en memoria para las tablas de multiplanta/correlacion que si
#   exporta), asi que aqui se recalcula el mismo bloque leyendo la
#   macrobase fresca, con las mismas columnas/formula/ANIO_BASE/
#   winsorize() -- no es una reimplementacion de memoria, es el mismo
#   codigo citado, reejecutado porque no quedo un artefacto que leer.
# - Definicion de "Multi_f" (firma con >1 establecimiento en 2022) para
#   el conteo multiplanta/monoplanta pedido: identica a
#   pipeline/opcional_establecimiento.R, bloque 4.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - validacion_pretendencias_panel_formal_exposure_est.csv
# - validacion_pretendencias_panel_formal_exposure_est_coeficientes.csv
# - validacion_pretendencias_panel_formal_exposure_est_metadatos.csv
# - comparacion_pretendencias_panel_formal_firma_vs_establecimiento.csv (tabla lado a lado, 16 filas)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
if (!file.exists(panel_formal_path)) {
  stop("Falta panel_establecimiento_formal.rds. Corre construccion/construir_panel_establecimiento_formal.R primero (Paso A).")
}
resultado_firma_path <- file.path(out_dir, "validacion_pretendencias_panel_formal.csv")
if (!file.exists(resultado_firma_path)) {
  stop("Falta validacion_pretendencias_panel_formal.csv. Corre validaciones/validar_pretendencias_panel_formal.R primero (version de firma).")
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

ANIO_BASE <- 2022

# ------------------------------------------------------------------
# 1) Exposure2022_obreros_est (propia del establecimiento, ANIO_BASE=2022),
#    recalculada desde la macrobase -- ver nota "DERIVADO de" arriba.
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_mecanismo <- c("C3R23C3", "C3R41C3", "C7R10C2", "VALORVEN")
cols_necesarias <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico, cols_mecanismo))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", cols_necesarias))

base_establecimiento <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, dplyr::all_of(cols_necesarias)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_necesarias), safe_numeric))

baseline_est_2022 <- base_establecimiento %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::mutate(
    total_obreros = rowSums(dplyr::across(dplyr::all_of(cols_obreros)), na.rm = TRUE),
    total_administrativos = rowSums(dplyr::across(dplyr::all_of(cols_administrativos)), na.rm = TRUE),
    total_prof_tecnico = rowSums(dplyr::across(dplyr::all_of(cols_prof_tecnico)), na.rm = TRUE),
    empleo_total_categorias = total_obreros + total_administrativos + total_prof_tecnico,
    participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias)
  ) %>%
  dplyr::transmute(NORDEST, NORDEMP, Exposure2022_obreros_est = winsorize(participacion_obreros_raw)) %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est))

# Multi_f (firmas con >1 establecimiento en 2022) -- para el conteo
# multiplanta/monoplanta pedido, identico a opcional_establecimiento.R.
n_est_por_firma_2022 <- base_establecimiento %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_multiplanta <- n_est_por_firma_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)

# ------------------------------------------------------------------
# 2) Panel formal (Paso A) 2015-2019 + Exposure2022_obreros_est (union
#    DIRECTA por NORDEST) + mecanismo.
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path) %>%
  dplyr::filter(ANIO %in% 2015:2019) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(2015:2019)))

mecanismo_raw <- base_establecimiento %>%
  dplyr::filter(ANIO %in% 2015:2019) %>%
  dplyr::distinct(NORDEST, ANIO, .keep_all = TRUE) %>%
  dplyr::select(NORDEST, ANIO, dplyr::all_of(cols_mecanismo))

panel_built <- panel_formal %>%
  dplyr::inner_join(baseline_est_2022 %>% dplyr::select(NORDEST, Exposure2022_obreros_est), by = "NORDEST") %>%
  dplyr::left_join(mecanismo_raw, by = c("NORDEST", "ANIO")) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    exposicion_10pp = Exposure2022_obreros_est / 0.1,
    asinh_C3R23C3 = asinh(C3R23C3),
    asinh_C3R41C3 = asinh(C3R41C3),
    asinh_C7R10C2 = asinh(C7R10C2),
    asinh_VALORVEN = asinh(VALORVEN),
    es_multiplanta = NORDEMP %in% firmas_multiplanta
  )

# ------------------------------------------------------------------
# 3) Dimensiones (identicas a la version de firma).
# ------------------------------------------------------------------

metrics_info <- tibble::tribble(
  ~var,               ~label,                                                                  ~tipo,
  "empleo_total",             "Empleo total",                                                  "principal",
  "empleo_permanente",        "Empleo permanente",                                              "principal",
  "empleo_temporal",          "Empleo temporal (directo + agencia)",                            "principal",
  "participacion_permanente", "Participacion de permanentes en el empleo total (%)",            "principal",
  "asinh_C3R23C3",            "asinh(mantenimiento, reparaciones, accesorios y repuestos)",     "mecanismo_extension",
  "asinh_C3R41C3",            "asinh(outsourcing / servicios contratados con terceros)",        "mecanismo_extension",
  "asinh_C7R10C2",            "asinh(total inversiones en activos fijos)",                      "mecanismo_extension",
  "asinh_VALORVEN",           "asinh(valor de las ventas)",                                     "mecanismo_extension"
)

# ------------------------------------------------------------------
# 4) Estudio de evento, sin y con controles, misma muestra filtrada.
# ------------------------------------------------------------------

coeficientes_list <- list()
metadatos_list <- list()

correr_evento <- function(data, var_y, tipo, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEST" else paste("NORDEST", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, exposicion_10pp, ref = '2015') | ", fe
  ))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  prueba_f <- fixest::wald(modelo, keep = "ANIO_F::(2016|2017|2018|2019)", print = FALSE)

  clave <- paste(var_y, etiqueta)
  coeficientes_list[[clave]] <<- extraer_coeficientes_tidy_fixest(modelo, var_y, paste0("2015 (ref.); especificacion: ", etiqueta)) %>%
    dplyr::mutate(tipo_dimension = tipo, .after = variable)

  vars_regresion <- c(var_y, "ANIO_F", "exposicion_10pp", "NORDEST")
  if (!is.null(fe_adicionales)) vars_regresion <- c(vars_regresion, "CIIU4", "tamano_empresa", "DPTO_fijo")
  clusters_info <- contar_clusters_fixest(modelo, data, "NORDEMP", vars_regresion)

  metadatos_list[[clave]] <<- tibble::tibble(
    variable = var_y,
    tipo_dimension = tipo,
    especificacion = etiqueta,
    efectos_fijos = fe,
    controles = if (is.null(fe_adicionales)) "Ninguno" else "sector (CIIU4) x anio, tamano_empresa x anio, departamento (DPTO_fijo) x anio",
    variable_cluster = "NORDEMP",
    n_clusters = clusters_info$n_clusters,
    n_obs = stats::nobs(modelo),
    n_obs_reconstruido_coincide = clusters_info$coincide_con_modelo,
    ventana = "2015-2019 (pre-choque, NO ampliada)",
    filtro_muestra = "Exposure2022_obreros_est (establecimiento) no NA, CIIU4 no NA, DPTO_fijo no NA, tamano_empresa no NA; panel formal Paso A (NORDEST-ANIO)",
    variable_exposicion = "Exposure2022_obreros_est (ESTABLECIMIENTO, no firma), continua, escalada a 10pp, unida DIRECTO por NORDEST"
  )

  tibble::tibble(
    variable = var_y,
    tipo_dimension = tipo,
    especificacion = etiqueta,
    n_obs = stats::nobs(modelo),
    n_establecimientos = dplyr::n_distinct(data$NORDEST[!is.na(data[[var_y]])]),
    n_firmas = dplyr::n_distinct(data$NORDEMP[!is.na(data[[var_y]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4)
  )
}

resultado_f_est <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  dplyr::bind_rows(
    correr_evento(panel_built, metric$var, metric$tipo, "Sin controles"),
    correr_evento(panel_built, metric$var, metric$tipo, "Con sector*anio + tamano*anio + departamento*anio",
                   "CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F")
  )
})

readr::write_csv(resultado_f_est, file.path(out_dir, "validacion_pretendencias_panel_formal_exposure_est.csv"))

coeficientes_tabla <- dplyr::bind_rows(coeficientes_list)
metadatos_tabla <- dplyr::bind_rows(metadatos_list)
readr::write_csv(coeficientes_tabla, file.path(out_dir, "validacion_pretendencias_panel_formal_exposure_est_coeficientes.csv"))
readr::write_csv(metadatos_tabla, file.path(out_dir, "validacion_pretendencias_panel_formal_exposure_est_metadatos.csv"))

# ------------------------------------------------------------------
# 5) Tabla de comparacion lado a lado (firma vs. establecimiento), 16 filas.
# ------------------------------------------------------------------

resultado_f_firma <- readr::read_csv(resultado_firma_path, show_col_types = FALSE)

ALFA <- 0.05
comparacion <- resultado_f_firma %>%
  dplyr::select(variable, tipo_dimension, especificacion, f_stat_firma = f_stat, p_value_firma = p_value) %>%
  dplyr::inner_join(
    resultado_f_est %>% dplyr::select(variable, especificacion, f_stat_establecimiento = f_stat, p_value_establecimiento = p_value),
    by = c("variable", "especificacion")
  ) %>%
  dplyr::mutate(
    rechaza_firma = p_value_firma < ALFA,
    rechaza_establecimiento = p_value_establecimiento < ALFA,
    cambia_conclusion = rechaza_firma != rechaza_establecimiento
  )

if (nrow(comparacion) != 16) {
  stop("La tabla de comparacion no tiene 16 filas (tiene ", nrow(comparacion), "). Revisar el join firma vs. establecimiento.")
}

readr::write_csv(comparacion, file.path(out_dir, "comparacion_pretendencias_panel_formal_firma_vs_establecimiento.csv"))

n_cambian <- sum(comparacion$cambia_conclusion)

# ------------------------------------------------------------------
# 6) Correlacion Exposure2022_obreros (firma) vs. Exposure2022_obreros_est
#    (establecimiento) EN LA MUESTRA FILTRADA de esta validacion (no se
#    asume el 0.964 ya reportado sobre la muestra completa de 2022).
# ------------------------------------------------------------------

exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_firma_eam.rds. Corre pipeline/02_construir_exposicion.R primero.")
exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

establecimientos_muestra <- panel_built %>%
  dplyr::distinct(NORDEST, NORDEMP, Exposure2022_obreros_est, es_multiplanta) %>%
  dplyr::inner_join(exposicion_firma, by = "NORDEMP")

correlacion_muestra <- tibble::tibble(
  n_establecimientos = nrow(establecimientos_muestra),
  correlacion_pearson = round(cor(establecimientos_muestra$Exposure2022_obreros_est, establecimientos_muestra$Exposure2022_obreros, method = "pearson"), 4),
  correlacion_spearman = round(cor(establecimientos_muestra$Exposure2022_obreros_est, establecimientos_muestra$Exposure2022_obreros, method = "spearman"), 4)
)
readr::write_csv(correlacion_muestra, file.path(out_dir, "correlacion_exposure_firma_vs_establecimiento_muestra_pretendencias.csv"))

# ------------------------------------------------------------------
# 7) Multiplanta vs. monoplanta en la muestra (donde firma/establecimiento
#    PUEDEN diferir vs. donde coinciden por construccion).
# ------------------------------------------------------------------

resumen_multiplanta <- establecimientos_muestra %>%
  dplyr::count(es_multiplanta) %>%
  dplyr::mutate(grupo = ifelse(es_multiplanta, "Multiplanta (firma con >1 establecimiento en 2022)", "Monoplanta (coincide con la firma por construccion)")) %>%
  dplyr::select(grupo, n_establecimientos = n)
readr::write_csv(resumen_multiplanta, file.path(out_dir, "resumen_multiplanta_muestra_pretendencias.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("validar_pretendencias_panel_formal_exposure_est.R -- Pre-tendencias con Exposure2022_obreros_est (establecimiento)")
message("")
message("Panel (2015-2019, muestra filtrada): ", nrow(panel_built), " filas NORDEST-ANIO, ",
        dplyr::n_distinct(panel_built$NORDEST), " establecimientos, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("=== Comparacion lado a lado (firma vs. establecimiento), 16 filas ===")
print(comparacion %>% dplyr::select(variable, especificacion, f_stat_firma, p_value_firma, f_stat_establecimiento, p_value_establecimiento, cambia_conclusion), n = Inf, width = Inf)
message("")
if (n_cambian == 0) {
  message(">>> NINGUNA de las 16 celdas cambia de conclusion (rechaza vs. no rechaza al 5%) entre firma y establecimiento. <<<")
} else {
  message(">>> ", n_cambian, " de 16 celdas CAMBIAN de conclusion entre firma y establecimiento: <<<")
  print(comparacion %>% dplyr::filter(cambia_conclusion), n = Inf, width = Inf)
}
message("")
message("Correlacion Exposure2022_obreros (firma) vs. Exposure2022_obreros_est (establecimiento), EN ESTA MUESTRA (n=", correlacion_muestra$n_establecimientos, "):")
print(correlacion_muestra, width = Inf)
message("")
message("Multiplanta vs. monoplanta en la muestra:")
print(resumen_multiplanta, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
