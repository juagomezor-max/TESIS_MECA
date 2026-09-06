# validar_pretendencias_panel_formal.R
#
# Estudio de evento 2015-2019 (chequeo de pre-tendencias) sobre el panel
# formal a nivel ESTABLECIMIENTO reconstruido en
# construccion/construir_panel_establecimiento_formal.R (panel FRESCO,
# NO el archivado en el tag `archivo/panel-formal`). Con y SIN los 3
# controles del Paso A.4 (sector*anio + tamano*anio + departamento*anio),
# en 8 dimensiones: las 4 principales de empleo, mas 4 de
# mecanismo/extension.
#
# DERIVADO de (no reimplementado de memoria):
# - Forma funcional del estudio de evento (i(ANIO_F, exposicion_10pp,
#   ref='2015')) y prueba F conjunta 2016-2019: IDENTICA a
#   validaciones/validar_tendencias_paralelas_empleo_exposure_grafico.R.
# - Patron "correr ambas especificaciones (sin/con controles) desde la
#   MISMA muestra filtrada, exportar coeficientes tidy + metadatos
#   explicitos por especificacion": IDENTICO a
#   validaciones/validar_tendencias_paralelas_empleo_bite.R
#   (funcion correr_test_f con parametro fe_adicionales).
# - Helpers extraer_coeficientes_tidy_fixest() / contar_clusters_fixest():
#   pipeline/_utils_proyecto.R, sin cambios.
# - cluster = ~NORDEMP SIEMPRE (regla innegociable del proyecto, ver
#   pipeline/04_validaciones.R): un establecimiento no es una unidad de
#   observacion independiente de las otras plantas de su misma firma.
#
# DECISION EXPLICITA DE ESTA IMPLEMENTACION (no re-abre nada aprobado,
# pero se documenta porque no era la unica lectura posible de la
# instruccion): la exposicion usada es `Exposure2022_obreros` -- la
# version a nivel FIRMA (02_construir_exposicion.R), unida a este panel
# de establecimiento por NORDEMP. NO es `Exposure2022_obreros_est` (la
# version recalculada a nivel de planta que usa
# validar_tendencias_paralelas_establecimiento.R). Cada establecimiento
# hereda el Exposure de la firma que lo posee. Se eligio la lectura
# literal del nombre pedido ("Exposure2022_obreros"); si la intencion
# era la version de establecimiento, el cambio es de una linea (unir
# exposicion_firma_establecimiento_eam.rds por NORDEST en vez de
# exposicion_firma_eam.rds por NORDEMP).
#
# ESPECIFICACION:
#   Sin controles:  Y ~ i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST
#   Con controles:  Y ~ i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F
#   cluster = ~NORDEMP en ambas.
#
# Dimensiones:
#  PRINCIPALES (empleo, outcomes centrales del DiD):
#   empleo_total, empleo_permanente, empleo_temporal, participacion_permanente
#  MECANISMO/EXTENSION (NO son outcomes principales -- ver marca
#   explicita en el header y en 4. RESULTADOS/Validaciones/README.md):
#   asinh(C3R23C3) -- mantenimiento, reparaciones, accesorios y repuestos
#   asinh(C3R41C3) -- outsourcing / servicios contratados con terceros
#   asinh(C7R10C2) -- total inversiones en activos fijos
#   asinh(VALORVEN) -- valor de las ventas
#  Las 4 de mecanismo usan asinh() (no log, admite ceros/negativos) y NO
#  se deflactan: el efecto fijo ANIO_F ya absorbe cualquier tendencia de
#  precios agregada comun a todas las firmas en un anio dado. NO se usa
#  el deflactor del script exploratorio de Nicolas
#  (exploratorio_nicolas/construir_base_analitica_nicolas.R, nunca
#  validado -- ver README_SIMPLIFICACION.md).
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - validacion_pretendencias_panel_formal.csv (F-test conjunto, 16 filas: 8 dimensiones x 2 especificaciones)
# - validacion_pretendencias_panel_formal_coeficientes.csv (coeficientes tidy, SE, IC95%)
# - validacion_pretendencias_panel_formal_metadatos.csv (FE/controles/cluster/N explicitos)

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
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) {
  stop("Falta exposicion_firma_eam.rds. Corre pipeline/02_construir_exposicion.R primero.")
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# ------------------------------------------------------------------
# 1) Panel formal (Paso A), restringido a 2015-2019, mas Exposure2022_obreros
#    (firma) unida por NORDEMP, mas las 4 variables de mecanismo leidas
#    frescas de la macrobase (no estan en el panel formal, que solo
#    tiene las 4 de empleo).
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path) %>%
  dplyr::filter(ANIO %in% 2015:2019) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(2015:2019)))

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::filter(!is.na(Exposure2022_obreros))

cols_mecanismo <- c("C3R23C3", "C3R41C3", "C7R10C2", "VALORVEN")

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "ANIO", cols_mecanismo))

mecanismo_raw <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO), ANIO %in% 2015:2019) %>%
  dplyr::distinct(NORDEST, ANIO, .keep_all = TRUE) %>%
  dplyr::select(NORDEST, ANIO, dplyr::all_of(cols_mecanismo)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_mecanismo), safe_numeric))

panel_built <- panel_formal %>%
  dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
  dplyr::left_join(mecanismo_raw, by = c("NORDEST", "ANIO")) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    exposicion_10pp = Exposure2022_obreros / 0.1,
    asinh_C3R23C3 = asinh(C3R23C3),
    asinh_C3R41C3 = asinh(C3R41C3),
    asinh_C7R10C2 = asinh(C7R10C2),
    asinh_VALORVEN = asinh(VALORVEN)
  )

# ------------------------------------------------------------------
# 2) Dimensiones.
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
# 3) Estudio de evento, sin y con controles, misma muestra filtrada.
#    Patron identico a validar_tendencias_paralelas_empleo_bite.R
#    (correr_test_f con fe_adicionales opcional).
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
    filtro_muestra = "Exposure2022_obreros (firma) no NA, CIIU4 no NA, DPTO_fijo no NA, tamano_empresa no NA; panel formal Paso A (NORDEST-ANIO)",
    variable_exposicion = "Exposure2022_obreros (FIRMA, no _est), continua, escalada a 10pp, unida por NORDEMP"
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

resultado_f <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  dplyr::bind_rows(
    correr_evento(panel_built, metric$var, metric$tipo, "Sin controles"),
    correr_evento(panel_built, metric$var, metric$tipo, "Con sector*anio + tamano*anio + departamento*anio",
                   "CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F")
  )
})

readr::write_csv(resultado_f, file.path(out_dir, "validacion_pretendencias_panel_formal.csv"))

coeficientes_tabla <- dplyr::bind_rows(coeficientes_list)
metadatos_tabla <- dplyr::bind_rows(metadatos_list)
readr::write_csv(coeficientes_tabla, file.path(out_dir, "validacion_pretendencias_panel_formal_coeficientes.csv"))
readr::write_csv(metadatos_tabla, file.path(out_dir, "validacion_pretendencias_panel_formal_metadatos.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("validar_pretendencias_panel_formal.R -- Pre-tendencias 2015-2019 sobre el panel formal FRESCO (establecimiento)")
message("")
message("Panel (2015-2019, muestra filtrada): ", nrow(panel_built), " filas NORDEST-ANIO, ",
        dplyr::n_distinct(panel_built$NORDEST), " establecimientos, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
message("=== PRINCIPALES (empleo) ===")
print(resultado_f %>% dplyr::filter(tipo_dimension == "principal"), n = Inf, width = Inf)
message("")
message("=== MECANISMO/EXTENSION (NO son outcomes principales del DiD) ===")
print(resultado_f %>% dplyr::filter(tipo_dimension == "mecanismo_extension"), n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
