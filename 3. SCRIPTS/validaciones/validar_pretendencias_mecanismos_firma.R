# validar_pretendencias_mecanismos_firma.R
#
# Paso 2 de la linea "mecanismos de ajuste" (empresa-anio, panel_analitico_
# firma_eam.rds -- ya adoptado como el panel OFICIAL de la especificacion
# principal, ver BORRADOR_RESULTADOS.md seccion 0 / comparar_especificacion_
# principal_firma_vs_establecimiento.R). Chequeo de tendencias paralelas
# 2015-2019 (event study, ref='2015', prueba F conjunta 2016-2019) para
# las 4 variables de mecanismo que pasaron el inventario de cobertura
# (Paso 1): mantenimiento (C3R23C3), subcontratacion (C3R41C3, entra con
# advertencia de masa en cero 72.2%), inversion (C7R10C2), ventas
# (VALORVEN). NO se incluye Delta empleo (decision explicita del usuario:
# queda solo como limitacion documentada, no como variable de mecanismo).
#
# IMPORTANTE -- esta NO es una repeticion redundante de "8 dimensiones"
# (validaciones/validar_pretendencias_panel_formal.R): ese script corre
# sobre panel_establecimiento_formal.rds (NORDEST-ANIO, establecimiento),
# confirmado explicitamente releyendo su codigo -- carga panel_
# establecimiento_formal.rds en la linea 74 y usa FE_NORDEST. Nunca se
# corrio esta validacion sobre el panel de firma genuino. Se repite aqui
# desde cero, no se reusan sus numeros.
#
# ESPECIFICACION (ambas medidas de exposicion, sin y con controles):
#   Sin controles:  Y ~ i(ANIO_F, X_10pp, ref='2015') | NORDEMP
#   Con controles:  Y ~ i(ANIO_F, X_10pp, ref='2015') | NORDEMP + CIIU4^ANIO_F + tamano_empresa^ANIO_F
#   cluster = ~NORDEMP en ambas. SOLO 2 controles (sector*anio + tamano*anio)
#   -- instruccion explicita del usuario para esta linea de mecanismos, NO
#   incluye departamento*anio (evita ademas la limitacion conocida de
#   DPTO crudo vs. DPTO_fijo documentada en la seccion 0 de BORRADOR_RESULTADOS.md).
#
# DERIVADO de (no reimplementado de memoria):
# - Forma funcional del event-study y prueba F conjunta: IDENTICA a
#   validaciones/validar_pretendencias_panel_formal.R (funcion correr_evento).
# - Regla de agregacion NORDEMP-ANIO (suma si hay al menos 1 no-NA):
#   IDENTICA a pipeline/01_construir_base.R.
# - asinh() para las 4 variables (no log, admite ceros): IDENTICO patron
#   a validar_pretendencias_panel_formal.R.
# - bite_1sd: SD de Bite2022_obreros recalculada de exposicion_firma_eam.rds,
#   IDENTICO patron ya usado en toda estimacion de Bite en este repositorio.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - pretendencias_mecanismos_firma_ftest.csv (F conjunto, 4 vars x 2 medidas x 2 espec = 16 filas)
# - pretendencias_mecanismos_firma_coeficientes.csv (coeficientes tidy)
# - pretendencias_mecanismos_firma_metadatos.csv (FE/controles/cluster/N explicitos)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# ------------------------------------------------------------------
# 1) Panel de firma 2015-2019 + Bite escalado + variables de mecanismo
#    agregadas a NORDEMP-ANIO (regla ya auditada).
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path(data_dir, "panel_analitico_firma_eam.rds")) %>%
  dplyr::filter(ANIO %in% 2015:2019) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(2015:2019)))

exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))
bite_firma <- exposicion_firma %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

cols_mecanismo <- c("C3R23C3", "C3R41C3", "C7R10C2", "VALORVEN")

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO", cols_mecanismo))

mecanismo_firma <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO), ANIO %in% 2015:2019) %>%
  dplyr::select(NORDEMP, ANIO, dplyr::all_of(cols_mecanismo)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_mecanismo), safe_numeric)) %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(cols_mecanismo), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)), .groups = "drop")

panel_built <- panel_firma %>%
  dplyr::left_join(mecanismo_firma, by = c("NORDEMP", "ANIO")) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    asinh_C3R23C3 = asinh(C3R23C3), asinh_C3R41C3 = asinh(C3R41C3),
    asinh_C7R10C2 = asinh(C7R10C2), asinh_VALORVEN = asinh(VALORVEN),
    bite_1sd = Bite2022_obreros / sd_bite_muestra
  )

datos_exposure <- panel_built %>% dplyr::filter(!is.na(Exposure2022_obreros))
datos_bite <- panel_built %>% dplyr::filter(!is.na(bite_1sd))

# ------------------------------------------------------------------
# 2) Dimensiones x medidas.
# ------------------------------------------------------------------

metrics_info <- tibble::tribble(
  ~var,               ~label,
  "asinh_C3R23C3",  "asinh(mantenimiento, reparaciones, accesorios y repuestos)",
  "asinh_C3R41C3",  "asinh(outsourcing / servicios contratados con terceros)",
  "asinh_C7R10C2",  "asinh(total inversiones en activos fijos)",
  "asinh_VALORVEN", "asinh(valor de las ventas)"
)

medidas_info <- tibble::tribble(
  ~medida,                    ~var_x,            ~data_ref,
  "Exposure2022_obreros", "exposicion_10pp", "exposure",
  "Bite2022_obreros",     "bite_1sd",        "bite"
)

# ------------------------------------------------------------------
# 3) Estudio de evento, sin y con controles, misma muestra filtrada por
#    medida.
# ------------------------------------------------------------------

coeficientes_list <- list()
metadatos_list <- list()

correr_evento <- function(data, var_y, var_x, medida, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEMP" else paste("NORDEMP", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, ", var_x, ", ref = '2015') | ", fe
  ))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  prueba_f <- fixest::wald(modelo, keep = "ANIO_F::(2016|2017|2018|2019)", print = FALSE)

  clave <- paste(var_y, medida, etiqueta)
  coeficientes_list[[clave]] <<- extraer_coeficientes_tidy_fixest(modelo, var_y, paste0("2015 (ref.); ", medida, "; especificacion: ", etiqueta)) %>%
    dplyr::mutate(medida = medida, .after = variable)

  vars_regresion <- c(var_y, "ANIO_F", var_x, "NORDEMP")
  if (!is.null(fe_adicionales)) vars_regresion <- c(vars_regresion, "CIIU4", "tamano_empresa")
  clusters_info <- contar_clusters_fixest(modelo, data, "NORDEMP", vars_regresion)

  metadatos_list[[clave]] <<- tibble::tibble(
    variable = var_y, medida = medida, especificacion = etiqueta,
    efectos_fijos = fe,
    controles = if (is.null(fe_adicionales)) "Ninguno" else "sector (CIIU4) x anio, tamano_empresa x anio",
    variable_cluster = "NORDEMP", n_clusters = clusters_info$n_clusters, n_obs = stats::nobs(modelo),
    n_obs_reconstruido_coincide = clusters_info$coincide_con_modelo,
    ventana = "2015-2019 (pre-choque, NO ampliada)",
    filtro_muestra = paste0(medida, " no NA, CIIU4 no NA, tamano_empresa no NA; panel de firma (NORDEMP-ANIO)"),
    variable_exposicion = paste0(medida, " (FIRMA), continua, escalada")
  )

  tibble::tibble(
    variable = var_y, medida = medida, especificacion = etiqueta,
    n_obs = stats::nobs(modelo), n_firmas = dplyr::n_distinct(data$NORDEMP[!is.na(data[[var_y]])]),
    f_stat = round(prueba_f$stat, 3), df1 = prueba_f$df1, df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4)
  )
}

resultado_f <- purrr::pmap_dfr(medidas_info, function(medida, var_x, data_ref) {
  data <- if (data_ref == "exposure") datos_exposure else datos_bite
  purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
    metric <- metrics_info[i, ]
    dplyr::bind_rows(
      correr_evento(data, metric$var, var_x, medida, "Sin controles"),
      correr_evento(data, metric$var, var_x, medida, "Con sector*anio + tamano*anio", "CIIU4^ANIO_F + tamano_empresa^ANIO_F")
    )
  })
})

readr::write_csv(resultado_f, file.path(out_dir, "pretendencias_mecanismos_firma_ftest.csv"))

coeficientes_tabla <- dplyr::bind_rows(coeficientes_list)
metadatos_tabla <- dplyr::bind_rows(metadatos_list)
readr::write_csv(coeficientes_tabla, file.path(out_dir, "pretendencias_mecanismos_firma_coeficientes.csv"))
readr::write_csv(metadatos_tabla, file.path(out_dir, "pretendencias_mecanismos_firma_metadatos.csv"))

script_header("validar_pretendencias_mecanismos_firma.R -- Pre-tendencias 2015-2019, 4 variables de mecanismo, panel de FIRMA")
message("")
message("Panel (2015-2019, muestra Exposure): ", nrow(datos_exposure), " filas NORDEMP-ANIO, ", dplyr::n_distinct(datos_exposure$NORDEMP), " firmas.")
message("Panel (2015-2019, muestra Bite): ", nrow(datos_bite), " filas NORDEMP-ANIO, ", dplyr::n_distinct(datos_bite$NORDEMP), " firmas.")
message("")
print(resultado_f, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
