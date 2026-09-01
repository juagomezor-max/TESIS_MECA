# Utilidades compartidas para el flujo reproducible del proyecto.
# Este archivo centraliza:
# - carga/instalacion minima de paquetes
# - construccion de rutas estandar
# - creacion de carpetas esperadas
# - validaciones simples de archivos y columnas

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

load_project_packages <- function(pkgs) {
  install_if_missing(pkgs)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

get_project_paths <- function(root_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  list(
    root = root_dir,
    preparacion = file.path(root_dir, "0. PREPARACION"),
    datos = file.path(root_dir, "1. DATOS"),
    datos_eam = file.path(root_dir, "1. DATOS", "1. EAM"),
    datos_eac = file.path(root_dir, "1. DATOS", "2. EAC"),
    diccionarios = file.path(root_dir, "1. DATOS", "3. DICCIONARIOS"),
    analisis_inicial = file.path(root_dir, "1. DATOS", "4. ANALISIS_INICIAL"),
    macrobase = file.path(root_dir, "1. DATOS", "5. MACROBASE"),
    bases_derivadas = file.path(root_dir, "1. DATOS", "6. BASES_DERIVADAS"),
    bases_derivadas_panel = file.path(root_dir, "1. DATOS", "6. BASES_DERIVADAS", "panel_diagnostico"),
    bases_derivadas_exposicion = file.path(root_dir, "1. DATOS", "6. BASES_DERIVADAS", "descriptivos_exposicion"),
    procesamiento = file.path(root_dir, "2. PROCESAMIENTO"),
    tmp_unzip = file.path(root_dir, "2. PROCESAMIENTO", "_tmp_unzip"),
    tmp_diccionario = file.path(root_dir, "2. PROCESAMIENTO", "_tmp_diccionario"),
    tmp_macrobase = file.path(root_dir, "2. PROCESAMIENTO", "_tmp_macro_base_eam"),
    scripts = file.path(root_dir, "3. SCRIPTS"),
    resultados = file.path(root_dir, "4. RESULTADOS"),
    resultados_panel = file.path(root_dir, "4. RESULTADOS", "panel_diagnostico"),
    resultados_exposicion = file.path(root_dir, "4. RESULTADOS", "descriptivos_exposicion"),
    resultados_validaciones = file.path(root_dir, "4. RESULTADOS", "Validaciones"),
    diccionario_docx = file.path(root_dir, "1. DATOS", "Diccionarios_EAM_EAC.docx"),
    diccionario_maestro = file.path(root_dir, "1. DATOS", "3. DICCIONARIOS", "diccionario_maestro_variables.csv"),
    macro_base_eam = file.path(root_dir, "1. DATOS", "5. MACROBASE", "macro_base_eam.rds")
  )
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

ensure_project_structure <- function(paths = get_project_paths()) {
  dirs <- c(
    paths$preparacion,
    paths$datos,
    paths$datos_eam,
    paths$datos_eac,
    paths$diccionarios,
    paths$analisis_inicial,
    paths$macrobase,
    paths$bases_derivadas,
    paths$bases_derivadas_panel,
    paths$bases_derivadas_exposicion,
    paths$procesamiento,
    paths$tmp_unzip,
    paths$tmp_diccionario,
    paths$tmp_macrobase,
    paths$scripts,
    paths$resultados,
    paths$resultados_panel,
    paths$resultados_exposicion,
    paths$resultados_validaciones
  )

  invisible(lapply(dirs, ensure_dir))
  paths
}

find_existing_path <- function(candidates, label = "archivo") {
  found <- candidates[file.exists(candidates)][1]
  if (is.na(found) || !nzchar(found)) {
    stop(
      "No se encontro ", label, " en rutas candidatas:\n",
      paste0("- ", candidates, collapse = "\n")
    )
  }
  found
}

check_required_vars <- function(data, vars) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Faltan columnas requeridas: ", paste(missing_vars, collapse = ", "))
  }
}

script_header <- function(title) {
  line <- paste(rep("=", nchar(title)), collapse = "")
  message(line)
  message(title)
  message(line)
}

# ------------------------------------------------------------------
# Exportacion (NO estimacion) de resultados de modelos fixest::feols.
# Agregadas 2026-09-01 para que los coeficientes ano-a-ano/quintil-a-
# quintil, sus errores estandar e intervalos de confianza queden
# guardados como tabla, no solo visualizados en el PNG de fixest::iplot.
# No cambian ninguna especificacion, efecto fijo, control, muestra ni
# ventana de los scripts que las usan -- solo extraen y formatean lo que
# el modelo ya estimo.
# ------------------------------------------------------------------

extraer_coeficientes_tidy_fixest <- function(modelo, variable_y, anio_o_grupo_base) {
  ct <- summary(modelo)$coeftable
  ci <- stats::confint(modelo, level = 0.95)
  term <- rownames(ct)
  ci <- ci[term, , drop = FALSE]

  tibble::tibble(
    variable = variable_y,
    term = term,
    estimate = round(unname(ct[, 1]), 6),
    std.error = round(unname(ct[, 2]), 6),
    statistic = round(unname(ct[, 3]), 4),
    p.value = signif(unname(ct[, 4]), 4),
    conf.low = round(unname(ci[, 1]), 6),
    conf.high = round(unname(ci[, 2]), 6),
    grupo_base_omitido = anio_o_grupo_base
  )
}

# Cuenta clusters de forma explicita (no inferida del ojo del lector),
# reconstruyendo la muestra efectivamente usada por el modelo (todas las
# variables de la formula no-NA) y comparando su tamano contra
# stats::nobs(modelo) para detectar si fixest descarto observaciones
# adicionales (ej. por FE singleton) que esta reconstruccion no capturaria --
# si no coinciden, lo marca explicitamente en vez de reportar una cifra
# falsamente precisa.
contar_clusters_fixest <- function(modelo, data, cluster_var, vars_regresion) {
  filas_completas <- data[stats::complete.cases(data[, vars_regresion, drop = FALSE]), , drop = FALSE]
  n_obs_reconstruido <- nrow(filas_completas)
  n_obs_modelo <- stats::nobs(modelo)
  coincide <- n_obs_reconstruido == n_obs_modelo
  list(
    n_clusters = dplyr::n_distinct(filas_completas[[cluster_var]]),
    n_obs_reconstruido = n_obs_reconstruido,
    n_obs_modelo = n_obs_modelo,
    coincide_con_modelo = coincide
  )
}
