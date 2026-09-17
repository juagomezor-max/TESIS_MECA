# INTERACCION EXPOSURE × BITE
# Ejecuta la construccion original sin editar sus scripts.

# 1. Ubicar el analisis archivado desde la carpeta actual.
ubicar_analisis <- function() {
  carpeta <- normalizePath(getwd(), winslash = "/")
  
  repeat {
    candidatos <- c(
      carpeta,
      file.path(carpeta, "0. ANALISIS INICIAL IA")
    )
    
    for (ruta in candidatos) {
      if (file.exists(file.path(
        ruta, "3. SCRIPTS", "pipeline", "01_construir_base.R"
      ))) return(ruta)
    }
    
    superior <- dirname(carpeta)
    if (superior == carpeta) stop("No se encontro el analisis archivado.")
    carpeta <- superior
  }
}

ejecutar_interaccion <- function() {
  carpeta_inicial <- getwd()
  on.exit(setwd(carpeta_inicial), add = TRUE)
  setwd(ubicar_analisis())
  
  source("3. SCRIPTS/pipeline/_utils_proyecto.R")
  load_project_packages(c("dplyr", "readr", "tibble", "tidyr", "fixest"))
  
  macro_path <- normalizePath(
    "../1. DATOS/5. MACROBASE/macro_base_eam.rds",
    winslash = "/", mustWork = TRUE
  )
  
  auditoria_path <- paste0(
    "4. RESULTADOS/Validaciones/",
    "auditoria_dpto_estabilidad_nordest_casos.csv"
  )
  stopifnot(file.exists(auditoria_path))
  
  # Carpeta exclusiva de este ejercicio.
  carpeta_ejercicio <- file.path(
    getwd(), "1. DATOS", "6. BASES_DERIVADAS", "interaccion_nicolas"
  )
  dir.create(carpeta_ejercicio, recursive = TRUE, showWarnings = FALSE)
  
  # 2. Ejecutar cada script original cambiando solo las rutas en memoria.
  ejecutar_original <- function(archivo) {
    instrucciones <- parse(file = archivo)
    entorno <- new.env(parent = environment())
    
    encontro_paths <- FALSE
    
    for (instruccion in instrucciones) {
      eval(instruccion, envir = entorno)
      
      if (identical(
        instruccion,
        quote(paths <- ensure_project_structure())
      )) {
        entorno$paths$macro_base_eam <- macro_path
        entorno$paths$bases_derivadas_exposicion <- carpeta_ejercicio
        encontro_paths <- TRUE
      }
    }
    
    if (!encontro_paths) stop("No se encontro la asignacion de paths.")
    invisible(NULL)
  }
  
  ejecutar_original("3. SCRIPTS/pipeline/01_construir_base.R")
  ejecutar_original("3. SCRIPTS/pipeline/02_construir_exposicion.R")
  ejecutar_original(
    "3. SCRIPTS/construccion/construir_panel_establecimiento_formal.R"
  )
  
  # 3. Cargar las bases recien generadas.
  panel <- readr::read_rds(file.path(
    carpeta_ejercicio, "panel_establecimiento_formal.rds"
  ))
  
  exposiciones <- readr::read_rds(file.path(
    carpeta_ejercicio, "exposicion_firma_eam.rds"
  )) |>
    dplyr::select(NORDEMP, Exposure2022_obreros, Bite2022_obreros)
  
  stopifnot(!anyDuplicated(exposiciones$NORDEMP))
  stopifnot(!anyDuplicated(panel[c("NORDEST", "ANIO")]))
  
  panel <- panel |>
    dplyr::inner_join(exposiciones, by = "NORDEMP") |>
    dplyr::filter(
      !is.na(NORDEMP),
      is.finite(Exposure2022_obreros),
      is.finite(Bite2022_obreros),
      !is.na(CIIU4),
      !is.na(tamano_empresa),
      !is.na(DPTO_fijo)
    )
  
  firmas <- panel |>
    dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros)
  
  sd_bite <- stats::sd(firmas$Bite2022_obreros)
  stopifnot(is.finite(sd_bite), sd_bite > 0)
  
  panel <- panel |>
    dplyr::mutate(
      post_2023 = as.integer(ANIO >= 2023),
      ANIO_F = factor(ANIO),
      exposicion_10pp = Exposure2022_obreros / 0.1,
      bite_1sd = Bite2022_obreros / sd_bite
    )
  
  cat("\nCorrelacion Exposure-Bite entre firmas:\n")
  print(stats::cor(
    firmas$Exposure2022_obreros,
    firmas$Bite2022_obreros
  ))
  
  # 4. Estimar los cuatro outcomes, sin y con controles.
  terminos <- paste(
    "post_2023:exposicion_10pp",
    "post_2023:bite_1sd",
    "post_2023:exposicion_10pp:bite_1sd",
    sep = " + "
  )
  
  efectos_fijos <- c(
    sin_controles = "NORDEST + ANIO_F",
    con_controles = paste(
      "NORDEST + ANIO_F + CIIU4^ANIO_F",
      "+ tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"
    )
  )
  
  outcomes <- c(
    "empleo_total", "empleo_permanente",
    "empleo_temporal", "participacion_permanente"
  )
  
  modelos <- list()
  resultados <- list()
  
  for (y in outcomes) {
    # Misma muestra para los modelos sin y con controles de cada Y.
    datos_y <- panel[is.finite(panel[[y]]), ]
    
    for (spec in names(efectos_fijos)) {
      nombre <- paste(y, spec, sep = "_")
      
      formula <- stats::as.formula(
        paste(y, "~", terminos, "|", efectos_fijos[[spec]])
      )
      
      modelo <- fixest::feols(
        formula, data = datos_y, cluster = ~NORDEMP
      )
      modelos[[nombre]] <- modelo
      
      ct <- summary(modelo)$coeftable
      ci <- stats::confint(modelo)
      nombres <- rownames(ct)
      
      resultados[[nombre]] <- data.frame(
        outcome = y,
        especificacion = spec,
        termino = nombres,
        coeficiente = ct[, 1],
        error_estandar = ct[, 2],
        p_valor = ct[, 4],
        limite_inferior = ci[nombres, 1],
        limite_superior = ci[nombres, 2],
        n = stats::nobs(modelo),
        row.names = NULL
      )
    }
    
    cat("\n\nRESULTADO:", y, "\n")
    print(fixest::etable(
      modelos[paste(y, names(efectos_fijos), sep = "_")],
      fitstat = ~ n + r2
    ))
  }
  
  # 5. Guardar todos los coeficientes y los modelos del ejercicio.
  readr::write_csv(
    dplyr::bind_rows(resultados),
    file.path(carpeta_ejercicio, "resultados_interaccion.csv")
  )
  
  saveRDS(
    modelos,
    file.path(carpeta_ejercicio, "modelos_interaccion.rds")
  )
  
  message("\nEjercicio guardado en: ", carpeta_ejercicio)
  invisible(modelos)
}

modelos_interaccion <- ejecutar_interaccion()


# Exportar los modelos ya estimados; no vuelve a correr regresiones.
tabla_interaccion <- dplyr::bind_rows(
  lapply(names(modelos_interaccion), function(nombre) {
    
    modelo <- modelos_interaccion[[nombre]]
    resumen <- summary(modelo)
    ct <- resumen$coeftable
    ci <- stats::confint(modelo)
    terminos <- rownames(ct)
    
    # Numero de clusters usado en la matriz de covarianza.
    n_clusters <- attr(resumen$cov.scaled, "G")
    if (is.null(n_clusters)) {
      stop("No se pudo recuperar el numero de clusters de: ", nombre)
    }
    
    con_controles <- endsWith(nombre, "_con_controles")
    
    data.frame(
      outcome = sub("_(sin|con)_controles$", "", nombre),
      especificacion = if (con_controles) {
        "Con sector*anio + tamano*anio + departamento*anio"
      } else {
        "Sin controles"
      },
      term = terminos,
      estimate = round(ct[, 1], 6),
      std.error = round(ct[, 2], 6),
      statistic = round(ct[, 3], 4),
      p.value = signif(ct[, 4], 4),
      conf.low = round(ci[terminos, 1], 6),
      conf.high = round(ci[terminos, 2], 6),
      n_obs = stats::nobs(modelo),
      n_clusters = n_clusters,
      row.names = NULL
    )
  })
)

carpeta_tablas <- file.path(
  ubicar_analisis(), "4. RESULTADOS", "Estimacion_DiD"
)
dir.create(carpeta_tablas, recursive = TRUE, showWarnings = FALSE)

archivo_tabla <- file.path(
  carpeta_tablas, "did_estatico_empleo_interaccion_coeficientes.csv"
)

readr::write_csv(tabla_interaccion, archivo_tabla)
message("Tabla guardada en: ", archivo_tabla)