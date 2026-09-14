# validar_grupos_bite_participacion_con_tendencia.R
#
# VERIFICACION (2026-09-14), motivada por una inconsistencia real que
# el usuario detecto: Bite2022_obreros x participacion_permanente ya
# fue declarado "NO ROBUSTO" en BORRADOR_RESULTADOS.md (seccion 4.2) --
# el coeficiente LINEAL CONTINUO (+1.173, p=0.005) pierde significancia
# y cambia de signo (-0.844, p=0.052) al agregar tendencia lineal
# pre-existente interactuada con exposicion. Pero el analisis por
# GRUPOS de exposure/bite (estimacion/estimar_did_por_grupos_exposicion.R)
# para esa MISMA celda no incluyo ese control, y mostro un patron
# monotonico y significativo en los 4-5 grupos -- posible inconsistencia
# metodologica: dos especificaciones de la MISMA pregunta con distinta
# robustez a tendencia, sin haberlo verificado.
#
# Este script repite el analisis por grupos, SOLO para Bite2022_obreros
# x participacion_permanente, terciles y quintiles, agregando el MISMO
# tipo de control de tendencia lineal pre-existente que ya se uso para
# la version continua (validar_post_controlando_tendencia_lineal.R):
# en vez de anio_lineal:exposicion_continua, aqui es
# i(grupo, anio_lineal, ref=<grupo mas bajo>) -- dummies de GRUPO
# interactuadas con la tendencia lineal, agregadas junto a
# i(grupo, post_2023, ref=<grupo mas bajo>) en la misma especificacion.
#
# NO SE TOCA ninguna otra celda ni conclusion ya cerrada -- este script
# es especifico a esta unica combinacion (medida=Bite, outcome=
# participacion_permanente, ambas particiones).
#
# DERIVADO de (no reimplementado de memoria):
# - Panel, controles, cluster, grupos (terciles/quintiles ya
#   construidos): identico a estimacion/estimar_did_por_grupos_exposicion.R.
# - Logica de agregar tendencia lineal junto al termino de interes:
#   identica a validaciones/validar_post_controlando_tendencia_lineal.R
#   (misma idea, aqui aplicada a dummies de grupo en vez de a la
#   exposicion continua).
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - verificacion_grupos_bite_participacion_con_tendencia.csv

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal <- readr::read_rds(file.path(data_dir, "panel_establecimiento_formal.rds"))
exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))

make_terciles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))
  if (length(valid) < 3 || dplyr::n_distinct(x[valid]) < 3) return(out)
  terc <- dplyr::ntile(x[valid], 3)
  labels <- c("T1 - Baja", "T2 - Media", "T3 - Alta")
  out[valid] <- labels[terc]
  factor(out, levels = labels, ordered = TRUE)
}

exposicion_firma <- exposicion_firma %>%
  dplyr::mutate(tercil_bite2022_obreros = make_terciles(Bite2022_obreros))

datos_bite <- panel_formal %>%
  dplyr::inner_join(
    exposicion_firma %>% dplyr::select(NORDEMP, tercil_bite2022_obreros, quintil_bite2022_obreros) %>%
      dplyr::filter(!is.na(quintil_bite2022_obreros)),
    by = "NORDEMP"
  ) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023))

FE_CON <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"
VAR_Y <- "participacion_permanente"

correr_par <- function(var_grupo, ref_label, particion) {
  # Modelo ORIGINAL (sin tendencia) -- para tener el mismo numero de
  # referencia, recalculado aqui identico al ya reportado.
  f_original <- stats::as.formula(paste0(
    VAR_Y, " ~ i(", var_grupo, ", post_2023, ref = '", ref_label, "') | ", FE_CON
  ))
  m_original <- fixest::feols(f_original, data = datos_bite, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  # Modelo CON TENDENCIA: agrega i(grupo, anio_lineal, ref=mismo grupo).
  f_tendencia <- stats::as.formula(paste0(
    VAR_Y, " ~ i(", var_grupo, ", post_2023, ref = '", ref_label, "') + i(", var_grupo, ", anio_lineal, ref = '", ref_label, "') | ", FE_CON
  ))
  m_tendencia <- fixest::feols(f_tendencia, data = datos_bite, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  ct_orig <- summary(m_original)$coeftable
  ct_tend <- summary(m_tendencia)$coeftable

  # Coeficientes "post" en el modelo con tendencia (excluyendo los de
  # tendencia, que tienen ":anio_lineal" en el nombre).
  filas_post_tend <- grep(":post_2023$", rownames(ct_tend))
  filas_tend_tend <- grep(":anio_lineal$", rownames(ct_tend))

  tibble::tibble(
    particion = particion,
    grupo = sub(paste0("^", var_grupo, "::"), "", sub(":post_2023$", "", rownames(ct_orig))),
    post_estimate_original = round(unname(ct_orig[, 1]), 6),
    post_p_original = signif(unname(ct_orig[, 4]), 4),
    post_estimate_con_tendencia = round(unname(ct_tend[filas_post_tend, 1]), 6),
    post_p_con_tendencia = signif(unname(ct_tend[filas_post_tend, 4]), 4),
    tendencia_estimate = round(unname(ct_tend[filas_tend_tend, 1]), 6),
    tendencia_p = signif(unname(ct_tend[filas_tend_tend, 4]), 4),
    post_significativo_original = unname(ct_orig[, 4]) < 0.05,
    post_significativo_con_tendencia = unname(ct_tend[filas_post_tend, 4]) < 0.05
  )
}

resultado_terciles <- correr_par("tercil_bite2022_obreros", "T1 - Baja", "Terciles")
resultado_quintiles <- correr_par("quintil_bite2022_obreros", "Q1 - Muy baja", "Quintiles")

resultado <- dplyr::bind_rows(resultado_terciles, resultado_quintiles) %>%
  dplyr::mutate(medida = "Bite2022_obreros", variable = "participacion_permanente", .before = 1)

readr::write_csv(resultado, file.path(out_dir, "verificacion_grupos_bite_participacion_con_tendencia.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("validar_grupos_bite_participacion_con_tendencia.R -- Bite x participacion_permanente, por grupo, con y sin tendencia")
message("")
print(resultado, n = Inf, width = Inf)
message("")
message("Monotonicidad ORIGINAL (sin tendencia):")
print(resultado %>% dplyr::group_by(particion) %>% dplyr::summarise(secuencia = paste(round(post_estimate_original,3), collapse=" -> ")))
message("")
message("Monotonicidad CON TENDENCIA:")
print(resultado %>% dplyr::group_by(particion) %>% dplyr::summarise(secuencia = paste(round(post_estimate_con_tendencia,3), collapse=" -> ")))
message("")
message("Tabla exportada en: ", file.path(out_dir, "verificacion_grupos_bite_participacion_con_tendencia.csv"))
