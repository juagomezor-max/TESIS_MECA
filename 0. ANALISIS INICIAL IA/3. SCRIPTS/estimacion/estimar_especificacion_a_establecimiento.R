# estimar_especificacion_a_establecimiento.R
#
# PRIMERA CORRIDA de la Especificacion A de la seccion 4.5 (nivel
# establecimiento, muestra completa mono+multiplanta) -- nunca se habia
# estimado antes, solo se habia validado el panel y las tendencias
# paralelas (validar_pretendencias_panel_formal*.R). A diferencia de la
# ronda de robustez de la especificacion principal, aqui el control de
# tendencia lineal pre-existente y el placebo 2022 se corren DESDE LA
# PRIMERA VEZ, en paralelo, no como auditoria posterior.
#
# *** AVISO METODOLOGICO EXPLICITO (2026-09-14) ***: no existe en este
# repositorio una construccion validada de "Bite2022_obreros_est"
# (version de Bite especifica por establecimiento) -- se busco
# explicitamente y no aparece en ningun script anterior. La instruccion
# la referencia como "ya validada en la seccion 4.5 del contexto", pero
# ese documento (CONTEXTO_DE_LA_TESIS) tampoco existe en este
# repositorio (ya reportado en BORRADOR_RESULTADOS.md). Se usa por lo
# tanto Bite2022_obreros a nivel FIRMA (pipeline/02_construir_exposicion.R),
# unida por NORDEMP -- cada establecimiento hereda el Bite de su firma
# duena, EXACTAMENTE el mismo patron ya usado en estimar_did_principal_
# empleo_bite.R y validar_primer_eslabon_costo_laboral_bite.R. Si existe
# una version de establecimiento ya construida en otro lugar (fuera de
# este repo), este es un cambio de una linea de join.
#
# ESPECIFICACION A:
#   Y_et = a + b1(Post_t x Exposure_e) + b2(Post_t x Exposure_e x Multi_f)
#          + b3(Post_t x Multi_f) + FE_NORDEST + FE_ANIO_F
#          + controles(sector*anio + tamano*anio + departamento*anio) + e_et
#   cluster = ~NORDEMP (firma) SIEMPRE, nunca ~NORDEST.
#   Multi_f: firmas con >1 establecimiento EN 2022 (corte pre-choque) --
#   IDENTICA logica ya validada en pipeline/opcional_establecimiento.R
#   (262 firmas), releida aqui, no rederivada de memoria.
#
# (i)  SIN tendencia: formula de arriba tal cual.
# (ii) CON tendencia: se agregan anio_lineal:Exposure, anio_lineal:
#      Exposure:Multi_f, anio_lineal:Multi_f -- mismo tipo de control ya
#      usado en la especificacion principal (validar_post_controlando_
#      tendencia_lineal.R), extendido a las 3 interacciones.
#
# PLACEBO 2022: mismo panel PRE (2015-2019+2021-2022, sin 2023-2024),
# post_2022 = as.integer(ANIO >= 2022) en vez de post_2023, con y sin
# los 3 controles (NO se agrega tendencia al placebo -- no se pidio).
#
# REGLA DE PARADA (instruccion explicita del usuario, para no repetir
# la ronda larga anterior): un coeficiente que ya sea no significativo
# SIN tendencia se reporta como nulo, sin mas pruebas. Uno que sea
# significativo SIN tendencia pero pierda significancia o cambie de
# signo CON tendencia se marca "no robusto" directamente. SOLO un
# coeficiente significativo en AMBAS versiones activa la bateria
# adicional (cuadratica, leave-one-year-out) -- eso se corre en un
# script separado, solo si aplica.
#
# DERIVADO de (no reimplementado de memoria):
# - Exposure2022_obreros_est: IDENTICA formula/derivacion ya usada en
#   validar_pretendencias_panel_formal_exposure_est.R y validar_primer_
#   eslabon_costo_laboral_exposure_est.R (recalculada fresca, no
#   persistida).
# - Multi_f (262 firmas, corte 2022): IDENTICA logica de pipeline/
#   opcional_establecimiento.R (n_est_por_firma_2022, firmas_262).
# - bite_1sd (escala de Bite): IDENTICA a validar_primer_eslabon_costo_
#   laboral_bite.R.
# - Logica de agregar tendencia lineal: IDENTICA a validar_post_
#   controlando_tendencia_lineal.R, extendida a 3 terminos.
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - especificacion_a_establecimiento_principal.csv (8 celdas x 3 betas x 2 versiones = 48 filas)
# - especificacion_a_establecimiento_placebo2022.csv (8 celdas x 3 betas x 2 versiones = 48 filas)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(panel_formal_path)) stop("Falta panel_establecimiento_formal.rds.")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_firma_eam.rds.")

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

ANIO_BASE <- 2022
outcomes_info <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")

# ------------------------------------------------------------------
# 1) Exposure2022_obreros_est (recalculada fresca, misma formula ya
#    validada).
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_necesarias <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico))

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
  dplyr::transmute(NORDEST, Exposure2022_obreros_est = winsorize(participacion_obreros_raw)) %>%
  dplyr::filter(!is.na(Exposure2022_obreros_est))

# ------------------------------------------------------------------
# 2) Multi_f (262 firmas, corte 2022) -- misma logica de
#    opcional_establecimiento.R.
# ------------------------------------------------------------------

base_2022_multi <- base_establecimiento %>% dplyr::filter(ANIO == ANIO_BASE) %>% dplyr::distinct(NORDEST, NORDEMP)
n_est_por_firma_2022 <- base_2022_multi %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_por_firma_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)
message("Multi_f (2026-09-14, releida de opcional_establecimiento.R): ", length(firmas_262), " firmas (esperado: 262).")

# ------------------------------------------------------------------
# 3) Panel base + las 2 medidas + Multi_f + post_2023.
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path)
bite_firma <- readr::read_rds(exposicion_firma_path) %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

panel_base <- panel_formal %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(
    Multi_f = as.integer(NORDEMP %in% firmas_262),
    post_2023 = as.integer(ANIO >= 2023)
  )

datos_exposure <- panel_base %>%
  dplyr::inner_join(baseline_est_2022, by = "NORDEST") %>%
  dplyr::mutate(exposicion_10pp = Exposure2022_obreros_est / 0.1)

datos_bite <- panel_base %>%
  dplyr::inner_join(bite_firma, by = "NORDEMP") %>%
  dplyr::mutate(bite_1sd = Bite2022_obreros / sd_bite_muestra)

FE_CON <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"

combinaciones <- tibble::tribble(
  ~medida, ~data_ref, ~var_x,
  "Exposure2022_obreros_est", "exposure", "exposicion_10pp",
  "Bite2022_obreros", "bite", "bite_1sd"
)

# ------------------------------------------------------------------
# 4) Estimacion PRINCIPAL: sin y con tendencia, 3 betas cada una.
# ------------------------------------------------------------------

correr_espec_a <- function(data, var_y, var_x, con_tendencia) {
  terms_post <- c(
    paste0("post_2023:", var_x),
    paste0("post_2023:", var_x, ":Multi_f"),
    paste0("post_2023:Multi_f")
  )
  terms_tend <- c(
    paste0("anio_lineal:", var_x),
    paste0("anio_lineal:", var_x, ":Multi_f"),
    paste0("anio_lineal:Multi_f")
  )
  terms <- if (con_tendencia) c(terms_post, terms_tend) else terms_post
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ ", paste(terms, collapse = " + "), " | ", FE_CON))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  ct <- summary(modelo)$coeftable
  purrr::map_dfr(seq_along(terms_post), function(i) {
    termino <- terms_post[i]
    fila <- which(rownames(ct) == termino)
    if (length(fila) != 1) return(tibble::tibble())
    tibble::tibble(
      beta = paste0("beta", i), termino_regresion = termino,
      estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
      p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05
    )
  }) %>% dplyr::mutate(n_obs = stats::nobs(modelo), .after = beta)
}

resultado_principal <- purrr::pmap_dfr(combinaciones, function(medida, data_ref, var_x) {
  data <- if (data_ref == "exposure") datos_exposure else datos_bite
  purrr::map_dfr(outcomes_info, function(var_y) {
    dplyr::bind_rows(
      correr_espec_a(data, var_y, var_x, FALSE) %>% dplyr::mutate(con_tendencia = FALSE, .after = beta),
      correr_espec_a(data, var_y, var_x, TRUE) %>% dplyr::mutate(con_tendencia = TRUE, .after = beta)
    ) %>% dplyr::mutate(medida = medida, variable = var_y, .before = 1)
  })
})

readr::write_csv(resultado_principal, file.path(out_dir, "especificacion_a_establecimiento_principal.csv"))

# ------------------------------------------------------------------
# 5) Placebo 2022: panel PRE, post_2022, con/sin controles.
# ------------------------------------------------------------------

ANIOS_PRE <- c(2015:2019, 2021, 2022)

datos_exposure_pre <- datos_exposure %>% dplyr::filter(ANIO %in% ANIOS_PRE) %>% dplyr::mutate(post_2022 = as.integer(ANIO >= 2022))
datos_bite_pre <- datos_bite %>% dplyr::filter(ANIO %in% ANIOS_PRE) %>% dplyr::mutate(post_2022 = as.integer(ANIO >= 2022))

correr_placebo_a <- function(data, var_y, var_x, con_controles) {
  fe <- if (con_controles) FE_CON else "NORDEST + ANIO_F"
  terms_post <- c(paste0("post_2022:", var_x), paste0("post_2022:", var_x, ":Multi_f"), paste0("post_2022:Multi_f"))
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ ", paste(terms_post, collapse = " + "), " | ", fe))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  ct <- summary(modelo)$coeftable
  purrr::map_dfr(seq_along(terms_post), function(i) {
    termino <- terms_post[i]
    fila <- which(rownames(ct) == termino)
    if (length(fila) != 1) return(tibble::tibble())
    tibble::tibble(
      beta = paste0("beta", i), termino_regresion = termino,
      estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
      p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05
    )
  }) %>% dplyr::mutate(n_obs = stats::nobs(modelo), .after = beta)
}

resultado_placebo <- purrr::pmap_dfr(combinaciones, function(medida, data_ref, var_x) {
  data <- if (data_ref == "exposure") datos_exposure_pre else datos_bite_pre
  purrr::map_dfr(outcomes_info, function(var_y) {
    dplyr::bind_rows(
      correr_placebo_a(data, var_y, var_x, FALSE) %>% dplyr::mutate(con_controles = FALSE, .after = beta),
      correr_placebo_a(data, var_y, var_x, TRUE) %>% dplyr::mutate(con_controles = TRUE, .after = beta)
    ) %>% dplyr::mutate(medida = medida, variable = var_y, .before = 1)
  })
})

readr::write_csv(resultado_placebo, file.path(out_dir, "especificacion_a_establecimiento_placebo2022.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("estimar_especificacion_a_establecimiento.R -- PRIMERA CORRIDA Especificacion A (seccion 4.5), muestra completa")
message("")
message("=== Principal: sin y con tendencia (con controles) ===")
print(resultado_principal, n = Inf, width = Inf)
message("")
message("=== Placebo 2022: sin y con controles ===")
print(resultado_placebo, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
