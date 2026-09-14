# estimar_especificacion_b_establecimiento.R
#
# PRIMERA CORRIDA de la Especificacion B de la seccion 4.5 (dentro de
# firma, SOLO multiplanta) -- nunca se habia estimado antes. Mismo
# aviso metodologico que estimar_especificacion_a_establecimiento.R:
# Bite2022_obreros se usa a nivel FIRMA (no existe una version de
# establecimiento validada en este repositorio), heredada por
# NORDEMP -- ver ese script para el detalle completo del aviso.
#
# ESPECIFICACION B:
#   Y_et = a + b(Post_t x Exposure_e) + FE_NORDEST + FE_(NORDEMP x ANIO_F) + e_et
#   cluster = ~NORDEMP SIEMPRE. Restringida a firmas MULTIPLANTA
#   (Multi_f=1, corte 2022, misma definicion de 262 firmas ya validada).
#   FE_(NORDEMP x ANIO_F) = efecto fijo firma-anio: absorbe CUALQUIER
#   choque especifico de la firma en un anio dado (incluye sector,
#   tamano, departamento si son mayormente fijos dentro de una firma) --
#   por eso esta especificacion NO agrega los 3 controles
#   sector*anio+tamano*anio+depto*anio por separado, serian redundantes
#   con FE_(NORDEMP x ANIO_F) (que es un FE estrictamente mas fino).
#   "Sin controles" aqui = solo FE_NORDEST + FE_ANIO_F (sin firma x
#   anio); "Con controles" = la especificacion completa de arriba.
#
# MUESTRAS:
# - Principal: cohorte BALANCEADA (181 firmas presentes los 9 anios) --
#   misma logica ya validada en pipeline/opcional_establecimiento.R.
# - Secundaria (robustez): las 262 firmas multiplanta SIN balancear.
#
# (i) SIN tendencia, (ii) CON tendencia (+anio_lineal:Exposure) -- mismo
# patron que Especificacion A.
#
# PLACEBO 2022: panel PRE, post_2022 en vez de post_2023, con/sin
# controles (mismo criterio que Especificacion A).
#
# REGLA DE PARADA: identica a estimar_especificacion_a_establecimiento.R.
#
# DERIVADO de (no reimplementado de memoria):
# - Exposure2022_obreros_est, Multi_f (262), cohorte balanceada (181):
#   IDENTICA logica ya usada/validada (ver estimar_especificacion_a_
#   establecimiento.R y pipeline/opcional_establecimiento.R).
#
# Salidas (versionadas, 4. RESULTADOS/Estimacion_DiD/):
# - especificacion_b_establecimiento_principal.csv (8 celdas x 2 muestras x 4 versiones = 64 filas;
#   la 4ta version, sin_controles+con_tendencia, se agrego porque para
#   Bite2022_obreros la version con_controles=TRUE es colineal con el FE
#   NORDEMP^ANIO_F -- ver nota junto a resultado_principal)
# - especificacion_b_establecimiento_placebo2022.csv (8 celdas x 2 muestras x 2 versiones = 32 filas)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- file.path(paths$resultados, "Estimacion_DiD")

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  pmin(pmax(x, limites[[1]]), limites[[2]])
}

ANIO_BASE <- 2022
PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)
outcomes_info <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")

# ------------------------------------------------------------------
# 1) Exposure2022_obreros_est + Multi_f + cohorte balanceada (identico
#    a estimar_especificacion_a_establecimiento.R / opcional_establecimiento.R).
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

base_2022_multi <- base_establecimiento %>% dplyr::filter(ANIO == ANIO_BASE) %>% dplyr::distinct(NORDEST, NORDEMP)
n_est_por_firma_2022 <- base_2022_multi %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_por_firma_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)

base_ventana_est <- base_establecimiento %>% dplyr::filter(ANIO %in% PANEL_ANIOS_FINAL)
n_est_por_firma_anio <- base_ventana_est %>% dplyr::group_by(NORDEMP, ANIO) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
persistencia <- n_est_por_firma_anio %>%
  dplyr::filter(NORDEMP %in% firmas_262) %>%
  tidyr::complete(NORDEMP = firmas_262, ANIO = PANEL_ANIOS_FINAL, fill = list(n_est = 0)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(mantiene_2plus_todos_los_anios = all(n_est >= 2), .groups = "drop")
firmas_181 <- persistencia %>% dplyr::filter(mantiene_2plus_todos_los_anios) %>% dplyr::pull(NORDEMP)

message("Multi_f: ", length(firmas_262), " firmas (esperado 262). Cohorte balanceada: ", length(firmas_181), " firmas (esperado 181).")

# ------------------------------------------------------------------
# 2) Panel base + medidas + post_2023, restringido a multiplanta.
# ------------------------------------------------------------------

panel_formal <- readr::read_rds(panel_formal_path)
bite_firma <- readr::read_rds(exposicion_firma_path) %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::filter(!is.na(Bite2022_obreros))
sd_bite_muestra <- stats::sd(bite_firma$Bite2022_obreros, na.rm = TRUE)

panel_base <- panel_formal %>%
  dplyr::filter(NORDEMP %in% firmas_262) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023))

datos_exposure <- panel_base %>% dplyr::inner_join(baseline_est_2022, by = "NORDEST") %>% dplyr::mutate(exposicion_10pp = Exposure2022_obreros_est / 0.1)
datos_bite <- panel_base %>% dplyr::inner_join(bite_firma, by = "NORDEMP") %>% dplyr::mutate(bite_1sd = Bite2022_obreros / sd_bite_muestra)

combinaciones <- tibble::tribble(
  ~medida, ~data_ref, ~var_x,
  "Exposure2022_obreros_est", "exposure", "exposicion_10pp",
  "Bite2022_obreros", "bite", "bite_1sd"
)
muestras <- tibble::tribble(
  ~muestra, ~firmas_incluidas,
  "Balanceada (181)", list(firmas_181),
  "No balanceada (262)", list(firmas_262)
)

# ------------------------------------------------------------------
# 3) Estimacion PRINCIPAL.
# ------------------------------------------------------------------

correr_espec_b <- function(data, var_y, var_x, con_controles, con_tendencia) {
  fe <- if (con_controles) "NORDEST + NORDEMP^ANIO_F" else "NORDEST + ANIO_F"
  terms <- paste0("post_2023:", var_x)
  if (con_tendencia) terms <- c(terms, paste0("anio_lineal:", var_x))
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ ", paste(terms, collapse = " + "), " | ", fe))
  modelo <- tryCatch(fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE), error = function(e) NULL)
  if (is.null(modelo)) return(tibble::tibble(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_, significativo = NA, n_obs = NA_integer_, nota = "modelo no convergio"))
  ct <- summary(modelo)$coeftable
  termino_post <- paste0("post_2023:", var_x)
  fila <- which(rownames(ct) == termino_post)
  tibble::tibble(
    estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
    p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05,
    n_obs = stats::nobs(modelo), nota = NA_character_
  )
}

resultado_principal <- purrr::pmap_dfr(combinaciones, function(medida, data_ref, var_x) {
  data <- if (data_ref == "exposure") datos_exposure else datos_bite
  purrr::pmap_dfr(muestras, function(muestra, firmas_incluidas) {
    data_muestra <- data %>% dplyr::filter(NORDEMP %in% unlist(firmas_incluidas))
    purrr::map_dfr(outcomes_info, function(var_y) {
      dplyr::bind_rows(
        correr_espec_b(data_muestra, var_y, var_x, FALSE, FALSE) %>% dplyr::mutate(controles = FALSE, con_tendencia = FALSE),
        correr_espec_b(data_muestra, var_y, var_x, TRUE, FALSE) %>% dplyr::mutate(controles = TRUE, con_tendencia = FALSE),
        correr_espec_b(data_muestra, var_y, var_x, TRUE, TRUE) %>% dplyr::mutate(controles = TRUE, con_tendencia = TRUE),
        # NOTA: para Bite2022_obreros (firma) la version con_controles=TRUE
        # (FE NORDEMP^ANIO_F) es mecanicamente colineal -- post:Bite es
        # constante dentro de cada celda firma-anio, exactamente lo que ese
        # FE absorbe. Por eso se agrega esta 4ta version (sin_controles +
        # tendencia) como chequeo de robustez a tendencia NO colineal.
        correr_espec_b(data_muestra, var_y, var_x, FALSE, TRUE) %>% dplyr::mutate(controles = FALSE, con_tendencia = TRUE)
      ) %>% dplyr::mutate(medida = medida, muestra = muestra, variable = var_y, .before = 1)
    })
  })
})

readr::write_csv(resultado_principal, file.path(out_dir, "especificacion_b_establecimiento_principal.csv"))

# ------------------------------------------------------------------
# 4) Placebo 2022.
# ------------------------------------------------------------------

ANIOS_PRE <- c(2015:2019, 2021, 2022)
datos_exposure_pre <- datos_exposure %>% dplyr::filter(ANIO %in% ANIOS_PRE) %>% dplyr::mutate(post_2022 = as.integer(ANIO >= 2022))
datos_bite_pre <- datos_bite %>% dplyr::filter(ANIO %in% ANIOS_PRE) %>% dplyr::mutate(post_2022 = as.integer(ANIO >= 2022))

correr_placebo_b <- function(data, var_y, var_x, con_controles) {
  fe <- if (con_controles) "NORDEST + NORDEMP^ANIO_F" else "NORDEST + ANIO_F"
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ post_2022:", var_x, " | ", fe))
  modelo <- tryCatch(fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE), error = function(e) NULL)
  if (is.null(modelo)) return(tibble::tibble(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_, significativo = NA, n_obs = NA_integer_))
  ct <- summary(modelo)$coeftable
  termino_post <- paste0("post_2022:", var_x)
  fila <- which(rownames(ct) == termino_post)
  tibble::tibble(
    estimate = round(unname(ct[fila, 1]), 6), std_error = round(unname(ct[fila, 2]), 6),
    p_value = signif(unname(ct[fila, 4]), 4), significativo = unname(ct[fila, 4]) < 0.05, n_obs = stats::nobs(modelo)
  )
}

resultado_placebo <- purrr::pmap_dfr(combinaciones, function(medida, data_ref, var_x) {
  data <- if (data_ref == "exposure") datos_exposure_pre else datos_bite_pre
  purrr::pmap_dfr(muestras, function(muestra, firmas_incluidas) {
    data_muestra <- data %>% dplyr::filter(NORDEMP %in% unlist(firmas_incluidas))
    purrr::map_dfr(outcomes_info, function(var_y) {
      dplyr::bind_rows(
        correr_placebo_b(data_muestra, var_y, var_x, FALSE) %>% dplyr::mutate(controles = FALSE),
        correr_placebo_b(data_muestra, var_y, var_x, TRUE) %>% dplyr::mutate(controles = TRUE)
      ) %>% dplyr::mutate(medida = medida, muestra = muestra, variable = var_y, .before = 1)
    })
  })
})

readr::write_csv(resultado_placebo, file.path(out_dir, "especificacion_b_establecimiento_placebo2022.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("estimar_especificacion_b_establecimiento.R -- PRIMERA CORRIDA Especificacion B (seccion 4.5), dentro de firma multiplanta")
message("")
message("=== Principal ===")
print(resultado_principal, n = Inf, width = Inf)
message("")
message("=== Placebo 2022 ===")
print(resultado_placebo, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
