# Comparacion de inferencia IID vs. clusterizada por NORDEMP para
# validar_tendencias_paralelas_empleo_bite.R. Hallazgo del 2026-09-01
# (INDICE_RESULTADOS.md, fila 28): ese script usa errores estandar IID
# por defecto de fixest (no especifica cluster=), a diferencia de los
# otros 2 scripts de tendencias paralelas (Exposure2022_obreros a nivel
# firma y establecimiento), que si clusterizan por NORDEMP.
#
# MISMA especificacion, mismos efectos fijos, mismos controles, misma
# muestra, misma ventana 2015-2019 que validar_tendencias_paralelas_empleo_bite.R.
# La UNICA diferencia entre las dos columnas de este script es la
# inferencia (IID vs. cluster ~NORDEMP) -- ningun otro elemento cambia.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - comparacion_inferencia_iid_vs_cluster_bite_ftest.csv
# - comparacion_inferencia_iid_vs_cluster_bite_coeficientes.csv

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

make_quintiles <- function(x) {
  out <- rep(NA_character_, length(x))
  valid <- which(!is.na(x))
  if (length(valid) < 5 || dplyr::n_distinct(x[valid]) < 5) return(out)
  quint <- dplyr::ntile(x[valid], 5)
  labels <- c("Q1 - Muy baja", "Q2 - Baja", "Q3 - Media", "Q4 - Alta", "Q5 - Muy alta")
  out[valid] <- labels[quint]
  factor(out, levels = labels, ordered = TRUE)
}

# ------------------------------------------------------------------
# Reconstruccion IDENTICA del panel de validar_tendencias_paralelas_empleo_bite.R.
# ------------------------------------------------------------------

cols_permanente <- c("C4R2C1", "C4R2C2", "C4R2C3", "C4R2C4", "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E")
cols_temporal <- c(
  "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E", "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"
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

conteo_path <- file.path(data_dir, "conteo_personal_categoria_eam.rds")
if (!file.exists(conteo_path)) stop("Falta conteo_personal_categoria_eam.rds.")
conteo <- readr::read_rds(conteo_path) %>% dplyr::select(NORDEMP, ANIO, empleo_total = empleo_total_categorias)

bite_path <- file.path(data_dir, "bite_obreros_eam.rds")
if (!file.exists(bite_path)) stop("Falta bite_obreros_eam.rds.")
bite_baseline <- readr::read_rds(bite_path) %>%
  dplyr::distinct(NORDEMP, Bite2022_obreros) %>%
  dplyr::filter(!is.na(Bite2022_obreros)) %>%
  dplyr::mutate(quintil_bite2022_obreros = make_quintiles(Bite2022_obreros))

panel_built <- panel_vinculacion %>%
  dplyr::inner_join(conteo, by = c("NORDEMP", "ANIO")) %>%
  dplyr::inner_join(bite_baseline, by = "NORDEMP") %>%
  dplyr::filter(!is.na(quintil_bite2022_obreros), !is.na(CIIU4), !is.na(DPTO)) %>%
  dplyr::mutate(
    participacion_permanente = safe_divide(empleo_permanente, empleo_total) * 100,
    anio_lineal = ANIO - 2015,
    ANIO_F = factor(ANIO),
    CIIU4 = factor(CIIU4),
    DPTO = factor(DPTO)
  )

QUINTIL_REFERENCIA <- "Q1 - Muy baja"

# ------------------------------------------------------------------
# Ajusta el MISMO modelo dos veces: IID (sin cluster=, replica exacta
# del script original) y clusterizado por NORDEMP (unico cambio).
# ------------------------------------------------------------------

ajustar_ambos <- function(data, var_y, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEMP" else paste("NORDEMP", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ anio_lineal + i(quintil_bite2022_obreros, anio_lineal, ref = '", QUINTIL_REFERENCIA, "') | ", fe
  ))

  modelo_iid <- fixest::feols(formula_modelo, data = data, warn = FALSE, notes = FALSE)
  modelo_cluster <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

  wald_iid <- fixest::wald(modelo_iid, keep = "quintil_bite2022_obreros", print = FALSE)
  wald_cluster <- fixest::wald(modelo_cluster, keep = "quintil_bite2022_obreros", print = FALSE)

  fila_f <- tibble::tibble(
    variable = var_y,
    especificacion = etiqueta,
    n_obs = stats::nobs(modelo_iid),
    f_stat_iid = round(wald_iid$stat, 3),
    df1_iid = wald_iid$df1,
    df2_iid = round(wald_iid$df2, 1),
    p_value_iid = signif(wald_iid$p, 4),
    f_stat_cluster = round(wald_cluster$stat, 3),
    df1_cluster = wald_cluster$df1,
    df2_cluster = round(wald_cluster$df2, 1),
    p_value_cluster = signif(wald_cluster$p, 4),
    rechaza_al_5pct_iid = wald_iid$p < 0.05,
    rechaza_al_5pct_cluster = wald_cluster$p < 0.05,
    conclusion_cambia = (wald_iid$p < 0.05) != (wald_cluster$p < 0.05)
  )

  ct_iid <- summary(modelo_iid)$coeftable
  ct_cluster <- summary(modelo_cluster)$coeftable
  terms_comunes <- intersect(rownames(ct_iid), rownames(ct_cluster))
  filas_coef <- tibble::tibble(
    variable = var_y,
    especificacion = etiqueta,
    term = terms_comunes,
    estimate = round(unname(ct_iid[terms_comunes, 1]), 6),
    std_error_iid = round(unname(ct_iid[terms_comunes, 2]), 6),
    p_value_iid = signif(unname(ct_iid[terms_comunes, 4]), 4),
    std_error_cluster = round(unname(ct_cluster[terms_comunes, 2]), 6),
    p_value_cluster = signif(unname(ct_cluster[terms_comunes, 4]), 4),
    razon_se_cluster_sobre_iid = round(unname(ct_cluster[terms_comunes, 2]) / unname(ct_iid[terms_comunes, 2]), 3)
  )

  list(f = fila_f, coef = filas_coef)
}

variables_y <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")

resultados <- purrr::map(variables_y, function(var_y) {
  list(
    ajustar_ambos(panel_built, var_y, "Sin controles"),
    ajustar_ambos(panel_built, var_y, "Con sector*anio + departamento*anio", "CIIU4^ANIO_F + DPTO^ANIO_F")
  )
}) %>% purrr::flatten()

tabla_f <- purrr::map_dfr(resultados, "f")
tabla_coef <- purrr::map_dfr(resultados, "coef")

readr::write_csv(tabla_f, file.path(out_dir, "comparacion_inferencia_iid_vs_cluster_bite_ftest.csv"))
readr::write_csv(tabla_coef, file.path(out_dir, "comparacion_inferencia_iid_vs_cluster_bite_coeficientes.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Comparacion de inferencia: IID vs. clusterizado por NORDEMP (Bite2022_obreros)")
message("")
print(tabla_f, n = Inf, width = Inf)
message("")
message("Filas donde la conclusion de rechazo/no-rechazo al 5% CAMBIA entre IID y cluster:")
print(tabla_f %>% dplyr::filter(conclusion_cambia), n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
