# Validacion INDEPENDIENTE de tendencias paralelas 2015-2019, en 4
# dimensiones de empleo, usando Bite2022_obreros (indice de Kaitz) como
# medida de exposicion en vez de Exposure2022_obreros (composicion
# ocupacional), con controles de sector (CIIU4) y departamento (DPTO).
#
# Dimensiones evaluadas (Y):
# 1. empleo_total          -- empleo_total_categorias (obreros + admin +
#                              PT, ya construido y validado contra DANE en
#                              construir_conteo_personal_categoria_eam.R).
# 2. empleo_permanente      -- personal permanente (fila C4R2 del
#                              diccionario DANE), todas las categorias
#                              ocupacionales excepto propietarios.
# 3. empleo_temporal        -- temporal directo (C4R3) + temporal via
#                              agencia (C4R4), todas las categorias
#                              ocupacionales excepto propietarios. NO
#                              incluye aprendices (C4R6): son una figura
#                              contractual distinta (contrato de
#                              aprendizaje), ni permanente ni temporal en
#                              el sentido de estas dos filas del
#                              diccionario, y se deja fuera para no forzar
#                              una clasificacion binaria sin sustento.
# 4. participacion_permanente -- 100 * empleo_permanente / empleo_total.
#
# Bite2022_obreros = SM_2023 anualizado / salario promedio de obreros en
# 2022 (construir_bite_obreros_eam.R). Se discretiza en quintiles con la
# MISMA funcion (make_quintiles, ntile) usada para Exposure2022_obreros en
# construir_exposicion_obreros_eam.R, para mantener el mismo marco de
# comparacion por quintiles y test F conjunto. Q5 = Bite mas alto = mayor
# distancia salarial respecto al SM_2023 = mayor exposicion.
#
# Panel: 2015-2019 (pre-choque), SIN excluir firmas atipicas -- esa
# exclusion (divergencia_firmas_q4_top.csv) fue diagnosticada
# especificamente para el Q4 de Exposure2022_obreros
# (investigar_divergencia_pretendencias_2018_2019.R) y no tiene por que
# aplicar a una medida de exposicion distinta (Bite). Esta validacion se
# corre de forma independiente, sobre el panel completo disponible.
#
# Controles: sector(CIIU4)*anio + departamento(DPTO)*anio, ademas de
# efectos fijos de firma (NORDEMP). Se reporta con y sin estos controles
# para que la comparacion sea transparente (misma logica que
# investigar_validez_test_pretendencias.R, Paso 2).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - validacion_tendencias_paralelas_empleo_bite.csv

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
  if (length(valid) < 5 || dplyr::n_distinct(x[valid]) < 5) {
    message("No hay suficiente variacion para construir quintiles. Se devolvera NA.")
    return(out)
  }
  quint <- dplyr::ntile(x[valid], 5)
  labels <- c("Q1 - Muy baja", "Q2 - Baja", "Q3 - Media", "Q4 - Alta", "Q5 - Muy alta")
  out[valid] <- labels[quint]
  factor(out, levels = labels, ordered = TRUE)
}

# ------------------------------------------------------------------
# 1) Columnas C4R por tipo de vinculacion (todas las categorias
#    ocupacionales excepto propietarios), confirmadas estables 2008-2024
#    en verificar_estabilidad_columnas_c3r_c4r.R.
# ------------------------------------------------------------------

cols_permanente <- c(
  "C4R2C1", "C4R2C2",                             # Obreros (M/H)
  "C4R2C3", "C4R2C4",                             # Administrativos (M/H)
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E"      # Prof-tecnico (nac/ext)
)

cols_temporal <- c(
  "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2",         # Obreros: directo + agencia
  "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4",         # Admin: directo + agencia
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",     # PT directo (nac/ext)
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"      # PT agencia (nac/ext)
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
# 2) empleo_total: reusa empleo_total_categorias, ya construido y
#    validado (Paso 3 de la rama feature/exposicion-obreros-operarios).
# ------------------------------------------------------------------

conteo_path <- file.path(data_dir, "conteo_personal_categoria_eam.rds")
if (!file.exists(conteo_path)) stop("Falta conteo_personal_categoria_eam.rds. Corre construir_conteo_personal_categoria_eam.R primero.")

conteo <- readr::read_rds(conteo_path) %>%
  dplyr::select(NORDEMP, ANIO, empleo_total = empleo_total_categorias)

# ------------------------------------------------------------------
# 3) Bite2022_obreros: quintiles (mismo criterio que
#    quintil_exposure2022_obreros).
# ------------------------------------------------------------------

bite_path <- file.path(data_dir, "bite_obreros_eam.rds")
if (!file.exists(bite_path)) stop("Falta bite_obreros_eam.rds. Corre construir_bite_obreros_eam.R primero.")

bite_baseline <- readr::read_rds(bite_path) %>%
  dplyr::distinct(NORDEMP, Bite2022_obreros) %>%
  dplyr::filter(!is.na(Bite2022_obreros)) %>%
  dplyr::mutate(quintil_bite2022_obreros = make_quintiles(Bite2022_obreros))

# ------------------------------------------------------------------
# 4) Panel final: empleo_total + empleo_permanente + empleo_temporal +
#    participacion_permanente, con quintil de Bite y controles.
# ------------------------------------------------------------------

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

correr_test_f <- function(data, var_y, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEMP" else paste("NORDEMP", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ anio_lineal + i(quintil_bite2022_obreros, anio_lineal, ref = '", QUINTIL_REFERENCIA, "') | ", fe
  ))
  modelo <- fixest::feols(formula_modelo, data = data, warn = FALSE, notes = FALSE)
  comparacion <- fixest::wald(modelo, keep = "quintil_bite2022_obreros", print = FALSE)
  tibble::tibble(
    variable = var_y,
    especificacion = etiqueta,
    n_obs = stats::nobs(modelo),
    n_firmas = dplyr::n_distinct(data$NORDEMP[!is.na(data[[var_y]])]),
    f_stat = round(comparacion$stat, 3),
    df1 = comparacion$df1,
    df2 = round(comparacion$df2, 1),
    p_value = signif(comparacion$p, 4)
  )
}

variables_y <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")

resultado <- purrr::map_dfr(variables_y, function(var_y) {
  dplyr::bind_rows(
    correr_test_f(panel_built, var_y, "Sin controles"),
    correr_test_f(panel_built, var_y, "Con sector*anio + departamento*anio", "CIIU4^ANIO_F + DPTO^ANIO_F")
  )
})

readr::write_csv(resultado, file.path(out_dir, "validacion_tendencias_paralelas_empleo_bite.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Validacion de tendencias paralelas 2015-2019 (Bite2022_obreros)")
message("Panel: ", nrow(panel_built), " filas NORDEMP-ANIO, ", dplyr::n_distinct(panel_built$NORDEMP), " firmas.")
message("")
print(resultado, n = Inf, width = Inf)
message("")
message("Tabla exportada en: ", file.path(out_dir, "validacion_tendencias_paralelas_empleo_bite.csv"))
