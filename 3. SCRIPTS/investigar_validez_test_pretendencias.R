# Investiga si el rechazo de pre-tendencias paralelas (test F,
# investigar_divergencia_pretendencias_2018_2019.R) es valido o esta
# contaminado por un problema de diseño, antes de decidir como responder.
#
# Paso 1: revisa si costo_laboral_total tiene solapamiento MECANICO con
#         Exposure2022_obreros (misma fuente de datos reusada), y lo prueba
#         empiricamente dividiendo por headcount (salario_promedio).
# Paso 2: repite el test F de empleo_total y produccion agregando controles
#         de sector(CIIU4)*anio y tamano_empresa*anio.
#
# Usa el MISMO panel 2015-2019 sin las 24 firmas atipicas identificadas en
# investigar_divergencia_pretendencias_2018_2019.R, para comparabilidad
# directa con esos resultados (F=3.87/p=0.004 empleo, F=2.68/p=0.030
# produccion, F=18.7/p=2.2e-15 costo_laboral_total).
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/validez_test_f_solapamiento.csv
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/validez_test_f_con_controles.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

# ====================================================================
# PASO 1 (documentacion): columnas fuente de costo_laboral_total vs.
# Exposure2022_obreros.
#
# Exposure2022_obreros (construir_conteo_personal_categoria_eam.R +
# construir_exposicion_obreros_eam.R, ambos ya en main):
#   total_obreros = suma de C4R2C1, C4R2C2, C4R3C1, C4R3C2, C4R4C1,
#                   C4R4C2, C4R6OM, C4R6OH (personal OCUPADO, capitulo
#                   C4R, en PERSONAS).
#   empleo_total_categorias = total_obreros + total_administrativos +
#                              total_prof_tecnico (mismo capitulo C4R).
#   Exposure2022_obreros = total_obreros / empleo_total_categorias.
#
# costo_laboral_total (descriptivo_exposicion_eam.R y las copias usadas
# en diagnostico_preliminar_tendencias_2015_2019.R e
# investigar_divergencia_pretendencias_2018_2019.R):
#   costo_laboral_total = C3R10C3 (preferido) o SALPEYTE+PRESPYTE o
#                         SALARPER+PRESSPER+REMUTEMP -- capitulo C3R,
#                         en MILES DE PESOS.
#   Descripcion oficial DANE de C3R10C3 (verificada contra el
#   diccionario EAM2024 en el Paso 2 de la rama
#   feature/exposicion-obreros-operarios): "Total sueldos, salarios,
#   prestaciones, cotizaciones patronales, aportes, apoyo de
#   sostenimiento y otros gastos" -- es la columna C3 (Total, suma de
#   Obreros + Administrativos + Profesional-tecnico) de la fila R10.
#
# NO hay una misma celda cruda reusada dos veces: Exposure2022_obreros
# viene integramente de C4R (personas) y costo_laboral_total
# integramente de C3R (pesos). No es un caso de "la misma columna en
# ambos lados de la regresion".
#
# PERO hay un canal MECANICO indirecto real: costo_laboral_total (C3R10C3)
# es, por definicion, la suma del gasto en Obreros + Administrativos + PT.
# El componente "Obreros" de ese gasto es (aproximadamente)
# total_obreros x salario_promedio_obrero. Si el NUMERO de obreros de una
# firma (el mismo headcount que tambien determina su quintil de
# Exposure2022_obreros) tiene una tendencia propia 2015-2019 -- crecimiento
# o contraccion de plantilla, sin que cambie ningun salario -- eso ya
# mueve costo_laboral_total mecanicamente, sin que exista ninguna relacion
# economica real entre "exposicion" y "costo laboral" mas alla de que
# ambos comparten el mismo headcount de obreros como insumo.
#
# Prueba empirica: si se divide costo_laboral_total por empleo_total
# (obteniendo salario_promedio, un costo POR PERSONA en vez de un nivel
# agregado), el canal mecanico de headcount se cancela. Si la
# significancia del test F cae fuertemente al pasar de nivel a
# per-capita, confirma que gran parte de la señal en costo_laboral_total
# era mecanica (composicion de headcount), no una diferencia real de
# TASA salarial por quintil.
# ====================================================================

# ------------------------------------------------------------------
# Reconstruccion del panel 2015-2019 sin firmas atipicas (identico al
# usado en investigar_divergencia_pretendencias_2018_2019.R)
# ------------------------------------------------------------------

exposicion_path <- file.path(data_dir, "exposicion_obreros_eam.rds")
if (!file.exists(exposicion_path)) stop("Falta exposicion_obreros_eam.rds.")

quintiles <- readr::read_rds(exposicion_path) %>%
  dplyr::distinct(NORDEMP, quintil_exposure2022_obreros) %>%
  dplyr::filter(!is.na(quintil_exposure2022_obreros))

divergencia_path <- file.path(data_dir, "divergencia_firmas_q4_top.csv")
if (!file.exists(divergencia_path)) stop("Falta divergencia_firmas_q4_top.csv (Paso 1 de investigar_divergencia_pretendencias_2018_2019.R).")
nordemp_atipicas <- readr::read_csv(divergencia_path, show_col_types = FALSE) %>%
  dplyr::pull(NORDEMP) %>%
  unique() %>%
  as.character()

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

sum_if_exists <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) return(rep(NA_real_, nrow(data)))
  out <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), safe_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::coalesce(.x, 0))) %>%
    dplyr::mutate(.sum = rowSums(dplyr::across(dplyr::everything()))) %>%
    dplyr::pull(.sum)
  all_missing <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), ~is.na(safe_numeric(.x)))) %>%
    dplyr::mutate(.all_missing = dplyr::if_all(dplyr::everything(), identity)) %>%
    dplyr::pull(.all_missing)
  out[all_missing] <- NA_real_
  out
}

coalesce_positive <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) return(rep(NA_real_, nrow(data)))
  out <- rep(NA_real_, nrow(data))
  for (var in present) {
    current <- safe_numeric(data[[var]])
    current[current <= 0] <- NA_real_
    out <- dplyr::coalesce(out, current)
  }
  out
}

first_existing_var <- function(data, candidates) {
  match <- candidates[candidates %in% names(data)][1]
  if (is.na(match) || !nzchar(match)) return(NA_character_)
  match
}

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))

component_candidates <- c(
  "PERTOTAL", "PERSOCU", "PERSOESC", "PERTEM3", "PPERYTEM",
  "SALARPER", "PRESSPER", "PRESPYTE", "SALPEYTE", "REMUTEMP",
  "C3R10C3", "VALAGRI", "PRODBIND", "VALORVEN", "VALVFAB"
)
present_components <- component_candidates[component_candidates %in% names(macro_base)]

panel_raw <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = suppressWarnings(as.integer(ANIO))) %>%
  dplyr::filter(
    !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO),
    ANIO %in% 2015:2019,
    NORDEMP %in% quintiles$NORDEMP,
    !NORDEMP %in% nordemp_atipicas
  ) %>%
  dplyr::select(NORDEMP, ANIO, CIIU4, dplyr::all_of(present_components)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(present_components), safe_numeric))

panel <- panel_raw %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(present_components), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    CIIU4 = dplyr::first(CIIU4),
    .groups = "drop"
  )

empleo_var <- first_existing_var(panel, c("PERTOTAL", "PPERYTEM", "PERSOESC"))

panel_built <- panel %>%
  dplyr::mutate(
    empleo_total = if (!is.na(empleo_var)) safe_numeric(.data[[empleo_var]]) else NA_real_,
    costo_laboral_total = if ("C3R10C3" %in% names(.)) {
      safe_numeric(C3R10C3)
    } else if (all(c("SALPEYTE", "PRESPYTE") %in% names(.))) {
      sum_if_exists(., c("SALPEYTE", "PRESPYTE"))
    } else {
      sum_if_exists(., c("SALARPER", "PRESSPER", "REMUTEMP"))
    },
    base_resultado = coalesce_positive(., c("VALAGRI", "PRODBIND", "VALORVEN", "VALVFAB")),
    salario_promedio = safe_divide(costo_laboral_total, empleo_total),
    tamano_empresa = dplyr::case_when(
      is.na(empleo_total) ~ NA_character_,
      empleo_total < 50 ~ "Pequena",
      empleo_total < 200 ~ "Mediana",
      empleo_total >= 200 ~ "Grande",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::left_join(quintiles, by = "NORDEMP") %>%
  dplyr::filter(!is.na(quintil_exposure2022_obreros)) %>%
  dplyr::mutate(
    anio_lineal = ANIO - 2015,
    quintil_exposure2022_obreros = factor(quintil_exposure2022_obreros)
  )

QUINTIL_REFERENCIA <- "Q1 - Muy baja"

correr_test_f <- function(data, var_y, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEMP" else paste("NORDEMP", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ anio_lineal + i(quintil_exposure2022_obreros, anio_lineal, ref = '", QUINTIL_REFERENCIA, "') | ", fe
  ))
  modelo <- fixest::feols(formula_modelo, data = data, warn = FALSE, notes = FALSE)
  comparacion <- fixest::wald(modelo, keep = "quintil_exposure2022_obreros", print = FALSE)
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

# ------------------------------------------------------------------
# PASO 1 (empirico): nivel (costo_laboral_total) vs. per-capita
# (salario_promedio) -- si cae la significancia, confirma el canal
# mecanico de headcount.
# ------------------------------------------------------------------

resultado_paso1 <- dplyr::bind_rows(
  correr_test_f(panel_built, "empleo_total", "Sin controles (referencia)"),
  correr_test_f(panel_built, "costo_laboral_total", "Sin controles (nivel agregado, referencia)"),
  correr_test_f(panel_built, "salario_promedio", "Sin controles (costo POR PERSONA, sin headcount)")
)

readr::write_csv(resultado_paso1, file.path(data_dir, "validez_test_f_solapamiento.csv"))

# ------------------------------------------------------------------
# PASO 2: controles de sector(CIIU4)*anio y tamano_empresa*anio para
# empleo_total y produccion (base_resultado).
# ------------------------------------------------------------------

panel_controles <- panel_built %>%
  dplyr::filter(!is.na(CIIU4), !is.na(tamano_empresa)) %>%
  dplyr::mutate(CIIU4 = factor(CIIU4), tamano_empresa = factor(tamano_empresa), ANIO_F = factor(ANIO))

resultado_paso2 <- dplyr::bind_rows(
  correr_test_f(panel_controles, "empleo_total", "Sin controles adicionales"),
  correr_test_f(panel_controles, "empleo_total", "Con sector*anio + tamano*anio", "CIIU4^ANIO_F + tamano_empresa^ANIO_F"),
  correr_test_f(panel_controles, "base_resultado", "Sin controles adicionales"),
  correr_test_f(panel_controles, "base_resultado", "Con sector*anio + tamano*anio", "CIIU4^ANIO_F + tamano_empresa^ANIO_F")
)

readr::write_csv(resultado_paso2, file.path(data_dir, "validez_test_f_con_controles.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Validez del test F de pre-tendencias")

message("PASO 1: nivel agregado vs. per-capita (canal mecanico de headcount)")
print(resultado_paso1, n = Inf, width = Inf)

message("")
message("PASO 2: con y sin controles sector*anio + tamano*anio (mismo panel sin atipicas)")
print(resultado_paso2, n = Inf, width = Inf)

message("")
message("Tablas exportadas en: ", data_dir)
