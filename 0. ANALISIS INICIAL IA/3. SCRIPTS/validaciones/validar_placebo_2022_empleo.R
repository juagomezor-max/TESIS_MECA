# validar_placebo_2022_empleo.R
#
# Placebo/falsificacion: si el "salto atipico" que aparece en 2022->2023
# para el primer eslabon (costo laboral) es realmente atribuible al
# choque REAL de salario minimo de 2023, NO deberia aparecer el mismo
# patron en 2021->2022 -- un tramo en el que el salario minimo REAL
# CAYO (-3,05pp, cifra provista por el usuario, no verificada ni
# recalculada en este repositorio). Si aparece una señal similar en
# 2022 (un año sin choque, o con un choque de signo opuesto), es una
# alerta de que el "salto" podria no ser especifico de 2023. Si NO
# aparece, es evidencia a favor de que el mecanismo responde
# especificamente al choque de 2023.
#
# APLICADO A LAS 4 VARIABLES DE RESULTADO PRINCIPALES (empleo_total,
# empleo_permanente, empleo_temporal, participacion_permanente), NO al
# costo laboral -- ese ya se hizo en validar_primer_eslabon_costo_
# laboral*.R. Para AMBAS medidas de exposicion (Exposure2022_obreros y
# Bite2022_obreros), en pie de igualdad, sin declarar ninguna principal
# (decision de la seccion 4.3 de Estrategia Empirica).
#
# VENTANA: SOLO datos pre-2023 (2015-2019+2021-2022, 2020 excluido) --
# no se usan 2023-2024 porque contendrian el choque REAL, contaminando
# el placebo. Es la MISMA ventana ya usada en validar_pretendencias_
# panel_formal_ventana_completa.R.
#
# CONTRASTE (mismo diseño que el Paso 3 del primer eslabon, adaptado:
# "el contraste que tenga sentido dado el diseño ya construido"):
#   contraste_placebo = (b_2022 - b_2021) - promedio(b_2017-b_2016, b_2018-b_2017, b_2019-b_2018)
#                      = (b_2022 - b_2021) - (b_2019 - b_2016)/3   [misma suma telescopica]
#   SE = sqrt(w' V w), t = contraste/SE, df = fixest::degrees_freedom(modelo, "t")
#   Mismo criterio que el primer eslabon: "atipico" si p<0.05.
#
# DERIVADO de (no reimplementado de memoria):
# - Forma funcional del event-study (i(ANIO_F, exposicion, ref='2015')),
#   los 3 controles, cluster=~NORDEMP: identico a validar_pretendencias_
#   panel_formal_ventana_completa.R.
# - Formula del contraste lineal manual (coef/vcov, telescopica):
#   IDENTICA a validar_primer_eslabon_costo_laboral.R / _bite.R --
#   unico cambio es que aqui se contrasta 2021->2022 en vez de
#   2022->2023, y el outcome es empleo (no costo laboral).
# - bite_1sd (escala de Bite): identica a validar_primer_eslabon_costo_
#   laboral_bite.R.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - validacion_placebo_2022_empleo_exposure.csv (F-test conjunto, 4 outcomes x 2 especificaciones)
# - validacion_placebo_2022_empleo_bite.csv (idem, Bite)
# - validacion_placebo_2022_contraste_salto_atipico_exposure.csv (Paso 3 adaptado)
# - validacion_placebo_2022_contraste_salto_atipico_bite.csv

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal_path <- file.path(data_dir, "panel_establecimiento_formal.rds")
if (!file.exists(panel_formal_path)) {
  stop("Falta panel_establecimiento_formal.rds. Corre construccion/construir_panel_establecimiento_formal.R primero.")
}
exposicion_firma_path <- file.path(data_dir, "exposicion_firma_eam.rds")
if (!file.exists(exposicion_firma_path)) {
  stop("Falta exposicion_firma_eam.rds. Corre pipeline/02_construir_exposicion.R primero.")
}

ANIOS_PRE_COMPLETO <- c(2015:2019, 2021, 2022)  # excluye 2020 (pandemia) y 2023-2024 (choque real)
ANIOS_NO_REF <- setdiff(ANIOS_PRE_COMPLETO, 2015)
PATRON_F <- paste0("ANIO_F::(", paste(ANIOS_NO_REF, collapse = "|"), ")")

outcomes_info <- tibble::tribble(
  ~var, ~label,
  "empleo_total",             "Empleo total",
  "empleo_permanente",        "Empleo permanente",
  "empleo_temporal",          "Empleo temporal (directo + agencia)",
  "participacion_permanente", "Participacion de permanentes en el empleo total (%)"
)

panel_formal <- readr::read_rds(panel_formal_path) %>%
  dplyr::filter(ANIO %in% ANIOS_PRE_COMPLETO) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(ANIOS_PRE_COMPLETO)))

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, Bite2022_obreros)

# ------------------------------------------------------------------
# Funciones genericas: correr evento (F conjunto) + contraste del
# salto 2021->2022, dado un nombre de variable de exposicion ya en los
# datos.
# ------------------------------------------------------------------

correr_evento_placebo <- function(data, var_y, var_x, etiqueta, fe_adicionales = NULL) {
  fe <- if (is.null(fe_adicionales)) "NORDEST" else paste("NORDEST", fe_adicionales, sep = " + ")
  formula_modelo <- stats::as.formula(paste0(var_y, " ~ i(ANIO_F, ", var_x, ", ref = '2015') | ", fe))
  modelo <- fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
  prueba_f <- fixest::wald(modelo, keep = PATRON_F, print = FALSE)

  resumen_f <- tibble::tibble(
    variable = var_y,
    especificacion = etiqueta,
    n_obs = stats::nobs(modelo),
    n_establecimientos = dplyr::n_distinct(data$NORDEST[!is.na(data[[var_y]])]),
    n_firmas = dplyr::n_distinct(data$NORDEMP[!is.na(data[[var_y]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_value = signif(prueba_f$p, 4)
  )

  nombres <- names(stats::coef(modelo))
  buscar <- function(anio) {
    hallado <- grep(paste0("^ANIO_F::", anio, ":"), nombres, value = TRUE)
    if (length(hallado) != 1) stop("No se encontro (o se encontro mas de 1) coeficiente para ANIO_F::", anio, " en '", etiqueta, "' / ", var_y, ".")
    hallado
  }
  n2016 <- buscar(2016); n2019 <- buscar(2019); n2021 <- buscar(2021); n2022 <- buscar(2022)

  b <- stats::coef(modelo)
  V <- stats::vcov(modelo)
  w <- setNames(rep(0, length(b)), nombres)
  w[n2016] <- 1/3
  w[n2019] <- -1/3
  w[n2021] <- -1
  w[n2022] <- 1

  estimate <- as.numeric(sum(w * b))
  se <- as.numeric(sqrt(t(w) %*% V %*% w))
  t_stat <- estimate / se
  df <- fixest::degrees_freedom(modelo, type = "t")
  p_value_contraste <- 2 * stats::pt(-abs(t_stat), df = df)

  contraste <- tibble::tibble(
    variable = var_y,
    especificacion = etiqueta,
    incremento_2021_2022 = round(unname(b[n2022]) - unname(b[n2021]), 6),
    incremento_tipico_2016_2019 = round((unname(b[n2019]) - unname(b[n2016])) / 3, 6),
    contraste = round(estimate, 6),
    se_contraste = round(se, 6),
    t_stat = round(t_stat, 3),
    df = round(df, 1),
    p_value = signif(p_value_contraste, 4),
    atipico_al_5pct = p_value_contraste < 0.05
  )

  list(resumen_f = resumen_f, contraste = contraste)
}

correr_medida <- function(medida) {
  filtro_no_na <- if (medida == "exposure") "Exposure2022_obreros" else "Bite2022_obreros"
  var_x <- if (medida == "exposure") "exposicion_10pp" else "bite_1sd"

  datos <- panel_formal %>%
    dplyr::inner_join(exposicion_firma, by = "NORDEMP") %>%
    dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa), !is.na(.data[[filtro_no_na]]))

  if (medida == "exposure") {
    datos <- datos %>% dplyr::mutate(exposicion_10pp = Exposure2022_obreros / 0.1)
  } else {
    sd_muestra <- stats::sd(datos %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::pull(Bite2022_obreros), na.rm = TRUE)
    datos <- datos %>% dplyr::mutate(bite_1sd = Bite2022_obreros / sd_muestra)
  }

  resultados <- purrr::map(seq_len(nrow(outcomes_info)), function(i) {
    var_y <- outcomes_info$var[i]
    r_sin <- correr_evento_placebo(datos, var_y, var_x, "Sin controles")
    r_con <- correr_evento_placebo(datos, var_y, var_x, "Con sector*anio + tamano*anio + departamento*anio",
                                    "CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F")
    list(resumen_f = dplyr::bind_rows(r_sin$resumen_f, r_con$resumen_f),
         contraste = dplyr::bind_rows(r_sin$contraste, r_con$contraste))
  })

  list(
    resumen_f = dplyr::bind_rows(purrr::map(resultados, "resumen_f")),
    contraste = dplyr::bind_rows(purrr::map(resultados, "contraste")),
    n_establecimientos = dplyr::n_distinct(datos$NORDEST),
    n_firmas = dplyr::n_distinct(datos$NORDEMP)
  )
}

resultado_exposure <- correr_medida("exposure")
resultado_bite <- correr_medida("bite")

readr::write_csv(resultado_exposure$resumen_f, file.path(out_dir, "validacion_placebo_2022_empleo_exposure.csv"))
readr::write_csv(resultado_bite$resumen_f, file.path(out_dir, "validacion_placebo_2022_empleo_bite.csv"))
readr::write_csv(resultado_exposure$contraste, file.path(out_dir, "validacion_placebo_2022_contraste_salto_atipico_exposure.csv"))
readr::write_csv(resultado_bite$contraste, file.path(out_dir, "validacion_placebo_2022_contraste_salto_atipico_bite.csv"))

# ------------------------------------------------------------------
# Reporte en consola (numeros crudos, sin interpretar)
# ------------------------------------------------------------------

script_header("validar_placebo_2022_empleo.R -- Placebo: contraste 2021->2022 (salario minimo real CAYO -3.05pp), empleo, ambas medidas")
message("")
message("Exposure2022_obreros: ", resultado_exposure$n_establecimientos, " establecimientos, ", resultado_exposure$n_firmas, " firmas.")
message("Bite2022_obreros: ", resultado_bite$n_establecimientos, " establecimientos, ", resultado_bite$n_firmas, " firmas.")
message("")
message("=== Contraste del salto 2021->2022 vs. incremento tipico 2016-2019 -- Exposure2022_obreros ===")
print(resultado_exposure$contraste, n = Inf, width = Inf)
message("")
message("=== Contraste del salto 2021->2022 vs. incremento tipico 2016-2019 -- Bite2022_obreros ===")
print(resultado_bite$contraste, n = Inf, width = Inf)
message("")
message("Tablas exportadas en: ", out_dir)
