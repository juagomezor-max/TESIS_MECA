# Extension del diagnostico de atricion diferencial (nivel firma),
# rama feature/panel-establecimiento. Complementa
# diagnostico_atricion_diferencial_exposicion_eam.R (commit 15105d6,
# ya re-corrido y versionado) con 4 piezas que le faltaban:
#
# (a) Signo y magnitud relativa: tasa de salida por quintil con error
#     estandar, brecha Q5-Q1 en pp Y como razon relativa.
# (b) Especificacion continua (no solo quintiles): P(salir) sobre
#     Exposure2022_obreros continua, con controles de sector (CIIU4) y
#     tamaño, LPM con errores estandar robustos e IC 95%.
# (c) PLACEBO pre-choque: mismo diagnostico con año base 2017,
#     seguimiento a 2018 y 2019 (evita 2020). Si la brecha pre-choque es
#     del mismo orden que 2023/2024, la salida es rotacion normal; si
#     solo aparece despues del choque, es señal real.
# (d) Descomposicion de la salida: desaparicion completa vs. caida por
#     debajo del umbral de cobertura de la EAM. Umbral verificado en la
#     ficha metodologica oficial de DANE (ver cita exacta abajo), NO
#     asumido de memoria.
#
# ------------------------------------------------------------------
# Umbral de cobertura de la EAM (verificado, no asumido): DANE,
# "Metodologia General Encuesta Anual Manufacturera - EAM", codigo
# DSO-EAM-MET-001, version 11, agosto 2025.
# https://www.dane.gov.co/files/operaciones/EAM/met-EAM.pdf
#
# Cita textual (seccion "Alcance", pag. 8): "La operacion estadistica se
# aplica a establecimientos industriales con diez o mas personal ocupado
# o con un valor de produccion establecido anualmente, el cual se
# incrementa con base en el Indice de Precios del Productor (IPP)
# seccion industria."
#
# Cita textual (seccion "Numero de establecimientos", pag. 15): "Para la
# recoleccion de la EAM 2016, se establecio como parametro de inclusion
# 500 millones de pesos anuales en ingresos o 10 personas ocupadas [...]
# En adelante se evolucionara con la variacion del IPP seccion
# industria." Antes de 2016 (desde 1992) el umbral de produccion era $65
# millones (año base), tambien indexado por IPP -- es decir, el criterio
# CAMBIO DE BASE en 2016 (65M -> 500M), pero la ventana de este
# diagnostico (2017-2019 placebo, 2022-2024 real) es INTEGRAMENTE
# posterior a ese cambio, asi que no contamina las comparaciones aqui.
#
# LIMITACION EXPLICITA (Regla 3: si algo no se puede calcular con los
# datos disponibles, decirlo en vez de aproximarlo sin avisar): la
# macrobase EAM y el diccionario maestro NO tienen ninguna columna de
# "novedad"/"estado"/motivo de salida (verificado: busqueda de
# NOVEDAD|ESTADO|LIQUID|INACTIV en las 398 columnas, sin resultados). No
# podemos usar la clasificacion real de DANE (liquidada/inactiva/cambio
# de sector/bajo umbral). El Paso (d) de abajo usa como PROXY unicamente
# la pata de EMPLEO del criterio (PERTOTAL < 10 en el ultimo año
# observado), NO la pata de valor de produccion (requeriria construir un
# deflactor IPP industrial indexado desde 2016, fuera de alcance aqui) --
# el proxy es una cota, no una clasificacion definitiva.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - atricion_a_tasa_por_quintil_con_se.csv
# - atricion_b_especificacion_continua.csv
# - atricion_c_placebo_2017_2018_2019.csv
# - atricion_d_descomposicion_umbral.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

conteo_path <- file.path(data_dir, "conteo_personal_categoria_eam.rds")
exposicion_path <- file.path(data_dir, "exposicion_obreros_eam.rds")
if (!file.exists(conteo_path)) stop("Falta conteo_personal_categoria_eam.rds.")
if (!file.exists(exposicion_path)) stop("Falta exposicion_obreros_eam.rds.")

conteo <- readr::read_rds(conteo_path)
exposicion <- readr::read_rds(exposicion_path)

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO", "CIIU4", "PERTOTAL"))

macro_firma <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(suppressWarnings(as.numeric(ANIO))), PERTOTAL = suppressWarnings(as.numeric(PERTOTAL))) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(CIIU4 = dplyr::first(CIIU4), PERTOTAL = if (all(is.na(PERTOTAL))) NA_real_ else sum(PERTOTAL, na.rm = TRUE), .groups = "drop")

tamano_de <- function(pertotal) {
  dplyr::case_when(
    is.na(pertotal) ~ NA_character_,
    pertotal < 50 ~ "Pequena",
    pertotal < 200 ~ "Mediana",
    TRUE ~ "Grande"
  )
}

# ====================================================================
# Funcion generica: dado un anio base y anios de seguimiento, construye
# la cohorte, el quintil/exposicion continua, y devuelve todo lo
# necesario para (a), (b) y (c) -- se reusa para 2022 (real) y 2017
# (placebo).
# ====================================================================

construir_cohorte <- function(anio_base, anios_seguimiento) {
  firmas_base <- exposicion %>%
    dplyr::filter(ANIO == anio_base) %>%
    dplyr::distinct(NORDEMP, Exposure2022_obreros, quintil_exposure2022_obreros)

  if (anio_base != 2022) {
    # Placebo: Exposure_obreros propia del anio base (no la de 2022),
    # misma formula/winsorizacion/quintiles que construir_exposicion_obreros_eam.R.
    safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
    winsorize <- function(x, probs = c(0.01, 0.99)) {
      if (all(is.na(x))) return(x)
      limites <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
      pmin(pmax(x, limites[[1]]), limites[[2]])
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
    firmas_base <- conteo %>%
      dplyr::filter(ANIO == anio_base) %>%
      dplyr::mutate(participacion_obreros_raw = safe_divide(total_obreros, empleo_total_categorias)) %>%
      dplyr::transmute(
        NORDEMP,
        Exposure2022_obreros = winsorize(participacion_obreros_raw),
        quintil_exposure2022_obreros = make_quintiles(Exposure2022_obreros)
      )
  }

  controles_base <- macro_firma %>%
    dplyr::filter(ANIO == anio_base) %>%
    dplyr::transmute(NORDEMP, CIIU4 = factor(CIIU4), tamano_empresa = factor(tamano_de(PERTOTAL)), PERTOTAL_base = PERTOTAL)

  cohorte <- firmas_base %>%
    dplyr::left_join(controles_base, by = "NORDEMP") %>%
    dplyr::mutate(exposicion_10pp = Exposure2022_obreros / 0.1)

  for (anio_seg in anios_seguimiento) {
    nordemp_ese_anio <- macro_firma %>% dplyr::filter(ANIO == anio_seg) %>% dplyr::pull(NORDEMP) %>% unique()
    cohorte[[paste0("sale_", anio_seg)]] <- as.integer(!cohorte$NORDEMP %in% nordemp_ese_anio)
  }

  cohorte
}

reportar_tasa_por_quintil <- function(cohorte, anios_seguimiento, etiqueta_anio_base) {
  purrr::map_dfr(anios_seguimiento, function(anio_seg) {
    var_sale <- paste0("sale_", anio_seg)
    datos <- cohorte %>%
      dplyr::filter(!is.na(quintil_exposure2022_obreros)) %>%
      dplyr::mutate(quintil = as.character(quintil_exposure2022_obreros))

    tabla <- datos %>%
      dplyr::group_by(quintil) %>%
      dplyr::summarise(
        n_firmas = dplyr::n(),
        n_sale = sum(.data[[var_sale]]),
        tasa_salida_pct = round(100 * mean(.data[[var_sale]]), 3),
        se_pct = round(100 * sqrt(mean(.data[[var_sale]]) * (1 - mean(.data[[var_sale]])) / dplyr::n()), 3),
        .groups = "drop"
      ) %>%
      dplyr::mutate(anio_seguimiento = anio_seg, anio_base = etiqueta_anio_base)

    q1 <- tabla %>% dplyr::filter(quintil == "Q1 - Muy baja")
    q5 <- tabla %>% dplyr::filter(quintil == "Q5 - Muy alta")
    if (nrow(q1) == 1 && nrow(q5) == 1) {
      brecha_pp <- round(q5$tasa_salida_pct - q1$tasa_salida_pct, 3)
      se_brecha <- round(sqrt(q5$se_pct^2 + q1$se_pct^2), 3)
      z_stat <- round(brecha_pp / se_brecha, 3)
      p_valor <- signif(2 * (1 - pnorm(abs(z_stat))), 4)
      base_promedio <- (q1$tasa_salida_pct + q5$tasa_salida_pct) / 2
      brecha_relativa_pct <- round(100 * brecha_pp / base_promedio, 1)
      tabla <- tabla %>% dplyr::mutate(
        brecha_q5_q1_pp = brecha_pp,
        se_brecha_pp = se_brecha,
        z_stat_brecha = z_stat,
        p_valor_brecha = p_valor,
        brecha_relativa_pct_sobre_base_promedio = brecha_relativa_pct
      )
    }
    tabla
  })
}

# ====================================================================
# (a) y parte de (c): tasa por quintil con SE, 2022->2023/2024
# ====================================================================

cohorte_2022 <- construir_cohorte(2022, c(2023, 2024))
tabla_a <- reportar_tasa_por_quintil(cohorte_2022, c(2023, 2024), "2022 (real, post-choque)")

# ====================================================================
# (b) Especificacion continua con controles, LPM (feols), SE robustos
# ====================================================================

correr_lpm <- function(cohorte, var_sale, etiqueta) {
  datos <- cohorte %>% dplyr::filter(!is.na(exposicion_10pp), !is.na(CIIU4), !is.na(tamano_empresa))
  formula_sin <- stats::as.formula(paste0(var_sale, " ~ exposicion_10pp"))
  formula_con <- stats::as.formula(paste0(var_sale, " ~ exposicion_10pp | CIIU4 + tamano_empresa"))

  m_sin <- fixest::feols(formula_sin, data = datos, vcov = "hetero", warn = FALSE, notes = FALSE)
  m_con <- fixest::feols(formula_con, data = datos, vcov = "hetero", warn = FALSE, notes = FALSE)

  extraer <- function(m, spec) {
    ci <- confint(m, level = 0.95)
    tibble::tibble(
      especificacion = etiqueta,
      spec = spec,
      n_obs = stats::nobs(m),
      coef_10pp = round(unname(coef(m)["exposicion_10pp"]), 5),
      se = round(unname(fixest::se(m)["exposicion_10pp"]), 5),
      ic95_bajo = round(ci["exposicion_10pp", 1], 5),
      ic95_alto = round(ci["exposicion_10pp", 2], 5),
      p_valor = signif(fixest::pvalue(m)["exposicion_10pp"], 4)
    )
  }

  dplyr::bind_rows(
    extraer(m_sin, "Sin controles"),
    extraer(m_con, "Con sector(CIIU4) + tamano")
  )
}

tabla_b <- dplyr::bind_rows(
  correr_lpm(cohorte_2022, "sale_2023", "P(salir en 2023) ~ Exposure2022_obreros continua"),
  correr_lpm(cohorte_2022, "sale_2024", "P(salir en 2024) ~ Exposure2022_obreros continua")
)

# ====================================================================
# (c) PLACEBO pre-choque: anio base 2017, seguimiento 2018/2019
# ====================================================================

cohorte_2017 <- construir_cohorte(2017, c(2018, 2019))
tabla_c_quintiles <- reportar_tasa_por_quintil(cohorte_2017, c(2018, 2019), "2017 (placebo, pre-choque)")
tabla_c_continua <- dplyr::bind_rows(
  correr_lpm(cohorte_2017, "sale_2018", "PLACEBO: P(salir en 2018) ~ Exposure2017_obreros continua"),
  correr_lpm(cohorte_2017, "sale_2019", "PLACEBO: P(salir en 2019) ~ Exposure2017_obreros continua")
)

tabla_c <- list(quintiles = tabla_c_quintiles, continua = tabla_c_continua)

# ====================================================================
# (d) Descomposicion: desaparicion completa vs. proxy de umbral (PERTOTAL<10)
# ====================================================================

descomponer_salida <- function(cohorte, var_sale, etiqueta) {
  datos <- cohorte %>% dplyr::filter(.data[[var_sale]] == 1)
  datos <- datos %>%
    dplyr::mutate(
      categoria = dplyr::case_when(
        is.na(PERTOTAL_base) ~ "PERTOTAL del anio base faltante (no clasificable)",
        PERTOTAL_base < 10 ~ "PERTOTAL_base < 10 (candidato a umbral, NO confirmado)",
        TRUE ~ "PERTOTAL_base >= 10 (atricion NO explicada por pata de empleo del umbral)"
      )
    )
  datos %>%
    dplyr::count(categoria, name = "n_firmas") %>%
    dplyr::mutate(pct = round(100 * n_firmas / sum(n_firmas), 2), especificacion = etiqueta)
}

tabla_d <- dplyr::bind_rows(
  descomponer_salida(cohorte_2022, "sale_2023", "Salen en 2023 (base 2022)"),
  descomponer_salida(cohorte_2022, "sale_2024", "Salen en 2024 (base 2022)"),
  descomponer_salida(cohorte_2017, "sale_2018", "PLACEBO: salen en 2018 (base 2017)"),
  descomponer_salida(cohorte_2017, "sale_2019", "PLACEBO: salen en 2019 (base 2017)")
)

# ====================================================================
# Exportar
# ====================================================================

readr::write_csv(tabla_a, file.path(out_dir, "atricion_a_tasa_por_quintil_con_se.csv"))
readr::write_csv(tabla_b, file.path(out_dir, "atricion_b_especificacion_continua.csv"))
readr::write_csv(dplyr::bind_rows(tabla_c$quintiles), file.path(out_dir, "atricion_c_placebo_2017_2018_2019.csv"))
readr::write_csv(tabla_c$continua, file.path(out_dir, "atricion_c_placebo_especificacion_continua.csv"))
readr::write_csv(tabla_d, file.path(out_dir, "atricion_d_descomposicion_umbral.csv"))

# ====================================================================
# Reporte en consola
# ====================================================================

script_header("Extension del diagnostico de atricion diferencial")

message("\n=== (a) Tasa de salida por quintil, con SE y brecha Q5-Q1 (2022->2023/2024) ===")
print(tabla_a, n = Inf, width = Inf)

message("\n=== (b) Especificacion continua, LPM con SE robustos (2022->2023/2024) ===")
print(tabla_b, n = Inf, width = Inf)

message("\n=== (c) PLACEBO 2017->2018/2019: tasa por quintil ===")
print(tabla_c$quintiles, n = Inf, width = Inf)
message("\n=== (c) PLACEBO 2017->2018/2019: especificacion continua ===")
print(tabla_c$continua, n = Inf, width = Inf)

message("\n=== (d) Descomposicion de la salida (proxy PERTOTAL<10, ver limitacion en cabecera del script) ===")
print(tabla_d, n = Inf, width = Inf)

message("\nTablas exportadas en: ", out_dir)
