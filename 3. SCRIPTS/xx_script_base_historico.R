# ==============================================================================
# xx_script_base_historico.R
#
# Título:  Análisis manual del efecto del salario mínimo 2023 sobre firmas
#          manufactureras (EAM)
# Autor:   Julio Gómez
# Fecha:   2026-09-15 (versión corregida 2026-09-16)
#
# Se ejecuta con el directorio de trabajo en la RAÍZ del repositorio.
#
# "1. DATOS/":
#   - panel_analitico_firma_eam.{rds,csv,dta}      -> panel EMPRESA-AÑO
#   - panel_establecimiento_formal.{rds,csv,dta}   -> panel ESTABLECIMIENTO-AÑO
#
# El trabajo previo con asistencia de Claude Code está archivado en
# "0. ANALISIS INICIAL IA/".
#
# ------------------------------------------------------------------------------
# HISTÓRICO -- NO CORRER
#
# Superado por 01_descriptivos_y_contexto.R. Se conserva como registro
# de la revisión que se hizo en su momento (ver "REGISTRO DE CORRECCIONES"
# abajo, con 9 correcciones metodológicas que pueden servir de referencia
# para la sección de metodología de la tesis).
#
# No correrlo: su carpeta de salida (4. RESULTADOS/Descriptivos/, Estimacion/,
# Robustez/, Validaciones/) colisiona con la de 01_descriptivos_y_contexto.R --
# pisaría esos resultados.
# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------
# REGISTRO DE CORRECCIONES (2026-09-16) respecto a la versión del 15-sep
# ------------------------------------------------------------------------------
# [C1] CONTROLES FIJADOS EN 2022. En el panel, tamano_empresa se calcula cada
#      año con el empleo_total de ese año (03_construir_panel.R, línea 72).
#      Usar tamano_empresa^ANIO_F con outcomes de empleo condiciona en el
#      resultado (bad control). Se usan tamano_2022, CIIU4_2022 y DPTO_2022.
#      La versión con controles contemporáneos se conserva SOLO como
#      comparación (sección 5.2), para documentar cuánto cambia.
# [C2] MUESTRA COMÚN. Bite falta para ~17% de las firmas (sin obreros
#      permanentes en 2022). Exposure y Bite se estiman ahora también sobre
#      la muestra donde ambas existen, para separar "medida" de "muestra".
# [C3] ESCALA COMPARABLE. Ambas medidas se estandarizan a DE = 1 (corte 2022,
#      muestra común). La tabla de escalas permite volver a +10pp.
# [C4] WINSORIZACIÓN DE BITE sobre el corte transversal de firmas (una fila
#      por NORDEMP), no sobre todas las filas firma-año.
# [C5] PRE-TENDENCIAS SOBRE TODA LA VENTANA PRE. Se mantiene el Wald
#      2015-2019 (continuidad) y se agrega un event study con referencia
#      2022 que incluye 2021.
# [C6] PRIMER ESLABÓN incorporado al script (antes solo en el archivo).
# [C7] TENDENCIA ESTIMADA SOLO EN EL PRE-PERÍODO (dos pasos), además de la
#      versión conjunta original, que deja que 2023-2024 influyan la tendencia.
# [C8] ESCRUTINIO AUTOMÁTICO: las celdas se seleccionan por regla, no a mano.
# [C9] Técnicos: script autocontenido (el diagnóstico que lee la carpeta
#      archivada es opcional), across() sin '...' deprecado, wald() sin
#      imprimir, ref de i() como texto, tablas también en CSV (versionables).
# ==============================================================================

rm(list = ls())
gc()

# ------------------------------------------------------------------
# 1) Librerías
# ------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(fixest)
library(ggplot2)

# flextable NO está en renv.lock. Si falta:
#   renv::install("flextable"); renv::snapshot()
if (!requireNamespace("flextable", quietly = TRUE)) {
  stop("Falta 'flextable'. Corre renv::install('flextable') y luego renv::snapshot().")
}
library(flextable)

# ------------------------------------------------------------------
# 2) Parámetros, carga de datos y variables de trabajo
# ------------------------------------------------------------------

ANIO_BASE       <- 2022
ANIOS_PRE_TEST  <- 2015:2019          # prueba conjunta original
ANIOS_PRE_TODOS <- c(2015:2019, 2021) # coeficientes pre del event study (ref 2022)
ANIOS_POST      <- 2023:2024
UMBRAL_P        <- 0.05

panel_firma <- readr::read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds"))
panel_establecimiento <- readr::read_rds(file.path("1. DATOS", "panel_establecimiento_formal.rds"))

# Verificaciones mínimas antes de estimar
stopifnot(
  !anyDuplicated(panel_firma[, c("NORDEMP", "ANIO")]),
  all(c("Exposure2022_obreros", "Bite2022_obreros", "empleo_total", "CIIU4",
        "DPTO", "tamano_empresa", "post_2023", "anio_lineal") %in% names(panel_firma)),
  all(c("NORDEMP", "ANIO", "costo_laboral_total", "empleo_total") %in% names(panel_establecimiento))
)
panel_firma <- panel_firma %>% mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

winsorize <- function(x, probs = c(0.01, 0.99)) {
  limites <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, limites[1]), limites[2])
}

## 2.1 -- Medidas y controles a nivel firma (corte 2022) -------------
# [C1][C3][C4] Una fila por firma: medidas, controles fijos y muestra común.

firma_2022 <- panel_firma %>%
  filter(ANIO == ANIO_BASE) %>%
  transmute(
    NORDEMP,
    Exposure2022_obreros,
    Bite2022_obreros,
    Bite2022_obreros_wins = winsorize(Bite2022_obreros),   # [C4] sobre el corte
    CIIU4_2022   = factor(CIIU4),
    DPTO_2022    = factor(DPTO),
    tamano_2022  = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")),
    en_muestra_comun = !is.na(Exposure2022_obreros) & !is.na(Bite2022_obreros)
  )

escalas <- firma_2022 %>%
  filter(en_muestra_comun) %>%
  summarise(
    de_exposure = sd(Exposure2022_obreros),
    de_bite_wins = sd(Bite2022_obreros_wins)
  )

firma_2022 <- firma_2022 %>%
  mutate(
    exposure_de = Exposure2022_obreros / escalas$de_exposure,
    bite_de     = Bite2022_obreros_wins / escalas$de_bite_wins
  )

panel_firma <- panel_firma %>%
  select(-any_of(c("Bite2022_obreros_wins"))) %>%
  left_join(
    firma_2022 %>% select(NORDEMP, Bite2022_obreros_wins, exposure_de, bite_de,
                          CIIU4_2022, DPTO_2022, tamano_2022, en_muestra_comun),
    by = "NORDEMP"
  ) %>%
  mutate(
    en_muestra_comun = coalesce(en_muestra_comun, FALSE),
    ANIO_F = factor(ANIO)
  )

## 2.2 -- Salario promedio firma-año para el primer eslabón ----------
# [C6] Se agrega el panel de establecimiento a firma-año. Si algún
# establecimiento de la firma tiene costo faltante, el año queda NA
# (evita salarios promedio subestimados por sumas parciales).

salario_firma <- panel_establecimiento %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO))) %>%
  group_by(NORDEMP, ANIO) %>%
  summarise(
    costo_laboral_firma = if (any(is.na(costo_laboral_total))) NA_real_ else sum(costo_laboral_total),
    empleo_est_firma    = sum(empleo_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    salario_promedio_firma = ifelse(empleo_est_firma > 0, costo_laboral_firma / empleo_est_firma, NA_real_),
    asinh_salario_promedio = asinh(salario_promedio_firma)
  )

panel_firma <- panel_firma %>%
  left_join(salario_firma %>% select(NORDEMP, ANIO, asinh_salario_promedio), by = c("NORDEMP", "ANIO"))

message("Cobertura del salario promedio en el panel de firma: ",
        round(100 * mean(!is.na(panel_firma$asinh_salario_promedio)), 1), "%")

## 2.3 -- Helpers ----------------------------------------------------

dir_desc     <- file.path("4. RESULTADOS", "Descriptivos")
dir_fig      <- file.path(dir_desc, "figuras")
dir_valid    <- file.path("4. RESULTADOS", "Validaciones")
dir_estim    <- file.path("4. RESULTADOS", "Estimacion")
dir_robustez <- file.path("4. RESULTADOS", "Robustez")
for (d in c(dir_desc, dir_fig, dir_valid, dir_estim, dir_robustez)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Guarda en Word y además en CSV (el CSV sí se puede comparar en git) [C9]
guardar_tabla_word <- function(tabla, nombre_archivo, caption = NULL, digits = 3, carpeta = dir_desc) {
  ft <- flextable::flextable(tabla)
  ft <- flextable::colformat_double(ft, digits = digits)
  ft <- flextable::autofit(ft)
  if (!is.null(caption)) ft <- flextable::set_caption(ft, caption = caption)
  ruta <- file.path(carpeta, nombre_archivo)
  flextable::save_as_docx(ft, path = ruta)
  readr::write_csv(tabla, sub("\\.docx$", ".csv", ruta))
  message("Guardado: ", ruta)
  invisible(tabla)
}

tema_hist <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11)
  )

# Efectos fijos: [C1] controles fijados en 2022 (principal) o contemporáneos
# (solo comparación).
fe_modelo <- function(controles = c("base2022", "contemporaneos", "ninguno")) {
  controles <- match.arg(controles)
  switch(controles,
         base2022       = "NORDEMP + ANIO_F + CIIU4_2022^ANIO_F + tamano_2022^ANIO_F + DPTO_2022^ANIO_F",
         contemporaneos = "NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO^ANIO_F",
         ninguno        = "NORDEMP + ANIO_F"
  )
}

filtrar_muestra <- function(data, muestra = c("completa", "comun")) {
  muestra <- match.arg(muestra)
  if (muestra == "comun") filter(data, en_muestra_comun) else data
}

n_clusters <- function(modelo, data) dplyr::n_distinct(data$NORDEMP[fixest::obs(modelo)])

outcomes <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")
medidas  <- c("exposure_de", "bite_de")
muestras <- c("completa", "comun")

# ------------------------------------------------------------------
# 3) Estadísticas descriptivas
# ------------------------------------------------------------------

## 3.1 -- Estructura del panel ------------------------------------

tabla_obs_por_anio <- panel_firma %>%
  group_by(ANIO) %>%
  summarise(n_firmas = n(), n_firmas_muestra_comun = sum(en_muestra_comun))
guardar_tabla_word(tabla_obs_por_anio, "tabla_obs_por_anio.docx",
                   "Observaciones por año, panel de firma (total y muestra común)")

tabla_balance_panel <- panel_firma %>%
  count(NORDEMP) %>%
  count(n, name = "n_firmas") %>%
  arrange(desc(n)) %>%
  rename(anios_presente = n)
guardar_tabla_word(tabla_balance_panel, "tabla_balance_panel.docx",
                   "Firmas según número de años presentes en el panel")

## 3.2 -- Medidas de exposición ------------------------------------

resumir <- function(x) {
  c(Media = mean(x, na.rm = TRUE), DE = sd(x, na.rm = TRUE), Mín = min(x, na.rm = TRUE),
    P25 = unname(quantile(x, 0.25, na.rm = TRUE)), Mediana = median(x, na.rm = TRUE),
    P75 = unname(quantile(x, 0.75, na.rm = TRUE)), Máx = max(x, na.rm = TRUE),
    pct_en_0 = mean(x == 0, na.rm = TRUE), pct_en_1 = mean(x == 1, na.rm = TRUE),
    N_valido = sum(!is.na(x)), N_faltante = sum(is.na(x)))
}

tabla_resumen_exposicion <- tibble(
  Estadístico = names(resumir(firma_2022$Exposure2022_obreros)),
  Exposure2022_obreros = resumir(firma_2022$Exposure2022_obreros),
  Bite2022_obreros = resumir(firma_2022$Bite2022_obreros),
  Bite2022_obreros_wins = resumir(firma_2022$Bite2022_obreros_wins)
)
guardar_tabla_word(tabla_resumen_exposicion, "tabla_resumen_exposicion.docx",
                   "Estadísticas descriptivas de las medidas de exposición (corte 2022, una fila por firma)")

tabla_correlacion_medidas <- firma_2022 %>%
  summarise(
    Pearson_bruto        = cor(Exposure2022_obreros, Bite2022_obreros, use = "complete.obs"),
    Spearman_bruto       = cor(Exposure2022_obreros, Bite2022_obreros, method = "spearman", use = "complete.obs"),
    Pearson_winsorizado  = cor(Exposure2022_obreros, Bite2022_obreros_wins, use = "complete.obs"),
    N_muestra_comun      = sum(en_muestra_comun)
  )
guardar_tabla_word(tabla_correlacion_medidas, "tabla_correlacion_medidas.docx",
                   "Correlación entre Exposure2022_obreros y Bite2022_obreros")

# [C3] Escalas para convertir coeficientes por DE a otras unidades:
#   beta por +10pp de Exposure = beta_de * 0.1 / de_exposure
#   beta por +1 unidad de Bite = beta_de / de_bite_wins
guardar_tabla_word(escalas, "tabla_escalas_medidas.docx",
                   "Desviaciones estándar usadas para estandarizar (corte 2022, muestra común)",
                   digits = 4)

# [C2] ¿Quiénes quedan fuera de la muestra común? Si difieren en temporalidad,
# comparar Exposure y Bite en muestras distintas mezcla medida y composición.
tabla_faltantes_bite <- panel_firma %>%
  filter(ANIO == ANIO_BASE) %>%
  mutate(grupo = ifelse(is.na(Bite2022_obreros), "Sin Bite (fuera de muestra común)", "Con Bite")) %>%
  group_by(grupo) %>%
  summarise(
    n_firmas = n(),
    exposure_media = mean(Exposure2022_obreros, na.rm = TRUE),
    empleo_total_mediana = median(empleo_total, na.rm = TRUE),
    participacion_permanente_media = mean(participacion_permanente, na.rm = TRUE),
    pct_firmas_sin_permanentes = mean(empleo_permanente == 0, na.rm = TRUE),
    pct_pequenas = mean(tamano_empresa == "Pequena", na.rm = TRUE)
  )
guardar_tabla_word(tabla_faltantes_bite, "tabla_faltantes_bite_2022.docx",
                   "Firmas con y sin Bite2022_obreros (corte 2022)")

g1 <- ggplot(firma_2022, aes(x = Exposure2022_obreros)) +
  geom_histogram(bins = 30, fill = "#1F4E79", color = "white", alpha = 0.9, na.rm = TRUE) +
  labs(title = "Distribución de Exposure2022_obreros",
       subtitle = "Proporción de obreros y operarios sobre el empleo total (corte 2022)",
       x = "Exposure2022_obreros", y = "Número de firmas") +
  tema_hist
ggsave(file.path(dir_fig, "hist_exposure.png"), g1, width = 7, height = 4.5, dpi = 200, bg = "white")

g2 <- ggplot(firma_2022, aes(x = Bite2022_obreros_wins)) +
  geom_histogram(bins = 30, fill = "#C00000", color = "white", alpha = 0.9, na.rm = TRUE) +
  labs(title = "Distribución de Bite2022_obreros (winsorizado 1%-99%)",
       subtitle = "Índice de Kaitz: salario mínimo 2023 / salario promedio del obrero permanente (corte 2022)",
       x = "Bite2022_obreros_wins", y = "Número de firmas") +
  tema_hist
ggsave(file.path(dir_fig, "hist_bite.png"), g2, width = 7, height = 4.5, dpi = 200, bg = "white")

## 3.2b -- Valores extremos de Bite (diagnóstico opcional) --------
# Bite usa C3R2C1 / (C4R2C1 + C4R2C2): salario de obreros PERMANENTES sobre
# su conteo. Con 1-2 obreros permanentes el "promedio" es de una persona.
# Este diagnóstico lee el panel crudo archivado; si no existe, se omite.

ruta_crudo <- file.path("0. ANALISIS INICIAL IA", "1. DATOS", "6. BASES_DERIVADAS",
                        "descriptivos_exposicion", "panel_firma_eam.rds")
firmas_bite_extremo <- firma_2022 %>% filter(Bite2022_obreros > 2) %>% pull(NORDEMP)

if (file.exists(ruta_crudo) && length(firmas_bite_extremo) > 0) {
  diagnostico_bite_extremo <- readr::read_rds(ruta_crudo) %>%
    mutate(NORDEMP = as.character(NORDEMP)) %>%
    filter(ANIO == ANIO_BASE, NORDEMP %in% firmas_bite_extremo) %>%
    mutate(personal_permanente_obrero = C4R2C1 + C4R2C2,
           salario_promedio_obrero = ifelse(personal_permanente_obrero > 0,
                                            C3R2C1 / personal_permanente_obrero, NA_real_)) %>%
    select(NORDEMP, C3R2C1, personal_permanente_obrero, salario_promedio_obrero) %>%
    arrange(personal_permanente_obrero)
  guardar_tabla_word(diagnostico_bite_extremo, "tabla_diagnostico_bite_extremo.docx",
                     "Firmas con Bite2022_obreros > 2: obreros permanentes y salario promedio")
} else {
  message("Diagnóstico 3.2b omitido (no está el panel crudo archivado o no hay Bite > 2).")
}

## 3.3 -- Variables de resultado (empleo) --------------------------

tabla_resumen_empleo <- panel_firma %>%
  summarise(across(
    all_of(outcomes),
    list(Media = ~mean(.x, na.rm = TRUE), DE = ~sd(.x, na.rm = TRUE),
         Mediana = ~median(.x, na.rm = TRUE), N_faltante = ~sum(is.na(.x))),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("Variable", "Estadístico"), names_sep = "__") %>%
  pivot_wider(names_from = Variable, values_from = value)
guardar_tabla_word(tabla_resumen_empleo, "tabla_resumen_empleo.docx",
                   "Estadísticas descriptivas de las variables de empleo")

tabla_empleo_por_anio <- panel_firma %>%
  group_by(ANIO) %>%
  summarise(across(all_of(outcomes), ~mean(.x, na.rm = TRUE)))
guardar_tabla_word(tabla_empleo_por_anio, "tabla_empleo_por_anio.docx",
                   "Promedio de variables de empleo por año")

## 3.4 -- Variables de mecanismo ------------------------------------
# Nominales y muy asimétricas: antes de usarlas como outcome hay que
# deflactarlas y transformarlas (asinh/log o participación). Aquí solo se
# describen.

tabla_resumen_mecanismos <- panel_firma %>%
  summarise(across(
    c(C3R23C3, C3R41C3, C7R10C2, VALORVEN),
    list(Media = ~mean(.x, na.rm = TRUE), Mediana = ~median(.x, na.rm = TRUE),
         pct_en_0 = ~mean(.x == 0, na.rm = TRUE), N_faltante = ~sum(is.na(.x))),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("Variable", "Estadístico"), names_sep = "__") %>%
  pivot_wider(names_from = Variable, values_from = value)
guardar_tabla_word(tabla_resumen_mecanismos, "tabla_resumen_mecanismos.docx",
                   "Estadísticas descriptivas de las variables de mecanismo (nominales)")

## 3.5 -- Controles ---------------------------------------------

tabla_tamano <- firma_2022 %>% count(tamano_2022, name = "n_firmas")
guardar_tabla_word(tabla_tamano, "tabla_tamano_empresa.docx",
                   "Tamaño de empresa fijado en 2022 (control usado en las estimaciones)")

# [C1] Cuántas firmas cambian de categoría de tamaño en la ventana: mide qué
# tan grave era usar el tamaño contemporáneo.
tabla_cambio_tamano <- panel_firma %>%
  group_by(NORDEMP) %>%
  summarise(n_categorias = n_distinct(tamano_empresa, na.rm = TRUE), .groups = "drop") %>%
  summarise(n_firmas = n(), pct_cambia_tamano = mean(n_categorias > 1))
guardar_tabla_word(tabla_cambio_tamano, "tabla_cambio_tamano.docx",
                   "Firmas que cambian de categoría de tamaño entre años")

tabla_top_sectores <- firma_2022 %>%
  count(CIIU4_2022, sort = TRUE, name = "n_firmas") %>%
  head(10)
guardar_tabla_word(tabla_top_sectores, "tabla_top10_sectores.docx",
                   "10 sectores (CIIU4, corte 2022) con más firmas")

## 3.6 -- Estructura mono/multiplanta -------------------------------

tabla_multi <- panel_establecimiento %>%
  filter(ANIO == ANIO_BASE) %>%
  distinct(NORDEMP, Multi_f) %>%
  count(Multi_f, name = "n_firmas")
guardar_tabla_word(tabla_multi, "tabla_mono_multiplanta.docx",
                   "Firmas mono vs. multiplanta (corte 2022)")

# ------------------------------------------------------------------
# 4) Validación de supuestos
# ------------------------------------------------------------------

## 4.1 -- Prueba conjunta 2015-2019 (continuidad con la versión anterior)

wald_pre <- function(data, outcome, medida, controles) {
  f <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", medida, ", ref = '2015') | ", fe_modelo(controles)))
  m <- feols(f, data = data, cluster = ~NORDEMP)
  fixest::wald(m, keep = "^ANIO_F::", print = FALSE)$p
}

panel_pre_test <- panel_firma %>%
  filter(ANIO %in% ANIOS_PRE_TEST) %>%
  mutate(ANIO_F = droplevels(ANIO_F))

tabla_tendencias_paralelas <- tidyr::expand_grid(outcome = outcomes, medida = medidas, muestra = muestras) %>%
  rowwise() %>%
  mutate(
    datos = list(filtrar_muestra(panel_pre_test, muestra)),
    p_sin_controles       = wald_pre(datos, outcome, medida, "ninguno"),
    p_controles_2022      = wald_pre(datos, outcome, medida, "base2022"),
    p_controles_contemp   = wald_pre(datos, outcome, medida, "contemporaneos")
  ) %>%
  ungroup() %>%
  select(-datos)
print(tabla_tendencias_paralelas)
guardar_tabla_word(tabla_tendencias_paralelas, "tabla_tendencias_paralelas.docx",
                   "Test conjunto de tendencias diferenciales 2015-2019 (ref. 2015): sin controles, controles 2022 y controles contemporáneos",
                   carpeta = dir_valid)

## 4.2 -- Event study completo (ref. 2022) -------------------------
# [C5] Incluye 2021, el año pre más cercano al choque. La prueba conjunta
# usa solo coeficientes pre (2015-2019, 2021).

patron_pre <- paste0("^ANIO_F::(", paste(ANIOS_PRE_TODOS, collapse = "|"), "):")

event_study <- function(data, outcome, medida, controles = "base2022") {
  f <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", medida, ", ref = '", ANIO_BASE, "') | ", fe_modelo(controles)))
  feols(f, data = data, cluster = ~NORDEMP)
}

extraer_evento <- function(modelo) {
  ct <- as.data.frame(coeftable(modelo))
  tibble(
    termino = rownames(ct),
    anio = as.integer(sub("^ANIO_F::(\\d{4}):.*$", "\\1", rownames(ct))),
    beta = ct[, "Estimate"], ee = ct[, "Std. Error"]
  ) %>%
    bind_rows(tibble(termino = "referencia", anio = ANIO_BASE, beta = 0, ee = 0)) %>%
    mutate(ic_inf = beta - 1.96 * ee, ic_sup = beta + 1.96 * ee) %>%
    arrange(anio)
}

coef_evento <- list()
resumen_evento <- list()
for (muestra in muestras) {
  datos <- filtrar_muestra(panel_firma, muestra)
  for (outcome in outcomes) {
    for (medida in medidas) {
      m <- event_study(datos, outcome, medida)
      clave <- paste(muestra, outcome, medida)
      coef_evento[[clave]] <- extraer_evento(m) %>% mutate(muestra = muestra, outcome = outcome, medida = medida)
      resumen_evento[[clave]] <- tibble(
        muestra = muestra, outcome = outcome, medida = medida,
        p_conjunto_pre = fixest::wald(m, keep = patron_pre, print = FALSE)$p,
        n_obs = nobs(m), n_firmas = n_clusters(m, datos)
      )
    }
  }
}
tabla_event_study_coef <- bind_rows(coef_evento)
tabla_event_study_pre  <- bind_rows(resumen_evento)
print(tabla_event_study_pre)

readr::write_csv(tabla_event_study_coef, file.path(dir_valid, "event_study_coeficientes.csv"))
guardar_tabla_word(tabla_event_study_pre, "tabla_event_study_pre.docx",
                   "Event study (ref. 2022, controles 2022): prueba conjunta de coeficientes pre (2015-2019, 2021)",
                   carpeta = dir_valid)

for (muestra_graf in muestras) {
  g_evento <- tabla_event_study_coef %>%
    filter(muestra == muestra_graf) %>%
    ggplot(aes(x = anio, y = beta, color = medida)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_vline(xintercept = ANIO_BASE + 0.5, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup), position = position_dodge(width = 0.4), size = 0.25) +
    facet_wrap(~outcome, scales = "free_y") +
    scale_color_manual(values = c(exposure_de = "#1F4E79", bite_de = "#C00000")) +
    scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
    labs(title = "Event study por medida de exposición (efecto de +1 DE)",
         subtitle = paste0("Ref. 2022; controles CIIU4, tamaño y DPTO fijados en 2022 x año; cluster NORDEMP; muestra: ", muestra_graf),
         x = NULL, y = "Coeficiente (IC 95%)", color = NULL) +
    tema_hist
  ggsave(file.path(dir_valid, paste0("event_study_empleo_", muestra_graf, ".png")),
         g_evento, width = 10, height = 6.5, dpi = 200, bg = "white")
}

## 4.3 -- Primer eslabón: ¿la exposición predice el salto del costo laboral?
# [C6] Outcome: asinh(costo laboral / empleo), nominal; ANIO_F y los
# efectos sector×año absorben la inflación común. Contraste del salto:
#   (b_2023 - b_2022) - incremento anual típico 2016-2019
#   = b_2023 - (b_2019 - b_2016) / 3        (b_2022 = 0 por referencia)

contraste_salto <- function(modelo, medida) {
  nm <- function(a) paste0("ANIO_F::", a, ":", medida)
  pesos <- setNames(c(1, 1/3, -1/3), c(nm(2023), nm(2016), nm(2019)))
  b <- coef(modelo)[names(pesos)]
  V <- vcov(modelo)[names(pesos), names(pesos)]
  est <- sum(pesos * b)
  ee <- sqrt(as.numeric(t(pesos) %*% V %*% pesos))
  gl <- fixest::degrees_freedom(modelo, "t")
  tibble(contraste = est, ee = ee, t = est / ee, p = 2 * pt(-abs(est / ee), df = gl))
}

tabla_primer_eslabon <- tidyr::expand_grid(muestra = muestras, medida = medidas, controles = c("ninguno", "base2022")) %>%
  rowwise() %>%
  mutate(res = list({
    datos <- filtrar_muestra(panel_firma, muestra)
    m <- event_study(datos, "asinh_salario_promedio", medida, controles)
    contraste_salto(m, medida) %>% mutate(n_obs = nobs(m), n_firmas = n_clusters(m, datos))
  })) %>%
  ungroup() %>%
  tidyr::unnest(res)
print(tabla_primer_eslabon)
guardar_tabla_word(tabla_primer_eslabon, "tabla_primer_eslabon.docx",
                   "Primer eslabón: salto 2023 del salario promedio (asinh) por +1 DE de exposición, frente al incremento típico 2016-2019",
                   digits = 4, carpeta = dir_valid)

## 4.4 -- ¿El patrón de Bite se mueve con el año base? ----------------
# Si las tendencias de composición "convergen" hacia el año en que se mide
# Bite y se revierten después, el patrón es un artefacto de medir la
# exposición en ese año (reversión a la media), no un efecto del choque.
# Se reconstruye Bite con base 2019 y 2021 (salario mínimo del año
# siguiente, igual que Bite2022 usa el de 2023) y se comparan los event
# studies sobre las MISMAS firmas.

SM_ANUAL_MILES <- c(`2020` = 877803, `2022` = 1000000, `2023` = 1160000) * 12 / 1000

if (!file.exists(ruta_crudo)) stop("La sección 4.4 necesita el panel crudo archivado: ", ruta_crudo)
panel_crudo <- readr::read_rds(ruta_crudo) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

construir_bite <- function(anio_base, sm_anual) {
  panel_crudo %>%
    filter(ANIO == anio_base) %>%
    mutate(
      personal_perm_obrero = C4R2C1 + C4R2C2,
      salario_obrero = ifelse(!is.na(C3R2C1) & !is.na(personal_perm_obrero) & personal_perm_obrero > 0,
                              C3R2C1 / personal_perm_obrero, NA_real_),
      bite = sm_anual / salario_obrero
    ) %>%
    filter(!is.na(bite), is.finite(bite)) %>%
    transmute(NORDEMP, bite = winsorize(bite))
}

bites_base <- construir_bite(2019, SM_ANUAL_MILES[["2020"]]) %>% rename(bite2019 = bite) %>%
  inner_join(construir_bite(2021, SM_ANUAL_MILES[["2022"]]) %>% rename(bite2021 = bite), by = "NORDEMP") %>%
  inner_join(firma_2022 %>% filter(!is.na(Bite2022_obreros_wins)) %>%
               select(NORDEMP, bite2022 = Bite2022_obreros_wins), by = "NORDEMP") %>%
  mutate(across(c(bite2019, bite2021, bite2022), ~ .x / sd(.x), .names = "{.col}_de"))

# Persistencia de la medida: si Bite es sobre todo ruido transitorio, la
# correlación entre años será baja y la reversión a la media será fuerte.
tabla_persistencia_bite <- bites_base %>%
  summarise(
    n_firmas = n(),
    cor_2019_2021 = cor(bite2019, bite2021),
    cor_2021_2022 = cor(bite2021, bite2022),
    cor_2019_2022 = cor(bite2019, bite2022)
  )
print(tabla_persistencia_bite)
guardar_tabla_word(tabla_persistencia_bite, "tabla_persistencia_bite.docx",
                   "Correlación de Bite entre años base (firmas con Bite en 2019, 2021 y 2022)",
                   carpeta = dir_valid)

panel_bites <- panel_firma %>%
  inner_join(bites_base %>% select(NORDEMP, bite2019_de, bite2021_de, bite2022_de), by = "NORDEMP")

coef_base <- list()
pre_base <- list()
for (outcome in outcomes) {
  for (medida in c("bite2019_de", "bite2021_de", "bite2022_de")) {
    m <- event_study(panel_bites, outcome, medida)
    coef_base[[paste(outcome, medida)]] <- extraer_evento(m) %>%
      mutate(outcome = outcome, anio_base = as.integer(substr(medida, 5, 8)))
    pre_base[[paste(outcome, medida)]] <- tibble(
      outcome = outcome, anio_base = as.integer(substr(medida, 5, 8)),
      p_conjunto_pre = fixest::wald(m, keep = patron_pre, print = FALSE)$p,
      beta_2023 = coef(m)[[paste0("ANIO_F::2023:", medida)]],
      n_obs = nobs(m), n_firmas = n_clusters(m, panel_bites)
    )
  }
}
tabla_coef_anio_base <- bind_rows(coef_base)
tabla_pre_anio_base  <- bind_rows(pre_base)
print(tabla_pre_anio_base, n = Inf)

readr::write_csv(tabla_coef_anio_base, file.path(dir_valid, "event_study_bite_por_anio_base.csv"))
guardar_tabla_word(tabla_pre_anio_base, "tabla_event_study_bite_por_anio_base.docx",
                   "Event study de Bite según año base (ref. 2022, controles 2022, mismas firmas)",
                   carpeta = dir_valid)

g_base <- tabla_coef_anio_base %>%
  mutate(anio_base = factor(anio_base)) %>%
  ggplot(aes(x = anio, y = beta, color = anio_base)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_vline(xintercept = c(2019, 2021, 2022), linetype = "dotted", color = "grey70") +
  geom_vline(xintercept = ANIO_BASE + 0.5, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup), position = position_dodge(width = 0.5), size = 0.2) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  scale_color_manual(values = c(`2019` = "#7F7F7F", `2021` = "#E69F00", `2022` = "#C00000")) +
  labs(title = "Bite según año de medición: ¿el quiebre sigue al año base o a 2023?",
       subtitle = "+1 DE; ref. 2022; controles fijados en 2022 x año; cluster NORDEMP; mismas firmas",
       x = NULL, y = "Coeficiente (IC 95%)", color = "Año base de Bite") +
  tema_hist
ggsave(file.path(dir_valid, "event_study_bite_por_anio_base.png"), g_base,
       width = 10, height = 6.5, dpi = 200, bg = "white")

## 4.5 -- Conciliación: ¿el rechazo de la ventana pre completa viene del
##        panel o del control de tamaño? -----------------------------------
# Prueba conjunta de coeficientes pre (2015-2019, 2021 vs. 2022) variando
# UNA cosa a la vez:
#   panel     : firma (NORDEMP-año) vs. establecimiento (NORDEST-año, exposición heredada de la firma)
#   controles : contemporáneos vs. fijados en 2022
#   muestra   : sin restringir (como el análisis con IA) vs. solo unidades con datos en 2022
#               (así "contemporáneos vs. 2022" se compara sobre las MISMAS observaciones)
#   datos     : solo años pre (como el análisis con IA) vs. panel completo (como el event study 4.2)
# Todas las especificaciones incluyen ANIO_F.
# Chequeo de reproducción: fila establecimiento / contemporaneos / sin_restringir / solo_pre
# con Exposure debería dar ~0,311 / 0,574 / 0,057 / 0,101
# (comparacion_pretendencias_ventana_corta_vs_completa.csv, fila "Con ...").

panel_est_conc <- panel_establecimiento %>%
  mutate(
    NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(as.character(ANIO)), ANIO_F = factor(ANIO),
    CIIU4 = factor(CIIU4), DPTO_fijo = factor(DPTO_fijo),
    tamano_empresa = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  ) %>%
  select(-any_of(c("exposure_de", "bite_de", "CIIU4_2022", "tamano_2022"))) %>%
  inner_join(firma_2022 %>% select(NORDEMP, exposure_de, bite_de), by = "NORDEMP")
stopifnot(!anyDuplicated(panel_est_conc[, c("NORDEST", "ANIO")]))

# Controles fijados en 2022 A NIVEL ESTABLECIMIENTO (tamaño y sector de la
# propia planta en 2022; DPTO_fijo ya es fijo por construcción)
panel_est_conc <- panel_est_conc %>%
  left_join(
    panel_est_conc %>% filter(ANIO == ANIO_BASE) %>%
      transmute(NORDEST, CIIU4_2022 = CIIU4, tamano_2022 = tamano_empresa),
    by = "NORDEST"
  )

especificaciones_conc <- tibble::tribble(
  ~panel,            ~controles,        ~muestra,          ~datos,
  "establecimiento", "contemporaneos",  "sin_restringir",  "solo_pre",
  "establecimiento", "contemporaneos",  "con_datos_2022",  "solo_pre",
  "establecimiento", "base2022",        "con_datos_2022",  "solo_pre",
  "firma",           "contemporaneos",  "sin_restringir",  "solo_pre",
  "firma",           "contemporaneos",  "con_datos_2022",  "solo_pre",
  "firma",           "base2022",        "con_datos_2022",  "solo_pre",
  "firma",           "base2022",        "con_datos_2022",  "completo"
)

datos_conciliacion <- function(panel, muestra, datos) {
  d <- if (panel == "firma") panel_firma else panel_est_conc
  if (datos == "solo_pre") d <- filter(d, ANIO <= ANIO_BASE)
  if (muestra == "con_datos_2022") d <- filter(d, !is.na(CIIU4_2022), !is.na(tamano_2022))
  if (muestra == "con_datos_2022" && panel == "firma") d <- filter(d, !is.na(DPTO_2022))
  d %>% mutate(ANIO_F = droplevels(ANIO_F))
}

fe_conciliacion <- function(panel, controles) {
  if (panel == "firma") return(fe_modelo(controles))
  switch(controles,
         contemporaneos = "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F",
         base2022       = "NORDEST + ANIO_F + CIIU4_2022^ANIO_F + tamano_2022^ANIO_F + DPTO_fijo^ANIO_F"
  )
}

tabla_conciliacion <- especificaciones_conc %>%
  tidyr::expand_grid(outcome = outcomes, medida = medidas) %>%
  rowwise() %>%
  mutate(res = list({
    d <- datos_conciliacion(panel, muestra, datos)
    f <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", medida, ", ref = '", ANIO_BASE, "') | ",
                           fe_conciliacion(panel, controles)))
    m <- feols(f, data = d, cluster = ~NORDEMP, notes = FALSE)
    tibble(p_conjunto_pre = fixest::wald(m, keep = patron_pre, print = FALSE)$p,
           n_obs = nobs(m), n_firmas = n_clusters(m, d))
  })) %>%
  ungroup() %>%
  tidyr::unnest(res)

guardar_tabla_word(tabla_conciliacion, "tabla_conciliacion_ventana_pre.docx",
                   "Conciliación: prueba conjunta pre (2015-2019, 2021 vs. 2022) por panel, controles, muestra y datos",
                   digits = 4, carpeta = dir_valid)

# Vista compacta: p-valores por outcome, una tabla por medida
for (med in medidas) {
  cat("\n==== ", med, " ====\n")
  print(
    tabla_conciliacion %>%
      filter(medida == med) %>%
      select(panel, controles, muestra, datos, outcome, p_conjunto_pre, n_obs) %>%
      tidyr::pivot_wider(names_from = outcome, values_from = c(p_conjunto_pre)) %>%
      group_by(panel, controles, muestra, datos) %>%
      summarise(n_obs = max(n_obs), across(all_of(outcomes), ~ round(max(.x, na.rm = TRUE), 4)), .groups = "drop") %>%
      arrange(panel, datos, muestra, controles),
    width = Inf
  )
}

# ------------------------------------------------------------------
# 5) Estimación -- especificación principal
# ------------------------------------------------------------------
# Tres versiones por celda:
#   sin_tendencia       : y ~ post_2023:medida | FE
#   tendencia_conjunta  : y ~ post_2023:medida + anio_lineal:medida | FE
#                         (versión original; 2023-2024 también informan la tendencia)
#   tendencia_solo_pre  : [C7] 1) se estima la tendencia en ANIO <= 2022,
#                         2) se resta a y en todos los años, 3) se estima post.
#                         Los EE del paso 3 no incorporan la incertidumbre del
#                         paso 1 (quedan algo subestimados).

estimar_post <- function(data, outcome, medida, version, controles = "base2022") {
  fe <- fe_modelo(controles)
  termino <- paste0("post_2023:", medida)
  
  if (version == "sin_tendencia") {
    f <- as.formula(paste0(outcome, " ~ post_2023:", medida, " | ", fe))
  } else if (version == "tendencia_conjunta") {
    f <- as.formula(paste0(outcome, " ~ post_2023:", medida, " + anio_lineal:", medida, " | ", fe))
  } else if (version == "tendencia_solo_pre") {
    f_pre <- as.formula(paste0(outcome, " ~ anio_lineal:", medida, " | ", fe))
    m_pre <- feols(f_pre, data = filter(data, ANIO <= ANIO_BASE), cluster = ~NORDEMP)
    pendiente <- coef(m_pre)[paste0("anio_lineal:", medida)]
    data <- data %>% mutate(.y_sin_tend = .data[[outcome]] - pendiente * anio_lineal * .data[[medida]])
    f <- as.formula(paste0(".y_sin_tend ~ post_2023:", medida, " | ", fe))
  } else {
    stop("Versión desconocida: ", version)
  }
  
  m <- feols(f, data = data, cluster = ~NORDEMP)
  ct <- coeftable(m)[termino, ]
  tibble(beta = ct[["Estimate"]], ee = ct[["Std. Error"]], p = ct[["Pr(>|t|)"]],
         n_obs = nobs(m), n_firmas = n_clusters(m, data))
}

versiones <- c("sin_tendencia", "tendencia_conjunta", "tendencia_solo_pre")

## 5.1 -- Tabla principal (controles 2022) --------------------------

tabla_estimacion_principal <- tidyr::expand_grid(muestra = muestras, outcome = outcomes,
                                                 medida = medidas, version = versiones) %>%
  rowwise() %>%
  mutate(res = list(estimar_post(filtrar_muestra(panel_firma, muestra), outcome, medida, version))) %>%
  ungroup() %>%
  tidyr::unnest(res)
print(tabla_estimacion_principal, n = Inf)
guardar_tabla_word(tabla_estimacion_principal, "tabla_estimacion_principal.docx",
                   "Post 2023 x exposición (+1 DE), controles fijados en 2022, cluster NORDEMP",
                   carpeta = dir_estim)

## 5.2 -- Comparación: controles contemporáneos vs. fijados en 2022 --
# [C1] Solo para documentar el cambio respecto a la versión anterior.

tabla_comparacion_controles <- tidyr::expand_grid(outcome = outcomes, medida = medidas,
                                                  controles = c("base2022", "contemporaneos")) %>%
  rowwise() %>%
  mutate(res = list(estimar_post(panel_firma, outcome, medida, "sin_tendencia", controles))) %>%
  ungroup() %>%
  tidyr::unnest(res)
print(tabla_comparacion_controles, n = Inf)
guardar_tabla_word(tabla_comparacion_controles, "tabla_comparacion_controles.docx",
                   "Sin tendencia, muestra completa: controles fijados en 2022 vs. contemporáneos",
                   carpeta = dir_estim)

# ------------------------------------------------------------------
# 6) Robustez
# ------------------------------------------------------------------
## 6.1 -- Escrutinio de celdas significativas en ambas versiones ----
# [C8] Regla de activación (la misma que se usaba, ahora automática): celda
# significativa en la muestra común tanto sin tendencia como con tendencia
# solo-pre. Ojo: cuadrática y leave-one-year-out siguen suponiendo una
# tendencia paramétrica; si la pre-tendencia se rechaza, no rescatan el
# resultado (para eso: Honest DiD).

celdas_escrutinio <- tabla_estimacion_principal %>%
  filter(muestra == "comun", version %in% c("sin_tendencia", "tendencia_solo_pre")) %>%
  group_by(outcome, medida) %>%
  filter(all(p < UMBRAL_P)) %>%
  distinct(outcome, medida) %>%
  ungroup()
print(celdas_escrutinio)

resultados_robustez <- list()
datos_comun <- filtrar_muestra(panel_firma, "comun")
fe_principal <- fe_modelo("base2022")

for (i in seq_len(nrow(celdas_escrutinio))) {
  outcome <- celdas_escrutinio$outcome[i]
  medida  <- celdas_escrutinio$medida[i]
  termino <- paste0("post_2023:", medida)
  
  f_cuadratica <- as.formula(paste0(
    outcome, " ~ post_2023:", medida, " + anio_lineal:", medida,
    " + I(anio_lineal^2):", medida, " | ", fe_principal
  ))
  m_cuad <- feols(f_cuadratica, data = datos_comun, cluster = ~NORDEMP)
  resultados_robustez[[paste(outcome, medida, "cuadratica")]] <- tibble(
    outcome = outcome, medida = medida, prueba = "tendencia_cuadratica_conjunta",
    beta = coeftable(m_cuad)[termino, "Estimate"], p = coeftable(m_cuad)[termino, "Pr(>|t|)"]
  )
  
  for (anio_excluido in ANIOS_PRE_TODOS) {
    res <- estimar_post(filter(datos_comun, ANIO != anio_excluido), outcome, medida, "tendencia_solo_pre")
    resultados_robustez[[paste(outcome, medida, anio_excluido)]] <- tibble(
      outcome = outcome, medida = medida, prueba = paste0("solo_pre_excluye_", anio_excluido),
      beta = res$beta, p = res$p
    )
  }
}

tabla_robustez <- bind_rows(resultados_robustez)
if (nrow(tabla_robustez) > 0) {
  print(tabla_robustez, n = Inf)
  guardar_tabla_word(tabla_robustez, "tabla_robustez_escrutinio.docx",
                     "Escrutinio (tendencia cuadrática y leave-one-year-out) de celdas significativas en ambas versiones, muestra común",
                     carpeta = dir_robustez)
} else {
  message("Ninguna celda activó el escrutinio en la muestra común.")
}

# ------------------------------------------------------------------
# 7) Extensiones
# ------------------------------------------------------------------
