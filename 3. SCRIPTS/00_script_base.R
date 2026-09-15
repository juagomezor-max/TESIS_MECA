# ==============================================================================
# 00_script_base.R
#
# Título:  Análisis manual del efecto del salario mínimo 2023 sobre firmas
#          manufactureras (EAM)
# Autor:   Julio Gómez
# Fecha:   2026-09-15
#
# Se ejecuta con el directorio de trabajo en la RAÍZ del repositorio.
#
# "1. DATOS/":
#   - panel_analitico_firma_eam.{rds,csv,dta}      -> panel EMPRESA-AÑO
#   - panel_establecimiento_formal.{rds,csv,dta}   -> panel ESTABLECIMIENTO-AÑO
#   Ver CODEBOOK.md (definición de columnas) y README.md (cómo regenerarlos)
#   en esa misma carpeta.
#
# El trabajo previo con asistencia de Claude Code (construcción, estimación,
# validación) está archivado en "0. ANALISIS INICIAL IA/".
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
# install.packages("flextable")
library(flextable)

# ------------------------------------------------------------------
# 2) Carga de datos
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds"))
panel_establecimiento <- readr::read_rds(file.path("1. DATOS", "panel_establecimiento_formal.rds"))

# ------------------------------------------------------------------
# 3) Estadísticas descriptivas
# ------------------------------------------------------------------

dir_desc <- file.path("4. RESULTADOS", "Descriptivos")
dir_fig  <- file.path(dir_desc, "figuras")
dir.create(dir_desc, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

# Helper para guardar tablas en Word
guardar_tabla_word <- function(tabla, nombre_archivo, caption = NULL, digits = 3, carpeta = dir_desc) {
  ft <- flextable::flextable(tabla)
  ft <- flextable::colformat_double(ft, digits = digits)
  ft <- flextable::autofit(ft)
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption = caption)
  }
  ruta <- file.path(carpeta, nombre_archivo)
  flextable::save_as_docx(ft, path = ruta)
  message("Guardado: ", ruta)
  tabla
}

# Tema para los histogramas
tema_hist <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11)
  )

## 3.1 -- Estructura del panel ------------------------------------

tabla_obs_por_anio <- panel_firma %>%
  count(ANIO_F, name = "n_firmas")
guardar_tabla_word(tabla_obs_por_anio, "tabla_obs_por_anio.docx",
                   "Observaciones por año, panel de firma")

tabla_balance_panel <- panel_firma %>%
  count(NORDEMP) %>%
  count(n, name = "n_firmas") %>%
  arrange(desc(n)) %>%
  rename(anios_presente = n)
guardar_tabla_word(tabla_balance_panel, "tabla_balance_panel.docx",
                   "Firmas según número de años presentes en el panel")

## 3.2 -- Medidas de exposición ------------------------------------

tabla_resumen_exposicion <- panel_firma %>%
  filter(ANIO_F == 2022) %>%
  summarise(
    across(
      c(Exposure2022_obreros, Bite2022_obreros),
      list(
        Media = ~mean(.x, na.rm = TRUE),
        DE = ~sd(.x, na.rm = TRUE),
        Mín = ~min(.x, na.rm = TRUE),
        P25 = ~quantile(.x, 0.25, na.rm = TRUE),
        Mediana = ~median(.x, na.rm = TRUE),
        P75 = ~quantile(.x, 0.75, na.rm = TRUE),
        Máx = ~max(.x, na.rm = TRUE),
        pct_en_0 = ~mean(.x == 0, na.rm = TRUE),
        pct_en_1 = ~mean(.x == 1, na.rm = TRUE),
        N_faltante = ~sum(is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  tidyr::pivot_longer(everything(), names_to = c("Variable", "Estadístico"), names_sep = "__") %>%
  tidyr::pivot_wider(names_from = Variable, values_from = value)
guardar_tabla_word(tabla_resumen_exposicion, "tabla_resumen_exposicion.docx",
                   "Estadísticas descriptivas de las medidas de exposición (2022)")

tabla_correlacion_medidas <- panel_firma %>%
  filter(ANIO_F == 2022) %>%
  summarise(
    Pearson  = cor(Exposure2022_obreros, Bite2022_obreros, method = "pearson", use = "complete.obs"),
    Spearman = cor(Exposure2022_obreros, Bite2022_obreros, method = "spearman", use = "complete.obs")
  )
guardar_tabla_word(tabla_correlacion_medidas, "tabla_correlacion_medidas.docx",
                   "Correlación entre Exposure2022_obreros y Bite2022_obreros")

g1 <- panel_firma %>%
  filter(ANIO_F == 2022) %>%
  ggplot(aes(x = Exposure2022_obreros)) +
  geom_histogram(bins = 30, fill = "#1F4E79", color = "white", alpha = 0.9) +
  labs(
    title = "Distribución de Exposure2022_obreros",
    subtitle = "Proporción de obreros y operarios sobre el empleo total (corte 2022)",
    x = "Exposure2022_obreros", y = "Número de firmas"
  ) +
  tema_hist
ggsave(file.path(dir_fig, "hist_exposure.png"), g1, width = 7, height = 4.5, dpi = 200, bg = "white")

g2 <- panel_firma %>%
  filter(ANIO_F == 2022) %>%
  ggplot(aes(x = Bite2022_obreros)) +
  geom_histogram(bins = 30, fill = "#C00000", color = "white", alpha = 0.9) +
  labs(
    title = "Distribución de Bite2022_obreros",
    subtitle = "Índice de Kaitz: salario mínimo 2023 / salario promedio del obrero (corte 2022)",
    x = "Bite2022_obreros", y = "Número de firmas"
  ) +
  tema_hist
ggsave(file.path(dir_fig, "hist_bite.png"), g2, width = 7, height = 4.5, dpi = 200, bg = "white")

## 3.2b -- Investigación y tratamiento de valores extremos en Bite -------
#
# Se detectaron 23 firmas (0.45% de la muestra 2022, n=5,099) con
# Bite2022_obreros > 2 (p99 = 1.61). Investigación contra el panel crudo
# (panel_firma_eam.rds, archivado en 0. ANALISIS INICIAL IA/): las 5 firmas
# con los valores más extremos tienen 1 solo obrero reportado -- el
# "salario promedio" es, en esos casos, el salario de una sola persona,
# sin ningún efecto de promediar. El script de construcción original
# (02_construir_exposicion.R) winsoriza Exposure2022_obreros al 1%-99%
# pero NO aplica el mismo tratamiento a Bite2022_obreros (confirmado por
# inspección directa del código). Se decide winsorizar Bite2022_obreros
# al 1%-99%, por consistencia con el tratamiento ya aplicado a Exposure.

panel_firma_crudo <- readr::read_rds(file.path(
  "0. ANALISIS INICIAL IA", "1. DATOS", "6. BASES_DERIVADAS",
  "descriptivos_exposicion", "panel_firma_eam.rds"
))

# Diagnóstico: salario promedio del obrero y personal detrás de cada
# firma con Bite extremo (confirma la causa: N de obreros muy bajo)
panel_firma_crudo %>%
  filter(ANIO == 2022, NORDEMP %in% c("141501","142815","144128","144200",
                                      "144698","144940","145418","145902",
                                      "146564","366934")) %>%
  mutate(
    personal_permanente_obrero = C4R2C1 + C4R2C2,
    salario_promedio_obrero = ifelse(
      is.na(C3R2C1) | is.na(personal_permanente_obrero) | personal_permanente_obrero == 0,
      NA_real_, C3R2C1 / personal_permanente_obrero
    )
  ) %>%
  select(NORDEMP, C3R2C1, personal_permanente_obrero, salario_promedio_obrero) %>%
  arrange(personal_permanente_obrero)

# Winsorización -- se guarda como columna nueva, sin sobrescribir la
# variable original
winsorize <- function(x, probs = c(0.01, 0.99)) {
  limites <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, limites[1]), limites[2])
}

panel_firma <- panel_firma %>%
  mutate(Bite2022_obreros_wins = winsorize(Bite2022_obreros))

# Verificación: máximo antes/después y cuántas firmas quedaron afectadas
panel_firma %>%
  filter(ANIO_F == 2022) %>%
  summarise(
    max_original = max(Bite2022_obreros, na.rm = TRUE),
    max_winsorizado = max(Bite2022_obreros_wins, na.rm = TRUE),
    p99_original = quantile(Bite2022_obreros, 0.99, na.rm = TRUE),
    n_afectadas = sum(Bite2022_obreros != Bite2022_obreros_wins, na.rm = TRUE)
  )

## 3.3 -- Variables de resultado (empleo) --------------------------

tabla_resumen_empleo <- panel_firma %>%
  summarise(
    across(
      c(empleo_total, empleo_permanente, empleo_temporal, participacion_permanente),
      list(
        Media = ~mean(.x, na.rm = TRUE),
        DE = ~sd(.x, na.rm = TRUE),
        Mediana = ~median(.x, na.rm = TRUE),
        N_faltante = ~sum(is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  tidyr::pivot_longer(everything(), names_to = c("Variable", "Estadístico"), names_sep = "__") %>%
  tidyr::pivot_wider(names_from = Variable, values_from = value)
guardar_tabla_word(tabla_resumen_empleo, "tabla_resumen_empleo.docx",
                   "Estadísticas descriptivas de las variables de empleo")

tabla_empleo_por_anio <- panel_firma %>%
  group_by(ANIO_F) %>%
  summarise(across(
    c(empleo_total, empleo_permanente, empleo_temporal, participacion_permanente),
    ~mean(.x, na.rm = TRUE)
  ))
guardar_tabla_word(tabla_empleo_por_anio, "tabla_empleo_por_anio.docx",
                   "Promedio de variables de empleo por año")

## 3.4 -- Variables de mecanismo ------------------------------------

tabla_resumen_mecanismos <- panel_firma %>%
  summarise(
    across(
      c(C3R23C3, C3R41C3, C7R10C2, VALORVEN),
      list(
        Media = ~mean(.x, na.rm = TRUE),
        pct_en_0 = ~mean(.x == 0, na.rm = TRUE),
        N_faltante = ~sum(is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  tidyr::pivot_longer(everything(), names_to = c("Variable", "Estadístico"), names_sep = "__") %>%
  tidyr::pivot_wider(names_from = Variable, values_from = value)
guardar_tabla_word(tabla_resumen_mecanismos, "tabla_resumen_mecanismos.docx",
                   "Estadísticas descriptivas de las variables de mecanismo")

## 3.5 -- Controles ---------------------------------------------

tabla_tamano <- panel_firma %>%
  count(tamano_empresa, name = "n_firma_anio")
guardar_tabla_word(tabla_tamano, "tabla_tamano_empresa.docx",
                   "Distribución de tamaño de empresa")

tabla_top_sectores <- panel_firma %>%
  count(CIIU4, sort = TRUE, name = "n_firma_anio") %>%
  head(10)
guardar_tabla_word(tabla_top_sectores, "tabla_top10_sectores.docx",
                   "10 sectores (CIIU4) más frecuentes")

## 3.6 -- Estructura mono/multiplanta -------------------------------

tabla_multi <- panel_establecimiento %>%
  filter(ANIO_F == 2022) %>%
  distinct(NORDEMP, Multi_f) %>%
  count(Multi_f, name = "n_firmas")
guardar_tabla_word(tabla_multi, "tabla_mono_multiplanta.docx",
                   "Firmas mono vs. multiplanta (corte 2022)")

# ------------------------------------------------------------------
# 4) Validación de supuestos (tendencias paralelas, etc.)
# ------------------------------------------------------------------

dir_valid <- file.path("4. RESULTADOS", "Validaciones")
dir.create(dir_valid, recursive = TRUE, showWarnings = FALSE)

panel_pre <- panel_firma %>%
  filter(ANIO_F %in% 2015:2019)

outcomes <- c("empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente")
medidas  <- c("Exposure2022_obreros", "Bite2022_obreros_wins")

resultados_tendencias <- list()

for (outcome in outcomes) {
  for (medida in medidas) {
    
    f_sin_controles <- as.formula(paste0(
      outcome, " ~ i(ANIO_F, ", medida, ", ref = 2015) | NORDEMP + ANIO_F"
    ))
    m_sin_controles <- fixest::feols(f_sin_controles, data = panel_pre, cluster = ~NORDEMP)
    
    f_con_controles <- as.formula(paste0(
      outcome, " ~ i(ANIO_F, ", medida, ", ref = 2015) | ",
      "NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO^ANIO_F"
    ))
    m_con_controles <- fixest::feols(f_con_controles, data = panel_pre, cluster = ~NORDEMP)
    
    wald_sin <- fixest::wald(m_sin_controles, "ANIO_F")
    wald_con <- fixest::wald(m_con_controles, "ANIO_F")
    
    resultados_tendencias[[paste(outcome, medida)]] <- tibble::tibble(
      outcome = outcome,
      medida = medida,
      p_sin_controles = wald_sin$p,
      p_con_controles = wald_con$p
    )
  }
}

tabla_tendencias_paralelas <- dplyr::bind_rows(resultados_tendencias)
print(tabla_tendencias_paralelas)

guardar_tabla_word(tabla_tendencias_paralelas, "tabla_tendencias_paralelas.docx",
                   "Test conjunto de tendencias diferenciales pre-choque (2015-2019), sin y con controles",
                   carpeta = dir_valid)

# ------------------------------------------------------------------
# 5) Estimación -- especificación principal
# ------------------------------------------------------------------



# ------------------------------------------------------------------
# 6) Robustez
# ------------------------------------------------------------------



# ------------------------------------------------------------------
# 7) Extensiones
# ------------------------------------------------------------------