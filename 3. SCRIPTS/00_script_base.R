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
library(knitr)
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
guardar_tabla_word <- function(tabla, nombre_archivo, caption = NULL, digits = 3) {
  ft <- flextable::flextable(tabla)
  ft <- flextable::colformat_double(ft, digits = digits)
  ft <- flextable::autofit(ft)
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption = caption)
  }
  ruta <- file.path(dir_desc, nombre_archivo)
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
# ------------------------------------------------------------------
# 4) Validación de supuestos (tendencias paralelas, etc.)
# ------------------------------------------------------------------



# ------------------------------------------------------------------
# 5) Estimación -- especificación principal
# ------------------------------------------------------------------



# ------------------------------------------------------------------
# 6) Robustez
# ------------------------------------------------------------------



# ------------------------------------------------------------------
# 7) Extensiones
# ------------------------------------------------------------------