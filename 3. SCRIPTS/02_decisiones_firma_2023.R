# ============================================================
# TESIS MECA
# DECISIONES DE FIRMA ANTE EL AUMENTO DEL SML DE 2023
#
# Este script parte de las bases procesadas en el pipeline
# anterior. No reconstruye la macrobase EAM.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

# ------------------------------------------------------------
# 1. CARGAR PUNTO DE PARTIDA
# ------------------------------------------------------------

ruta_checkpoint <- file.path(
  "1. DATOS",
  "6. BASES_DERIVADAS",
  "checkpoint_decisiones_firma_2023.rds"
)

if (!file.exists(ruta_checkpoint)) {
  stop(
    "No existe el checkpoint. Ejecute primero la sección 54 ",
    "del script anterior."
  )
}

checkpoint <- readRDS(ruta_checkpoint)

base_analitica <- checkpoint$base_analitica
panel_principal <- checkpoint$panel_principal
panel_balanceado <- checkpoint$panel_balanceado

rm(checkpoint)

# ------------------------------------------------------------
# 1.1. VERIFICACIÓN MÍNIMA
# ------------------------------------------------------------

variables_minimas <- c(
  "NORDEMP",
  "NORDEST",
  "ANIO",
  "Exposure2022_obreros"
)

faltantes <- setdiff(
  variables_minimas,
  names(panel_principal)
)

if (length(faltantes) > 0) {
  stop(
    "Faltan estas variables: ",
    paste(faltantes, collapse = ", ")
  )
}

message("Base cargada correctamente.")

# ============================================================
# 1.1. CONSTRUCCIÓN Y REVISIÓN DEL EMPLEO ASALARIADO
# ============================================================
#
# En esta sección se construye el empleo asalariado total como
# la suma del personal permanente, temporal directo, temporal
# contratado por agencias y aprendices. Luego se identifica
# cuántas observaciones tienen empleo asalariado igual a cero,
# pues en esos casos no puede calcularse el logaritmo.
# ============================================================

panel_princiapl <- panel_principal |>
  dplyr::mutate(
    empleo_asalariado =
      personal_permanente_total +
      personal_temporal_directo_total +
      personal_temporal_agencia_total +
      personal_aprendices_total
  )

# ------------------------------------------------------------
# REVISIÓN DE CEROS Y DATOS FALTANTES
# ------------------------------------------------------------

diagnostico_empleo_asalariado <- panel_principal |>
  dplyr::summarise(
    observaciones_totales = dplyr::n(),
    
    empleo_asalariado_cero = sum(
      empleo_asalariado == 0,
      na.rm = TRUE
    ),
    
    porcentaje_cero = round(
      100 * mean(empleo_asalariado == 0, na.rm = TRUE),
      2
    ),
    
    empleo_asalariado_faltante = sum(
      is.na(empleo_asalariado)
    ),
    
    porcentaje_faltante = round(
      100 * mean(is.na(empleo_asalariado)),
      2
    ),
    
    empleo_asalariado_positivo = sum(
      empleo_asalariado > 0,
      na.rm = TRUE
    )
  )

diagnostico_empleo_asalariado