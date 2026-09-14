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

# ------------------------------------------------------------------
# 1) Librerías
# ------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(fixest)

# ------------------------------------------------------------------
# 2) Carga de datos
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds"))
panel_establecimiento <- readr::read_rds(file.path("1. DATOS", "panel_establecimiento_formal.rds"))

# ------------------------------------------------------------------
# 3) Estadísticas descriptivas
# ------------------------------------------------------------------



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