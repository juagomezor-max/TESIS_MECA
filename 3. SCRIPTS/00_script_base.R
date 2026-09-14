# ==============================================================================
# 00_script_base.R
#
# Título:  Script base -- análisis manual del efecto del salario mínimo 2023
#          sobre firmas manufactureras (EAM)
# Autor:   Julio Gómez
# Fecha:   2026-09-15
#
# Este script se ejecuta con el directorio de trabajo en la RAÍZ del
# repositorio (todas las rutas de abajo son relativas a la raíz, no
# absolutas -- si usas RStudio, abre la carpeta del repositorio como
# directorio de trabajo, o usa setwd() a la raíz antes de correr esto).
#
# Qué hay en "1. DATOS/":
#   - panel_analitico_firma_eam.{rds,csv,dta}: panel EMPRESA-AÑO
#     (NORDEMP-ANIO, 62,816 filas). Es el panel OFICIAL de la
#     especificación PRINCIPAL de la tesis (sección 4.4) -- úsalo para
#     estimar el efecto promedio del choque de 2023 sobre la firma.
#   - panel_establecimiento_formal.{rds,csv,dta}: panel
#     ESTABLECIMIENTO-AÑO (NORDEST-ANIO, 68,447 filas). Úsalo para el
#     diseño DENTRO DE FIRMA (sección 4.5: heterogeneidad mono/
#     multiplanta con Multi_f, identificación dentro de firma con
#     efectos fijos firma×año).
#   Ver "1. DATOS/CODEBOOK.md" para el detalle de cada columna
#   (definición, unidad, fuente, cobertura) y "1. DATOS/README.md" para
#   cómo regenerar estos archivos si hace falta.
#
# Todo el trabajo de construcción/estimación/validación hecho con
# asistencia de Claude Code hasta el 2026-09-15 quedó archivado en
# "0. ANALISIS INICIAL IA/" (scripts originales, documentación completa
# en INDICE_RESULTADOS.md y BORRADOR_RESULTADOS.md dentro de esa carpeta).
# ==============================================================================

# ------------------------------------------------------------------
# 1) Librerías
# ------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(fixest)

# ------------------------------------------------------------------
# 2) Carga de los 2 paneles (rutas relativas a la raíz del proyecto)
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds"))
panel_establecimiento <- readr::read_rds(file.path("1. DATOS", "panel_establecimiento_formal.rds"))

# ==============================================================================
# 3) SANITY CHECK (celda cerrada) -- confirma que tu entorno reproduce
#    exactamente los números ya reportados antes de construir nada nuevo.
#
#    Especificación: empleo_total ~ post_2023:exposicion_10pp, panel de
#    FIRMA (empresa-año), controles sector(CIIU4)×año + tamaño×año +
#    departamento(DPTO)×año, cluster ~NORDEMP. Se compara la versión SIN
#    tendencia lineal pre-existente contra la versión CON tendencia
#    (agrega anio_lineal:exposicion_10pp) -- mismo patrón usado en toda
#    la ronda de robustez archivada.
#
#    Números esperados (comparar_especificacion_principal_firma_vs_
#    establecimiento.R, commit 924200f, panel="Firma (empresa-anio)"):
#      Sin tendencia: p = 0.206
#      Con tendencia: p = 0.655
# ==============================================================================

# Filtramos a la muestra de estimación: controles no faltantes y
# Exposure2022_obreros no faltante (misma muestra que el script original).
datos_sanity <- panel_firma %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO), !is.na(tamano_empresa), !is.na(Exposure2022_obreros))

# Fórmula SIN tendencia: solo el término post x exposición.
f_sin_tendencia <- empleo_total ~ post_2023:exposicion_10pp |
  NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO^ANIO_F

modelo_sin_tendencia <- fixest::feols(
  f_sin_tendencia,
  data = datos_sanity,
  cluster = ~NORDEMP
)

# Fórmula CON tendencia: se agrega anio_lineal:exposicion_10pp (tendencia
# lineal pre-existente interactuada con la exposición) junto al término
# post x exposición.
f_con_tendencia <- empleo_total ~ post_2023:exposicion_10pp + anio_lineal:exposicion_10pp |
  NORDEMP + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO^ANIO_F

modelo_con_tendencia <- fixest::feols(
  f_con_tendencia,
  data = datos_sanity,
  cluster = ~NORDEMP
)

# Extraemos el p-valor del término post x exposición en cada versión y
# los comparamos contra los valores ya reportados.
p_sin_tendencia <- summary(modelo_sin_tendencia)$coeftable["post_2023:exposicion_10pp", "Pr(>|t|)"]
p_con_tendencia <- summary(modelo_con_tendencia)$coeftable["post_2023:exposicion_10pp", "Pr(>|t|)"]

cat("Sanity check -- empleo_total ~ post_2023:exposicion_10pp, panel de firma\n")
cat("  Sin tendencia: p =", round(p_sin_tendencia, 3), "(esperado: 0.206)\n")
cat("  Con tendencia: p =", round(p_con_tendencia, 3), "(esperado: 0.655)\n")
cat("  Coincide sin tendencia:", isTRUE(all.equal(round(p_sin_tendencia, 3), 0.206)), "\n")
cat("  Coincide con tendencia:", isTRUE(all.equal(round(p_con_tendencia, 3), 0.655)), "\n")

# ==============================================================================
# 4) A partir de aquí, análisis manual de Julio.
# ==============================================================================

## siguiente paso: ...
