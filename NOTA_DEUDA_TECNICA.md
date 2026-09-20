# Deuda técnica: estructura del pipeline

Diagnóstico de septiembre de 2026. No resuelto a propósito: el costo
excedía el beneficio antes de la defensa.

## El problema

El pipeline de construcción vive en `0. ANALISIS INICIAL IA/`, aunque ya no
es exploratorio: es la dependencia de todos los scripts de producción.

No se puede mover sin tocar 23 archivos, por dos razones:

1. **Bootstrap dependiente del directorio de trabajo.** Los 23 scripts
   empiezan con `source(file.path("3. SCRIPTS", "pipeline",
   "_utils_proyecto.R"))`. Esa línea corre antes de que exista cualquier
   función de anclaje, así que arreglar `get_project_paths()` no la
   soluciona: el problema no es qué construye la función, es cómo se llega
   a ella.

2. **Dos scripts con rutas propias**, fuera de `get_project_paths()`:
   - `construccion/ampliar_variables_paneles.R:36-37`
   - `construccion/construir_panel_firma_con_2020.R:43`
     Ambos con `raiz <- ".."`, asumiendo que el directorio de trabajo es su
     propia carpeta.
   - `construccion/ampliar_variables_paneles.R:579` usa
     `file.path("0. PREPARACION", ...)` literal, inconsistente con el resto
     del mismo archivo, que sí usa `paths$`.

## La salida viable

Un `.Rprofile` en la raíz que localice `_utils_proyecto.R` por anclaje
(buscando `TESIS_MECA.Rproj` hacia arriba) y lo cargue automáticamente. Así
los 23 scripts no necesitan su línea `source()`.

Riesgo: cambia el arranque de toda sesión de R del proyecto, incluido el
trabajo manual en RStudio, y los `.Rprofile` fallan de forma silenciosa.
Los dos scripts con `raiz <- ".."` habría que editarlos igual.

## Estado

Documentado en el README cómo correr cada parte. La reproducibilidad está
garantizada siguiendo esas instrucciones. Reorganizar queda para después
de la defensa.
