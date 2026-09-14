# Análisis inicial con IA (Claude Code)

Esta carpeta archiva la construcción de variables, estimación y validación desarrollada con asistencia de Claude Code hasta el 2026-09-15, conservada íntegra como primer análisis exploratorio previo a la fase de análisis manual de Julio Gómez.

La estructura interna (`0. PREPARACION/`, `1. DATOS/`, `2. PROCESAMIENTO/`, `3. SCRIPTS/`, `4. RESULTADOS/`, `descartado/`) y toda la documentación (`INDICE_RESULTADOS.md`, `BORRADOR_RESULTADOS.md`, `INVENTARIO_REPO.md`, `NOTA_PREANALISIS.md`) quedan sin cambios respecto a su ubicación original — solo se movieron un nivel más adentro, con su historial de git intacto.

Para las instrucciones completas de cómo reproducir este pipeline (requisitos de R, `renv::restore()`, orden de scripts), ver `README_PIPELINE_ANALISIS_ANTERIOR.md` en esta misma carpeta — es el README original del repositorio antes del reinicio, movido aquí sin modificar su contenido.

Las carpetas numeradas en la raíz del repositorio (fuera de esta carpeta) están vacías intencionalmente: son el punto de partida para el análisis manual nuevo.

**Nota técnica para reproducir cualquier script de aquí** (verificado 2026-09-15, `verificar_cifras_clave.R` reproduce 38/38 cifras): `renv/` se quedó en la raíz del repositorio (no se movió), así que R debe arrancar ahí para activar el entorno correcto — corre `Rscript -e 'setwd("0. ANALISIS INICIAL IA"); source("3. SCRIPTS/ruta/al/script.R")'` desde la raíz del repo, no `cd` directo a esta carpeta antes de invocar R.
