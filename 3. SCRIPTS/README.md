# 3. SCRIPTS

Esta carpeta contiene los scripts ejecutables del proyecto, organizados en subcarpetas por funcion (reorganizacion mecanica de archivos, 2026-09-02 -- sin cambios de logica). Todos los scripts siguen suponiendo que se ejecutan desde la RAIZ del repositorio (no desde dentro de `3. SCRIPTS/` ni de sus subcarpetas).

## Mapa de subcarpetas

### `pipeline/` -- flujo principal, de punta a punta

Utilidades compartidas y los dos pipelines reproducibles del proyecto: el ETL crudo -> macrobase, y el pipeline analitico simplificado que parte de la macrobase.

- `_utils_proyecto.R`: utilidades compartidas de rutas, paquetes y validaciones (todo lo demas la fuente vía `source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))`)
- `00_ejecutar_flujo_eam.R`: corre el flujo ETL completo (pasos 1-4 de abajo)
- `00_limpiar_temporales.R`: reinicia temporales regenerables en `2. PROCESAMIENTO/`
- `analisis_eam_eac.R`: inventario exploratorio de archivos por fuente
- `construir_diccionario_maestro.R`: construccion del diccionario integrado EAM/EAC
- `construir_macro_base_eam.R`: consolidacion anual de la macrobase EAM
- `diagnostico_panel_nordemp_eam.R`: chequeos de consistencia longitudinal de NORDEMP
- `descriptivo_exposicion_eam.R`: descriptivos exploratorios de exposicion a shocks laborales
- `01_construir_base.R` a `05_descriptivos.R` + `run_all.R`: pipeline analitico simplificado a nivel de FIRMA (parte de la macrobase ya construida, ver `README.md` de la raiz)
- `opcional_establecimiento.R`: modulo opcional a nivel de establecimiento (NORDEST), no lo corre `run_all.R`
- `verificar_cifras_clave.R`: control de calidad -- produce `CIFRAS_CLAVE.csv`

Uso minimo (flujo ETL):

```powershell
Rscript "3. SCRIPTS/pipeline/00_ejecutar_flujo_eam.R"
```

Pipeline analitico simplificado: ver seccion "Pipeline analitico simplificado" del `README.md` de la raiz.

### `construccion/` -- construccion de variables intermedias (nivel establecimiento)

`construir_conteo_personal_categoria_eam.R`, `construir_conteo_personal_categoria_establecimiento_eam.R`, `construir_exposicion_obreros_establecimiento_eam.R`, `construir_panel_efectivo_especificacion_b_por_anio.R`.

### `validaciones/` -- validaciones de identificacion del diseno DiD

Tendencias paralelas (Exposure y Bite, firma y establecimiento), atricion diferencial, comparaciones de robustez (IID vs. cluster, matriz funcional Exposure/Bite, exposure establecimiento vs. firma, multiplanta vs. monoplanta) y diagnosticos asociados. Incluye `graficar_participacion_permanente_por_quintil_bite_vs_exposure.R` (contraste grafico de la hipotesis de tendencia previa monotonica).

### `auditorias/` -- auditorias de calidad de los identificadores de panel

Confiabilidad de `NORDEST` como ID de establecimiento, estabilidad y cobertura de `DPTO`, deduplicacion `NORDEMP`-año, recodificacion multiplanta, comparacion contra cifras DANE.

### `descriptivos/` -- descriptivos de la estructura multiplanta 2022

Peso de empleo por variacion interna, descomposicion de salidas de la cohorte 2022, estructura multiplanta 2022.

### `verificaciones/` -- verificaciones puntuales de cobertura/estabilidad de variables

Cobertura de exposicion, consistencia cruzada multiplanta 2022, estabilidad de columnas C3R/C4R, exclusion de prestaciones del salario obrero, nombres de columnas de la macrobase.

### `exploratorio_nicolas/` -- script exploratorio del compañero (nunca validado)

`construir_base_analitica_nicolas.R` (antes `3. SCRIPTS/3. SCRIPTS/construir_base analitica.R`, renombrado al mover para quitar el espacio y la carpeta anidada). Nunca validado ni usado en ninguna cifra reportada -- ver `0. PREPARACION/README_SIMPLIFICACION.md`.

## En la raiz de esta carpeta

- `Analisis_EAM_Markdown.Rmd`: notebook exploratorio
- `README.md`: este archivo
