# TESIS_MECA - Guia Descriptiva Del Flujo

Este documento explica con mas detalle como esta organizado el proyecto, que hace cada script y como interpretar las carpetas y salidas.

## 1. Logica general del proyecto

El repositorio esta organizado como un pipeline reproducible en R:

1. inspeccion de archivos fuente
2. construccion de metadatos
3. consolidacion de la macrobase EAM
4. diagnostico longitudinal del identificador empresarial
5. analisis descriptivo preliminar de exposicion a choques laborales

La idea es que cualquier persona pueda restaurar el entorno, correr el flujo desde la raiz y regenerar tanto las bases derivadas como los graficos.

## 2. Estructura de carpetas

### `0. PREPARACION`

Carpeta para notas y material metodologico liviano.

Hoy ya no almacena las tablas del diccionario maestro; esas pasan a `1. DATOS/3. DICCIONARIOS/`.

### `1. DATOS`

Carpeta no versionada donde viven:

- fuentes originales EAM y EAC
- diccionarios construidos
- inventarios tabulares
- macrobase EAM
- bases derivadas y tablas auxiliares

Subcarpetas esperadas:

- `1. EAM/`: ZIP y documentos fuente de la EAM
- `2. EAC/`: ZIP fuente de la EAC
- `3. DICCIONARIOS/`: salidas del diccionario maestro
- `4. ANALISIS_INICIAL/`: inventarios y resumentes tabulares del barrido inicial
- `5. MACROBASE/`: macrobase consolidada EAM y sus tablas asociadas
- `6. BASES_DERIVADAS/`: salidas tabulares posteriores a la macrobase

### `2. PROCESAMIENTO`

Temporales regenerables del pipeline. No deben tratarse como salidas finales.

Subcarpetas temporales:

- `_tmp_unzip/`
- `_tmp_diccionario/`
- `_tmp_macro_base_eam/`

### `3. SCRIPTS`

Contiene toda la logica del proyecto.

Scripts clave:

- `_utils_proyecto.R`
- `00_ejecutar_flujo_eam.R`
- `00_limpiar_temporales.R`
- `analisis_eam_eac.R`
- `construir_diccionario_maestro.R`
- `construir_macro_base_eam.R`
- `diagnostico_panel_nordemp_eam.R`
- `descriptivo_exposicion_eam.R`

### `4. RESULTADOS`

Contiene principalmente graficos y otras salidas visuales ligeras.

Subcarpetas activas:

- `panel_diagnostico/`
- `descriptivos_exposicion/`

## 3. Script maestro del flujo

El punto de entrada recomendado es:

```powershell
Rscript "3. SCRIPTS/00_ejecutar_flujo_eam.R"
```

Ese script:

1. verifica y crea la estructura esperada de carpetas
2. ejecuta el analisis inicial de archivos EAM
3. construye el diccionario maestro
4. construye la macrobase EAM
5. corre el diagnostico de panel
6. corre el descriptivo de exposicion

Con esto queda regenerado el flujo principal de EAM.

## 4. Que hace cada script

### `analisis_eam_eac.R`

Hace un barrido de archivos comprimidos por fuente y anio.

Produce:

- inventario de archivos
- resumen por fuente/anio
- tabla de variables mas comunes
- grafico de cobertura por anio

Salidas:

- tablas en `1. DATOS/4. ANALISIS_INICIAL/`
- grafico en `4. RESULTADOS/`

### `construir_diccionario_maestro.R`

Integra dos fuentes de metadatos:

- el diccionario documental en Word
- los labels/nombres de variables observados en DTA

Produce:

- `diccionario_word_extraido.csv`
- `metadatos_dta_variables.csv`
- `diccionario_maestro_variables.csv`
- `variables_sin_descripcion.csv`

Salidas:

- `1. DATOS/3. DICCIONARIOS/`

### `construir_macro_base_eam.R`

Lee los ZIP anuales de EAM, extrae el DTA principal de cada anio, estandariza nombres y consolida una sola macrobase con trazabilidad.

Produce:

- `macro_base_eam.rds`
- `macro_base_eam_codebook.csv`
- `macro_base_eam_resumen.csv`

Salidas:

- `1. DATOS/5. MACROBASE/`

### `diagnostico_panel_nordemp_eam.R`

Evalua si `NORDEMP` es una llave razonable para analisis panel:

- duplicados por empresa-anio
- cantidad de firmas por anio
- permanencia por firma
- discontinuidades temporales

Salidas:

- tablas en `1. DATOS/6. BASES_DERIVADAS/panel_diagnostico/`
- grafico en `4. RESULTADOS/panel_diagnostico/`

### `descriptivo_exposicion_eam.R`

Construye variables laborales y productivas, define proxies de exposicion y genera descriptivos previos a cualquier estimacion econometrica.

Choques considerados:

- reforma tributaria de 2012
- aumento fuerte del salario minimo en 2023

Salidas:

- base reducida y tablas en `1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/`
- graficos en `4. RESULTADOS/descriptivos_exposicion/`

## 5. Flujo recomendado de ejecucion manual

Si no quieres usar el script maestro, el orden recomendado es:

1. `Rscript "3. SCRIPTS/construir_diccionario_maestro.R"`
2. `Rscript "3. SCRIPTS/construir_macro_base_eam.R"`
3. `Rscript "3. SCRIPTS/diagnostico_panel_nordemp_eam.R"`
4. `Rscript "3. SCRIPTS/descriptivo_exposicion_eam.R"`

Opcionalmente, antes de eso:

`Rscript "3. SCRIPTS/analisis_eam_eac.R" EAM`

Y cuando quieras limpiar temporales:

`Rscript "3. SCRIPTS/00_limpiar_temporales.R"`

## 6. Convenciones de reproducibilidad

- El proyecto usa `renv`; la primera accion debe ser `renv::restore()`.
- Los scripts asumen ejecucion desde la raiz del repositorio.
- Las salidas de datos van a `1. DATOS/`.
- Las figuras van a `4. RESULTADOS/`.
- Los temporales van a `2. PROCESAMIENTO/`.
- `_utils_proyecto.R` centraliza rutas y utilidades comunes para reducir inconsistencias.

## 7. Control de versiones y limites del repo

`1. DATOS/` esta ignorada en git. Eso significa:

- las bases y tablas derivadas no se versionan
- si quieres compartir resultados tabulares, debes regenerarlos localmente
- lo que si queda versionado es la logica para reconstruirlos

Tambien estan ignorados los temporales de `2. PROCESAMIENTO/_tmp_*`.

## 8. Estado actual del flujo

Hoy el proyecto queda listo para:

- reconstruir el diccionario maestro
- regenerar la macrobase EAM
- diagnosticar la estructura panel por `NORDEMP`
- producir descriptivos exploratorios de exposicion para EAM

La extension natural a futuro es agregar una macrobase EAC y, mas adelante, una capa comun de variables homologadas entre EAM y EAC.
