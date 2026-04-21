# TESIS_MECA

Repositorio para analisis reproducible de microdatos EAM y EAC en R.

## Estructura del proyecto

- `0. PREPARACION/`: notas y material de preparacion del proyecto.
- `1. DATOS/`: fuentes originales (no se versionan en Git).
- `2. PROCESAMIENTO/`: archivos temporales e intermedios del pipeline.
- `3. SCRIPTS/`: scripts de analisis en R.
- `4. RESULTADOS/`: salidas del analisis (tablas, graficos, reportes).

## Requisitos

- R 4.5+ recomendado.
- Paquete `renv` para restaurar el entorno.

## Configuracion del entorno reproducible

En la raiz del proyecto:

```r
renv::restore()
```

Este comando instala las versiones registradas en `renv.lock`.

## Ejecucion del analisis principal

Script principal:

- `3. SCRIPTS/analisis_eam_eac.R`

Primera base a analizar (EAM, por defecto):

```powershell
Rscript "3. SCRIPTS/analisis_eam_eac.R"
```

Ejecucion explicita por fuente:

```powershell
Rscript "3. SCRIPTS/analisis_eam_eac.R" EAM
Rscript "3. SCRIPTS/analisis_eam_eac.R" EAC
Rscript "3. SCRIPTS/analisis_eam_eac.R" ALL
```

## Macro base EAM

Para construir la base consolidada anual de la EAM:

```powershell
Rscript "3. SCRIPTS/construir_macro_base_eam.R"
```

Salidas generadas:

- `1. DATOS/5. MACROBASE/macro_base_eam.rds`
- `1. DATOS/5. MACROBASE/macro_base_eam_codebook.csv`
- `1. DATOS/5. MACROBASE/macro_base_eam_resumen.csv`

## Salidas esperadas

El script genera en `4. RESULTADOS/`:

- `inventario_archivos_eam.csv` (u otro sufijo segun fuente)
- `resumen_por_fuente_anio_eam.csv` (u otro sufijo segun fuente)
- `variables_mas_comunes_eam.csv` (u otro sufijo segun fuente)
- `plot_archivos_por_anio_eam.png` (u otro sufijo segun fuente)

Tambien utiliza `2. PROCESAMIENTO/_tmp_unzip` para descompresion temporal.

## Notas de versionamiento

- `1. DATOS/` esta excluida del control de versiones en `.gitignore`.
- `renv.lock` debe versionarse para garantizar replicabilidad.
