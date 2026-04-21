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

Desde la raiz del proyecto:

```powershell
Rscript "3. SCRIPTS/analisis_eam_eac.R"
```

## Salidas esperadas

El script genera en `4. RESULTADOS/`:

- `inventario_archivos.csv`
- `resumen_por_fuente_anio.csv`
- `variables_mas_comunes.csv`
- `plot_archivos_por_anio.png`

Tambien utiliza `2. PROCESAMIENTO/_tmp_unzip` para descompresion temporal.

## Notas de versionamiento

- `1. DATOS/` esta excluida del control de versiones en `.gitignore`.
- `renv.lock` debe versionarse para garantizar replicabilidad.
