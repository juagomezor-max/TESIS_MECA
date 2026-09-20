# Rigideces laborales y decisiones de la firma

Tesis de Maestría en Economía Aplicada — Universidad de los Andes
Julio Gómez y Nicolás Jácome · Director: Andrés Ham

Estudio de la respuesta de las firmas manufactureras colombianas al aumento
del salario mínimo de 2023, usando la Encuesta Anual Manufacturera del DANE.

## Cómo reproducir

**Importante:** el pipeline de construcción y los scripts de análisis corren
desde directorios de trabajo distintos. Respetar esto o los scripts fallan.

### 1. Entorno

    renv::restore()     # R 4.5.2

### 2. Construir la base

Con `0. ANALISIS INICIAL IA/` como directorio de trabajo:

    source("3. SCRIPTS/pipeline/run_all.R")

Orden interno: construir_macro_base_eam → 01_construir_base →
02_construir_exposicion → 03_construir_panel → 04_validaciones →
05_descriptivos.

Luego, para enriquecer y extender el panel:

    source("3. SCRIPTS/construccion/ampliar_variables_paneles.R")
    source("3. SCRIPTS/construccion/construir_panel_firma_con_2020.R")

### 3. Análisis

Desde la raíz del proyecto, abriendo `TESIS_MECA.Rproj`, en este orden:

| Script | Qué produce |
|---|---|
| `01_resultados_principales.R` | Resultado principal de empleo y primer eslabón |
| `01b_resultados_principales_con_2020.R` | El mismo análisis incluyendo 2020 |
| `02_validaciones.R` | Validaciones V1 a V12 |
| `03_tratamiento_continuo.R` | Tratamiento continuo C1 a C8 |
| `04_descriptivos_evolucion.R` | Descriptivos D1 a D8 |

### Verificación

Si estas cifras no coinciden, algo salió mal antes de estimar:

- Panel: 62.816 filas, 183 columnas, 9.087 firmas
- Muestra de análisis: 5.099 firmas, 44.631 firma-años
- Años: 2015-2019 y 2021-2024 (2020 excluido por la pandemia)

### Datos

Los microdatos de la EAM no están en el repositorio. Se descargan desde
Zenodo antes de correr el pipeline.
