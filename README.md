# Rigideces laborales y decisiones de la firma

Tesis de Maestría en Economía Aplicada — Universidad de los Andes
Julio Gómez y Nicolás Jácome · Director: Andrés Ham

Estudio de la respuesta de las firmas manufactureras colombianas al aumento
del salario mínimo de 2023, usando la Encuesta Anual Manufacturera del DANE.

## Cómo reproducir

**Importante:** el pipeline de construcción y los scripts de análisis corren
desde directorios de trabajo distintos. Respetar esto o los scripts fallan.

### 1. Entorno

Requiere **R 4.5.2 exactamente**, no 4.6.x: `renv::restore()` falla al
compilar el paquete `S7` bajo R 4.6.1. En Windows, además, Rtools45.

    renv::restore()

Desde una terminal, en lugar de RStudio, la confirmación es interactiva:

    Rscript -e 'renv::restore(prompt = FALSE)'

### 2. Datos

Los microdatos de la EAM no están en el repositorio. Se descargan de Zenodo:

**https://zenodo.org/records/19675205**

Deben quedar así, dentro de `0. ANALISIS INICIAL IA/`:

    1. DATOS/
    ├── 1. EAM/
    │   ├── EAM_2008.zip ... EAM_2024.zip      (17 archivos, un .dta por zip)
    │   └── Diccionario de datos EAM2024.xlsx
    ├── 2. EAC/
    │   └── EAC_2009.zip ... EAC_2024.zip      (14 archivos)
    └── Diccionarios_EAM_EAC.docx

Los scripts descomprimen los zips sobre la marcha: no hay que hacerlo a mano.

### 3. Construir la base

Con `0. ANALISIS INICIAL IA/` como directorio de trabajo, **en este orden**.
El segundo paso falla si no se corrió el primero.

**Primero**, el diccionario maestro y la macrobase:

    Rscript "3. SCRIPTS/pipeline/00_ejecutar_flujo_eam.R"

Corre: analisis_eam_eac → construir_diccionario_maestro →
construir_macro_base_eam → diagnostico_panel_nordemp_eam →
descriptivo_exposicion_eam.

**Después**, el panel analítico:

    source("3. SCRIPTS/pipeline/run_all.R")

Corre: 01_construir_base → 02_construir_exposicion → 03_construir_panel →
04_validaciones → 05_descriptivos.

**Por último**, para enriquecer y extender el panel:

    source("3. SCRIPTS/construccion/ampliar_variables_paneles.R")
    source("3. SCRIPTS/construccion/construir_panel_firma_con_2020.R")

### 4. Análisis

Desde la raíz del proyecto, abriendo `TESIS_MECA.Rproj`, en este orden:

| Script | Qué produce |
|---|---|
| `01_resultados_principales.R` | Resultado principal de empleo y primer eslabón |
| `01b_resultados_principales_con_2020.R` | El mismo análisis incluyendo 2020 |
| `02_validaciones.R` | Validaciones V1 a V12 |
| `03_tratamiento_continuo.R` | Tratamiento continuo C1 a C8 |
| `04_descriptivos_evolucion.R` | Descriptivos D1 a D8 |

### Pendiente de documentar

No está establecido si el depósito de Zenodo entrega un paquete único que
hay que descomprimir, o los archivos sueltos. Verificar al descargar.

### Verificación

Si estas cifras no coinciden, algo salió mal antes de estimar:

- Panel: 62.816 filas, 183 columnas, 9.087 firmas
- Muestra de análisis: 5.099 firmas, 44.631 firma-años
- Años: 2015-2019 y 2021-2024 (2020 excluido por la pandemia)
