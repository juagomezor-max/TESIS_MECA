# TESIS_MECA

Repositorio para construir, diagnosticar y analizar de forma reproducible microdatos de la EAM y, en una capa exploratoria, de la EAC.

## Objetivo

El flujo actual permite:

- inventariar y revisar archivos fuente EAM/EAC
- construir un diccionario maestro de variables
- consolidar una macrobase anual de la EAM
- diagnosticar si `NORDEMP` funciona como identificador panel
- producir descriptivos exploratorios de exposicion a choques laborales

## Estructura del proyecto

- `0. PREPARACION/`: notas y documentacion metodologica liviana
- `1. DATOS/`: insumos crudos y bases derivadas no versionadas
- `2. PROCESAMIENTO/`: temporales regenerables del pipeline
- `3. SCRIPTS/`: scripts ejecutables del proyecto
- `4. RESULTADOS/`: graficos y salidas visuales

## Requisitos

- R 4.5 o superior
- `renv` para restaurar el entorno del proyecto

## Restaurar el entorno

Desde la raiz del repositorio:

```r
renv::restore()
```

Esto instala las versiones registradas en `renv.lock`.

## Flujo recomendado

La forma mas simple de correr el pipeline EAM completo es:

```powershell
Rscript "3. SCRIPTS/00_ejecutar_flujo_eam.R"
```

Ese script ejecuta, en orden:

1. `analisis_eam_eac.R` con fuente `EAM`
2. `construir_diccionario_maestro.R`
3. `construir_macro_base_eam.R`
4. `diagnostico_panel_nordemp_eam.R`
5. `descriptivo_exposicion_eam.R`

## Ejecucion por etapas

Si quieres correr partes del flujo por separado:

```powershell
Rscript "3. SCRIPTS/analisis_eam_eac.R" EAM
Rscript "3. SCRIPTS/construir_diccionario_maestro.R"
Rscript "3. SCRIPTS/construir_macro_base_eam.R"
Rscript "3. SCRIPTS/diagnostico_panel_nordemp_eam.R"
Rscript "3. SCRIPTS/descriptivo_exposicion_eam.R"
```

Tambien puedes usar:

```powershell
Rscript "3. SCRIPTS/00_limpiar_temporales.R"
```

para reiniciar temporales regenerables en `2. PROCESAMIENTO/`.

## Donde queda cada salida

### Datos derivados

Estas salidas viven en `1. DATOS/` y no se versionan en Git:

- `1. DATOS/3. DICCIONARIOS/`: diccionario maestro y metadatos extraidos
- `1. DATOS/4. ANALISIS_INICIAL/`: inventarios y tablas del barrido inicial
- `1. DATOS/5. MACROBASE/`: macrobase EAM, codebook y resumen
- `1. DATOS/6. BASES_DERIVADAS/panel_diagnostico/`: tablas del chequeo de panel
- `1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/`: base reducida y tablas resumen del descriptivo

### Resultados graficos

Las figuras se guardan en `4. RESULTADOS/`:

- `4. RESULTADOS/panel_diagnostico/`
- `4. RESULTADOS/descriptivos_exposicion/`

## Convenciones del flujo

- `1. DATOS/` contiene insumos y bases derivadas pesadas
- `2. PROCESAMIENTO/` contiene solo temporales regenerables
- `4. RESULTADOS/` se reserva principalmente para salidas visuales
- los scripts suponen que se ejecutan desde la raiz del proyecto

## Control de versiones

- `1. DATOS/` esta ignorada en `.gitignore`
- `2. PROCESAMIENTO/_tmp_*` tambien esta ignorada
- `renv.lock` y los scripts/documentacion si deben versionarse

## Documentacion adicional

Para una explicacion mas detallada del flujo, revisa `README_DESCRIPTIVO.md`.
