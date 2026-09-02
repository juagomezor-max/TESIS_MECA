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

- R **4.5.2 exacto** (la version fijada en `renv.lock`)
- Rtools (compatible con la serie 4.5, ej. Rtools45) para instalar paquetes que requieren compilacion
- `renv` para restaurar el entorno del proyecto

> **Importante:** usa exactamente R 4.5.2, no una version mas nueva. Se probo con R 4.6.1 y
> `renv::restore()` falla al compilar el paquete `S7` (dependencia de `ggplot2`) porque los headers
> internos de R 4.6.x ya no exponen `R_NamespaceRegistry`, que esa version de `S7` necesita. Si ya
> tienes una version distinta instalada, instala 4.5.2 desde el archivo historico de CRAN
> (`https://cran.r-project.org/bin/windows/base/old/4.5.2/`) sin desinstalar la otra; ambas coexisten
> en Windows.

## Restaurar el entorno

Desde la raiz del repositorio, con R 4.5.2 activo:

```r
renv::restore()
```

Esto instala las versiones registradas en `renv.lock`.

## Archivo de datos en Zenodo

El paquete comprimido de datos del proyecto se distribuye externamente a traves de Zenodo:

- `https://zenodo.org/records/19675205`

Esto permite mantener el repositorio liviano en Git y, al mismo tiempo, preservar un punto de acceso estable para el archivo `.zip` que acompana la replicacion.

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
- los paquetes comprimidos de datos se preservan en Zenodo y no deben subirse al repositorio

## Pipeline analitico simplificado (nivel firma, diseno DiD)

A partir de la macrobase ya construida (`1. DATOS/5. MACROBASE/macro_base_eam.rds`, ver "Flujo recomendado" arriba), un segundo pipeline construye el panel analitico a nivel de FIRMA usado para el diseno de diferencias en diferencias de la tesis, y las validaciones de identificacion (tendencias paralelas, atricion diferencial).

### Que hace cada script

- `01_construir_base.R`: deduplica la macrobase a nivel NORDEMP-ANIO (panel de firma).
- `02_construir_exposicion.R`: construye `Exposure2022_obreros` y `Bite2022_obreros` (medidas alternativas de exposicion al choque de salario minimo), 2022 como ano base.
- `03_construir_panel.R`: ensambla el panel analitico 2015-2019 + 2021-2022 + 2023-2024 (2020 excluido por la pandemia).
- `04_validaciones.R`: tendencias paralelas 2015-2019 y atricion diferencial (real y placebo) -- `cluster = ~NORDEMP` siempre, sin excepcion.
- `05_descriptivos.R`: denominadores a nivel firma.
- `opcional_establecimiento.R` (modulo opcional, NO se corre automaticamente desde `run_all.R`): la misma logica a nivel de establecimiento (NORDEST).
- `verificar_cifras_clave.R`: control de calidad -- compara cada cifra del pipeline nuevo contra el valor ya reportado historicamente, produce `CIFRAS_CLAVE.csv`.

Detalle completo de cada script, formulas y su derivacion (que script de `main` origino cada pieza) en `README_SIMPLIFICACION.md`.

### Como correrlo de cero

Requiere que ya exista `1. DATOS/5. MACROBASE/macro_base_eam.rds` (este pipeline **parte de la macrobase ya construida, no reconstruye el ETL desde los DTA crudos** -- para eso, correr primero `3. SCRIPTS/00_ejecutar_flujo_eam.R`, seccion "Flujo recomendado" arriba):

```r
source("3. SCRIPTS/run_all.R")                  # 01 a 05, nucleo minimo (nivel firma)
source("3. SCRIPTS/opcional_establecimiento.R")  # modulo opcional (nivel establecimiento)
source("3. SCRIPTS/verificar_cifras_clave.R")    # control de calidad -> CIFRAS_CLAVE.csv
```

Salidas en `4. RESULTADOS/Validaciones/simplificado_*.csv` y `4. RESULTADOS/Validaciones/CIFRAS_CLAVE.csv`.

## Historial

El repositorio acumulo 48 scripts y 84 salidas en varias ramas de trabajo mientras se validaba el diseno (identificador de panel, medida de exposicion, controles, ventana temporal). Esa exploracion se consolido en el pipeline analitico descrito arriba (2026-09). El trabajo original **no se perdio**: sigue completo en el historial de git y en las siguientes etiquetas de archivo (cada una apunta a la punta de la rama de trabajo correspondiente, antes de fusionarla/borrarla):

| Etiqueta | Commit | Contenido |
|---|---|---|
| `archivo/panel-establecimiento` | `e6f1813` | Panel a nivel establecimiento (NORDEST): auditorias de confiabilidad NORDEST/DPTO, `Exposure2022_obreros_est`, estructura multiplanta 2022, cohorte balanceada. Ver `INDICE_RESULTADOS.md`. |
| `archivo/estimacion-preliminar` | `397e349` | Nota de preanalisis, exportacion ampliada de tendencias paralelas (coeficientes + metadatos), correccion de clustering de `Bite2022_obreros` (`cluster = ~NORDEMP`, antes IID). |
| `archivo/panel-formal` | `da04873` | Trabajo **pendiente y no fusionado a main**: panel formal a nivel establecimiento, Paso 1 completo (68,447 filas / 9,919 establecimientos), Paso 2 escrito pero nunca ejecutado (interrumpido). No representa resultados verificados. |
| `archivo/simplificacion` | `29271be` | Rama donde se construyo el pipeline analitico simplificado (01-05 + `opcional_establecimiento.R` + `verificar_cifras_clave.R`), ya fusionada a `main` en `f3bf8fe`. |

`4. RESULTADOS/Validaciones/CIFRAS_CLAVE.csv` es el control de calidad que conecta ambos mundos: cada cifra que produce el pipeline nuevo se compara, con su especificacion exacta y su tolerancia documentada, contra el valor ya reportado en el trabajo archivado.

## Documentacion adicional

Para una explicacion mas detallada del flujo, revisa `README_DESCRIPTIVO.md`.
