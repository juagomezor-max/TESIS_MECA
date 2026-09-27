# Rigideces laborales y decisiones de la firma

Tesis de Maestría en Economía Aplicada — Universidad de los Andes
Julio Gómez y Nicolás Jácome · Director: Andrés Ham

Estudio de la respuesta de las firmas manufactureras colombianas al aumento
del salario mínimo de 2023, usando la Encuesta Anual Manufacturera del DANE.

## Cómo reproducir

### 1. Entorno

Requiere **R 4.5.2 exactamente**, no 4.6.x: `renv::restore()` falla al
compilar el paquete `S7` bajo R 4.6.1. En Windows, además, Rtools45.

    renv::restore()

Desde una terminal, en lugar de RStudio, la confirmación es interactiva:

    Rscript -e 'renv::restore(prompt = FALSE)'

### 2. Datos

Los microdatos de la EAM **no están en este repositorio**. Se descargan de
Zenodo:

**Gómez Orduz, J. A., y Jácome, N. (2026).** *Harmonized Microdata from the
Annual Manufacturing Survey (EAM) and Supporting Metadata for the Analysis
of Labor Shocks in Colombia, 2008-2024* (v1.1) [Conjunto de datos]. Zenodo.
https://doi.org/10.5281/zenodo.19675581

Es un archivo único, `1. DATOS.zip` (759,6 MB), que se descomprime dentro de
`0. ANALISIS INICIAL IA/` para reconstruir los microdatos y las bases
intermedias del pipeline de construcción (`0. ANALISIS INICIAL IA/3. SCRIPTS/pipeline/`).
Los paneles ya construidos que usa el pipeline de análisis (`1. DATOS/*.rds`)
se generan a partir de ahí; ver los encabezados de esos scripts para el
detalle de ese primer tramo, que no forma parte del pipeline documentado más
abajo.

**Versión:** las cifras de verificación de este README corresponden a la
v1.1 de los datos. Otra versión puede dar resultados distintos.

Licencia de los datos: CC BY 4.0.

### 3. El pipeline de análisis (`3. SCRIPTS/`)

Con `1. DATOS/` ya reconstruido, abrir `TESIS_MECA.Rproj` (así R trabaja
desde la raíz del repositorio, que es lo que todos los scripts asumen — ninguno
usa `setwd()` ni detecta su propia ubicación).

Hay 17 scripts, numerados en dos grupos:

- **`01` a `08`** — el pipeline vivo, numerado por **orden de lectura** (cómo
  se lee el estudio: descriptivos → medidas de exposición → análisis →
  validaciones → validaciones adicionales), que **no es el orden de
  ejecución**.
- **`09` a `16`** — diagnóstico de validez de la medida de exposición
  principal (Bite, el índice de Kaitz). No son parte de la estimación: auditan
  el pipeline desde afuera, sin modificarlo ni volver a correrlo. Ver
  `4. RESULTADOS/AUDITORIA_CADENA.md` y `4. RESULTADOS/CRONOLOGIA_DECISIONES.md`
  para el resumen de hallazgos.
- **`xx_script_base_historico.R`** — versión manual anterior, superada.
  **No correr**: su carpeta de salida colisiona con la de `01`.

**Orden de ejecución real** (distinto del orden de lectura): correr **`02`
primero** — escribe `1. DATOS/exposicion_alternativa_2022.rds`, que leen `03`,
`04` y `05`. Los demás scripts del pipeline vivo (`01`, `06`, `07`, `08`) son
terminales y se pueden correr en cualquier orden, antes o después de `02`,
siempre que `03`, `04` y `05` corran después.

Detalle completo de cada script (qué lee, qué escribe, a qué capítulo de la
tesis alimenta): `3. SCRIPTS/README.md` y `3. SCRIPTS/NARRATIVA.md`.

### Verificación

Correr `02_medidas_exposicion.R` primero que cualquier otro. Si estas cifras
no coinciden, algo salió mal antes de estimar:

- Firmas en 2022: **6.186**
- Firmas con Kaitz de obreros (Bite) válido: **5.099**

## Estado del repositorio

Los controles `sector_2022`/`depto_2022`/`tamano_2022` de `05`, `06` y `07`
se corrigieron el 26-sep-2026 (antes se recalculaban con el año propio de
cada fila, no con el de 2022 — ver el mensaje del commit correspondiente y
`4. RESULTADOS/Auditoria_adversarial/` para el detalle). Las cifras de
empleo y heterogeneidad por tamaño cambiaron con esa corrección; los números
vigentes son los que producen los scripts tal como están hoy en esta rama,
no los de versiones anteriores de la tesis o el póster.
