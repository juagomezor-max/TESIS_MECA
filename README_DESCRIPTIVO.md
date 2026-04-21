# TESIS_MECA - README Descriptivo del Proceso

Este documento complementa el README principal con una descripcion integral de lo construido hasta ahora en el proyecto: organizacion, entorno reproducible, scripts implementados, resultados generados, decisiones de control de versiones y validaciones aplicadas.

## 1. Objetivo del trabajo realizado

El objetivo fue dejar un flujo reproducible en R para trabajar con microdatos de encuestas economicas (enfasis inicial en EAM), desde la lectura de fuentes comprimidas hasta la construccion de una macrobase consolidada y su diccionario tecnico.

De forma paralela, se implemento una estrategia de higiene de repositorio para evitar subir datos crudos y artefactos pesados o temporales a Git.

## 2. Estructura del proyecto

Se organizo el repositorio con carpetas numeradas y en mayusculas para facilitar replicabilidad:

- 0. PREPARACION: insumos de preparacion y metadatos integrados.
- 1. DATOS: fuentes originales y artefactos derivados pesados (no versionados).
- 2. PROCESAMIENTO: temporales de descompresion y transformacion.
- 3. SCRIPTS: logica de procesamiento y analisis en R.
- 4. RESULTADOS: salidas analiticas ligeras (tablas, resumenes y graficos).

## 3. Entorno reproducible con renv

Se inicializo el proyecto con renv para garantizar reproducibilidad de paquetes y versiones.

Elementos clave:

- renv.lock: congelacion de dependencias.
- .Rprofile con activacion de renv al abrir el proyecto.

Resultado: el entorno queda restaurable en otra maquina con renv::restore().

## 4. Scripts creados y su funcion

### 4.1 analisis_eam_eac.R

Script de exploracion y resumen de archivos por fuente/anio.

Capacidades principales:

- Lectura de ZIP por encuesta.
- Inventario de archivos por anio.
- Resumen por fuente/anio.
- Conteo de variables comunes.
- Generacion de grafico de cobertura.

Se ajusto para priorizar EAM por defecto y admitir argumentos EAM, EAC o ALL.

### 4.2 construir_diccionario_maestro.R

Script para integrar metadatos de variables desde varias fuentes:

- Diccionario en Word (DOCX).
- Metadatos de variables en archivos Stata (DTA).
- Integracion final en una tabla maestra.

Salidas en 0. PREPARACION:

- 1. DATOS/3. DICCIONARIOS/diccionario_maestro_variables.csv
- 1. DATOS/3. DICCIONARIOS/diccionario_word_extraido.csv
- 1. DATOS/3. DICCIONARIOS/metadatos_dta_variables.csv
- 1. DATOS/3. DICCIONARIOS/variables_sin_descripcion.csv

### 4.3 construir_macro_base_eam.R

Script para consolidar anualmente la base EAM desde ZIP con DTA:

- Extrae y lee DTA por anio.
- Estandariza nombres de variables (mayusculas).
- Anexa columnas de trazabilidad: FUENTE, ANIO, ARCHIVO_ORIGEN.
- Consolida en una unica macrobase.
- Genera codebook y resumen de cobertura.

Ubicacion actual de salidas:

- Macrobase pesada: 1. DATOS/5. MACROBASE/macro_base_eam.rds
- Salidas tabulares: 1. DATOS/5. MACROBASE/macro_base_eam_codebook.csv y 1. DATOS/5. MACROBASE/macro_base_eam_resumen.csv

## 5. Resultados principales alcanzados

### 5.1 Diccionario maestro

Se consolidaron metadatos de variables EAM/EAC en un unico archivo maestro, combinando descripcion tecnica (DTA) y descripcion semantica (diccionario documental).

### 5.2 Macrobase EAM

La macrobase consolidada EAM se construyo con cobertura anual completa disponible en el proyecto.

Validacion realizada:

- 140835 filas
- 398 columnas totales
- 395 columnas de datos (sin metacampos)
- Cobertura de anios: 2008 a 2024
- Variables del codebook: 395
- Variables sin descripcion_final: 0

## 6. Decisiones de Git y manejo de archivos pesados

Se reforzo .gitignore para impedir versionar:

- Datos crudos y derivados pesados dentro de 1. DATOS.
- Temporales de procesamiento (_tmp_*).
- Artefactos locales de R/RStudio.
- Temporales comunes de sistema/editor.
- Archivo legado de macrobase en resultados (4. RESULTADOS/macro_base_eam.rds).

Adicionalmente:

- Se limpio historial local cuando se detecto un intento de commit con archivo pesado.
- Se reconstruyo un commit limpio sin el .rds pesado.
- Se elimino la copia pesada en 4. RESULTADOS para evitar duplicidad.

## 7. Estado actual del flujo

El flujo esta operativo para:

- Reproducir entorno con renv.
- Regenerar diccionario maestro.
- Regenerar macrobase EAM y su codebook.
- Mantener el repositorio liviano y controlado.

## 8. Proximos pasos recomendados

- Construir macrobase EAC con el mismo patron de trazabilidad.
- Definir una capa de estandarizacion de variables comparables EAM/EAC.
- Implementar controles de calidad adicionales (duplicados por llave, tipos esperados, rangos plausibles).
- Agregar un script de chequeo automatizado posterior a la construccion de macrobases.

## 9. Guion minimo de ejecucion

Desde la raiz del proyecto:

1. Restaurar entorno:
   renv::restore()
2. Construir diccionario:
   Rscript "3. SCRIPTS/construir_diccionario_maestro.R"
3. Construir macrobase EAM:
   Rscript "3. SCRIPTS/construir_macro_base_eam.R"

Con esto quedan actualizados metadatos en 0. PREPARACION, la macrobase pesada en 1. DATOS/5. MACROBASE y salidas ligeras en 4. RESULTADOS.
