# README - Exposure2022_obreros (rama `feature/exposicion-obreros-operarios`)

Este documento describe una extension del flujo EAM (ver `README.md` y `README_DESCRIPTIVO.md`
para el pipeline principal): una medida alternativa de exposicion al choque de salario minimo
de 2023, basada en la participacion de **obreros y operarios** en el empleo total de la firma,
en vez del inverso del salario promedio general que usa la `Exposure2022` original.

Vive en una rama separada (`feature/exposicion-obreros-operarios`) porque todavia no se ha
fusionado a `main`; este README documenta el trabajo mientras esta en revision.

## Motivacion

La EAM distingue tres categorias ocupacionales oficiales en el modulo de personal (obreros y
operarios, profesional/tecnico/tecnologo, directivos y administracion/ventas). Los obreros son
el grupo peor pagado en los 17 anios de la macrobase (2008-2024) sin excepcion, asi que su peso
en el empleo total de una firma es, en principio, un proxy directo de cuanto le pega un aumento
fuerte del salario minimo. Esta rama construye esa medida y la compara contra la `Exposure2022`
ya existente, sin reemplazarla.

## Requisito previo

Estos scripts leen `1. DATOS/5. MACROBASE/macro_base_eam.rds` y, para el diagnostico final,
`1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/base_reducida_exposicion_eam.rds`. Corre
primero el flujo principal (`3. SCRIPTS/00_ejecutar_flujo_eam.R`) antes de estos scripts.

## Scripts, en orden

1. `3. SCRIPTS/verificar_cobertura_exposicion_eam.R`
   Confirma el rango real de `ANIO` y `NORDEMP` unicos por anio en la macrobase, antes de
   asumir cobertura del panel.

2. `3. SCRIPTS/verificar_estabilidad_columnas_c3r_c4r.R`
   Mapea las columnas C3R (costos) y C4R (personal ocupado) a categoria ocupacional y tipo de
   vinculacion contra el diccionario oficial de DANE, y valida anio por anio (no solo agregado)
   que los totales oficiales de C4R igualan la suma de sus componentes.

3. `3. SCRIPTS/verificar_nombres_columnas_macrobase.R`
   Escanea las 398 columnas de la macrobase buscando variables renombradas entre anios (hallazgo
   principal: CIIU3 vs CIIU4).

4. `3. SCRIPTS/construir_conteo_personal_categoria_eam.R`
   Construye `total_obreros`, `total_prof_tecnico`, `total_administrativos`, `total_propietarios`
   y `empleo_total_categorias` a nivel NORDEMP-ANIO.

5. `3. SCRIPTS/construir_salarios_promedio_categoria_eam.R`
   Construye `salario_promedio_obrero/administrativo/prof_tecnico` usando personal permanente
   (numerador y denominador consistentes), para validar cual categoria esta peor pagada.

6. `3. SCRIPTS/construir_exposicion_obreros_eam.R`
   Construye `Exposure2022_obreros` (linea base 2022, parametrizable), sin tocar `Exposure2022`.

7. `3. SCRIPTS/diagnosticos_validacion_exposicion_obreros_eam.R`
   Cobertura, histograma comparado, relacion con sector/tamano/DPTO, y tabla de salarios por
   categoria. Salidas en `4. RESULTADOS/descriptivos_exposicion/`.

## Como correrlos

```powershell
Rscript "3. SCRIPTS/verificar_cobertura_exposicion_eam.R"
Rscript "3. SCRIPTS/verificar_estabilidad_columnas_c3r_c4r.R"
Rscript "3. SCRIPTS/verificar_nombres_columnas_macrobase.R"
Rscript "3. SCRIPTS/construir_conteo_personal_categoria_eam.R"
Rscript "3. SCRIPTS/construir_salarios_promedio_categoria_eam.R"
Rscript "3. SCRIPTS/construir_exposicion_obreros_eam.R"
Rscript "3. SCRIPTS/diagnosticos_validacion_exposicion_obreros_eam.R"
```

## Donde quedan las salidas

- `1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/`: tablas intermedias (mapa de columnas,
  conteos, salarios, `exposicion_obreros_eam.rds`). No versionadas (misma convencion que el
  resto de `1. DATOS/`).
- `4. RESULTADOS/descriptivos_exposicion/`: tablas y grafico de diagnostico del Paso 6, si
  versionadas (misma convencion que el resto de `4. RESULTADOS/`).
- `0. PREPARACION/notas_exposicion_obreros_eam.md`: resumen metodologico completo (anios
  cubiertos, codigos que cambiaron de significado, correlacion entre medidas, decisiones
  abiertas). Es la referencia principal para entender los hallazgos de esta rama.

## Resultado clave

`Exposure2022_obreros` correlaciona **0.36** con `Exposure2022` (n=6,178, linea base 2022):
relacionada pero no equivalente. Ambas coinciden en que obreros es el grupo peor pagado en
todos los anios. El detalle completo, incluidas las decisiones metodologicas que quedan
abiertas para revision, esta en `0. PREPARACION/notas_exposicion_obreros_eam.md`.

## Estado

Rama sin fusionar a `main`. No modifica ningun script del flujo principal existente.
