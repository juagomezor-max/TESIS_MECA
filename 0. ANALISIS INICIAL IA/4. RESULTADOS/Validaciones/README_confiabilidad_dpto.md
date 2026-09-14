# Cobertura y consistencia de DPTO (departamento) por establecimiento

Rama: `feature/panel-establecimiento`. Paso 2: confirmar que la ubicacion (departamento) de cada establecimiento esta reportada de forma consistente en toda la serie 2008-2024, antes de usarla para construir `departamento*anio` como efecto fijo en la especificacion a nivel establecimiento. Este paso es exclusivamente validacion de la variable de ubicacion -- no se construyo el panel de establecimiento-anio ni se corrio ninguna regresion.

Notas completas del proceso: `0. PREPARACION/notas_panel_establecimiento.md`.

## Variable confirmada

`DPTO` = "Departamento" (diccionario maestro EAM, 17/17 anios). Es la UNICA variable de ubicacion en la macrobase (no hay municipio, ciudad ni codigo DIVIPOLA completo). Numerico, 23 valores unicos, codigos estilo DIVIPOLA (ej. 5=Antioquia, 11=Bogota D.C., 76=Valle del Cauca). Granularidad de departamento, no de municipio.

## 1. `auditoria_dpto_cobertura_por_anio.csv` / `auditoria_dpto_codigos_por_anio.csv`
Script: `3. SCRIPTS/auditar_cobertura_dpto_establecimiento.R`.

**Cobertura: 100% en los 17 anios (2008-2024), sin excepcion.** Ningun anio por debajo del 100%. Ademas, **el conjunto exacto de 23 codigos DPTO es identico en los 17 anios** -- sin evidencia de cambio de formulario o metodologia de captura.

## 2. `auditoria_dpto_estabilidad_nordest_resumen.csv` / `auditoria_dpto_estabilidad_nordest_casos.csv` / `auditoria_dpto_cambio_sostenido_por_anio.csv`
Script: `3. SCRIPTS/auditar_estabilidad_dpto_nordest.R`.

De 12,621 establecimientos:

| Valores unicos de DPTO en su panel | N | % |
|---|---|---|
| 1 (esperado) | 12,156 | 96.32% |
| 2 o mas | 465 | 3.68% |

Clasificacion de los 465 inestables:

| Patron | N | % de inestables |
|---|---|---|
| Cambio sostenido (permanente, sin reversion) | 415 | 89.2% |
| Salto aislado (un anio se desvia y vuelve) | 13 | 2.8% |
| Patron irregular (alternancia, revision manual) | 37 | 8.0% |

**Hallazgo principal:** 334 de los 465 (71.8%) alternan EXCLUSIVAMENTE entre Bogota D.C. (11) y Cundinamarca (25); 409 de 465 (88%) involucran a 11 o 25 en algun momento. Dentro de los 415 `cambio_sostenido`, 291 son especificamente 11->25 (permanente), concentrados en 2010-2013 (234 casos), con un goteo menor 2016-2024.

**Verificacion externa (WebSearch):** no se encontro evidencia de un cambio de codigos DIVIPOLA de DEPARTAMENTO en Colombia para 11 o 25 durante 2008-2024 (el unico cambio DIVIPOLA identificado en el periodo es un MUNICIPIO nuevo en 2023, no relacionado). Combinado con el punto 1 (conjunto de codigos identico en los 17 anios), se **descarta la hipotesis de recodificacion DIVIPOLA nacional**. Interpretacion: ambiguedad de clasificacion geografica especifica de la EAM entre Bogota D.C. y los municipios industriales vecinos de Cundinamarca (cinturon industrial de la sabana), no relocalizaciones fisicas reales.

**Tratamiento APROBADO (regla diferenciada por patron):** fijar DPTO a un unico valor por establecimiento (invariante en el tiempo). `cambio_sostenido` (415 casos) -> DPTO vigente en 2022 (coherencia con Exposure2022_obreros/Bite2022_obreros). `salto_aislado` (13) y `patron_irregular` (37) -> DPTO modal (mas robusto cuando no hay un antes/despues limpio).

## 3. `auditoria_dpto_distribucion_establecimientos.csv` / `auditoria_dpto_celdas_departamento_anio.csv`
Script: `3. SCRIPTS/auditar_distribucion_dpto_establecimiento.R`.

**23 departamentos representados.** Distribucion concentrada pero no degenerada: top 3 (Bogota 35.4%, Antioquia 21.0%, Valle del Cauca 12.8%) = 69.3% del total. Solo 1 departamento (Vichada, 99) tiene menos de 30 establecimientos en todo el panel (26).

**Celdas departamento-anio en la ventana relevante del DiD (2015-2019 + 2023, 23 x 6 = 138 celdas):**
- Ninguna celda tiene menos de 10 establecimientos (minimo observado: 14, en Casanare y Vichada).
- Cero celdas completamente vacias.

**No hay riesgo de celdas `departamento*anio` sin variacion** en la ventana de estimacion relevante, pese a la concentracion geografica.

## 4. `auditoria_dpto_ciiu4_concentracion.csv` / `auditoria_dpto_ciiu4_detalle.csv`
Script: `3. SCRIPTS/auditar_cruce_dpto_ciiu4.R`. Ventana: establecimiento-anio 2015-2019+2023 (47,951 obs. con DPTO y CIIU4 validos).

**Solo 1 de 23 departamentos supera el 50% de sus establecimiento-anio en un unico sector CIIU4: Casanare (85), 50.5% en el sector 1051 (lacteos).** Vichada (99) le sigue con 44.8%.

| Departamento | Estab.-anio | Sectores CIIU4 | % top1 sector | HHI |
|---|---|---|---|---|
| Casanare (85) | 107 | 4 | 50.5% | 0.354 |
| Vichada (99) | 105 | 4 | 44.8% | 0.306 |
| Bogota D.C. (11) | 16,195 | 116 | 9.2% | 0.033 |
| Valle del Cauca (76) | 6,175 | 91 | 9.9% | 0.031 |
| Cundinamarca (25) | 3,894 | 79 | 6.4% | 0.026 |

**Patron detectado:** los 2 departamentos con menos establecimientos (Paso 2.4: Casanare y Vichada) son tambien los 2 con mayor concentracion sectorial (HHI mas alto) -- compone el riesgo de celdas pequeñas: ademas de tener pocos establecimientos, esos pocos estan concentrados en 1-2 sectores. Riesgo de colinealidad `sector*anio`/`departamento*anio` acotado a estos 1-2 departamentos, no generalizado.

Nota: el sector CIIU 3290 ("Otras industrias manufactureras n.c.p.") aparece como top1 en 14 de 23 departamentos -- es un codigo generico/heterogeneo, no indica especializacion real.

## 5. `auditoria_celdas_dpto_anio_panel_final.csv` / `auditoria_celdas_dpto_anio_incluye_2020.csv` / `auditoria_celdas_casanare_vichada_todos_anios.csv`
Script: `3. SCRIPTS/auditar_celdas_departamento_anio_ventana_final.R`. Repite el chequeo de celdas del punto 3, pero en la ventana REAL del panel final del DiD (no solo 2015-2019+2023).

**Ventana del panel final confirmada** (consolidando decisiones ya explicitas en otros archivos del repo): pre-periodo 2015-2019 + 2021-2022, **2020 excluido por pandemia**, post-periodo 2023-2024. `PANEL_ANIOS_FINAL` = 9 anios (2015-2019, 2021-2024).

**Celdas departamento-anio en la ventana final (23 x 9 = 207 celdas):**
- Ninguna celda tiene menos de 10 establecimientos. Minimo: **13** (Vichada, 2024).
- Cero celdas completamente vacias.

**¿2020/2021 cambian el panorama en Casanare/Vichada?** No. Casanare crece suave y sostenidamente (14 en 2015 -> 26 en 2024, sin quiebre en 2020: 2019=20, 2020=21, 2021=22). Vichada se mantiene plano (16 establecimientos en 2019, 2020 y 2021, identico). Ningun departamento muestra disrupcion atribuible a la pandemia en su conteo de establecimientos reportantes.

## 6. Limitacion conocida: Casanare y Vichada

Casanare (85) y Vichada (99) combinan **baja N** (min. 13-14 establecimientos/anio en la ventana final) **y alta concentracion sectorial** (HHI 0.354 y 0.306, muy por encima del resto). Esto es una limitacion conocida y documentada del control `departamento*anio` para estos 2 casos especificos -- baja potencia estadistica para separar el efecto de Casanare/Vichada de sus sectores dominantes. No compromete la especificacion completa (ningun otro departamento combina ambos riesgos, ninguna celda vacia).

**Criterio de robustez preparado (ejecucion pendiente para cuando corra la regresion formal):**
1. Especificacion principal: 23 departamentos completos.
2. Robustez A: excluir Casanare y Vichada.
3. Robustez B: agrupar Casanare, Vichada y otros departamentos de baja N en una categoria "Otros".
4. Comparar los 3 resultados; coeficientes estables entre especificaciones confirma que estos 2 departamentos no distorsionan el resultado general.

## Conclusion consolidada del Paso 2

`DPTO` tiene cobertura perfecta (100%, 17/17 anios) y es estable en el 96.32% de los establecimientos. La inestabilidad restante (3.68%) esta concentrada casi por completo (71.8%) en un patron identificable y explicable (ambiguedad Bogota/Cundinamarca), no en ruido disperso ni en un cambio de codificacion DIVIPOLA, y tiene un tratamiento APROBADO (regla diferenciada 2022/modal). La distribucion geografica esta concentrada pero no genera celdas `departamento*anio` vacias o muy pequeñas en la ventana REAL del panel final (207 celdas, minimo 13, cero vacias; 2020/2021 no cambian el panorama). El unico riesgo de colinealidad `sector*anio`/`departamento*anio` detectado se limita a Casanare y Vichada, documentado como limitacion conocida con un criterio de robustez ya preparado.

**Paso 2 completo. DPTO se puede usar para construir `departamento*anio`.**
