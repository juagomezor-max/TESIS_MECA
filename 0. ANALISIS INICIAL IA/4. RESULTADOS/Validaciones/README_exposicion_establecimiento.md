# Exposure2022_obreros_est y estructura multiplanta (Paso 3)

Rama: `feature/panel-establecimiento`. Paso 3: recalcular la exposicion a nivel establecimiento y generar los descriptivos que deciden si el diseño "dentro de firma" (con `delta_f(e),t`) es viable.

Notas completas del proceso: `0. PREPARACION/notas_panel_establecimiento.md`.

## Tabla de denominadores

Hay 4 denominadores distintos en circulacion en este documento -- cada
porcentaje usa uno especifico, no son intercambiables:

| Denominador | Valor | Que mide |
|---|---|---|
| Establecimientos activos 2022 (todos) | **6,775** | Universo completo de plantas reportando en 2022, sin filtrar por validez de exposicion |
| Establecimientos con `Exposure2022_obreros_est` valida | **6,761** | Subconjunto de los 6,775 con dato de exposicion no faltante (14 faltantes) |
| Firmas del panel 2022 (todas) | **6,186** | Universo completo de empresas reportando en 2022 |
| Firmas con `Exposure2022_obreros` valida | **6,180** | Subconjunto de las 6,186 con dato de exposicion a nivel firma no faltante (6 faltantes) |

Verificacion cruzada de estos cortes: `3. SCRIPTS/verificar_consistencia_cruzada_multiplanta_2022.R`, salida `verificacion_consistencia_cruzada_multiplanta_2022.csv` (confirma con codigo, no de memoria, que 6,775 es el mismo numero en los 3 scripts que lo usan, y que 5,924 firmas monoplanta = 5,924 establecimientos monoplanta contados directamente).

### Que denominador usa cada porcentaje reportado en este documento

| Porcentaje | Valor | Denominador usado | Base |
|---|---|---|---|
| % =0 y % =1 de `Exposure2022_obreros_est` (Seccion 1) | 3.03% / 2.48% | Establecimientos con exposicion valida | 6,761 |
| % =0 y % =1 de `Exposure2022_obreros` (Seccion 1) | 2.88% / 2.01% | Firmas con exposicion valida | 6,180 |
| Frecuencia de establecimientos por firma (Seccion 2, 95.76% monoplanta, etc.) | -- | Firmas totales del panel 2022 | 6,186 |
| Departamentos distintos por firma multiplanta (Seccion 2, 30.9%/42.8%/26.3%) | -- | Firmas multiplanta (`Multi_f`) | 262 |
| % de firmas multiplanta | 4.24% | Firmas totales del panel 2022 | 6,186 |
| **% del empleo total que concentran las firmas multiplanta** | **21.44%** | Empleo total (PERTOTAL, suma en personas, NO un conteo de establecimientos/firmas) | 710,060 personas |
| % de firmas que supera el umbral de variacion (Seccion 3, 64.6%/52.7%) | -- | Firmas multiplanta con 2+ establecimientos con exposicion valida | 260 (subconjunto de las 262) |
| **% de establecimientos monoplanta** | **87.4%** | Establecimientos activos 2022 (todos) | 6,775 |
| % de establecimientos en firmas multiplanta (complemento) | 12.6% | Establecimientos activos 2022 (todos) | 6,775 |

## Construccion

`Exposure2022_obreros_est` = participacion de obreros y operarios en el empleo total, calculada con la composicion ocupacional PROPIA de cada establecimiento (`NORDEST`) en 2022 -- no heredada de la firma (`NORDEMP`). Misma formula, winsorizacion (1%-99%) y quintiles que `Exposure2022_obreros` (nivel empresa). Scripts: `construir_conteo_personal_categoria_establecimiento_eam.R`, `construir_exposicion_obreros_establecimiento_eam.R`.

- **6,761 establecimientos** con `Exposure2022_obreros_est` valida en 2022 (de 6,775 totales; 14 faltantes, 0.21%).
- Correlacion con `Exposure2022_obreros` (empresa, heredada): **0.964**.

## 1. Distribucion comparativa: establecimiento vs. firma
Script: `comparar_distribucion_exposure_establecimiento_vs_firma.R`. Salida: `descriptivos_comparacion_exposure_establecimiento_vs_firma_2022.csv`.

| Nivel | N | Media | Mediana | DE | p10 | p25 | p50 | p75 | p90 | % =0 | % =1 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Establecimiento | 6,761 | 0.601 | 0.633 | 0.231 | 0.286 | 0.466 | 0.633 | 0.769 | 0.875 | 3.03% | 2.48% |
| Firma | 6,180 | 0.602 | 0.632 | 0.226 | 0.292 | 0.472 | 0.632 | 0.769 | 0.870 | 2.88% | 2.01% |

**La dispersion marginal es prácticamente igual** (DE 1.019x, IQR 1.02x, rango p90-p10 1.02x) -- la diferencia de ~2% no es economicamente relevante. Esto NO contradice la heterogeneidad interna encontrada en el punto 3 (son preguntas distintas: dispersion marginal sobre toda la poblacion vs. variacion condicional dentro de cada firma multiplanta).

## 2. Estructura multiplanta 2022
Scripts: `descriptivos_estructura_multiplanta_2022.R`, `descriptivos_estructura_multiplanta_2022_parte2.R`. Salidas: `descriptivos_multiplanta_2022_*.csv`.

**Universo oficial (`Multi_f`, medido en 2022): 262 firmas** (distinto de las 447 del Paso 1.5, que cubren todo 2008-2024 -- corte temporal distinto, no aplica aqui).

### Frecuencia de establecimientos por firma (todas las firmas, N=6,186)

| N establecimientos | Firmas | % |
|---|---|---|
| 1 (monoplanta) | 5,924 | 95.76% |
| 2 | 155 | 2.51% |
| 3 | 45 | 0.73% |
| 4 | 21 | 0.34% |
| 5+ | 41 | 0.66% |

### Departamentos distintos entre las 262 firmas multiplanta

| N departamentos | Firmas | % |
|---|---|---|
| 1 (todas sus plantas en el mismo depto.) | 81 | 30.9% |
| 2 | 112 | 42.8% |
| 3+ | 69 | 26.3% |

81 de 262 (30.9%) tienen todas sus plantas en el mismo departamento -- multiplanta pero concentrada geograficamente.

### Peso economico -- el numero que decide si el diseño es central o marginal

| Metrica | Valor |
|---|---|
| Firmas totales panel 2022 | 6,186 |
| Firmas multiplanta (`Multi_f`) | 262 |
| % de firmas multiplanta | 4.24% |
| Empleo total muestra 2022 (PERTOTAL) | 710,060 |
| Empleo en firmas multiplanta | 152,227 |
| **% del empleo total que concentran** | **21.44%** |

**Las firmas multiplanta son el 4.24% de las firmas pero concentran el 21.44% del empleo total de la muestra 2022** -- un factor de ~5x entre peso poblacional y peso economico. El diseño "dentro de firma" cubre una porcion no trivial del empleo manufacturero, no es un ejercicio marginal.

## 3. Variacion interna de exposicion DENTRO de cada firma multiplanta
Script: `descriptivos_estructura_multiplanta_2022.R`. Salida: `descriptivos_multiplanta_2022_variacion_interna.csv`, `descriptivos_multiplanta_2022_umbral_variacion.csv`.

260 de las 262 firmas (2 excluidas por tener solo 1 establecimiento con dato valido -- ver `descriptivos_multiplanta_2022_reconciliacion.csv`) tienen rango (max-min) de exposicion entre sus plantas con mediana **21.4pp**, p75=37.0pp, p90=55.2pp. **64.6% de las firmas supera el umbral de 15pp** de variacion sustancial (52.7% con el umbral mas conservador de 20pp).

## 4. Nivel de exposicion: plantas de firmas multiplanta vs. monoplanta
Script: `comparar_exposure_est_multiplanta_vs_monoplanta.R`. Salida: `descriptivos_comparacion_exposure_multiplanta_vs_monoplanta_2022.csv`.

Pregunta distinta de la variacion interna (punto 3): ¿el NIVEL de exposicion de una planta de firma multiplanta es sistematicamente distinto al de una planta monoplanta?

| Grupo | N establecimientos | N firmas | Media | Mediana | DE | p10 | p90 |
|---|---|---|---|---|---|---|---|
| Multiplanta | 843 | 262 | 0.584 | 0.610 | 0.250 | 0.222 | 0.894 |
| Monoplanta | 5,918 | 5,918 | 0.604 | 0.636 | 0.228 | 0.290 | 0.872 |

Diferencia de medias: **-0.0202** (multiplanta ligeramente MENOS expuesta en promedio). Prueba t de Welch: t=-2.217, p=0.027 -- **estadisticamente significativa pero economicamente pequeña** (2pp). Las plantas multiplanta tienen mayor DE (0.250 vs 0.228) y p10 notablemente mas bajo (0.222 vs 0.290), sugiriendo mas casos de baja exposicion en la cola izquierda del grupo multiplanta.

## Nota clave: monoplanta = coincidencia por construccion

**El 87.4% de los establecimientos (5,924 de 6,775 en 2022) pertenecen a firmas monoplanta**, donde `Exposure2022_obreros_est` (planta) y `Exposure2022_obreros` (firma) **coinciden por construccion** -- son literalmente el mismo calculo, porque la planta ES la firma. La diferencia entre las dos medidas de exposicion, y por tanto el valor añadido del diseño "dentro de firma", se concentra integramente en el 12.6% restante de establecimientos (843 de 6,775) que pertenecen a las 262 firmas multiplanta.

## 5. Peso en empleo de firmas con variacion interna sustancial
Script: `calcular_peso_empleo_variacion_interna_2022.R`. Salida: `descriptivos_peso_empleo_variacion_interna_2022.csv`.

El 21.44% de la Seccion 2 cubre las 262 firmas multiplanta COMPLETAS, incluyendo algunas cuyas plantas tienen exposicion casi identica (no aportan variacion a `delta_f,t`). Esta seccion aisla el peso de solo las firmas con variacion SUSTANCIAL.

| Grupo | N firmas (% de 262) | Empleo | % del empleo de las 262 | % del empleo total (710,060) | Empleo promedio/firma | Razon vs. promedio muestra (115) |
|---|---|---|---|---|---|---|
| Las 262 completas (referencia) | 262 (100%) | 152,227 | 100% | 21.4% | 581 | 5.06x |
| Brecha >=15pp | 168 (64.1%) | 115,594 | **75.9%** | 16.3% | **688** | **5.99x** |
| Brecha >=20pp | 137 (52.3%) | 98,231 | **64.5%** | 13.8% | **717** | **6.25x** |

**La caida NO es proporcional al numero de firmas: las firmas con variacion interna sustancial son sistematicamente MAS GRANDES, no equivalentes al resto.** Las 168 firmas (>=15pp) son 64.1% de las 262 en numero pero concentran 75.9% de su empleo; las 137 (>=20pp) son 52.3% en numero pero 64.5% en empleo. El empleo promedio confirma el patron: 581 (todas las multiplanta) -> 688 (>=15pp) -> 717 (>=20pp), todas muy por encima del promedio de TODA la muestra 2022 (114.8, ~115 personas/firma).

**Implicacion de generalizacion:** la especificacion "dentro de firma" (`delta_f(e),t`) se identifica sobre un subconjunto de firmas **~6 veces mas grandes** que la firma promedio del panel (razon 5.99x-6.25x). Los efectos estimados con este diseño describen el comportamiento de firmas manufactureras grandes con presencia multiplanta, no necesariamente el de la firma promedio de la EAM -- una limitacion de validez externa que debe declararse explicitamente al reportar resultados de la especificacion B.

## 6. Panel efectivo de la especificacion B por anio
Script: `construir_panel_efectivo_especificacion_b_por_anio.R`. Ventana confirmada en el Paso 2.6: 2015-2019 + 2021-2022 (pre) + 2023-2024 (post), 2020 excluido (9 anios).

**Cuidado: dos poblaciones distintas.** "Cualquier firma multiplanta ese año" (referencia amplia, salida `descriptivos_panel_efectivo_especificacion_b_por_anio.csv`) NO es lo mismo que "las mismas 262 firmas de la cohorte 2022" (poblacion correcta para medir el panel efectivo de `delta_f(e),t`, salida `descriptivos_panel_efectivo_especificacion_b_cohorte_2022_por_anio.csv`). La primera mezcla entradas/salidas de firmas distintas cada año y OCULTA la tendencia real de la cohorte fija.

### Referencia amplia (cualquier firma, NO es la poblacion relevante)

| Año | Firmas multiplanta ese año | Establecimientos |
|---|---|---|
| 2015 | 268 | 894 |
| 2016 | 279 | 925 |
| 2017 | 287 | 959 |
| 2018 | 278 | 933 |
| 2019 | 282 | 929 |
| 2021 | 266 | 857 |
| 2022 | 262 | 851 |
| 2023 | 263 | 858 |
| 2024 | 270 | 880 |

Muestra una falsa estabilidad (rango 262-287) que no refleja lo que le pasa a las 262 firmas especificas de la cohorte.

### Cohorte 2022 (poblacion CORRECTA: las MISMAS 262 firmas, por año)

| Año | Firmas de las 262 con >=2 plantas | Establecimientos aportados | % de las 262 |
|---|---|---|---|
| 2015 | 197 | 739 | 75.2% |
| 2016 | 207 | 771 | 79.0% |
| 2017 | 219 | 812 | 83.6% |
| 2018 | 229 | 828 | 87.4% |
| 2019 | 242 | 843 | 92.4% |
| 2021 | 255 | 835 | 97.3% |
| 2022 | 262 | 851 | 100% (año base, por construccion) |
| 2023 | 251 | 833 | 95.8% |
| 2024 | 243 | 813 | 92.8% |

**Patron real: crecimiento SOSTENIDO de 75.2% (2015) a 100% (2022), luego leve declive a 92.8% (2024).** Sustancialmente distinto de la falsa estabilidad de la tabla de referencia amplia -- la cohorte de firmas multiplanta creció progresivamente hacia el año base y se mantuvo mayormente estable despues del choque.

**De las 262 firmas de la cohorte 2022, 181 (69.1%) mantienen >=2 plantas en LOS 9 AÑOS completos de la ventana** (salida `descriptivos_panel_efectivo_especificacion_b_persistencia_262.csv`). El resto se distribuye en 3 a 8 años (nunca menos de 3).

**Nota metodologica importante: la rampa 2015->2022 (75.2% -> 100%) es MECANICA, no consolidacion empresarial.** La cohorte de 262 firmas se DEFINE por tener >=2 plantas especificamente en 2022 (el año base). Hacia atras en el tiempo, solo las firmas que YA eran multiplanta en cada año anterior pueden cumplir esa condicion dentro de este conjunto fijo -- es una consecuencia aritmetica de la seleccion de la cohorte (survivorship hacia atras), no evidencia de que las firmas se hayan ido expandiendo a multiples plantas progresivamente. La unica parte de la serie con contenido informativo real sobre dinamica de plantas es el tramo POST-2022 (Seccion 7).

## 7. Descomposicion de las salidas de la cohorte 2022 (2023 y 2024)
Script: `descomponer_salidas_cohorte_2022.R`. Salidas: `descriptivos_descomposicion_salidas_cohorte_2023.csv`, `descriptivos_descomposicion_salidas_cohorte_2024.csv`, `descriptivos_comparacion_exposure_salen_vs_mantienen_2023_2024.csv`.

Objetivo: determinar si la caida post-2022 en la cohorte balanceada (100% en 2022 -> 95.8% en 2023 -> 92.8% en 2024) es churn ordinario o seleccion inducida por el choque de salario minimo 2023.

### Descomposicion por año (atricion y perdida de planta son fenomenos DISTINTOS)

| Año | Caen bajo 2 plantas | Atricion (desaparece del panel) | Perdida de planta (sigue en EAM, 1 planta) | Se mantiene (>=2) |
|---|---|---|---|---|
| 2023 | 11 (4.2%) | 7 (2.67%) | 4 (1.53%) | 251 (95.8%) |
| 2024 | 19 (7.3%) | 9 (3.44%) | 10 (3.82%) | 243 (92.8%) |

### Exposicion 2022 (firma): salen vs. se mantienen

| Año | Grupo | N | Media | DE | Diferencia | Error estandar | t | p-valor |
|---|---|---|---|---|---|---|---|---|
| 2023 | Salen | 11 | 0.500 | 0.231 | -0.0651 | 0.0708 | -0.92 | 0.378 |
| 2023 | Se mantienen | 251 | 0.565 | 0.189 | | | | |
| 2024 | Salen | 19 | 0.488 | 0.211 | -0.0804 | 0.0499 | -1.61 | 0.123 |
| 2024 | Se mantienen | 243 | 0.568 | 0.189 | | | | |

**Las firmas que salen tienen exposicion 2022 MAS BAJA en promedio (no mas alta), pero la diferencia NO es estadisticamente significativa en ningun año** (p=0.378 en 2023, p=0.123 en 2024; muestras pequeñas, n=11 y n=19, limitan la potencia). No hay evidencia de que el choque de 2023 este expulsando selectivamente a las firmas mas expuestas -- la caida post-2022 es mas consistente con churn ordinario que con seleccion inducida por el tratamiento, aunque con esta N no se puede descartar con confianza un efecto moderado.

## Implicacion para el event study de la especificacion B

- **Muestra PRINCIPAL: la cohorte BALANCEADA** -- las 181 firmas que mantienen >=2 plantas en los 9 años de la ventana. En un panel no balanceado la composicion de firmas cambia año a año, lo que confundiria composicion con tendencia (una aparente tendencia en el event study podria reflejar cambios en QUIENES estan en la muestra, no un efecto real dentro de firma).
- **Muestra de ROBUSTEZ: la cohorte NO balanceada** (las 262 completas) -- util para verificar que los resultados no dependen de excluir a las 81 firmas que entran/salen, pero no debe ser la especificacion principal por el problema de composicion.

## Conclusion del Paso 3

`Exposure2022_obreros_est` esta construida y validada. El diseño "dentro de firma" tiene una base empirica solida: aunque las firmas multiplanta son pocas (4.24%), concentran una porcion desproporcionada del empleo (21.44%), tienen variacion interna sustancial de exposicion (mediana 21.4pp, 64.6% supera 15pp), y sus plantas no son solo heterogeneas entre si sino tambien, en promedio, levemente distintas en nivel de exposicion frente a las plantas monoplanta. El panel de estas firmas es razonablemente balanceado en el tiempo (69.1% mantiene >=2 plantas en los 9 años de la ventana de estimacion).

**Pero la identificacion no es gratuita**: las firmas que aportan variacion sustancial son sistematicamente ~6 veces mas grandes que la firma promedio de la muestra (Seccion 5). La especificacion "dentro de firma" describe el comportamiento de firmas manufactureras grandes con presencia multiplanta, no el de la firma tipica de la EAM -- esta limitacion de validez externa debe declararse explicitamente al reportar resultados de la especificacion B, y contrasta con la especificacion "entre firmas" (nivel establecimiento con Exposure2022_obreros_est completo), que si cubre a la firma promedio via el 87.4% de establecimientos monoplanta.
