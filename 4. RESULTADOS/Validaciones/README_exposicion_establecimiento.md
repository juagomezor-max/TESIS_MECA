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

## Conclusion del Paso 3

`Exposure2022_obreros_est` esta construida y validada. El diseño "dentro de firma" tiene una base empirica solida: aunque las firmas multiplanta son pocas (4.24%), concentran una porcion desproporcionada del empleo (21.44%), tienen variacion interna sustancial de exposicion (mediana 21.4pp, 64.6% supera 15pp), y sus plantas no son solo heterogeneas entre si sino tambien, en promedio, levemente distintas en nivel de exposicion frente a las plantas monoplanta.
