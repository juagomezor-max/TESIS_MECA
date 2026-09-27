# Panel firma-año EAM completo, con 2020, para medidas de exposición alternativas

Generado por `construir_panel_completo.R`. **No construye ninguna medida de exposición ni calcula ratios/promedios/índices.** Solo extrae, agrega a firma-año, valida y documenta. Las fórmulas de exposición van en un script posterior, que consume este panel.

Panel: `130859` filas (firma-año), `289` columnas, `11687` firmas distintas, años 2008-2024 (incluye 2020, a diferencia del panel analítico oficial).

## Hallazgos del PASO 0 (verificaciones previas)

**0.1 -- Denominador de `Bite2022_obreros`:** revisado en `pipeline/02_construir_exposicion.R`. Está correctamente emparejado -- numerador (`C3R2C1`, sueldos permanentes) y denominador (`C4R2C1+C4R2C2`, conteo de permanentes) miden la MISMA población. No usa el total ocupado (`C4R5C1+C4R5C2`, que sí incluiría temporales). No hay confusor de temporalidad en esa medida.

**0.2 -- Serie de conteos profesional/técnico (`C4R1C*N` vs `C4R2C*E`):** NO son series duplicadas -- son complementarias. Probadas ambas contra la identidad obreros+profesionales+administrativos+aprendices = `PERTOTAL`: solo N acierta 97.56% de las filas, solo E acierta 28-39%, **N+E juntas acierta 100.00% (0 de 140,835 filas con diferencia)**. La serie E aparece en 1-5% de las firmas por año y casi nunca coexiste con N en la misma fila (2.23%). Se sumaron ambas en cada subcategoría, igual que ya hacía `pipeline/01_construir_base.R`.

**0.3 -- Aprendices en `C4R5*`/`PERTOTAL`:** SÍ están incluidos en ambos (confirmado empíricamente: sin aprendices el conteo de obreros coincide con `C4R5C1+C4R5C2` solo 80.14% de las veces, con aprendices sube a 99.46%). Pero su costo NO está en `C3R2C1` (sueldos permanentes) -- va aparte en `R1CSAP`-`R4CSAP` (apoyo de sostenimiento, Ley 789). Quien use `C4R5*` o `PERTOTAL` como denominador de un salario promedio con `C3R2C1` como numerador está incluyendo aprendices en el denominador sin su pago en el numerador. Se extrajeron los aprendices como variable propia por categoría.

**0.4 -- Disponibilidad por año:**

| Variable | Años disponibles |
|---|---|
| `C3R1*` (salario integral) | 2008-2019 (12 de 17 años, falta 2020-2024) |
| `C3R20*` (Impuesto Renta Equidad) | 2013-2024 (12 de 17 años, falta 2008-2012) |
| `C4R6*` (aprendices, todas las categorías) | 2008-2024 (los 17 años) |

**0.5 -- Unidades:** confirmado empíricamente (no declarado por el diccionario EAM para `C3R2C1` específicamente) que las masas salariales `C3R*` son anuales, en miles de pesos COP. Ver `0. PREPARACION/notas_exposicion_obreros_eam.md`. Se hereda este supuesto para el resto de variables `C3R*`, documentado como supuesto (no como hecho verificado variable por variable) en la columna `unidad` del diccionario.

## Cierre del panel: 6 correcciones tras la revisión del diccionario

Esta sección documenta la revisión que encontró y corrigió seis problemas en la primera versión del panel/diccionario. Los tres primeros eran bloqueantes porque cambian qué medidas de exposición se pueden construir.

### 1. La columna `poblacion` estaba mal en casi todos los costos

Confirmado: un `case_when` con `TRUE ~ "permanentes"` como último caso capturaba por defecto todo lo que no fuera `apoyo_sostenimiento_aprendices` o `salario_integral`, sin mirar la etiqueta EAM real de cada variable. Corregido fila por fila contra la etiqueta cruda:

| Concepto | `poblacion` antes | `poblacion` ahora (según etiqueta EAM) |
|---|---|---|
| C3R4* (sueldos+prest. temporal directo) | permanentes | **temporal_directo** ("personal contratado directamente") |
| C3R5* (cotizaciones) | permanentes | **total_ocupado** ("...del personal ocupado", explícito) |
| C3R6* (parafiscales) | permanentes | **total_ocupado (inferido)** -- etiqueta truncada antes de declarar población, no confirmado por texto |
| C3R7* (seguros vida voluntarios) | permanentes | **total_ocupado** ("...ampara al personal ocupado", explícito) |
| C3R8* (pago agencias temporales) | permanentes | **temporal_agencias** ("empresas que suministran personal temporal") |
| C3R9* (otros gastos personal) | permanentes | **no_determinada** -- etiqueta no especifica población |
| C3R10* (costo total personal) | permanentes | **total_ocupado** ("Costos y Gastos Causados por el Personal Ocupado", explícito) |

De paso, se encontró y corrigió un error de nombre: las variables que se habían llamado `control_temporal_agencias_mujer/hombre_c4r4c9t/c10t` en realidad son `C4R4C9T`/`C4R4C10T` = "Total Personal Ocupado Mujer/Hombre" -- no tienen nada que ver con agencias temporales. Renombradas a `control_total_ocupado_mujer/hombre_...`.

### 2. Estructura del cuadro 3: la identidad NO cierra

Se probó `C3R10 = C3R2+C3R3+C3R4+C3R5+C3R6+C3R7+C3R8+C3R9 (+C3R1 donde existe)` por categoría y en el total. Ver tabla `EA_T07_estructura_c3r10`. **No cierra de forma confiable en ningún año ni categoría**: el % de filas que coincide exacto va de 26.2% a 81.2%, con una caída marcada a partir de 2021 en todas las categorías. Se revisó si faltaba una fila C3R11/C3R12 en la macrobase que explicara la diferencia -- no existe ninguna. La causa exacta queda abierta (no se inventa una explicación): puede ser redondeo sistemático, un cambio de formulario en 2021, o que el establecimiento reporte C3R10 de forma independiente y no como suma mecánica de las filas anteriores.

**Esto no invalida la conclusión sobre qué medidas son internamente consistentes**, porque esa conclusión se apoya en la etiqueta propia de cada variable (C3R2/C3R3 dicen explícitamente "del personal permanente"; C3R10 dice explícitamente "del Personal Ocupado"), no en que la suma cierre. Las dos combinaciones que quedan habilitadas:

- **Salarial**: `(C3R2 + C3R3) / permanentes` -- numerador y denominador sobre permanentes.
- **Costo total**: `C3R10 / (total_ocupado - propietarios)` -- numerador y denominador sobre ocupados.

No hay punto intermedio defendible: cotizaciones (C3R5) y parafiscales (C3R6) están sobre personal ocupado, no desagregados por tipo de vínculo, así que no se puede armar un costo laboral "por permanente" que los incluya. Nota de paso: bajo esta lectura, el `salario_promedio` que usa `01_resultados_principales.R` (C3R10C3 / empleo_total) es internamente consistente -- ambos lados sobre personal ocupado.

### 3. CIIU4 falta en 2008-2011 -- no hay empalme defendible con CIIU3

Ver tabla `EA_T09_solapamiento_ciiu`. Confirmado: `CIIU4` (2012-2024) y `CIIU3` (2008-2011) tienen **0 años de solapamiento** -- nunca coexisten en la misma fila. Sin años en común no hay forma de aprender un empalme empírico dentro de este panel. Se revisaron las columnas `CORRELA*` de la macrobase: pertenecen a la **EAC, no a la EAM**, y codifican "dominios de estudio" (16 o 9), una clasificación distinta a CIIU -- no son una tabla de correlación CIIU3→CIIU4. No se encontró ninguna tabla de correlación oficial en el repositorio. **No se construyó ninguna variable de sector armonizada.** Conclusión explícita: la ventana del panel con sector confiable (CIIU4) empieza en 2012, no en 2008. `CIIU3` queda como variable propia, sin integrar, para quien quiera intentar un empalme con una tabla externa del DANE por su cuenta.

### 4. Reconciliación panel-diccionario

El script ahora verifica programáticamente, antes de guardar, que `setdiff(names(panel), diccionario$variable)` y su inverso estén ambos vacíos -- si no, se detiene con `stop()` en vez de guardar un panel/diccionario que no reconcilian. Causa de la brecha original (295 columnas vs. 158 filas): 131 columnas eran códigos EAM crudos que el panel conserva junto a su versión renombrada, por trazabilidad, y nunca se documentaron; las otras 4 eran columnas auxiliares de esta misma validación (sumas y diferencias reconstruidas) que se colaron en el panel guardado por error de scoping -- se descartan ahora antes de guardar, no son datos.

### 5. R1/R2/R3CSAP: sigue sin determinarse, con las dos pruebas hechas

Ver tablas `EA_T08_csap_correlaciones`. Identidad `R1+R2+R3=R4`: cumple 96.83% de las filas -- confirma que R4 es el total, pero no dice nada sobre a qué categoría corresponde cada uno de R1/R2/R3 individualmente. Correlación de cada una contra el conteo de aprendices por categoría (obreros/profesional-técnico/administrativos): la correlación máxima observada en las 9 combinaciones es 0.095 -- prácticamente nula. **La pista del parecido en % de ceros no se confirma con la correlación real.** Se probaron las dos vías que pedía la tarea y ninguna resuelve la ambigüedad -- queda `no_determinada`, documentado con ambos resultados en el diccionario.

### 6. Unidades: el README y el diccionario se contradecían

Corregido. El diccionario ya no dice `"(supuesto)"` en las variables de costo laboral (el Paso 0.5 sí lo confirmó empíricamente para C3R2C1, y se hereda como supuesto explícito -- no verificado variable por variable -- para el resto de `C3R*`). Para producción, se resolvió caso por caso: `PORCVT` es porcentaje (0-100); `EELEC` es kWh anuales (etiqueta EAM: "Energía Eléctrica en kw"); el resto (`VALORVEN`, `PRODBR2`, `INVEBRTA`, etc.) son monetarias, miles de pesos anuales heredado del mismo supuesto de C3R2C1, marcado como no re-verificado variable por variable.

### 7. Obreros permanentes: selección sobre el mecanismo (limitación declarada)

`obreros_total_ocupado` tiene 2.7%-4.2% de ceros según el año; `obreros_permanentes` tiene 17.0%-24.4% (ver tabla `EA_T03_cobertura_variables_clave`, columna `pct_cero`, para el detalle año por año). La fracción es relativamente estable en el tiempo -- **no creciente** (de hecho, algo más alta en 2008-2012 que en 2019-2024). `Exposure2022_obreros` usa el total ocupado como denominador; `Bite2022_obreros` (Kaitz) usa permanentes. De ahí la pérdida de firmas al construir Kaitz: no son "firmas sin obreros", son **firmas sin un solo obrero permanente** -- producción enteramente con personal temporal o de agencia. Esto no es un problema de cobertura de datos: es una limitación de diseño. El Kaitz actual excluye del tratamiento precisamente a las firmas que ya usan la temporalidad como margen de ajuste -- uno de los mecanismos que la tesis quiere estudiar. Es selección sobre el mecanismo, no ruido de medición.

## Resultado de las identidades (PASO 3)

Ver tablas `EA_T01_identidad_empleo` y `EA_T02_identidad_costos`. `EA_T01`: permanentes+propietarios+temporal_directo+temporal_agencias+aprendices (3 categorías) = `PERTOTAL`. Esta MISMA descomposición de 5 componentes por categoría se validó al 100.00% contra la macrobase antes de escribir este script (ver PASO 0.2/0.3 arriba) -- rango observado al re-verificarla ya en el panel agregado a firma-año: 100% a 100%. Cierra al 100% en los 17 años: coincide exactamente con el resultado a nivel establecimiento, sin degradarse al agregar a firma. (Una primera versión de este chequeo comparaba contra una descomposición distinta, 'total_ocupado + propietarios', que sí se degradaba al agregar por un artefacto de la regla 'todas NA -> NA, si no suma con na.rm=TRUE' aplicada columna por columna -- se corrigió para usar la MISMA descomposición de 5 componentes ya validada en el Paso 0, no la que se degradaba.) `EA_T02` (costos, `C3R10C1+C3R10PT+C3R10C2=C3R10C3`, las 3 categorías sumadas contra el total ya reportado) cierra 92-99% según el año -- confirmado que esa brecha ya existe a nivel establecimiento antes de cualquier agregación (94.43% en la macrobase cruda, mediana de diferencia ±1), así que es una característica de los datos fuente, no un artefacto de la agregación a firma. Distinto y más severo es `EA_T07` (Cierre, punto 2): la estructura completa del cuadro 3, `C3R10 = suma(C3R1..C3R9)`, NO cierra de forma confiable (26%-83% según año y categoría) -- ver esa sección arriba.

## Comparación contra `panel_analitico_firma_eam.rds`

Ver tabla `EA_T04_comparacion_panel_actual`. Comparación firma a firma en empleo, ventas y costo laboral total, en los años comunes a ambos paneles (2015-2019, 2021-2024 -- el panel actual no tiene 2020).

## 2020 en cobertura

`sueldos_permanentes_obreros_c3r2c1` (variable cruda de encuesta, no una suma derivada): 0% de faltantes en 2019 vs. 0.03% en 2020 (ver tabla `EA_T03_cobertura_variables_clave` completa para todas las variables clave y todos los años -- ahí también está el hallazgo de CEROS, no faltantes, del punto 7 de "Cierre del panel" arriba, que es la señal más relevante para Kaitz). Esta cifra de faltantes es la que debe informar si 2020 tuvo problemas de recolección -- no se tomó ninguna decisión de incluir/excluir 2020 en la estimación aquí.

## Variables buscadas y no encontradas, o con categoría ambigua

- **Tamaño de empresa (variable cruda), año de inicio de operaciones, organización jurídica**: NO existen en la macrobase bajo ningún nombre plausible (se buscó por patrones TAMA*, ORGJUR/ORGANIZ/JURID, INICI/ANOINI/FUNDA/APERTU -- cero resultados). `tamano_empresa` SÍ se generó, pero es una variable DERIVADA (misma regla `tamano_de()` que ya usa `03_construir_panel.R`), no un campo crudo de la encuesta.
- **`CIIU3`**: existe, pero con cobertura muy limitada -- confirmar años antes de usarlo (columna `anios_disponibles` del diccionario).
- **`R1CSAP`/`R2CSAP`/`R3CSAP`** (apoyo de sostenimiento de aprendices): sus etiquetas en el diccionario EAM están truncadas de forma idéntica ("...aprendices y pasantes (Ley 789"), no se pudo determinar cuál corresponde a obreros/profesional-técnico/administrativos sin adivinar. Se extrajeron como 3 variables independientes con `categoria_ocupacional = "no_determinada"`. `R4CSAP` sí dice "Total" explícitamente.

## Qué queda pendiente

- Decidir la categoría ocupacional real de `R1CSAP`/`R2CSAP`/`R3CSAP` (posiblemente contactando la ficha metodológica del DANE, no solo el diccionario extraído del DOCX).
- Decidir si 2020 entra o no a la estimación, con base en la cobertura reportada aquí.
- Ninguna medida de exposición se calculó -- ese es el script siguiente, que debe leer `panel_firma_eam_expalt_completo.rds` y el diccionario para saber qué numeradores y denominadores son compatibles (columna `poblacion`).

## Diagnóstico del residuo de C3R10 (costo total del personal)

`C3R10` (costo total del personal, numerador de `salario_promedio` en `01_resultados_principales.R`) no cerraba como `suma(C3R1..C3R9)` en el diagnóstico previo (`EA_T07`, 17%-74% de cumplimiento). Este script diagnostica por qué, con datos, no con especulación.

### 1. Dirección del residuo: casi enteramente positiva

Ver `RC_T01_reparto_signos_residuo`. El residuo negativo es prácticamente inexistente (0.01%-0.06% de las filas en las 4 categorías). El resto se reparte entre "cero dentro de tolerancia" (48%-75%) y **positivo** (25%-52%). No hay mezcla de signos que sugiera un problema de calidad de dato: la dirección es sistemática y apunta limpiamente a un **rubro que no se extrajo**, no a doble conteo.

### 2. Magnitud: pequeña en la mediana, concentrada en una minoría de firmas

Ver `RC_T02_magnitud_residuo`. Mediana del residuo (categoría total): 0.209% de `C3R10`. Solo 6.68% de las filas supera el 5%, y 0.61% supera el 20%. Esto NO es "un rubro entero por fuera" para la firma promedio -- es un residuo que afecta de forma importante solo a una minoría de firmas: exactamente las que tienen aprendices.

### 3. Estructura: no es agregación, es un dato de la macrobase cruda

Por año (`RC_T03`): el cumplimiento empeora progresivamente de 2008 a 2024 en las 4 categorías, sin un salto abrupto en un año específico que sugiera un cambio puntual de formulario -- es una tendencia gradual, no un quiebre. Por tamaño (`RC_T04`): empeora con el tamaño de la firma (Pequeña 67.9% cumple, Mediana 16.1%, Grande 6.4%) -- coherente con que las firmas grandes tienen más probabilidad de tener aprendices. Por sector (`RC_T05`): varía bastante (29%-59%) pero sin un sector claramente atípico que domine el patrón. Establecimiento vs. firma (`RC_T06`): 73.58% vs. 73.78% -- prácticamente idéntico. **La agregación a firma NO amplifica el residuo**: ya está en los datos crudos de la macrobase, por establecimiento.

### 4. El rubro identificado: R4CSAP (apoyo de sostenimiento de aprendices)

Ver `RC_T07_correlacion_candidatos`, `RC_T08_prueba_r4csap` y `RC_T11_residuo_neto_tras_r4csap`. De los 6 candidatos probados (R4CSAP, R1+R2+R3CSAP, SALARPER, SALPEYTE, PRESSPER, PRESPYTE), R4CSAP es, por lejos, el que mejor explica el residuo: correlación 0.8828 en toda la muestra, 0.9355 restringido a las filas con R4CSAP > 0. La razón residuo/R4CSAP tiene **mediana exactamente 1.0**, y 87.63% de esas filas caen entre 0.9 y 1.1. Al descontar R4CSAP del residuo, el % de filas dentro de tolerancia (0.1%) sube de 48.01% a 91.19%. Es la prueba que pedía la tarea, y confirma la hipótesis: los aprendices están en el conteo de personal que determina `C3R10` ("Costos y Gastos Causados por el Personal Ocupado"), pero su pago (`R4CSAP`, apoyo de sostenimiento, Ley 789) vive en una fila aparte del cuadro 3, fuera de `C3R1`-`C3R9`.

### 5. Totales de control: parcialmente consistentes, no perfectos

Ver `RC_T09_contraste_totales_control`. `SALARPER` vs. `C3R2C3` y `PRESSPER` vs. `C3R3C3` coinciden en 84.33% de las filas cada uno -- alto, pero no total, así que incluso las filas individuales R2/R3 tienen algo de inconsistencia interna frente a sus propios totales de control, con una magnitud menor a la del hallazgo principal. `SALPEYTE` vs. `C3R2C3+C3R4C3` coincide solo 51.11% -- más ruido en la frontera permanente/temporal directo, consistente con lo ya documentado en el punto 2 del cierre anterior (la estructura completa del cuadro 3 no cierra de forma perfecta en ningún cruce probado hasta ahora).

### 6. Conclusión

**"C3R10 = suma de R1-R9 más X", con X identificado: R4CSAP** (apoyo de sostenimiento de aprendices, Ley 789). No es una explicación perfecta al 100% -- queda un residuo menor sin explicar tras descontar R4CSAP (ver `RC_T11`), pero pasa de ser el problema dominante a ser un remanente secundario. `salario_promedio` (`C3R10C3 / empleo_total`) **está bien definido**: incluye el costo de aprendices que `empleo_total` también cuenta en el denominador (ver PASO 0.3 del script principal -- los aprendices SÍ están en `PERTOTAL`/`empleo_total`). Usar `C3R10` es, de hecho, la opción MÁS consistente entre numerador y denominador, no una caja opaca.

**Efecto sobre `salario_promedio` 2022 y 2023** (ver `RC_T10_impacto_salario_promedio`): la diferencia mediana entre usar `C3R10` y usar la suma explícita de R1-R9 es **1.33% en 2022 y 1.10% en 2023** -- pequeña, y el percentil 10 es 0% (la mitad de las firmas o más no tiene diferencia alguna, porque no tienen aprendices). El percentil 90 sí llega a 15-16%, para las firmas que sí tienen aprendices con apoyo de sostenimiento. **El hallazgo es una nota metodológica, no una corrección al resultado del primer eslabón**: la diferencia en la mediana es pequeña y el numerador actual (`C3R10`) es, si acaso, más correcto que la alternativa de sumar R1-R9 a mano, porque esa suma excluiría el costo de los aprendices que sí están contados en el denominador.

**Lo que queda pendiente**: identificar la causa exacta del residuo restante tras descontar R4CSAP (12.37% de las filas con R4CSAP>0 caen fuera del rango 0.9-1.1 en la razón residuo/R4CSAP) -- no se investigó más a fondo por quedar fuera del alcance de esta tarea. Candidato más plausible para ese remanente: redondeo o pequeñas inconsistencias de reporte en R1CSAP/R2CSAP/R3CSAP individuales (ver el 96.83% -no 100%- de la identidad R1+R2+R3=R4 ya documentado en el cierre anterior).


## Validación de la tasa de parafiscales como ventana a la distribución salarial

Prueba si `tasa_parafiscales = C3R6 / nómina` revela la fracción de nómina por encima de 10 SMLV, aprovechando la exención de SENA+ICBF (Ley 1607 de 2013) para trabajadores bajo ese umbral. Esta sección NO construye ninguna medida -- solo prueba si la señal existe.

### Base de aportes elegida

Se probaron 3 definiciones del denominador. Ver `TP_T01_comparacion_bases_denominador`. **Elegida: C3R2 + C3R4 (permanentes + temporal directo)**, con 98% de las tasas dentro del rango teórico [0, 0.10], frente a 85.67% y 89.56% de las otras dos. C3R2 solo (sueldos permanentes) subestima la base al dejar fuera a los temporales directos, que también generan parafiscales.

### PRUEBA 1: bimodalidad -- PASA

Ver `TP_T02_bandas_percentiles_2022` y `TP01_histograma_tasa_2022`. En la categoría total, 44.15% de las firmas cae en [0.035, 0.045] (banda de exención total) y 7.59% en [0.085, 0.095] (banda sin exención), con 40.58% distribuidas en el medio y solo 1.46% fuera del rango [0, 0.10]. Hay masa reconocible en ambos extremos teóricos, con más peso cerca de 0.04 que de 0.09 -- coherente con que la mayoría de la nómina está por debajo de un umbral tan alto como 10 SMLV.

### PRUEBA 2: placebo pre-2013 -- PASA (la que decide)

Ver `TP_T03_serie_anual_placebo`, `TP_T04_veredicto_prueba2`, `TP02_serie_anual_placebo` y `TP03_histograma_2011_vs_2022`. En 2008-2012 la mediana de la tasa (categoría total) es **exactamente 0.0900 los 5 años**, con dispersión baja. Desde 2013 la mediana cae y la dispersión sube (SD promedio 0.18 en 2013-2024 frente a 0.038 en 2008-2012, 4.8x). 2013 es un año de transición clara (mediana ~0.065, a medio camino entre 0.09 y 0.04) -- coherente con una Ley 1607 que no aplicó desde el 1 de enero para todas las firmas. Desde 2014 la mediana se estabiliza cerca de 0.04. **No se detectó un quiebre claro en 2016-2017** (Ley 1819, eliminación del CREE) -- la mediana se mueve de forma continua en esos años, sin salto visible.

**Nota de calidad de dato**: la SD post-2013 está inflada por valores atípicos extremos en algunos años/categorías (ej. SD=1.71 en administrativos 2021, muy por encima del rango teórico [0,0.1] de la propia tasa) -- son unas pocas firmas con denominadores muy pequeños generando razones extremas, no una dispersión genuina de esa magnitud. El criterio de la prueba (SD se duplica) se cumple ampliamente incluso ignorando ese ruido; los percentiles p10/p90 (robustos a outliers, en la misma tabla) muestran el mismo patrón de apertura desde 2013 de forma más limpia.

### Diagnósticos adicionales (solo porque ambas pruebas pasaron)

**3. Consistencia interna** (`TP_T05_consistencia_interna`): % de firmas con fracción implícita aritméticamente incoherente con su salario promedio observado -- administrativos 31.16%, obreros 30.76%, profesional_tecnico 27.83%, total 34.19%.

**4. Cruce con salario integral, 2008-2019** (`TP_T06_cruce_salario_integral`, `TP_T07_test_wilcoxon_salario_integral`): p-valor Wilcoxon por categoría -- obreros=0, profesional_tecnico=0, administrativos=0, total=0.

**5. Correlación con `Bite2022_obreros`** (`TP_T08_correlacion_bite2022`): Pearson = -0.052, Spearman = -0.143 (n=5099) -- correlación débil, casi nula. La fracción implícita captura información en buena medida DISTINTA de Kaitz, no una reformulación de lo mismo.


### Veredicto

**La señal existe y es parcialmente usable, con una reserva importante.** Las dos pruebas que deciden (bimodalidad y, sobre todo, el placebo pre-2013) pasan con evidencia contundente: la mediana pre-2013 da exactamente 0.09 los 5 años, y la dispersión se abre de forma clara justo cuando entra en vigor la Ley 1607. El cruce con salario integral (punto 4) es la validación externa más limpia de las tres: las firmas que sí reportan salario integral tienen una tasa sistemáticamente más alta, en las 4 categorías, con p-valores indistinguibles de cero. La correlación con `Bite2022_obreros` (punto 5) es débil -- esto sería información nueva, no una repetición de lo que ya se tiene.

**La reserva**: la prueba de consistencia interna (punto 3) encuentra que 31% en promedio de las firmas (27.8%-34.2% según categoría) tiene una fracción implícita sobre 10 SMLV que es aritméticamente incoherente con su propio salario promedio observado -- más de una cuarta parte de la muestra. Por instrucción de la tarea ("si es alto, la señal está contaminada"), este % es alto y no se minimiza: la fórmula lineal `(tasa-0.04)/0.05` es una aproximación razonable para ver SI hay señal (que es lo que pedía esta tarea), pero no es directamente utilizable como medida firma por firma sin refinarla -- posiblemente porque ignora la exención parcial del salario integral (base 70%, tasa efectiva 6.3% no 9%, ver caveats) y trata la relación tasa-fracción como lineal cuando probablemente no lo es en los extremos. **Conclusión operativa: la idea se queda para otra sesión, con esta reserva documentada -- no se recomienda usarla en su forma actual sin resolver la inconsistencia del punto 3.**

### Caveats

- La exención requiere ser contribuyente de renta y tener 2+ empleados -- casi todas las firmas EAM califican, pero no todas.
- Para trabajadores con salario integral la base de aportes es el 70% del salario, así que su tasa efectiva es 6.3%, no 9% -- afecta la lectura de los valores intermedios, sobre todo en 2008-2019 donde C3R1 existe.
- No se usó C3R5 (salud, pensión, ARL) para este ejercicio: el ARL varía por clase de riesgo y ensucia la tasa.
- El umbral de la exención es **10 SMLV, no 1**. Esta tasa NO identifica el % de trabajadores en el salario mínimo -- identifica la fracción por encima de 10 mínimos.


## Diagnóstico: por qué 28-34% de firmas tienen fracción implícita incoherente

Atribuye la incoherencia encontrada en la validación de la tasa de parafiscales (28%-34% de firmas, según categoría) a 4 causas candidatas. Ver `IP_T09_atribucion_final` para la síntesis. **Aviso metodológico**: las 4 causas se prueban en ventanas de años distintas -- Causa 1 solo es observable 2008-2019 (donde existe `C3R1`), Causa 3 solo 2013-2016 (único período donde el CREE fue un impuesto real y `C3R20` es informativo), Causa 2 y 4 en todos los años. Por eso esta sección NO es un *waterfall* que sume 100% sobre las firmas incoherentes de 2022 -- se reporta la fuerza de la evidencia de cada causa en su propia ventana válida.

### Causa 1: base reducida del salario integral -- asociada, NO resuelta

Ver `IP_T01_contingencia_causa1`, `IP01_magnitud_vs_peso_integral`, `IP_T02_correccion_causa1`. La asociación es fuerte: 80.95% de las firmas con salario integral positivo son incoherentes, frente a 61.14% de las que no reportan salario integral (chi²=2789, p≈0, 2008-2019). Pero la corrección probada -- un divisor mezclado entre 0.05 (tasa plena) y 0.023 (tasa reducida al 70% de base) según el peso de `C3R1` en la nómina -- **no reduce la incoherencia**: pasa de 65.27% a 65.51%, prácticamente sin cambio. La corrección, si acaso, empeora ligeramente. Hipótesis más plausible: la corrección solo ajusta la fórmula tasa→fracción, pero `w_promedio` (el salario promedio contra el que se contrasta) probablemente también está distorsionado -- `C3R2` (sueldos permanentes) parece EXCLUIR a los trabajadores con salario integral (son filas separadas del cuadro 3), pero el conteo de personal permanente (`C4R2`) probablemente SÍ los incluye, así que `w_promedio` se calcula dividiendo una nómina que excluye a los integral-salariados entre un conteo que sí los incluye -- subestimándolo. No se pudo corregir esto sin un conteo específico de trabajadores con salario integral, que la EAM no reporta por separado. **Veredicto: asociada, pero no se logró una corrección funcional con los datos disponibles.**

### Causa 2: mezcla de poblaciones en el denominador -- rechazada

Ver `IP_T03_incoherencia_por_intensidad_temporal`, `IP02_incoherencia_vs_intensidad_temporal`, `IP_T04_causa2_poblaciones_emparejadas`. La correlación entre incoherencia e intensidad de temporal directo es prácticamente nula (-0.019), y por bandas no hay un patrón monótono (31.7% / 42.3% / 44.7% / 33.4% de incoherencia según la intensidad crece). La prueba decisiva: usar poblaciones perfectamente emparejadas (solo `C3R2`, solo conteo de permanentes) **empeora** la incoherencia, de 34.9% a 51.24% -- de las firmas incoherentes bajo la base mixta, solo 0.75% se vuelve coherente al emparejar poblaciones. **Veredicto: rechazada.** La base C2+C4 no solo es mejor para la bimodalidad (ya establecido en la validación anterior, 98% vs. 85.67% dentro de rango), también es mejor para la coherencia -- no es la causa del problema.

### Causa 3: firmas no contribuyentes de renta -- la evidencia más fuerte, no verificable en 2022

Ver `IP_T05_causa3_c3r20_por_sector`, `IP_T06_causa3_resumen`. **Hallazgo previo al diagnóstico mismo**: `C3R20` (Impuesto de Renta para la Equidad, CREE) existe en la macrobase 2013-2024, pero el CREE fue eliminado por la Ley 1819 de 2016 -- confirmado empíricamente, 64%-78% de firmas lo reportan positivo en 2013-2016, cae a ~0% desde 2017. Probar esta causa en 2022 (como el resto del diagnóstico) habría sido inútil. Se usó 2013-2016, el último período donde `C3R20` discrimina. Resultado: 66.8% de las firmas incoherentes reporta `C3R20`>0, frente a 77.82% de las coherentes -- una brecha de 11 puntos porcentuales (chi²=506, p=4.6e-112), **consistente en dirección en los 23 sectores con datos suficientes** (diferencia promedio 9.98pp, coherentes siempre reportan más). Es la evidencia más limpia y consistente de las 4 causas. **Pero no se puede verificar directamente en 2022**, el año de interés, porque `C3R20` ya no discrimina ahí. Es plausible que el estatus de contribuyente de renta sea una característica relativamente estable de la firma en el tiempo, pero eso no se comprobó -- es una extrapolación, no un hallazgo directo para 2022.

### Causa 4: ruido de reporte -- mayoritariamente sí, con un núcleo estructural

Ver `IP_T07_persistencia_anual`, `IP03_persistencia_condicional`, `IP_T08_matriz_persistencia_2019_2021_2022`. Antes de 2013 la incoherencia es casi universal (~98%, esperado: el concepto de "fracción sobre 10 SMLV" no aplica sin la exención). Desde 2013, la persistencia condicional (57%-70%, dado que la firma fue incoherente el año anterior) es sistemáticamente más alta que la tasa base (25%-35%) -- **no es ruido puro**, hay un componente estructural. La matriz 2019-2021-2022 lo cuantifica: 40.42% de las firmas nunca es incoherente en esos 3 años (compatible con ruido en los casos aislados que sí aparecen), pero 10.46% es incoherente **los 3 años** -- un núcleo persistente y estructural, sin explicación identificada por las causas 1-3. El resto (49.12%) es intermitente. **Veredicto: la incoherencia NO es ruido puro** -- hay un subconjunto persistente de firmas (~10%) con una causa estructural todavía sin identificar.

### Qué usos quedan habilitados (no se decide aquí)

**Uso 1 (medida de tratamiento)**: NO habilitado con la evidencia actual. Ninguna corrección probada resuelve la incoherencia en el año de interés (2022); usar la fracción implícita firma por firma como medida de exposición heredaría ese ~34% de incoherencia sin explicación aplicable.

**Uso 2 (control en la especificación principal)**: sigue habilitado. Este uso solo requiere que la tasa ORDENE bien la exposición relativa entre firmas, no que sea exacta firma por firma. La correlación de -0.05 con `Bite2022_obreros` (validación anterior) y la validación externa fuerte contra salario integral (p<1e-40) sostienen que hay señal real y en buena medida ortogonal a Kaitz, incluso con el ~34% de casos individualmente incoherentes -- mientras la incoherencia no esté sistemáticamente correlacionada con el propio Kaitz de forma que invierta el ordenamiento (no se probó esto explícitamente, queda pendiente antes de usar la tasa como control).

**Uso 3 (partición de muestra)**: sigue habilitado, y es el menos exigente de los tres. Mostrar que el resultado principal se sostiene en el subconjunto de firmas con fracción baja sobre 10 SMLV no requiere que la fracción sea exacta para las firmas incoherentes -- solo que las firmas de fracción claramente baja (cerca de 0.04) estén correctamente clasificadas, y esas son precisamente las menos propensas a la incoherencia (la incoherencia ocurre cuando la fracción implícita es inverosímilmente ALTA, no baja).

**Nota sobre persistencia (Causa 4)**: los usos 2 y 3 sobreviven aunque la Causa 4 sea dominante en parte de la muestra, siempre que el ordenamiento entre firmas sea estable -- pero el núcleo del 10.46% con incoherencia persistente en los 3 años (2019, 2021, 2022) es señal de un riesgo real: si esas firmas también tienen Kaitz sistemáticamente distinto, podrían sesgar el uso 2. No se probó esto tampoco -- queda pendiente.

**Nota para la tesis**: la Ley 1607 de 2013 aparece dos veces en este proyecto -- se descartó en su momento como choque de interés (no era el foco de la tesis), y reaparece aquí como instrumento de medición de la distribución salarial, aprovechando su propia estructura de exención para inferir la composición de la nómina. Es un uso más interesante que el original y vale la pena contarlo así.

