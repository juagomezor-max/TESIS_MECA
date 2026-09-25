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
