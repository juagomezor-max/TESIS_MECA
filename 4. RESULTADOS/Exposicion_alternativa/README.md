# Panel firma-año EAM completo, con 2020, para medidas de exposición alternativas

Generado por `construir_panel_completo.R`. **No construye ninguna medida de exposición ni calcula ratios/promedios/índices.** Solo extrae, agrega a firma-año, valida y documenta. Las fórmulas de exposición van en un script posterior, que consume este panel.

Panel: `130859` filas (firma-año), `293` columnas, `11687` firmas distintas, años 2008-2024 (incluye 2020, a diferencia del panel analítico oficial).

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

## Resultado de las identidades (PASO 3)

Ver tablas `EA_T01_identidad_empleo` y `EA_T02_identidad_costos`. `EA_T01`: permanentes+propietarios+temporal_directo+temporal_agencias+aprendices (3 categorías) = `PERTOTAL`. Esta MISMA descomposición de 5 componentes por categoría se validó al 100.00% contra la macrobase antes de escribir este script (ver PASO 0.2/0.3 arriba); al re-verificarla ya en el panel agregado a firma-año, el porcentaje que coincide exacto varía por año -- rango observado: 100% a 100%. La caída frente al 100% de la macrobase es un artefacto conocido de la regla de agregación 'todas NA -> NA, si no suma con na.rm=TRUE' aplicada columna por columna: si en una firma-año una columna tiene NA en algunas plantas/años pero otra columna relacionada no, cada suma trata esos NA de forma independiente y la identidad se puede romper aunque cada columna esté bien agregada por separado. Es la MISMA regla ya validada y usada por `pipeline/01_construir_base.R`, no una regla nueva. `EA_T02` (costos, `C3R10C1+C3R10PT+C3R10C2=C3R10C3`) usa las mismas 4 columnas sin desagregar en sub-componentes, así que no hereda este artefacto de la misma forma.

## Comparación contra `panel_analitico_firma_eam.rds`

Ver tabla `EA_T04_comparacion_panel_actual`. Comparación firma a firma en empleo, ventas y costo laboral total, en los años comunes a ambos paneles (2015-2019, 2021-2024 -- el panel actual no tiene 2020).

## 2020 en cobertura

`obreros_permanentes`: 0% de faltantes en 2019 vs. 0% en 2020 (ver tabla `EA_T03_cobertura_variables_clave` completa para todas las variables clave y todos los años). Esta cifra es la que debe informar si 2020 se usa o no en la estimación -- no se tomó esa decisión aquí.

## Variables buscadas y no encontradas, o con categoría ambigua

- **Tamaño de empresa (variable cruda), año de inicio de operaciones, organización jurídica**: NO existen en la macrobase bajo ningún nombre plausible (se buscó por patrones TAMA*, ORGJUR/ORGANIZ/JURID, INICI/ANOINI/FUNDA/APERTU -- cero resultados). `tamano_empresa` SÍ se generó, pero es una variable DERIVADA (misma regla `tamano_de()` que ya usa `03_construir_panel.R`), no un campo crudo de la encuesta.
- **`CIIU3`**: existe, pero con cobertura muy limitada -- confirmar años antes de usarlo (columna `anios_disponibles` del diccionario).
- **`R1CSAP`/`R2CSAP`/`R3CSAP`** (apoyo de sostenimiento de aprendices): sus etiquetas en el diccionario EAM están truncadas de forma idéntica ("...aprendices y pasantes (Ley 789"), no se pudo determinar cuál corresponde a obreros/profesional-técnico/administrativos sin adivinar. Se extrajeron como 3 variables independientes con `categoria_ocupacional = "no_determinada"`. `R4CSAP` sí dice "Total" explícitamente.

## Qué queda pendiente

- Decidir la categoría ocupacional real de `R1CSAP`/`R2CSAP`/`R3CSAP` (posiblemente contactando la ficha metodológica del DANE, no solo el diccionario extraído del DOCX).
- Decidir si 2020 entra o no a la estimación, con base en la cobertura reportada aquí.
- Ninguna medida de exposición se calculó -- ese es el script siguiente, que debe leer `panel_firma_eam_expalt_completo.rds` y el diccionario para saber qué numeradores y denominadores son compatibles (columna `poblacion`).
