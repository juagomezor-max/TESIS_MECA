# Nota de decisiones — análisis manual

Registro fechado de decisiones metodológicas. Cada regla de decisión se comitea ANTES de correr el bloque que la aplica.

## 2026-09-16 — Decisiones tomadas (bloques 4.2, 4.4 y 4.5 ya corridos)

| Elemento | Decisión | Evidencia |
|---|---|---|
| Control de tamaño | Fijado en 2022 (`tamano_2022`); el tamaño contemporáneo es un bad control para outcomes de empleo | 4.5: con el mismo N, Exposure × empleo_total pasa de p=0,146 (contemporáneo) a p=0,012 (2022) |
| Efectos fijos de año | `ANIO_F` en todas las especificaciones | Sin `ANIO_F`, el primer eslabón "sin controles" de Exposure daba p=1,5e-19; con `ANIO_F`, p=0,98 |
| Exposure2022_obreros | Descartada como medida de tratamiento | Sin primer eslabón (p=0,88 con controles; p=0,98 sin) y pre-tendencias rechazadas en empleo total y temporales con controles 2022, en panel de firma y de establecimiento |
| Bite × participación de permanentes y temporales | Fuera como outcomes causales | 4.4: el pico/mínimo de las pre-tendencias se desplaza con el año base de Bite (2019, 2021, 2022) |
| Bite × empleo permanente | No identificado; solo descriptivo | 4.4: con base 2019, los niveles 2015-2019 coinciden con 2023-2024 |
| Resultado principal | Empleo total | Pre-tendencias no rechazan con ninguna base de Bite (p=0,46 / 0,18 / 0,78) ni en ningún panel con controles 2022 (p=0,90–0,93) |
| Referencia del efecto | Promedio pre-período, no 2022 | 2022 queda por encima del promedio pre en Bite × empleo total (+1,2 con base 2022) |
| Panel principal | Empresa-año; establecimiento como robustez | 4.5: las conclusiones de Bite no cambian entre paneles |
| Diagnóstico de atrición archivado | Sin bad control ni omisión de efecto fijo de año; hay que rehacerlo con Bite | `extender_diagnostico_atricion_diferencial.R` fija el tamaño con PERTOTAL del año base (líneas 139-141) y es de corte transversal por cohorte; usa Exposure, ya descartada |

## 2026-09-16 — Regla de decisión para el bloque 4.6 (fijada antes de correrlo)

Contenido del bloque 4.6:
1. Primer eslabón con Bite 2019 y con Bite promedio 2019/2021 (salto 2023 del salario promedio frente al promedio pre).
2. Contraste de empleo total "promedio post − promedio pre" con error estándar, en niveles y en logaritmo, para Bite base 2019, 2021 y 2022, con efecto mínimo detectable.

| Resultado | Decisión |
|---|---|
| Bite 2019 predice el salto de costo 2023 (p<0,05) **y** el contraste de empleo total es robusto entre niveles y logaritmo | Bite 2019 pasa a ser la medida principal; empleo total, el resultado principal |
| Bite 2019 no tiene primer eslabón | Se mantiene Bite 2022, con contraste contra el promedio pre y sensibilidad al año base declarada |
| El contraste en logaritmo pierde significancia | El resultado se reporta como nulo acotado, con IC y efecto mínimo detectable |

Estas reglas no se revisan según los resultados de empleo.

## 2026-09-16 — Precisión operativa (antes de correr 4.6)

- Medida evaluada: Kaitz 2019 en la muestra amplia (todas las firmas con esa medida). La muestra de 4.650 firmas con Kaitz 2019, 2021 y 2022 es chequeo de consistencia.
- Primer eslabón: contraste de la sección 4.3, b_2023 − (b_2019 − b_2016)/3, controles 2022. Pasa si p < 0,05 **y** el signo es positivo.
- Empleo total, especificación principal: promedio de coeficientes 2023–2024 menos promedio de 2015–2019, 2021 y 2022 (2022 = 0 por ser referencia). Robustez: el mismo contraste excluyendo 2022.
- "Robusto entre niveles y logaritmo": mismo signo y p < 0,05 en ambos.
- Si el primer eslabón de Kaitz 2019 falla, la clasificación del empleo se hace con Kaitz 2022 en la muestra amplia.
- Efecto mínimo detectable: 80% de potencia, prueba bilateral al 5%.
- Nota: la cifra de "−2,0 a −2,7 trabajadores" del commit del bloque 4.4 excluía 2022 del promedio pre; corresponde a la versión de robustez, no a la principal.

## 2026-09-17 — Decisión: elección de la medida de exposición (Kaitz 2022, 2019 o promedio)

**Motivo.** Las validaciones mostraron que Kaitz 2022 alto identifica, además de firmas que pagan cerca del mínimo, firmas con un 2022 atípicamente malo: el salario, las ventas y la brecha salarial tienen una "V" con mínimo en 2022. Con Kaitz 2019, el salto salarial de 2023 es +2,2% frente a +4,4% con Kaitz 2022.

**Medidas candidatas** (mismas firmas, cada una en sus propias desviaciones estándar):
1. Kaitz 2022: mínimo 2023 / salario del obrero permanente en 2022.
2. Kaitz 2019: mínimo 2020 / salario del obrero permanente en 2019.
3. Kaitz promedio: promedio de Kaitz 2019, 2021 y 2022 (mínimo del año siguiente / salario del obrero de ese año), cada año recortado al 1%-99%, firmas con al menos 2 de 3 años.

**Regla de decisión:**
1. Pasa el primer eslabón si el salto salarial de 2023 frente al cambio típico 2015-2018 es positivo y significativo al 5%.
2. Entre las que pasan, la principal es la de mayor M de quiebre del cambio salarial 2022 -> 2023 (sensibilidad simple de V1).
3. El empleo no se usa para elegir; se reporta con las tres medidas.
4. Si ninguna pasa, se declara un problema de diseño y se evalúa construir la exposición desde la distribución salarial de la firma.

**Transparencia.** Esta regla se escribe después de haber visto el primer eslabón y el empleo con Kaitz 2022 y Kaitz 2019 (validación V3), pero antes de ver cualquier resultado con Kaitz promedio y antes de calcular el M de quiebre con Kaitz 2019.

## Pendientes conocidos

- Sección 6 (Robustez) no corrida con el script corregido; `4. RESULTADOS/Robustez/tabla_robustez_escrutinio.docx` corresponde al script anterior y no debe citarse.
- Repetir el diagnóstico de atrición diferencial con Bite en lugar de Exposure.
- Diferencia menor no explicada entre la fila 1 de la conciliación 4.5 y el análisis archivado (0,349/0,610/0,057/0,116 vs. 0,311/0,574/0,057/0,101).
- Tablas compactas de la conciliación 4.5 no generadas; usar `tabla_conciliacion_ventana_pre.csv`.

## Sesión 2026-09-17: bases ampliadas, script principal, validaciones y elección de medida

### 1. Bases ampliadas
- Script: 0. ANALISIS INICIAL IA/3. SCRIPTS/construccion/ampliar_variables_paneles.R. Agrega a los dos paneles de 1. DATOS/ variables de la macrobase (costos laborales por categoría, tipos de contrato, inversión, producción, inventarios, energía) sin cambiar filas ni columnas originales.
- Verificación con datos reales: panel de firmas 62.816 filas, 183 columnas, 9.087 firmas; panel de plantas 68.447 filas. Identidad del empleo (empleo_total = permanentes + temporal directo + temporal agencias + aprendices): 62.816 de 62.816 firmas-año. El empleo total incluye aprendices.
- Salario promedio: ahora sale del panel de firmas (costos_totales_personal_total_c3r10c3 / empleo_total). Coincide con el cálculo anterior por suma de plantas: correlación 1, 0 firmas-año con diferencia mayor a 1%.
- La EAM no tiene horas trabajadas: hay que retirarlas de los objetivos de la propuesta.

### 2. Organización de resultados
- 01_resultados_principales.R guarda en 4. RESULTADOS/Descriptivos, Principal, Estimacion y Validaciones (T00 y T15), con gráficos en subcarpetas "figuras".
- 02_validaciones.R guarda todo en 4. RESULTADOS/Validaciones/script_validaciones.
- Los archivos que empiezan con "tabla_" en esas carpetas son del script anterior (00_script_base.R); no citarlos.

### 3. Primer eslabón: tres lecturas (Kaitz 2022, muestra completa)
- A. Cambio del salario 2022 -> 2023: +4,04% por DE (EE 0,57).
- B. Salto 2023 frente al cambio típico 2016-2019: +4,79% por DE.
- C. Promedio después menos promedio antes (DiD simple): -1,00% (p = 0,012).
- C sale negativo porque el salario relativo de las firmas con Kaitz 2022 alto cae de forma sostenida entre 2015 y 2022 (prueba de años previos p < 0,001). El primer eslabón se lee con A y B, no con C.

### 4. Resultado principal (sin cambios)
- Empleo total: -1,73 trabajadores (p = 0,198); en log -0,96% (p = 0,200). Años previos: p = 0,917 (niveles) y 0,454 (log). Nulo acotado.

### 5. Resultados de las validaciones (Kaitz 2022 salvo que se indique)
- V1 Sensibilidad a diferencias previas: salario, M de quiebre 0,76. Empleo (log) promedio 2023-24 frente a 2022: -1,91% (p = 0,002), M de quiebre 0,49. Honest DiD formal (magnitudes relativas): Mbar 0,5 [-3,99%; +0,16%]; Mbar 1 [-5,43%; +1,57%]; Mbar 2 [-8,52%; +4,67%]. La caída frente a 2022 no es robusta.
- V2 Período de comparación, empleo (log): frente a 2022 -1,91%***; frente a 2021-22 -1,52%**; frente a 2015-19 -0,74% (p = 0,419); frente a todos los previos -0,96% (p = 0,203). Salario frente a 2015-19: -2,62%***, reflejo del declive previo de las firmas seleccionadas.
- V3 Kaitz 2019 (4.744 firmas, correlación con Kaitz 2022 = 0,565): salto salarial 2023 frente a 2015-2018 +2,21% (p < 0,001), frente a +4,43% con Kaitz 2022 en las mismas firmas. Con Kaitz 2019 el salario de 2023-24 supera el nivel de 2015-17. Empleo (log) con Kaitz 2019: +1,08% (p = 0,154), años previos p < 0,001.
- V4 2023 y 2024 por separado, empleo (log): solo 2023 -0,86% (p = 0,282); solo 2024 -1,06% (p = 0,217).
- V5 Resultados exploratorios: los siete rechazan años previos (empleo permanente p = 0,024; temporal p = 0,003; participación de permanentes, ventas, salario obrero y brechas salariales p < 0,001). Salen como resultados causales.
- V6 Placebo 2018 (datos 2015-2019): empleo (log) -0,83% (p = 0,219), empleo en trabajadores +0,03 (p = 0,98). Salario (-1,89%***) y ventas (-2,88%***) sí dan "efecto" placebo por su tendencia previa.
- V7 Sin salarios extremos (476 firmas-año fuera): empleo (log) -1,24% (p = 0,099).
- V9 Panel balanceado (4.472 firmas): empleo (log) -0,85% (p = 0,262).
- V10 Errores agrupados por sector: empleo (log) p = 0,190; salario p < 0,001.

### 6. Elección de la medida de exposición (V11)

Regla de decisión: ya registrada arriba, sección "2026-09-17 — Decisión: elección de la medida de exposición" — no se repite aquí.

Resultados (4.744 firmas comunes; correlaciones: K22-K19 0,565; K22-promedio 0,877; K19-promedio 0,829):

| Medida | Salto salarial 2023 | M de quiebre | Empleo (log) después-antes | Años previos empleo (log) |
|---|---|---|---|---|
| Kaitz 2022 | +4,43%*** | 0,72 | -0,89% (p = 0,251) | p = 0,232 |
| Kaitz 2019 | +2,21%*** | 0,05 | +1,08% (p = 0,154) | p < 0,001 |
| Kaitz promedio | +4,02%*** | 0,76 | -0,29% (p = 0,717) | p = 0,001 |

- La regla elige Kaitz promedio, por un margen mínimo frente a Kaitz 2022 (0,76 frente a 0,72). Ninguna medida supera M = 1.
- Kaitz promedio rechaza años previos en el empleo; Kaitz 2022 no.
- DECISIÓN PENDIENTE: no se cambia todavía la medida del script principal. Se discute con el director (Andrés Ham) con estos resultados.

Transparencia: la regla de decisión ya estaba registrada en NOTA_DECISIONES.md desde el commit `e8a13c1` (2026-09-17 01:11:05 -0500), anterior a esta corrida de 02_validaciones.R. Además, antes de escribirla ya se conocían el primer eslabón y el empleo con Kaitz 2022 y Kaitz 2019 (V3).

### 7. Controles (V12): el tamaño es el que importa
| Controles | Empleo (log) | Años previos |
|---|---|---|
| Firma y año | -4,77%*** | p < 0,001 |
| + sector x año | -4,57%*** | p < 0,001 |
| + tamaño x año | -0,85% (p = 0,240) | p = 0,536 |
| + departamento x año | -4,48%*** | p < 0,001 |
| + sector y tamaño x año | -0,98% (p = 0,196) | p = 0,437 |
| + sector y departamento x año | -4,33%*** | p < 0,001 |
| Completo | -0,96% (p = 0,200) | p = 0,454 |
- Sin el control de tamaño los grupos no vienen parecidos antes de 2023; con él, sí. Justificación: 65% de las pequeñas están en alta exposición frente a 19% de las grandes.
- Riesgo: el tamaño se fija con el empleo de 2022, que es un año de pico para las firmas expuestas.

### 8. Hallazgos que cambian la lectura
- Kaitz 2022 alto selecciona firmas con declive relativo sostenido de salarios y ventas entre 2015 y 2022, no solo un 2022 atípico.
- Compresión salarial: descartada. La brecha administrativo-obrero está plana en torno a -0,11 y salta a 0 exactamente en 2022 (efecto del año base).
- Empleo relativo de las firmas expuestas: bajo en 2015-19, alto en 2021-22 y bajo otra vez en 2023-24, con las tres medidas. Hipótesis (no demostrada, la EAM no identifica beneficiarios): el pico de 2021-22 coincide con el PAEF y el incentivo a nuevos empleos (Ley 2155), que pesaban más en firmas de salarios bajos. Las comparaciones frente a 2021-2022 mezclan el mínimo con el fin de esos subsidios.
- Lectura actual: el aumento de 2023 subió más el costo laboral de las firmas expuestas (entre 2% y 4% por DE según la medida; la evidencia más limpia, Kaitz 2019, apunta al extremo bajo). No hay evidencia de caída del empleo; el diseño no descarta caídas moderadas.

### 9. Pendientes (sesión 2026-09-17)
1. Decidir con el director la medida principal (sección 6).
2. Validación: tamaño medido en 2019 en lugar de 2022.
3. Validación: comparación frente a 2015-2019 con las tres medidas (se agrega después de ver el pico de 2021-22; declararlo).
4. Validación: tendencia lineal por firma, por el declive sostenido de las firmas con Kaitz 2022 alto.
5. Cifras oficiales DANE / Banco de la República: aumento real del mínimo e inflación 2015-2024.
6. Retirar "horas trabajadas" de los objetivos de la propuesta.
7. Ajustes de gráficos: "p = 0" como "p < 0,001", nombre del año de Kaitz en los subtítulos, separar salario y empleo en GR01 y usar las siete versiones de V12.

## Validación 2026-09-17: resultados incluyendo 2020

**Motivo.** La especificación principal excluye 2020 por la pandemia, que afectó con más fuerza a las firmas pequeñas y de salarios bajos (las más expuestas). Esta validación responde "¿y con 2020 qué pasa?".

**Datos.** 1. DATOS/panel_analitico_firma_eam_con_2020.rds (commit fa33397): 69.539 firmas-año, 9.087 firmas; 6.723 firmas en 2020. Comprobación contra el panel principal en los demás años: 108 de 108 comparaciones al 100% (12 variables x 9 años). Chequeo de 2020: 0% de vacíos en costo laboral y sueldos de obreros; mediana del costo por trabajador 25.347 (entre 24.722 en 2019 y 27.875 en 2021).

**Script.** 3. SCRIPTS/01b_resultados_principales_con_2020.R. Salidas en 4. RESULTADOS/RESULTADOS2020/. Base de análisis: 49.670 firmas-año, 5.099 firmas.

**Resultados (Kaitz 2022, mismos controles)**
| | Sin 2020 | Con 2020 |
|---|---|---|
| Firmas-año | 44.631 | 49.670 |
| Salario, cambio 2022 -> 2023 | +4,04%*** | +4,03%*** |
| Salario, salto 2023 frente a 2016-2019 | +4,79%*** | +4,78%*** |
| Empleo (trabajadores) | -1,73 (p = 0,198) | -1,72 (p = 0,190) |
| Empleo (log) | -0,96% (p = 0,200) | -0,90% (p = 0,221) |
| Años previos, empleo (trabajadores) | p = 0,917 | p = 0,954 |
| Años previos, empleo (log) | p = 0,454 | p = 0,554 |
| Pequeñas, empleo (log) | -1,97% (p = 0,080) | -1,81% (p = 0,099) |
| Brecha administrativo-obrero | -1,30%** | -0,73% (p = 0,247) |

**Lectura.**
- Incluir 2020 no cambia la conclusión: el primer eslabón y el resultado del empleo son prácticamente idénticos, y la prueba de años previos del empleo mejora levemente.
- En los promedios sin controles, 2020 muestra una caída del empleo mayor en las firmas expuestas; con el control de tamaño x año, 2020 queda alineado con los demás años previos (empleo en trabajadores +1,01; en log -1,38%, entre 2019 y 2021).
- La compresión salarial deja de ser significativa con 2020, lo que refuerza que era frágil.

**Decisión.** La especificación principal sigue excluyendo 2020. Los resultados con 2020 se reportan como robustez.

**Nota sobre la muestra de Nicolás.** Sus tablas de Kaitz x Exposure tienen 49.969 firmas-año; con 2020 nosotros tenemos 49.670. 2020 explica casi toda la diferencia, pero quedan 299 filas por aclarar antes de reportar sus resultados junto a los nuestros.

**Nota sobre T00b.** Como se usó la base construida en VS Code (camino A del script), la tabla T00b compara todos los años distintos de 2020 contra el panel principal, aunque su título diga "Reconstrucción de 2019 y 2021 desde la macrobase". Corregir el título si se cita.
