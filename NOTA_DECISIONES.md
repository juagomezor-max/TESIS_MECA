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
