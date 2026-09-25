# Cronología de decisiones metodológicas

Reconstrucción a partir de `git log` sobre **todas las ramas** (los commits
relevantes están repartidos entre `feature/medida-exposicion-alternativa`,
`feature/continuo-y-descriptivos` y `refactor/estructura-pipeline`) más el
contenido de `NOTA_DECISIONES.md` (existe en las dos últimas, no en esta
rama — ver la auditoría anterior, sección 0). Este documento describe el
orden en que ocurrieron las cosas y con qué evidencia contaba cada decisión
en su momento. **No evalúa si las decisiones fueron correctas** — eso les
corresponde a los autores.

Cada entrada cita el commit exacto (`git show -s --format="%h %ci %s" <hash>`)
y, cuando aplica, el archivo y la línea donde quedó registrada.

---

## 1. Construcción de `Exposure2022_obreros`

| Campo | Contenido |
|---|---|
| Fecha y commit | `1be1afa`, 2026-08-09 16:51 |
| Decisión | Construir una medida de exposición basada en la composición ocupacional (proporción de obreros), separada de la `Exposure2022` original |
| Evidencia disponible entonces | Ninguna de resultado — es la medida de partida, construida antes de cualquier prueba |
| Evidencia que NO existía todavía | Que fallaría la primera etapa (eso se supo después, sección 4) |
| Alternativas consideradas | El commit conserva `Exposure2022` original junto a la nueva, según su propio mensaje ("conservando Exposure2022 original") — no se descartó nada todavía |
| Dónde quedó registrada | `0. ANALISIS INICIAL IA/3. SCRIPTS/pipeline/02_construir_exposicion.R`, commit `1be1afa` |

## 2. Validación inicial de `Exposure2022_obreros`

| Campo | Contenido |
|---|---|
| Fecha y commit | `4fc7a51` (17:04) y `401964d` (17:07), 2026-08-09 |
| Decisión | Generar diagnósticos de validación y un resumen metodológico (Pasos 6-7) antes de decidir nada sobre su uso |
| Evidencia disponible entonces | Solo la construcción del Paso 1 |
| Evidencia que NO existía todavía | El resultado de la primera etapa (sección 4) y la existencia de Bite (sección 5, un día después) |
| Alternativas consideradas | No registradas en el mensaje de commit |
| Dónde quedó registrada | Mensaje de commit; documento en `0. PREPARACION/` según el propio texto de `401964d` |

## 3. Construcción de `Bite2022_obreros`

| Campo | Contenido |
|---|---|
| Fecha y commit | `2f290cf`, 2026-08-10 22:32 — **un día después** de Exposure2022_obreros |
| Decisión | Construir el índice de Kaitz con salario base del obrero permanente (`Bite2022_obreros = SM_2023_mensual×12/1000 ÷ salario_promedio_obrero_2022`) |
| Evidencia disponible entonces | La construcción y validación inicial de Exposure (secciones 1-2); no hay en el mensaje de commit una razón explícita de por qué se construye una segunda medida en este punto |
| Evidencia que NO existía todavía | El resultado de primera etapa de ninguna de las dos medidas |
| Alternativas consideradas | No registradas en el mensaje de commit |
| Dónde quedó registrada | `0. ANALISIS INICIAL IA/3. SCRIPTS/pipeline/02_construir_exposicion.R:112`, commit `2f290cf` |

## 4. Validación de Bite y correlación Bite-Exposure

| Campo | Contenido |
|---|---|
| Fecha y commit | `d141151` (22:33) y `f011fa6` (22:39), 2026-08-10 |
| Decisión | Diagnosticar Bite y su robustez frente a Exposure, el mismo día de su construcción |
| Evidencia disponible entonces | Ambas medidas construidas, sin resultado de primera etapa todavía |
| Evidencia que NO existía todavía | Primera etapa de ninguna medida (eso llega en septiembre, sección 6 abajo) |
| Alternativas consideradas | No registradas |
| Dónde quedó registrada | Mensajes de commit `d141151`, `f011fa6` |

## 5. Consolidación del panel (ventana 2012-2024, CIIU4)

| Campo | Contenido |
|---|---|
| Fecha y commit | `9174215` (2026-09-01 00:50, "Pipeline simplificado... 33/33 CIFRAS_CLAVE verificadas"); reconstrucción del panel ampliado en `54241b6` (2026-09-21 19:43) y cierre en `2ed8d21` (2026-09-24 19:54, "6 correcciones tras revisión del diccionario") |
| Decisión | Fijar el panel firma-año desde 2012 (límite impuesto por la disponibilidad de CIIU4) como base de todo el pipeline |
| Evidencia disponible entonces | 33/33 cifras clave verificadas contra la EAM (según el mensaje del commit `9174215`) |
| Evidencia que NO existía todavía | Que ningún script del pipeline final terminaría usando años anteriores a 2015 — eso se confirma en esta misma auditoría (ver `AUDITORIA_CADENA.md`, eslabón 1), no estaba verificado en el momento de construir el panel |
| Alternativas consideradas | No hay tabla de correspondencia verificable CIIU3→CIIU4 (0 años de solapamiento) — mencionado en `NARRATIVA.md` sección 3, ítem 7, pero sin fecha de commit específica que registre cuándo se investigó esa alternativa y se descartó |
| Dónde quedó registrada | `0. ANALISIS INICIAL IA/3. SCRIPTS/pipeline/03_construir_panel.R`; la razón (CIIU4) en `NARRATIVA.md` (documento posterior, de esta sesión, no del momento de la decisión) |

## 6. `NOTA_DECISIONES.md`: controles y descarte de Exposure

| Campo | Contenido |
|---|---|
| Fecha y commit | `ac7ba36`, 2026-09-16 22:36 |
| Decisión | (a) Fijar controles (tamaño, sector, depto) en 2022, no contemporáneos — evita bad control; (b) descartar `Exposure2022_obreros` como medida de tratamiento |
| Evidencia disponible entonces | Con el mismo N, Exposure×empleo_total pasa de p=0,146 (control contemporáneo) a p=0,012 (control 2022); Exposure sin primer eslabón: **p=0,88 con controles, p=0,98 sin controles**; pre-tendencias rechazadas en empleo total y temporales |
| Evidencia que NO existía todavía | La cifra exacta "p=0,99" que cita esta auditoría es de una corrida posterior (`03_primer_eslabon_medidas.R`, commit `e1afa75`, 24-sep) — mismo resultado cualitativo, número no idéntico porque la especificación se afinó entre medias |
| Alternativas consideradas | Sí, explícitamente: la tabla del commit compara Exposure con Bite en el mismo bloque |
| Dónde quedó registrada | `NOTA_DECISIONES.md`, sección "2026-09-16 — Decisiones tomadas", fila "Exposure2022_obreros" |

## 7. Primera etapa de Exposure con la cifra citada en esta auditoría (p=0,99)

| Campo | Contenido |
|---|---|
| Fecha y commit | `e1afa75`, 2026-09-24 21:53 (fecha de commit; la corrida real es de antes, sin fecha propia en el archivo — ver `AUDITORIA_CADENA.md` sección 0 sobre archivos no rastreados) |
| Decisión | Ninguna nueva — es la confirmación, con la especificación final del pipeline, del hallazgo de agosto/16-sep |
| Evidencia disponible entonces | Todo lo de las entradas 1, 2 y 6 |
| Evidencia que NO existía todavía | N/A — es la culminación, no el origen |
| Alternativas consideradas | N/A |
| Dónde quedó registrada | `3. SCRIPTS/03_primer_eslabon_medidas.R` (tabla T02, medida Exposure, muestra propia) |

## 8. Construcción de las medidas alternativas (`golpe_c`, `golpe_a`, `golpe_costo`)

| Campo | Contenido |
|---|---|
| Fecha y commit | **No se pudo fechar con precisión.** Primera aparición en cualquier archivo `.R` versionado: `e1afa75`, 2026-09-24 21:53 — pero ese commit trackea seis scripts que ya existían sin rastrear en disco (ver `AUDITORIA_CADENA.md` sección 0). La fecha real de escritura del código es anterior y no está en git |
| Decisión | Construir tres medidas adicionales de exposición además de Bite y Exposure |
| Evidencia disponible entonces | No verificable — no hay commit intermedio que documente el razonamiento |
| Evidencia que NO existía todavía | No verificable |
| Alternativas consideradas | No verificable desde el historial de git |
| Dónde quedó registrada | Únicamente en el código de `3. SCRIPTS/02_medidas_exposicion.R`, sin nota de decisión asociada en ningún `NOTA_DECISIONES.md` de ninguna rama |

## 9. Regla de decisión de medida — versión 1 (M de quiebre, Kaitz 2022/2019/promedio)

| Campo | Contenido |
|---|---|
| Fecha y commit | `e8a13c1`, 2026-09-17 01:11 |
| Decisión | Elegir entre Kaitz 2022, Kaitz 2019 y Kaitz promedio según: (1) primer eslabón positivo y significativo al 5%; (2) entre las que pasan, la de mayor M de quiebre (Honest DiD); (3) el empleo no se usa para elegir |
| Evidencia disponible entonces | Primer eslabón y empleo ya corridos con Kaitz 2022 y Kaitz 2019 (validación V3, mencionada explícitamente como conocida antes de escribir la regla) |
| Evidencia que NO existía todavía | El resultado de aplicar la regla (llega 10 minutos después, entrada 10) y Kaitz promedio (explícitamente: la nota dice que la regla se escribe "antes de ver cualquier resultado con Kaitz promedio") |
| Alternativas consideradas | Sí — las tres versiones de Kaitz están explícitamente comparadas en el mismo documento |
| Dónde quedó registrada | `NOTA_DECISIONES.md`, sección "2026-09-17 — Decisión: elección de la medida de exposición" (rama `feature/continuo-y-descriptivos` / `refactor/estructura-pipeline`, no en esta rama) |

## 10. Resultado de la regla v1 — decisión dejada pendiente

| Campo | Contenido |
|---|---|
| Fecha y commit | `10fe264`, 2026-09-17 01:22 |
| Decisión | Ninguna — **se registra explícitamente que la decisión NO se toma**: "DECISIÓN PENDIENTE: no se cambia todavía la medida del script principal. Se discute con el director (Andrés Ham) con estos resultados" |
| Evidencia disponible entonces | Kaitz 2022 M=0,72; Kaitz 2019 M=0,05; Kaitz promedio M=0,76 (gana por margen mínimo, 0,76 vs 0,72); Kaitz promedio rechaza años previos en empleo, Kaitz 2022 no |
| Evidencia que NO existía todavía | Cualquier cosa relacionada con Bite/Exposure/golpe_* y celda limpia — ese marco no existe todavía en ningún commit |
| Alternativas consideradas | Las tres versiones de Kaitz, con su M de quiebre comparado explícitamente |
| Dónde quedó registrada | `NOTA_DECISIONES.md`, sección "6. Elección de la medida de exposición (V11)" |

## 11. Especificación principal del event study (estándar, ref=2022)

| Campo | Contenido |
|---|---|
| Fecha y commit | `9a0e697`, 2026-09-17 01:21 — **10 minutos después** de la entrada 9, y 1 minuto antes de la entrada 10 |
| Decisión | Escribir el script principal con el event study de tres lecturas (A, B, C), Bite2022 fijo, 2022 como año de referencia |
| Evidencia disponible entonces | La misma que en la entrada 9 — este script se escribe en paralelo a la discusión de la regla de decisión, no después de resolverla |
| Evidencia que NO existía todavía | El concepto de "celda limpia" no existe en ningún commit hasta el 24-sep (entrada 13) — **la especificación estándar no se eligió por sobre la limpia; se escribió antes de que la limpia existiera como alternativa** |
| Alternativas consideradas | Ninguna registrada — no había alternativa "limpia" todavía |
| Dónde quedó registrada | `3. SCRIPTS/01_descriptivos_y_contexto.R` (línea 415, tabla del salario mínimo, mismo commit); especificación en la sección del event study del mismo archivo |

## 12. Detección y explicación de la tendencia previa

| Campo | Contenido |
|---|---|
| Fecha y commit | `10fe264`, 2026-09-17 01:22 (mismo commit que la entrada 10) |
| Decisión | Ninguna decisión de diseño — es un hallazgo que se documenta y se explica |
| Evidencia disponible entonces | "C sale negativo porque el salario relativo de las firmas con Kaitz 2022 alto cae de forma sostenida entre 2015 y 2022 (prueba de años previos p < 0,001)" |
| Evidencia que NO existía todavía | La generalización de este mismo patrón a *todos* los años de la serie (placebo rodante, `Diagnostico_reversion/T01`, sin fecha propia en git — ver `AUDITORIA_CADENA.md` sección 0) y la prueba directa de esta auditoría (sección 2 de `AUDITORIA_CADENA.md`) |
| Alternativas consideradas | No registradas — se documenta la explicación ("esperable por construcción") sin registrar si se consideró tratarla como amenaza a la especificación principal, no solo al período previo |
| Dónde quedó registrada | `NOTA_DECISIONES.md`, sección "3. Primer eslabón: tres lecturas"; retomado en `05_resultados_y_mecanismos.R` (bloque "LUGAR EN LA TESIS", capítulo 6) en esta sesión, semanas después |

## 13. Regla de decisión de medida — versión 2 (celda limpia, cinco medidas)

| Campo | Contenido |
|---|---|
| Fecha y commit | `e1afa75`, 2026-09-24 21:53 — primera aparición de la cadena `celda_limpia` en cualquier archivo `.R` de cualquier rama |
| Decisión | Elegir entre Bite, Exposure, golpe_a, golpe_c y golpe_costo según: (1) positivo y significativo en la especificación estándar; (2) se sostiene en la celda limpia (exposición 2019 contra crecimiento 2022-2023); (3) cobertura y estabilidad entre años base |
| Evidencia disponible entonces | No verificable con precisión — el commit trackea código que ya existía sin rastrear (ver entrada 8) |
| Evidencia que NO existía todavía | N/A |
| Alternativas consideradas | Las cinco medidas están comparadas explícitamente en `04_decision_medida.R` |
| Dónde quedó registrada | `3. SCRIPTS/04_decision_medida.R`, sección 7 ("CÓMO SE TOMA LA DECISIÓN") |
| **Nota crítica** | **Entre la entrada 10 (17-sep 01:22, regla v1 pendiente) y esta entrada (24-sep 21:53, regla v2 ya aplicada) hay 7 días y ~20 horas sin ningún commit relacionado con la decisión de medida, en ninguna rama.** No se encontró commit, mensaje, comentario de código, ni entrada de `NOTA_DECISIONES.md` que documente cuándo o por qué se abandonó el marco de Kaitz 2022/2019/promedio + M de quiebre a favor del marco de cinco medidas + celda limpia. La conversación con el director que la entrada 10 decía sería necesaria ("se discute con el director... con estos resultados") no tiene registro escrito en el repositorio, en ninguna rama |

## 14. La estimación principal sigue usando la especificación estándar, no la celda limpia

| Campo | Contenido |
|---|---|
| Fecha y commit | No hay un commit que tome esta decisión explícitamente — es la ausencia de un cambio, no una acción |
| Decisión | Ninguna decisión nueva registrada: el script principal (entrada 11, escrito 17-sep) nunca se revisó después de que la celda limpia apareciera (entrada 13, 24-sep) |
| Evidencia disponible entonces | N/A |
| Evidencia que NO existía todavía | N/A |
| Alternativas consideradas | Ninguna registrada — no hay evidencia de que se haya considerado, y descartado, aplicar la celda limpia (o un año base distinto de 2022) a la especificación principal |
| Dónde quedó registrada | La única justificación textual encontrada es una descripción, no una decisión pre-comprometida: `NARRATIVA.md` línea 44, "la celda limpia... es la estimación más creíble metodológicamente, aunque no la más citada — es una cota inferior por atenuación, no una medición exacta." Esta nota es de esta sesión (documentación posterior), no un registro contemporáneo a cuando se fijó la especificación principal |

---

## Resumen de eslabones sin registro completo

Por si se necesita ubicarlos rápido, sin repetir el detalle de arriba:

- **Entrada 8** (golpe_c/golpe_a/golpe_costo): sin fecha de construcción verificable en git; sin nota de decisión en ningún `NOTA_DECISIONES.md`.
- **Entrada 13, nota crítica**: 7 días y 20 horas sin registro entre la regla v1 (pendiente) y la regla v2 (ya aplicada). Es el hueco más largo y más importante de esta cronología.
- **Entrada 14**: no es un hueco de fechas sino de decisión — nunca se registró la pregunta de si la especificación principal debía revisarse a la luz de la celda limpia.

---

## Parte 3 — Plan de validación ordenada

Orden pensado para que cada prueba condicione a la siguiente: si un eslabón
temprano no se sostiene, no tiene sentido gastar esfuerzo en los que dependen
de él.

### 1. La premisa del choque

¿El aumento real de 2023 (descontada la inflación) fue atípico frente a los
demás años de la serie?

- **Ya existe**: la serie nominal completa 2015-2024 (`01_descriptivos_y_contexto.R:415-419`), y el reconocimiento explícito, en el propio código, de que falta el ajuste real (línea 413-414). `NOTA_DECISIONES.md` lo señaló como pendiente el 16 y 17 de septiembre.
- **Falta**: traer la serie de IPC (DANE) o el histórico de salario mínimo real (Banco de la República), recalcular el aumento real de cada año 2015-2024, y confirmar si 2023 sigue siendo el máximo o dejar de serlo una vez descontada la inflación.

### 2. La medida de exposición

¿Qué criterio elige la medida principal, y es independiente del outcome que
va a explicar?

- **Ya existe**: la regla v2 (`04_decision_medida.R`), la prueba directa de contaminación (`AUDITORIA_CADENA.md` sección 2, ya corrida sobre las cinco medidas con outcome 2023→2024), y el placebo rodante de `Diagnostico_reversion/T01` (sin código versionado).
- **Falta**: aplicar el mismo criterio de independencia (outcome que no comparte año base) a la propia regla de decisión, no solo a la especificación estándar cross-seccional; y decidir formalmente si la regla v2 reemplaza a la v1 o si ambas deben reconciliarse (entrada 13 de la cronología).

### 3. La primera etapa

Con un outcome que no comparta año base con la exposición.

- **Ya existe**: corrida ad hoc de esta sesión (`AUDITORIA_CADENA.md` sección 2) para las cinco medidas, muestra común de 4.640 firmas, controles estándar.
- **Falta**: que esa prueba se incorpore como parte formal y versionada del pipeline (hoy es un script sin comitear, ver Parte 1 de esta tarea), y que se repita sobre la muestra propia de cada medida, no solo la común.

### 4. Tendencias paralelas

Event study completo, coeficientes año a año.

- **Ya existe**: los β_s completos en `05_resultados_y_mecanismos.R` y el contraste con `contdid` en `08_tratamiento_continuo.R` (C1-C6).
- **Falta**: extender el criterio de M de quiebre (Honest DiD, ya usado en la entrada 9 de la cronología para elegir la medida) a la especificación **principal**, no solo al paso de selección de medida que quedó abandonado.

### 5. El resultado de empleo

Solo tiene sentido interpretarlo si 1-4 se sostienen.

- **Ya existe**: el resultado nulo acotado (`01_descriptivos_y_contexto.R`, secciones 6-9) y su registro en `NOTA_DECISIONES.md`.
- **Falta**: nada nuevo que calcular — es una relectura del resultado ya calculado, condicionada a lo que arrojen los puntos 1-4.

### 6. Mecanismos

Exploratorios en cualquier caso, según el propio pipeline ya los declara.

- **Ya existe**: `06_mecanismos_por_grupo.R` con la lectura B (salto contra la tendencia propia de cada mecanismo).
- **Falta**: la misma prueba del punto 3 (outcome que no comparte año base) aplicada a los mecanismos, no solo al primer eslabón — nunca se corrió.
