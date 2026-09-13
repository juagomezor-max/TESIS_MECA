# Resultados (PRIMER BORRADOR — 2026-09-13)

> **Este es un primer borrador de la sección de Resultados, generado a partir de un inventario verificado del repositorio (no de un resumen previo del autor).** Reporta los números con su evidencia de validez al lado. No concluye sobre el efecto del salario mínimo — esa interpretación queda para el autor y su asesora. Ver la sección final "Qué no está incluido en este borrador".

## Nota sobre el alcance de esta verificación

El diseño metodológico se tomó como dado, tal como lo describió el autor (DiD con tratamiento continuo, choque = aumento real del salario mínimo de 2023 definido ex post, dos medidas de exposición en paralelo sin jerarquía, panel formal con controles sector×año + tamaño×año + departamento×año y `cluster=~NORDEMP`). **No se encontraron en el repositorio los archivos `CONTEXTO_DE_LA_TESIS` ni una "sección 4.3 de Estrategia Empírica"** citados por el autor — se buscó en todo el árbol de archivos (`.md`, `.docx`, `.pdf`) y no aparecen. Las decisiones de diseño que el autor describió se respetan tal como las planteó, pero no pudieron verificarse contra un documento fuente dentro de este repositorio. Tampoco se encontró ni se recalculó en este repositorio la cifra de que el salario mínimo real cayó -3,05pp en 2022 — se usa como premisa dada por el autor para motivar el placebo de la sección 3.

## 1. Inventario verificado (antes de correr nada nuevo)

| # | Pregunta | Respuesta verificada |
|---|---|---|
| 1 | Estimaciones de la especificación principal ya corridas | `estimar_did_principal_empleo.R` (commit `64b04ea`), únicamente con `Exposure2022_obreros`, sobre `panel_establecimiento_formal.rds`. Salidas en `4. RESULTADOS/Estimacion_DiD/`. |
| 2 | ¿Existe una estimación de la especificación principal con `Bite2022_obreros`? | **No existía** al momento de esta verificación. El propio script de Exposure lo documentaba explícitamente: "Bite2022_obreros queda DELIBERADAMENTE fuera de esta ronda". Se corrió en esta sesión (ver sección 2 de este documento). |
| 3 | ¿Existe un placebo/falsificación con 2022 como año de comparación? | **No existía.** Se construyó en esta sesión (ver sección 3). |
| 4 | Estado de las especificaciones a nivel establecimiento (A y B, `Multi_f`, δ_f(e),t) | **Solo se validó el panel y las tendencias paralelas** (`validar_pretendencias_panel_formal.R` / `_exposure_est.R`) y se construyeron descriptivos de la estructura multiplanta (`Multi_f`, cohorte balanceada). **Ningún script del repositorio corre una regresión (`feols()`) para una especificación A o B a nivel establecimiento** — se verificó explícitamente que ninguno de los scripts relacionados con `Multi_f`/especificación B contiene una llamada a `feols()`. No llegaron a estimarse. |

## 2. Lo que se corrió en esta sesión para completar el inventario

- **`3. SCRIPTS/estimacion/estimar_did_principal_empleo_bite.R`** (commit `3b55e37`): espejo exacto de la estimación principal, con `Bite2022_obreros` en vez de `Exposure2022_obreros`. Mismo panel, ventana, controles y cluster.
- **`3. SCRIPTS/validaciones/validar_placebo_2022_empleo.R`** (commit `dd11d9f`): placebo con 2022 como año de comparación (2021→2022 en vez de 2022→2023), para las 4 variables de resultado, con ambas medidas de exposición.

## 3. Especificación principal: Exposure2022_obreros

### 3.1 Manipulation check (primer eslabón) — cítese antes de leer los coeficientes de abajo

Script: `validaciones/validar_primer_eslabon_costo_laboral.R` (commit `576133a`). Salida: `4. RESULTADOS/Validaciones/validacion_primer_eslabon_contraste_salto_atipico.csv`.

| Especificación | Contraste (salto 2022→2023 vs. incremento típico 2016-2019) | t | p |
|---|---|---|---|
| Sin controles | 0.00728 | 9.08 | 1.5e-19 (atípico) |
| **Con controles (recomendada)** | -0.00106 | -0.29 | **0.771 (NO atípico)** |

`Exposure2022_obreros` **no predice de forma robusta** el choque real de costo laboral de 2023 bajo la especificación con controles. Diagnóstico de sobre-control relacionado (`validaciones/diagnosticar_sobrecontrol_exposure_fwl.R`, salida `diagnostico_sobrecontrol_r2.csv`): sector+tamaño+departamento explican R²=0.218 de la varianza de `Exposure2022_obreros` (1-R²=0.782 sobrevive).

### 3.2 Estimación DiD, empleo (modelo estático, con controles)

> Nota de terminología: "modelo estático" aquí es una forma funcional (post_2023 como dummy único), **no** la "Especificación A" de la sección 4.5 de la tesis (muestra completa a nivel establecimiento) — el script corre sobre la muestra completa, sin restricción a multiplanta; el nombre "Especificación A/B" que usaba originalmente el script (hasta el commit `64b04ea`) era una colisión de nombres accidental, ya corregida en el código.

Script: `estimacion/estimar_did_principal_empleo.R`. Salida: `did_estatico_empleo_coeficientes.csv` (fila "Con sector*anio + tamano*anio + departamento*anio").

| Outcome | Coeficiente | Error estándar | p | N (obs / clusters) |
|---|---|---|---|---|
| Empleo total | 0.5477 | 0.3520 | 0.1198 | 59,491 / 6,180 |
| Empleo permanente | 0.3991 | 0.2839 | 0.1598 | 59,491 / 6,180 |
| Empleo temporal ⚠️ | 0.1488 | 0.2306 | 0.5187 | 59,491 / 6,180 |
| Participación permanente (%) ⚠️ | -0.1568 | 0.1495 | 0.2946 | 59,411 / 6,180 |

**Empleo total y empleo permanente: nulos estables.** No significativos bajo el modelo estático, y su condición de no-significativo no cambió bajo ninguna de las pruebas de robustez corridas (tendencia lineal, tendencia cuadrática, leave-one-year-out — `auditoria_post_vs_tendencia_lineal_exposure.csv`). Dado el resultado de 3.1, estos 2 no-rechazos son **ambiguos entre "no hay efecto de `Exposure2022_obreros`" y "la medida no tiene la potencia estadística para detectar el mecanismo que el diseño asume"** — no se puede distinguir entre ambas explicaciones con esta evidencia sola, porque el propio primer eslabón de `Exposure2022_obreros` no queda demostrado bajo esta misma especificación. No se usa lenguaje causal para ninguno de estos 2 resultados.

> ⚠️ **Empleo temporal y participación permanente: CERRADO como NO ROBUSTO (2026-09-13). No interpretar como efecto causal.**
>
> Los coeficientes de la tabla de arriba para estas 2 dimensiones (0.1488, p=0.5187 y -0.1568, p=0.2946) no se sostienen como evidencia utilizable en ninguna dirección — no como "efecto nulo confiable" ni como el resultado alternativo que aparece al agregar tendencia. Evidencia convergente por tres vías independientes:
>
> - **(a) Tabla completa de 8 celdas (original vs. tendencia lineal)**, `auditoria_post_vs_tendencia_lineal_exposure.csv`: bajo tendencia lineal, ambos coeficientes cambian de signo y se vuelven significativos al 5% (empleo temporal: 0.149→-0.536, p 0.519→0.0213; participación permanente: -0.157→0.335, p 0.295→0.0161) — pero un cambio de signo inducido por agregar un solo control es en sí mismo una señal de inestabilidad, no de haber encontrado el resultado correcto.
> - **(b) Tendencia cuadrática, mismo signo/significancia que la lineal** (`robustez_tendencia_cuadratica_exposure.csv`: empleo temporal -0.788, p=0.0099; participación permanente +0.572, p=0.00088) — descarta que el cambio de signo se deba a una mala especificación polinómica del control (lineal vs. cuadrática coinciden entre sí, pero ninguna coincide con el modelo sin tendencia).
> - **(c) Leave-one-year-out** (`robustez_leave_one_year_out_exposure.csv`): el resultado con tendencia **pierde significancia al excluir 2022** en ambos outcomes (empleo temporal p=0.124, participación permanente p=0.287), y empleo temporal también la pierde al excluir 2021 — el efecto depende de incluir años específicos inmediatamente anteriores al tratamiento, no es estable a la muestra.
>
> **Esto se suma a que `Exposure2022_obreros` ya tiene evidencia independiente de ser una medida débil** (manipulation check fallido con controles, sección 3.1, p=0.771).
>
> **Aclaración explícita — esto NO contradice el placebo de 2022** (sección 5: ningún outcome de empleo mostró un salto anómalo 2021→2022 con controles, para ninguna medida). Son dos preguntas distintas: el placebo pregunta si hay una *ruptura brusca* en 2022; el leave-one-year-out pregunta si 2022 es un *punto influyente* dentro de una tendencia ya estimada. 2022 fue identificado en la sección 4.2 como un año atípico por razones independientes (pérdida real del salario mínimo, transición presidencial, reforma tributaria) — es plausible que sea influyente en una regresión de tendencia sin producir, por sí solo, un salto discreto detectable por el placebo.
>
> No se van a correr más pruebas de robustez sobre estos 2 coeficientes (no se agrega control de pandemia, no se sigue iterando forma funcional) — el caso queda cerrado como no robusto con la evidencia ya reunida.

## 4. Especificación principal: Bite2022_obreros

### 4.1 Manipulation check (primer eslabón) — cítese antes de leer los coeficientes de abajo

Script: `validaciones/validar_primer_eslabon_costo_laboral_bite.R` (commit `0ce0519`). Salida: `validacion_primer_eslabon_contraste_salto_atipico_bite.csv`.

| Especificación | Contraste | t | p |
|---|---|---|---|
| Sin controles | 0.0232 | 9.97 | 3.4e-23 (atípico) |
| **Con controles (recomendada)** | 0.0508 | 5.83 | **5.8e-9 (atípico)** |

`Bite2022_obreros` **sí predice** el choque real de costo laboral de 2023, tanto sin controles como con controles. A diferencia de `Exposure2022_obreros`, un resultado nulo de `Bite2022_obreros` en la sección 4.2 **no** tiene la misma ambigüedad de potencia — su primer eslabón pasa bajo la especificación recomendada.

### 4.2 Estimación DiD, empleo (modelo estático, con controles)

> ✅ **AUDITORÍA CONCLUIDA (2026-09-13) — los 2 coeficientes en negrita de abajo NO se sostienen como resultado confirmado.** El usuario encontró que estos 2 coeficientes significativos no tienen un quiebre visible en el event study del mismo outcome (ver nota al final de esta sección) y pidió investigar si el coeficiente "post" del modelo estático solo capturaba la extrapolación de una tendencia lineal pre-existente. Resultado de esa auditoría (`validaciones/validar_post_controlando_tendencia_lineal.R`, `auditoria_post_vs_tendencia_lineal_bite.csv`): al agregar un término de tendencia lineal continua (`anio_lineal:bite_1sd`) a la misma especificación, **ambos coeficientes "post" pierden significancia al 5% y cambian de signo** (empleo temporal: -1.72→+1.11, p pasa de 0.0124 a 0.0863; participación permanente: +1.17→-0.844, p pasa de 0.0051 a 0.0523) mientras el término de tendencia es altamente significativo en los dos (p=0.00102 y p=0.000335). A diferencia de Exposure2022_obreros (sección 3.2), aquí no se corrieron pruebas adicionales de forma funcional porque el resultado con tendencia es consistente (el efecto desaparece de forma limpia, sin necesidad de más chequeos) — no está pendiente, es la lectura final.

Script: `estimacion/estimar_did_principal_empleo_bite.R`. Salida: `did_estatico_empleo_bite_coeficientes.csv` (fila "Con sector*anio + tamano*anio + departamento*anio"). Escala: `bite_1sd` = Bite2022_obreros / desviación estándar muestral (sd=0.3056) — Bite no tiene una escala natural de "10pp"; la elección de escala no afecta significancia ni p-valor, solo la magnitud del coeficiente.

| Outcome | Coeficiente (+1 SD) | Error estándar | p | N (obs / clusters) |
|---|---|---|---|---|
| Empleo total | -1.1616 | 0.9949 | 0.2430 | 49,910 / 5,099 |
| Empleo permanente | 0.6112 | 0.7940 | 0.4414 | 49,910 / 5,099 |
| **Empleo temporal ⚠️** | **-1.7241** | 0.6893 | **0.0124** | 49,910 / 5,099 |
| **Participación permanente (%) ⚠️** | **1.1734** | 0.4189 | **0.0051** | 49,851 / 5,099 |

~~Empleo temporal y participación permanente son estadísticamente significativos al 5%; empleo total y empleo permanente no lo son. Dado que el primer eslabón de `Bite2022_obreros` sí pasa (sección 4.1), los 2 coeficientes significativos son consistentes con un efecto detectable del choque de costo laboral sobre esas 2 dimensiones.~~ **Retirado (2026-09-13): esta lectura no se sostiene — el efecto desaparece al controlar por tendencia lineal pre-existente (auditoría de arriba).**

**Empleo total y empleo permanente: nulos estables.** No significativos bajo el modelo estático (p=0.2430 y p=0.4414), y su condición de no-significativo no cambió bajo la prueba de tendencia lineal (`auditoria_post_vs_tendencia_lineal_bite.csv`). A diferencia de `Exposure2022_obreros`, estos 2 no tienen la ambigüedad de potencia de la sección 3.2 — el primer eslabón de `Bite2022_obreros` sí pasa (sección 4.1).

**Nota sin interpretar sobre el event study completo (ref=2022, con controles) — el hallazgo que motivó la auditoría de arriba:** ningún coeficiente 2023 o 2024 de `Bite2022_obreros` es significativo al 5% en esa forma funcional (p entre 0.11 y 0.93; `event_study_completo_bite_coeficientes.csv`), y las series no muestran un quiebre visible en 2023 — se ven como continuación de la tendencia pre-existente. Esto es lo que llevó a sospechar que el modelo estático de arriba estaba capturando esa misma tendencia, no un quiebre causado por 2023 (confirmado por la auditoría).

> ⚠️ **Verificación adicional (2026-09-14): el análisis por grupos de exposición (terciles y quintiles) para esta misma celda tampoco es robusto.** Un análisis exploratorio posterior (`estimacion/estimar_did_por_grupos_exposicion.R`) había mostrado, para Bite2022_obreros × participación_permanente, un patrón por grupo monótono y significativo (Q2–Q5 y T2–T3, todos positivos, p<0.02) — pero ese análisis no incluía control de tendencia. Al agregarlo (`validaciones/validar_grupos_bite_participacion_con_tendencia.R`), los coeficientes **cambian de signo** (a negativo) y siguen significativos en 5 de 6 celdas (p entre 0.004 y 0.00004; solo un grupo pierde significancia). Esto NO es un hallazgo distinto ni más confiable que la versión continua de arriba — es la misma inestabilidad, vista con otra forma funcional. No usar el patrón por grupo de esta celda como evidencia en ninguna dirección.

## 5. Placebo 2022 (salario mínimo real cayó, no subió)

Script: `validaciones/validar_placebo_2022_empleo.R` (commit `dd11d9f`). Contrasta el incremento 2021→2022 contra el incremento típico 2016-2019, misma lógica que las secciones 3.1/4.1, para las 4 variables de resultado y ambas medidas. Salidas: `validacion_placebo_2022_contraste_salto_atipico_exposure.csv` / `_bite.csv`.

**Con controles (la especificación recomendada) — ninguno de los 8 casos (4 outcomes × 2 medidas) muestra un salto atípico al 5%:**

| Outcome | Exposure2022_obreros (p) | Bite2022_obreros (p) |
|---|---|---|
| Empleo total | 0.829 | 0.923 |
| Empleo permanente | 0.744 | 0.757 |
| Empleo temporal | 0.628 | 0.880 |
| Participación permanente | 0.281 | 0.258 |

Sin controles, 3 de 4 outcomes sí muestran un salto atípico en 2022 para ambas medidas (empleo_total, empleo_permanente, empleo_temporal; no participación_permanente) — detalle completo en los CSV citados.

La ausencia de señal en 2022 bajo la especificación con controles, en un año en que el salario mínimo real cayó (premisa del autor, no verificada en este repositorio), es un hecho reportado sin interpretar más allá de los números: el lector puede leerlo como evidencia a favor de que el patrón detectado para 2023 responde específicamente a ese choque, o sacar su propia lectura.

## 6. Qué NO está incluido en este borrador

- **Especificaciones a nivel establecimiento (A y B), con `Multi_f` y δ_f(e),t**: no llegaron a estimarse (sección 1, punto 4). Solo existe la construcción del panel, los descriptivos de estructura multiplanta y la validación de tendencias paralelas a nivel establecimiento.
- **Honest DiD** (Rambachan y Roth, o equivalente) para acotar sesgo por violaciones de tendencias paralelas: no se ha corrido en este repositorio.
- **Formas funcionales por bins/cuantiles** de exposición para el DiD principal (más allá de la continua ya reportada): no se han estimado para la especificación principal — sí existen quintiles en otros ejercicios (matriz de comparación funcional, atrición diferencial), pero no como especificación principal alternativa.
- **Robustez adicional de la Especificación B (event study) para Bite**, dado que diverge de la Especificación A (sección 4.2): no se investigó la causa de esa discrepancia.
- **Cualquier interpretación causal, comparación entre medidas, o conclusión sobre el efecto del salario mínimo**: deliberadamente fuera de este borrador, por instrucción explícita del autor.
