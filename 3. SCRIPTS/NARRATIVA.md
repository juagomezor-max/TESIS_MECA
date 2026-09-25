# NARRATIVA.md — mapa de la tesis a los scripts

Este documento existe para que nadie tenga que reconstruir de memoria qué
script alimenta qué capítulo, ni por qué circulan cifras distintas del mismo
resultado. Se escribió tras revisar y corregir los comentarios de los 9
scripts de `3. SCRIPTS/` (2026-09), alineándolos con la estructura real de la
tesis.

---

## 1. Capítulos y qué scripts los alimentan

| Cap. | Título | Scripts | Contenido |
|---|---|---|---|
| 1 | Análisis inicial y descriptivos | `02_resultados_poster.R` (secciones 1-4) | La EAM, el panel, el choque de 2023, quién está expuesto. También el descriptivo de `01_exposicion_alternativa.R` (obrero permanente típico ~1,44 SM). |
| 2 | Medición | `01_exposicion_alternativa.R`, `05_primer_eslabon_medidas.R`, `06_decision_medida.R` (parte) | Construcción de las medidas de exposición (Bite, Exposure, golpe_c, golpe_a, golpe_costo) y elección de la principal. |
| 3 | Identificación | `07_reconciliacion.R` | Estrategia, supuestos, y por qué circulan cifras distintas del primer eslabón. |
| 4 | Validación de la identificación (pruebas que el diseño PASA) | `02_resultados_poster.R` (sección 10), `08_tratamiento_continuo.R` (C1-C6) | Tendencias previas, formas funcionales, event studies, contraste con `contdid`. |
| 5 | Resultados | `02_resultados_poster.R` (secciones 6-9), `03_resultados_y_mecanismos.R`, `04_mecanismos_por_grupo.R` | Primer eslabón, segundo eslabón, mecanismos, heterogeneidad por tamaño. |
| 6 | Validaciones adicionales y amenazas (lo que NO cierra) | `06_decision_medida.R` (parte), `08_tratamiento_continuo.R` (C7-C8), `03_resultados_y_mecanismos.R` (parte) | Sensibilidad al año base, reversión a la media, tendencias previas no resueltas, placebo no informativo. Ver la lista completa en la sección 3 de este documento. |

**Regla aplicada sin excepciones**: si una prueba respalda el diseño, va al
capítulo 4. Si lo amenaza, contradice o limita, va al capítulo 6. No se movió
nada del 6 al 4 por conveniencia narrativa -- donde un script mezclaba las
dos cosas, se marcó sección por sección (`08_tratamiento_continuo.R`
es el caso más claro: C1-C6 al capítulo 4, C7-C8 al capítulo 6).

**`99_script_base_historico.R`**: histórico, superado por `02_resultados_poster.R`, no
se corre.

---

## 2. Glosario de cifras

La razón de ser de este glosario: **cinco números distintos han circulado
como "el primer eslabón"**, y no son intercambiables porque miden objetos
distintos.

| Cifra | Origen | Qué mide exactamente |
|---|---|---|
| **4,03%** | `02_resultados_poster.R` y `03_resultados_y_mecanismos.R`, coeficiente de 2023 del event study en panel (lectura A) | Cuánto subió el costo laboral por trabajador en 2023 frente a 2022, en una firma una DE más expuesta. **Cifra principal de la tesis.** |
| **4,80%** | Mismo event study, lectura B (`03_resultados_y_mecanismos.R`, `contraste()` con pesos `{2023=1, 2016=1/3, 2019=-1/3}`) | El mismo salto de 2023, ajustado por la pendiente anual previa (2016-2019). Como la tendencia previa es descendente, B sale MAYOR que A, no menor. **Pendiente sin resolver** (ver sección 4): el póster describe esta misma cifra como "el cambio típico de 2016-2019", que es una descripción distinta y potencialmente contradictoria -- si fueran la misma cosa, el diferencial del choque sería menor que el de un año normal. |
| **2,98%** | `05_primer_eslabon_medidas.R`, sección 1 | Tasa de crecimiento del costo laboral 2022-2023 en **corte transversal** (una fila por firma, sin efectos fijos de panel). Ver `07_reconciliacion.R` para la descomposición completa de por qué difiere de 4,03%. |
| **1,15%** (p=0,008) | `06_decision_medida.R`, sección 3, tabla T01_celda_limpia | La "celda limpia": exposición medida en **2019** contra crecimiento del costo laboral **2022-2023**. Rompe el traslape aritmético entre el denominador de la exposición y la base del outcome. Es la estimación más creíble metodológicamente, aunque no la más citada -- es una **cota inferior** por atenuación, no una medición exacta. |
| **-1,95%** | `06_decision_medida.R`, sección 4, matriz de combinaciones | Coeficiente de 2023 con exposición medida en 2022 y el outcome que arrastra año base 2019. Exposición **post-tratamiento** respecto del aumento del mínimo de 2022 -- no es interpretable, se reporta solo como diagnóstico de por qué esa combinación no sirve. |

**Cómo se relacionan**: 4,03% y 4,80% vienen del mismo modelo en panel
(distintas lecturas del mismo event study); 2,98% viene de una especificación
distinta (corte transversal); 1,15% y -1,95% vienen de mover el año base de
la exposición, no el modelo. Ningún par de estas cifras es "el mismo número
mal calculado" -- cada una responde una pregunta ligeramente distinta.

**Otras cifras clave** (no del primer eslabón, pero recurrentes):
- Bite gana la celda limpia por ser la única medida que pasa los 3 criterios
  de decisión (`06_decision_medida.R`) -- ver la regla en `05_primer_eslabon_medidas.R` y `06_decision_medida.R`.
- Compresión salarial: 0,22 DE, cuatro veces cualquier otro canal de ajuste,
  estable en los 3 grupos de tamaño (0,218 pequeñas / 0,212 medianas / 0,179
  grandes) -- `04_mecanismos_por_grupo.R`.
- 5.742 firmas con `golpe_c` definido frente a 5.099 con Bite (643
  recuperadas) -- `01_exposicion_alternativa.R`, tabla T06.

---

## 3. Capítulo 6 — lista de amenazas

Una línea por amenaza, con el script donde está la evidencia:

1. **Tendencia previa descendente en el costo laboral, esperable por
   construcción.** Kaitz se mide con el salario de 2022, así que las firmas
   de exposición alta son por definición las que llegaron a 2022 con el
   costo más bajo. — `03_resultados_y_mecanismos.R` (sección 3),
   `08_tratamiento_continuo.R` (C7.1).
2. **El coeficiente de 2023 pasa de +4,03% a -1,95% al mover el año base de
   exposición a 2019** (mientras el outcome se queda en 2022-2023 con
   arrastre a 2019) — exposición post-tratamiento, no interpretable.
   — `06_decision_medida.R` (sección 4).
3. **La mayoría de los mecanismos no pasa tendencias paralelas** bajo la
   lectura A (coeficiente de 2023 contra 2022 sin ajustar por pendiente
   previa) — `03_resultados_y_mecanismos.R` (sección 5, revisar
   `p_previos`). Parcialmente resuelto con la lectura B en
   `04_mecanismos_por_grupo.R`, pero declarado como asociación
   comparativa, no efecto causal limpio.
4. **El placebo 2018-2019 del primer eslabón no es informativo por
   construcción**: da coeficiente negativo y significativo en las 5 medidas,
   incluida Exposure (que no usa salarios). La razón es aritmética
   (exposición alta → costo bajo en 2022 → por persistencia, costo bajo en
   2019 → crecimiento 2018-2019 bajo), no diseño. — `05_primer_eslabon_medidas.R`
   (sección 7).
5. **Bite es la medida menos estable entre años base** (Pearson 0,57,
   `06_decision_medida.R`, tabla T03) **y la de menor cobertura en la celda
   limpia** (n menor que otras medidas en la muestra de 2019) — declarar
   junto con la elección de Bite como medida principal, no ocultarlo porque
   gane.
6. **La EAM no observa precios, rotación, horas ni estándares de
   desempeño**, que Hirsch, Kaufman y Zelenska (2015) documentan como
   canales relevantes de ajuste. La ausencia de evidencia sobre un canal no
   observable no es evidencia de su ausencia. — mencionado en
   `03_resultados_y_mecanismos.R` (sección 5) y en
   `04_mecanismos_por_grupo.R`.
7. **El panel arranca en 2012 con sector confiable** porque `CIIU4` no
   existe antes de 2012 y no hay tabla de correspondencia verificable desde
   `CIIU3` (0 años de solapamiento, confirmado en el cierre del panel
   ampliado). No es material de ningún script de esta
   revisión directamente, pero condiciona la ventana de todos los que usan
   el panel ampliado.
8. **20,5% de firmas-año sin obreros permanentes queda fuera del tratamiento
   con Bite** (Kaitz de obreros no tiene denominador), y son precisamente
   las firmas que ya usan la temporalidad como margen de ajuste -- selección
   sobre el mecanismo mismo que la tesis estudia. `golpe_c` y `golpe_a` las
   recuperan si tienen alguna otra categoría ocupacional. —
   `01_exposicion_alternativa.R`.

---

## 4. Preguntas abiertas — no se resuelven antes de la entrega

Encontradas durante la revisión de comentarios, marcadas explícitamente en el
script correspondiente con una nota, **sin resolverlas por cuenta propia**:

1. **¿Qué mide exactamente "4,80%"?** `07_reconciliacion.R` (punto
   6 de la sección 6) trae, sin resolver desde antes de esta revisión, que el
   póster describe esta cifra como "el cambio típico de 2016-2019", mientras
   que en `02_resultados_poster.R` y `03_resultados_y_mecanismos.R` la Lectura B se calcula como "el
   salto de 2023 ajustado por la pendiente previa" (`contraste()`, resta el
   promedio 2016-2019 al coeficiente de 2023). Son descripciones distintas.
   `02_resultados_poster.R` deja una nota que sugiere que son la misma cosa descrita de
   forma imprecisa en el póster, pero no se confirmó corriendo el número.
   **Antes de escribir la tesis, correr ambos cálculos y verificar si
   coinciden.**
2. **¿La relación por tramos de exposición se aplana o es monotónica
   creciente?** `07_reconciliacion.R` (sección 6, punto 5) dice
   que se aplana en el quintil más expuesto; `06_decision_medida.R`
   (sección 6) dice, con los mismos controles, que es monotónica
   creciente. No se sabe cuál quedó desactualizada. **Verificar contra la
   tabla `T04_efecto_por_tramos` de `06_decision_medida.R` antes de escribir
   cualquiera de las dos afirmaciones.**
3. **El criterio de decisión de medida que se eliminó** (el placebo
   2018-2019 como criterio 3 en `05_primer_eslabon_medidas.R`) -- confirmado que ya no se
   usa en la decisión, pero no se decidió formalmente si hace falta un
   tercer criterio de reemplazo o si dos criterios (celda limpia +
   consistencia con la especificación estándar) son suficientes por sí
   solos.
4. **C8 de `08_tratamiento_continuo.R`** (tres formas de construir
   la exposición, probadas contra el mismo placebo que detectó el problema
   en C7) no tiene todavía un veredicto: depende de cuál medida (A: salario
   propio, B: promediada, C: por celda de firmas parecidas) pase la
   comparación real-vs-placebo de C8.3. Mientras no se decida, el script
   completo queda documentado en el capítulo 6; si una medida concreta
   resuelve el problema, ese hallazgo puntual se movería al capítulo 4.
5. **La categoría ocupacional de `R1CSAP`/`R2CSAP`/`R3CSAP`** (apoyo de
   sostenimiento de aprendices) sigue sin determinarse -- las dos pruebas
   ya intentadas (identidad contra R4CSAP, correlación con conteos de
   aprendices por categoría) no la resuelven. Documentado en el diccionario
   del panel ampliado, no en estos 9 scripts.
