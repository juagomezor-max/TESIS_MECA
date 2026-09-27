# Auditoría de la cadena causal desde el origen

Fecha: 2026-09-25. Rama: `feature/medida-exposicion-alternativa`. Diagnóstico
puro — **ningún script del pipeline se modificó ni se volvió a correr** para
producir este documento. Los únicos cálculos nuevos están en un script ad hoc
fuera de `3. SCRIPTS/` (sección 3), escrito exclusivamente para esta auditoría.

---

## 0. Qué se pudo auditar y qué no — dos huecos de partida

Antes de entrar en los eslabones, dos cosas que el prompt de esta tarea da por
existentes y que **no están en esta rama**:

1. **`09_diagnostico_reversion.R` no existe como script**, ni en `3. SCRIPTS/`,
   ni en ninguna rama, ni en el historial de git (`git log --all` no encuentra
   ningún commit que lo haya agregado). Lo que sí existe es su *salida*:
   `4. RESULTADOS/descartado/Diagnostico_reversion/` (6 tablas + 4 figuras), **no
   rastreada por git** (`git status` la marca `??`), con fecha de modificación
   2026-09-24 23:33 — la misma tarde de la reorganización de scripts de esta
   sesión. No pude leer el código que la generó; lo que reporto de esas tablas
   en la sección 3 es lectura directa de los CSV, no verificación contra
   fórmulas. Si el script existe en otro directorio o en la máquina del autor
   fuera de este repositorio, no se pudo confirmar desde aquí.
2. **`NOTA_DECISIONES.md` no existe en esta rama**, aunque el código la cita
   seis veces (`02_medidas_exposicion.R`, `03_primer_eslabon_medidas.R`) como
   la fuente de la regla de decisión. Sí existe, idéntica, en
   `feature/continuo-y-descriptivos` y `refactor/estructura-pipeline`
   (último commit `fe75b9a`, 2026-09-17). Nunca se trajo a esta rama. Su
   contenido completo se usa en la sección 4 — leído directamente de esas
   ramas con `git show`, no de memoria ni de inferencia.

Estos dos huecos son en sí mismos un hallazgo (sección 4 y 6): la
documentación de decisiones no viajó con el código cuando el trabajo se
movió de rama, y al menos un script que produjo evidencia relevante para esta
auditoría quedó fuera de control de versiones por completo.

---

## 1. Tabla de eslabones

| # | Eslabón | Afirmación | Evidencia | Verificado | Asumido |
|---|---|---|---|---|---|
| 1 | Construcción del panel | El panel de firma es correcto y consistente | `NOTA_DECISIONES.md` (sesión 17-sep): identidad del empleo 62.816/62.816; salario promedio, correlación 1,0 contra el panel de establecimientos | Sí, ambas cifras están citadas en la nota con el método de cálculo | Que el panel arranca en 2012 no afecta ningún resultado — **verificado aquí**: ningún script de `3. SCRIPTS/` usa datos anteriores a 2015 (`grep` sin resultados); el arranque en 2012 solo acorta la serie sin usar, no la ventana de estimación (2015-2024 sin 2020) |
| 2 | Elección del choque (2023 fue atípico) | El aumento de 2023 (+16% nominal) es el mayor de la serie y justifica tratarlo como choque | Tabla `salario_minimo` en `01_descriptivos_y_contexto.R:415-419` (valores tomados a mano de los decretos, 2015-2024); +16% es efectivamente el máximo nominal de la serie | Sí, el +16% nominal está verificado contra la tabla hardcodeada, y es el máximo | **Nunca se verificó en términos reales.** El propio código lo marca: `01_descriptivos_y_contexto.R:413-414`, comentario "Pendiente para validaciones: agregar la inflación del DANE para ver el aumento real." `NOTA_DECISIONES.md` (17-sep, pendiente #5) ya listaba esto como tarea sin hacer. Ocho días después (esta auditoría, 25-sep) sigue sin hacerse. **La premisa central del diseño (que 2023 es un choque distinguible de la inflación general) no tiene respaldo cuantitativo.** |
| 3 | Construcción de la medida (Bite) | Bite mide exposición real al salario mínimo, no un artefacto aritmético | Fórmula verificada en el código fuente: `0. ANALISIS INICIAL IA/3. SCRIPTS/pipeline/02_construir_exposicion.R:112`: `Bite2022_obreros = SM_2023_mensual×12/1000 ÷ salario_promedio_obrero_2022` | **Ver sección 3 — es el eslabón de la hipótesis principal, con prueba empírica nueva** | Que el "primer eslabón" (Bite predice el crecimiento del costo 2022→2023) refleja un mecanismo económico y no la aritmética de compartir el año 2022 en el denominador de la medida y en la base del outcome |
| 4 | Regla de decisión de la medida | La medida principal (Bite) se eligió por una regla comiteada de antemano, según la evidencia | `NOTA_DECISIONES.md`, texto completo citado en sección 4 | Existe una regla comiteada — pero **no es la regla que el pipeline actual aplica** | Que la regla de `04_decision_medida.R` (celda limpia + 3 criterios) es continuación de la regla de `NOTA_DECISIONES.md` (M de quiebre, Kaitz 2022/2019/promedio). **No lo es**: candidatos, criterio y resultado son distintos, y no hay nota que documente el cambio |
| 5 | Primera etapa | Bite predice el crecimiento del costo laboral 2022→2023 mejor que Exposure | Reproducibilidad del 25-sep: 5 medidas, 21/21 cifras verificadas (ver auditoría anterior) | Los coeficientes están verificados como reproducibles | Que ese poder predictivo es evidencia de un canal económico. **Contradicho parcialmente** — ver sección 3 |
| 6 | Especificación del event study | Los controles fijados en 2022 son correctos; la tendencia previa es "esperable por construcción" | `NOTA_DECISIONES.md` (4.5): con controles 2022, Exposure×empleo pasa de p=0,146 a p=0,012 (evita bad control) | El argumento del bad control está verificado y es sólido | Que "esperable por construcción" es solo una explicación de la tendencia previa y no también una admisión sobre el período posterior — **ver sección 4, análisis** |
| 7 | Resultados y mecanismos | Los resultados de mecanismos (compresión salarial, heterogeneidad) son válidos | `06_mecanismos_por_grupo.R`: compresión salarial 0,22 DE, dominante | El cálculo es reproducible (verificado 25-sep) | Que la "lectura B" (salto contra la tendencia propia) inmuniza completamente contra el mismo problema del eslabón 3. **No se probó** — ver sección 5 |

---

## 2. La hipótesis principal, puesta a prueba

**Mecanismo verificado en el código fuente** (no inferido): `Bite2022_obreros`
tiene el salario promedio del obrero permanente **de 2022** en el
denominador. La especificación "estándar" del primer eslabón
(`03_primer_eslabon_medidas.R`) usa como outcome
`log(costo_2023) − log(costo_2022)`. **El año 2022 aparece en los dos lados**:
en el denominador de la medida y en la base del outcome. Una firma que por
azar reportó un costo bajo en 2022 sale "más expuesta" (Bite alto) Y además
su crecimiento hacia 2023 sale alto, sin que haya pasado nada económico.

### La prueba directa (pedida en el encargo)

Corrí la primera etapa de las cinco medidas con dos outcomes, misma muestra
(4.640 firmas, las que tienen las cinco medidas y ambos outcomes), mismos
controles (`sector_2022 + depto_2022 + tamano_2022`), mismo filtro de
plausibilidad y estandarización que `03_primer_eslabon_medidas.R` —
replicado línea por línea en un script ad hoc, sin tocar el original:

| Medida | Estándar (2022→2023, comparte 2022) | Prueba (2023→2024, no toca 2022) | Caída | ¿Sigue significativo al 5%? |
|---|---|---|---|---|
| golpe_costo | 8,66%*** | 3,18%*** | **-63%** | Sí |
| Bite2022_obreros | 2,83%*** | 1,43%*** | **-50%** | Sí |
| golpe_a | 3,92%*** | 2,43%*** | -38% | Sí |
| golpe_c | 3,68%*** | 2,43%*** | -34% | Sí |
| Exposure2022_obreros | -0,28% (p=0,63) | +0,87% (p=0,076) | cambia de signo | No (marginal) |

### Lectura, sin dar la hipótesis por confirmada de más

**La hipótesis se confirma en su forma moderada, no en su forma fuerte.**

- Las cuatro medidas con salario en la construcción pierden entre **34% y
  63%** de su coeficiente al mover el outcome a una ventana que no comparte
  año con el denominador. Eso es exactamente lo que predice la contaminación
  aritmética, y el orden de magnitud coincide con la vulnerabilidad que el
  propio código ya documentaba cualitativamente (`03_primer_eslabon_medidas.R`,
  comentario: "golpe_costo, su denominador ES el costo laboral del outcome" →
  es, en efecto, la medida más contaminada, 63%).
- **Pero ninguna de las cuatro pierde toda su significancia.** Queda un
  coeficiente positivo y significativo al 1% incluso en la ventana limpia. La
  forma fuerte de la hipótesis ("Bite no mide nada económico, solo
  aritmética") **no se sostiene** con esta sola prueba.
- **Hallazgo no anticipado en el encargo**: entre las cuatro medidas con
  salario, **Bite es la segunda más contaminada (-50%), no la menos
  contaminada.** golpe_c (-34%) y golpe_a (-38%) pierden proporcionalmente
  menos. Si el criterio para elegir la medida principal fuera "cuál sobrevive
  mejor a esta prueba", Bite no sería la ganadora.
- Exposure (la única sin salarios) es la más errática: pasa de negativo/nulo
  a positivo marginal. Con ambos p-valores lejos de cualquier umbral cómodo,
  esto se reporta como anomalía sin interpretar, no como hallazgo.

**Advertencia sobre esta misma prueba**: la ventana 2023→2024 no es un
placebo perfecto. Ya no comparte año con el denominador de Bite, pero mide
crecimiento *después* del choque de 2023, en un período que incluye el
aumento del mínimo de 2024. No prueba que lo que sobrevive sea el efecto
causal del mínimo; solo prueba que no es *puramente* el sesgo de división del
año compartido.

### Evidencia complementaria (de `descartado/Diagnostico_reversion/`, script no auditable — ver sección 0)

Tres tablas de esa carpeta, leídas directamente, apuntan en la misma
dirección y la refuerzan:

- **`T01`/`T02` (placebo rodante del costo laboral)**: corre la misma prueba
  de primera etapa para **cada** transición de año 2015-2024, no solo
  2022→2023. El resultado es contundente: el coeficiente del año del choque
  real (2023, aumento nominal +16%) es **2,89%** — el **más bajo o casi el
  más bajo de los ocho años probados** (rango de los años sin choque:
  3,16%-5,67%; promedio 4,37%). Textualmente, la tabla `T02` dice: "¿2023
  está por encima de TODOS los años normales? **0**" (no). Si Bite midiera un
  mecanismo específico del choque de 2023, el año del choque debería tener el
  coeficiente más alto, no de los más bajos.
- **`T05` (colinealidad de una medida "C" — salario de firmas parecidas, no
  de la propia firma)**: una medida que usa el salario de firmas
  *parecidas* en vez del propio (rompe el vínculo aritmético directo) **no
  predice nada**: coeficiente entre -0,28% y -0,42%, nunca significativo
  (p entre 0,61 y 0,99), en las cuatro especificaciones de control probadas.
  Bite, la versión "propia firma", predice significativamente (p<0,0001) en
  las cuatro. Esto es coherente con que lo que sobrevive en Bite es
  específico de la firma (no composición sectorial), pero el placebo rodante
  de arriba dice que esa especificidad no es del choque de 2023.
- Junto con lo anterior, `NOTA_DECISIONES.md` (17-sep) ya había notado, de
  forma independiente y antes de esta auditoría: "Kaitz 2022 alto
  identifica, además de firmas que pagan cerca del mínimo, firmas con un
  2022 atípicamente malo: el salario, las ventas y la brecha salarial tienen
  una 'V' con mínimo en 2022." Esa es la misma familia de problema (reversión
  a la media alrededor de 2022) mostrándose por tercera vía distinta.

**Síntesis de las tres piezas**: la prueba directa (que sí pedía el encargo)
muestra que la contaminación explica una parte sustancial pero no toda la
primera etapa. El placebo rodante (evidencia complementaria, no verificada
por mí en el código) sugiere que incluso lo que sobrevive podría no ser
específico del choque de 2023, sino una propiedad general de cómo el salario
de una firma se mueve año a año. Esta segunda parte de la síntesis **es
inferencia, no una prueba nueva corrida para esta auditoría** — queda como
hipótesis de trabajo, no como hecho verificado con el mismo rigor que la
tabla de arriba.

---

## 3. El punto de falla

**No hay un solo punto de falla; hay dos, y son distintos en severidad.**

### Punto de falla primario: el año 2022 es compartido entre la medida y el outcome en TODA la especificación, no solo en la regla de decisión

`04_decision_medida.R` ya reconoce el sesgo de división explícitamente — sus
propias etiquetas de tabla dicen "Sesgo de división: 2022 en los dos lados" y
construyen la "celda limpia" (exposición 2019, outcome 2022-2023) para
evitarlo. **Ese cuidado no se extendió a la especificación principal.** Los
ocho scripts vivos del pipeline usan `Bite2022_obreros` como regresor, y el
event study central (`05_resultados_y_mecanismos.R:338`,
`i(ANIO_F, Bite2022_obreros_de, ref = '2022')`) fija 2022 como año de
referencia — la misma estructura que genera la contaminación en la prueba de
primera etapa, aplicada ahora a la serie completa de coeficientes β_s, no
solo al coeficiente de 2023. Esto significa que la cifra principal de la
tesis (4,03%, el coeficiente de 2023 contra la referencia 2022) nace de la
misma construcción que ya se sabe contaminada en otro lado del mismo
pipeline.

**Evidencia que lo sostiene**: fórmula de Bite verificada en el código
(sección 1, eslabón 3); prueba directa de la sección 2 (34%-63% del
coeficiente estándar desaparece al romper el año compartido); el hecho de
que el propio equipo ya diseñó una defensa contra esto (celda limpia) para
un uso — la selección de medida — pero no para el otro — la especificación
principal.

### Punto de falla secundario: la regla de decisión documentada no es la regla que corre

`NOTA_DECISIONES.md` (comiteada, 17-sep) define una regla de decisión
completa —candidatos Kaitz 2022/2019/promedio, criterio M de quiebre
(Honest DiD)— y la deja **explícitamente pendiente**: "DECISIÓN PENDIENTE:
no se cambia todavía la medida del script principal. Se discute con el
director (Andrés Ham) con estos resultados." La regla que efectivamente
corre en `04_decision_medida.R` (candidatos Bite/Exposure/golpe_a/golpe_c/
golpe_costo, criterio celda limpia + especificación estándar + estabilidad)
no aparece en `NOTA_DECISIONES.md` en ninguna versión, en ninguna rama. No
encontré ningún documento comiteado que registre esa transición — ni cuándo
se decidió cambiar de marco, ni qué mostró la conversación con el director,
ni por qué se abandonó el criterio de M de quiebre.

Esto no prueba que el cambio fue arbitrario. Pero rompe la cadena de
trazabilidad que el propio equipo se había impuesto (comitear la regla ANTES
de correr el bloque, práctica seguida rigurosamente hasta el 17-sep) justo en
el paso más importante: cuál es la medida principal de toda la tesis.

**Relación entre los dos puntos de falla**: no son independientes. Si el
marco "celda limpia + 3 criterios" se adoptó precisamente porque alguien
notó el problema de sesgo de división (razonamiento plausible dado que la
celda limpia existe para eso), entonces el punto de falla secundario podría
ser la CAUSA administrativa del primario: se corrigió el sesgo en el paso de
selección de medida, pero se olvidó corregirlo en el paso de estimación
principal, quizás porque la decisión de adoptar la corrección nunca se
escribió como una decisión formal que obligara a revisar todo el pipeline
aguas abajo.

### Sobre "esperable por construcción" (eslabón 6)

El pipeline explica la tendencia previa descendente del costo laboral en las
firmas expuestas como "esperable por construcción": Kaitz se mide con el
salario de 2022, así que las firmas de alta exposición son por definición
las que llegaron a 2022 con el costo más bajo. **Esa explicación es
correcta como descripción del pasado (2015-2022)**, y honesta en el sentido
de que no se oculta. Pero, leída junto con el punto de falla primario, es
también, sin que el texto lo diga así, una admisión parcial: si el mecanismo
que genera la tendencia previa (reversión/persistencia del salario propio de
la firma) es "esperable por construcción" antes de 2023, no hay razón
estadística para que ese mismo mecanismo se apague exactamente en 2023. La
frase funciona como defensa de la tendencia previa, pero no está escrita
como lo que también podría ser: una razón para dudar del efecto del período
posterior, no solo del anterior.

---

## 4. Qué sobrevive al punto de falla y qué no

| Resultado | ¿Depende del primer eslabón siendo "limpio"? | Veredicto |
|---|---|---|
| Descriptivos (cap. 1): salario mínimo, perfil de firmas, conteos de exposición | No — son descripción, sin afirmación causal | **Sobrevive intacto** |
| Identidad del empleo, correlación panel firma/establecimiento (eslabón 1) | No | **Sobrevive intacto** |
| Que existe una relación estadística entre medidas basadas en salario y crecimiento del costo laboral | No, es un hecho, confirmado tres veces (sección 2 y 3) | **Sobrevive, pero cambia de interpretación**: de "evidencia del choque de 2023" a "propiedad general de la dinámica salarial de las firmas, con una parte —34% a 63%— atribuible al choque específico de 2022-2023 sin poder descartar que el resto sea genérico" |
| 4,03% / 4,80% como cifra principal del primer eslabón | Sí, directamente — misma construcción que la prueba contaminada | **No sobrevive sin matiz.** Sigue siendo la cifra correctamente calculada de la especificación que el pipeline define como principal, pero su interpretación como "efecto del choque de 2023" no está aislada de la reversión genérica. Debe reportarse con esta limitación explícita, no como estaba |
| 1,15% (celda limpia) | Es, por diseño, la versión que evita el problema | **Es la cifra más defendible del conjunto**, aunque sea la menos citada — coherente con lo que ya decía `NARRATIVA.md` antes de esta auditoría |
| Elección de Bite como medida principal | Sí, la elección se basó en parte en que Bite "gana" la primera etapa | **Debilitada**: Bite es la segunda medida más contaminada del grupo de cuatro (sección 2); golpe_c y golpe_a sobreviven proporcionalmente mejor a la prueba de la ventana limpia |
| Segundo eslabón (empleo) | Indirectamente — la interpretación de "el choque redujo/no redujo empleo" asume que la exposición mide el choque de 2023, no una "mala racha" genérica de 2022 | **En duda por una vía distinta**: `NOTA_DECISIONES.md` ya documentó que Kaitz 2022 alto identifica firmas con un 2022 atípicamente malo en salario, ventas Y brecha salarial simultáneamente (forma de "V"). Si el empleo de esas firmas también tiene una dinámica propia alrededor de 2022 no relacionada con el mínimo, el resultado de empleo hereda el mismo problema por una ruta distinta a la del costo laboral |
| Heterogeneidad por tamaño, mecanismos (compresión salarial 0,22 DE) | Sí, usa Bite2022 y la misma referencia 2022 | **No probado, ni a favor ni en contra.** La lectura B (salto contra la tendencia propia) es una defensa metodológicamente razonable contra la versión de este problema que `NOTA_DECISIONES.md` había detectado en la brecha salarial cruda ("salta a 0 exactamente en 2022"). Pero nadie corrió el equivalente de la prueba de la sección 2 (outcome que no toca 2022) sobre los mecanismos. No se puede afirmar que sobrevive ni que no sobrevive |
| Placebo 2018-2019 del primer eslabón (ya declarado no informativo) | Es la misma familia de problema, ya reconocida y correctamente excluida de la decisión | **Consistente con el resto de esta auditoría** — el placebo rodante de `descartado/Diagnostico_reversion` (T01) generaliza esta misma observación a los ocho años de la serie, no solo a 2018-2019 |

---

## 5. Qué se habría hecho distinto — en orden cronológico

Material para la sección de limitaciones, no reproche:

1. **Antes de fijar la especificación del primer eslabón (pre 16-sep)**:
   correr la prueba de la sección 2 de esta auditoría (outcome que no
   comparte año con la medida) **como parte del diseño de la medida**, no
   como auditoría posterior. Si se hubiera visto entonces que golpe_costo
   pierde 63% y Bite pierde 50% de su coeficiente al romper el año
   compartido, la discusión de qué medida usar habría empezado con esa
   información, no habría llegado a ella ocho días después de fijar el
   pipeline final.
2. **En el momento de decidir la medida principal (17-sep, `NOTA_DECISIONES.md`)**:
   la decisión quedó explícitamente pendiente ("se discute con el
   director"). Lo que habría faltado no es la pausa — pausar fue correcto —
   sino que la reanudación de esa decisión, cuando ocurrió, no se
   documentó con el mismo estándar que el resto de esa nota (registrar la
   evidencia y el criterio ANTES de aplicarlo). El cambio de marco completo
   (de M de quiebre sobre tres Kaitz, a celda-limpia sobre cinco medidas) es
   silencioso en el repositorio.
3. **Al construir la especificación principal del event study**: aplicar el
   mismo cuidado que ya existía para la selección de medida (usar un año
   base para la exposición distinto del año de referencia del outcome, como
   hace la celda limpia) también a la estimación principal, o al menos
   documentar explícitamente por qué no se aplicó ahí y correr esa versión
   como robustez obligatoria, no opcional.
4. **Al escribir `05_resultados_y_mecanismos.R` y `06_mecanismos_por_grupo.R`**
   (fase de esta misma sesión, sep-2026): la lectura B para los mecanismos
   fue un intento correcto de responder al hallazgo de `NOTA_DECISIONES.md`
   sobre la brecha salarial. Habría faltado, en ese mismo momento, extender
   la prueba de la sección 2 de esta auditoría a los mecanismos, ya que la
   lectura B resuelve un síntoma distinto (nivel plano que salta a 0) del
   que se audita aquí (contaminación del denominador).
5. **Sobre el aumento real del salario mínimo**: la tarea de traer cifras de
   inflación del DANE/Banrep se anotó como pendiente el 16-sep, se repitió
   como pendiente el 17-sep, y sigue pendiente el 25-sep. No es un fallo de
   una sola decisión sino de seguimiento: tres oportunidades registradas de
   cerrarla, ninguna tomada.
6. **Sobre el control de versiones**: `NOTA_DECISIONES.md` debió moverse con
   el código cuando el trabajo pasó a esta rama — es la bitácora de
   decisiones citada por el propio código en ejecución (`grep` la encuentra
   seis veces). Y `09_diagnostico_reversion.R`, que generó evidencia
   directamente relevante para el problema central de la tesis, debió
   comitearse junto con su salida, no dejarse como archivos sueltos sin
   rastro del código que los produjo.

---

## 6. Resumen para quien solo lea esta sección

La hipótesis del encargo se confirma parcialmente: el año 2022 compartido
entre la medida de exposición y el outcome del primer eslabón infla el
coeficiente estándar entre 34% y 63% según la medida, y Bite —la medida
elegida— no es la que mejor resiste esa prueba entre las cuatro basadas en
salario. No se confirma en su forma fuerte: queda una relación significativa
incluso después de romper el año compartido, así que no todo es aritmética.

El problema más serio no es la primera etapa en sí — es que la misma
estructura contaminada (Bite fijo en 2022, 2022 como año de referencia) es
la base de la especificación principal de la tesis, no solo de un paso de
selección de medida que ya tenía su propia defensa (la celda limpia). Esa
defensa nunca se extendió al resultado principal.

Separado de todo lo anterior: la premisa de que el aumento de 2023 fue un
choque real distinguible de la inflación normal nunca se verificó con cifras
oficiales, algo que el propio equipo señaló como pendiente hace más de una
semana y que sigue sin resolverse.

Ninguno de estos hallazgos invalida el trabajo hecho. La celda limpia
(1,15%) sigue siendo, con esta auditoría, la estimación más defendible del
conjunto — y ya estaba marcada así antes de empezar. Lo que esta auditoría
agrega es evidencia directa de por qué esa cautela estaba justificada, y de
que no se aplicó de forma pareja a todo el pipeline.
