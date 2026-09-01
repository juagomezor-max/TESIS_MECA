# Notas: evaluacion de mover la unidad de observacion a establecimiento-anio

Rama: `feature/panel-establecimiento`. Objetivo: evaluar si el diseño DiD de
la tesis (choque de salario minimo 2023, EAM) debe correrse a nivel de
establecimiento-anio (NORDEST-ANIO) en vez de empresa-anio (NORDEMP-ANIO).
No se reconstruye nada del panel principal hasta aprobar cada paso.

## Paso 1.2 — Identificador de establecimiento

**Confirmado: `NORDEST`.**

Evidencia:
- Diccionario maestro de variables (`1. DATOS/3. DICCIONARIOS/diccionario_maestro_variables.csv`,
  generado desde los labels DTA oficiales de la EAM): `NORDEST` = "Numero de
  Establecimiento", presente en los 17 de 17 anios del panel (2008-2024).
  `NORDEMP` = "Numero de Empresa", tambien en los 17 anios.
- Macrobase: `NORDEST` es numerico, 0 valores NA/vacios en las 140,835 filas.

## Paso 1.3 — Confiabilidad de NORDEST como panel longitudinal

Script: `3. SCRIPTS/auditar_confiabilidad_nordest.R`. Mismo rigor que se
aplico a NORDEMP en la auditoria de reproducibilidad (rama
`feature/auditoria-reproducibilidad-macrobase`, ya en main).

### 1) Establecimientos unicos por anio (2008-2024)

Pico en 2010 (9,945 establecimientos), declive sostenido hasta 6,583 en
2024 (-34% desde el pico, mismo patron de contraccion del marco muestral
ya documentado para NORDEMP -37% 2010-2024). Dato adicional relevante:
en **los 17 anios, `filas == establecimientos`** (verificado con
`dplyr::n()` vs `dplyr::n_distinct(NORDEST)` por ANIO) -- confirma que
`NORDEST-ANIO` es unico en **todo el panel** 2008-2024, no solo en la
ventana 2015-2019 verificada previamente en
`validar_tendencias_paralelas_establecimiento.R`.

### 2) Estabilidad del NORDEST en firmas de UN establecimiento

Proxy: entre 114,073 pares anio-consecutivo (NORDEMP con exactamente 1
NORDEST tanto en el anio t como en t+1), solo **4 casos (0.004%)**
muestran cambio de NORDEST sin razon aparente. Evidencia muy fuerte de
que el identificador no se recodifica arbitrariamente en firmas de una
sola planta (donde el chequeo es inequivoco, sin ambiguedad de cual
establecimiento es cual).

### 3) Distribucion de anios presentes por NORDEST

De 12,621 establecimientos unicos en 2008-2024:
- 5.48% aparece en un solo anio (altas/bajas genuinas).
- 31.11% aparece en los 17 anios completos (panel largo/balanceado).
- El resto se distribuye de forma pareja entre esos extremos (3-7% por
  cada conteo intermedio de anios) -- consistente con entradas/salidas
  reales del marco muestral, no con ruido de codificacion.

### 4) NORDEST asociados a mas de un NORDEMP (cambio de empresa duena)

**169 de 12,621 establecimientos (1.34%)** tienen mas de un NORDEMP
distinto en el periodo (181 transiciones totales). La distribucion de
estos cambios por anio es pareja (5-21 casos/anio, sin picos
sospechosos) -- consistente con M&A/reestructuracion corporativa
genuina, no con un artefacto de recodificacion masiva concentrado en un
anio especifico (se revisaron especificamente los anios extremos del
panel, incluido 2024, sin encontrar sobre-representacion).

**Tratamiento propuesto** (pendiente de aprobacion explicita del
usuario, TODAVIA NO decidido unilateralmente): mantener estos 169
establecimientos en el panel, usando NORDEST como efecto fijo de la
unidad fisica y clusterizando errores estandar por NORDEMP vigente en
cada anio; opcionalmente correr una robustez excluyendolos para
confirmar que el resultado no cambia materialmente.

### 5) Chequeo adicional: recodificacion en firmas MULTIPLANTA

Script: `3. SCRIPTS/auditar_recodificacion_multiplanta_nordest.R`. El
Paso 2 (arriba) solo puede detectar recodificacion sin ambiguedad en
firmas de un establecimiento. En firmas multiplanta, un cambio real de
NORDEST podria confundirse con una apertura+cierre genuina de plantas.

Señal indirecta usada: un NORDEST que desaparece de una firma en el anio
t y OTRO NORDEST que aparece en la MISMA firma en t+1 ("swap" 1-a-1),
con mismo CIIU4 y tamaño (PERTOTAL) similar (razon min/max >= 0.5) --
sugestivo de recodificacion, no confirmatorio.

Resultado: de 447 firmas multiplanta (2008-2024) y 6,697 transiciones
anio-consecutivo, solo **15 son swaps limpios 1-a-1**, y de esos solo
**4 son "candidatos a recodificacion"** (mismo CIIU4 + tamaño similar) --
0.06% de las transiciones, 0.03% de los 12,621 establecimientos totales.
Los 4 casos se inspeccionaron individualmente (ver
`auditoria_nordest_swaps_candidatos_recodificacion.csv`): son plausibles
pero no concluyentes.

**LIMITACION CONOCIDA (documentada, no resuelta):** la macrobase EAM no
tiene columnas de direccion/municipio (solo DPTO, geografia gruesa), asi
que este chequeo es indirecto e imperfecto -- no distingue con certeza
una recodificacion real de una firma que genuinamente cierra una planta
y abre otra similar el mismo anio. Se documenta como cota superior de la
posible recodificacion no detectada, no como conteo definitivo. Dada la
magnitud marginal (4 de 12,621 establecimientos), no compromete la
confiabilidad general de NORDEST.

### Conclusion del Paso 1.3

NORDEST es un identificador tan confiable como NORDEMP para uso como
panel longitudinal: unico en el 100% del panel (17/17 anios), con
recodificacion practicamente inexistente en firmas de un establecimiento
(0.004%) y marginal incluso bajo el chequeo mas permisivo en firmas
multiplanta (0.03% de establecimientos). El unico punto que requiere una
decision explicita es el tratamiento de los 169 establecimientos (1.34%)
que cambian de empresa duena -- ver propuesta en el punto 4, pendiente
de aprobacion.

## Paso 2.1 — Variable de ubicacion (departamento)

**Confirmado: `DPTO`.**

Evidencia:
- Diccionario maestro EAM: `DPTO` = "Departamento", presente en los 17 de
  17 anios (2008-2024). Es la UNICA variable de ubicacion en la
  macrobase -- no hay columna de municipio, ciudad ni codigo DIVIPOLA
  completo por separado (busqueda de `DPTO|DEPARTA|DIVIPOLA|COD_DPT|MUNI|REGION`
  en las 398 columnas, sin otros resultados).
- Formato: numerico, 23 valores unicos, 0 NA/vacios en las 140,835
  filas. Valores (5, 8, 11, 13, 15, 17, 19, 20, 23, 25, 41, 47, 50, 52,
  54, 63, 66, 68, 70, 73, 76, 85, 99) consistentes con codigos DIVIPOLA
  de departamento (ej. 5=Antioquia, 11=Bogota D.C., 76=Valle del Cauca).
  Granularidad de DEPARTAMENTO, no de municipio.

## Paso 2.2 — Cobertura y consistencia de DPTO por anio

Script: `3. SCRIPTS/auditar_cobertura_dpto_establecimiento.R`.

**Cobertura: 100% en los 17 anios (2008-2024), sin excepcion.** Ningun
anio tiene cobertura por debajo del 99% -- de hecho ningun anio tiene
cobertura por debajo del 100%. No hay evidencia de un anio con captura
notablemente peor que el resto.

**Chequeo adicional (mismo espiritu que el Paso 1.5):** ademas del % de
cobertura, se comparo el CONJUNTO EXACTO de codigos DPTO presentes cada
anio (un cambio de metodologia podria mantener 100% de cobertura pero
alterar que codigos se usan, ej. reagrupar departamentos o introducir un
codigo de "no clasificado"). Resultado: **el conjunto de 23 codigos DPTO
es IDENTICO en los 17 anios**, sin una sola variacion. Sin evidencia de
cambio de formulario o metodologia de captura que afecte esta variable
en ningun punto del periodo.

### Conclusion del Paso 2.2

DPTO tiene cobertura y consistencia perfectas en todo el panel
2008-2024. Se puede usar sin reservas para construir
`departamento*anio` como efecto fijo en la especificacion a nivel
establecimiento.

## Paso 2.3 — Estabilidad de DPTO DENTRO de un mismo NORDEST

Script: `3. SCRIPTS/auditar_estabilidad_dpto_nordest.R`. Un
establecimiento es una ubicacion fisica: el departamento no deberia
cambiar de un anio a otro para el mismo NORDEST, salvo relocalizacion
real (rara) o error/recodificacion.

### Cuantificacion

**465 de 12,621 establecimientos (3.68%) tienen mas de un DPTO distinto
en su panel.** Clasificacion del patron (run-length encoding de la
secuencia DPTO ordenada por anio):

| Patron | N | % de inestables |
|---|---|---|
| `cambio_sostenido` (cambio permanente, sin reversion -- 2 valores, 2 corridas) | 415 | 89.2% |
| `patron_irregular` (alternancia repetida o >2 valores -- revision manual) | 37 | 7.96% |
| `salto_aislado` (un anio se desvia y vuelve al valor original -- probable error puntual) | 13 | 2.8% |

### Hallazgo principal: concentracion en el par Bogota D.C. (11) / Cundinamarca (25)

**334 de los 465 establecimientos inestables (71.8%) alternan
EXCLUSIVAMENTE entre DPTO 11 (Bogota D.C.) y DPTO 25 (Cundinamarca)**;
409 de 465 (88%) involucran a 11 o 25 en algun momento de su secuencia.
Dentro de los 415 `cambio_sostenido`, 291 son especificamente el par
11->25 (permanente, sin reversion), concentrados en **2010-2013** (234
de esos 291), con un goteo menor y sostenido 2016-2024. Los casos
`salto_aislado` y `patron_irregular` muestran el mismo patron
dominante (alternancia entre 11 y 25), solo que con timing menos limpio.

### Verificacion externa: ¿cambio de codificacion DIVIPOLA?

Se busco explicitamente (WebSearch, no memoria) si DANE tuvo algun
cambio de codigos DIVIPOLA de DEPARTAMENTO (no municipio) en Colombia
durante 2008-2024, especificamente para Bogota D.C. (11) o Cundinamarca
(25). No se encontro evidencia de tal cambio -- el unico cambio DIVIPOLA
identificado en el periodo es la creacion de un MUNICIPIO nuevo en 2023
(Nuevo Belen de Bajira, segregado de Riosucio), no relacionado con
codigos de departamento. Fuentes: DANE (codificacion DIVIPOLA),
datos.gov.co (codigos de departamentos).

Esto, combinado con el hallazgo del Paso 2.2 (el conjunto de 23 codigos
de departamento es IDENTICO en los 17 anios, sin altas ni bajas de
codigos), **descarta la hipotesis de un cambio de codificacion DIVIPOLA
nacional** como explicacion del patron 11<->25.

### Interpretacion

El patron es casi con certeza una **correccion/ambiguedad de
clasificacion geografica especifica de la EAM**, no relocalizaciones
fisicas reales ni un cambio de codigo DIVIPOLA: establecimientos
fisicamente ubicados en municipios industriales de Cundinamarca vecinos
a Bogota (cinturon industrial de la sabana: Soacha, Mosquera, Funza,
Cota, Chia, Zipaquira, Tocancipa) son un caso de ambiguedad conocida en
Colombia entre la direccion fiscal/administrativa de la firma (a
menudo en Bogota) y la ubicacion fisica de la planta (a menudo en
Cundinamarca) -- consistente con que DANE haya ido corrigiendo esta
clasificacion de forma gradual a lo largo del panel, mas intensamente
en 2010-2013.

### Implicacion practica para el panel del DiD

La mayoria de las transiciones 11->25 ocurrieron en 2010-2013, ANTES de
la ventana de pre-choque 2015-2019 que se usara en el diseño DiD -- el
impacto practico en la ventana de estimacion es limitado (para
2015-2019+2023, la mayoria de estos establecimientos ya habian
"asentado" su DPTO). Aun asi, queda un goteo de cambios en 2016-2024 que
si cae dentro de la ventana de estimacion.

**Tratamiento APROBADO (regla diferenciada por patron):** fijar DPTO a
un UNICO valor por establecimiento (invariante en el tiempo), en vez de
dejarlo variar anio a anio, con el criterio dependiendo del patron de
inestabilidad:

- **`cambio_sostenido` (415 casos, 89.2% de los inestables):** usar el
  DPTO vigente en el anio base de exposicion (**2022**), por
  coherencia metodologica con el resto del diseño (Exposure2022_obreros
  y Bite2022_obreros tambien se miden en 2022). Tiene sentido especifico
  para este patron porque el cambio es permanente -- el valor de 2022
  ya refleja la ubicacion "asentada" post-cambio en la gran mayoria de
  los casos (la mayoria de las transiciones ocurrieron en 2010-2013,
  antes de 2022).
- **`salto_aislado` (13 casos, 2.8%) y `patron_irregular` (37 casos,
  8.0%):** usar el DPTO **modal** (mas frecuente en todo el panel del
  establecimiento). Tiene sentido para estos patrones porque no hay un
  "antes/despues" limpio que el anio 2022 pueda representar de forma
  confiable -- el modal es mas robusto a un dato puntual erroneo (salto
  aislado) o a alternancia sin patron claro (irregular).

Los 37 casos `patron_irregular` (0.29% del total de establecimientos)
se marcan para revision manual si se requiere mayor precision, aunque
dada su magnitud marginal no se considera bloqueante.

### Conclusion del Paso 2.3

DPTO es estable dentro del 96.32% de los establecimientos. La
inestabilidad restante (3.68%) esta concentrada de forma casi exclusiva
(71.8%) en un patron identificable y explicable (ambiguedad
Bogota/Cundinamarca en la EAM, no relocalizacion real ni cambio
DIVIPOLA), y tiene un tratamiento aprobado (regla diferenciada por
patron: 2022 para `cambio_sostenido`, modal para `salto_aislado` y
`patron_irregular`).

## Paso 2.4 — Distribucion de establecimientos por departamento

Script: `3. SCRIPTS/auditar_distribucion_dpto_establecimiento.R`. Objetivo:
dimensionar el riesgo de celdas `departamento*anio` pequeñas/sin
variacion para la especificacion a nivel establecimiento. DPTO
representativo usado para la distribucion cross-seccional: el MODAL de
cada NORDEST (robusto a la inestabilidad del Paso 2.3, no depende de la
decision de tratamiento).

### Distribucion cross-seccional (23 departamentos)

Concentrada pero no degenerada: top 3 (Bogota D.C. 35.4%, Antioquia
21.0%, Valle del Cauca 12.8%) = 69.3% del total; cola larga hasta
Vichada (99), con 26 establecimientos (0.21%). Solo **1 departamento
(Vichada) tiene menos de 30 establecimientos** en todo el panel
2008-2024.

| Rango | Departamentos (codigo DIVIPOLA) | Establecimientos |
|---|---|---|
| Top 3 | Bogota (11), Antioquia (5), Valle del Cauca (76) | 8,750 (69.3%) |
| Top 5 | + Cundinamarca (25), Santander (68) | 10,160 (80.5%) |
| Mas pequeño | Vichada (99) | 26 (0.21%) |

### Celdas departamento-anio en la ventana relevante del DiD (2015-2019 + 2023)

Esto es lo que realmente importa para identificar `departamento*anio`
como efecto fijo (no la distribucion cross-seccional total). Se revisaron
las 23 x 6 = 138 celdas departamento-anio de la ventana pre/post del
diseño:

- **Ninguna celda tiene menos de 10 establecimientos** (el minimo
  observado es 14, en Casanare (85) y Vichada (99), los departamentos
  mas pequeños).
- **Cero celdas completamente vacias.**
- El departamento mas grande (Bogota, 11) va de 2,100 a 3,214
  establecimientos por anio en esa ventana.

### Conclusion del Paso 2.4

Pese a la concentracion geografica fuerte (69.3% en solo 3
departamentos), NO hay riesgo de celdas `departamento*anio` sin
variacion en la ventana de estimacion relevante -- incluso los
departamentos mas pequeños mantienen al menos 14 establecimientos por
anio. `departamento*anio` esta bien identificado para la especificacion
a nivel establecimiento.

## Paso 2.5 — Cruce departamento x sector (CIIU4): riesgo de colinealidad

Script: `3. SCRIPTS/auditar_cruce_dpto_ciiu4.R`. Objetivo: verificar si
hay departamentos donde la actividad manufacturera esta muy concentrada
en uno o dos sectores -- riesgo de colinealidad entre `sector*anio` y
`departamento*anio` si un departamento es casi monosectorial (ambos
efectos fijos absorberian esencialmente la misma variacion para ese
departamento). NO se construyo el panel de establecimiento-anio ni se
corrio ninguna regresion -- exclusivamente validacion de la variable de
ubicacion. Ventana: establecimiento-anio 2015-2019+2023 (misma que
Paso 2.4), 47,951 observaciones con DPTO y CIIU4 validos.

### Concentracion sectorial por departamento

Metrica principal: HHI (Herfindahl-Hirschman, participaciones de cada
CIIU4 dentro del DPTO, escala 0-1; mas alto = mas concentrado).

**Solo 1 de 23 departamentos supera el 50% de sus establecimiento-anio
en UN SOLO sector CIIU4: Casanare (85), con 50.5% en el sector 1051
(elaboracion de productos lacteos)**, coherente con su economia
ganadera. Le sigue Vichada (99) con 44.8% en el sector 3290.

| Departamento (DIVIPOLA) | Estab.-anio (ventana) | Sectores CIIU4 distintos | % top1 sector | HHI |
|---|---|---|---|---|
| Casanare (85) | 107 | 4 | 50.5% | 0.354 |
| Vichada (99) | 105 | 4 | 44.8% | 0.306 |
| Nariño (52) | 277 | 9 | 35.0% | 0.210 |
| Sucre (70) | 117 | 5 | 34.2% | 0.263 |
| ... (19 departamentos intermedios, HHI 0.03-0.19) | | | | |
| Cundinamarca (25) | 3,894 | 79 | 6.4% | 0.026 |
| Santander (68) | 2,119 | 58 | 8.3% | 0.039 |
| Atlantico (8) | 1,979 | 66 | 9.2% | 0.033 |
| Bogota D.C. (11) | 16,195 | 116 | 9.2% | 0.033 |
| Valle del Cauca (76) | 6,175 | 91 | 9.9% | 0.031 |

### Patron detectado: los departamentos pequeños son tambien los mas concentrados

Los 2 departamentos con MENOS establecimientos (Paso 2.4: Casanare y
Vichada, ambos con minimo anual de 14 establecimientos) son tambien los
2 con MAYOR concentracion sectorial (HHI 0.354 y 0.306). Los
departamentos grandes (Bogota, Valle, Santander, Cundinamarca, todos con
>2,000 establecimiento-anio) son los mas diversificados (HHI entre
0.026 y 0.039). Este patron **compone** (no crea de la nada, pero
agrava) el riesgo de celdas pequeñas ya identificado en el Paso 2.4:
ademas de tener pocos establecimientos, esos pocos estan concentrados en
1-2 sectores, dejando menos variacion independiente para separar
`sector*anio` de `departamento*anio` especificamente en esos casos
puntuales (Casanare y, en menor medida, Vichada).

### Nota metodologica: el sector CIIU 3290 no es señal de especializacion real

El sector CIIU4 3290 ("Otras industrias manufactureras n.c.p.") aparece
como el sector TOP1 en 14 de los 23 departamentos. Esto NO indica
especializacion real: es un codigo generico/heterogeneo que agrupa
actividades misceláneas de manufactura y es grande en casi todo el pais
por construccion de la clasificacion, no por una concentracion
economica genuina en un departamento especifico.

### Conclusion del Paso 2.5

El riesgo de colinealidad `sector*anio` / `departamento*anio` es
marginal y esta acotado a 1-2 departamentos pequeños (Casanare y,
en menor medida, Vichada), que ya estaban señalados como los de menor
tamaño en el Paso 2.4. El resto de los 23 departamentos tiene HHI bajo
(<0.22) y suficiente diversidad sectorial. No es un problema
generalizado que comprometa la especificacion completa, pero se
documenta como punto de atencion especifico para esos 1-2
departamentos si se usan controles `sector*anio` y `departamento*anio`
simultaneamente.

## Paso 2.6 — Celdas departamento-anio en la ventana REAL del panel final

Script: `3. SCRIPTS/auditar_celdas_departamento_anio_ventana_final.R`. El
Paso 2.4 solo verifico celdas en 2015-2019+2023 (ventana del diagnostico
preliminar de pre-tendencias). Falta confirmar la ventana real del panel
formal del DiD y repetir el chequeo ahi.

### Ventana del panel final (confirmada, no una decision nueva)

Se consolido a partir de decisiones YA explicitas en otros archivos del
repo:

- **Pre-periodo: 2015-2019 + 2021-2022** -- pendiente explicito en
  `0. PREPARACION/notas_exposicion_obreros_eam.md`, seccion "Pendientes
  abiertos para la siguiente sesion": *"Construir el panel formal
  2015-2019 + 2021-2022 para el event study"*.
- **2020 EXCLUIDO** del pre-periodo -- mismo criterio ya usado en
  `3. SCRIPTS/diagnostico_preliminar_tendencias_2015_2019.R`: *"EXCLUYE
  2020 explicitamente porque ese anio arranca el choque de la pandemia
  (COVID-19), que introduciria una discontinuidad ajena al diseño de
  pre-tendencias"*.
- **Post-periodo: 2023-2024** -- siguiendo la convencion `periodo_2023`
  ya usada en todo el proyecto (`construir_exposicion_obreros_eam.R`,
  `descriptivo_exposicion_eam.R`): `ANIO %in% 2023:2024 ~ "Post (2023-2024)"`.

**PANEL_ANIOS_FINAL = 2015, 2016, 2017, 2018, 2019, 2021, 2022, 2023,
2024 (9 anios).**

### Celdas departamento-anio en la ventana final (23 x 9 = 207 celdas)

- **Ninguna celda tiene menos de 10 establecimientos.** Minimo
  observado: **13** (Vichada, 2024). Segundo minimo: 14 (Casanare, 2015).
- **Cero celdas completamente vacias.**
- El departamento mas grande (Bogota, 11) va de 1,998 a 3,214
  establecimientos por anio en esta ventana.

### ¿2020 o 2021 cambian el panorama en Casanare/Vichada?

Se reviso la serie ANUAL COMPLETA 2008-2024 de ambos departamentos
(`auditoria_celdas_casanare_vichada_todos_anios.csv`):

- **Casanare (85):** crecimiento suave y sostenido en establecimientos
  reportantes durante todo el periodo (14 en 2015 -> 26 en 2024). El
  valor de 2020 (21) esta perfectamente en linea con la tendencia
  (2019=20, 2020=21, 2021=22) -- **sin quiebre visible por la pandemia**.
- **Vichada (99):** conteo estable/plano (16 establecimientos en 2019,
  2020 Y 2021, identico) -- **tampoco hay disrupcion visible**.

**Conclusion: 2020 y 2021 NO cambian el panorama de riesgo en estos
departamentos.** El numero de establecimientos reportantes no muestra
ninguna caida ni salto atribuible a la pandemia en ninguno de los dos
casos; la exclusion de 2020 del panel se sostiene por el criterio ya
documentado (discontinuidad de TENDENCIAS, no de cobertura/conteo), no
porque el conteo de establecimientos se vea afectado.

### Conclusion del Paso 2.6

Con la ventana real del panel final (9 anios, 2020 excluido), el
resultado es igual de solido que con la ventana preliminar del Paso 2.4:
ninguna celda departamento-anio queda vacia o por debajo del umbral de
10 establecimientos. `departamento*anio` sigue bien identificado.

## Paso 2.7 — Robustez Casanare/Vichada: limitacion conocida y criterio preparado

### Limitacion conocida (documentada explicitamente)

Casanare (85) y Vichada (99) combinan, de forma simultanea:

1. **Baja N**: los 2 departamentos con menos establecimientos del panel
   (minimo anual 13-14 en la ventana final, ver Paso 2.6; total 26-31 en
   todo el panel 2008-2024, ver Paso 2.4).
2. **Alta concentracion sectorial**: los 2 departamentos con mayor HHI
   (Casanare 0.354, Vichada 0.306 -- ver Paso 2.5), muy por encima del
   resto (siguiente mas alto: Nariño con 0.210).

Esta combinacion -- pocas observaciones Y poca variacion sectorial
dentro de esas pocas observaciones -- es una **limitacion conocida del
control `departamento*anio`** para estos 2 departamentos especificamente:
la potencia estadistica para identificar efectos especificos de
Casanare/Vichada, separados de sus sectores dominantes, es baja. No es
un problema que invalide la especificacion completa (ningun otro
departamento combina ambos riesgos, y ninguna celda esta vacia -- Paso
2.6), pero se documenta explicitamente para no ignorarlo al interpretar
resultados de esos 2 departamentos en particular.

### Criterio de robustez preparado (NO ejecutado todavia)

Cuando se corra la especificacion formal del DiD a nivel establecimiento
(pendiente, fuera del alcance de este paso de validacion), se preparara
la siguiente comparacion de robustez:

1. **Especificacion principal**: los 23 departamentos completos, cada
   uno como categoria propia de `departamento*anio`.
2. **Robustez A -- exclusion**: repetir la especificacion principal
   EXCLUYENDO Casanare y Vichada del panel, para verificar que el
   resultado no depende de esos 2 casos de baja potencia.
3. **Robustez B -- agrupacion**: repetir la especificacion principal
   agrupando Casanare, Vichada, y otros departamentos de baja N (usar el
   mismo umbral de <30 establecimientos totales del Paso 2.4, si aplica
   a mas casos en el panel final) en una categoria unica "Otros" dentro
   de `departamento*anio`, en vez de excluirlos.
4. Comparar los 3 resultados (principal, robustez A, robustez B): si los
   coeficientes de interes son estables entre las 3 especificaciones,
   confirma que Casanare/Vichada no estan distorsionando el resultado
   general. Si divergen, hay que investigar mas antes de reportar.

Este criterio queda preparado y documentado; su ejecucion queda
pendiente para cuando se corra la regresion formal del panel de
establecimiento-anio (no forma parte de este paso de validacion de la
variable de ubicacion).

## Paso 3.1 — Exposure2022_obreros_est (nivel establecimiento)

Scripts: `3. SCRIPTS/construir_conteo_personal_categoria_establecimiento_eam.R`,
`3. SCRIPTS/construir_exposicion_obreros_establecimiento_eam.R`. Version
a nivel establecimiento de `Exposure2022_obreros`, usando la composicion
ocupacional PROPIA de cada NORDEST en 2022 (no heredada de NORDEMP).
Misma formula, winsorizacion y quintiles que la version a nivel empresa.

- No requiere `group_by`/sumar para deduplicar: NORDEST-ANIO ya es unico
  (Paso 1). El script valida esto explicitamente y aborta si deja de
  cumplirse.
- **6,761 establecimientos con Exposure2022_obreros_est valida en 2022.**
- Chequeo de sanidad: correlacion con Exposure2022_obreros (empresa,
  heredada) = **0.964** -- alta, como se esperaba (la mayoria de las
  firmas tiene un solo establecimiento, donde ambas medidas coinciden
  casi exactamente; la diferencia proviene de las firmas multiplanta).
- Salidas (no versionadas): `exposicion_obreros_establecimiento_eam.rds/.csv`.

## Paso 3.2 — Descriptivos de estructura multiplanta EN 2022

Script: `3. SCRIPTS/descriptivos_estructura_multiplanta_2022.R`. Objetivo:
decidir si el diseño "dentro de firma" (delta_f(e),t, explotando
variacion de exposicion ENTRE establecimientos de una misma empresa)
tiene algo real que identificar.

### Reconciliacion de los 3 numeros (447 / 262 / 260) y missingness

**1) 447 vs 262 -- cortes temporales distintos, NO un error.** Las "447
firmas multiplanta" del Paso 1.5 (`auditar_recodificacion_multiplanta_nordest.R`)
son el universo medido sobre **todo el panel 2008-2024** (cualquier anio
en que la firma tuviera >1 NORDEST). Como `Exposure2022_obreros_est` es
un atributo medido especificamente en 2022, lo relevante para el diseño
"dentro de firma" es el universo multiplanta EN 2022, que es un numero
distinto por construccion (firmas que fueron multiplanta en otro anio
del periodo pero no en 2022, o que no reportaron en 2022, o viceversa).
**447 NO aplica a este analisis** -- es una estadistica de otro
chequeo, con otro corte temporal.

**`Multi_f` OFICIAL de este analisis (base 2022): 262 firmas.**

**2) 262 vs 260 -- confirmado: 2 firmas excluidas por dato insuficiente,
no un error.** De las 262 firmas multiplanta en 2022:

| Establecimientos con exposicion valida | N firmas |
|---|---|
| 0 | 0 |
| 1 (excluidas del calculo de rango) | 2 |
| 2+ (base del calculo de variacion interna) | 260 |
| **Total (verificacion)** | **262** |

Las 2 firmas excluidas: NORDEMP 141327 (6 establecimientos totales, 1
valido, 5 con `Exposure2022_obreros_est` faltante) y NORDEMP 977239 (2
establecimientos totales, 1 valido, 1 faltante). Con un solo valor
valido no se puede calcular un rango (max-min) -- de ahi que el
denominador del calculo de variacion interna sea **260, no 262**.

**Missingness general de `Exposure2022_obreros_est` en 2022** (punto 2
del Paso 3, reportado aqui): de **6,775 establecimientos totales** en
2022 (macrobase), **6,761 tienen exposicion valida** y **14 (0.21%)
faltan**. Dentro de las firmas multiplanta especificamente: 851
establecimientos totales, 843 validos, **8 faltantes (0.94%)** -- tasa
de missingness algo mayor que el promedio general, pero pequeña en
terminos absolutos.

Script: `3. SCRIPTS/descriptivos_estructura_multiplanta_2022.R`, salida
`descriptivos_multiplanta_2022_reconciliacion.csv`.

### 1) Distribucion de N establecimientos por firma (2022)

| N establecimientos | Firmas | % |
|---|---|---|
| 2 | 155 | 59.2% |
| 3 | 45 | 17.2% |
| 4+ | 62 | 23.7% |

### 2) Departamentos distintos por firma (2022)

**181 de 262 (69.1%) tienen presencia en 2 o mas departamentos** -- la
mayoria de las firmas multiplanta son geograficamente dispersas, no
concentradas en un solo departamento. Solo 81 (30.9%) estan en un unico
departamento (multiplanta pero concentrada geograficamente, sin el
problema que motivo el cambio de diseño).

### 3) Variacion interna de Exposure2022_obreros_est dentro de cada firma

260 de las 262 firmas tienen exposicion valida en 2+ de sus
establecimientos (2 firmas quedan fuera por datos faltantes). Rango
(max-min) entre establecimientos de la misma firma:

| Estadistico | Valor (pp) |
|---|---|
| Promedio | 25.7 |
| Mediana | 21.4 |
| p75 | 37.0 |
| p90 | 55.2 |
| Maximo | 94.1 |

### 4) Umbral de "variacion sustancial" y conteo de firmas que lo superan

**Umbral propuesto: 15 puntos porcentuales** (0.15 en la escala 0-1 de
Exposure2022_obreros_est). Justificacion: la medida es una proporcion
(participacion de obreros en el empleo); 15pp representa una diferencia
economicamente no trivial entre dos establecimientos de la misma firma,
no ruido de redondeo o winsorizacion, dado que la dispersion tipica de
Exposure2022_obreros cubre buena parte de la escala 0-1 (ver
diagnosticos de la rama feature/exposicion-obreros-operarios). Se
reporta tambien 20pp (limite superior del rango sugerido) como chequeo
de sensibilidad.

| Umbral | Firmas que lo superan | % |
|---|---|---|
| >= 15pp | 168 de 260 | **64.6%** |
| >= 20pp | 137 de 260 | **52.7%** |

### Conclusion del Paso 3.2

**No es un puñado de casos.** Bajo cualquiera de los dos umbrales
considerados, mas de la mitad de las 262 firmas multiplanta de 2022
tienen variacion interna sustancial de exposicion entre sus
establecimientos -- incluso con el umbral mas conservador (20pp), 137
firmas la superan. Ademas, 69.1% de las firmas multiplanta tienen sedes
en departamentos distintos (no es solo variacion dentro del mismo
departamento). El diseño "dentro de firma" (delta_f(e),t) tiene una
base empirica solida para identificar, no solo una justificacion
teorica.

## Paso 3.3 — Distribucion comparativa: establecimiento vs. firma (2022)

Script: `3. SCRIPTS/comparar_distribucion_exposure_establecimiento_vs_firma.R`.

| Nivel | N | Media | Mediana | DE | p10 | p25 | p50 | p75 | p90 | % =0 | % =1 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Establecimiento | 6,761 | 0.601 | 0.633 | 0.231 | 0.286 | 0.466 | 0.633 | 0.769 | 0.875 | 3.03% | 2.48% |
| Firma | 6,180 | 0.602 | 0.632 | 0.226 | 0.292 | 0.472 | 0.632 | 0.769 | 0.870 | 2.88% | 2.01% |

**La dispersion MARGINAL (sobre toda la poblacion) es practicamente
igual entre los dos niveles** (razon DE=1.019x, IQR=1.02x, rango
p90-p10=1.02x) -- NO mayor a nivel planta que a nivel firma. Esto no
contradice la heterogeneidad interna del Paso 3.2 (variacion DENTRO de
cada firma multiplanta): son preguntas distintas. La razon aritmetica de
por que la dispersion marginal casi no cambia: los establecimientos de
firmas multiplanta son solo 12.6% del total (843 de 6,761), asi que su
variacion interna, aunque sustancial dentro de cada firma, pesa poco en
la distribucion agregada de TODA la poblacion.

## Paso 3.4 — Estructura multiplanta 2022 (parte 2): frecuencias y peso economico

Script: `3. SCRIPTS/descriptivos_estructura_multiplanta_2022_parte2.R`.

### Frecuencia de establecimientos por firma (TODAS las firmas, N=6,186)

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
| 3 | 25 | 9.5% |
| 4-11 | 46 | 17.6% |
| 16 | 1 | 0.4% |

81 de 262 (30.9%) tienen todas sus plantas en el mismo departamento.

### Peso economico de las firmas multiplanta -- EL NUMERO CLAVE

| Metrica | Valor |
|---|---|
| Firmas totales panel 2022 | 6,186 |
| Firmas multiplanta (`Multi_f`) | 262 |
| % de firmas multiplanta | 4.24% |
| Empleo total muestra 2022 (PERTOTAL, suma) | 710,060 |
| Empleo en firmas multiplanta (PERTOTAL, suma) | 152,227 |
| **% del empleo total que concentran** | **21.44%** |

**Las 262 firmas multiplanta son el 4.24% de las firmas pero concentran
el 21.44% del empleo total de la muestra 2022** -- factor ~5x entre peso
poblacional y peso economico. Este es el numero que define si el diseño
"dentro de firma" es central o marginal: cubre una porcion no trivial
del empleo manufacturero, no es un ejercicio de robustez marginal.

Nota de reconciliacion menor: el denominador de firmas puede reportarse
como 6,186 (todas las firmas del panel 2022) o 6,180 (firmas con
`Exposure2022_obreros` valida en 2022, 6 menos) -- la diferencia es
marginal y no cambia la conclusion (~4.24% en ambos casos).

## Paso 3.5 — Nivel de exposicion: plantas multiplanta vs. monoplanta

Script: `3. SCRIPTS/comparar_exposure_est_multiplanta_vs_monoplanta.R`.
Pregunta DISTINTA de la variacion interna (Paso 3.2): ¿el NIVEL de
exposicion de una planta de firma multiplanta es sistematicamente
distinto al de una planta monoplanta (heterogeneidad ENTRE grupos, no
DENTRO de uno)?

| Grupo | N establecimientos | N firmas | Media | Mediana | DE | p10 | p90 |
|---|---|---|---|---|---|---|---|
| Multiplanta | 843 | 262 | 0.584 | 0.610 | 0.250 | 0.222 | 0.894 |
| Monoplanta | 5,918 | 5,918 | 0.604 | 0.636 | 0.228 | 0.290 | 0.872 |

Diferencia de medias: -0.0202 (multiplanta ligeramente MENOS expuesta en
promedio). Prueba t de Welch: t=-2.217, df=1049.9, **p=0.027** --
estadisticamente significativa pero economicamente pequeña (2pp). Las
plantas multiplanta tienen mayor dispersion (DE 0.250 vs 0.228) y p10
notablemente mas bajo (0.222 vs 0.290), sugiriendo mas casos de baja
exposicion en la cola izquierda del grupo multiplanta.

### Nota clave: monoplanta = coincidencia por construccion

**El 87.4% de los establecimientos (5,924 de 6,775 en 2022) pertenecen
a firmas monoplanta**, donde `Exposure2022_obreros_est` (planta) y
`Exposure2022_obreros` (firma) coinciden por construccion -- son el
mismo calculo, porque la planta ES la firma. Toda la diferencia entre
las dos medidas de exposicion, y por tanto el valor añadido del diseño
"dentro de firma", se concentra en el 12.6% restante de establecimientos
(843 de 6,775) que pertenecen a las 262 firmas multiplanta.

## Paso 3.6 — Peso en empleo de firmas con variacion interna sustancial

Script: `3. SCRIPTS/calcular_peso_empleo_variacion_interna_2022.R`.
Distingue el peso en empleo del universo COMPLETO de 262 firmas
multiplanta (21.44%, Paso 3.4) del peso en empleo de SOLO las firmas
que superan el umbral de variacion sustancial (algunas de las 262
tienen plantas con exposicion casi identica, que no aportan variacion a
`delta_f,t`).

| Grupo | N firmas (% de 262) | Empleo (PERTOTAL) | % del empleo de las 262 | % del empleo total (710,060) | Empleo promedio/firma | Razon vs. promedio muestra (115) |
|---|---|---|---|---|---|---|
| Las 262 firmas multiplanta completas (referencia) | 262 (100%) | 152,227 | 100% | 21.4% | 581 | 5.06x |
| Firmas con brecha >=15pp | 168 (**64.1%**) | 115,594 | **75.9%** | **16.3%** | **688** | **5.99x** |
| Firmas con brecha >=20pp (conservador) | 137 (**52.3%**) | 98,231 | **64.5%** | **13.8%** | **717** | **6.25x** |

**La caida NO es proporcional al numero de firmas -- las firmas con
variacion interna sustancial son sistematicamente MAS GRANDES, no
equivalentes al resto.** Las 168 firmas (>=15pp) son 64.1% de las 262
en NUMERO, pero concentran 75.9% de su EMPLEO. Las 137 (>=20pp) son
52.3% en numero pero 64.5% en empleo. El empleo promedio por firma
confirma el patron: 581 personas en el universo completo de
multiplanta, 688 en las de brecha >=15pp, 717 en las de brecha >=20pp --
todas muy por encima del promedio de TODA la muestra (114.8, redondeado
115). Las firmas que aportan variacion sustancial a `delta_f,t` son en
promedio **~6 veces mas grandes** que la firma promedio de la muestra
(razon 5.99x y 6.25x respectivamente).

**Implicacion de generalizacion:** la especificacion "dentro de firma"
se identifica sobre un subconjunto de firmas sistematicamente mas
grandes que la firma tipica del panel. Los efectos estimados con este
diseño describen el comportamiento de firmas manufactureras grandes con
presencia multiplanta, no necesariamente el de la firma promedio de la
EAM -- una limitacion de validez externa que debe declararse
explicitamente al reportar resultados de la especificacion B.

## Paso 3.7 — Panel efectivo de la especificacion B por anio

Script: `3. SCRIPTS/construir_panel_efectivo_especificacion_b_por_anio.R`.
Ventana confirmada en el Paso 2.6: 2015-2019 + 2021-2022 (pre) +
2023-2024 (post), 2020 excluido (9 anios).

### CORRECCION: la tabla "cualquier firma" mezclaba dos poblaciones distintas

La primera version de esta tabla contaba, para cada anio, CUALQUIER
firma con >=2 establecimientos ese anio -- no necesariamente las mismas
262 firmas de la cohorte 2022 (Multi_f). Esto es una poblacion
DISTINTA: mezcla entradas y salidas de firmas distintas cada anio y
oculta la tendencia real de la cohorte fija. Se mantiene como
referencia amplia (`descriptivos_panel_efectivo_especificacion_b_por_anio.csv`)
pero NO es la poblacion relevante para medir el panel efectivo de
`delta_f(e),t`:

| Anio | Firmas multiplanta ese anio (cualquier firma) | Establecimientos |
|---|---|---|
| 2015 | 268 | 894 |
| 2016 | 279 | 925 |
| 2017 | 287 | 959 |
| 2018 | 278 | 933 |
| 2019 | 282 | 929 |
| 2021 | 266 | 857 |
| 2022 | 262 | 851 |
| 2023 | 263 | 858 |
| 2024 | 270 | 880 |

### Version CORRECTA: restringida a las MISMAS 262 firmas de la cohorte 2022

Script: `3. SCRIPTS/construir_panel_efectivo_especificacion_b_por_anio.R`
(seccion 2), salida `descriptivos_panel_efectivo_especificacion_b_cohorte_2022_por_anio.csv`.
De las 262 firmas identificadas como multiplanta EN 2022 (Multi_f
oficial, Paso 3.2), cuantas de ESAS MISMAS 262 tienen >=2 plantas
observadas cada anio de la ventana:

| Anio | Firmas de las 262 con >=2 plantas | Establecimientos aportados | % de las 262 |
|---|---|---|---|
| 2015 | 197 | 739 | 75.2% |
| 2016 | 207 | 771 | 79.0% |
| 2017 | 219 | 812 | 83.6% |
| 2018 | 229 | 828 | 87.4% |
| 2019 | 242 | 843 | 92.4% |
| 2021 | 255 | 835 | 97.3% |
| 2022 | 262 | 851 | 100% (por construccion, es el año base) |
| 2023 | 251 | 833 | 95.8% |
| 2024 | 243 | 813 | 92.8% |

**Patron real: crecimiento SOSTENIDO de 75.2% a 100% entre 2015 y 2022,
luego leve declive a 92.8% en 2024.** Esto es sustancialmente distinto
de la falsa estabilidad (262-287 firmas) que mostraba la tabla de
"cualquier firma" -- esa tabla ocultaba la tendencia real de la cohorte
fija porque mezclaba salidas de unas firmas con entradas de otras
firmas distintas cada anio, compensandose numericamente sin que fuera
la misma poblacion.

### Persistencia de las 262 firmas de 2022 en toda la ventana

| Anios con >=2 plantas (de 9) | N firmas | % |
|---|---|---|
| 9 (todos) | **181** | **69.1%** |
| 8 | 19 | 7.25% |
| 7 | 17 | 6.49% |
| 6 | 11 | 4.20% |
| 5 | 11 | 4.20% |
| 4 | 15 | 5.73% |
| 3 | 8 | 3.05% |

**181 de las 262 firmas multiplanta de 2022 (69.1%) mantienen >=2
plantas en LOS 9 ANIOS de la ventana** -- panel razonablemente
balanceado para la especificacion "dentro de firma": la mayoria de las
firmas identificadas como multiplanta en el anio base no son un
fenomeno transitorio de un solo anio, sino una estructura persistente
a lo largo de todo el periodo de estimacion.

## Paso 3.8 — Descomposicion de las salidas de la cohorte 2022 (2023 y 2024)

Script: `3. SCRIPTS/descomponer_salidas_cohorte_2022.R`. Objetivo:
determinar si la caida post-2022 en la cohorte balanceada (Paso 3.7:
100% en 2022 -> 95.8% en 2023 -> 92.8% en 2024) es churn ordinario o
seleccion inducida por el tratamiento (choque de salario minimo 2023).

### Descomposicion por anio

| Anio | Caen bajo 2 plantas | Atricion (desaparece del panel) | Perdida de planta (sigue en EAM, 1 planta) | Se mantiene (>=2) |
|---|---|---|---|---|
| 2023 | 11 (4.2%) | 7 (2.67%) | 4 (1.53%) | 251 (95.8%) |
| 2024 | 19 (7.3%) | 9 (3.44%) | 10 (3.82%) | 243 (92.8%) |

Atricion y perdida de planta son fenomenos distintos y se reportan por
separado: atricion = la firma deja de reportar a la EAM por completo;
perdida de planta = la firma sigue en la EAM pero se queda con una sola
planta (deja de ser multiplanta sin salir del panel).

### Exposicion 2022 (firma, `Exposure2022_obreros`): salen vs. se mantienen

| Anio | Grupo | N | Media | DE | Diferencia de medias | Error estandar | t | p-valor |
|---|---|---|---|---|---|---|---|---|
| 2023 | Salen (atricion + perdida de planta) | 11 | 0.500 | 0.231 | -0.0651 | 0.0708 | -0.92 | 0.378 |
| 2023 | Se mantienen | 251 | 0.565 | 0.189 | | | | |
| 2024 | Salen | 19 | 0.488 | 0.211 | -0.0804 | 0.0499 | -1.61 | 0.123 |
| 2024 | Se mantienen | 243 | 0.568 | 0.189 | | | | |

### Conclusion del Paso 3.8

Las firmas que salen tienen exposicion 2022 MAS BAJA en promedio (no
mas alta), pero la diferencia NO es estadisticamente significativa en
ninguno de los dos anios (p=0.378 en 2023, p=0.123 en 2024), con
muestras pequeñas (n=11 y n=19) que limitan la potencia estadistica. No
hay evidencia de que el choque de 2023 este expulsando selectivamente a
las firmas mas expuestas -- la caida post-2022 en la cohorte balanceada
es mas consistente con churn ordinario que con seleccion inducida por
el tratamiento, aunque con esta N no se puede descartar con confianza
un efecto moderado (potencia limitada, no ausencia de efecto confirmada).

## Nota metodologica: la rampa 2015->2022 es MECANICA, no consolidacion

La tabla de la cohorte 2022 por anio (Paso 3.7) muestra crecimiento
sostenido de 75.2% (2015) a 100% (2022). **Esto es mecanico por
construccion, no debe interpretarse como consolidacion empresarial
real:** la cohorte de 262 firmas se DEFINE por tener >=2 plantas
especificamente en 2022 (el año base). Hacia atras en el tiempo, solo
las firmas que YA eran multiplanta en cada año anterior pueden cumplir
"tener >=2 plantas ese año" dentro de este conjunto fijo de 262 -- es
una consecuencia aritmetica de como se selecciono la cohorte (survivor-
ship hacia atras dentro de un grupo fijo), no evidencia de que las
firmas se fueran expandiendo a multiples plantas progresivamente. La
unica parte de la serie con contenido informativo real sobre dinamica
de plantas es el tramo POST-2022 (2023-2024), analizado en el Paso 3.8.

## Implicacion para el event study de la especificacion B

Dado el Paso 3.7 (cohorte balanceada: 181 de 262 mantienen >=2 plantas
en los 9 años) y el Paso 3.8 (la caida post-2022 no muestra seleccion
significativa pero N es chica), la especificacion B debe estructurarse
asi:

- **Muestra PRINCIPAL: la cohorte BALANCEADA** -- las 181 firmas que
  mantienen >=2 plantas en LOS 9 AÑOS de la ventana. En un panel no
  balanceado, la composicion de firmas cambia año a año (entran y salen
  distintas firmas), lo que confundiria composicion con tendencia: una
  aparente tendencia en el evento estudio podria ser en realidad un
  cambio en QUIENES estan en la muestra, no un efecto real dentro de
  firma a lo largo del tiempo.
- **Muestra de ROBUSTEZ: la cohorte NO balanceada** (las 262 completas,
  con entradas/salidas segun disponibilidad por año) -- util para
  verificar que los resultados de la muestra balanceada no dependen de
  excluir a las 81 firmas que entran/salen, pero no debe ser la
  especificacion principal por el problema de composicion arriba
  descrito.

## Conclusion general del Paso 2

`DPTO` tiene cobertura perfecta (100%, 17/17 anios) y es estable en el
96.32% de los establecimientos. La inestabilidad restante (3.68%) tiene
un tratamiento aprobado (regla diferenciada 2022/modal). La distribucion
geografica esta concentrada pero no genera celdas `departamento*anio`
vacias o muy pequeñas en la ventana real del panel final (207 celdas,
minimo 13 establecimientos, cero vacias). El unico riesgo de
colinealidad `sector*anio`/`departamento*anio` detectado se limita a
Casanare y Vichada, documentado como limitacion conocida con un
criterio de robustez preparado para cuando se corra la regresion
formal. Paso 2 completo.
