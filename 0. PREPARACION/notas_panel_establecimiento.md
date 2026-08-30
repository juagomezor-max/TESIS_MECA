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
