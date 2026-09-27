# 3. SCRIPTS — estructura y convenciones

Reorganizado el 2026-09-24 tras perder el orden del repositorio en un cambio de
rama (ver commits de esa fecha para el diagnóstico completo). Primero se
agruparon los 9 scripts en subcarpetas por rol (`construccion/`, `principal/`,
`validacion/`, `descartado/`); luego se aplanaron de vuelta a `3. SCRIPTS/`
porque las subcarpetas dificultaban la navegación, numerando por orden de
ejecución; y por último se renumeraron una segunda vez, ese mismo día, para
que el orden de los números sea el de **lectura de un estudio empírico**
(descriptivos → medidas de exposición → análisis → validaciones →
validaciones adicionales) en vez del orden en que se fue descubriendo el
problema.

**El orden de lectura ya no coincide con el orden de ejecución** — ver la
sección dedicada más abajo antes de correr nada desde cero.

## Convención de numeración

Dos dígitos, correlativos, en el orden en que se **lee** el pipeline como
estudio empírico: `01`-`08` para el pipeline vivo, `09`-`10` para el
diagnóstico de validez de la medida de exposición principal (no son parte de
la estimación: auditan el pipeline desde afuera, sin modificarlo ni volver a
correrlo — ver la sección dedicada más abajo), `xx` reservado para material
histórico que **no se corre** (queda al final de cualquier listado alfabético
y el nombre lo deja explícito), y `descartado/` para versiones superadas de
los scripts de diagnóstico (ver `descartado/README.md`). Dos dígitos y no uno
solo porque con nueve archivos un `10_` se ordenaría antes que un `2_` en
cualquier listado alfabético, rompiendo el orden visual.

Convención para sesiones futuras: un script nuevo se inserta donde
corresponda por lectura, no al final — si eso obliga a renumerar vecinos, se
hace con `git mv` en dos pasadas (primero a nombres temporales, luego a los
definitivos) para no pisar archivos al intercambiar números. El histórico
permanece en `xx` en adelante si llega a acumularse más de uno.

## Inventario

| Script | Líneas | Sección del estudio | Lee | Escribe | Carpeta de salida |
|---|---|---|---|---|---|
| `01_descriptivos_y_contexto.R` | 1076 | Descriptivos y resultados principales | `panel_analitico_firma_eam.rds`, `panel_establecimiento_formal.rds` | — | `Descriptivos`, `Principal`, `Estimacion`, `Validaciones`, `Robustez` |
| `02_medidas_exposicion.R` | 753 | Construcción de las medidas de exposición | `panel_firma_eam_expalt_completo.rds`, y condicionalmente `panel_analitico_firma_eam.rds` (comparación contra el Kaitz actual, sección 7 — si el archivo no existe, sigue sin esa comparación) | **`1. DATOS/exposicion_alternativa_2022.rds`** | `Exposicion_alternativa` |
| `03_primer_eslabon_medidas.R` | 665 | Primera etapa con las cinco medidas | los tres paneles | — | `Primer_eslabon` |
| `04_decision_medida.R` | 665 | Elección de la medida principal | los tres paneles | — | `Decision_medida` |
| `05_resultados_y_mecanismos.R` | 870 | Análisis principal y mecanismos | los tres paneles | — | `Resultados_mecanismos` |
| `06_mecanismos_por_grupo.R` | 622 | Mecanismos por tamaño de firma | `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds` | — | `Mecanismos_por_grupo` |
| `07_reconciliacion.R` | 534 | Validaciones (las tres lecturas) | `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds` | — | `Reconciliacion` |
| `08_tratamiento_continuo.R` | 1183 | Validaciones adicionales (forma funcional) | `panel_analitico_firma_eam.rds` | — | `Continuo` |
| `09_validez_exposicion.R` | — | Diagnóstico: validez de la medida de exposición (Bite) | `panel_firma_eam_expalt_completo.rds` | — | `Validez_exposicion` |
| `10_bug_controles.R` | 328 | Diagnóstico: descomposición del bug de controles fijos en 2022 (controles vs. muestra) | `panel_firma_eam_expalt_completo.rds`, `exposicion_alternativa_2022.rds`, `panel_analitico_firma_eam.rds` | — | `Descomposicion_bug` |
| `xx_script_base_historico.R` | 829 | Histórico, **NO correr** | `panel_analitico_firma_eam.rds`, `panel_establecimiento_formal.rds` | — | `Descriptivos`, `Estimacion`, `Robustez`, `Validaciones` |

"Los tres paneles" = `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds`, `1. DATOS/exposicion_alternativa_2022.rds`.

**Notas sobre el inventario** (correcciones encontradas al verificarlo contra
el repositorio real, 2026-09-24):
- `06_mecanismos_por_grupo.R` tiene 622 líneas, no 665 — la cifra original
  quedó desactualizada. El script termina con su bloque de cierre completo, no
  está truncado.
- `02_medidas_exposicion.R` también lee `panel_analitico_firma_eam.rds`
  (lectura condicional), no solo el panel ampliado — no estaba en el
  inventario original.
- `03_primer_eslabon_medidas.R` y `04_decision_medida.R` coinciden en 665
  líneas cada uno por pura coincidencia — son 948 líneas distintas entre sí
  (verificado con diff), temas claramente distintos. No es duplicación.

**Dos scripts alimentan más de una sección de la tesis y no se partieron:**
- `01_descriptivos_y_contexto.R` produce tanto los descriptivos (capítulo 1)
  como los resultados principales (capítulo 5) — qué secciones del script
  corresponden a cada capítulo está documentado en su propio encabezado
  ("LUGAR EN LA TESIS").
- `02_medidas_exposicion.R`, además de construir las medidas (capítulo 2),
  produce en su sección 8 el descriptivo del salario por categoría
  ocupacional (obrero permanente típico ~1,44 salarios mínimos), que es
  material del capítulo 1, no del 2 — también documentado en su encabezado
  (bloque "OJO").

Ver `NARRATIVA.md` para el mapa completo capítulo-por-capítulo.

## Los dos paneles — no son intercambiables

Conviven dos paneles de firma distintos:

- **`panel_analitico_firma_eam.rds`** (el original): usado por
  `01_descriptivos_y_contexto.R` y `08_tratamiento_continuo.R`.
- **`panel_firma_eam_expalt_completo.rds`** (el ampliado): incluye 2020 y
  variables desagregadas por categoría ocupacional (obreros/profesional-técnico/
  administrativos). Usado por el resto de los scripts.

**Los resultados de unos y otros no son directamente comparables** — distinta
ventana temporal y distinta granularidad de variables. Si una tabla de un
script no cuadra con la de otro, lo primero que hay que revisar es cuál de los
dos paneles usa cada uno.

## Orden de lectura vs. orden de ejecución

**Estos dos órdenes ya NO coinciden** desde la renumeración por flujo de
estudio empírico. Confundirlos hace que alguien corra `01` esperando que `02`
ya esté hecho, cuando es al revés.

- **Orden de lectura** (el que indica el número, pensado para quien abre la
  carpeta y quiere entender el estudio de principio a fin): `01`, `02`, `03`,
  `04`, `05`, `06`, `07`, `08`.
- **Orden de ejecución** (para correr el pipeline desde cero): **`02` primero**
  — escribe `1. DATOS/exposicion_alternativa_2022.rds`, que leen `03`, `04` y
  `05`. Los demás (`01`, `06`, `07`, `08`) son terminales (no producen insumos
  que otro script lea) y se pueden correr en cualquier orden, antes o después
  de `02`, siempre que `03`, `04` y `05` corran después de `02`.

`xx_script_base_historico.R` **no debe correrse** en ningún orden: su carpeta
de salida colisiona con la de `01_descriptivos_y_contexto.R` (ambos escriben
en `Descriptivos`, `Estimacion`, `Robustez`, `Validaciones`) — es el script
manual inicial, superado por `01_descriptivos_y_contexto.R`. Se conserva
únicamente como registro histórico: su encabezado documenta 9 correcciones
metodológicas (controles fijados en 2022, muestra común entre medidas, escala
comparable, winsorización sobre el corte transversal, pre-tendencias sobre
toda la ventana, primer eslabón, tendencia estimada solo en el pre-período,
escrutinio automático por regla) que pueden servir de referencia para la
sección de metodología de la tesis.

## Diagnóstico de validez de la medida de exposición (09-10)

`09_validez_exposicion.R` y `10_bug_controles.R` no son parte de la
estimación: auditan desde afuera si `Bite2022_obreros` (la medida elegida en
`04_decision_medida.R`) mide de verdad exposición al salario mínimo o si el
traslape aritmético entre el denominador de la exposición y la base del
outcome contamina el resultado. Se corren aparte, no modifican ni vuelven a
correr el pipeline `01`-`08`, y sus cifras no reemplazan a las del glosario de
`NARRATIVA.md` — lo complementan con el análisis de robustez.

- **`09_validez_exposicion.R`** consolida en una sola carpeta de salida
  (`4. RESULTADOS/Validez_exposicion/`) cuatro pruebas que antes vivían en
  scripts separados: placebo rodante con aumentos reales del mínimo, celda
  limpia con la indexación del rezago corregida, sensibilidad del outcome de
  2023 al aumento de 2022, y una auditoría adversarial de cinco afirmaciones
  (buscando activamente dónde el diagnóstico se equivoca). Su última sección
  (síntesis) resume qué resiste, qué queda falsado y qué queda pendiente.
- **`10_bug_controles.R`** (antes `16_descomposicion_bug.R`) descompone si la
  mejora observada al corregir el bug de controles fijos en 2022 (ver más
  abajo) viene de los controles o de la restricción de muestra que trae esa
  corrección. Resultado: 100% controles, 0% muestra.
- **`descartado/`** guarda las versiones superadas de estas pruebas
  (`09_diagnostico_reversion.R`, `10_celda_limpia_rodante.R`,
  `12_celda_limpia_real.R`) con su historial de git intacto — no se corren;
  `descartado/README.md` explica qué probaba cada una y por qué quedó
  superada (orden por aumento nominal en vez de real, bug de indexación del
  rezago).

**Bug de controles fijos en 2022** (encontrado en esta ronda de diagnóstico,
ya corregido en `05_resultados_y_mecanismos.R`, `06_mecanismos_por_grupo.R` y
`07_reconciliacion.R`): `sector_2022`/`depto_2022`/`tamano_2022` se
recalculaban con el CIIU4/DPTO/tamaño de cada fila en su propio año, pese al
nombre — no estaban fijos en 2022. Para ventanas de outcome de más de un año,
eso fragmentaba a las firmas que cambiaron de clasificación entre años. El
detalle completo, con las cifras exactas de cuántas firmas-año estaban
afectadas, está en la sección 4.1 de `09_validez_exposicion.R`.

## Ramas históricas

`git fsck` encontró 2 commits de `stash` que ya no pertenecían a ninguna
rama (de antes del reinicio del repositorio del 13-sep-2026, contra un `main`
que ya no existe). Se conservan como ramas de solo consulta:

- `historico_wip_2026-09-12` (`bc38774`)
- `historico_wip_2026-09-05` (`dbac233`)

**No son trabajo vivo y no deben fusionarse.** Su único uso es poder consultar
qué se estaba probando en esas fechas si surge una duda puntual.
