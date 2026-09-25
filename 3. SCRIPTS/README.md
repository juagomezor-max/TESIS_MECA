# 3. SCRIPTS — estructura y convenciones

Reorganizado el 2026-09-24 tras perder el orden del repositorio en un cambio de
rama (ver commits de esa fecha para el diagnóstico completo). Primero se
agruparon los 9 scripts en subcarpetas por rol (`construccion/`, `principal/`,
`validacion/`, `descartado/`); ese mismo día se aplanaron de vuelta a
`3. SCRIPTS/` porque las subcarpetas dificultaban la navegación, y se
renumeraron de forma correlativa siguiendo el orden real de ejecución.

## Convención de numeración

Dos dígitos, correlativos, en el orden en que hay que correr los scripts:
`01`-`08` para el pipeline vivo, `99` reservado para material histórico que
**no se corre** (queda al final de cualquier listado alfabético y el nombre lo
deja explícito). Dos dígitos y no uno solo porque con nueve archivos un `10_`
se ordenaría antes que un `2_` en cualquier listado alfabético, rompiendo el
orden visual.

Convención para sesiones futuras: un script nuevo que se inserte en el
pipeline va después del último número vivo (`09`, `10`...) — nunca se
reutiliza un número ya asignado, y el histórico permanece en `99` en adelante
si llega a acumularse más de uno.

## Inventario

| Script | Líneas | Rol | Lee | Escribe | Carpeta de salida |
|---|---|---|---|---|---|
| `01_exposicion_alternativa.R` | 753 | Construye las medidas de exposición | `panel_firma_eam_expalt_completo.rds`, y condicionalmente `panel_analitico_firma_eam.rds` (comparación contra el Kaitz actual, sección 7 — si el archivo no existe, sigue sin esa comparación) | **`1. DATOS/exposicion_alternativa_2022.rds`** | `Exposicion_alternativa` |
| `02_resultados_poster.R` | 1076 | Descriptivos y resultados principales | `panel_analitico_firma_eam.rds`, `panel_establecimiento_formal.rds` | — | `Descriptivos`, `Principal`, `Estimacion`, `Validaciones`, `Robustez` |
| `03_resultados_y_mecanismos.R` | 870 | Resultados y mecanismos | los tres paneles | — | `Resultados_mecanismos` |
| `04_mecanismos_por_grupo.R` | 622 | Mecanismos por tamaño | `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds` | — | `Mecanismos_por_grupo` |
| `05_primer_eslabon_medidas.R` | 665 | Primera etapa, cinco medidas | los tres paneles | — | `Primer_eslabon` |
| `06_decision_medida.R` | 665 | Decisión de medida | los tres paneles | — | `Decision_medida` |
| `07_reconciliacion.R` | 534 | Las tres lecturas | `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds` | — | `Reconciliacion` |
| `08_tratamiento_continuo.R` | 1183 | Forma funcional | `panel_analitico_firma_eam.rds` | — | `Continuo` |
| `99_script_base_historico.R` | 829 | Histórico, **NO correr** | `panel_analitico_firma_eam.rds`, `panel_establecimiento_formal.rds` | — | `Descriptivos`, `Estimacion`, `Robustez`, `Validaciones` |

"Los tres paneles" = `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds`, `1. DATOS/exposicion_alternativa_2022.rds`.

**Notas sobre el inventario** (correcciones encontradas al verificarlo contra
el repositorio real, 2026-09-24):
- `04_mecanismos_por_grupo.R` tiene 622 líneas, no 665 — la cifra original
  quedó desactualizada. El script termina con su bloque de cierre completo, no
  está truncado.
- `01_exposicion_alternativa.R` también lee `panel_analitico_firma_eam.rds`
  (lectura condicional), no solo el panel ampliado — no estaba en el
  inventario original.
- `05_primer_eslabon_medidas.R` y `06_decision_medida.R` coinciden en 665
  líneas cada uno por pura coincidencia — son 948 líneas distintas entre sí
  (verificado con diff), temas claramente distintos. No es duplicación.

## Los dos paneles — no son intercambiables

Conviven dos paneles de firma distintos:

- **`panel_analitico_firma_eam.rds`** (el original): usado por
  `02_resultados_poster.R` y `08_tratamiento_continuo.R`.
- **`panel_firma_eam_expalt_completo.rds`** (el ampliado): incluye 2020 y
  variables desagregadas por categoría ocupacional (obreros/profesional-técnico/
  administrativos). Usado por el resto de los scripts.

**Los resultados de unos y otros no son directamente comparables** — distinta
ventana temporal y distinta granularidad de variables. Si una tabla de un
script no cuadra con la de otro, lo primero que hay que revisar es cuál de los
dos paneles usa cada uno.

## Orden de ejecución

Única dependencia real del repositorio: `01_exposicion_alternativa.R` escribe
`1. DATOS/exposicion_alternativa_2022.rds`, que leen `03_resultados_y_mecanismos.R`,
`05_primer_eslabon_medidas.R` y `06_decision_medida.R`. Correr `01` primero.

El resto de los scripts (`02`, `04`, `07`, `08`) son terminales (no producen
insumos que otro script lea) y se pueden correr en cualquier orden entre sí,
después de `01`.

`99_script_base_historico.R` **no debe correrse**: su carpeta de salida
colisiona con la de `02_resultados_poster.R` (ambos escriben en
`Descriptivos`, `Estimacion`, `Robustez`, `Validaciones`) — es el script manual
inicial, superado por `02_resultados_poster.R`. Se conserva únicamente como
registro histórico: su encabezado documenta 9 correcciones metodológicas
(controles fijados en 2022, muestra común entre medidas, escala comparable,
winsorización sobre el corte transversal, pre-tendencias sobre toda la
ventana, primer eslabón, tendencia estimada solo en el pre-período, escrutinio
automático por regla) que pueden servir de referencia para la sección de
metodología de la tesis.

## Ramas históricas

`git fsck` encontró 2 commits de `stash` que ya no pertenecían a ninguna
rama (de antes del reinicio del repositorio del 13-sep-2026, contra un `main`
que ya no existe). Se conservan como ramas de solo consulta:

- `historico_wip_2026-09-12` (`bc38774`)
- `historico_wip_2026-09-05` (`dbac233`)

**No son trabajo vivo y no deben fusionarse.** Su único uso es poder consultar
qué se estaba probando en esas fechas si surge una duda puntual.
