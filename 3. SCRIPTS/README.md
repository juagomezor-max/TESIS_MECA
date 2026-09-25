# 3. SCRIPTS — estructura y convenciones

Reorganizado el 2026-09-24 tras perder el orden del repositorio en un cambio de
rama (ver commits de esa fecha para el diagnóstico completo). El número de
cada script ahora codifica su **rol**, no el orden en que se creó, para que dos
scripts nunca vuelvan a colisionar en el mismo prefijo — eso fue justo lo que
pasó con los dos `03_` de esta reorganización.

## Estructura de carpetas

```
3. SCRIPTS/
├── construccion/   construye bases, no estima nada
├── principal/       10, 11, 12...  resultados que van en la tesis
├── validacion/       20, 21, 22, 23...  robustez y diagnósticos
└── descartado/       histórico, no se corre
```

- **`construccion/`**: hoy solo `exposicion_alternativa.R` (sin número — es el
  único script de esta carpeta y no hay riesgo de colisión ahí). Construye
  `exposicion_alternativa_2022.rds`, no produce resultados de tesis.
- **`principal/`**: numeración desde 10. Lo que efectivamente va en la tesis.
- **`validacion/`**: numeración desde 20. Robustez, decisión de medida,
  reconciliación, diagnósticos.
- **`descartado/`**: scripts superados, se conservan como registro histórico de
  decisiones metodológicas. **No correr.**

Convención para sesiones futuras: al agregar un script nuevo, seguir la
siguiente decena disponible en `principal/` o `validacion/` (13, 14... / 24,
25...) — nunca reutilizar un número, y nunca empezar un nombre de archivo con
un prefijo que ya exista en otra carpeta.

## Inventario

| Script | Líneas | Lee | Escribe | Carpeta de salida |
|---|---|---|---|---|
| `descartado/00_script_base.R` | 829 | `panel_analitico_firma_eam.rds`, `panel_establecimiento_formal.rds` | — | `Descriptivos`, `Estimacion`, `Robustez`, `Validaciones` |
| `principal/10_resultados_poster.R` | 1076 | `panel_analitico_firma_eam.rds`, `panel_establecimiento_formal.rds` | — | `Descriptivos`, `Principal`, `Estimacion`, `Validaciones`, `Robustez` |
| `principal/11_resultados_y_mecanismos.R` | 870 | los tres paneles | — | `Resultados_mecanismos` |
| `principal/12_mecanismos_por_grupo.R` | 622 | `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds` | — | `Mecanismos_por_grupo` |
| `construccion/exposicion_alternativa.R` | 753 | `panel_firma_eam_expalt_completo.rds`, y condicionalmente `panel_analitico_firma_eam.rds` (comparación contra el Kaitz actual, sección 7 — si el archivo no existe, sigue sin esa comparación) | **`1. DATOS/exposicion_alternativa_2022.rds`** | `Exposicion_alternativa` |
| `validacion/20_primer_eslabon_medidas.R` | 665 | los tres paneles | — | `Primer_eslabon` |
| `validacion/21_decision_medida.R` | 665 | los tres paneles | — | `Decision_medida` |
| `validacion/22_reconciliacion.R` | 534 | `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds` | — | `Reconciliacion` |
| `validacion/23_tratamiento_continuo.R` | 1183 | `panel_analitico_firma_eam.rds` | — | `Continuo` |

"Los tres paneles" = `panel_analitico_firma_eam.rds`, `panel_firma_eam_expalt_completo.rds`, `1. DATOS/exposicion_alternativa_2022.rds`.

**Notas sobre el inventario** (correcciones encontradas al verificarlo contra
el repositorio real, 2026-09-24):
- `12_mecanismos_por_grupo.R` tiene 622 líneas, no 665 — la cifra original
  quedó desactualizada. El script termina con su bloque de cierre completo, no
  está truncado.
- `construccion/exposicion_alternativa.R` también lee
  `panel_analitico_firma_eam.rds` (lectura condicional), no solo el panel
  ampliado — no estaba en el inventario original.
- `20_primer_eslabon_medidas.R` y `21_decision_medida.R` coinciden en 665
  líneas cada uno por pura coincidencia — son 948 líneas distintas entre sí
  (verificado con diff), temas claramente distintos. No es duplicación.

## Los dos paneles — no son intercambiables

Conviven dos paneles de firma distintos:

- **`panel_analitico_firma_eam.rds`** (el original): usado por
  `principal/10_resultados_poster.R` y `validacion/23_tratamiento_continuo.R`.
- **`panel_firma_eam_expalt_completo.rds`** (el ampliado): incluye 2020 y
  variables desagregadas por categoría ocupacional (obreros/profesional-técnico/
  administrativos). Usado por el resto de los scripts.

**Los resultados de unos y otros no son directamente comparables** — distinta
ventana temporal y distinta granularidad de variables. Si una tabla de
`principal/` no cuadra con una de `validacion/`, lo primero que hay que
revisar es cuál de los dos paneles usa cada una.

## Orden de ejecución

Única dependencia real del repositorio: `construccion/exposicion_alternativa.R`
escribe `1. DATOS/exposicion_alternativa_2022.rds`, que leen
`validacion/20_primer_eslabon_medidas.R`, `validacion/21_decision_medida.R` y
`principal/11_resultados_y_mecanismos.R`. Correr `construccion/` primero.

El resto de los scripts son terminales (no producen insumos que otro script
lea) y se pueden correr en cualquier orden entre sí.

`descartado/00_script_base.R` **no debe correrse**: su carpeta de salida
colisiona con la de `principal/10_resultados_poster.R` (ambos escriben en
`Descriptivos`, `Estimacion`, `Robustez`, `Validaciones`) — es el script manual
inicial, superado por `10_resultados_poster.R`. Se conserva únicamente como
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
