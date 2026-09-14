# Pipeline simplificado — rama `simplificacion`

El repositorio acumuló 48 scripts y 84 salidas a lo largo de varias ramas para validar el diseño de la tesis (identificador de panel, medida de exposición, controles, ventanas temporales). Este pipeline consolida esa validación en **5 scripts numerados + `run_all.R`**, a nivel de **FIRMA** (núcleo mínimo), más **1 módulo opcional** a nivel de establecimiento.

**Regla seguida en toda la construcción: derivar, no reescribir.** Cada script nuevo copia/adapta fórmulas y patrones ya validados en `main` (citados explícitamente en la cabecera de cada script), nunca reimplementados de memoria. El control de calidad (`CIFRAS_CLAVE.csv`) confirmó esto: **33/33 cifras coinciden** con los valores ya reportados en `main` (y en `feature/estimacion-preliminar` para la corrección de clustering de Bite, ver más abajo).

## Qué hace cada script

| Script | Qué hace | Derivado de (en `main`) |
|---|---|---|
| `01_construir_base.R` | Carga la macrobase EAM y aplica la deduplicación NORDEMP-año (regla ya auditada) | `construir_conteo_personal_categoria_eam.R`, auditoría de `auditar_deduplicacion_nordemp_anio.R` |
| `02_construir_exposicion.R` | Construye `Exposure2022_obreros` y `Bite2022_obreros` a nivel firma, con winsorización y quintiles | `construir_conteo_personal_categoria_eam.R`, `construir_exposicion_obreros_eam.R`, `construir_salarios_promedio_categoria_eam.R`, `construir_bite_obreros_eam.R` |
| `03_construir_panel.R` | Ensambla el panel analítico firma-año: ventana 2015-2019+2021-2022+2023-2024 (2020 excluido), 4 variables de resultado, controles sector/tamaño | `validar_tendencias_paralelas_empleo_exposure_grafico.R`, `investigar_validez_test_pretendencias.R` (umbrales de tamaño) |
| `04_validaciones.R` | Tendencias paralelas 2015-2019 (Exposure y Bite) y atrición diferencial con placebo — **`cluster = ~NORDEMP` siempre, sin excepción** | `validar_tendencias_paralelas_empleo_exposure_grafico.R`, `validar_tendencias_paralelas_empleo_bite.R` **(versión corregida, ver aviso abajo)**, `diagnostico_atricion_diferencial_exposicion_eam.R`, `extender_diagnostico_atricion_diferencial.R` (partes a y c) |
| `05_descriptivos.R` | Denominadores a nivel firma (firmas del panel 2022, firmas con exposición válida) | `descriptivos_estructura_multiplanta_2022_parte2.R`, `comparar_distribucion_exposure_establecimiento_vs_firma.R` |
| `run_all.R` | Corre 01→05 en orden, de cero a resultados | — |
| `opcional_establecimiento.R` | Módulo opcional (NO llamado por `run_all.R`): panel NORDEST 2008-2024, `Exposure2022_obreros_est`, estructura multiplanta 2022, cohorte balanceada | `construir_conteo_personal_categoria_establecimiento_eam.R`, `construir_exposicion_obreros_establecimiento_eam.R`, `descriptivos_estructura_multiplanta_2022.R` (+ `_parte2`), `calcular_peso_empleo_variacion_interna_2022.R`, `construir_panel_efectivo_especificacion_b_por_anio.R`, `verificar_consistencia_cruzada_multiplanta_2022.R` |
| `verificar_cifras_clave.R` | Control de calidad: compara cada cifra del pipeline nuevo contra el valor ya reportado en `main`/`feature/estimacion-preliminar`, produce `CIFRAS_CLAVE.csv` | — (script nuevo, sin equivalente en `main`) |

## Cómo correrlo de cero

```r
# 1. Confirmar que existe 1. DATOS/5. MACROBASE/macro_base_eam.rds
#    (si no existe, correr primero 3. SCRIPTS/construir_macro_base_eam.R,
#    fuera del alcance de este pipeline -- ver nota en 01_construir_base.R).
source("3. SCRIPTS/run_all.R")              # 01 a 05, nucleo minimo (nivel firma)
source("3. SCRIPTS/opcional_establecimiento.R")  # modulo opcional (nivel establecimiento)
source("3. SCRIPTS/verificar_cifras_clave.R")    # QA: compara contra main
```

Salidas en `4. RESULTADOS/Validaciones/simplificado_*.csv` y `CIFRAS_CLAVE.csv`.

## Qué quedó fuera del núcleo mínimo, y por qué

- **Deflactores / variables en términos reales**: ningún script ya validado en `main` construye o usa una serie de deflactor IPP/IPC citable. La única referencia existe en el script exploratorio del compañero (`3. SCRIPTS/3. SCRIPTS/construir_base analitica.R`), nunca validado ni usado en ninguna cifra reportada. Siguiendo la regla "derivar, no reescribir", no se reimplementó esa lógica de memoria. Si se necesita en el futuro, debe construirse y validarse como un paso aparte, coordinado con el compañero.
- **Especificación continua de atrición (parte b de `extender_diagnostico_atricion_diferencial.R`) y descomposición por umbral de cobertura (parte d)**: no sustentan ninguna cifra de `CIFRAS_CLAVE.csv`. El pipeline mínimo cubre exactamente lo que se verifica (partes a y c: brecha Q5-Q1 con error estándar, y el placebo). El resto sigue disponible en el historial (`feature/estimacion-preliminar`, tag `panel-establecimiento-v1`).
- **Nivel de establecimiento como núcleo**: el núcleo mínimo es a nivel de FIRMA. El nivel de establecimiento (`Exposure2022_obreros_est`, estructura multiplanta, cohorte balanceada) es una extensión real pero no es indispensable para el diseño DiD principal — queda como módulo opcional, no en la ruta que corre `run_all.R`.
- **Validaciones a nivel establecimiento de tendencias paralelas** (`validar_tendencias_paralelas_establecimiento.R`): no se reprodujeron en este pipeline simplificado porque el núcleo mínimo es a nivel firma; si se necesitan, están documentadas en `main` y en `INDICE_RESULTADOS.md`.

## ⚠ Aviso importante: la validación de Bite2022_obreros usa una corrección que NO está en `main` todavía

El script `04_validaciones.R` deriva la validación de tendencias paralelas de `Bite2022_obreros` de la versión **corregida** (clusterizada por `NORDEMP`) de `validar_tendencias_paralelas_empleo_bite.R`, que vive en `feature/estimacion-preliminar` (commit `397e349`), **no en `main`**, que todavía tiene la versión anterior con errores estándar IID. Esa corrección fue necesaria: la versión IID producía una conclusión errónea ("Bite rechaza tendencias paralelas en 3 de 4 dimensiones") que se retractó tras clusterizar correctamente (queda en 1 de 4 dimensiones). `CIFRAS_CLAVE.csv` usa los valores corregidos como "esperados" para las filas de Bite, siguiendo instrucción explícita. Esto significa que **`feature/estimacion-preliminar` debería fusionarse a `main` en algún momento** para que esta rama de simplificación no dependa de una rama sin fusionar — queda como decisión pendiente, no se fusionó aquí sin instrucción explícita.

## Limitación conocida

La cifra "correlación planta-firma = 0.964" (fila 33 de `CIFRAS_CLAVE.csv`) nunca se guardó como archivo en `main` — solo se imprimió en la consola de `construir_exposicion_obreros_establecimiento_eam.R` en su momento. Se usó como valor esperado citando esa consola como fuente, no un CSV. `opcional_establecimiento.R` sí la deja guardada en `simplificado_establecimiento_correlacion_planta_firma.csv`, corrigiendo esa laguna de documentación hacia adelante.

## Bug encontrado y corregido durante la derivación (documentado, no oculto)

Al derivar `Bite2022_obreros` en `02_construir_exposicion.R`, la primera versión usó `C3R2C1` (masa salarial total de obreros permanentes) directamente como "salario promedio", en vez de dividirlo por el conteo de personal permanente (`C4R2C1+C4R2C2`), como hace `construir_salarios_promedio_categoria_eam.R`. Esto producía una correlación Exposure-Bite de signo y magnitud equivocados (-0.087 en vez de 0.124). Se detectó exactamente por el control `CIFRAS_CLAVE.csv` (esa es su función), se corrigió derivando la fórmula exacta del script original, y se verificó que el resultado coincide (n=5,099, Pearson=0.124, Spearman=0.159).
