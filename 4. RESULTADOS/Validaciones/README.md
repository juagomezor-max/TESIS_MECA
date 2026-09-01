# Validaciones

Salidas de las validaciones de identificacion (tendencias paralelas 2015-2019; atricion diferencial, antecedente a nivel firma ya corrido -- ver seccion "Antecedente" mas abajo, pendiente adaptar a nivel establecimiento) desarrolladas en la rama `feature/atricion-tendencias-paralelas`.

Todas las validaciones de esta carpeta comparten el mismo panel base: 2015-2019 (pre-choque de salario minimo de 2023), sin excluir firmas/establecimientos atipicos (a diferencia de investigaciones previas de la rama `feature/postpandemia-descartar-2012`, que si excluian un grupo especifico diagnosticado para `Exposure2022_obreros` Q4 -- esa exclusion no se reusa aqui porque no esta validada para las medidas y niveles de agregacion usados en esta carpeta).

## Dimensiones de empleo evaluadas (Y)

Definidas a partir de las columnas C4R de personal ocupado de la EAM (confirmadas estables 2008-2024 en `verificar_estabilidad_columnas_c3r_c4r.R`), excluyendo siempre propietarios/socios sin remuneracion fija (no tienen salario fijo sujeto a la norma de salario minimo):

1. **empleo_total**: obreros + administrativos + profesional-tecnico (fila `empleo_total_categorias` de `construir_conteo_personal_categoria_eam.R`).
2. **empleo_permanente**: fila C4R2 del diccionario DANE ("Personal permanente"), las 3 categorias.
3. **empleo_temporal**: C4R3 (temporal directo) + C4R4 (temporal via agencia), las 3 categorias. No incluye aprendices (C4R6): es una figura contractual distinta (contrato de aprendizaje), ni permanente ni temporal en el sentido de estas dos filas del diccionario.
4. **participacion_permanente**: 100 x empleo_permanente / empleo_total.

## Validaciones incluidas

### 1. `validacion_tendencias_paralelas_empleo_bite.csv`
Script: `3. SCRIPTS/validar_tendencias_paralelas_empleo_bite.R`.
Exposicion: **Bite2022_obreros** (indice de Kaitz: SM_2023 anualizado / salario promedio de obreros en 2022), discretizada en quintiles (misma funcion `make_quintiles` que usa `Exposure2022_obreros`).
Nivel: empresa (NORDEMP). Efectos fijos: NORDEMP (+ CIIU4*anio + DPTO*anio en la especificacion con controles). **Cluster: NORDEMP** (corregido 2026-09-01, ver aviso abajo).
Prueba: F conjunto de `i(quintil_bite2022_obreros, anio_lineal, ref="Q1 - Muy baja")`.

> **⚠ CONCLUSION ANTERIOR CORREGIDA (2026-09-01).** La tabla y la conclusion de esta seccion se recalcularon: la version original del script NO clusterizaba los errores estandar (usaba el default IID de `fixest`), a diferencia de las otras 2 validaciones de esta pagina, que si clusterizan por NORDEMP. La tabla de abajo ya refleja la inferencia CORREGIDA (clusterizada). Comparacion completa IID-vs-cluster, conservada para auditoria: `comparacion_inferencia_iid_vs_cluster_bite_ftest.csv` / `comparacion_inferencia_iid_vs_cluster_bite_coeficientes.csv` (`3. SCRIPTS/comparar_inferencia_iid_vs_cluster_bite.R`). Ver `INDICE_RESULTADOS.md` para el detalle fila por fila.

| Variable | Sin controles (F / p) | Con sector*anio + dpto*anio (F / p) |
|---|---|---|
| empleo_total | 2.42 / 0.047 | 2.31 / 0.056 |
| empleo_permanente | 1.03 / 0.389 | 1.15 / 0.330 |
| empleo_temporal | 1.80 / 0.126 | 1.75 / 0.136 |
| participacion_permanente | **8.04 / 1.9e-6** | **7.71 / 3.5e-6** |

**Con la inferencia correcta (clusterizada), Bite2022_obreros rechaza tendencias paralelas en 1 de 4 dimensiones, no en 3 de 4 como se reporto originalmente.** `empleo_total`, `empleo_permanente` y `empleo_temporal` dejaban de rechazar al 5% (o quedan al borde) una vez clusterizado -- esa parte del hallazgo anterior era un artefacto de inferencia, no una propiedad de la medida. **`participacion_permanente` es la excepcion: sigue rechazando con fuerza** (p=1.9e-6 sin controles, p=3.5e-6 con controles) y **`Exposure2022_obreros` NO rechaza esa misma dimension** (p=0.873, ya clusterizado desde el inicio, ver seccion 3) -- esa divergencia especifica es real, no un artefacto, y sigue siendo un motivo valido para preferir `Exposure2022_obreros` como especificacion principal.

### 2. `evento_tendencias_2015_2019_*.png` + `tabla_evento_tendencias_2015_2019_exposure.csv`
Script: `3. SCRIPTS/validar_tendencias_paralelas_empleo_exposure_grafico.R`.
Exposicion: **Exposure2022_obreros** (participacion de obreros en el empleo total de la firma, continua, winsorizada 1%-99%), escalada a `exposicion_10pp` (por 10 puntos porcentuales) para interpretabilidad.
Nivel: empresa (NORDEMP). Estudio de evento: `i(ANIO_F, exposicion_10pp, ref="2015") | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F`, cluster por NORDEMP.
Prueba: F conjunto de los coeficientes 2016-2019 (todo el panel es pre-choque, asi que estos son 100% chequeos de pre-tendencia).

| Variable | F | p-valor |
|---|---|---|
| empleo_total | 0.77 | 0.546 |
| empleo_permanente | 1.03 | 0.390 |
| empleo_temporal | 1.56 | 0.183 |
| participacion_permanente | 0.31 | 0.873 |

**No rechaza tendencias paralelas en ninguna de las 4 dimensiones.** Contraste directo con Bite2022_obreros (mismo panel, mismos controles): Exposure2022_obreros es la especificacion mas defendible para el diseño DiD de esta tesis.

### 3. `evento_tendencias_establecimiento_2015_2019_*.png` + `tabla_evento_tendencias_establecimiento_2015_2019.csv`
Script: `3. SCRIPTS/validar_tendencias_paralelas_establecimiento.R`.
Exposicion: **Exposure2022_obreros_est**, recalculada a nivel de establecimiento (no reusa el artefacto a nivel empresa) -- relevante porque 336 firmas del panel tienen mas de un establecimiento (NORDEST), y agregarlas a nivel NORDEMP puede mezclar tendencias propias de una planta especifica.
Nivel: establecimiento (NORDEST). Verificado antes de construir: NORDEST-ANIO ya es unico en la macrobase 2015-2019 y 2022 (0 duplicados), no requiere deduplicar.
Estudio de evento: `i(ANIO_F, exposicion_10pp, ref="2015") | NORDEST + CIIU4^ANIO_F + DPTO^ANIO_F`, cluster por NORDEMP (no por NORDEST: varios establecimientos de una misma empresa pueden compartir shocks correlacionados).

| Variable | F | p-valor |
|---|---|---|
| empleo_total | 0.39 | 0.815 |
| empleo_permanente | 0.67 | 0.611 |
| empleo_temporal | 0.58 | 0.676 |
| participacion_permanente | 0.27 | 0.898 |

**No rechaza tendencias paralelas en ninguna dimension, con p-valores aun mas altos que a nivel de empresa.** Panel: 32,516 filas establecimiento-anio, 6,670 establecimientos, 6,137 firmas.

## Conclusion consolidada

`Exposure2022_obreros` (composicion ocupacional) pasa la validacion de tendencias paralelas de forma robusta, tanto a nivel de empresa como de establecimiento, en las 4 dimensiones de empleo. `Bite2022_obreros` (indice de Kaitz), con la inferencia CORREGIDA (clusterizada por NORDEMP, 2026-09-01), no la pasa en 1 de 4 dimensiones (`participacion_permanente`) -- las otras 3 (`empleo_total`, `empleo_permanente`, `empleo_temporal`) que originalmente parecian rechazar eran un artefacto de errores estandar IID, no un problema real de la medida. La divergencia que SI persiste (`participacion_permanente`) sigue siendo un motivo valido para preferir `Exposure2022_obreros` como especificacion principal del DiD, pero el alcance del problema de Bite2022_obreros es mucho mas acotado de lo que se penso originalmente.

## Antecedente: atricion diferencial por quintil de exposicion (nivel FIRMA, ya corrido)

Encontrado el 2026-08-31 al auditar el inventario del repositorio (`INVENTARIO_REPO.md`, rama `feature/panel-establecimiento`): el diagnostico de atricion diferencial YA se corrio el 2026-08-09, en la rama `feature/exposicion-obreros-operarios` (ya fusionada a `main`), antes de que existiera esta carpeta `Validaciones/` -- por eso nunca quedo documentado aqui.

- **Script**: `3. SCRIPTS/diagnostico_atricion_diferencial_exposicion_eam.R`.
- **Commit**: `15105d6`, "Diagnosticar atricion diferencial por quintil de exposicion (2022->2023/2024)", 2026-08-09 17:11:58.
- **Que mide**: para las 6,186 firmas presentes en el panel en 2022 (año base), verifica presencia real en 2023 y en 2024 (no asumida, contra el panel deduplicado), desagregado por quintil de `Exposure2022_obreros` **a nivel FIRMA** (no a nivel establecimiento -- `Exposure2022_obreros_est` no existia todavia el 9 de agosto).
- **Resultado**: diferencia Q5-Q1 = **0.57pp en 2023** y **1.7pp en 2024** -- tasas de salida similares entre quintiles (2.75%/2.43%/2.35%/1.94%/3.32% en 2023, 7.28%/7.52%/5.91%/4.94%/8.98% en 2024, para Q1 a Q5 respectivamente), sin patron monotonico claro por exposicion. **No hay señal de atricion diferencial que amenace la comparacion pre/post 2023**, a nivel firma.
- **Re-corrido el 2026-08-31** (mismo script, sin modificar, insumos identicos a los de agosto): **reproduce exactamente** los valores del commit `15105d6` (0.57pp y 1.7pp) -- sin discrepancia. Salida ahora **versionada**: `atricion_por_quintil_exposicion_eam.csv` (copiada del path no versionado donde el script la escribe por diseño, `1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/`, igual que el resto de los scripts `diagnostico_*`/`auditar_*`/`construir_*` del proyecto).
- **Limitaciones frente a lo que probablemente necesita el trabajo pendiente de esta rama**: nivel firma (no establecimiento), presencia/ausencia binaria (no separa perdida de planta vs. desaparicion completa, a diferencia del Paso 3.8 de `feature/panel-establecimiento`), y no usa `Bite2022_obreros` (no existia aun) ni controles de sector/departamento.

## Extension del diagnostico de atricion diferencial (4 piezas nuevas)

Script: `3. SCRIPTS/extender_diagnostico_atricion_diferencial.R`. Salidas: `atricion_a_tasa_por_quintil_con_se.csv`, `atricion_b_especificacion_continua.csv`, `atricion_c_placebo_2017_2018_2019.csv`, `atricion_c_placebo_especificacion_continua.csv`, `atricion_d_descomposicion_umbral.csv`.

**(a) Tasa por quintil con SE e IC 95%:** brecha Q5-Q1 = 0.57pp en 2023 (IC [-0.78, 1.92], p=0.412) y 1.70pp en 2024 (IC [-0.46, 3.85], p=0.122). No se detecta atricion diferencial atribuible al choque de 2023, pero el IC 95% de 2024 no permite descartar una brecha real de hasta ~3.9pp -- ausencia de significancia no es evidencia de ausencia de efecto (N chica, potencia limitada).

**(b) Especificacion continua (LPM, controles sector+tamaño):** coeficiente **negativo** en ambos años (2024 con controles: -0.00304 por 10pp, p=0.111) -- opuesto al signo del gap Q5-Q1. **No monotonico**: implica que el modelo principal del DiD deberia probar tambien bins/cuantiles de exposicion, no solo tratamiento continuo lineal.

**(c) PLACEBO 2017->2018/2019, doble lectura:**
1. *Como amenaza de seleccion*: DESCARTADA. El patron pre-choque (2019: 3.93pp, IC [1.81, 6.05], p=0.0003) es igual o mayor que el post-choque -- la atricion diferencial, en la magnitud que existe, ya estaba ahi antes de 2023.
2. *Como caracterizacion del tratamiento*: la exposicion cruda SI esta correlacionada con dinamicas de salida preexistentes, explicadas por composicion sectorial/tamaño (desaparece con controles: 2019 sin controles p=0.004, con controles p=0.513). **Mismo patron que las validaciones de tendencias paralelas de este documento**: la identificacion depende de `sector(CIIU4)*anio` + `tamano*anio`, no de la exposicion cruda sola.

**(d) Descomposicion via proxy del umbral de cobertura EAM:** umbral verificado en la ficha metodologica oficial de DANE (10+ personal ocupado O valor de produccion indexado por IPP industrial, base $500M desde 2016). La macrobase no tiene variable de motivo de salida -- proxy usa SOLO la pata de empleo (PERTOTAL<10), NO la pata de produccion (requeriria deflactor IPP, no verificado). 35-47% de las salidas son candidatas a umbral (proporcion similar en real y placebo, tampoco distingue 2023).

## Pendiente

- Adaptar el diagnostico de atricion diferencial a nivel ESTABLECIMIENTO (`Exposure2022_obreros_est`) y/o con `Bite2022_obreros` como exposicion alternativa -- el antecedente a nivel firma y su extension ya estan corridos y versionados arriba, no necesita repetirse, pero no cubre el nivel de analisis de esta rama.
- Verificar la pata de valor de produccion del umbral de cobertura EAM (requiere deflactor IPP industrial indexado desde 2016) para completar la descomposicion (d).
- Investigar la causa de la divergencia de Bite2022_obreros antes de decidir si se descarta como robustez principal o se corrige el diseño.
