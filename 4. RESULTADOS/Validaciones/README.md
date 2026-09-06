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

## Matriz de comparacion funcional (2x2) y contraste grafico de la hipotesis (2026-09-02)

La "Conclusion consolidada" de arriba compara Exposure (continua, event-study anio-a-anio) contra Bite (quintiles, tendencia lineal) -- forma funcional y estadistico de prueba DISTINTOS. Un F conjunto sobre dummies de anio tiene mas potencia contra desviaciones no lineales; un test de tendencia lineal tiene mas potencia contra desviaciones monotonicas. `comparar_matriz_funcional_exposure_bite.R` corrio las 2 celdas que faltaban (Exposure en quintiles+lineal, Bite en event-study), mismos FE y cluster (`NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F`, `cluster=~NORDEMP`), mismo panel 2015-2019 -- tabla completa en `matriz_comparacion_funcional_exposure_bite.csv`.

| Medida | Forma funcional | F (participacion_permanente) | p |
|---|---|---|---|
| Exposure2022_obreros | continua, event-study | 0.308 | 0.873 |
| Exposure2022_obreros | quintiles, tendencia lineal | 1.52 | 0.193 |
| Bite2022_obreros | continua, event-study | 2.35 | 0.052 |
| Bite2022_obreros | quintiles, tendencia lineal | 7.71 | 0.0000035 |

**Interpretacion:**

- Bajo el MISMO test, Bite es sistematicamente mas debil que Exposure en `participacion_permanente` (0.052 vs 0.873 en event-study; 3.5e-6 vs 0.193 en tendencia lineal). La divergencia es real y consistente en direccion -- no depende de que test se use.
- El test de tendencia lineal es el apropiado cuando la desviacion pre-choque es monotonica, y el F sobre dummies de anio pierde potencia contra esa forma especifica. El patron observado (Bite rechaza mucho mas fuerte en tendencia lineal que en event-study) es el esperado ante una tendencia previa MONOTONICA en Bite, no evidencia de que el hallazgo original fuera un artefacto de eleccion de test.
- **Implicacion: `Bite2022_obreros` no es utilizable como especificacion para `participacion_permanente`.**
- Un p de 0.052 no es un aprobado: el 5% es una convencion, no una frontera con contenido economico. Bite tampoco pasa el chequeo de tendencias paralelas en `participacion_permanente` bajo event-study.

### Contraste grafico de la hipotesis de tendencia monotonica

Si la razon de que Bite rechace con tanta fuerza en tendencia lineal es que su desviacion pre-choque 2015-2019 es monotonica (mientras que la de Exposure no lo es), eso deberia verse en las medias de `participacion_permanente` por quintil y anio. Grafico: `participacion_permanente_por_quintil_bite_vs_exposure.png`; tabla: `participacion_permanente_por_quintil_bite_vs_exposure.csv`.

**Evidencia encontrada (descriptiva, no un test estadistico -- consistente o inconsistente con la hipotesis, no confirmacion):**

- **Bite**: la brecha Q5-Q1 en `participacion_permanente` CRECE de forma monotonica cada anio, 2015-2019: 9.35pp -> 9.79pp -> 10.88pp -> 13.00pp -> 14.79pp (crece en los 4 pasos, sin reversiones). El quintil Q5 (mas expuesto) sube de forma monotonica los 5 anios (75.11% -> 75.80% -> 76.25% -> 77.95% -> 78.92%). En 2019, el orden de los 5 quintiles es perfectamente monotonico (Q1=64.14 < Q2=70.59 < Q3=75.52 < Q4=78.57 < Q5=78.92).
- **Exposure**: la brecha Q5-Q1 es mas plana y NO crece de forma monotonica: -10.27pp -> -10.52pp -> -10.20pp -> -11.11pp -> -11.96pp (se angosta entre 2016 y 2017 antes de volver a ensancharse). Los quintiles individuales se mueven poco (rango total <1.5pp en Q5 a lo largo de los 5 anios, contra >3.8pp en el Q5 de Bite). En 2019, el orden de los 5 quintiles NO es monotonico (Q1=71.20 > Q2=67.32 < Q3=67.83 > Q4=63.92 > Q5=59.24 -- Q3 rompe el patron).
- **Lectura**: el patron observado es CONSISTENTE con la hipotesis -- Bite muestra una divergencia pre-choque mayor en magnitud y mas monotonica en forma que Exposure, en la misma direccion que predice la hipotesis. No es una confirmacion causal (son medias descriptivas de una sola muestra, sin prueba formal de la forma funcional de la tendencia en si), pero el patron visual no contradice la explicacion de que Bite2022_obreros tiene una tendencia previa monotonica que Exposure2022_obreros no tiene.

## Pre-tendencias sobre el panel formal a nivel establecimiento (2026-09-02)

Script: `3. SCRIPTS/validaciones/validar_pretendencias_panel_formal.R`. Corre sobre el **panel formal** reconstruido en `3. SCRIPTS/construccion/construir_panel_establecimiento_formal.R` (NORDEST-ANIO, 2015-2019+2021-2024, DPTO fijo por establecimiento, reconstruido desde cero SIN copiar el tag `archivo/panel-formal` -- ver `README.md` raiz, seccion Historial) -- **no es el mismo panel** que usa la seccion 3 de arriba (`validar_tendencias_paralelas_establecimiento.R`), que corre sobre una construccion anterior sin la regla de DPTO fijo ni los 3 controles obligatorios de esta version.

Exposicion: **`Exposure2022_obreros` a nivel FIRMA** (no `Exposure2022_obreros_est`), unida al panel de establecimiento por `NORDEMP` -- cada planta hereda la exposicion de su firma dueña. Especificacion: `i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST [+ CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F]`, `cluster=~NORDEMP` siempre, corrida SIN y CON los 3 controles del panel formal (misma muestra filtrada en ambas, solo cambia la estructura de efectos fijos). Tabla completa: `validacion_pretendencias_panel_formal.csv` (+ `_coeficientes.csv` / `_metadatos.csv`).

### Dimensiones PRINCIPALES (empleo) -- outcomes centrales del DiD

| Variable | F sin controles / p | F con controles / p |
|---|---|---|
| empleo_total | 1.90 / 0.107 | 0.511 / 0.728 |
| empleo_permanente | 2.70 / 0.029 | 0.709 / 0.586 |
| empleo_temporal | 0.72 / 0.578 | 1.03 / 0.389 |
| participacion_permanente | 1.52 / 0.194 | 0.301 / 0.877 |

Con los 3 controles, **ninguna de las 4 dimensiones principales rechaza tendencias paralelas** -- consistente con el resultado ya establecido a nivel firma para `Exposure2022_obreros`.

### Dimensiones de MECANISMO/EXTENSION -- ⚠ NO SON OUTCOMES PRINCIPALES DEL DISEÑO DiD

**Marca explicita, para que quede sin ambiguedad:** las siguientes 4 variables son extensiones exploratorias sobre posibles MECANISMOS del efecto (via costos de mantenimiento/outsourcing, inversion, y ventas), no resultados centrales de la tesis. Pasar o no pasar este chequeo de pre-tendencias **no las convierte en resultado central** ni cambia la especificacion principal del DiD (que sigue siendo las 4 dimensiones de empleo de arriba, con `Exposure2022_obreros`). Se transforman con `asinh()` (admite ceros/negativos, a diferencia de `log()`) y **no se deflactan**: el efecto fijo `ANIO_F` ya absorbe cualquier tendencia de precios agregada comun a todas las firmas en un anio dado; no se usa el deflactor del script exploratorio de Nicolas (`3. SCRIPTS/exploratorio_nicolas/construir_base_analitica_nicolas.R`, nunca validado).

| Variable (asinh) | Que mide | F sin controles / p | F con controles / p |
|---|---|---|---|
| `C3R23C3` | Mantenimiento, reparaciones, accesorios y repuestos | 6.02 / 7.8e-05 | 0.175 / 0.951 |
| `C3R41C3` | Outsourcing / servicios contratados con terceros | 9.18 / 2.2e-07 | 0.818 / 0.514 |
| `C7R10C2` | Total inversiones en activos fijos | 13.3 / 9.3e-11 | 1.30 / 0.269 |
| `VALORVEN` | Valor de las ventas | 21.4 / 1.5e-17 | 0.51 / 0.728 |

**Lectura:** las 4 rechazan con fuerza SIN controles, y ninguna rechaza CON controles -- el mismo patron transversal ya documentado en este proyecto (la identificacion depende de `sector*anio` + `tamano*anio` + `departamento*anio`, no de la exposicion cruda) aparece tambien en estas 4 variables de mecanismo. Como son extensiones, no outcomes principales, esto no altera ninguna conclusion del DiD central -- se deja registrado para cuando se explore el mecanismo del efecto.

### Chequeo de robustez: Exposure2022_obreros (firma) vs. Exposure2022_obreros_est (establecimiento)

Script: `3. SCRIPTS/validaciones/validar_pretendencias_panel_formal_exposure_est.R` -- COPIA exacta del script de arriba, unico cambio: `Exposure2022_obreros_est` (propia de cada establecimiento, ANIO_BASE 2022, union DIRECTA por `NORDEST`) en vez de `Exposure2022_obreros` (de la firma dueña, union por `NORDEMP`). Tabla lado a lado: `comparacion_pretendencias_panel_formal_firma_vs_establecimiento.csv` (16 filas).

**De las 16 celdas (8 dimensiones x 2 especificaciones), 1 cambia de conclusion al 5%:** `empleo_total`, especificacion SIN controles (firma: F=1.90/p=0.107, no rechaza; establecimiento: F=2.63/p=0.033, rechaza). **Con los 3 controles -- la especificacion recomendada -- ambas versiones coinciden en las 8 dimensiones, incluida `empleo_total`** (firma p=0.728, establecimiento p=0.843). Las 15 celdas restantes coinciden en conclusion (rechaza/no rechaza) entre las dos versiones de exposicion.

**Correlacion Exposure2022_obreros vs. Exposure2022_obreros_est, recalculada EN LA MUESTRA de esta validacion** (no se asume el 0.964 ya reportado sobre el universo completo de 2022): Pearson = 0.963, Spearman = 0.959 (n=6,660 establecimientos) -- practicamente identica a la cifra historica, esperable porque la mayoria de la muestra es monoplanta. Tabla: `correlacion_exposure_firma_vs_establecimiento_muestra_pretendencias.csv`.

**Multiplanta vs. monoplanta en esta muestra** (donde las 2 versiones de exposicion pueden diferir vs. donde coinciden por construccion): 806 establecimientos multiplanta (12.1%) y 5,854 monoplanta (87.9%). Tabla: `resumen_multiplanta_muestra_pretendencias.csv`.

**Especificacion principal para el panel de establecimiento:** dado que las conclusiones NO cambian en la especificacion CON controles (la recomendada) en ninguna de las 8 dimensiones, y que la correlacion entre ambas medidas de exposicion es muy alta (0.963) en esta muestra, se mantiene **`Exposure2022_obreros` (firma) como especificacion principal**, por parsimonia y continuidad con el resto del proyecto (que usa esta misma medida a nivel firma en todas las demas validaciones). `Exposure2022_obreros_est` queda documentada como chequeo de robustez disponible, no descartada -- util especificamente para analisis que exploten variacion DENTRO de firmas multiplanta (la "especificacion B" de `NOTA_PREANALISIS.md`), donde por definicion `Exposure2022_obreros` (firma) no varia entre plantas de una misma firma y `Exposure2022_obreros_est` si.

## "Primer eslabon" / manipulation check: ¿predice Exposure2022_obreros el choque de costo laboral en 2023? (2026-09-06)

> **⚠ MARCA EXPLICITA: esto NO es el resultado principal del diseño DiD.** Es un chequeo de PRIMERA ETAPA (manipulation check): verifica que la exposicion efectivamente predice el choque de costo que el diseño asume, antes de estimar efectos sobre empleo (que sigue siendo las 4 dimensiones principales de la seccion "Pre-tendencias sobre el panel formal" de arriba). `costo_laboral_total`/`salario_promedio` **no se usan como outcome estandar** del DiD -- tienen la contaminacion del pre-periodo ya documentada en este mismo README (seccion "Antecedente" de atricion, y el resto del proyecto). Pasar o no pasar este chequeo no cambia el resultado principal.

Scripts: `3. SCRIPTS/validaciones/validar_primer_eslabon_costo_laboral.R` (Exposure2022_obreros, firma) y su version de robustez `..._exposure_est.R` (Exposure2022_obreros_est, establecimiento). Panel formal completo (Paso A), ventana COMPLETA 2015-2024 (2020 excluido) -- no solo el pre-periodo 2015-2019. Outcome: `asinh(salario_promedio)`, `salario_promedio = costo_laboral_total / empleo_total`, con `costo_laboral_total` construido con la formula de fallback ya validada (`C3R10C3` si existe, si no `SALPEYTE+PRESPYTE`, si no `SALARPER+PRESSPER+REMUTEMP`) -- SIN deflactar (el efecto fijo `ANIO_F` absorbe la tendencia de precios agregada, misma logica que las 4 variables de mecanismo de la seccion anterior). Especificacion: `i(ANIO_F, exposicion_10pp, ref='2015') | NORDEST [+ CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F]`, `cluster=~NORDEMP` siempre.

### Resultado principal: el salto 2022→2023 es atipico SIN controles, pero NO con controles

**Prueba formal del salto atipico** (Paso 3): contraste lineal entre el incremento del coeficiente 2022→2023 y el incremento tipico anio-a-anio del pre-periodo (promedio de 2016→2017, 2017→2018, 2018→2019).

| Especificacion | Exposicion | Incremento 2022→2023 | Incremento tipico pre-periodo | Contraste | t | p-valor | ¿Atipico al 5%? |
|---|---|---|---|---|---|---|---|
| Sin controles | Firma | 0.0160 | 0.0087 | 0.00728 | 9.08 | **1.5e-19** | **SI** |
| Con sector×año+tamaño×año+depto×año | Firma | -0.0024 | -0.0014 | -0.00106 | -0.29 | **0.771** | **NO** |
| Sin controles | Establecimiento | 0.0161 | 0.0087 | 0.00739 | 8.21 | **2.7e-16** | **SI** |
| Con sector×año+tamaño×año+depto×año | Establecimiento | 0.0001 | -0.0013 | 0.00142 | 0.33 | **0.740** | **NO** |

**Lectura, sin suavizar el resultado:** bajo la especificacion SIN controles, el choque 2022→2023 es estadisticamente atipico con una significancia extrema (p<3e-16 en ambas medidas de exposicion) -- consistente con la narrativa de que el salario minimo golpeo mas fuerte a las firmas mas expuestas en 2023. **Pero esa señal desaparece POR COMPLETO bajo la especificacion CON controles (la que el resto de este proyecto trata como principal)**: p=0.77 (firma) y p=0.74 (establecimiento), muy lejos de cualquier umbral convencional. Ademas, mirando los coeficientes individuales (no solo el contraste): con controles, los coeficientes de 2022 y 2023 son **negativos y solo marginalmente significativos** (p≈0.03 en ambos, signo opuesto al esperado), mientras que sin controles la serie completa 2016-2024 crece de forma suave y monotonica (no hay un quiebre visualmente aislado en 2023, es una tendencia continua). Tabla completa de coeficientes: `validacion_primer_eslabon_costo_laboral_coeficientes.csv` (+ `_exposure_est` para la version de establecimiento). Grafico: `evento_primer_eslabon_costo_laboral.png`.

**Interpretacion:** el mismo patron transversal de todo este proyecto (la identificacion depende de sector×año + tamaño×año + departamento×año, no de la exposicion cruda) aparece tambien aqui. Bajo la especificacion controlada, `Exposure2022_obreros` **no predice de forma robusta un choque diferencial de costo laboral especificamente atribuible a 2023** -- la aparente señal fuerte sin controles es explicada por composicion sectorial/tamaño/geografica, no por el mecanismo de exposicion en si. Esto no invalida el diseño DiD (que sigue reposando en las 4 dimensiones de empleo con controles, ya validadas en la seccion anterior), pero SI significa que el "primer eslabon" del mecanismo (exposicion -> choque de costo observable) no queda demostrado con esta medida de costo laboral bajo la especificacion principal -- limitacion que queda documentada, no oculta.

## Antecedente: atricion diferencial por quintil de exposicion (nivel FIRMA, ya corrido)

Encontrado el 2026-08-31 al auditar el inventario del repositorio (`INVENTARIO_REPO.md`, rama `feature/panel-establecimiento`): el diagnostico de atricion diferencial YA se corrio el 2026-08-09, en la rama `feature/exposicion-obreros-operarios` (ya fusionada a `main`), antes de que existiera esta carpeta `Validaciones/` -- por eso nunca quedo documentado aqui.

> **CORRECCION (2026-09-02).** "Ya fusionada a `main`" es impreciso. El contenido relevante para ESTE antecedente si esta en `main`: el commit `15105d6` (2026-08-09, citado abajo) es ancestro de `main`, y `diagnostico_atricion_diferencial_exposicion_eam.R` sigue en el arbol de trabajo. Pero la rama `feature/exposicion-obreros-operarios` como tal **nunca se fusiono por completo**: al 2026-09-02 tiene un commit adicional sin fusionar (`add6577`, 2026-08-25, autor Nicolas Jacome), que agrega una seccion de diagnostico de cobertura de obreros/salarios 2022 a nivel establecimiento al script exploratorio del compañero (`3. SCRIPTS/3. SCRIPTS/construir_base analitica.R`, nunca validado ni citado en `CIFRAS_CLAVE.csv` ni en `INDICE_RESULTADOS.md`). Esa rama se conserva sin borrar hasta decidir que hacer con ese commit -- ver `README.md` de la raiz, seccion Historial.

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
