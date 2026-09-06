# Estimacion DiD principal — empleo

> **⚠ ESTA ES LA ESPECIFICACION PRINCIPAL DE LA TESIS.** No es un mecanismo, no es una validacion de identificacion, no es un chequeo de primera etapa -- es el resultado central del diseño de diferencias en diferencias: efecto del choque de salario minimo de 2023 sobre 4 dimensiones de empleo (`empleo_total`, `empleo_permanente`, `empleo_temporal`, `participacion_permanente`), en funcion de `Exposure2022_obreros` (composicion ocupacional pre-choque, a nivel FIRMA), sobre el panel formal ya validado (`panel_establecimiento_formal.rds`, Paso A).

Script: `3. SCRIPTS/estimacion/estimar_did_principal_empleo.R`.

## Bite2022_obreros queda fuera de esta ronda -- pospuesta, no descartada

Esta estimacion usa **unicamente** `Exposure2022_obreros`, por decision explicita. `Bite2022_obreros` no se incluye aqui -- queda pendiente de evaluar en el futuro con otras medidas de exposicion alternativas. No es un juicio sobre la validez de Bite: es una decision de alcance de esta ronda de estimacion.

## Limitacion conocida que debe citarse al interpretar cualquier resultado nulo

El **"primer eslabon"** (`validaciones/validar_primer_eslabon_costo_laboral.R`) encontro que `Exposure2022_obreros` **no predice de forma robusta** el choque real de costo laboral en 2023 bajo la especificacion CON controles (p=0.771) -- aunque `Bite2022_obreros` SI lo predice bajo la misma especificacion (p=5.8e-9, `validaciones/validar_primer_eslabon_costo_laboral_bite.R`). El diagnostico de sobre-control relacionado (`validaciones/diagnosticar_sobrecontrol_exposure_fwl.R`) encontro que sector+tamaño+departamento explican ~20-22% de la varianza de `Exposure2022_obreros`.

**Implicacion directa para la lectura de los resultados de este documento:** un coeficiente NO significativo en las tablas de abajo es **ambiguo** entre dos explicaciones que estos datos no permiten distinguir:
1. No hay efecto real de `Exposure2022_obreros` sobre esa dimension de empleo, o
2. La medida `Exposure2022_obreros`, bajo la especificacion con controles, no tiene la potencia estadistica para detectar el mecanismo que el diseño asume (dado que el primer eslabon del mecanismo -- el choque de costo -- no queda demostrado con esta medida bajo esa misma especificacion).

Esta ambigüedad no se resuelve en este documento -- se deja explicita para que cualquier interpretacion posterior de un resultado nulo la tenga en cuenta.

## Especificacion A: DiD estatico

`Y_ft = a + b*(post_2023 x Exposure2022_obreros_10pp) + FE_establecimiento + FE_año [+ controles]`

- Sin controles: `outcome ~ post_2023:exposicion_10pp | NORDEST + ANIO_F`
- Con controles: `outcome ~ post_2023:exposicion_10pp | NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F`
- `cluster = ~NORDEMP` siempre. Ventana: panel formal completo (2015-2019+2021-2022 pre, 2023-2024 post, 2020 excluido).

Tabla formato academico (coeficiente, error estandar entre parentesis, asteriscos de significancia, N, R2, fila de controles): `tabla_did_estatico_empleo.csv` / `.html`. Coeficientes tidy con IC95%: `did_estatico_empleo_coeficientes.csv`.

| Outcome | Sin controles | Con controles (sector×año+tamaño×año+depto×año) |
|---|---|---|
| Empleo total | -0.164 (0.388), p=0.672 | 0.548 (0.352), p=0.120 |
| Empleo permanente | -0.205 (0.274), p=0.455 | 0.399 (0.284), p=0.160 |
| Empleo temporal | 0.061 (0.230), p=0.792 | 0.149 (0.231), p=0.519 |
| Participacion permanente (%) | -0.218. (0.128), p=0.088 | -0.157 (0.150), p=0.295 |

Ninguno de los 8 coeficientes es significativo al 5% bajo la especificacion con controles (la recomendada).

## Especificacion B: event study completo (pre + post en una sola imagen)

`outcome ~ i(ANIO_F, exposicion_10pp, ref='2022') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F`, `cluster=~NORDEMP`, SOLO con controles (la especificacion recomendada). Referencia = **2022** (ultimo año pre-tratamiento), a diferencia del ejercicio de pre-tendencias (que usa ref=2015) -- aqui el objetivo es ver el efecto completo pre+post en una sola imagen, no solo chequear el pre-periodo.

Coeficientes tidy: `event_study_completo_coeficientes.csv`. Metadatos: `event_study_completo_metadatos.csv`. Graficos: `evento_did_completo_<outcome>.png` (uno por outcome) y `evento_did_completo_panel_2x2.png` (las 4 dimensiones en un solo panel).

## Convenciones (identicas al resto del proyecto)

- `cluster = ~NORDEMP` en todos los modelos, sin excepcion.
- Los 3 controles obligatorios (sector×año + tamaño×año + departamento×año) son los mismos ya usados en `validar_pretendencias_panel_formal.R` y `validar_primer_eslabon_costo_laboral.R`.
- `exposicion_10pp = Exposure2022_obreros / 0.1` y `post_2023 = as.integer(ANIO >= 2023)`: formulas identicas a `pipeline/03_construir_panel.R`.
