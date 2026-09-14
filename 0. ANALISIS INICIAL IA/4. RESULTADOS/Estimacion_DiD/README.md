# Estimacion DiD principal — empleo

> **⚠ ESTA ES LA ESPECIFICACION PRINCIPAL DE LA TESIS.** No es un mecanismo, no es una validacion de identificacion, no es un chequeo de primera etapa -- es el resultado central del diseño de diferencias en diferencias: efecto del choque de salario minimo de 2023 sobre 4 dimensiones de empleo (`empleo_total`, `empleo_permanente`, `empleo_temporal`, `participacion_permanente`), sobre el panel formal ya validado (`panel_establecimiento_formal.rds`, Paso A).

> **CORRECCION (2026-09-13):** la version original de este README (commit `64b04ea`, 2026-09-05) decia que `Bite2022_obreros` "queda fuera de esta ronda -- pospuesta, no descartada". Eso ya no aplica: `Bite2022_obreros` se estimo el 2026-09-13 (`estimar_did_principal_empleo_bite.R`, commit ver git log). **Exposure2022_obreros y Bite2022_obreros se presentan EN PARALELO, sin jerarquia entre ellas** -- decision de la seccion 4.3 de Estrategia Empirica de la tesis. No se borro el texto anterior en silencio: se corrige aqui, explicitamente, y el commit original sigue en el historial.

Scripts: `3. SCRIPTS/estimacion/estimar_did_principal_empleo.R` (Exposure2022_obreros) y `estimar_did_principal_empleo_bite.R` (Bite2022_obreros, espejo exacto salvo la medida de exposicion y su escala).

## Limitacion conocida que debe citarse al interpretar cualquier resultado nulo -- DISTINTA para cada medida

El **"primer eslabon"** (manipulation check, `validaciones/validar_primer_eslabon_costo_laboral*.R`) encontro un resultado DIFERENTE para cada medida:

- **Exposure2022_obreros**: NO predice de forma robusta el choque real de costo laboral en 2023 bajo la especificacion CON controles (p=0.771). El diagnostico de sobre-control relacionado (`validaciones/diagnosticar_sobrecontrol_exposure_fwl.R`) encontro que sector+tamaño+departamento explican ~20-22% de la varianza de `Exposure2022_obreros`.
- **Bite2022_obreros**: SI predice el choque de costo laboral, tanto sin controles (p=3.4e-23) como CON controles (p=5.8e-9).

**Implicacion directa para la lectura de los resultados de este documento:**
- Un coeficiente de **Exposure2022_obreros** NO significativo es **ambiguo** entre dos explicaciones que estos datos no permiten distinguir: (1) no hay efecto real sobre esa dimension de empleo, o (2) la medida, bajo la especificacion con controles, no tiene la potencia estadistica para detectar el mecanismo que el diseño asume (dado que su propio primer eslabon no queda demostrado bajo esa misma especificacion).
- Un coeficiente de **Bite2022_obreros** NO significativo NO tiene esa misma ambigüedad especifica -- su primer eslabon SI pasa, tanto sin como con controles.

Esta asimetria no se resuelve en este documento -- se deja explicita para que cualquier interpretacion posterior de un resultado (nulo o no) la tenga en cuenta.

## Especificacion A: DiD estatico

`Y_ft = a + b*(post_2023 x exposicion) + FE_establecimiento + FE_año [+ controles]`

- Sin controles: `outcome ~ post_2023:exposicion | NORDEST + ANIO_F`
- Con controles: `outcome ~ post_2023:exposicion | NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F`
- `cluster = ~NORDEMP` siempre. Ventana: panel formal completo (2015-2019+2021-2022 pre, 2023-2024 post, 2020 excluido).
- Exposure2022_obreros escalada a +10pp (`exposicion_10pp = Exposure2022_obreros / 0.1`); Bite2022_obreros escalada a +1 SD muestral (`bite_1sd`, sd=0.3056) -- Bite no tiene una escala natural de "10pp" (indice de Kaitz, no una participacion 0-1), ver header de `estimar_did_principal_empleo_bite.R`. La escala no afecta significancia ni p-valor, solo la magnitud interpretable del coeficiente.

Tabla formato academico (coeficiente, error estandar entre parentesis, asteriscos de significancia, N, R2, fila de controles): `tabla_did_estatico_empleo.csv`/`.html` (Exposure) y `tabla_did_estatico_empleo_bite.csv`/`.html` (Bite). Coeficientes tidy con IC95%: `did_estatico_empleo_coeficientes.csv` / `did_estatico_empleo_bite_coeficientes.csv`.

**Exposure2022_obreros** (+10pp):

| Outcome | Sin controles | Con controles (sector×año+tamaño×año+depto×año) |
|---|---|---|
| Empleo total | -0.164 (0.388), p=0.672 | 0.548 (0.352), p=0.120 |
| Empleo permanente | -0.205 (0.274), p=0.455 | 0.399 (0.284), p=0.160 |
| Empleo temporal | 0.061 (0.230), p=0.792 | 0.149 (0.231), p=0.519 |
| Participacion permanente (%) | -0.218. (0.128), p=0.088 | -0.157 (0.150), p=0.295 |

Ninguno de los 8 coeficientes de Exposure es significativo al 5% con controles.

**Bite2022_obreros** (+1 SD):

| Outcome | Sin controles | Con controles (sector×año+tamaño×año+depto×año) |
|---|---|---|
| Empleo total | -6.49 (1.16), p=2.6e-8 | -1.16 (0.995), p=0.243 |
| Empleo permanente | -3.82 (0.850), p=7.1e-6 | 0.611 (0.794), p=0.441 |
| Empleo temporal | -2.61 (0.644), p=5.3e-5 | -1.72 (0.689), p=0.0124 |
| Participacion permanente (%) | 1.37 (0.355), p=1.2e-4 | 1.17 (0.419), p=0.00511 |

Con controles, 2 de 4 coeficientes de Bite son significativos al 5% (empleo temporal y participacion permanente); empleo total y empleo permanente no lo son.

## Especificacion B: event study completo (pre + post en una sola imagen)

`outcome ~ i(ANIO_F, exposicion, ref='2022') | NORDEST + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F`, `cluster=~NORDEMP`, SOLO con controles (la especificacion recomendada). Referencia = **2022** (ultimo año pre-tratamiento), a diferencia del ejercicio de pre-tendencias (que usa ref=2015) -- aqui el objetivo es ver el efecto completo pre+post en una sola imagen, no solo chequear el pre-periodo.

Coeficientes tidy: `event_study_completo_coeficientes.csv` (Exposure) / `event_study_completo_bite_coeficientes.csv` (Bite). Metadatos: `event_study_completo_metadatos.csv` / `_bite_metadatos.csv`. Graficos: `evento_did_completo_<outcome>.png` / `evento_did_completo_bite_<outcome>.png` (uno por outcome cada uno) y los paneles 2x2 combinados.

**Nota sin interpretar:** en la Especificacion B (event study), NINGUN coeficiente 2023/2024 de Bite es significativo al 5% (p entre 0.11 y 0.93) -- un patron distinto al de la Especificacion A (DiD estatico), donde 2 de 4 outcomes con controles si lo eran. Las dos especificaciones usan formas funcionales distintas (post_2023 como dummy unico vs. coeficiente año a año); la discrepancia se reporta como hecho, no se explica aqui.

## Convenciones (identicas al resto del proyecto)

- `cluster = ~NORDEMP` en todos los modelos, sin excepcion.
- Los 3 controles obligatorios (sector×año + tamaño×año + departamento×año) son los mismos ya usados en `validar_pretendencias_panel_formal.R` y `validar_primer_eslabon_costo_laboral*.R`.
- `post_2023 = as.integer(ANIO >= 2023)`: formula identica a `pipeline/03_construir_panel.R`.
