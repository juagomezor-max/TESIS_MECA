# construir_panel_establecimiento_formal.R -- Panel formal a nivel
# ESTABLECIMIENTO (NORDEST-ANIO), reconstruido DESDE CERO (no se copio ni
# se consulto el tag `archivo/panel-formal`, que archiva un intento
# anterior interrumpido -- ver README.md raiz, seccion Historial).
#
# DERIVADO de (no reimplementado de memoria):
# - Columnas C4R de obreros/administrativos/prof-tecnico (excluyendo
#   propietarios) y la verificacion de que NORDEST-ANIO ya es unico en
#   el 100% de la macrobase (0 duplicados, Paso 1 de
#   feature/panel-establecimiento): IDENTICAS a
#   construccion/construir_conteo_personal_categoria_establecimiento_eam.R.
# - Columnas C4R de personal permanente/temporal: IDENTICAS a
#   pipeline/01_construir_base.R (cols_permanente/cols_temporal).
# - Formulas de empleo_total/empleo_permanente/empleo_temporal/
#   participacion_permanente: IDENTICAS a pipeline/03_construir_panel.R.
# - Umbrales de tamano (Pequena<50, Mediana<200, Grande>=200): IDENTICOS
#   a pipeline/03_construir_panel.R (investigar_validez_test_pretendencias.R
#   originalmente).
#
# ESPECIFICACION (decisiones YA APROBADAS, citadas explicitamente en cada
# bloque -- NO se re-derivan aqui, se aplican):
#
# 1. Unidad: NORDEST-ANIO (establecimiento-año).
# 2. Ventana: PANEL_ANIOS_FINAL = 2015-2019 + 2021-2022 (pre) + 2023-2024
#    (post), 2020 excluido (disrupcion pandemia). 9 años.
#    Fuente: NOTA_PREANALISIS.md §2 ("Ventana del panel final"),
#    "0. PREPARACION/notas_panel_establecimiento.md" Paso 2.6,
#    README_confiabilidad_dpto.md punto 5 (confirma el mismo
#    PANEL_ANIOS_FINAL para el chequeo de celdas departamento-anio).
# 3. DPTO fijo por establecimiento (invariante en el tiempo), regla
#    DIFERENCIADA por patron de inestabilidad:
#      - Estable (1 solo DPTO en toda su serie, 96.32% de los casos):
#        ese unico valor.
#      - "cambio_sostenido" (415 casos): DPTO vigente en 2022.
#      - "salto_aislado" (13) / "patron_irregular" (37): DPTO modal.
#    Fuente de la REGLA: NOTA_PREANALISIS.md §2 ("Variable de
#    ubicacion"), README_confiabilidad_dpto.md punto 2 ("Tratamiento
#    APROBADO"). Fuente de la CLASIFICACION por NORDEST (no se
#    re-corre el algoritmo de rle() aqui, se LEE el archivo ya
#    versionado): auditorias/auditar_estabilidad_dpto_nordest.R ->
#    4. RESULTADOS/Validaciones/auditoria_dpto_estabilidad_nordest_casos.csv.
#    El calculo del valor FIJO en si (lookup 2022 / moda) es nuevo en
#    este script -- ningun script anterior lo hacia, solo clasificaba
#    el patron.
# 4. Controles a construir en el panel: sector(CIIU4)*anio +
#    tamano_empresa*anio + departamento(DPTO)*anio -- LAS TRES.
#    IMPORTANTE (decision tomada AHORA, no reabre ambiguedad de rondas
#    anteriores): NOTA_PREANALISIS.md §2 fila "Controles obligatorios"
#    dice literalmente "sector(CIIU4)*anio + tamano*anio" -- SIN
#    mencionar departamento. Esta version del panel formal agrega
#    departamento*anio como tercer control obligatorio de forma
#    EXPLICITA y deliberada (autorizado en esta instruccion), no
#    porque estuviera implicito antes.
#    Nota adicional (decision de esta implementacion, no pre-aprobada
#    en ningun documento -- se documenta para que quede visible, no
#    oculta): "tamano_empresa" se calcula con el `empleo_total` del
#    PROPIO ESTABLECIMIENTO (no el total agregado de la firma dueña).
#    Razon: el proposito de un panel a nivel establecimiento es
#    capturar heterogeneidad entre plantas de una misma firma
#    multiplanta (ver Validaciones/README.md, seccion 3: "agregarlas a
#    nivel NORDEMP puede mezclar tendencias propias de una planta
#    especifica") -- usar el tamano de la FIRMA reintroduciria esa
#    misma mezcla por la puerta de atras. Si se prefiere el tamano de
#    la firma duena, es un cambio de una linea, sujeto a decision.
# 5. Deduplicacion / columnas C3R/C4R: ver bloque "DERIVADO de" arriba.
#    NORDEST-ANIO ya es unico (Paso 1), no requiere group_by/summarise
#    para deduplicar (a diferencia de NORDEMP-ANIO en 01_construir_base.R).
#
# Salidas (no versionadas, 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/):
# - panel_establecimiento_formal.rds/.csv (incluye costo_laboral_total y
#   salario_promedio desde 2026-09-05, agregadas para el chequeo de
#   "primer eslabon" -- ver validaciones/validar_primer_eslabon_costo_laboral.R.
#   Formula de fallback EXACTA, ya validada en 3 scripts, ver bloque de
#   calculo abajo -- NO reinventada.)

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

if (!file.exists(paths$macro_base_eam)) {
  stop("No se encontro la macrobase en ", paths$macro_base_eam, ".")
}

casos_dpto_path <- file.path(out_dir, "auditoria_dpto_estabilidad_nordest_casos.csv")
if (!file.exists(casos_dpto_path)) {
  stop(
    "Falta ", casos_dpto_path, ". Corre auditorias/auditar_estabilidad_dpto_nordest.R primero ",
    "(este script LEE esa clasificacion, no la recalcula)."
  )
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

# ------------------------------------------------------------------
# 1) Columnas C4R (identicas a construccion/construir_conteo_personal_categoria_establecimiento_eam.R
#    y pipeline/01_construir_base.R).
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_permanente <- c("C4R2C1", "C4R2C2", "C4R2C3", "C4R2C4", "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E")
cols_temporal <- c(
  "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E", "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"
)
cols_costo_laboral <- c(
  # costo_laboral_total: formula de fallback EXACTA, ya validada en
  # pipeline/descriptivo_exposicion_eam.R (linea 269),
  # validaciones/investigar_divergencia_pretendencias_2018_2019.R y
  # validaciones/diagnostico_preliminar_tendencias_2015_2019.R -- no
  # re-derivada aqui, ver bloque de calculo abajo.
  "C3R10C3", "SALPEYTE", "PRESPYTE", "SALARPER", "PRESSPER", "REMUTEMP"
)
cols_numericas <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico, cols_permanente, cols_temporal, cols_costo_laboral))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", "CIIU4", "DPTO", cols_numericas))

# ------------------------------------------------------------------
# 2) Filtrado a NORDEST-ANIO validos. Sin group_by/summarise: NORDEST-ANIO
#    ya es unico (Paso 1, auditar_confiabilidad_nordest.R).
# ------------------------------------------------------------------

base_establecimiento <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, CIIU4, DPTO, dplyr::all_of(cols_numericas)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_numericas), safe_numeric))

n_dup <- base_establecimiento %>% dplyr::count(NORDEST, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup > 0) {
  stop(
    "NORDEST-ANIO no es unico (", n_dup, " grupos duplicados). Contradice el hallazgo del Paso 1 -- revisar antes de continuar."
  )
}

# ------------------------------------------------------------------
# 3) DPTO fijo por establecimiento (regla diferenciada aprobada).
#    Se usa la serie COMPLETA 2008-2024 de DPTO por NORDEST (no solo la
#    ventana del panel) porque asi se calculo la clasificacion original
#    en auditar_estabilidad_dpto_nordest.R.
# ------------------------------------------------------------------

dpto_por_anio <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO), !is.na(DPTO)) %>%
  dplyr::distinct(NORDEST, ANIO, DPTO)

casos_dpto <- readr::read_csv(casos_dpto_path, show_col_types = FALSE) %>%
  dplyr::mutate(NORDEST = as.character(NORDEST))

dpto_2022 <- dpto_por_anio %>% dplyr::filter(ANIO == 2022) %>% dplyr::select(NORDEST, dpto_2022 = DPTO)

moda <- function(x) {
  tabla <- table(x)
  as.numeric(names(tabla)[which.max(tabla)])
}
dpto_modal <- dpto_por_anio %>%
  dplyr::group_by(NORDEST) %>%
  dplyr::summarise(dpto_modal = moda(DPTO), .groups = "drop")

dpto_estable <- dpto_por_anio %>%
  dplyr::group_by(NORDEST) %>%
  dplyr::summarise(n_dptos_distintos = dplyr::n_distinct(DPTO), dpto_unico = dplyr::first(DPTO), .groups = "drop")

dpto_fijo_tabla <- dpto_estable %>%
  dplyr::left_join(casos_dpto %>% dplyr::select(NORDEST, patron), by = "NORDEST") %>%
  dplyr::left_join(dpto_2022, by = "NORDEST") %>%
  dplyr::left_join(dpto_modal, by = "NORDEST") %>%
  dplyr::mutate(
    DPTO_fijo = dplyr::case_when(
      n_dptos_distintos == 1 ~ dpto_unico,
      patron == "cambio_sostenido" & !is.na(dpto_2022) ~ dpto_2022,
      # Fallback documentado (no cubierto por la regla aprobada tal
      # cual): si un "cambio_sostenido" no tiene reporte en 2022,
      # se usa el valor modal en su lugar -- se reporta cuantos casos
      # caen aqui, no se asume que sean cero.
      patron == "cambio_sostenido" & is.na(dpto_2022) ~ dpto_modal,
      patron %in% c("salto_aislado", "patron_irregular") ~ dpto_modal,
      TRUE ~ dpto_modal
    ),
    dpto_2022_faltante_en_cambio_sostenido = patron == "cambio_sostenido" & is.na(dpto_2022)
  ) %>%
  dplyr::select(NORDEST, DPTO_fijo, patron, dpto_2022_faltante_en_cambio_sostenido)

n_fallback <- sum(dpto_fijo_tabla$dpto_2022_faltante_en_cambio_sostenido, na.rm = TRUE)

# ------------------------------------------------------------------
# 4) Ventana del panel + variables de resultado + controles.
# ------------------------------------------------------------------

PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)

tamano_de <- function(empleo) {
  dplyr::case_when(
    is.na(empleo) ~ NA_character_,
    empleo < 50 ~ "Pequena",
    empleo < 200 ~ "Mediana",
    TRUE ~ "Grande"
  )
}

panel_ventana <- base_establecimiento %>%
  dplyr::filter(ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::mutate(
    empleo_total = rowSums(dplyr::across(dplyr::all_of(c(cols_obreros, cols_administrativos, cols_prof_tecnico))), na.rm = TRUE),
    empleo_permanente = rowSums(dplyr::across(dplyr::all_of(cols_permanente)), na.rm = TRUE),
    empleo_temporal = rowSums(dplyr::across(dplyr::all_of(cols_temporal)), na.rm = TRUE),
    participacion_permanente = safe_divide(empleo_permanente, empleo_total) * 100,
    tamano_empresa = tamano_de(empleo_total),
    # costo_laboral_total / salario_promedio: formula de fallback EXACTA
    # (no reinventada, ver "cols_costo_laboral" arriba). Con las 6
    # columnas siempre presentes (Paso 0, 2026-09-05), la rama C3R10C3
    # siempre se toma -- se deja la cadena completa por fidelidad al
    # patron ya validado, no solo la primera rama.
    costo_laboral_total = if ("C3R10C3" %in% names(.)) {
      C3R10C3
    } else if (all(c("SALPEYTE", "PRESPYTE") %in% names(.))) {
      SALPEYTE + PRESPYTE
    } else {
      SALARPER + PRESSPER + REMUTEMP
    },
    salario_promedio = safe_divide(costo_laboral_total, empleo_total)
  ) %>%
  dplyr::left_join(dpto_fijo_tabla, by = "NORDEST") %>%
  dplyr::mutate(
    ANIO_F = factor(ANIO),
    anio_lineal = ANIO - min(PANEL_ANIOS_FINAL),
    CIIU4 = factor(CIIU4),
    DPTO_fijo = factor(DPTO_fijo),
    tamano_empresa = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  ) %>%
  dplyr::select(
    NORDEST, NORDEMP, ANIO, ANIO_F, anio_lineal, CIIU4, DPTO_fijo,
    tamano_empresa, empleo_total, empleo_permanente, empleo_temporal, participacion_permanente,
    costo_laboral_total, salario_promedio
  )

n_sin_dpto_fijo <- sum(is.na(panel_ventana$DPTO_fijo))

readr::write_rds(panel_ventana, file.path(data_dir, "panel_establecimiento_formal.rds"))
readr::write_csv(panel_ventana, file.path(data_dir, "panel_establecimiento_formal.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("construir_panel_establecimiento_formal.R -- Panel formal NORDEST-ANIO (reconstruido desde cero)")
message("")
message("Filas NORDEST-ANIO en la ventana (", length(PANEL_ANIOS_FINAL), " anios): ", nrow(panel_ventana))
message("Establecimientos (NORDEST) unicos: ", dplyr::n_distinct(panel_ventana$NORDEST))
message("Firmas (NORDEMP) unicas: ", dplyr::n_distinct(panel_ventana$NORDEMP))
message("")
message("Filas por anio:")
print(panel_ventana %>% dplyr::count(ANIO), n = Inf)
message("")
message("Establecimientos con >1 DPTO en su serie completa (465 esperado por auditoria previa): ",
        sum(!is.na(dpto_fijo_tabla$patron)))
message("Distribucion de patron entre los que caen en la ventana del panel:")
print(panel_ventana %>% dplyr::distinct(NORDEST) %>% dplyr::left_join(dpto_fijo_tabla, by = "NORDEST") %>%
        dplyr::count(patron = dplyr::coalesce(patron, "estable_1_dpto")), n = Inf)
message("")
message("Casos 'cambio_sostenido' sin reporte en 2022 (fallback a moda usado): ", n_fallback)
message("Filas con DPTO_fijo faltante tras el join (deberia ser 0): ", n_sin_dpto_fijo)
message("")
message("Cobertura de costo_laboral_total / salario_promedio (agregadas 2026-09-05, 'primer eslabon'):")
message("  Filas totales: ", nrow(panel_ventana))
message("  Filas con costo_laboral_total NA: ", sum(is.na(panel_ventana$costo_laboral_total)))
message("  Filas con salario_promedio NA: ", sum(is.na(panel_ventana$salario_promedio)))
message("")
message("Base exportada en: ", file.path(data_dir, "panel_establecimiento_formal.rds"))
