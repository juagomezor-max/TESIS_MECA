# construir_panel_firma_con_2020.R
#
# Construye una base APARTE de panel_analitico_firma_eam.rds, identica
# en columnas/reglas pero con la ventana 2015-2024 completa (incluye
# 2020), para la validacion "01b" (repetir el analisis principal
# incluyendo 2020). NO toca 1. DATOS/panel_analitico_firma_eam.rds ni
# panel_establecimiento_formal.rds -- solo LEE de ahi para comparar al
# final.
#
# DERIVADO de (no reimplementado de memoria, mismas reglas/formulas
# copiadas de los scripts ya validados, solo con la ventana ampliada):
# - Deduplicacion NORDEMP-ANIO: pipeline/01_construir_base.R. NO se
#   re-corre: panel_firma_eam.rds (la salida de ese script) ya cubre
#   2008-2024 completo, incluido 2020 -- confirmado leyendolo (6,723
#   filas en 2020). Se reusa tal cual, sin reconstruirlo.
# - Formulas de empleo_total/empleo_permanente/empleo_temporal/
#   participacion_permanente/tamano_empresa/post_2023/anio_lineal/
#   exposicion_10pp: IDENTICAS a pipeline/03_construir_panel.R, con
#   PANEL_ANIOS_FINAL ampliado a 2015:2024 (unica diferencia).
# - Exposure2022_obreros/Bite2022_obreros/quintiles: se unen tal cual de
#   exposicion_firma_eam.rds (medida transversal de 2022, no depende de
#   la ventana de anios del panel).
# - Multi_f: misma logica de pipeline/opcional_establecimiento.R /
#   estimacion/estimar_especificacion_a_establecimiento.R (corte 2022,
#   tampoco depende de la ventana).
# - C3R23C3/C3R41C3/C7R10C2/VALORVEN (nombres crudos) y las 161
#   variables de estimacion/../construccion/ampliar_variables_paneles.R
#   (mapa_simples + grupos de empleo + n_establecimientos): MISMAS
#   formulas/nombres, copiadas de ese script, aplicadas sobre la
#   macrobase con la ventana ampliada.
#
# Salida: 1. DATOS/panel_analitico_firma_eam_con_2020.rds (SOLO .rds,
# solo si la verificacion del Paso 3 pasa).

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
raiz <- ".."
datos_raiz <- file.path(raiz, "1. DATOS")

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

PANEL_ANIOS_CON_2020 <- 2015:2024

# ------------------------------------------------------------------
# 1) Panel de firma deduplicado (YA incluye 2020 -- no se reconstruye).
# ------------------------------------------------------------------

panel_firma <- readr::read_rds(file.path(data_dir, "panel_firma_eam.rds"))
exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))

message("panel_firma_eam.rds (intermedio, deduplicado): ", nrow(panel_firma), " filas, anios ",
        paste(range(panel_firma$ANIO), collapse = "-"))
message("Filas 2020 en el intermedio: ", sum(panel_firma$ANIO == 2020))

# ------------------------------------------------------------------
# 2) Formulas de 03_construir_panel.R, con la ventana ampliada.
# ------------------------------------------------------------------

tamano_de <- function(empleo) {
  dplyr::case_when(
    is.na(empleo) ~ NA_character_,
    empleo < 50 ~ "Pequena",
    empleo < 200 ~ "Mediana",
    TRUE ~ "Grande"
  )
}

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

panel_ventana <- panel_firma %>%
  dplyr::filter(ANIO %in% PANEL_ANIOS_CON_2020) %>%
  dplyr::mutate(
    empleo_total = rowSums(dplyr::across(dplyr::all_of(c(cols_obreros, cols_administrativos, cols_prof_tecnico))), na.rm = TRUE),
    empleo_permanente = rowSums(dplyr::across(dplyr::all_of(cols_permanente)), na.rm = TRUE),
    empleo_temporal = rowSums(dplyr::across(dplyr::all_of(cols_temporal)), na.rm = TRUE),
    participacion_permanente = safe_divide(empleo_permanente, empleo_total) * 100,
    tamano_empresa = tamano_de(empleo_total)
  ) %>%
  dplyr::select(NORDEMP, ANIO, CIIU4, DPTO, tamano_empresa, empleo_total, empleo_permanente, empleo_temporal, participacion_permanente)

panel_base <- panel_ventana %>%
  dplyr::left_join(
    exposicion_firma %>% dplyr::select(NORDEMP, Exposure2022_obreros, quintil_exposure2022_obreros, Bite2022_obreros, quintil_bite2022_obreros),
    by = "NORDEMP"
  ) %>%
  dplyr::mutate(
    post_2023 = as.integer(ANIO >= 2023),
    anio_lineal = ANIO - min(PANEL_ANIOS_CON_2020),
    ANIO_F = factor(ANIO),
    CIIU4 = factor(CIIU4),
    DPTO = factor(DPTO),
    tamano_empresa = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")),
    exposicion_10pp = Exposure2022_obreros / 0.1
  )

message("panel_base (con 2020, antes de enriquecer): ", nrow(panel_base), " filas, ", ncol(panel_base), " columnas")

# ------------------------------------------------------------------
# 3) Multi_f (corte 2022, no depende de la ventana de anios).
# ------------------------------------------------------------------

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))

base_2022_multi <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP), ANIO = as.integer(safe_numeric(ANIO))) %>%
  dplyr::filter(ANIO == 2022, !is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "") %>%
  dplyr::distinct(NORDEST, NORDEMP)
n_est_por_firma_2022 <- base_2022_multi %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_por_firma_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)
message("Multi_f: ", length(firmas_262), " firmas (esperado 262).")

panel_base <- panel_base %>% dplyr::mutate(Multi_f = as.integer(NORDEMP %in% firmas_262))

# ------------------------------------------------------------------
# 4) Variables "simples" de ampliar_variables_paneles.R -- mismo mapa,
#    copiado de ese script (no reinventado), aplicado con la ventana
#    ampliada. Incluye las 4 columnas de nombre crudo (C3R23C3, C3R41C3,
#    C7R10C2, VALORVEN) que ya estaban en el panel antes de esa
#    ampliacion (de exportar_paneles_manuales_1datos.R), para que las
#    columnas del resultado final coincidan exactamente con las del
#    panel actual.
# ------------------------------------------------------------------

mapa_simples <- tibble::tribble(
  ~variable_nueva, ~codigo_eam, ~tipo,
  "C3R23C3", "C3R23C3", "suma", # nombre crudo -- ya estaba en el panel actual (exportar_paneles_manuales_1datos.R)
  "C3R41C3", "C3R41C3", "suma", # idem
  "C7R10C2", "C7R10C2", "suma", # idem
  "VALORVEN", "VALORVEN", "suma", # idem
  "personal_total_pertotal", "PERTOTAL", "suma",
  "personal_permanente_persocu", "PERSOCU", "suma",
  "personal_permanente_propietarios_persoesc", "PERSOESC", "suma",
  "personal_temporal_directo_pertem3", "PERTEM3", "suma",
  "personal_permanente_temporal_pperytem", "PPERYTEM", "suma",
  "personal_mujeres_c4r4c9t", "C4R4C9T", "suma",
  "personal_hombres_c4r4c10t", "C4R4C10T", "suma",
  "salario_integral_obreros_c3r1c1", "C3R1C1", "suma",
  "salario_integral_administrativos_c3r1c2", "C3R1C2", "suma",
  "salario_integral_total_c3r1c3", "C3R1C3", "suma",
  "salario_integral_profesionales_c3r1pt", "C3R1PT", "suma",
  "sueldos_permanentes_obreros_c3r2c1", "C3R2C1", "suma",
  "sueldos_permanentes_administrativos_c3r2c2", "C3R2C2", "suma",
  "sueldos_permanentes_total_c3r2c3", "C3R2C3", "suma",
  "sueldos_permanentes_profesionales_c3r2pt", "C3R2PT", "suma",
  "prestaciones_permanentes_obreros_c3r3c1", "C3R3C1", "suma",
  "prestaciones_permanentes_administrativos_c3r3c2", "C3R3C2", "suma",
  "prestaciones_permanentes_total_c3r3c3", "C3R3C3", "suma",
  "prestaciones_permanentes_profesionales_c3r3pt", "C3R3PT", "suma",
  "sueldos_temporal_directo_obreros_c3r4c1", "C3R4C1", "suma",
  "sueldos_temporal_directo_administrativos_c3r4c2", "C3R4C2", "suma",
  "sueldos_temporal_directo_total_c3r4c3", "C3R4C3", "suma",
  "sueldos_temporal_directo_profesionales_c3r4pt", "C3R4PT", "suma",
  "cotizaciones_patronales_obreros_c3r5c1", "C3R5C1", "suma",
  "cotizaciones_patronales_administrativos_c3r5c2", "C3R5C2", "suma",
  "cotizaciones_patronales_total_c3r5c3", "C3R5C3", "suma",
  "cotizaciones_patronales_profesionales_c3r5pt", "C3R5PT", "suma",
  "aportes_nomina_obreros_c3r6c1", "C3R6C1", "suma",
  "aportes_nomina_administrativos_c3r6c2", "C3R6C2", "suma",
  "aportes_nomina_total_c3r6c3", "C3R6C3", "suma",
  "aportes_nomina_profesionales_c3r6pt", "C3R6PT", "suma",
  "seguros_vida_voluntarios_obreros_c3r7c1", "C3R7C1", "suma",
  "seguros_vida_voluntarios_administrativos_c3r7c2", "C3R7C2", "suma",
  "seguros_vida_voluntarios_total_c3r7c3", "C3R7C3", "suma",
  "seguros_vida_voluntarios_profesionales_c3r7pt", "C3R7PT", "suma",
  "agencias_personal_temporal_obreros_c3r8c1", "C3R8C1", "suma",
  "agencias_personal_temporal_administrativos_c3r8c2", "C3R8C2", "suma",
  "agencias_personal_temporal_total_c3r8c3", "C3R8C3", "suma",
  "agencias_personal_temporal_profesionales_c3r8pt", "C3R8PT", "suma",
  "otros_gastos_personal_obreros_c3r9c1", "C3R9C1", "suma",
  "otros_gastos_personal_administrativos_c3r9c2", "C3R9C2", "suma",
  "otros_gastos_personal_total_c3r9c3", "C3R9C3", "suma",
  "otros_gastos_personal_profesionales_c3r9pt", "C3R9PT", "suma",
  "costos_totales_personal_obreros_c3r10c1", "C3R10C1", "suma",
  "costos_totales_personal_administrativos_c3r10c2", "C3R10C2", "suma",
  "costos_totales_personal_total_c3r10c3", "C3R10C3", "suma",
  "costos_totales_personal_profesionales_c3r10pt", "C3R10PT", "suma",
  "salario_permanentes_salarper", "SALARPER", "suma",
  "salario_permanente_temporal_salpeyte", "SALPEYTE", "suma",
  "prestaciones_permanentes_pressper", "PRESSPER", "suma",
  "prestaciones_permanente_temporal_prespyte", "PRESPYTE", "suma",
  "remuneracion_temporales_remutemp", "REMUTEMP", "suma",
  "salarios_aprendices_salapren", "SALAPREN", "suma",
  "prestaciones_aprendices_perapren", "PERAPREN", "suma",
  "apoyo_sostenimiento_aprendices_r4csap", "R4CSAP", "suma",
  "terceros_produccion_c3r41c1", "C3R41C1", "suma",
  "terceros_administracion_c3r41c2", "C3R41C2", "suma",
  "terceros_total_c3r41c3", "C3R41C3", "suma",
  "terceros_industriales_produccion_c3r14c1", "C3R14C1", "suma",
  "terceros_industriales_total_c3r14c3", "C3R14C3", "suma",
  "honorarios_produccion_c3r15c1", "C3R15C1", "suma",
  "honorarios_administracion_c3r15c2", "C3R15C2", "suma",
  "honorarios_total_c3r15c3", "C3R15C3", "suma",
  "mantenimiento_produccion_c3r23c1", "C3R23C1", "suma",
  "mantenimiento_administracion_c3r23c2", "C3R23C2", "suma",
  "mantenimiento_total_c3r23c3", "C3R23C3", "suma",
  "arriendo_maquinaria_produccion_c3r17c1", "C3R17C1", "suma",
  "arriendo_maquinaria_administracion_c3r17c2", "C3R17C2", "suma",
  "arriendo_maquinaria_total_c3r17c3", "C3R17C3", "suma",
  "inversion_total_c7r10c2", "C7R10C2", "suma",
  "inversion_terrenos_c7r1c2", "C7R1C2", "suma",
  "inversion_edificios_c7r5c2", "C7R5C2", "suma",
  "inversion_maquinaria_c7r6c2", "C7R6C2", "suma",
  "inversion_transporte_c7r8c2", "C7R8C2", "suma",
  "inversion_informatica_c7c4r8", "C7C4R8", "suma",
  "inversion_oficina_c7c5r8", "C7C5R8", "suma",
  "retiros_edificios_c7c2r11", "C7C2R11", "suma",
  "retiros_maquinaria_c7c3r11", "C7C3R11", "suma",
  "retiros_informatica_c7c4r11", "C7C4R11", "suma",
  "retiros_oficina_c7c5r11", "C7C5R11", "suma",
  "retiros_transporte_c7c6r11", "C7C6R11", "suma",
  "retiros_total_c7c7r11", "C7C7R11", "suma",
  "leasing_terrenos_c7c1r19", "C7C1R19", "suma",
  "leasing_edificios_c7c2r19", "C7C2R19", "suma",
  "leasing_maquinaria_c7c3r19", "C7C3R19", "suma",
  "leasing_informatica_c7c4r19", "C7C4R19", "suma",
  "leasing_oficina_c7c5r19", "C7C5R19", "suma",
  "leasing_transporte_c7c6r19", "C7C6R19", "suma",
  "leasing_total_c7c7r19", "C7C7R19", "suma",
  "desvalorizaciones_total_c7r16c7", "C7R16C7", "suma",
  "inversion_bruta_invebrta", "INVEBRTA", "suma",
  "activos_fijos_activfi", "ACTIVFI", "suma",
  "depreciacion_deprecia", "DEPRECIA", "suma",
  "produccion_bruta_prodbr2", "PRODBR2", "suma",
  "produccion_industrial_prodbind", "PRODBIND", "suma",
  "valor_agregado_valagri", "VALAGRI", "suma",
  "consumo_intermedio_consin2", "CONSIN2", "suma",
  "consumo_materias_consmate", "CONSMATE", "suma",
  "otros_gastos_consin", "CONSIN", "suma",
  "valor_ventas_valorven", "VALORVEN", "suma",
  "costos_produccion_c3r35c1", "C3R35C1", "suma",
  "gastos_administracion_ventas_c3r35c2", "C3R35C2", "suma",
  "costos_totales_c3r35c3", "C3R35C3", "suma",
  "intereses_c3r31c3", "C3R31C3", "suma",
  "gastos_financieros_c3r46c3", "C3R46C3", "suma",
  "arriendo_inmuebles_c3r16c3", "C3R16C3", "suma",
  "seguros_c3r18c3", "C3R18C3", "suma",
  "valor_energia_comprada_c3r19c3", "C3R19C3", "suma",
  "agua_c3r21c3", "C3R21C3", "suma",
  "publicidad_c3r22c3", "C3R22C3", "suma",
  "franquicias_marcas_c3r24c3", "C3R24C3", "suma",
  "comunicaciones_c3r36c3", "C3R36C3", "suma",
  "industria_comercio_c3r37c3", "C3R37C3", "suma",
  "predial_vehiculos_c3r38c3", "C3R38C3", "suma",
  "transporte_materias_primas_c3r42c3", "C3R42C3", "suma",
  "transporte_productos_c3r45c3", "C3R45C3", "suma",
  "provisiones_c3r26c3", "C3R26C3", "suma",
  "otros_costos_c3r27c3", "C3R27C3", "suma",
  "costo_productos_no_fabricados_c3r44c3", "C3R44C3", "suma",
  "costo_materias_primas_sin_transformar_c3r40c3", "C3R40C3", "suma",
  "muestras_gratis_c3r13c3", "C3R13C3", "suma",
  "impuesto_4x1000_c3r25c3", "C3R25C3", "suma",
  "materias_primas_inicio_c6r1c1", "C6R1C1", "suma",
  "materias_primas_fin_c6r1c3", "C6R1C3", "suma",
  "productos_proceso_inicio_c6r2c1", "C6R2C1", "suma",
  "productos_proceso_fin_c6r2c3", "C6R2C3", "suma",
  "productos_terminados_inicio_c6r3c1", "C6R3C1", "suma",
  "productos_terminados_fin_c6r3c3", "C6R3C3", "suma",
  "inventario_total_inicio_c6r5c1", "C6R5C1", "suma",
  "inventario_total_fin_c6r5c3", "C6R5C3", "suma",
  "valor_libros_inicio_c7r10c1", "C7R10C1", "suma",
  "valor_libros_fin_c7r10c6", "C7R10C6", "suma",
  "depreciacion_causada_c7r10c7", "C7R10C7", "suma",
  "valorizaciones_c7r10c4", "C7R10C4", "suma",
  "valor_libros_maquinaria_c7r6c6", "C7R6C6", "suma",
  "depreciacion_maquinaria_c7r6c7", "C7R6C7", "suma",
  "activos_nuevos_c7c7r2", "C7C7R2", "suma",
  "activos_usados_c7c7r3", "C7C7R3", "suma",
  "mejoras_activos_c7c7r7", "C7C7R7", "suma",
  "activos_trasladados_c7r17c7", "C7R17C7", "suma",
  "activos_recibidos_c7r18c7", "C7R18C7", "suma",
  "materia_prima_comprada_valorcom", "VALORCOM", "suma",
  "materia_prima_importada_valorcx", "VALORCX", "suma",
  "porcentaje_exportado_porcvt", "PORCVT", "wmean",
  "porcentaje_insumos_importados_porcon", "PORCON", "wmean",
  "energia_comprada_kwh_c5r1c1", "C5R1C1", "suma",
  "energia_consumida_kwh_c5r1c4", "C5R1C4", "suma",
  "energia_electrica_kw_eelec", "EELEC", "suma",
  "valor_energeticos_totalv", "TOTALV", "suma",
  "ingresos_cert_c2r5cert", "C2R5CERT", "suma"
)

codigos_faltantes <- setdiff(mapa_simples$codigo_eam, names(macro_base))
if (length(codigos_faltantes) > 0) {
  message("AVISO -- estos codigos no existen en la macrobase, sus variables quedaran en NA: ",
          paste(codigos_faltantes, collapse = ", "))
}

cols_obreros_permanentes <- c("C4R2C1", "C4R2C2")
cols_administrativos_permanentes <- c("C4R2C3", "C4R2C4")
cols_profesionales_permanentes <- c("C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E")
cols_temporal_directo <- c("C4R3C1", "C4R3C2", "C4R3C3", "C4R3C4", "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E")
cols_temporal_agencias <- c("C4R4C1", "C4R4C2", "C4R4C3", "C4R4C4", "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E")
cols_aprendices <- c("C4R6OM", "C4R6OH", "C4R6DM", "C4R6DH", "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE")
cols_propietarios <- c("C4R1C1", "C4R1C2", "C4R1C3", "C4R1C4", "C4R1C1N", "C4R1C2N", "C4R2C1E", "C4R2C2E")

cols_grupos_c4r <- unique(c(
  cols_obreros, cols_administrativos, cols_prof_tecnico, cols_propietarios,
  cols_obreros_permanentes, cols_administrativos_permanentes, cols_profesionales_permanentes,
  cols_temporal_directo, cols_temporal_agencias, cols_aprendices
))
codigos_c4r_faltantes <- setdiff(cols_grupos_c4r, names(macro_base))
if (length(codigos_c4r_faltantes) > 0) {
  message("AVISO -- estas columnas C4R para los grupos de empleo no existen en la macrobase: ",
          paste(codigos_c4r_faltantes, collapse = ", "))
  for (col in codigos_c4r_faltantes) macro_base[[col]] <- NA_real_
}

sumar_grupo <- function(datos, cols) {
  mat <- as.data.frame(lapply(datos[cols], safe_numeric))
  rowSums(mat, na.rm = TRUE)
}

codigos_simples_presentes <- intersect(unique(mapa_simples$codigo_eam), names(macro_base))

base_establecimiento <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, dplyr::all_of(unique(c(codigos_simples_presentes, cols_grupos_c4r)))) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(unique(c(codigos_simples_presentes, cols_grupos_c4r))), safe_numeric))

variables_grupo <- c(
  "obreros_total", "administrativos_total", "profesionales_total", "propietarios",
  "obreros_permanentes", "administrativos_permanentes", "profesionales_permanentes",
  "temporal_directo", "temporal_agencias", "aprendices"
)

nuevas_establecimiento <- base_establecimiento %>% dplyr::select(NORDEST, NORDEMP, ANIO)
for (i in seq_len(nrow(mapa_simples))) {
  var_nueva <- mapa_simples$variable_nueva[i]
  codigo <- mapa_simples$codigo_eam[i]
  if (codigo %in% names(base_establecimiento) && !(var_nueva %in% names(nuevas_establecimiento))) {
    nuevas_establecimiento[[var_nueva]] <- base_establecimiento[[codigo]]
  } else if (!(var_nueva %in% names(nuevas_establecimiento))) {
    nuevas_establecimiento[[var_nueva]] <- NA_real_
  }
}
nuevas_establecimiento$obreros_total <- sumar_grupo(base_establecimiento, cols_obreros)
nuevas_establecimiento$administrativos_total <- sumar_grupo(base_establecimiento, cols_administrativos)
nuevas_establecimiento$profesionales_total <- sumar_grupo(base_establecimiento, cols_prof_tecnico)
nuevas_establecimiento$propietarios <- sumar_grupo(base_establecimiento, cols_propietarios)
nuevas_establecimiento$obreros_permanentes <- sumar_grupo(base_establecimiento, cols_obreros_permanentes)
nuevas_establecimiento$administrativos_permanentes <- sumar_grupo(base_establecimiento, cols_administrativos_permanentes)
nuevas_establecimiento$profesionales_permanentes <- sumar_grupo(base_establecimiento, cols_profesionales_permanentes)
nuevas_establecimiento$temporal_directo <- sumar_grupo(base_establecimiento, cols_temporal_directo)
nuevas_establecimiento$temporal_agencias <- sumar_grupo(base_establecimiento, cols_temporal_agencias)
nuevas_establecimiento$aprendices <- sumar_grupo(base_establecimiento, cols_aprendices)

variables_suma <- c(unique(mapa_simples$variable_nueva[mapa_simples$tipo == "suma"]), variables_grupo)
variables_wmean <- unique(mapa_simples$variable_nueva[mapa_simples$tipo == "wmean"])

n_establecimientos_tabla <- nuevas_establecimiento %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")

suma_firma <- nuevas_establecimiento %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(variables_suma), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

wmean_firma <- nuevas_establecimiento %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(variables_wmean),
      ~{
        peso <- valor_ventas_valorven
        valido <- !is.na(.x) & !is.na(peso) & peso > 0
        if (!any(valido)) NA_real_ else sum(.x[valido] * peso[valido]) / sum(peso[valido])
      }
    ),
    .groups = "drop"
  )

nuevas_firma <- n_establecimientos_tabla %>%
  dplyr::left_join(suma_firma, by = c("NORDEMP", "ANIO")) %>%
  dplyr::left_join(wmean_firma, by = c("NORDEMP", "ANIO"))

n_dup_firma <- nuevas_firma %>% dplyr::count(NORDEMP, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup_firma > 0) stop("NORDEMP-ANIO no quedo unico tras sumar plantas (", n_dup_firma, " grupos). Revisar.")

# ------------------------------------------------------------------
# 5) Union final: panel_base (con Multi_f) + variables nuevas.
# ------------------------------------------------------------------

panel_con_2020 <- panel_base %>%
  dplyr::left_join(nuevas_firma, by = c("NORDEMP", "ANIO"))

message("panel_con_2020 final: ", nrow(panel_con_2020), " filas, ", ncol(panel_con_2020), " columnas")

# ------------------------------------------------------------------
# 6) Verificacion obligatoria contra el panel actual, en los anios
#    compartidos (todos menos 2020). Si algo no coincide, NO se guarda.
# ------------------------------------------------------------------

panel_actual <- readr::read_rds(file.path(datos_raiz, "panel_analitico_firma_eam.rds"))

message("")
message("=== Verificacion: columnas ===")
cols_solo_actual <- setdiff(names(panel_actual), names(panel_con_2020))
cols_solo_nuevo <- setdiff(names(panel_con_2020), names(panel_actual))
if (length(cols_solo_actual) > 0) message("Columnas SOLO en el panel actual (faltan en el nuevo): ", paste(cols_solo_actual, collapse = ", "))
if (length(cols_solo_nuevo) > 0) message("Columnas SOLO en el panel nuevo (no estaban en el actual): ", paste(cols_solo_nuevo, collapse = ", "))
columnas_ok <- length(cols_solo_actual) == 0 && length(cols_solo_nuevo) == 0
message("Mismas columnas: ", columnas_ok)

message("")
message("=== Verificacion: filas por anio (anios compartidos, sin 2020) ===")
anios_compartidos <- setdiff(sort(unique(panel_actual$ANIO)), 2020)
conteo_actual <- panel_actual %>% dplyr::filter(ANIO %in% anios_compartidos) %>% dplyr::count(ANIO, name = "n_actual")
conteo_nuevo <- panel_con_2020 %>% dplyr::filter(ANIO %in% anios_compartidos) %>% dplyr::count(ANIO, name = "n_nuevo")
comparacion_filas <- conteo_actual %>% dplyr::full_join(conteo_nuevo, by = "ANIO") %>% dplyr::mutate(coincide = n_actual == n_nuevo)
print(comparacion_filas, n = Inf)
filas_ok <- all(comparacion_filas$coincide, na.rm = TRUE) && !any(is.na(comparacion_filas$n_actual)) && !any(is.na(comparacion_filas$n_nuevo))
message("Filas por anio coinciden en todos los anios compartidos: ", filas_ok)

message("")
message("=== Verificacion: valores identicos fila por fila (anios compartidos) ===")
variables_a_comparar <- c(
  "empleo_total", "empleo_permanente", "empleo_temporal", "participacion_permanente",
  "VALORVEN", "Bite2022_obreros", "costos_totales_personal_total_c3r10c3",
  "sueldos_permanentes_obreros_c3r2c1", "obreros_permanentes", "temporal_directo",
  "temporal_agencias", "aprendices"
)

actual_comparable <- panel_actual %>% dplyr::filter(ANIO %in% anios_compartidos) %>% dplyr::arrange(NORDEMP, ANIO)
nuevo_comparable <- panel_con_2020 %>% dplyr::filter(ANIO %in% anios_compartidos) %>% dplyr::arrange(NORDEMP, ANIO)

valores_ok <- TRUE
if (!identical(nrow(actual_comparable), nrow(nuevo_comparable)) ||
    !identical(actual_comparable$NORDEMP, nuevo_comparable$NORDEMP) ||
    !identical(actual_comparable$ANIO, nuevo_comparable$ANIO)) {
  message("ERROR -- las filas (NORDEMP, ANIO) no quedaron en el mismo orden/cantidad tras arrange(). No se puede comparar celda a celda.")
  valores_ok <- FALSE
} else {
  for (v in variables_a_comparar) {
    idénticos <- isTRUE(all.equal(actual_comparable[[v]], nuevo_comparable[[v]], tolerance = 1e-8))
    message(sprintf("  %-45s %s", v, if (idénticos) "OK -- identico" else "DIFIERE"))
    if (!idénticos) {
      valores_ok <- FALSE
      diffs <- which(!mapply(function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-8)) || (is.na(a) && is.na(b)), actual_comparable[[v]], nuevo_comparable[[v]]))
      message("    Primeras filas distintas (NORDEMP, ANIO, actual, nuevo): ")
      for (idx in head(diffs, 5)) {
        message("    ", actual_comparable$NORDEMP[idx], " ", actual_comparable$ANIO[idx], " -- ",
                actual_comparable[[v]][idx], " vs ", nuevo_comparable[[v]][idx])
      }
    }
  }
}
message("Valores identicos en las variables listadas: ", valores_ok)

# ------------------------------------------------------------------
# 7) Guardar SOLO si todo paso.
# ------------------------------------------------------------------

todo_ok <- columnas_ok && filas_ok && valores_ok
if (todo_ok) {
  readr::write_rds(panel_con_2020, file.path(datos_raiz, "panel_analitico_firma_eam_con_2020.rds"))
  message("")
  message("GUARDADO: ", file.path(datos_raiz, "panel_analitico_firma_eam_con_2020.rds"))
} else {
  message("")
  message("NO SE GUARDA -- la verificacion no paso. Revisar los mensajes de arriba.")
}

# ------------------------------------------------------------------
# 8) Resumen de 2020.
# ------------------------------------------------------------------

datos_2020 <- panel_con_2020 %>% dplyr::filter(ANIO == 2020)
message("")
message("=== 2020 ===")
message("Firmas en 2020: ", nrow(datos_2020))
message("Firmas en 2020 con Bite2022_obreros no NA: ", sum(!is.na(datos_2020$Bite2022_obreros)))
