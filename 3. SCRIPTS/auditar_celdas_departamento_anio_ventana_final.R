# Paso 2.6 de feature/panel-establecimiento: extender el chequeo de
# celdas departamento-anio (Paso 2.4) a la ventana REAL del panel final
# del DiD, no solo 2015-2019+2023 (esa ventana solo cubria el
# diagnostico preliminar de pre-tendencias).
#
# Ventana del panel final CONFIRMADA (consolidando decisiones ya
# explicitas en otros archivos del repo, no una decision nueva):
# - Pre-periodo: 2015-2019 + 2021-2022 (pendiente explicito en
#   "0. PREPARACION/notas_exposicion_obreros_eam.md", seccion
#   "Pendientes abiertos para la siguiente sesion": "Construir el panel
#   formal 2015-2019 + 2021-2022 para el event study").
# - 2020 EXCLUIDO del pre-periodo: mismo criterio ya usado en
#   "3. SCRIPTS/diagnostico_preliminar_tendencias_2015_2019.R"
#   ("EXCLUYE 2020 explicitamente porque ese anio arranca el choque de
#   la pandemia COVID-19, que introduciria una discontinuidad ajena al
#   diseño de pre-tendencias").
# - Post-periodo: 2023-2024, siguiendo la convencion `periodo_2023` ya
#   usada en todo el proyecto ("3. SCRIPTS/construir_exposicion_obreros_eam.R",
#   "3. SCRIPTS/descriptivo_exposicion_eam.R": `ANIO %in% 2023:2024 ~ "Post (2023-2024)"`).
#
# PANEL_ANIOS_FINAL = 2015,2016,2017,2018,2019,2021,2022,2023,2024 (9 anios).
#
# Este script repite el conteo de celdas departamento-anio del Paso 2.4
# (auditar_distribucion_dpto_establecimiento.R) para esta ventana de 9
# anios (23 departamentos x 9 anios = 207 celdas), y ademas incluye 2020
# como fila de REFERENCIA/COMPARACION (no forma parte del panel final)
# para poder responder explicitamente si 2020 o 2021 cambian el
# panorama en los departamentos chicos ya identificados (Casanare 85,
# Vichada 99).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - auditoria_celdas_dpto_anio_panel_final.csv (23 x 9, panel final)
# - auditoria_celdas_dpto_anio_incluye_2020.csv (23 x 10, con 2020 de referencia)
# - auditoria_celdas_casanare_vichada_todos_anios.csv (detalle anual, incluye 2008-2024 completo)

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "ANIO", "DPTO"))

base <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO), !is.na(DPTO)) %>%
  dplyr::distinct(NORDEST, ANIO, DPTO)

PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)
todos_dptos <- sort(unique(base$DPTO))

# ------------------------------------------------------------------
# 1) Celdas departamento-anio, ventana del panel final (9 anios, 2020
#    excluido).
# ------------------------------------------------------------------

celdas_panel_final <- base %>%
  dplyr::filter(ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::count(DPTO, ANIO, name = "n_establecimientos") %>%
  tidyr::complete(DPTO = todos_dptos, ANIO = PANEL_ANIOS_FINAL, fill = list(n_establecimientos = 0)) %>%
  dplyr::arrange(DPTO, ANIO)

readr::write_csv(celdas_panel_final, file.path(out_dir, "auditoria_celdas_dpto_anio_panel_final.csv"))

umbral_celda_pequena <- 10
celdas_pequenas_final <- celdas_panel_final %>% dplyr::filter(n_establecimientos < umbral_celda_pequena)
celdas_vacias_final <- celdas_panel_final %>% dplyr::filter(n_establecimientos == 0)

resumen_por_dpto_final <- celdas_panel_final %>%
  dplyr::group_by(DPTO) %>%
  dplyr::summarise(
    min_establecimientos_anio = min(n_establecimientos),
    anio_del_minimo = ANIO[which.min(n_establecimientos)],
    max_establecimientos_anio = max(n_establecimientos),
    .groups = "drop"
  ) %>%
  dplyr::arrange(min_establecimientos_anio)

# ------------------------------------------------------------------
# 2) Version con 2020 incluido como REFERENCIA (no es parte del panel
#    final), para responder explicitamente si 2020/2021 cambian el
#    panorama.
# ------------------------------------------------------------------

anios_con_2020 <- sort(c(PANEL_ANIOS_FINAL, 2020))

celdas_con_2020 <- base %>%
  dplyr::filter(ANIO %in% anios_con_2020) %>%
  dplyr::count(DPTO, ANIO, name = "n_establecimientos") %>%
  tidyr::complete(DPTO = todos_dptos, ANIO = anios_con_2020, fill = list(n_establecimientos = 0)) %>%
  dplyr::mutate(en_panel_final = ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::arrange(DPTO, ANIO)

readr::write_csv(celdas_con_2020, file.path(out_dir, "auditoria_celdas_dpto_anio_incluye_2020.csv"))

# ------------------------------------------------------------------
# 3) Detalle anual completo (2008-2024) para Casanare (85) y Vichada
#    (99), los 2 departamentos ya identificados como de menor N y
#    mayor concentracion sectorial (Paso 2.4 y 2.5).
# ------------------------------------------------------------------

detalle_chicos <- base %>%
  dplyr::filter(DPTO %in% c(85, 99)) %>%
  dplyr::count(DPTO, ANIO, name = "n_establecimientos") %>%
  tidyr::complete(DPTO = c(85, 99), ANIO = 2008:2024, fill = list(n_establecimientos = 0)) %>%
  dplyr::mutate(
    en_panel_final = ANIO %in% PANEL_ANIOS_FINAL,
    excluido_por_pandemia = ANIO == 2020
  ) %>%
  dplyr::arrange(DPTO, ANIO)

readr::write_csv(detalle_chicos, file.path(out_dir, "auditoria_celdas_casanare_vichada_todos_anios.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Celdas departamento-anio: ventana del panel final del DiD")

message("")
message("Ventana del panel final: ", paste(PANEL_ANIOS_FINAL, collapse = ", "), " (9 anios, 2020 excluido por pandemia)")
message("")
message("Resumen por departamento (min/max establecimientos-anio en la ventana final, 23 x 9 = 207 celdas):")
print(resumen_por_dpto_final, n = Inf, width = Inf)

message("")
message("Celdas con menos de ", umbral_celda_pequena, " establecimientos: ", nrow(celdas_pequenas_final), " de ", nrow(celdas_panel_final))
message("Celdas completamente vacias: ", nrow(celdas_vacias_final))

message("")
message("Detalle anual completo, Casanare (85) y Vichada (99), 2008-2024 (incluye 2020 y anios fuera del panel final):")
print(detalle_chicos, n = Inf, width = Inf)

message("")
message("Tablas exportadas en: ", out_dir)
