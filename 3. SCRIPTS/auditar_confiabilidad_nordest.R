# Auditoria de confiabilidad de NORDEST (identificador de establecimiento,
# EAM) como panel longitudinal, con el mismo rigor aplicado a NORDEMP en
# auditar_deduplicacion_nordemp_anio.R y auditar_empleo_total_vs_dane.R
# (rama feature/auditoria-reproducibilidad-macrobase, ya en main).
#
# Contexto: se evalua mover la unidad de observacion del diseño DiD de
# empresa-anio (NORDEMP-ANIO) a establecimiento-anio (NORDEST-ANIO).
# Antes de reconstruir nada, hay que confirmar que NORDEST es un
# identificador tan confiable y longitudinal como ya se confirmo que es
# NORDEMP.
#
# Preguntas (Paso 1.3 de feature/panel-establecimiento):
# 1. Establecimientos unicos por anio, 2008-2024, y como cambia el conteo.
# 2. Estabilidad del NORDEST para una misma planta: la macrobase EAM NO
#    tiene columnas de direccion/municipio/domicilio (unicamente DPTO,
#    geografia gruesa), asi que no se puede verificar "misma direccion,
#    distinto NORDEST" directamente. Proxy usado: entre firmas de UN SOLO
#    establecimiento (NORDEMP con 1 NORDEST en el anio t), se revisa si
#    el NORDEST de esa firma cambia de un anio al siguiente mientras el
#    NORDEMP sigue reportando -- evidencia indirecta de recodificacion
#    del identificador (asumiendo que la mayoria de firmas de una sola
#    planta no cierran una planta y abren una identica el mismo periodo).
# 3. Proporcion de NORDEST que aparecen en un solo anio (posibles
#    altas/bajas reales) vs. panel largo.
# 4. Cada NORDEST asociado a un unico NORDEMP en todo el periodo, o hay
#    establecimientos que cambian de empresa duena (M&A, spin-off,
#    recodificacion). Cuantificacion y propuesta de tratamiento.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - auditoria_nordest_establecimientos_por_anio.csv
# - auditoria_nordest_anios_por_establecimiento.csv
# - auditoria_nordest_recodificacion_sospechosa.csv
# - auditoria_nordest_cambio_nordemp.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$resultados_validaciones

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "NORDEST", "ANIO", "CIIU4", "DPTO"))

base <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    NORDEST = as.character(NORDEST),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO))

# ------------------------------------------------------------------
# 1) Establecimientos unicos por anio, 2008-2024
# ------------------------------------------------------------------

establecimientos_por_anio <- base %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    filas = dplyr::n(),
    establecimientos = dplyr::n_distinct(NORDEST),
    empresas = dplyr::n_distinct(NORDEMP),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ANIO) %>%
  dplyr::mutate(
    var_pct_establecimientos = round(100 * (establecimientos / dplyr::lag(establecimientos) - 1), 2)
  )

readr::write_csv(establecimientos_por_anio, file.path(data_dir, "auditoria_nordest_establecimientos_por_anio.csv"))

# ------------------------------------------------------------------
# 2) Estabilidad del NORDEST para firmas de un solo establecimiento:
#    proporcion de casos en que el NORDEMP persiste de un anio al
#    siguiente pero su (unico) NORDEST cambia.
# ------------------------------------------------------------------

est_por_firma_anio <- base %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(n_establecimientos_anio = dplyr::n_distinct(NORDEST), .groups = "drop")

firmas_un_establecimiento <- base %>%
  dplyr::inner_join(
    est_por_firma_anio %>% dplyr::filter(n_establecimientos_anio == 1),
    by = c("NORDEMP", "ANIO")
  ) %>%
  dplyr::distinct(NORDEMP, ANIO, NORDEST) %>%
  dplyr::arrange(NORDEMP, ANIO)

recodificacion_sospechosa <- firmas_un_establecimiento %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::mutate(
    ANIO_previo = dplyr::lag(ANIO),
    NORDEST_previo = dplyr::lag(NORDEST),
    anio_consecutivo = !is.na(ANIO_previo) & (ANIO - ANIO_previo == 1),
    nordest_cambio = anio_consecutivo & (NORDEST != NORDEST_previo)
  ) %>%
  dplyr::ungroup()

resumen_recodificacion <- recodificacion_sospechosa %>%
  dplyr::filter(anio_consecutivo) %>%
  dplyr::summarise(
    pares_anio_consecutivo = dplyr::n(),
    pares_con_cambio_nordest = sum(nordest_cambio),
    pct_con_cambio_nordest = round(100 * mean(nordest_cambio), 3)
  )

casos_recodificacion <- recodificacion_sospechosa %>%
  dplyr::filter(nordest_cambio) %>%
  dplyr::select(NORDEMP, ANIO_previo, NORDEST_previo, ANIO, NORDEST_nuevo = NORDEST)

readr::write_csv(casos_recodificacion, file.path(data_dir, "auditoria_nordest_recodificacion_sospechosa.csv"))

# ------------------------------------------------------------------
# 3) Distribucion de anios presentes por NORDEST (panel corto vs largo)
# ------------------------------------------------------------------

anios_por_establecimiento <- base %>%
  dplyr::distinct(NORDEST, ANIO) %>%
  dplyr::count(NORDEST, name = "n_anios_presente")

distribucion_anios <- anios_por_establecimiento %>%
  dplyr::count(n_anios_presente, name = "n_establecimientos") %>%
  dplyr::arrange(n_anios_presente) %>%
  dplyr::mutate(pct = round(100 * n_establecimientos / sum(n_establecimientos), 2))

readr::write_csv(distribucion_anios, file.path(data_dir, "auditoria_nordest_anios_por_establecimiento.csv"))

pct_un_solo_anio <- distribucion_anios %>% dplyr::filter(n_anios_presente == 1) %>% dplyr::pull(pct)
pct_panel_completo <- distribucion_anios %>% dplyr::filter(n_anios_presente == 17) %>% dplyr::pull(pct)

# ------------------------------------------------------------------
# 4) NORDEST asociados a mas de un NORDEMP distinto en el periodo
#    (posible cambio de empresa duena).
# ------------------------------------------------------------------

nordemp_por_establecimiento <- base %>%
  dplyr::distinct(NORDEST, NORDEMP) %>%
  dplyr::count(NORDEST, name = "n_nordemp_distintos")

establecimientos_multi_dueno <- nordemp_por_establecimiento %>%
  dplyr::filter(n_nordemp_distintos > 1)

detalle_multi_dueno <- base %>%
  dplyr::semi_join(establecimientos_multi_dueno, by = "NORDEST") %>%
  dplyr::distinct(NORDEST, NORDEMP, ANIO) %>%
  dplyr::arrange(NORDEST, ANIO)

readr::write_csv(detalle_multi_dueno, file.path(data_dir, "auditoria_nordest_cambio_nordemp.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Auditoria de confiabilidad de NORDEST (panel longitudinal)")

message("")
message("1) Establecimientos unicos por anio (2008-2024):")
print(establecimientos_por_anio, n = Inf, width = Inf)

message("")
message("2) Estabilidad del NORDEST en firmas de un solo establecimiento (pares anio-consecutivo):")
print(resumen_recodificacion, width = Inf)
message("   Casos de recodificacion sospechosa exportados: ", nrow(casos_recodificacion))

message("")
message("3) Distribucion de anios presentes por NORDEST (2008-2024, ", nrow(anios_por_establecimiento), " establecimientos unicos):")
print(distribucion_anios, n = Inf, width = Inf)
message("   % en un solo anio: ", pct_un_solo_anio, "%  |  % en los 17 anios: ", pct_panel_completo, "%")

message("")
message("4) NORDEST asociados a mas de un NORDEMP distinto en el periodo:")
message("   Establecimientos con >1 NORDEMP: ", nrow(establecimientos_multi_dueno),
        " de ", nrow(nordemp_por_establecimiento),
        " (", round(100 * nrow(establecimientos_multi_dueno) / nrow(nordemp_por_establecimiento), 3), "%)")

message("")
message("Tablas exportadas en: ", data_dir)
