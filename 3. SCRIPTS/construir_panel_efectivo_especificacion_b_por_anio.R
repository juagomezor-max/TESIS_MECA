# Panel efectivo de la especificacion B ("dentro de firma", delta_f(e),t)
# por anio, en la ventana confirmada del panel final (Paso 2.6):
# 2015-2019 + 2021-2022 (pre) + 2023-2024 (post), 2020 excluido por
# pandemia. Paso 3, pendiente.
#
# Para cada uno de los 9 anios de la ventana, DOS tablas distintas que
# NO deben confundirse (son dos poblaciones distintas):
#
# 1. "cualquier firma" (referencia amplia): firmas con >=2
#    establecimientos OBSERVADOS ese anio especifico, sin restringir a
#    ninguna cohorte -- el estatus multiplanta puede cambiar anio a
#    anio, y una firma puede entrar o salir de este conjunto en anios
#    distintos a 2022.
# 2. "cohorte 2022" (la que importa para la especificacion B): de las
#    262 firmas identificadas como multiplanta EN 2022 (Multi_f
#    oficial, Paso 3.2), cuantas de ESAS MISMAS 262 firmas tienen >=2
#    plantas observadas en cada anio de la ventana, y cuantos
#    establecimientos aportan. Esta es la poblacion correcta para medir
#    el panel efectivo de delta_f(e),t, porque la cohorte de firmas que
#    entra a la especificacion B se fija en 2022 (año base de
#    Exposure2022_obreros_est), no se redefine cada año.
#
# Ademas: de las 262 firmas de la cohorte 2022, cuantas MANTIENEN >=2
# plantas en TODOS los 9 anios de la ventana (criterio estricto: si la
# firma no aparece en algun anio de la ventana, no cumple "mantener >=2
# plantas en todos los anios" -- se cuenta como 0 establecimientos ese
# anio, no como dato faltante que se ignora).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - descriptivos_panel_efectivo_especificacion_b_por_anio.csv (cualquier firma, referencia amplia)
# - descriptivos_panel_efectivo_especificacion_b_cohorte_2022_por_anio.csv (cohorte 262, la que importa)
# - descriptivos_panel_efectivo_especificacion_b_persistencia_262.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)  # confirmado en el Paso 2.6

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO"))

base_ventana <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::distinct(NORDEST, NORDEMP, ANIO)

# ------------------------------------------------------------------
# 0) Cohorte oficial: las 262 firmas multiplanta de 2022 (Multi_f,
#    Paso 3.2). Se define ANTES de las tablas por anio porque la
#    tabla relevante (seccion 2) debe restringirse a estas 262 firmas
#    especificas, no a "cualquier firma multiplanta ese anio".
# ------------------------------------------------------------------

base_2022 <- macro_base %>%
  dplyr::mutate(NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP), ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO == 2022) %>%
  dplyr::distinct(NORDEST, NORDEMP)

n_est_2022 <- base_2022 %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_2022 %>% dplyr::filter(n_establecimientos > 1) %>% dplyr::pull(NORDEMP)

n_est_por_firma_anio <- base_ventana %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")

# ------------------------------------------------------------------
# 1) REFERENCIA AMPLIA (NO es la poblacion relevante para la
#    especificacion B): firmas con >=2 establecimientos ese anio,
#    SIN restringir a la cohorte 2022 -- cualquier firma que ese anio
#    en particular tenga 2+ plantas, aunque no haya sido multiplanta
#    en 2022 o haya dejado de serlo. Se mantiene solo como contexto.
# ------------------------------------------------------------------

panel_por_anio <- n_est_por_firma_anio %>%
  dplyr::filter(n_establecimientos >= 2) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    firmas_multiplanta_ese_anio = dplyr::n(),
    establecimientos_en_esas_firmas = sum(n_establecimientos),
    .groups = "drop"
  ) %>%
  tidyr::complete(ANIO = PANEL_ANIOS_FINAL, fill = list(firmas_multiplanta_ese_anio = 0, establecimientos_en_esas_firmas = 0)) %>%
  dplyr::arrange(ANIO)

readr::write_csv(panel_por_anio, file.path(out_dir, "descriptivos_panel_efectivo_especificacion_b_por_anio.csv"))

# ------------------------------------------------------------------
# 2) COHORTE 2022 (la poblacion CORRECTA para medir el panel efectivo
#    de la especificacion B): de las MISMAS 262 firmas identificadas
#    como multiplanta en 2022, cuantas tienen >=2 plantas OBSERVADAS
#    en cada anio de la ventana, y cuantos establecimientos aportan
#    esas firmas ese anio (0 si la firma no aparece o tiene <2 plantas
#    ese anio).
# ------------------------------------------------------------------

panel_cohorte_2022_por_anio <- n_est_por_firma_anio %>%
  dplyr::filter(NORDEMP %in% firmas_262) %>%
  tidyr::complete(NORDEMP = firmas_262, ANIO = PANEL_ANIOS_FINAL, fill = list(n_establecimientos = 0)) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    firmas_cohorte_2022_con_2plus_ese_anio = sum(n_establecimientos >= 2),
    establecimientos_aportados_ese_anio = sum(n_establecimientos[n_establecimientos >= 2]),
    .groups = "drop"
  ) %>%
  dplyr::mutate(pct_de_las_262 = round(100 * firmas_cohorte_2022_con_2plus_ese_anio / length(firmas_262), 2)) %>%
  dplyr::arrange(ANIO)

readr::write_csv(panel_cohorte_2022_por_anio, file.path(out_dir, "descriptivos_panel_efectivo_especificacion_b_cohorte_2022_por_anio.csv"))

# ------------------------------------------------------------------
# 3) De las 262 firmas de la cohorte 2022, cuantas mantienen >=2
#    plantas en TODOS los 9 anios de la ventana.
# ------------------------------------------------------------------

persistencia <- n_est_por_firma_anio %>%
  dplyr::filter(NORDEMP %in% firmas_262) %>%
  tidyr::complete(NORDEMP = firmas_262, ANIO = PANEL_ANIOS_FINAL, fill = list(n_establecimientos = 0)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(
    n_anios_con_2plus = sum(n_establecimientos >= 2),
    mantiene_2plus_todos_los_anios = all(n_establecimientos >= 2),
    .groups = "drop"
  )

n_persistentes <- sum(persistencia$mantiene_2plus_todos_los_anios)

distribucion_persistencia <- persistencia %>%
  dplyr::count(n_anios_con_2plus, name = "n_firmas") %>%
  dplyr::mutate(pct = round(100 * n_firmas / sum(n_firmas), 2)) %>%
  dplyr::arrange(dplyr::desc(n_anios_con_2plus))

readr::write_csv(distribucion_persistencia, file.path(out_dir, "descriptivos_panel_efectivo_especificacion_b_persistencia_262.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Panel efectivo de la especificacion B ('dentro de firma') por anio")

message("")
message("Ventana: ", paste(PANEL_ANIOS_FINAL, collapse = ", "), " (9 anios, 2020 excluido)")
message("")
message("1) REFERENCIA AMPLIA (cualquier firma, NO es la poblacion relevante para la especificacion B):")
print(panel_por_anio, n = Inf, width = Inf)

message("")
message("2) COHORTE 2022 (poblacion CORRECTA: las MISMAS 262 firmas de Multi_f, por anio):")
print(panel_cohorte_2022_por_anio, n = Inf, width = Inf)

message("")
message("3) De las 262 firmas de la cohorte 2022, distribucion de cuantos de los 9 anios mantienen >=2 plantas:")
print(distribucion_persistencia, n = Inf, width = Inf)
message("")
message(">>> Firmas que mantienen >=2 plantas en LOS 9 ANIOS de la ventana: ", n_persistentes, " de 262 <<<")

message("")
message("Tablas exportadas en: ", out_dir)
