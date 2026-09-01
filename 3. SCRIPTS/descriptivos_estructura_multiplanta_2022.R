# Descriptivos de estructura multiplanta en 2022 (Paso 3 de
# feature/panel-establecimiento), para decidir si el diseño "dentro de
# firma" (con delta_f(e),t, es decir explotar variacion de exposicion
# ENTRE establecimientos de una misma empresa) tiene algo real que
# identificar.
#
# IMPORTANTE: las "447 firmas multiplanta" del Paso 1.5
# (auditar_recodificacion_multiplanta_nordest.R) se definieron como
# NORDEMP con >1 NORDEST en ALGUN anio del periodo 2008-2024 -- no
# especificamente en 2022. Como Exposure2022_obreros_est es un atributo
# medido en 2022 (linea base pre-choque), lo relevante para el diseño
# "dentro de firma" es la estructura multiplanta EN 2022, que puede ser
# un numero distinto. Este script recalcula el universo de firmas
# multiplanta especificamente en 2022 y lo compara con el numero del
# Paso 1.5.
#
# Preguntas:
# 1. Distribucion de N establecimientos por firma multiplanta en 2022
#    (2, 3, 4+).
# 2. Departamentos distintos por firma en 2022 (1 = multiplanta mas
#    concentrada geograficamente; 2+ = sedes en departamentos distintos,
#    el caso relevante para el diseño).
# 3. Variacion interna de Exposure2022_obreros_est dentro de cada firma
#    (max-min y desviacion estandar entre sus establecimientos).
# 4. Umbral de "variacion sustancial" (15-20pp, justificado abajo) y
#    conteo de firmas que lo superan -- la cifra que responde si el
#    diseño "dentro de firma" tiene algo mas que un puñado de casos que
#    identificar.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - descriptivos_multiplanta_2022_reconciliacion.csv (missingness y
#   reconciliacion explicita de los cortes 447 / 262 / 260)
# - descriptivos_multiplanta_2022_resumen.csv
# - descriptivos_multiplanta_2022_distribucion_n_establecimientos.csv
# - descriptivos_multiplanta_2022_distribucion_n_departamentos.csv
# - descriptivos_multiplanta_2022_variacion_interna.csv (una fila por firma)
# - descriptivos_multiplanta_2022_umbral_variacion.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

ANIO_BASE <- 2022

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", "DPTO"))

exposicion_est_path <- file.path(data_dir, "exposicion_obreros_establecimiento_eam.rds")
if (!file.exists(exposicion_est_path)) {
  stop("Falta exposicion_obreros_establecimiento_eam.rds. Corre construir_exposicion_obreros_establecimiento_eam.R primero.")
}

# ------------------------------------------------------------------
# 0) Universo de firmas multiplanta ESPECIFICAMENTE en 2022 (no el
#    universo 2008-2024 del Paso 1.5).
# ------------------------------------------------------------------

base_2022 <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP, DPTO)

n_establecimientos_por_firma_2022 <- base_2022 %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")

firmas_multiplanta_2022 <- n_establecimientos_por_firma_2022 %>%
  dplyr::filter(n_establecimientos > 1) %>%
  dplyr::pull(NORDEMP)

n_multiplanta_2022 <- length(firmas_multiplanta_2022)
n_multiplanta_paso1_5 <- 447  # referencia: auditar_recodificacion_multiplanta_nordest.R, universo 2008-2024

message("Firmas multiplanta en 2022 (esta medicion, Multi_f oficial): ", n_multiplanta_2022)
message("Firmas multiplanta 2008-2024 (Paso 1.5, referencia, NO aplica aqui -- corte temporal distinto): ", n_multiplanta_paso1_5)

# ------------------------------------------------------------------
# 0b) Reconciliacion explicita: missingness general de
#    Exposure2022_obreros_est en 2022, y por que 262 (universo Multi_f)
#    se reduce a 260 (base del calculo de variacion interna).
# ------------------------------------------------------------------

exposicion_est_2022_completa <- readr::read_rds(exposicion_est_path) %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP, Exposure2022_obreros_est)

n_establecimientos_total_2022 <- dplyr::n_distinct(base_2022$NORDEST)
n_establecimientos_validos_2022 <- sum(!is.na(exposicion_est_2022_completa$Exposure2022_obreros_est))
n_establecimientos_faltantes_2022 <- n_establecimientos_total_2022 - n_establecimientos_validos_2022

detalle_multiplanta_missingness <- base_2022 %>%
  dplyr::filter(NORDEMP %in% firmas_multiplanta_2022) %>%
  dplyr::left_join(exposicion_est_2022_completa %>% dplyr::select(NORDEST, Exposure2022_obreros_est), by = "NORDEST") %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(
    n_establecimientos_total = dplyr::n(),
    n_con_exposicion_valida = sum(!is.na(Exposure2022_obreros_est)),
    .groups = "drop"
  )

n_firmas_0_validos <- sum(detalle_multiplanta_missingness$n_con_exposicion_valida == 0)
n_firmas_1_valido <- sum(detalle_multiplanta_missingness$n_con_exposicion_valida == 1)
n_firmas_2plus_validos <- sum(detalle_multiplanta_missingness$n_con_exposicion_valida >= 2)

n_establecimientos_multiplanta_total <- sum(detalle_multiplanta_missingness$n_establecimientos_total)
n_establecimientos_multiplanta_validos <- sum(detalle_multiplanta_missingness$n_con_exposicion_valida)
n_establecimientos_multiplanta_faltantes <- n_establecimientos_multiplanta_total - n_establecimientos_multiplanta_validos

reconciliacion <- tibble::tibble(
  metrica = c(
    "Establecimientos totales en 2022 (macrobase)",
    "Establecimientos con Exposure2022_obreros_est valida en 2022",
    "Establecimientos con Exposure2022_obreros_est FALTANTE en 2022",
    "Firmas multiplanta 2008-2024 (Paso 1.5, NO aplica -- corte temporal distinto)",
    "Firmas multiplanta EN 2022 (Multi_f oficial de este analisis)",
    "  ... de esas, con 0 establecimientos con exposicion valida",
    "  ... de esas, con exactamente 1 establecimiento con exposicion valida (excluidas del calculo de rango)",
    "  ... de esas, con 2+ establecimientos con exposicion valida (base oficial del calculo de variacion interna)",
    "Establecimientos totales dentro de firmas multiplanta 2022",
    "  ... con exposicion valida",
    "  ... FALTANTES (missingness dentro de multiplanta)"
  ),
  valor = c(
    n_establecimientos_total_2022,
    n_establecimientos_validos_2022,
    n_establecimientos_faltantes_2022,
    n_multiplanta_paso1_5,
    n_multiplanta_2022,
    n_firmas_0_validos,
    n_firmas_1_valido,
    n_firmas_2plus_validos,
    n_establecimientos_multiplanta_total,
    n_establecimientos_multiplanta_validos,
    n_establecimientos_multiplanta_faltantes
  )
)

readr::write_csv(reconciliacion, file.path(out_dir, "descriptivos_multiplanta_2022_reconciliacion.csv"))

message("")
message("=== RECONCILIACION (447 vs 262 vs 260, y missingness) ===")
print(reconciliacion, n = Inf, width = Inf)
message("Verificacion: ", n_firmas_0_validos, " + ", n_firmas_1_valido, " + ", n_firmas_2plus_validos,
        " = ", n_firmas_0_validos + n_firmas_1_valido + n_firmas_2plus_validos, " (debe ser ", n_multiplanta_2022, ")")

# ------------------------------------------------------------------
# 1) Distribucion de N establecimientos por firma multiplanta en 2022.
# ------------------------------------------------------------------

distribucion_n_establecimientos <- n_establecimientos_por_firma_2022 %>%
  dplyr::filter(NORDEMP %in% firmas_multiplanta_2022) %>%
  dplyr::mutate(categoria_n = dplyr::case_when(
    n_establecimientos == 2 ~ "2",
    n_establecimientos == 3 ~ "3",
    n_establecimientos >= 4 ~ "4+",
    TRUE ~ NA_character_
  )) %>%
  dplyr::count(categoria_n, name = "n_firmas") %>%
  dplyr::mutate(pct = round(100 * n_firmas / sum(n_firmas), 2)) %>%
  dplyr::arrange(factor(categoria_n, levels = c("2", "3", "4+")))

readr::write_csv(distribucion_n_establecimientos, file.path(out_dir, "descriptivos_multiplanta_2022_distribucion_n_establecimientos.csv"))

# ------------------------------------------------------------------
# 2) Departamentos distintos por firma en 2022.
# ------------------------------------------------------------------

n_dptos_por_firma_2022 <- base_2022 %>%
  dplyr::filter(NORDEMP %in% firmas_multiplanta_2022, !is.na(DPTO)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(n_departamentos_distintos = dplyr::n_distinct(DPTO), .groups = "drop")

distribucion_n_departamentos <- n_dptos_por_firma_2022 %>%
  dplyr::mutate(categoria_dpto = ifelse(n_departamentos_distintos == 1, "1 (concentrada)", "2+ (dispersa)")) %>%
  dplyr::count(categoria_dpto, name = "n_firmas") %>%
  dplyr::mutate(pct = round(100 * n_firmas / sum(n_firmas), 2))

readr::write_csv(distribucion_n_departamentos, file.path(out_dir, "descriptivos_multiplanta_2022_distribucion_n_departamentos.csv"))

n_multi_dpto <- n_dptos_por_firma_2022 %>% dplyr::filter(n_departamentos_distintos > 1) %>% nrow()

# ------------------------------------------------------------------
# 3) Variacion interna de Exposure2022_obreros_est dentro de cada firma
#    multiplanta.
# ------------------------------------------------------------------

exposicion_est_2022 <- readr::read_rds(exposicion_est_path) %>%
  dplyr::filter(ANIO == ANIO_BASE) %>%
  dplyr::distinct(NORDEST, NORDEMP, Exposure2022_obreros_est)

variacion_interna <- exposicion_est_2022 %>%
  dplyr::filter(NORDEMP %in% firmas_multiplanta_2022, !is.na(Exposure2022_obreros_est)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(
    n_establecimientos_con_exposicion = dplyr::n(),
    exposicion_min = min(Exposure2022_obreros_est),
    exposicion_max = max(Exposure2022_obreros_est),
    rango_max_menos_min = round(exposicion_max - exposicion_min, 4),
    desviacion_estandar = if (dplyr::n() >= 2) round(sd(Exposure2022_obreros_est), 4) else NA_real_,
    .groups = "drop"
  ) %>%
  dplyr::left_join(n_dptos_por_firma_2022, by = "NORDEMP") %>%
  dplyr::arrange(dplyr::desc(rango_max_menos_min))

readr::write_csv(variacion_interna, file.path(out_dir, "descriptivos_multiplanta_2022_variacion_interna.csv"))

n_con_2_mas_establecimientos_exposicion_valida <- variacion_interna %>%
  dplyr::filter(n_establecimientos_con_exposicion >= 2) %>%
  nrow()

resumen_variacion <- variacion_interna %>%
  dplyr::filter(n_establecimientos_con_exposicion >= 2) %>%
  dplyr::summarise(
    n_firmas = dplyr::n(),
    rango_promedio = round(mean(rango_max_menos_min), 4),
    rango_mediana = round(median(rango_max_menos_min), 4),
    rango_p75 = round(quantile(rango_max_menos_min, 0.75), 4),
    rango_p90 = round(quantile(rango_max_menos_min, 0.90), 4),
    rango_max = round(max(rango_max_menos_min), 4)
  )

# ------------------------------------------------------------------
# 4) Umbral de "variacion sustancial": 15 puntos porcentuales (0.15 en
#    la escala 0-1 de Exposure2022_obreros_est).
#
#    Justificacion del umbral: Exposure2022_obreros_est es una
#    PROPORCION (participacion de obreros en el empleo, escala 0-1). Un
#    umbral de 15pp es conservador frente a la dispersion total de la
#    medida: el rango intercuartilico tipico de Exposure2022_obreros a
#    nivel empresa (ver diagnosticos_validacion_exposicion_obreros_eam.R,
#    rama feature/exposicion-obreros-operarios) cubre buena parte de la
#    escala 0-1, asi que 15pp representa una diferencia economicamente
#    no trivial entre dos establecimientos de la misma firma -- no un
#    ruido de redondeo o de winsorizacion. Se reporta tambien el umbral
#    alternativo de 20pp (limite superior del rango sugerido) para
#    verificar sensibilidad.
# ------------------------------------------------------------------

UMBRAL_15PP <- 0.15
UMBRAL_20PP <- 0.20

umbral_variacion <- tibble::tibble(
  umbral_pp = c(15, 20),
  umbral_decimal = c(UMBRAL_15PP, UMBRAL_20PP)
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    n_firmas_supera_umbral = sum(variacion_interna$rango_max_menos_min[variacion_interna$n_establecimientos_con_exposicion >= 2] >= umbral_decimal),
    n_firmas_base = n_con_2_mas_establecimientos_exposicion_valida,
    pct_supera_umbral = round(100 * n_firmas_supera_umbral / n_firmas_base, 2)
  ) %>%
  dplyr::ungroup()

readr::write_csv(umbral_variacion, file.path(out_dir, "descriptivos_multiplanta_2022_umbral_variacion.csv"))

# ------------------------------------------------------------------
# Resumen consolidado
# ------------------------------------------------------------------

resumen <- tibble::tibble(
  n_multiplanta_2022 = n_multiplanta_2022,
  n_multiplanta_paso1_5_2008_2024 = n_multiplanta_paso1_5,
  n_multiplanta_con_2plus_departamentos = n_multi_dpto,
  n_multiplanta_con_exposicion_valida_2plus_est = n_con_2_mas_establecimientos_exposicion_valida,
  n_supera_umbral_15pp = umbral_variacion$n_firmas_supera_umbral[umbral_variacion$umbral_pp == 15],
  n_supera_umbral_20pp = umbral_variacion$n_firmas_supera_umbral[umbral_variacion$umbral_pp == 20]
)

readr::write_csv(resumen, file.path(out_dir, "descriptivos_multiplanta_2022_resumen.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Descriptivos de estructura multiplanta en 2022")

message("")
message("0) Universo de firmas multiplanta EN 2022: ", n_multiplanta_2022)
message("")
message("1) Distribucion de N establecimientos por firma (2022):")
print(distribucion_n_establecimientos, width = Inf)

message("")
message("2) Departamentos distintos por firma (2022):")
print(distribucion_n_departamentos, width = Inf)
message("Firmas con presencia en 2+ departamentos: ", n_multi_dpto, " de ", nrow(n_dptos_por_firma_2022))

message("")
message("3) Variacion interna de Exposure2022_obreros_est (firmas con >=2 establecimientos con exposicion valida, n=",
        n_con_2_mas_establecimientos_exposicion_valida, "):")
print(resumen_variacion, width = Inf)

message("")
message("4) Firmas que superan el umbral de variacion sustancial:")
print(umbral_variacion, width = Inf)

message("")
message("Resumen consolidado:")
print(resumen, width = Inf)

message("")
message("Tablas exportadas en: ", out_dir)
