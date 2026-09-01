# Descomposicion de las salidas de la cohorte 2022 (Multi_f, 262 firmas)
# en 2023 y 2024, Paso 3 de feature/panel-establecimiento. Objetivo:
# determinar si la caida post-2022 en la cohorte balanceada
# (construir_panel_efectivo_especificacion_b_por_anio.R, seccion 2:
# 100% en 2022 -> 95.8% en 2023 -> 92.8% en 2024) es churn (perdida de
# una planta puntual, sin salir del panel) o atricion real (la firma
# desaparece de la EAM por completo), y si las firmas que salen tienen
# una exposicion 2022 sistematicamente distinta a las que se mantienen
# (indicio de seleccion inducida por el tratamiento, no ruido).
#
# Para 2023 y 2024 por separado, de las 262 firmas:
# - Cuantas caen por debajo de 2 plantas (n_establecimientos_ese_anio < 2).
# - De esas, cuantas siguen en la EAM con exactamente 1 planta (perdida
#   de planta, la firma sigue reportando) vs. cuantas desaparecen del
#   panel por completo (atricion, 0 establecimientos ese anio).
# - Exposicion PROMEDIO EN 2022 (Exposure2022_obreros, nivel firma) de
#   las que salen (n<2 ese anio) vs. las que se mantienen (n>=2),
#   con diferencia de medias, su error estandar y prueba t (Welch).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - descriptivos_descomposicion_salidas_cohorte_2023.csv
# - descriptivos_descomposicion_salidas_cohorte_2024.csv
# - descriptivos_comparacion_exposure_salen_vs_mantienen_2023_2024.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO"))

base <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO %in% c(2022, 2023, 2024)) %>%
  dplyr::distinct(NORDEST, NORDEMP, ANIO)

n_est_por_firma_anio <- base %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")

firmas_262 <- n_est_por_firma_anio %>%
  dplyr::filter(ANIO == 2022, n_establecimientos > 1) %>%
  dplyr::pull(NORDEMP)

# Exposicion a nivel FIRMA en 2022 (Exposure2022_obreros, ya construida
# en la rama feature/exposicion-obreros-operarios, ya en main).
exposicion_firma_path <- file.path(data_dir, "exposicion_obreros_eam.rds")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_obreros_eam.rds.")
exposicion_firma_2022 <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::filter(ANIO == 2022) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros) %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP))

n_sin_exposicion_firma <- firmas_262[!firmas_262 %in% exposicion_firma_2022$NORDEMP[!is.na(exposicion_firma_2022$Exposure2022_obreros)]]
message("Nota: de las 262 firmas, ", length(n_sin_exposicion_firma), " no tienen Exposure2022_obreros (firma) valida y se excluyen de la comparacion de medias.")

analizar_anio <- function(anio_objetivo) {
  estado_anio <- tibble::tibble(NORDEMP = firmas_262) %>%
    dplyr::left_join(
      n_est_por_firma_anio %>% dplyr::filter(ANIO == anio_objetivo) %>% dplyr::select(NORDEMP, n_establecimientos),
      by = "NORDEMP"
    ) %>%
    dplyr::mutate(n_establecimientos = tidyr::replace_na(n_establecimientos, 0)) %>%
    dplyr::mutate(estado = dplyr::case_when(
      n_establecimientos >= 2 ~ "Se mantiene (>=2 plantas)",
      n_establecimientos == 1 ~ "Perdida de planta (sigue en EAM con 1 planta)",
      n_establecimientos == 0 ~ "Atricion (desaparece del panel EAM)"
    ))

  resumen <- estado_anio %>%
    dplyr::count(estado, name = "n_firmas") %>%
    dplyr::mutate(pct_de_262 = round(100 * n_firmas / length(firmas_262), 2))

  n_bajo_2 <- sum(estado_anio$estado != "Se mantiene (>=2 plantas)")

  list(anio = anio_objetivo, resumen = resumen, n_bajo_2 = n_bajo_2, estado_anio = estado_anio)
}

resultado_2023 <- analizar_anio(2023)
resultado_2024 <- analizar_anio(2024)

readr::write_csv(resultado_2023$resumen, file.path(out_dir, "descriptivos_descomposicion_salidas_cohorte_2023.csv"))
readr::write_csv(resultado_2024$resumen, file.path(out_dir, "descriptivos_descomposicion_salidas_cohorte_2024.csv"))

# ------------------------------------------------------------------
# Comparacion de Exposure2022_obreros (firma): salen (n<2 ese anio) vs
# se mantienen (n>=2), para 2023 y 2024.
# ------------------------------------------------------------------

comparar_exposicion <- function(resultado) {
  datos <- resultado$estado_anio %>%
    dplyr::left_join(exposicion_firma_2022, by = "NORDEMP") %>%
    dplyr::filter(!is.na(Exposure2022_obreros)) %>%
    dplyr::mutate(grupo = ifelse(estado == "Se mantiene (>=2 plantas)", "Se mantiene", "Sale (perdida de planta o atricion)"))

  grupo_sale <- datos %>% dplyr::filter(grupo == "Sale (perdida de planta o atricion)") %>% dplyr::pull(Exposure2022_obreros)
  grupo_mantiene <- datos %>% dplyr::filter(grupo == "Se mantiene") %>% dplyr::pull(Exposure2022_obreros)

  if (length(grupo_sale) < 2 || length(grupo_mantiene) < 2) {
    return(tibble::tibble(
      anio = resultado$anio, grupo = c("Sale", "Se mantiene"),
      n = c(length(grupo_sale), length(grupo_mantiene)),
      media = c(if (length(grupo_sale) > 0) mean(grupo_sale) else NA, if (length(grupo_mantiene) > 0) mean(grupo_mantiene) else NA),
      DE = NA_real_, diferencia_medias = NA_real_, error_estandar_diferencia = NA_real_, t_stat = NA_real_, p_valor = NA_real_
    ))
  }

  prueba_t <- t.test(grupo_sale, grupo_mantiene)

  tibble::tibble(
    anio = resultado$anio,
    grupo = c("Sale (perdida de planta o atricion)", "Se mantiene (>=2 plantas)"),
    n = c(length(grupo_sale), length(grupo_mantiene)),
    media = round(c(mean(grupo_sale), mean(grupo_mantiene)), 4),
    DE = round(c(sd(grupo_sale), sd(grupo_mantiene)), 4),
    diferencia_medias = round(mean(grupo_sale) - mean(grupo_mantiene), 4),
    error_estandar_diferencia = round(unname(prueba_t$stderr), 4),
    t_stat = round(unname(prueba_t$statistic), 3),
    p_valor = signif(prueba_t$p.value, 4)
  )
}

comparacion_2023 <- comparar_exposicion(resultado_2023)
comparacion_2024 <- comparar_exposicion(resultado_2024)
comparacion_completa <- dplyr::bind_rows(comparacion_2023, comparacion_2024)

readr::write_csv(comparacion_completa, file.path(out_dir, "descriptivos_comparacion_exposure_salen_vs_mantienen_2023_2024.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Descomposicion de salidas de la cohorte 2022 (262 firmas), 2023 y 2024")

message("")
message("=== 2023 ===")
print(resultado_2023$resumen, n = Inf, width = Inf)
message("Caen por debajo de 2 plantas en 2023: ", resultado_2023$n_bajo_2, " de 262")

message("")
message("=== 2024 ===")
print(resultado_2024$resumen, n = Inf, width = Inf)
message("Caen por debajo de 2 plantas en 2024: ", resultado_2024$n_bajo_2, " de 262")

message("")
message("=== Comparacion de Exposure2022_obreros (firma, 2022): salen vs se mantienen ===")
print(comparacion_completa, n = Inf, width = Inf)

message("")
message("Tablas exportadas en: ", out_dir)
