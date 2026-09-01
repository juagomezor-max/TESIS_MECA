# PASO 1 de feature/estimacion-preliminar: construir el panel FORMAL a
# nivel establecimiento-anio, aplicando TODAS las decisiones ya
# validadas y comiteadas en panel-establecimiento-v1 (ver
# NOTA_PREANALISIS.md). No corre ninguna regresion de empleo -- solo
# ensambla el panel.
#
# Decisiones aplicadas (cada una con su fuente):
# - Ventana: 2015-2019 + 2021-2022 (pre) + 2023-2024 (post), 2020
#   excluido por pandemia. 9 anios. (notas_panel_establecimiento.md, Paso 2.6)
# - Unidad: NORDEST-ANIO (establecimiento), confirmado unico en el 100%
#   del panel. (Paso 1.3)
# - DPTO fijo por establecimiento, regla diferenciada APROBADA: para
#   los 465 establecimientos con DPTO inestable, `cambio_sostenido` usa
#   el valor de 2022 (año base de exposicion); `salto_aislado` y
#   `patron_irregular` usan el modal. Los 12,156 establecimientos
#   estables usan su unico valor. (Paso 2.3)
# - Exposicion principal: Exposure2022_obreros_est (establecimiento,
#   propia) y Exposure2022_obreros (firma, heredada) -- ambas
#   incluidas, cada especificacion elige cual usar.
# - Controles obligatorios: sector (CIIU4) y tamaño, TIME-VARYING
#   (observados cada anio, no fijados en la linea base) para poder
#   interactuar con anio en la especificacion (sector*anio, tamano*anio).
# - Cohorte balanceada de la especificacion "dentro de firma" (B): 181
#   firmas con >=2 plantas en los 9 anios de la ventana, marcada como
#   columna indicadora (no filtrada aqui -- cada especificacion decide
#   si la usa). (README_exposicion_establecimiento.md, Paso 3.7)
#
# Salidas (no versionadas, mismo criterio que el resto de "construir_*"):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/panel_formal_establecimiento_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
val_dir <- paths$resultados_validaciones

PANEL_ANIOS_FINAL <- c(2015:2019, 2021:2024)

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "NORDEMP", "ANIO", "CIIU4", "DPTO"))

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_divide <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)

sum_if_exists <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) return(rep(NA_real_, nrow(data)))
  out <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), safe_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(.sum = rowSums(dplyr::across(dplyr::everything()))) %>%
    dplyr::pull(.sum)
  all_missing <- data %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(present), ~is.na(safe_numeric(.x)))) %>%
    dplyr::mutate(.all_missing = dplyr::if_all(dplyr::everything(), identity)) %>%
    dplyr::pull(.all_missing)
  out[all_missing] <- NA_real_
  out
}

tamano_de <- function(empleo) {
  dplyr::case_when(
    is.na(empleo) ~ NA_character_,
    empleo < 50 ~ "Pequena",
    empleo < 200 ~ "Mediana",
    TRUE ~ "Grande"
  )
}

# ------------------------------------------------------------------
# 1) Base cruda: NORDEST-ANIO en la ventana, con columnas C4R de
#    empleo por tipo de vinculacion (identicas a scripts previos,
#    confirmadas estables 2008-2024).
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
cols_necesarias <- unique(c(cols_obreros, cols_administrativos, cols_prof_tecnico))

check_required_vars(macro_base, cols_necesarias)

base_ventana <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST), NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", ANIO %in% PANEL_ANIOS_FINAL) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, CIIU4, DPTO, dplyr::all_of(cols_necesarias)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_necesarias), safe_numeric))

n_dup <- base_ventana %>% dplyr::count(NORDEST, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup > 0) stop("NORDEST-ANIO no es unico en la ventana (", n_dup, " grupos). Revisar antes de continuar.")

panel <- base_ventana %>%
  dplyr::mutate(
    total_obreros = sum_if_exists(., cols_obreros),
    total_administrativos = sum_if_exists(., cols_administrativos),
    total_prof_tecnico = sum_if_exists(., cols_prof_tecnico),
    empleo_total = total_obreros + total_administrativos + total_prof_tecnico,
    empleo_permanente = sum_if_exists(., cols_permanente),
    empleo_temporal = sum_if_exists(., cols_temporal),
    participacion_permanente = safe_divide(empleo_permanente, empleo_total) * 100,
    tamano_empresa = tamano_de(empleo_total)
  ) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, CIIU4, DPTO, tamano_empresa,
                empleo_total, empleo_permanente, empleo_temporal, participacion_permanente)

# ------------------------------------------------------------------
# 2) DPTO fijo por establecimiento (regla diferenciada aprobada, Paso 2.3).
# ------------------------------------------------------------------

casos_inestables_path <- file.path(val_dir, "auditoria_dpto_estabilidad_nordest_casos.csv")
if (!file.exists(casos_inestables_path)) stop("Falta auditoria_dpto_estabilidad_nordest_casos.csv (Paso 2.3).")
casos_inestables <- readr::read_csv(casos_inestables_path, show_col_types = FALSE) %>%
  dplyr::mutate(NORDEST = as.character(NORDEST))

dpto_2022 <- base_ventana %>%
  dplyr::filter(ANIO == 2022, !is.na(DPTO)) %>%
  dplyr::distinct(NORDEST, DPTO_2022 = DPTO)

moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tabla <- table(x)
  as.numeric(names(tabla)[which.max(tabla)])
}

dpto_modal <- base_ventana %>%
  dplyr::filter(!is.na(DPTO)) %>%
  dplyr::group_by(NORDEST) %>%
  dplyr::summarise(DPTO_modal = moda(DPTO), .groups = "drop")

dpto_unico <- base_ventana %>%
  dplyr::filter(!is.na(DPTO)) %>%
  dplyr::distinct(NORDEST, DPTO) %>%
  dplyr::group_by(NORDEST) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::rename(DPTO_unico = DPTO)

dpto_fijo <- dplyr::tibble(NORDEST = unique(base_ventana$NORDEST)) %>%
  dplyr::left_join(casos_inestables %>% dplyr::select(NORDEST, patron), by = "NORDEST") %>%
  dplyr::left_join(dpto_unico, by = "NORDEST") %>%
  dplyr::left_join(dpto_2022, by = "NORDEST") %>%
  dplyr::left_join(dpto_modal, by = "NORDEST") %>%
  dplyr::mutate(
    DPTO_fijo = dplyr::case_when(
      is.na(patron) ~ DPTO_unico,                                    # estable: unico valor
      patron == "cambio_sostenido" ~ dplyr::coalesce(DPTO_2022, DPTO_modal),  # 2022, fallback modal
      TRUE ~ DPTO_modal                                               # salto_aislado / patron_irregular
    )
  ) %>%
  dplyr::select(NORDEST, DPTO_fijo)

n_sin_dpto_fijo <- sum(is.na(dpto_fijo$DPTO_fijo))
message("Establecimientos sin DPTO_fijo asignable: ", n_sin_dpto_fijo, " de ", nrow(dpto_fijo))

panel <- panel %>%
  dplyr::select(-DPTO) %>%
  dplyr::left_join(dpto_fijo, by = "NORDEST")

# ------------------------------------------------------------------
# 3) Exposicion: Exposure2022_obreros_est (establecimiento) y
#    Exposure2022_obreros / Bite2022_obreros (firma, heredada).
# ------------------------------------------------------------------

exposicion_est_path <- file.path(data_dir, "exposicion_obreros_establecimiento_eam.rds")
exposicion_firma_path <- file.path(data_dir, "exposicion_obreros_eam.rds")
bite_path <- file.path(data_dir, "bite_obreros_eam.rds")

if (!file.exists(exposicion_est_path)) stop("Falta exposicion_obreros_establecimiento_eam.rds.")
if (!file.exists(exposicion_firma_path)) stop("Falta exposicion_obreros_eam.rds.")

exposicion_est <- readr::read_rds(exposicion_est_path) %>%
  dplyr::filter(ANIO == 2022) %>%
  dplyr::distinct(NORDEST, Exposure2022_obreros_est, quintil_exposure2022_obreros_est)

exposicion_firma <- readr::read_rds(exposicion_firma_path) %>%
  dplyr::filter(ANIO == 2022) %>%
  dplyr::distinct(NORDEMP, Exposure2022_obreros, quintil_exposure2022_obreros)

panel <- panel %>%
  dplyr::left_join(exposicion_est, by = "NORDEST") %>%
  dplyr::left_join(exposicion_firma, by = "NORDEMP")

if (file.exists(bite_path)) {
  bite <- readr::read_rds(bite_path) %>% dplyr::distinct(NORDEMP, Bite2022_obreros) %>% dplyr::mutate(NORDEMP = as.character(NORDEMP))
  panel <- panel %>% dplyr::left_join(bite, by = "NORDEMP")
} else {
  message("Nota: bite_obreros_eam.rds no encontrado, Bite2022_obreros no incluido (robustez, no bloqueante).")
  panel$Bite2022_obreros <- NA_real_
}

# ------------------------------------------------------------------
# 4) Cohorte balanceada de la especificacion "dentro de firma" (B):
#    181 firmas con >=2 plantas en LOS 9 anios de la ventana
#    (recalculado aqui, mismo criterio que
#    construir_panel_efectivo_especificacion_b_por_anio.R Paso 3.7).
# ------------------------------------------------------------------

base_2022_multiplanta <- base_ventana %>%
  dplyr::filter(ANIO == 2022) %>%
  dplyr::distinct(NORDEST, NORDEMP)
n_est_2022 <- base_2022_multiplanta %>% dplyr::group_by(NORDEMP) %>% dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")
firmas_262 <- n_est_2022 %>% dplyr::filter(n_est > 1) %>% dplyr::pull(NORDEMP)

n_est_por_firma_anio <- base_ventana %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(n_est = dplyr::n_distinct(NORDEST), .groups = "drop")

persistencia_262 <- n_est_por_firma_anio %>%
  dplyr::filter(NORDEMP %in% firmas_262) %>%
  tidyr::complete(NORDEMP = firmas_262, ANIO = PANEL_ANIOS_FINAL, fill = list(n_est = 0)) %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::summarise(mantiene_2plus_todos_los_anios = all(n_est >= 2), .groups = "drop")

firmas_181 <- persistencia_262 %>% dplyr::filter(mantiene_2plus_todos_los_anios) %>% dplyr::pull(NORDEMP)

panel <- panel %>%
  dplyr::mutate(
    firma_multiplanta_2022 = NORDEMP %in% firmas_262,
    cohorte_balanceada_181 = NORDEMP %in% firmas_181
  )

# ------------------------------------------------------------------
# 5) Variables de diseño DiD.
# ------------------------------------------------------------------

panel <- panel %>%
  dplyr::mutate(
    post_2023 = as.integer(ANIO >= 2023),
    anio_lineal = ANIO - min(PANEL_ANIOS_FINAL),
    ANIO_F = factor(ANIO),
    CIIU4 = factor(CIIU4),
    DPTO_fijo = factor(DPTO_fijo),
    tamano_empresa = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")),
    exposicion_10pp_est = Exposure2022_obreros_est / 0.1,
    exposicion_10pp_firma = Exposure2022_obreros / 0.1
  )

# ------------------------------------------------------------------
# Exportar
# ------------------------------------------------------------------

readr::write_rds(panel, file.path(data_dir, "panel_formal_establecimiento_eam.rds"))
readr::write_csv(panel, file.path(data_dir, "panel_formal_establecimiento_eam.csv"))

# ------------------------------------------------------------------
# Diagnostico de cobertura (consola)
# ------------------------------------------------------------------

script_header("Panel formal establecimiento-anio (PASO 1, sin resultados de empleo)")

message("")
message("Filas NORDEST-ANIO: ", nrow(panel))
message("Establecimientos unicos: ", dplyr::n_distinct(panel$NORDEST))
message("Firmas unicas: ", dplyr::n_distinct(panel$NORDEMP))
message("Ventana: ", paste(PANEL_ANIOS_FINAL, collapse = ", "))

message("")
message("Filas por anio:")
print(panel %>% dplyr::count(ANIO), n = Inf)

message("")
message("Cobertura de variables clave (% no NA sobre el total de filas):")
cobertura <- panel %>%
  dplyr::summarise(
    dplyr::across(
      c(empleo_total, DPTO_fijo, CIIU4, tamano_empresa, Exposure2022_obreros_est, Exposure2022_obreros, Bite2022_obreros),
      ~round(100 * mean(!is.na(.x)), 2)
    )
  )
print(cobertura, width = Inf)

message("")
message("Firmas multiplanta 2022 (Multi_f): ", length(firmas_262))
message("Cohorte balanceada (>=2 plantas los 9 anios): ", length(firmas_181))
message("Filas en la cohorte balanceada: ", sum(panel$cohorte_balanceada_181))

message("")
message("NINGUN resultado de empleo se calcula en este script -- solo se ensambla el panel.")
message("Panel exportado en: ", file.path(data_dir, "panel_formal_establecimiento_eam.rds"))
