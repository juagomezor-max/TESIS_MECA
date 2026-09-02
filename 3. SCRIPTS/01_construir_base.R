# 01_construir_base.R -- Pipeline simplificado (rama `simplificacion`).
#
# Carga la macrobase EAM 2008-2024 y aplica la deduplicacion NORDEMP-ANIO,
# produciendo el panel de FIRMA que usan 02-05.
#
# DERIVADO de (no reimplementado de memoria):
# - Regla de deduplicacion NORDEMP-ANIO: identica en
#   construir_conteo_personal_categoria_eam.R, validar_tendencias_paralelas_empleo_bite.R
#   y validar_tendencias_paralelas_empleo_exposure_grafico.R (todas en `main`):
#   group_by(NORDEMP, ANIO) %>% summarise(across(numericas, ~si todas NA -> NA, si no, suma)).
#   Auditada como economicamente correcta (multi-establecimiento) en
#   auditar_deduplicacion_nordemp_anio.R (rama feature/auditoria-reproducibilidad-macrobase, en main).
# - Columnas C4R/C3R necesarias: mismas listas usadas en
#   construir_conteo_personal_categoria_eam.R (obreros/administrativos/PT),
#   validar_tendencias_paralelas_empleo_exposure_grafico.R (permanente/temporal),
#   y construir_bite_obreros_eam.R (C3R2C1, salario obrero). Confirmadas
#   estables 2008-2024 en verificar_estabilidad_columnas_c3r_c4r.R.
#
# AJUSTE DE ALCANCE (documentado en README_SIMPLIFICACION.md): este script
# CARGA la macrobase ya construida (1. DATOS/5. MACROBASE/macro_base_eam.rds).
# No reconstruye el ETL desde los DTA crudos (eso lo hace
# construir_macro_base_eam.R, sin cambios, fuera del alcance de esta
# simplificacion) ni aplica deflactores/terminos reales: ningun script ya
# validado en main construye o usa una serie de deflactor IPP/IPC citable
# (la unica referencia a deflactores esta en el script exploratorio del
# compañero, "3. SCRIPTS/3. SCRIPTS/construir_base analitica.R", nunca
# validado ni usado en las cifras reportadas) -- no se re-implementa esa
# logica de memoria, siguiendo la regla "derivar, no reescribir".
#
# Salidas (no versionadas, 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/):
# - panel_firma_eam.rds/.csv: NORDEMP-ANIO unico, 2008-2024, con las
#   columnas C4R/C3R crudas ya sumadas, mas CIIU4/DPTO/PERTOTAL.

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

if (!file.exists(paths$macro_base_eam)) {
  stop(
    "No se encontro la macrobase en ", paths$macro_base_eam, ". ",
    "Corre 3. SCRIPTS/construir_macro_base_eam.R primero (ETL desde los DTA crudos, fuera del alcance de este pipeline simplificado)."
  )
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# ------------------------------------------------------------------
# Columnas C4R/C3R necesarias para 02-05 (union de las usadas en los
# scripts de origen, ver cabecera).
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_propietarios <- c(
  "C4R1C1", "C4R1C2", "C4R1C3", "C4R1C4",
  "C4R1C1N", "C4R1C2N", "C4R2C1E", "C4R2C2E"
)
cols_permanente <- c("C4R2C1", "C4R2C2", "C4R2C3", "C4R2C4", "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E")
cols_temporal <- c(
  "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E", "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"
)
col_salario_obrero <- "C3R2C1"  # construir_bite_obreros_eam.R

cols_numericas <- unique(c(
  cols_obreros, cols_administrativos, cols_prof_tecnico, cols_propietarios,
  cols_permanente, cols_temporal, col_salario_obrero, "PERTOTAL"
))

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO", "CIIU4", "DPTO", cols_numericas))

# ------------------------------------------------------------------
# Deduplicacion NORDEMP-ANIO (identica a la regla ya auditada).
# ------------------------------------------------------------------

base_cruda <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEMP, ANIO, CIIU4, DPTO, dplyr::all_of(cols_numericas)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_numericas), safe_numeric))

panel_firma <- base_cruda %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(cols_numericas), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    CIIU4 = dplyr::first(CIIU4),
    DPTO = dplyr::first(DPTO),
    .groups = "drop"
  )

n_dup <- panel_firma %>% dplyr::count(NORDEMP, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup > 0) stop("NORDEMP-ANIO no quedo unico tras deduplicar (", n_dup, " grupos). Revisar.")

readr::write_rds(panel_firma, file.path(data_dir, "panel_firma_eam.rds"))
readr::write_csv(panel_firma, file.path(data_dir, "panel_firma_eam.csv"))

script_header("01_construir_base.R -- Panel de firma deduplicado (NORDEMP-ANIO)")
message("")
message("Filas NORDEMP-ANIO: ", nrow(panel_firma))
message("Firmas unicas: ", dplyr::n_distinct(panel_firma$NORDEMP))
message("Anios: ", paste(sort(unique(panel_firma$ANIO)), collapse = ", "))
message("")
message("Base exportada en: ", file.path(data_dir, "panel_firma_eam.rds"))
