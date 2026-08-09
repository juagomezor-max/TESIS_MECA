# Construye salarios promedio por categoria ocupacional a nivel
# NORDEMP-ANIO, para validar si los obreros son efectivamente el grupo
# peor pagado (supuesto central de la medida de exposicion).
#
# Se usa exclusivamente personal PERMANENTE en numerador y denominador
# (costo C3R2 = "Sueldos y salarios del personal permanente", contado
# solo contra el conteo de personal permanente de esa misma categoria),
# no el conteo total con temporales/aprendices/propietarios, para que
# numerador y denominador midan al mismo grupo de trabajadores. Mezclar
# el costo de permanentes con un conteo que incluye temporales
# subestimaria el salario promedio real de los permanentes.
#
# Columnas confirmadas estables 2008-2024 en verificar_estabilidad_columnas_c3r_c4r.R:
# - Costo permanente: C3R2C1 (Obreros), C3R2C2 (Administrativos), C3R2PT (Profesional-tecnico)
# - Conteo permanente: C4R2C1/C4R2C2 (Obreros M/H), C4R2C3/C4R2C4 (Admin M/H),
#   C4R1C3N/C4R1C4N (PT nacional M/H) + C4R2C3E/C4R2C4E (PT extranjero M/H)
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/salarios_promedio_categoria_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

macro_path <- paths$macro_base_eam
if (!file.exists(macro_path)) {
  stop("No se encontro la macrobase EAM en: ", macro_path)
}

# Evita divisiones por cero y deja NA cuando el denominador no es usable
# (misma logica que descriptivo_exposicion_eam.R).
safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

macro_base <- readr::read_rds(macro_path)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO"))

cols_costo_permanente <- c("C3R2C1", "C3R2C2", "C3R2PT")
cols_conteo_permanente_obrero <- c("C4R2C1", "C4R2C2")
cols_conteo_permanente_admin <- c("C4R2C3", "C4R2C4")
cols_conteo_permanente_pt <- c("C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E")

cols_necesarias <- unique(c(
  cols_costo_permanente,
  cols_conteo_permanente_obrero,
  cols_conteo_permanente_admin,
  cols_conteo_permanente_pt
))

faltantes <- setdiff(cols_necesarias, names(macro_base))
if (length(faltantes) > 0) {
  stop("Faltan columnas esperadas en la macrobase (revisar Paso 2): ", paste(faltantes, collapse = ", "))
}

# ------------------------------------------------------------------
# Consolidacion a NORDEMP-ANIO (misma logica que los pasos anteriores).
# ------------------------------------------------------------------

panel_raw <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEMP, ANIO, dplyr::all_of(cols_necesarias)) %>%
  dplyr::mutate(dplyr::across(-c(NORDEMP, ANIO), ~suppressWarnings(as.numeric(.x))))

panel <- panel_raw %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(cols_necesarias),
      ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# Salarios promedio: costo permanente / conteo permanente, por categoria.
# ------------------------------------------------------------------

panel_salarios <- panel %>%
  dplyr::mutate(
    personal_permanente_obrero = C4R2C1 + C4R2C2,
    personal_permanente_administrativo = C4R2C3 + C4R2C4,
    personal_permanente_prof_tecnico = C4R1C3N + C4R1C4N + C4R2C3E + C4R2C4E,
    salario_promedio_obrero = safe_divide(C3R2C1, personal_permanente_obrero),
    salario_promedio_administrativo = safe_divide(C3R2C2, personal_permanente_administrativo),
    salario_promedio_prof_tecnico = safe_divide(C3R2PT, personal_permanente_prof_tecnico)
  ) %>%
  dplyr::select(
    NORDEMP, ANIO,
    personal_permanente_obrero, personal_permanente_administrativo, personal_permanente_prof_tecnico,
    salario_promedio_obrero, salario_promedio_administrativo, salario_promedio_prof_tecnico
  )

readr::write_rds(panel_salarios, file.path(data_output_dir, "salarios_promedio_categoria_eam.rds"))
readr::write_csv(panel_salarios, file.path(data_output_dir, "salarios_promedio_categoria_eam.csv"))

# ------------------------------------------------------------------
# Diagnostico: los obreros son el grupo peor pagado?
# ------------------------------------------------------------------

resumen_por_anio <- panel_salarios %>%
  dplyr::filter(
    !is.na(salario_promedio_obrero) | !is.na(salario_promedio_administrativo) | !is.na(salario_promedio_prof_tecnico)
  ) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    n_empresas_con_algun_salario = dplyr::n(),
    salario_obrero_mediana = round(median(salario_promedio_obrero, na.rm = TRUE), 1),
    salario_admin_mediana = round(median(salario_promedio_administrativo, na.rm = TRUE), 1),
    salario_pt_mediana = round(median(salario_promedio_prof_tecnico, na.rm = TRUE), 1),
    obrero_es_el_mas_bajo = salario_obrero_mediana <= pmin(salario_admin_mediana, salario_pt_mediana, na.rm = TRUE),
    ratio_admin_sobre_obrero = round(salario_admin_mediana / salario_obrero_mediana, 2),
    ratio_pt_sobre_obrero = round(salario_pt_mediana / salario_obrero_mediana, 2),
    .groups = "drop"
  )

readr::write_csv(resumen_por_anio, file.path(data_output_dir, "resumen_salarios_promedio_categoria_eam.csv"))

script_header("Salarios promedio por categoria ocupacional (personal permanente)")
message("Filas NORDEMP-ANIO: ", nrow(panel_salarios))
message("")
message("Mediana de salario promedio por categoria y anio (miles de pesos, unidad de la EAM):")
print(resumen_por_anio, n = Inf, width = Inf)
message("")
message("Base exportada en: ", file.path(data_output_dir, "salarios_promedio_categoria_eam.rds"))
