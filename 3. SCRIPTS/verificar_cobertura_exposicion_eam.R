# Verifica la cobertura real de anios y de NORDEMP en la macrobase EAM
# antes de construir la medida de exposicion basada en obreros y operarios.
# No transforma ni exporta la macrobase: solo diagnostica.
# Salida:
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/cobertura_anio_nordemp_eam.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

macro_path <- paths$macro_base_eam
if (!file.exists(macro_path)) {
  stop("No se encontro la macrobase EAM en: ", macro_path)
}

macro_base <- readr::read_rds(macro_path)
check_required_vars(macro_base, c("NORDEMP", "ANIO"))

macro_base <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO))

anio_min <- min(macro_base$ANIO)
anio_max <- max(macro_base$ANIO)
anios_disponibles <- sort(unique(macro_base$ANIO))

cobertura_anio <- macro_base %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    nordemp_unicos = dplyr::n_distinct(NORDEMP),
    filas = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ANIO)

readr::write_csv(cobertura_anio, file.path(data_output_dir, "cobertura_anio_nordemp_eam.csv"))

script_header("Cobertura real de la macrobase EAM")
message("Rango de ANIO disponible: ", anio_min, " - ", anio_max)
message("Anios presentes: ", paste(anios_disponibles, collapse = ", "))
message("2024 presente: ", 2024 %in% anios_disponibles)
message("2025 presente: ", 2025 %in% anios_disponibles)
message("")
message("NORDEMP unicos y filas por anio:")
print(cobertura_anio, n = Inf)
message("")
message("Tabla exportada en: ", file.path(data_output_dir, "cobertura_anio_nordemp_eam.csv"))
