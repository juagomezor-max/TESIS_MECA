# Construye la macro base de la Encuesta Anual Manufacturera (EAM)
# a partir de los archivos DTA anuales.
# Salidas:
# - 1. DATOS/5. MACROBASE/macro_base_eam.rds
# - 1. DATOS/5. MACROBASE/macro_base_eam_codebook.csv
# - 1. DATOS/5. MACROBASE/macro_base_eam_resumen.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "purrr", "stringr", "readr", "haven", "tibble", "janitor")
load_project_packages(required_packages)

# Rutas principales del flujo de construccion.
paths <- ensure_project_structure()
data_dir <- paths$datos_eam
dictionary_path <- paths$diccionario_maestro
tmp_dir <- paths$tmp_macrobase
macrobase_dir <- paths$macrobase

if (!file.exists(dictionary_path)) {
  stop("No se encontro el diccionario maestro en: ", dictionary_path)
}

dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)
eam_dictionary <- dictionary %>%
  dplyr::filter(fuente == "EAM") %>%
  dplyr::mutate(variable = toupper(variable))

zip_files <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE, ignore.case = TRUE)
if (length(zip_files) == 0) {
  stop("No se encontraron archivos ZIP de EAM en: ", data_dir)
}

target_years <- sort(stringr::str_extract(basename(zip_files), "(19|20)[0-9]{2}"))

read_eam_year <- function(zip_path) {
  # Cada ZIP deberia traer un DTA anual. Si hay mas de uno, se toma el primero.
  zip_list <- unzip(zip_path, list = TRUE)
  dta_name <- zip_list$Name[grepl("\\.dta$", zip_list$Name, ignore.case = TRUE)][1]

  if (is.na(dta_name) || !nzchar(dta_name)) {
    message("Sin DTA en ZIP: ", basename(zip_path))
    return(NULL)
  }

  year <- suppressWarnings(as.integer(stringr::str_extract(zip_path, "(19|20)[0-9]{2}")))
  exdir <- file.path(tmp_dir, tools::file_path_sans_ext(basename(zip_path)))
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

  unzip(zip_path, files = dta_name, exdir = exdir, overwrite = TRUE)
  dta_path <- file.path(exdir, dta_name)

  if (!file.exists(dta_path)) {
    message("No se pudo extraer DTA: ", basename(zip_path))
    return(NULL)
  }

  dat <- tryCatch(haven::read_dta(dta_path), error = function(e) {
    message("No se pudo leer DTA: ", basename(zip_path), " | ", e$message)
    NULL
  })

  if (is.null(dat)) return(NULL)

  # Se normalizan nombres en mayusulas para reducir problemas de consistencia
  # entre anios al consolidar.
  names(dat) <- toupper(names(dat))

  dat %>%
    dplyr::mutate(
      FUENTE = "EAM",
      ANIO = year,
      ARCHIVO_ORIGEN = basename(zip_path)
    ) %>%
    dplyr::select(FUENTE, ANIO, ARCHIVO_ORIGEN, dplyr::everything())
}

macro_base <- purrr::map_dfr(zip_files, read_eam_year)

if (nrow(macro_base) == 0) {
  stop("La macro base quedo vacia.")
}

macro_base <- macro_base %>%
  dplyr::mutate(
    FUENTE = as.character(FUENTE),
    ANIO = as.integer(ANIO),
    ARCHIVO_ORIGEN = as.character(ARCHIVO_ORIGEN)
  )

macro_cols <- names(macro_base)
meta_cols <- c("fuente", "anio", "archivo_origen")
data_cols <- setdiff(macro_cols, meta_cols)

# El codebook final se deriva del diccionario maestro, pero filtrado solo a las
# variables efectivamente presentes en la macrobase EAM.
codebook <- eam_dictionary %>%
  dplyr::mutate(variable = toupper(variable)) %>%
  dplyr::filter(variable %in% toupper(data_cols)) %>%
  dplyr::distinct(variable, .keep_all = TRUE) %>%
  dplyr::select(variable, label_dta, descripcion_diccionario, descripcion_final, aparece_en_anios)

summary_macro <- tibble::tibble(
  fuente = "EAM",
  archivos_procesados = length(zip_files),
  filas = nrow(macro_base),
  columnas = ncol(macro_base),
  anio_min = suppressWarnings(min(macro_base$ANIO, na.rm = TRUE)),
  anio_max = suppressWarnings(max(macro_base$ANIO, na.rm = TRUE))
)

readr::write_rds(macro_base, file.path(macrobase_dir, "macro_base_eam.rds"))
readr::write_csv(codebook, file.path(macrobase_dir, "macro_base_eam_codebook.csv"))
readr::write_csv(summary_macro, file.path(macrobase_dir, "macro_base_eam_resumen.csv"))

message("Macro base EAM construida correctamente.")
message("Filas: ", nrow(macro_base), " | Columnas: ", ncol(macro_base))
