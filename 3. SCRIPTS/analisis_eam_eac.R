# Analisis inicial EAM/EAC desde archivos ZIP
# Genera inventario de archivos, resumen anual y variables mas comunes.
# Uso esperado:
# - Rscript "3. SCRIPTS/analisis_eam_eac.R" EAM
# - Rscript "3. SCRIPTS/analisis_eam_eac.R" EAC
# - Rscript "3. SCRIPTS/analisis_eam_eac.R" ALL
# Salidas:
# - CSV de inventario y resumen en 1. DATOS/4. ANALISIS_INICIAL/
# - Grafico de cobertura anual por fuente en 4. RESULTADOS/

required_packages <- c(
  "dplyr", "purrr", "stringr", "readr", "haven", "readxl", "janitor", "ggplot2", "tibble", "tidyr", "scales"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# Rutas base del proyecto y carpeta temporal para extraer ZIP.
root_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
data_dir <- file.path(root_dir, "1. DATOS")
extract_root <- file.path(root_dir, "2. PROCESAMIENTO", "_tmp_unzip")
output_dir <- file.path(root_dir, "4. RESULTADOS")
data_output_dir <- file.path(root_dir, "1. DATOS", "4. ANALISIS_INICIAL")

# Se admite la fuente como argumento para reutilizar el mismo script
# en EAM, EAC o una corrida conjunta.
args <- commandArgs(trailingOnly = TRUE)
target_source <- if (length(args) >= 1) toupper(args[[1]]) else "EAM"

if (!target_source %in% c("EAM", "EAC", "ALL")) {
  stop("Parametro invalido. Usa: EAM, EAC o ALL.")
}

output_tag <- if (target_source == "ALL") "all" else tolower(target_source)

source_matches <- function(path, target) {
  if (target == "ALL") return(TRUE)
  stringr::str_detect(toupper(path), target)
}

if (!dir.exists(data_dir)) {
  stop("No se encontro la carpeta de datos en: ", data_dir)
}

dir.create(extract_root, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)

all_zip_files <- list.files(data_dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
zip_files <- all_zip_files[source_matches(all_zip_files, target_source)]

if (length(all_zip_files) == 0) {
  stop("No se encontraron archivos ZIP en la carpeta de datos.")
}

if (length(zip_files) == 0) {
  stop("No se encontraron ZIP para la fuente seleccionada: ", target_source)
}

# Descomprime cada ZIP en su propia subcarpeta para evitar colisiones de nombres.
for (z in zip_files) {
  zip_name <- tools::file_path_sans_ext(basename(z))
  dest_dir <- file.path(extract_root, zip_name)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(z, exdir = dest_dir)
}

candidate_files <- list.files(
  extract_root,
  pattern = "\\.(csv|dta|sav|xlsx)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(candidate_files) == 0) {
  stop("No se encontraron archivos compatibles (csv/dta/sav/xlsx) despues de descomprimir.")
}

preview_n <- 5000

# La fuente se infiere desde la ruta para evitar depender de nombres
# de columnas internas de archivos heterogeneos.
infer_source <- function(path) {
  p <- toupper(path)
  if (stringr::str_detect(p, "EAM")) return("EAM")
  if (stringr::str_detect(p, "EAC")) return("EAC")
  "OTRO"
}

# El anio tambien se obtiene desde el nombre/ruta del archivo.
infer_year <- function(path) {
  y <- stringr::str_extract(path, "(19|20)[0-9]{2}")
  suppressWarnings(as.integer(y))
}

# Se lee solo una muestra de filas para inventariar estructura sin cargar
# completamente cada archivo, lo que hace mas liviano el diagnostico.
read_preview <- function(path, n_max = 5000) {
  ext <- tolower(tools::file_ext(path))

  out <- tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, n_max = n_max, show_col_types = FALSE)
    } else if (ext == "dta") {
      haven::read_dta(path, n_max = n_max)
    } else if (ext == "sav") {
      haven::read_sav(path, n_max = n_max)
    } else if (ext == "xlsx") {
      readxl::read_excel(path, n_max = n_max)
    } else {
      NULL
    }
  }, error = function(e) {
    message("No se pudo leer: ", path, " | ", e$message)
    NULL
  })

  out
}

inventory <- purrr::map_dfr(candidate_files, function(f) {
  dat <- read_preview(f, n_max = preview_n)

  # Se guarda el listado crudo de nombres de columnas para luego medir
  # recurrencia de variables entre archivos.
  tibble::tibble(
    archivo = f,
    fuente = infer_source(f),
    anio = infer_year(f),
    extension = tolower(tools::file_ext(f)),
    filas_muestra = if (is.null(dat)) NA_integer_ else nrow(dat),
    columnas = if (is.null(dat)) NA_integer_ else ncol(dat),
    nombres_columnas = if (is.null(dat)) NA_character_ else paste(names(dat), collapse = "|")
  )
})

summary_year <- inventory %>%
  dplyr::group_by(fuente, anio) %>%
  dplyr::summarise(
    archivos = dplyr::n(),
    filas_muestra_totales = sum(filas_muestra, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(fuente, anio)

variables_common <- inventory %>%
  # Se separan los nombres de columnas y se normalizan para poder
  # comparar presencia aun si hay variaciones menores de estilo.
  dplyr::filter(!is.na(nombres_columnas), nombres_columnas != "") %>%
  dplyr::mutate(variable = stringr::str_split(nombres_columnas, "\\|")) %>%
  tidyr::unnest(variable) %>%
  dplyr::mutate(variable = janitor::make_clean_names(variable)) %>%
  dplyr::group_by(fuente, variable) %>%
  dplyr::summarise(presencia_en_archivos = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(fuente, dplyr::desc(presencia_en_archivos), variable)

readr::write_csv(inventory, file.path(data_output_dir, paste0("inventario_archivos_", output_tag, ".csv")))
readr::write_csv(summary_year, file.path(data_output_dir, paste0("resumen_por_fuente_anio_", output_tag, ".csv")))
readr::write_csv(variables_common, file.path(data_output_dir, paste0("variables_mas_comunes_", output_tag, ".csv")))

plot_df <- summary_year %>% dplyr::filter(!is.na(anio))

if (nrow(plot_df) > 0) {
  # El grafico resume cobertura de archivos por anio, no cobertura de observaciones.
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = anio, y = archivos, color = fuente)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      title = "Cantidad de archivos procesados por anio",
      x = "Anio",
      y = "Numero de archivos",
      color = "Fuente"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0("plot_archivos_por_anio_", output_tag, ".png")),
    plot = p,
    width = 10,
    height = 6,
    dpi = 120
  )
}

message("Analisis completado para ", target_source, ". Revisa la carpeta: ", output_dir)
