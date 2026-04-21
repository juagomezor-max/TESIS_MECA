# Construye una tabla maestra de variables combinando:
# 1) Diccionario en Word (DOCX)
# 2) Metadatos de archivos Stata (DTA) de EAM y EAC

required_packages <- c("dplyr", "purrr", "stringr", "readr", "haven", "tibble")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

root_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
data_dir <- file.path(root_dir, "1. DATOS")
out_dir <- file.path(root_dir, "0. PREPARACION")
tmp_dir <- file.path(root_dir, "2. PROCESAMIENTO", "_tmp_diccionario")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

docx_path <- file.path(data_dir, "Diccionarios_EAM_EAC.docx")
if (!file.exists(docx_path)) {
  stop("No se encontro el archivo de diccionario: ", docx_path)
}

extract_docx_lines <- function(path) {
  xml_txt <- readLines(unz(path, "word/document.xml"), warn = FALSE, encoding = "UTF-8")
  xml_one <- paste(xml_txt, collapse = "")

  m <- stringr::str_match_all(xml_one, "<w:t[^>]*>(.*?)</w:t>")[[1]]
  if (nrow(m) == 0) return(character(0))

  vals <- m[, 2]
  vals <- gsub("&amp;", "&", vals, fixed = TRUE)
  vals <- gsub("&lt;", "<", vals, fixed = TRUE)
  vals <- gsub("&gt;", ">", vals, fixed = TRUE)
  vals <- gsub("&#39;", "'", vals, fixed = TRUE)
  vals <- gsub("&quot;", '"', vals, fixed = TRUE)
  vals <- trimws(vals)
  vals[vals != ""]
}

infer_doc_source <- function(line) {
  u <- toupper(line)
  if (grepl("ENCUESTA ANUAL MANUFACTURERA|\\(EAM\\)", u)) return("EAM")
  if (grepl("ENCUESTA ANUAL DE COMERCIO|\\(EAC\\)", u)) return("EAC")
  NA_character_
}

is_variable_token <- function(x) {
  grepl("^[A-Z][A-Z0-9_]{1,35}$", x)
}

parse_dictionary_pairs <- function(lines) {
  current_source <- NA_character_
  out <- list()

  skip_tokens <- c(
    "SECCION", "DESCRIPCION", "NOMBRE VARIABLE", "NOMBRE", "VARIABLE",
    "DICCIONARIOS", "DATOS", "ENCUESTAS", "ECONOMICAS", "SECTORIALES", "DANE"
  )

  for (i in seq_along(lines)) {
    src <- infer_doc_source(lines[i])
    if (!is.na(src)) current_source <- src

    token <- toupper(lines[i])
    if (!is_variable_token(token)) next
    if (token %in% skip_tokens) next

    if (i <= 1) next
    desc <- trimws(lines[i - 1])
    if (desc == "") next

    out[[length(out) + 1]] <- tibble::tibble(
      fuente = current_source,
      variable = token,
      descripcion_diccionario = desc
    )
  }

  if (length(out) == 0) {
    return(tibble::tibble(
      fuente = character(),
      variable = character(),
      descripcion_diccionario = character()
    ))
  }

  dplyr::bind_rows(out) %>%
    dplyr::filter(!is.na(fuente)) %>%
    dplyr::distinct(fuente, variable, .keep_all = TRUE)
}

extract_dta_metadata <- function(data_root, tmp_root) {
  zip_files <- list.files(data_root, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  zip_files <- zip_files[grepl("EAM|EAC", toupper(zip_files))]

  if (length(zip_files) == 0) {
    stop("No se encontraron ZIP EAM/EAC en: ", data_root)
  }

  all_meta <- purrr::map_dfr(zip_files, function(z) {
    zip_list <- unzip(z, list = TRUE)
    dta_name <- zip_list$Name[grepl("\\.dta$", zip_list$Name, ignore.case = TRUE)][1]

    if (is.na(dta_name) || !nzchar(dta_name)) {
      return(tibble::tibble())
    }

    zip_base <- tools::file_path_sans_ext(basename(z))
    exdir <- file.path(tmp_root, zip_base)
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

    unzip(z, files = dta_name, exdir = exdir, overwrite = TRUE)
    dta_path <- file.path(exdir, dta_name)

    if (!file.exists(dta_path)) {
      return(tibble::tibble())
    }

    dat <- tryCatch(haven::read_dta(dta_path, n_max = 1), error = function(e) NULL)
    if (is.null(dat)) {
      return(tibble::tibble())
    }

    fuente <- if (grepl("EAM", toupper(z))) "EAM" else if (grepl("EAC", toupper(z))) "EAC" else "OTRO"
    anio <- suppressWarnings(as.integer(stringr::str_extract(z, "(19|20)[0-9]{2}")))

    vars <- names(dat)
    labels <- vapply(dat, function(col) {
      lbl <- attr(col, "label")
      if (is.null(lbl)) "" else as.character(lbl)
    }, character(1))

    tibble::tibble(
      fuente = fuente,
      anio = anio,
      archivo_zip = basename(z),
      archivo_dta = dta_name,
      variable = toupper(vars),
      label_dta = labels
    )
  })

  all_meta
}

lines_doc <- extract_docx_lines(docx_path)
dict_doc <- parse_dictionary_pairs(lines_doc)
meta_dta <- extract_dta_metadata(data_dir, tmp_dir)

if (nrow(meta_dta) == 0) {
  stop("No fue posible extraer metadatos desde los archivos DTA.")
}

meta_dta_unique <- meta_dta %>%
  dplyr::group_by(fuente, variable) %>%
  dplyr::summarise(
    label_dta = dplyr::first(label_dta[label_dta != ""], default = ""),
    aparece_en_anios = dplyr::n_distinct(anio, na.rm = TRUE),
    .groups = "drop"
  )

maestro <- meta_dta_unique %>%
  dplyr::left_join(dict_doc, by = c("fuente", "variable")) %>%
  dplyr::mutate(
    descripcion_final = dplyr::case_when(
      !is.na(descripcion_diccionario) & descripcion_diccionario != "" ~ descripcion_diccionario,
      label_dta != "" ~ label_dta,
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::arrange(fuente, variable)

sin_descripcion <- maestro %>%
  dplyr::filter(is.na(descripcion_final) | descripcion_final == "")

readr::write_csv(dict_doc, file.path(out_dir, "diccionario_word_extraido.csv"))
readr::write_csv(meta_dta, file.path(out_dir, "metadatos_dta_variables.csv"))
readr::write_csv(maestro, file.path(out_dir, "diccionario_maestro_variables.csv"))
readr::write_csv(sin_descripcion, file.path(out_dir, "variables_sin_descripcion.csv"))

message("Listo. Archivos generados en: ", out_dir)
message("Variables totales en maestro: ", nrow(maestro))
message("Variables sin descripcion final: ", nrow(sin_descripcion))
