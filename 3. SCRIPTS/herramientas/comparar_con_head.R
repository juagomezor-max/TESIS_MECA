# Compara los CSV y .md de una o mas carpetas de "4. RESULTADOS/" contra la
# version en HEAD de git. Ignora .docx y .png (y cualquier otra extension).
#
# Uso (desde la raiz del repo):
#   Rscript "3. SCRIPTS/herramientas/comparar_con_head.R" "4. RESULTADOS/Carpeta1" ["4. RESULTADOS/Carpeta2" ...]
#
# Estados posibles por archivo:
#   idéntico            - existe en HEAD y en disco, contenido igual (tolerancia 1e-8 en numeros)
#   difiere              - existe en ambos lados pero el contenido cambio
#   nuevo-sin-HEAD        - existe en disco pero no esta en HEAD
#   borrado-no-regenerado - existe en HEAD pero no se encontro en disco
#
# Codigo de salida: 1 si algun archivo no quedo "idéntico"; 0 si todo coincide.

TOL <- 1e-8

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("Uso: Rscript \"3. SCRIPTS/herramientas/comparar_con_head.R\" <carpeta1> [carpeta2 ...]\n")
  cat("Las carpetas deben darse relativas a la raiz del repo, p. ej. \"4. RESULTADOS/02_medidas_exposicion\"\n")
  quit(save = "no", status = 1)
}

normalizar_carpeta <- function(x) sub("/+$", "", x)
carpetas <- normalizar_carpeta(args)

archivos_head_de <- function(carpeta) {
  out <- suppressWarnings(system2("git", c("ls-tree", "-r", "--name-only", "HEAD", "--", shQuote(carpeta)),
                                   stdout = TRUE, stderr = FALSE))
  out[nzchar(out)]
}

archivos_disco_de <- function(carpeta) {
  if (!dir.exists(carpeta)) return(character(0))
  rel <- list.files(carpeta, recursive = TRUE, full.names = FALSE)
  file.path(carpeta, rel)
}

extension_de <- function(path) tolower(tools::file_ext(path))

leer_texto_head <- function(path) {
  tmp <- tempfile()
  ok <- suppressWarnings(system2("git", c("show", shQuote(paste0("HEAD:", path))),
                                  stdout = tmp, stderr = FALSE))
  if (!isTRUE(ok == 0) || !file.exists(tmp)) return(NULL)
  txt <- readLines(tmp, warn = FALSE, encoding = "UTF-8")
  unlink(tmp)
  txt
}

leer_csv_head <- function(path) {
  tmp <- tempfile(fileext = ".csv")
  ok <- suppressWarnings(system2("git", c("show", shQuote(paste0("HEAD:", path))),
                                  stdout = tmp, stderr = FALSE))
  if (!isTRUE(ok == 0) || !file.exists(tmp)) return(NULL)
  df <- tryCatch(
    read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  unlink(tmp)
  df
}

comparar_md <- function(path) {
  head_txt <- leer_texto_head(path)
  disco_txt <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(head_txt) || is.null(disco_txt)) {
    return(list(estado = "difiere", detalle = "no se pudo leer el archivo"))
  }
  if (identical(head_txt, disco_txt)) {
    return(list(estado = "idéntico", detalle = ""))
  }
  n <- min(length(head_txt), length(disco_txt))
  primera <- which(head_txt[seq_len(n)] != disco_txt[seq_len(n)])[1]
  if (is.na(primera)) primera <- n + 1
  detalle <- sprintf("%d vs %d lineas; primera diferencia en la linea %d",
                      length(head_txt), length(disco_txt), primera)
  list(estado = "difiere", detalle = detalle)
}

comparar_columna <- function(nombre, a, b) {
  if (identical(a, b)) return(NULL)
  if (is.numeric(a) && is.numeric(b)) {
    na_dist <- is.na(a) != is.na(b)
    if (any(na_dist)) {
      return(sprintf("col '%s': %d valores NA en un lado y no en el otro", nombre, sum(na_dist)))
    }
    ok <- is.na(a) | (abs(a - b) <= TOL)
    if (all(ok)) return(NULL)
    peor <- max(abs(a[!ok] - b[!ok]))
    return(sprintf("col '%s': max |diff| = %g en %d fila(s)", nombre, peor, sum(!ok)))
  }
  distintos <- which(as.character(a) != as.character(b) | is.na(a) != is.na(b))
  if (length(distintos) == 0) return(NULL)
  i <- distintos[1]
  sprintf("col '%s': %d valor(es) distinto(s) (ej. fila %d: '%s' vs '%s')",
          nombre, length(distintos), i, a[i], b[i])
}

comparar_csv <- function(path) {
  head_df <- leer_csv_head(path)
  disco_df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(head_df) || is.null(disco_df)) {
    return(list(estado = "difiere", detalle = "no se pudo leer el archivo"))
  }
  if (!identical(names(head_df), names(disco_df))) {
    return(list(estado = "difiere", detalle = sprintf(
      "columnas distintas: HEAD=[%s] actual=[%s]",
      paste(names(head_df), collapse = ","), paste(names(disco_df), collapse = ","))))
  }
  if (nrow(head_df) != nrow(disco_df)) {
    return(list(estado = "difiere", detalle = sprintf(
      "filas distintas: HEAD=%d actual=%d", nrow(head_df), nrow(disco_df))))
  }
  mensajes <- Filter(Negate(is.null), lapply(names(head_df), function(nombre) {
    comparar_columna(nombre, head_df[[nombre]], disco_df[[nombre]])
  }))
  if (length(mensajes) == 0) return(list(estado = "idéntico", detalle = ""))
  list(estado = "difiere", detalle = paste(unlist(mensajes), collapse = "; "))
}

filas <- list()

for (carpeta in carpetas) {
  head_files <- archivos_head_de(carpeta)
  disco_files <- archivos_disco_de(carpeta)
  todos <- sort(union(head_files, disco_files))
  todos <- todos[extension_de(todos) %in% c("csv", "md")]

  for (path in todos) {
    en_head <- path %in% head_files
    en_disco <- path %in% disco_files

    if (en_head && !en_disco) {
      resultado <- list(estado = "borrado-no-regenerado", detalle = "existe en HEAD, no se encontro en disco")
    } else if (!en_head && en_disco) {
      resultado <- list(estado = "nuevo-sin-HEAD", detalle = "no existe en HEAD")
    } else if (extension_de(path) == "md") {
      resultado <- comparar_md(path)
    } else {
      resultado <- comparar_csv(path)
    }

    filas[[length(filas) + 1]] <- data.frame(
      carpeta = carpeta, archivo = path,
      estado = resultado$estado, detalle = resultado$detalle,
      stringsAsFactors = FALSE
    )
  }
}

if (length(filas) == 0) {
  cat("No se encontraron .csv ni .md en las carpetas indicadas.\n")
  quit(save = "no", status = 1)
}

tabla <- do.call(rbind, filas)
tabla$detalle <- ifelse(nchar(tabla$detalle) > 100, paste0(substr(tabla$detalle, 1, 97), "..."), tabla$detalle)

options(width = 250)
print(tabla, row.names = FALSE)

n_dif <- sum(tabla$estado != "idéntico")
cat(sprintf("\nTotal: %d archivo(s), %d idéntico(s), %d con diferencia(s).\n",
            nrow(tabla), nrow(tabla) - n_dif, n_dif))

quit(save = "no", status = if (n_dif > 0) 1 else 0)
