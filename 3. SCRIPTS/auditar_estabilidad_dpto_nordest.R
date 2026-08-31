# Auditoria de estabilidad de DPTO DENTRO de un mismo NORDEST a lo largo
# del tiempo (Paso 2.3 de feature/panel-establecimiento). Un
# establecimiento es una ubicacion fisica: el departamento no deberia
# cambiar de un anio a otro para el mismo NORDEST, salvo relocalizacion
# real de la planta (rara) o un salto por error de captura/recodificacion.
#
# Clasificacion de los NORDEST con mas de un DPTO distinto en su panel,
# segun el patron de la secuencia de DPTO ordenada por anio (run-length
# encoding, ignorando huecos de anios sin reportar):
#
# - "cambio_sostenido": exactamente 2 valores distintos y exactamente 2
#   "corridas" (DPTO_A durante un prefijo de anios, luego cambia a
#   DPTO_B de forma permanente hasta el final de su panel, sin volver a
#   DPTO_A). Consistente con relocalizacion real O con un cambio de
#   codificacion DIVIPOLA que afecte a ese departamento especificamente.
# - "salto_aislado": tras remover anios donde el DPTO aparece UNA SOLA
#   VEZ en toda la serie del establecimiento, el resto de la secuencia
#   es un unico valor constante. Consistente con error de captura o
#   recodificacion puntual (vuelve al valor original despues).
# - "patron_irregular": ni lo uno ni lo otro (mas de 2 valores distintos,
#   o alternancia repetida sin patron claro de corte unico). Requiere
#   revision manual.
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - auditoria_dpto_estabilidad_nordest_resumen.csv
# - auditoria_dpto_estabilidad_nordest_casos.csv (detalle de cada NORDEST con >1 DPTO)
# - auditoria_dpto_cambio_sostenido_por_anio.csv (en que anio ocurre el cambio, para
#   cruzar contra posibles cambios de codificacion DIVIPOLA)

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEST", "ANIO", "DPTO"))

base <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO)))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(ANIO), !is.na(DPTO)) %>%
  dplyr::distinct(NORDEST, ANIO, DPTO) %>%
  dplyr::arrange(NORDEST, ANIO)

# ------------------------------------------------------------------
# 1) NORDEST con mas de un DPTO distinto en su panel.
# ------------------------------------------------------------------

n_dptos_por_nordest <- base %>%
  dplyr::group_by(NORDEST) %>%
  dplyr::summarise(n_dptos_distintos = dplyr::n_distinct(DPTO), n_anios = dplyr::n(), .groups = "drop")

nordest_inestables <- n_dptos_por_nordest %>% dplyr::filter(n_dptos_distintos > 1)

message("Establecimientos totales: ", nrow(n_dptos_por_nordest))
message("Establecimientos con mas de un DPTO en su panel: ", nrow(nordest_inestables),
        " (", round(100 * nrow(nordest_inestables) / nrow(n_dptos_por_nordest), 3), "%)")

# ------------------------------------------------------------------
# 2) Clasificacion del patron por NORDEST inestable.
# ------------------------------------------------------------------

clasificar_patron <- function(anios, dptos) {
  # anios y dptos ya ordenados por anio.
  rle_dpto <- rle(dptos)
  n_valores <- dplyr::n_distinct(dptos)
  n_corridas <- length(rle_dpto$lengths)

  if (n_valores == 2 && n_corridas == 2) {
    return(list(
      patron = "cambio_sostenido",
      anio_cambio = anios[rle_dpto$lengths[1] + 1],
      dpto_antes = rle_dpto$values[1],
      dpto_despues = rle_dpto$values[2]
    ))
  }

  # Salto aislado: valores que aparecen en una sola "corrida" de largo 1
  # (una sola vez en toda la serie); si al quitarlos el resto es un unico
  # valor constante, es un salto aislado (posiblemente mas de uno).
  # Un valor puede aparecer en mas de una corrida de largo 1 (ej. A,B,A,B,A);
  # eso NO es un salto aislado limpio, es alternancia -- se excluye
  # exigiendo que el valor aparezca EXACTAMENTE una vez en toda la serie
  # (no solo en una corrida de largo 1).
  candidatos <- unique(rle_dpto$values[rle_dpto$lengths == 1])
  valores_aislados_limpios <- candidatos[vapply(candidatos, function(v) sum(dptos == v) == 1, logical(1))]

  if (length(valores_aislados_limpios) > 0) {
    resto <- dptos[!dptos %in% valores_aislados_limpios]
    if (dplyr::n_distinct(resto) == 1) {
      return(list(
        patron = "salto_aislado",
        anio_cambio = paste(anios[dptos %in% valores_aislados_limpios], collapse = ";"),
        dpto_antes = resto[1],
        dpto_despues = paste(unique(valores_aislados_limpios), collapse = ";")
      ))
    }
  }

  list(patron = "patron_irregular", anio_cambio = NA_character_, dpto_antes = NA_real_, dpto_despues = NA_real_)
}

casos <- nordest_inestables %>%
  dplyr::pull(NORDEST) %>%
  purrr::map_dfr(function(id) {
    datos <- base %>% dplyr::filter(NORDEST == id)
    clasif <- clasificar_patron(datos$ANIO, datos$DPTO)
    tibble::tibble(
      NORDEST = id,
      n_anios = nrow(datos),
      n_dptos_distintos = dplyr::n_distinct(datos$DPTO),
      secuencia = paste0(datos$ANIO, ":", datos$DPTO, collapse = " | "),
      patron = clasif$patron,
      anio_cambio = as.character(clasif$anio_cambio),
      dpto_antes = clasif$dpto_antes,
      dpto_despues = as.character(clasif$dpto_despues)
    )
  })

readr::write_csv(casos, file.path(out_dir, "auditoria_dpto_estabilidad_nordest_casos.csv"))

resumen_patron <- casos %>%
  dplyr::count(patron, name = "n_establecimientos") %>%
  dplyr::mutate(pct_de_inestables = round(100 * n_establecimientos / sum(n_establecimientos), 2))

resumen <- tibble::tibble(
  establecimientos_totales = nrow(n_dptos_por_nordest),
  establecimientos_con_mas_de_1_dpto = nrow(nordest_inestables),
  pct_con_mas_de_1_dpto = round(100 * nrow(nordest_inestables) / nrow(n_dptos_por_nordest), 3)
)

readr::write_csv(resumen, file.path(out_dir, "auditoria_dpto_estabilidad_nordest_resumen.csv"))

# ------------------------------------------------------------------
# 3) Distribucion por anio de los "cambio_sostenido" (para cruzar contra
#    posibles cambios de codificacion DIVIPOLA a nivel de todo el pais).
# ------------------------------------------------------------------

cambios_sostenidos <- casos %>%
  dplyr::filter(patron == "cambio_sostenido") %>%
  dplyr::mutate(anio_cambio = as.integer(anio_cambio))

cambio_por_anio <- cambios_sostenidos %>%
  dplyr::count(anio_cambio, name = "n_establecimientos") %>%
  dplyr::arrange(anio_cambio)

readr::write_csv(cambio_por_anio, file.path(out_dir, "auditoria_dpto_cambio_sostenido_por_anio.csv"))

pares_dpto <- cambios_sostenidos %>%
  dplyr::count(dpto_antes, dpto_despues, name = "n_establecimientos") %>%
  dplyr::arrange(dplyr::desc(n_establecimientos))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Estabilidad de DPTO dentro de un mismo NORDEST")

message("")
print(resumen, width = Inf)
message("")
message("Clasificacion de los ", nrow(nordest_inestables), " casos inestables:")
print(resumen_patron, width = Inf)

message("")
message("Distribucion por anio de 'cambio_sostenido' (", nrow(cambios_sostenidos), " casos):")
print(cambio_por_anio, n = Inf, width = Inf)

message("")
message("Pares departamento_antes -> departamento_despues mas frecuentes en 'cambio_sostenido':")
print(pares_dpto, n = 15, width = Inf)

message("")
message("Tablas exportadas en: ", out_dir)
