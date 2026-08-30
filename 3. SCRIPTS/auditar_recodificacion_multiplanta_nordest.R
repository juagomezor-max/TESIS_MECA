# Chequeo adicional (barato) de recodificacion de NORDEST en firmas
# MULTIPLANTA -- complementa auditar_confiabilidad_nordest.R, cuyo Paso 2
# solo podia ver firmas de UN establecimiento (ahi el cambio de NORDEST es
# inequivoco porque no hay ambiguedad de cual establecimiento es cual).
#
# En firmas multiplanta, un cambio real de NORDEST puede confundirse con
# una apertura+cierre genuina de plantas. Señal indirecta que SI podemos
# ver sin datos de direccion (la macrobase EAM no tiene direccion/
# municipio, solo DPTO -- ver notas_panel_establecimiento.md): si un
# NORDEST desaparece de una firma en el anio t y OTRO NORDEST aparece en
# la MISMA firma en t+1, con tamaño (empleo) y actividad (CIIU4)
# similares, eso es sugestivo de que sea la misma planta recodificada, no
# una planta que cierra y otra que abre por coincidencia.
#
# LIMITACION CONOCIDA (documentada, no resuelta): esto es un chequeo
# indirecto e imperfecto. Sin direccion no se puede confirmar con
# certeza; una firma grande podria genuinamente cerrar una planta y abrir
# otra del mismo sector y tamaño similar el mismo anio, sin que sea
# recodificacion. Este script cuantifica cuantos casos "coinciden" en
# tamaño y actividad, como cota superior de la posible recodificacion no
# detectada por el Paso 2 -- no como conteo definitivo.
#
# Definicion de "candidato a recodificacion": swap 1-a-1 (exactamente un
# NORDEST desaparece y exactamente un NORDEST aparece en la misma firma
# entre anios consecutivos) + mismo CIIU4 + tamaño (PERTOTAL) similar
# (razon min/max >= 0.5, es decir no difieren en mas del doble).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - auditoria_nordest_swaps_multiplanta.csv
# - auditoria_nordest_swaps_candidatos_recodificacion.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$resultados_validaciones

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "NORDEST", "ANIO", "CIIU4", "PERTOTAL"))

base <- macro_base %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    NORDEST = as.character(NORDEST),
    ANIO = as.integer(suppressWarnings(as.numeric(ANIO))),
    PERTOTAL = suppressWarnings(as.numeric(PERTOTAL))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEMP, NORDEST, ANIO, CIIU4, PERTOTAL)

# ------------------------------------------------------------------
# 1) Firmas multiplanta: NORDEMP con >1 NORDEST distinto en ALGUN anio
#    del periodo (no solo 2015-2019).
# ------------------------------------------------------------------

est_por_firma_anio <- base %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(n_establecimientos_anio = dplyr::n_distinct(NORDEST), .groups = "drop")

firmas_multiplanta <- est_por_firma_anio %>%
  dplyr::filter(n_establecimientos_anio > 1) %>%
  dplyr::distinct(NORDEMP) %>%
  dplyr::pull(NORDEMP)

message("Firmas multiplanta (>1 establecimiento en algun anio, 2008-2024): ", length(firmas_multiplanta))

base_multiplanta <- base %>% dplyr::filter(NORDEMP %in% firmas_multiplanta)

# ------------------------------------------------------------------
# 2) Para cada firma multiplanta, comparar el set de NORDEST entre anios
#    consecutivos presentes en el panel: que desaparece, que aparece.
# ------------------------------------------------------------------

detectar_swaps_firma <- function(datos_firma) {
  anios <- sort(unique(datos_firma$ANIO))
  if (length(anios) < 2) return(NULL)

  purrr::map_dfr(seq_len(length(anios) - 1), function(i) {
    anio_t <- anios[i]
    anio_t1 <- anios[i + 1]
    if (anio_t1 - anio_t != 1) return(NULL)  # solo anios consecutivos

    fila_t <- datos_firma %>% dplyr::filter(ANIO == anio_t)
    fila_t1 <- datos_firma %>% dplyr::filter(ANIO == anio_t1)

    nordest_t <- unique(fila_t$NORDEST)
    nordest_t1 <- unique(fila_t1$NORDEST)

    desaparecen <- setdiff(nordest_t, nordest_t1)
    aparecen <- setdiff(nordest_t1, nordest_t)

    # Solo swaps limpios 1-a-1: exactamente un establecimiento desaparece
    # y exactamente uno aparece (evita ambiguedad de emparejamiento).
    if (length(desaparecen) != 1 || length(aparecen) != 1) return(NULL)

    tibble::tibble(
      NORDEMP = unique(datos_firma$NORDEMP),
      ANIO_t = anio_t,
      ANIO_t1 = anio_t1,
      NORDEST_desaparece = desaparecen,
      NORDEST_aparece = aparecen,
      CIIU4_desaparece = fila_t$CIIU4[fila_t$NORDEST == desaparecen][1],
      CIIU4_aparece = fila_t1$CIIU4[fila_t1$NORDEST == aparecen][1],
      PERTOTAL_desaparece = fila_t$PERTOTAL[fila_t$NORDEST == desaparecen][1],
      PERTOTAL_aparece = fila_t1$PERTOTAL[fila_t1$NORDEST == aparecen][1]
    )
  })
}

swaps <- base_multiplanta %>%
  dplyr::group_by(NORDEMP) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(detectar_swaps_firma)

swaps <- swaps %>%
  dplyr::mutate(
    mismo_ciiu4 = !is.na(CIIU4_desaparece) & !is.na(CIIU4_aparece) & CIIU4_desaparece == CIIU4_aparece,
    razon_tamano = dplyr::case_when(
      is.na(PERTOTAL_desaparece) | is.na(PERTOTAL_aparece) ~ NA_real_,
      PERTOTAL_desaparece <= 0 | PERTOTAL_aparece <= 0 ~ NA_real_,
      TRUE ~ pmin(PERTOTAL_desaparece, PERTOTAL_aparece) / pmax(PERTOTAL_desaparece, PERTOTAL_aparece)
    ),
    tamano_similar = !is.na(razon_tamano) & razon_tamano >= 0.5,
    candidato_recodificacion = mismo_ciiu4 & tamano_similar
  )

readr::write_csv(swaps, file.path(data_dir, "auditoria_nordest_swaps_multiplanta.csv"))

candidatos <- swaps %>% dplyr::filter(candidato_recodificacion)
readr::write_csv(candidatos, file.path(data_dir, "auditoria_nordest_swaps_candidatos_recodificacion.csv"))

# ------------------------------------------------------------------
# Reporte en consola
# ------------------------------------------------------------------

script_header("Chequeo de recodificacion en firmas multiplanta (swaps 1-a-1)")

message("")
message("Total de transiciones anio-consecutivo en firmas multiplanta: ",
        nrow(est_por_firma_anio %>% dplyr::filter(NORDEMP %in% firmas_multiplanta)))
message("Swaps limpios 1-a-1 detectados (1 NORDEST desaparece + 1 aparece, misma firma, mismo anio): ", nrow(swaps))
message("  De esos, mismo CIIU4: ", sum(swaps$mismo_ciiu4, na.rm = TRUE))
message("  De esos, tamaño similar (razon PERTOTAL >= 0.5): ", sum(swaps$tamano_similar, na.rm = TRUE))
message("  CANDIDATOS a recodificacion (mismo CIIU4 + tamaño similar): ", nrow(candidatos),
        " de ", nrow(swaps), " swaps (", round(100 * nrow(candidatos) / nrow(swaps), 1), "%)")
message("")
message("LIMITACION CONOCIDA: sin datos de direccion/municipio en la macrobase EAM, este chequeo")
message("es indirecto e imperfecto -- no distingue con certeza recodificacion real de una firma que")
message("genuinamente cierra una planta y abre otra similar el mismo anio. Se documenta como cota")
message("superior, no como conteo definitivo.")
message("")
message("Tablas exportadas en: ", data_dir)
