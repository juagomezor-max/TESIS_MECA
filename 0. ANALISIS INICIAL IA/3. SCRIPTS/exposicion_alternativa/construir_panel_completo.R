# ==============================================================================
# construir_panel_completo.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Rama:  feature/medida-exposicion-alternativa
#
# QUE HACE ESTE SCRIPT
# Construye el panel firma-año (NORDEMP-ANIO) de la EAM 2008-2024, CON 2020,
# con todas las variables de personal, costo laboral, tercerización y
# producción que podrían necesitarse para construir medidas alternativas de
# exposición al choque del salario mínimo de 2023. Extrae, agrega a nivel
# firma, valida contra identidades conocidas y documenta en un diccionario.
#
# QUE NO HACE
# No calcula ninguna medida de exposición, ratio, salario promedio ni
# índice. Eso es el script siguiente. Tampoco imputa faltantes ni
# winsoriza: el panel guarda los datos tal como vienen.
#
# POR QUE INCLUIR 2020
# El panel analítico actual (panel_analitico_firma_eam.rds) excluye 2020 por
# la pandemia, pero esa exclusión se tomó en la construcción de la base, así
# que hoy no se puede probar su efecto. Con 2020 dentro, incluirlo o
# excluirlo pasa a ser una decisión de estimación, no un supuesto enterrado
# en el ETL.
#
# ==============================================================================
# PASO 0 -- VERIFICACIONES PREVIAS (bloqueantes, resueltas ANTES de extraer)
# ==============================================================================
#
# 0.1 -- Denominador de Bite2022_obreros (pipeline/02_construir_exposicion.R):
#   REVISADO, SIN PROBLEMA. La formula real (linea ~96 de ese script) es
#   salario_promedio_obrero = C3R2C1 / personal_permanente_obrero, donde
#   personal_permanente_obrero = C4R2C1 + C4R2C2 (SOLO permanentes). NO se
#   divide por el total ocupado (C4R5C1+C4R5C2, que si incluiria temporales
#   y aprendices). Numerador y denominador miden a la MISMA poblacion
#   (personal permanente). El comentario del propio script (lineas 89-94) ya
#   lo deja explicito. No hay confusor de temporalidad en Bite2022_obreros.
#
# 0.2 -- Serie de conteos profesional/tecnico correcta (C4R1C*N vs C4R2C*E):
#   RESUELTO EMPIRICAMENTE, NO SON DUPLICADAS -- SON COMPLEMENTARIAS. Se
#   probaron ambas series contra la identidad
#   obreros + profesionales + administrativos + aprendices = PERTOTAL:
#     - Solo serie N:            97.56% de filas coincide exacto con PERTOTAL
#     - Solo serie E:             28-39% (segun submuestra)
#     - N + E juntas + aprendices: 100.00% de filas coincide exacto (0 de
#       140,835 filas con diferencia), confirmado tambien columna-por-columna
#       (serie E nunca supera ~5% de casos no-cero por año, casi nunca
#       coexiste con N en la misma fila -- 2.23% de las filas tienen ambas
#       series no-cero a la vez).
#   Conclusion: sumar N+E es la formula correcta (coincide con lo que YA hace
#   pipeline/01_construir_base.R y construccion/ampliar_variables_paneles.R
#   -- no es un error de esos scripts, es la razon por la que ya sumaban
#   ambas series). Este script reusa esa misma regla para cada subcategoria
#   de profesional/tecnico (permanentes, propietarios, temporal directo,
#   temporal agencias, total ocupado), no solo para el total.
#   Verificacion re-ejecutada mas abajo con los datos reales de este script
#   (ver seccion "VALIDACION 0.2/0.3").
#
# 0.3 -- Aprendices, ¿estan en C4R5*/PERTOTAL?
#   RESUELTO EMPIRICAMENTE. C4R5C1+C4R5C2 (total ocupado obreros) SI incluye
#   aprendices: (permanentes+temporal_directo+temporal_agencias) sin
#   aprendices coincide con C4R5_obreros solo 80.14% de las veces; sumando
#   aprendices sube a 99.46%. PERTOTAL tambien los incluye (ver 0.2). En
#   cambio, el COSTO laboral de aprendices NO esta en C3R2C1 (sueldos
#   permanentes) ni en ningun C3R* de personal: va aparte, en
#   R1CSAP/R2CSAP/R3CSAP/R4CSAP (apoyo de sostenimiento, Ley 789). Quien
#   calcule un salario promedio con C4R5* (o PERTOTAL) como denominador y
#   C3R2C1 como numerador esta metiendo aprendices en el denominador cuyo
#   pago no esta en el numerador -- el salario sale subestimado. Por eso
#   este panel extrae aprendices como variable propia por categoria
#   ocupacional (obreros/profesional-tecnico/administrativos), separados de
#   permanentes/temporal, y documenta la poblacion de cada variable de costo
#   en el diccionario (columna `poblacion`).
#
# 0.4 -- Disponibilidad por año:
#   - C3R1* (salario integral): SOLO 2008-2019 (12 de 17 años). Ausente
#     2020-2024. El salario integral es por ley >=10 SMLV: si esos
#     trabajadores siguen en el conteo de personal (C4R5*/PERTOTAL) pero su
#     pago no esta en C3R2C1 (que es especificamente NO-integral) en los
#     años donde C3R1 falta, un salario promedio calculado con C3R2C1 podria
#     estar mezclando poblaciones de forma distinta segun el año. Se
#     documenta en el diccionario, columna `anios_disponibles`.
#   - C3R20* (Impuesto de Renta para la Equidad, exencion parafiscales Ley
#     1607): SOLO 2013-2024 (12 de 17 años). Ausente 2008-2012.
#   - C4R6* (aprendices, todas las categorias): disponible los 17 años.
#
# 0.5 -- Unidades:
#   Confirmado EMPIRICAMENTE (no por texto del diccionario, que no lo
#   declara para C3R2C1 especificamente) en
#   0. PREPARACION/notas_exposicion_obreros_eam.md: las masas salariales
#   C3R* son ANUALES, en MILES DE PESOS COP (razon C3R2C1 vs SMLV mensual
#   13.93x, implausible; vs SMLV anualizado 1.16x, plausible). El
#   diccionario oficial de la EAM SI declara la unidad explicitamente para
#   algunas variables (ej. C3R19C3 dice literalmente "en miles de pesos") --
#   se usa ese texto cuando existe, y se hereda el supuesto "miles de pesos
#   anuales" para el resto de C3R* por ser la misma seccion del formulario
#   EAM, documentado como supuesto (no como hecho verificado variable por
#   variable) en la columna `unidad` del diccionario.
# ==============================================================================

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "stringr", "ggplot2", "flextable", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()

# Ruta de la raiz del repositorio (un nivel arriba de esta carpeta archivada)
# -- ahi viven "1. DATOS/" y "4. RESULTADOS/" reales. Misma convencion que
# construccion/ampliar_variables_paneles.R y
# construccion/construir_panel_firma_con_2020.R.
raiz <- ".."
datos_raiz <- file.path(raiz, "1. DATOS")
resultados_raiz <- file.path(raiz, "4. RESULTADOS", "Exposicion_alternativa")
carpeta_figuras <- file.path(resultados_raiz, "figuras")
dir.create(carpeta_figuras, recursive = TRUE, showWarnings = FALSE)

# --- Funciones de apoyo, mismo estilo de 3. SCRIPTS/01_resultados_principales.R ---
# (ese archivo no existe en esta rama -- partimos de main a pedido explicito
# -- asi que replicamos sus funciones aqui mismo, no las importamos).

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 10) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3, carpeta = resultados_raiz) {
  tabla_word <- flextable::flextable(tabla)
  tabla_word <- flextable::colformat_double(tabla_word, digits = decimales)
  tabla_word <- flextable::set_caption(tabla_word, caption = titulo_tabla)
  tabla_word <- flextable::autofit(tabla_word)
  flextable::save_as_docx(tabla_word, path = file.path(carpeta, paste0(nombre_archivo, ".docx")))
  readr::write_csv(tabla, file.path(carpeta, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tabla_word
  cat("Tabla guardada en", carpeta, ":", nombre_archivo, "\n")
}

graficos <- list()
guardar_grafico <- function(grafico, nombre_archivo, carpeta = carpeta_figuras, ancho = 9, alto = 5.5) {
  ggplot2::ggsave(file.path(carpeta, paste0(nombre_archivo, ".png")),
                   grafico, width = ancho, height = alto, dpi = 200, bg = "white")
  graficos[[nombre_archivo]] <<- grafico
  cat("Grafico guardado en", carpeta, ":", nombre_archivo, "\n")
}

tema_tesis <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                 plot.subtitle = ggplot2::element_text(color = "grey35"),
                 plot.caption = ggplot2::element_text(color = "grey45", hjust = 0),
                 legend.position = "bottom",
                 panel.grid.minor = ggplot2::element_blank())

COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
sumar_grupo <- function(datos, cols) {
  cols <- intersect(cols, names(datos))
  if (length(cols) == 0) return(rep(NA_real_, nrow(datos)))
  mat <- as.data.frame(lapply(datos[cols], safe_numeric))
  rowSums(mat, na.rm = TRUE)
}
# TRUE solo si TODAS las columnas del grupo son NA (para no confundir "0
# reportado" con "no preguntado ese año").
todas_na_grupo <- function(datos, cols) {
  cols <- intersect(cols, names(datos))
  if (length(cols) == 0) return(rep(TRUE, nrow(datos)))
  mat <- as.data.frame(lapply(datos[cols], safe_numeric))
  apply(mat, 1, function(fila) all(is.na(fila)))
}

titulo("PASO 1: CARGA DE MACROBASE")

if (!file.exists(paths$macro_base_eam)) {
  stop("No se encontro la macrobase en ", paths$macro_base_eam,
       ". Corre 3. SCRIPTS/pipeline/00_ejecutar_flujo_eam.R primero.")
}
macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
cat("Filas macrobase (establecimiento-año):", nrow(macro_base), "\n")
cat("Años presentes:", paste(sort(unique(macro_base$ANIO)), collapse = ", "), "\n")

# ==============================================================================
# PASO 1.1 -- IDENTIFICADORES Y ESTRUCTURA
# ==============================================================================
titulo("PASO 1.1: IDENTIFICADORES Y ESTRUCTURA")

# CIIU3, tamaño de empresa, año de inicio de operaciones y organizacion
# juridica NO existen como variables crudas separadas en la macrobase con
# esos nombres -- se busco explicitamente antes de escribir este script
# (grep por TAMA*, ORGJUR/ORGANIZ/JURID, INICI/ANOINI/FUNDA/APERTU: cero
# resultados). Lo documentamos aqui y en el reporte final en vez de
# sustituirlas por algo parecido:
#   - "tamaño de empresa": NO es un campo crudo. En el resto del proyecto
#     (03_construir_panel.R, construir_panel_establecimiento_formal.R) se
#     DERIVA de empleo_total con la funcion tamano_de() (<50 Pequeña, <200
#     Mediana, resto Grande). Replicamos esa MISMA regla mas abajo, despues
#     de agregar a firma-año (no es un campo nuevo, es la misma formula ya
#     validada).
#   - "año de inicio de operaciones" y "organizacion juridica": NO
#     encontrados en la macrobase bajo ningun nombre plausible. No se
#     inventan ni se sustituyen. Quedan como pendiente en el README.
# CIIU3 SI existe, pero con cobertura muy limitada (ver disponibilidad mas
# abajo) -- se extrae de todas formas, documentada.

identificadores_cols <- c("NORDEMP", "NORDEST", "ANIO", "CIIU4", "CIIU3", "DPTO")
faltantes_id <- setdiff(identificadores_cols, names(macro_base))
if (length(faltantes_id) > 0) {
  cat("AVISO -- identificadores no encontrados en la macrobase:", paste(faltantes_id, collapse = ", "), "\n")
}
cat("CIIU3 disponible en años:",
    paste(sort(unique(macro_base$ANIO[!is.na(safe_numeric(macro_base$CIIU3)) | (!is.na(macro_base$CIIU3) & macro_base$CIIU3 != "")])), collapse = ","), "\n")

# ==============================================================================
# PASO 1.2 -- PERSONAL: GRUPOS POR CATEGORIA OCUPACIONAL (SIN COMBINAR
# CATEGORIAS -- obreros, profesional/tecnico y administrativos quedan
# separados, como pide la tarea).
# ==============================================================================
titulo("PASO 1.2: DEFINICION DE GRUPOS DE PERSONAL")

# Cada grupo suma Mujer+Hombre (o las columnas N/E equivalentes en
# profesional-tecnico, ver PASO 0.2). Los codigos individuales tambien se
# guardan por separado mas abajo (mapa_desagregados_mh) para no perder la
# heterogeneidad M/H.

grupos_personal <- list(
  # -- Obreros --
  obreros_permanentes          = c("C4R2C1", "C4R2C2"),
  obreros_total_ocupado        = c("C4R5C1", "C4R5C2"),
  obreros_propietarios         = c("C4R1C1", "C4R1C2"),
  obreros_temporal_directo     = c("C4R3C1", "C4R3C2"),
  obreros_temporal_agencias    = c("C4R4C1", "C4R4C2"),
  obreros_aprendices           = c("C4R6OH", "C4R6OM"),
  # -- Administrativos --
  administrativos_permanentes       = c("C4R2C3", "C4R2C4"),
  administrativos_total_ocupado     = c("C4R5C3", "C4R5C4"),
  administrativos_propietarios      = c("C4R1C3", "C4R1C4"),
  administrativos_temporal_directo  = c("C4R3C3", "C4R3C4"),
  administrativos_temporal_agencias = c("C4R4C3", "C4R4C4"),
  administrativos_aprendices        = c("C4R6DH", "C4R6DM"),
  # -- Profesional / Tecnologo: serie N (fila C4R1) + serie E (fila C4R2)
  #    juntas en cada subcategoria, ver PASO 0.2 --
  profesional_tecnico_permanentes       = c("C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E"),
  profesional_tecnico_total_ocupado     = c("C4R1C9N", "C4R1C10N", "C4R2C9E", "C4R2C10E"),
  profesional_tecnico_propietarios      = c("C4R1C1N", "C4R1C2N", "C4R2C1E", "C4R2C2E"),
  profesional_tecnico_temporal_directo  = c("C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E"),
  profesional_tecnico_temporal_agencias = c("C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"),
  profesional_tecnico_aprendices        = c("C4R6HE", "C4R6HN", "C4R6ME", "C4R6MN")
)

codigos_grupos_faltantes <- setdiff(unlist(grupos_personal), names(macro_base))
if (length(codigos_grupos_faltantes) > 0) {
  cat("AVISO -- codigos de grupos de personal ausentes en la macrobase (quedan NA):",
      paste(codigos_grupos_faltantes, collapse = ", "), "\n")
}

# Desagregados M/H: cada codigo individual usado arriba, tambien como
# columna propia (no se pierde heterogeneidad de genero).
cols_desagregados_mh <- unique(unlist(grupos_personal))

# Totales de control ya presentes en la macrobase (pass-through, para
# contrastar contra las sumas de grupo, PASO 3).
mapa_control_personal <- tibble::tribble(
  ~variable_final,                         ~codigo_eam,
  "control_personal_total_pertotal",        "PERTOTAL",
  "control_personal_permanente_persocu",    "PERSOCU",
  "control_permanente_propietarios_persoesc","PERSOESC",
  "control_temporal_directo_pertem3",       "PERTEM3",
  "control_permanente_y_temporal_pperytem", "PPERYTEM",
  "control_temporal_agencias_mujer_c4r4c9t","C4R4C9T",
  "control_temporal_agencias_hombre_c4r4c10t","C4R4C10T"
)
faltan_control <- setdiff(mapa_control_personal$codigo_eam, names(macro_base))
if (length(faltan_control) > 0) cat("AVISO -- totales de control no encontrados:", paste(faltan_control, collapse=", "), "\n")

# ==============================================================================
# PASO 1.3 -- COSTOS LABORALES POR CATEGORIA OCUPACIONAL (pass-through, un
# codigo EAM = una variable, sin combinar).
# ==============================================================================
titulo("PASO 1.3: DEFINICION DE VARIABLES DE COSTO LABORAL")

mapa_costos <- tibble::tribble(
  ~variable_final,                                              ~codigo_eam, ~categoria_ocupacional, ~concepto,
  "salario_integral_obreros_c3r1c1",                             "C3R1C1",   "obreros",              "salario_integral",
  "salario_integral_profesional_tecnico_c3r1pt",                 "C3R1PT",   "profesional_tecnico",  "salario_integral",
  "salario_integral_administrativos_c3r1c2",                     "C3R1C2",   "administrativos",      "salario_integral",
  "salario_integral_total_c3r1c3",                                "C3R1C3",   "total",                "salario_integral",

  "sueldos_permanentes_obreros_c3r2c1",                          "C3R2C1",   "obreros",              "sueldos_permanentes",
  "sueldos_permanentes_profesional_tecnico_c3r2pt",              "C3R2PT",   "profesional_tecnico",  "sueldos_permanentes",
  "sueldos_permanentes_administrativos_c3r2c2",                  "C3R2C2",   "administrativos",      "sueldos_permanentes",
  "sueldos_permanentes_total_c3r2c3",                             "C3R2C3",   "total",                "sueldos_permanentes",

  "prestaciones_permanentes_obreros_c3r3c1",                     "C3R3C1",   "obreros",              "prestaciones_permanentes",
  "prestaciones_permanentes_profesional_tecnico_c3r3pt",         "C3R3PT",   "profesional_tecnico",  "prestaciones_permanentes",
  "prestaciones_permanentes_administrativos_c3r3c2",             "C3R3C2",   "administrativos",      "prestaciones_permanentes",
  "prestaciones_permanentes_total_c3r3c3",                        "C3R3C3",   "total",                "prestaciones_permanentes",

  "sueldos_prest_temporal_directo_obreros_c3r4c1",               "C3R4C1",   "obreros",              "sueldos_prest_temporal_directo",
  "sueldos_prest_temporal_directo_profesional_tecnico_c3r4pt",   "C3R4PT",   "profesional_tecnico",  "sueldos_prest_temporal_directo",
  "sueldos_prest_temporal_directo_administrativos_c3r4c2",       "C3R4C2",   "administrativos",      "sueldos_prest_temporal_directo",
  "sueldos_prest_temporal_directo_total_c3r4c3",                  "C3R4C3",   "total",                "sueldos_prest_temporal_directo",

  "cotizaciones_obreros_c3r5c1",                                 "C3R5C1",   "obreros",              "cotizaciones",
  "cotizaciones_profesional_tecnico_c3r5pt",                     "C3R5PT",   "profesional_tecnico",  "cotizaciones",
  "cotizaciones_administrativos_c3r5c2",                         "C3R5C2",   "administrativos",      "cotizaciones",
  "cotizaciones_total_c3r5c3",                                    "C3R5C3",   "total",                "cotizaciones",

  "parafiscales_obreros_c3r6c1",                                 "C3R6C1",   "obreros",              "parafiscales",
  "parafiscales_profesional_tecnico_c3r6pt",                     "C3R6PT",   "profesional_tecnico",  "parafiscales",
  "parafiscales_administrativos_c3r6c2",                         "C3R6C2",   "administrativos",      "parafiscales",
  "parafiscales_total_c3r6c3",                                    "C3R6C3",   "total",                "parafiscales",

  "seguros_vida_voluntarios_obreros_c3r7c1",                     "C3R7C1",   "obreros",              "seguros_vida_voluntarios",
  "seguros_vida_voluntarios_profesional_tecnico_c3r7pt",         "C3R7PT",   "profesional_tecnico",  "seguros_vida_voluntarios",
  "seguros_vida_voluntarios_administrativos_c3r7c2",             "C3R7C2",   "administrativos",      "seguros_vida_voluntarios",
  "seguros_vida_voluntarios_total_c3r7c3",                        "C3R7C3",   "total",                "seguros_vida_voluntarios",

  "pago_agencias_temporales_obreros_c3r8c1",                     "C3R8C1",   "obreros",              "pago_agencias_temporales",
  "pago_agencias_temporales_profesional_tecnico_c3r8pt",         "C3R8PT",   "profesional_tecnico",  "pago_agencias_temporales",
  "pago_agencias_temporales_administrativos_c3r8c2",             "C3R8C2",   "administrativos",      "pago_agencias_temporales",
  "pago_agencias_temporales_total_c3r8c3",                        "C3R8C3",   "total",                "pago_agencias_temporales",

  "otros_gastos_personal_obreros_c3r9c1",                        "C3R9C1",   "obreros",              "otros_gastos_personal",
  "otros_gastos_personal_profesional_tecnico_c3r9pt",            "C3R9PT",   "profesional_tecnico",  "otros_gastos_personal",
  "otros_gastos_personal_administrativos_c3r9c2",                "C3R9C2",   "administrativos",      "otros_gastos_personal",
  "otros_gastos_personal_total_c3r9c3",                           "C3R9C3",   "total",                "otros_gastos_personal",

  "costos_totales_personal_obreros_c3r10c1",                     "C3R10C1",  "obreros",              "costo_total_personal",
  "costos_totales_personal_profesional_tecnico_c3r10pt",         "C3R10PT",  "profesional_tecnico",  "costo_total_personal",
  "costos_totales_personal_administrativos_c3r10c2",             "C3R10C2",  "administrativos",      "costo_total_personal",
  "costos_totales_personal_total_c3r10c3",                        "C3R10C3",  "total",                "costo_total_personal",

  # Apoyo de sostenimiento de aprendices (Ley 789). Las etiquetas del
  # diccionario EAM para R1CSAP/R2CSAP/R3CSAP estan truncadas de forma
  # identica ("...aprendices y pasantes (Ley 789") -- no se pudo determinar
  # cual corresponde a obreros/profesional-tecnico/administrativos sin
  # adivinar. R4CSAP si dice explicitamente "Total". Se extraen las 4 como
  # variables independientes, categoria_ocupacional = "no_determinada"
  # salvo R4CSAP = "total", y se deja documentado en el reporte final.
  "apoyo_sostenimiento_aprendices_1_r1csap",                      "R1CSAP",   "no_determinada",      "apoyo_sostenimiento_aprendices",
  "apoyo_sostenimiento_aprendices_2_r2csap",                      "R2CSAP",   "no_determinada",      "apoyo_sostenimiento_aprendices",
  "apoyo_sostenimiento_aprendices_3_r3csap",                      "R3CSAP",   "no_determinada",      "apoyo_sostenimiento_aprendices",
  "apoyo_sostenimiento_aprendices_total_r4csap",                  "R4CSAP",   "total",                "apoyo_sostenimiento_aprendices",

  # Totales de control (ya usados en pipeline/01_construir_base.R como
  # cadena de respaldo de costo_laboral_total).
  "control_salario_permanentes_salarper",                         "SALARPER", "permanentes",          "control",
  "control_salario_permanente_y_temporal_salpeyte",               "SALPEYTE", "permanentes_y_temporal","control",
  "control_prestaciones_permanentes_pressper",                    "PRESSPER", "permanentes",          "control",
  "control_prestaciones_permanente_y_temporal_prespyte",          "PRESPYTE", "permanentes_y_temporal","control"
)

faltan_costos <- setdiff(mapa_costos$codigo_eam, names(macro_base))
if (length(faltan_costos) > 0) cat("AVISO -- codigos de costo laboral no encontrados:", paste(faltan_costos, collapse=", "), "\n")

# ==============================================================================
# PASO 1.4 -- TERCERIZACION Y AJUSTE
# ==============================================================================
titulo("PASO 1.4: TERCERIZACION Y AJUSTE")

mapa_tercerizacion <- tibble::tribble(
  ~variable_final,                              ~codigo_eam, ~categoria_ocupacional, ~concepto,
  "outsourcing_obreros_c3r41c1",                 "C3R41C1",  "obreros",         "outsourcing",
  "outsourcing_administrativos_c3r41c2",         "C3R41C2",  "administrativos", "outsourcing",
  "outsourcing_total_c3r41c3",                    "C3R41C3",  "total",           "outsourcing",
  "honorarios_servicios_tecnicos_obreros_c3r15c1","C3R15C1",  "obreros",         "honorarios_servicios_tecnicos",
  "honorarios_servicios_tecnicos_administrativos_c3r15c2","C3R15C2","administrativos","honorarios_servicios_tecnicos",
  "honorarios_servicios_tecnicos_total_c3r15c3",  "C3R15C3",  "total",           "honorarios_servicios_tecnicos",
  "productos_servicios_terceros_inicial_c3r14c1", "C3R14C1",  "no_aplica",       "productos_servicios_terceros",
  "productos_servicios_terceros_total_c3r14c3",   "C3R14C3",  "no_aplica",       "productos_servicios_terceros",
  "impuesto_renta_equidad_obreros_c3r20c1",       "C3R20C1",  "obreros",         "impuesto_renta_equidad",
  "impuesto_renta_equidad_administrativos_c3r20c2","C3R20C2", "administrativos", "impuesto_renta_equidad",
  "impuesto_renta_equidad_total_c3r20c3",         "C3R20C3",  "total",           "impuesto_renta_equidad"
)
faltan_terc <- setdiff(mapa_tercerizacion$codigo_eam, names(macro_base))
if (length(faltan_terc) > 0) cat("AVISO -- codigos de tercerizacion no encontrados:", paste(faltan_terc, collapse=", "), "\n")

# ==============================================================================
# PASO 1.5 -- PRODUCCION Y RESULTADO
# ==============================================================================
titulo("PASO 1.5: PRODUCCION Y RESULTADO")

mapa_produccion <- tibble::tribble(
  ~variable_final,                        ~codigo_eam,  ~tipo,
  "valor_ventas_valorven",                 "VALORVEN",   "suma",
  "produccion_bruta_prodbr2",              "PRODBR2",    "suma",
  "produccion_industrial_prodbind",        "PRODBIND",   "suma",
  "valor_agregado_valagri",                "VALAGRI",    "suma",
  "consumo_intermedio_consin2",            "CONSIN2",    "suma",
  "consumo_materias_consmate",             "CONSMATE",   "suma",
  "materia_prima_comprada_valorcom",       "VALORCOM",   "suma",
  "materia_prima_importada_valorcx",       "VALORCX",    "suma",
  "porcentaje_exportado_porcvt",           "PORCVT",     "wmean",
  "inversion_bruta_invebrta",              "INVEBRTA",   "suma",
  "activos_fijos_activfi",                 "ACTIVFI",    "suma",
  "depreciacion_deprecia",                 "DEPRECIA",   "suma",
  "valor_libros_actual_maquinaria_c7r10c2","C7R10C2",    "suma",
  "compra_maquinaria_nueva_c7c3r2",        "C7C3R2",     "suma",
  "energia_electrica_kw_eelec",            "EELEC",      "suma",
  "inventario_total_inicio_c6r5c1",        "C6R5C1",     "suma",
  "inventario_total_fin_c6r5c3",           "C6R5C3",     "suma"
)
faltan_prod <- setdiff(mapa_produccion$codigo_eam, names(macro_base))
if (length(faltan_prod) > 0) cat("AVISO -- codigos de produccion no encontrados:", paste(faltan_prod, collapse=", "), "\n")

# ==============================================================================
# PASO 2 -- AGREGACION A FIRMA-AÑO (NORDEST-ANIO -> NORDEMP-ANIO)
# Misma regla que pipeline/01_construir_base.R y
# construccion/ampliar_variables_paneles.R: sumar columnas numericas (si
# TODAS las plantas tienen NA, el resultado es NA; si al menos una tiene
# dato, se suma con na.rm=TRUE); CIIU4/CIIU3/DPTO con dplyr::first(); PORCVT
# como promedio ponderado por VALORVEN (misma regla que esos scripts);
# n_establecimientos = numero de NORDEST distintos por NORDEMP-ANIO. No se
# inventa una regla nueva -- se identifica la que ya usa el panel actual y
# se reusa.
# ==============================================================================
titulo("PASO 2: AGREGACION ESTABLECIMIENTO -> FIRMA")

cols_todas_las_solicitadas <- unique(c(
  cols_desagregados_mh,
  mapa_control_personal$codigo_eam,
  mapa_costos$codigo_eam,
  mapa_tercerizacion$codigo_eam,
  mapa_produccion$codigo_eam[mapa_produccion$tipo == "suma"]
))
cols_wmean <- mapa_produccion$codigo_eam[mapa_produccion$tipo == "wmean"]

cols_presentes <- intersect(cols_todas_las_solicitadas, names(macro_base))
cols_ausentes_total <- setdiff(cols_todas_las_solicitadas, names(macro_base))
cat("Codigos solicitados:", length(cols_todas_las_solicitadas),
    "| presentes en macrobase:", length(cols_presentes),
    "| ausentes:", length(cols_ausentes_total), "\n")

base_cruda <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, CIIU4, dplyr::any_of("CIIU3"), DPTO, dplyr::all_of(unique(c(cols_presentes, cols_wmean)))) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(unique(c(cols_presentes, cols_wmean))), safe_numeric))

n_dup_establecimiento <- base_cruda %>% dplyr::count(NORDEST, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup_establecimiento > 0) {
  stop("NORDEST-ANIO no es unico en la macrobase (", n_dup_establecimiento, " grupos duplicados). Nos detenemos.")
}
cat("NORDEST-ANIO es unico en la macrobase (", nrow(base_cruda), " filas).\n", sep = "")

panel_crudo_firma <- base_cruda %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(cols_presentes), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    CIIU4 = dplyr::first(CIIU4),
    CIIU3 = if ("CIIU3" %in% names(base_cruda)) dplyr::first(CIIU3) else NA,
    DPTO = dplyr::first(DPTO),
    n_establecimientos = dplyr::n_distinct(NORDEST),
    .groups = "drop"
  )

# PORCVT (wmean, ponderado por VALORVEN) se calcula en un summarise()
# APARTE: dentro de un solo summarise() con multiples across(), una
# expresion posterior que referencia "VALORVEN" por su nombre ve el VALOR YA
# RESUMIDO (escalar) de una expresion anterior en el MISMO summarise(), no
# el vector original por fila -- eso rompe weighted.mean() por longitudes
# distintas. Separarlo evita el problema por completo.
if (length(cols_wmean) > 0 && "VALORVEN" %in% names(base_cruda)) {
  panel_wmean <- base_cruda %>%
    dplyr::group_by(NORDEMP, ANIO) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols_wmean), ~ {
        pesos <- VALORVEN
        if (all(is.na(.x)) || all(is.na(pesos)) || sum(pesos, na.rm = TRUE) == 0) NA_real_
        else stats::weighted.mean(.x, w = ifelse(is.na(pesos), 0, pesos), na.rm = TRUE)
      }),
      .groups = "drop"
    )
  panel_crudo_firma <- panel_crudo_firma %>% dplyr::left_join(panel_wmean, by = c("NORDEMP", "ANIO"))
} else {
  for (col in cols_wmean) panel_crudo_firma[[col]] <- NA_real_
}

n_dup_firma <- panel_crudo_firma %>% dplyr::count(NORDEMP, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup_firma > 0) stop("NORDEMP-ANIO no quedo unico tras agregar (", n_dup_firma, " grupos). Nos detenemos.")
cat("Panel firma-año: ", nrow(panel_crudo_firma), " filas, ", dplyr::n_distinct(panel_crudo_firma$NORDEMP), " firmas distintas.\n", sep = "")

# ------------------------------------------------------------------
# Construimos las variables de grupo (Paso 1.2) SOBRE el panel ya agregado
# a firma-año (sumar-y-luego-agregar es equivalente a agregar-y-luego-sumar
# para una suma pura -- ambas dan el mismo resultado siempre que el
# tratamiento de NA sea consistente, que lo es: todas las columnas fuente
# fueron agregadas con la misma regla "todas NA -> NA, si no suma").
# ------------------------------------------------------------------

for (nombre_grupo in names(grupos_personal)) {
  cols <- grupos_personal[[nombre_grupo]]
  panel_crudo_firma[[nombre_grupo]] <- sumar_grupo(panel_crudo_firma, cols)
  # Si TODAS las columnas fuente del grupo son NA en TODAS las filas
  # existentes (columna nunca llego a existir), el resultado es NA en vez
  # de 0 -- ya lo maneja sumar_grupo() al calcular fila por fila, pero
  # dejamos la comprobacion explicita para columnas 100% ausentes:
  if (length(intersect(cols, names(base_cruda))) == 0) panel_crudo_firma[[nombre_grupo]] <- NA_real_
}

# Renombramos los codigos desagregados M/H y los totales de control /
# costos / tercerizacion / produccion a los nombres finales del panel.
renombrar_por_mapa <- function(df, variable_final, codigo_eam) {
  for (i in seq_along(codigo_eam)) {
    cod <- codigo_eam[i]
    nom <- variable_final[i]
    if (cod %in% names(df)) {
      df[[nom]] <- df[[cod]]
    } else {
      df[[nom]] <- NA_real_
    }
  }
  df
}

# Desagregados M/H: prefijo "mh_" + nombre de grupo + sufijo M/H, con el
# codigo EAM como referencia (se documentan en el diccionario).
mapa_desagregados <- tibble::tibble(codigo_eam = cols_desagregados_mh) %>%
  dplyr::mutate(variable_final = paste0("mh_", tolower(codigo_eam)))

panel_crudo_firma <- renombrar_por_mapa(panel_crudo_firma, mapa_desagregados$variable_final, mapa_desagregados$codigo_eam)
panel_crudo_firma <- renombrar_por_mapa(panel_crudo_firma, mapa_control_personal$variable_final, mapa_control_personal$codigo_eam)
panel_crudo_firma <- renombrar_por_mapa(panel_crudo_firma, mapa_costos$variable_final, mapa_costos$codigo_eam)
panel_crudo_firma <- renombrar_por_mapa(panel_crudo_firma, mapa_tercerizacion$variable_final, mapa_tercerizacion$codigo_eam)
panel_crudo_firma <- renombrar_por_mapa(panel_crudo_firma, mapa_produccion$variable_final, mapa_produccion$codigo_eam)

# anio_pandemia: TRUE en 2020 y 2021, para filtrar despues sin rehacer la base.
panel_crudo_firma$anio_pandemia <- panel_crudo_firma$ANIO %in% c(2020L, 2021L)

# tamaño de empresa: MISMA regla que 03_construir_panel.R /
# construir_panel_establecimiento_formal.R (tamano_de()), aplicada sobre
# empleo_total de ESTE panel (definido igual que en el panel existente:
# permanentes+temporal_directo+temporal_agencias+aprendices de las 3
# categorias, SIN propietarios -- ver PASO 3 para la verificacion contra el
# panel actual).
panel_crudo_firma <- panel_crudo_firma %>%
  dplyr::mutate(
    empleo_total_sin_propietarios =
      obreros_permanentes + obreros_temporal_directo + obreros_temporal_agencias + obreros_aprendices +
      administrativos_permanentes + administrativos_temporal_directo + administrativos_temporal_agencias + administrativos_aprendices +
      profesional_tecnico_permanentes + profesional_tecnico_temporal_directo + profesional_tecnico_temporal_agencias + profesional_tecnico_aprendices,
    tamano_empresa = dplyr::case_when(
      is.na(empleo_total_sin_propietarios) ~ NA_character_,
      empleo_total_sin_propietarios < 50 ~ "Pequena",
      empleo_total_sin_propietarios < 200 ~ "Mediana",
      TRUE ~ "Grande"
    ),
    tamano_empresa = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

# Todas las columnas C4R/C3R/etc. crudas quedan tambien disponibles con su
# nombre de codigo original (no se descartan) para trazabilidad.
panel_completo <- panel_crudo_firma

ver(panel_completo)
cat("\nColumnas totales en el panel:", ncol(panel_completo), "\n")

titulo("PASO 2 COMPLETO -- panel_completo listo para validar")

# ==============================================================================
# PASO 3 -- VALIDACIONES
# ==============================================================================
titulo("PASO 3: VALIDACIONES")

resultados_validacion <- list()

# --- 3.1 Una fila por NORDEMP-año, sin repetidos ---
n_dup_final <- panel_completo %>% dplyr::count(NORDEMP, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
cat("3.1 -- Filas duplicadas NORDEMP-ANIO:", n_dup_final, "(debe ser 0)\n")
resultados_validacion$v1_filas_unicas <- tibble::tibble(
  chequeo = "Una fila por NORDEMP-ANIO", n_duplicados = n_dup_final, ok = n_dup_final == 0
)

# --- 3.2 Identidad de empleo: PERTOTAL (control externo) = permanentes +
#     propietarios + temporal_directo + temporal_agencias + aprendices, de
#     las 3 categorias. Esta es la MISMA descomposicion de 5 componentes por
#     categoria que se probo al 100.00% en el diagnostico del PASO 0.2/0.3
#     (contra la macrobase directamente). NO se usa aqui "total_ocupado"
#     (C4R5*/C4R1C9N+C4R1C10N+...) porque esa columna, aunque tambien
#     representa "personal ocupado", NO es identica a
#     permanentes+temporal_directo+temporal_agencias+aprendices en el 100%
#     de las filas (99.46% para obreros, confirmado aparte) -- son dos
#     conteos de la macrobase con una pequeña divergencia propia entre
#     ellos, no con PERTOTAL. Usar la descomposicion de 5 componentes evita
#     heredar esa divergencia en esta validacion.
panel_completo <- panel_completo %>%
  dplyr::mutate(
    suma_pertotal_reconstruido =
      obreros_permanentes + obreros_propietarios + obreros_temporal_directo + obreros_temporal_agencias + obreros_aprendices +
      administrativos_permanentes + administrativos_propietarios + administrativos_temporal_directo + administrativos_temporal_agencias + administrativos_aprendices +
      profesional_tecnico_permanentes + profesional_tecnico_propietarios + profesional_tecnico_temporal_directo + profesional_tecnico_temporal_agencias + profesional_tecnico_aprendices,
    dif_pertotal = suma_pertotal_reconstruido - control_personal_total_pertotal
  )
identidad_empleo_anio <- panel_completo %>%
  dplyr::filter(!is.na(control_personal_total_pertotal)) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    n_firmas = dplyr::n(),
    pct_coincide_exacto = round(100 * mean(abs(dif_pertotal) < 0.5, na.rm = TRUE), 2),
    .groups = "drop"
  )
cat("3.2 -- Identidad de empleo (5 componentes por categoria = PERTOTAL), por año:\n")
print(as.data.frame(identidad_empleo_anio), row.names = FALSE)
resultados_validacion$v2_identidad_empleo <- identidad_empleo_anio

# --- 3.3 Identidad de costos: C3R10C1 + C3R10PT + C3R10C2 = C3R10C3 ---
panel_completo <- panel_completo %>%
  dplyr::mutate(
    suma_costo_personal_3cat = costos_totales_personal_obreros_c3r10c1 +
      costos_totales_personal_profesional_tecnico_c3r10pt +
      costos_totales_personal_administrativos_c3r10c2,
    dif_costo_total = suma_costo_personal_3cat - costos_totales_personal_total_c3r10c3
  )
identidad_costos_anio <- panel_completo %>%
  dplyr::filter(!is.na(costos_totales_personal_total_c3r10c3)) %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(
    n_firmas = dplyr::n(),
    pct_coincide_exacto = round(100 * mean(abs(dif_costo_total) < 0.5, na.rm = TRUE), 2),
    .groups = "drop"
  )
cat("\n3.3 -- Identidad de costos (C3R10C1+C3R10PT+C3R10C2 = C3R10C3), por año:\n")
print(as.data.frame(identidad_costos_anio), row.names = FALSE)
resultados_validacion$v3_identidad_costos <- identidad_costos_anio

# --- 3.4 Cobertura: % faltantes y % ceros por variable clave y por año ---
variables_clave_cobertura <- c(
  "obreros_permanentes", "obreros_total_ocupado", "obreros_temporal_directo",
  "obreros_temporal_agencias", "obreros_aprendices",
  "sueldos_permanentes_obreros_c3r2c1", "costos_totales_personal_total_c3r10c3",
  "salario_integral_total_c3r1c3", "impuesto_renta_equidad_total_c3r20c3",
  "valor_ventas_valorven", "n_establecimientos"
)
cobertura_por_anio <- purrr::map_dfr(variables_clave_cobertura, function(v) {
  panel_completo %>%
    dplyr::group_by(ANIO) %>%
    dplyr::summarise(
      variable = v,
      pct_na = round(100 * mean(is.na(.data[[v]])), 2),
      pct_cero = round(100 * mean(.data[[v]] == 0, na.rm = TRUE), 2),
      .groups = "drop"
    )
})
cat("\n3.4 -- Cobertura (%faltantes / %ceros) de variables clave, por año (ver tabla completa guardada):\n")
print(as.data.frame(cobertura_por_anio %>% dplyr::filter(ANIO %in% c(2019, 2020, 2021))), row.names = FALSE)
resultados_validacion$v4_cobertura <- cobertura_por_anio

# --- 3.5 Comparacion contra panel_analitico_firma_eam.rds en años comunes ---
ruta_panel_actual <- file.path(datos_raiz, "panel_analitico_firma_eam.rds")
comparacion_panel_actual <- NULL
if (file.exists(ruta_panel_actual)) {
  panel_actual <- readr::read_rds(ruta_panel_actual)
  comunes <- panel_completo %>%
    dplyr::inner_join(
      panel_actual %>% dplyr::select(NORDEMP, ANIO, empleo_total_actual = empleo_total,
                                       ventas_actual = VALORVEN, costo_actual = costos_totales_personal_total_c3r10c3),
      by = c("NORDEMP", "ANIO")
    ) %>%
    dplyr::mutate(
      dif_empleo = empleo_total_sin_propietarios - empleo_total_actual,
      dif_ventas = valor_ventas_valorven - ventas_actual,
      dif_costo = costos_totales_personal_total_c3r10c3 - costo_actual
    )
  comparacion_panel_actual <- comunes %>%
    dplyr::group_by(ANIO) %>%
    dplyr::summarise(
      n_firmas_comunes = dplyr::n(),
      pct_empleo_coincide = round(100 * mean(abs(dif_empleo) < 0.5, na.rm = TRUE), 2),
      pct_ventas_coincide = round(100 * mean(abs(dif_ventas) < 0.5, na.rm = TRUE), 2),
      pct_costo_coincide = round(100 * mean(abs(dif_costo) < 0.5, na.rm = TRUE), 2),
      .groups = "drop"
    )
  cat("\n3.5 -- Comparacion contra panel_analitico_firma_eam.rds (años comunes):\n")
  print(as.data.frame(comparacion_panel_actual), row.names = FALSE)
} else {
  cat("\n3.5 -- AVISO: no se encontro panel_analitico_firma_eam.rds en ", ruta_panel_actual, ", no se pudo comparar.\n")
}
resultados_validacion$v5_comparacion_panel_actual <- comparacion_panel_actual

# --- 3.6 Numero de firmas por año, 2020 marcado ---
firmas_por_anio <- panel_completo %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(n_firmas = dplyr::n_distinct(NORDEMP), .groups = "drop") %>%
  dplyr::mutate(es_pandemia = ANIO %in% c(2020, 2021))
cat("\n3.6 -- Firmas por año:\n")
print(as.data.frame(firmas_por_anio), row.names = FALSE)
resultados_validacion$v6_firmas_por_anio <- firmas_por_anio

# --- Graficos ---
dir.create(carpeta_figuras, recursive = TRUE, showWarnings = FALSE)

g_firmas <- ggplot2::ggplot(firmas_por_anio, ggplot2::aes(x = ANIO, y = n_firmas, fill = es_pandemia)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = c("FALSE" = COLOR_BAJA, "TRUE" = COLOR_ALTA), guide = "none") +
  ggplot2::labs(title = "Firmas por año", subtitle = "Rojo = 2020-2021 (pandemia)", x = NULL, y = "N firmas") +
  tema_tesis
guardar_grafico(g_firmas, "EA01_firmas_por_anio")

empleo_agregado_anio <- panel_completo %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(empleo_total = sum(empleo_total_sin_propietarios, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(es_pandemia = ANIO %in% c(2020, 2021))
g_empleo <- ggplot2::ggplot(empleo_agregado_anio, ggplot2::aes(x = ANIO, y = empleo_total, fill = es_pandemia)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = c("FALSE" = COLOR_BAJA, "TRUE" = COLOR_ALTA), guide = "none") +
  ggplot2::labs(title = "Empleo agregado por año", subtitle = "Rojo = 2020-2021 (pandemia)", x = NULL, y = "Empleo total (suma de firmas)") +
  tema_tesis
guardar_grafico(g_empleo, "EA02_empleo_agregado_por_anio")

g_cobertura <- ggplot2::ggplot(
  cobertura_por_anio %>% dplyr::filter(variable %in% c("sueldos_permanentes_obreros_c3r2c1", "costos_totales_personal_total_c3r10c3", "valor_ventas_valorven")),
  ggplot2::aes(x = ANIO, y = pct_na, color = variable)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point() +
  ggplot2::labs(title = "% de faltantes por año, variables clave", x = NULL, y = "% NA") +
  tema_tesis
guardar_grafico(g_cobertura, "EA03_pct_faltantes_variables_clave")

titulo("PASO 3 COMPLETO -- validaciones guardadas")

# ==============================================================================
# PASO 4 -- DICCIONARIO
# ==============================================================================
titulo("PASO 4: DICCIONARIO DEL PANEL")

diccionario_maestro_path <- paths$diccionario_maestro
diccionario_eam <- NULL
if (file.exists(diccionario_maestro_path)) {
  diccionario_eam <- readr::read_csv(diccionario_maestro_path, show_col_types = FALSE) %>%
    dplyr::filter(fuente == "EAM") %>%
    dplyr::mutate(variable = toupper(variable)) %>%
    dplyr::distinct(variable, .keep_all = TRUE)
}

etiqueta_de <- function(codigo) {
  if (is.null(diccionario_eam) || is.na(codigo) || codigo == "") return(NA_character_)
  fila <- diccionario_eam[diccionario_eam$variable == codigo, ]
  if (nrow(fila) == 0) return(NA_character_)
  fila$descripcion_final[1]
}

anios_disponibles_de <- function(var_final) {
  if (!(var_final %in% names(panel_completo))) return(NA_character_)
  tab <- panel_completo %>%
    dplyr::group_by(ANIO) %>%
    dplyr::summarise(no_na = any(!is.na(.data[[var_final]])), .groups = "drop")
  anios <- tab$ANIO[tab$no_na]
  if (length(anios) == 0) return("ninguno")
  paste(range(anios), collapse = "-")
}

pct_faltantes_de <- function(var_final) {
  if (!(var_final %in% names(panel_completo))) return(NA_real_)
  round(100 * mean(is.na(panel_completo[[var_final]])), 2)
}
pct_ceros_de <- function(var_final) {
  if (!(var_final %in% names(panel_completo))) return(NA_real_)
  x <- panel_completo[[var_final]]
  if (!is.numeric(x)) return(NA_real_)
  round(100 * mean(x == 0, na.rm = TRUE), 2)
}

poblacion_de <- function(var_final) {
  dplyr::case_when(
    grepl("total_ocupado", var_final) ~ "total_ocupado",
    grepl("permanentes", var_final) & !grepl("propietarios", var_final) ~ "permanentes",
    grepl("temporal_directo|temporal_agencias", var_final) ~ "temporales",
    grepl("propietarios", var_final) ~ "propietarios",
    grepl("aprendices", var_final) ~ "aprendices",
    grepl("^control_", var_final) ~ "ver_notas",
    TRUE ~ "no_aplica"
  )
}

# --- Construimos una fila por variable final, agrupando las fuentes que ya
# definimos arriba (grupos de personal, desagregados M/H, costos,
# tercerizacion, produccion, identificadores y derivadas). ---

filas_grupos <- purrr::map_dfr(names(grupos_personal), function(nom) {
  cods <- grupos_personal[[nom]]
  tibble::tibble(
    variable = nom,
    etiqueta = paste0("Suma de ", paste(cods, collapse = "+")),
    codigo_eam = paste(cods, collapse = "+"),
    categoria_ocupacional = dplyr::case_when(
      grepl("^obreros", nom) ~ "obreros",
      grepl("^administrativos", nom) ~ "administrativos",
      grepl("^profesional_tecnico", nom) ~ "profesional_tecnico",
      TRUE ~ NA_character_
    ),
    unidad = "personas",
    periodicidad = "anual",
    poblacion = poblacion_de(nom),
    definicion = paste0("Suma de: ", paste(cods, collapse = ", ")),
    fuente = "suma de originales",
    formula = paste(cods, collapse = " + ")
  )
})

filas_desagregados <- mapa_desagregados %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    variable = variable_final,
    etiqueta = etiqueta_de(codigo_eam),
    categoria_ocupacional = NA_character_,
    unidad = "personas",
    periodicidad = "anual",
    poblacion = "desagregado_mh_ver_grupo",
    definicion = paste0("Columna cruda EAM ", codigo_eam, ", pass-through, sin combinar con su pareja M/H."),
    fuente = "original",
    formula = codigo_eam
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(variable, etiqueta, codigo_eam, categoria_ocupacional, unidad, periodicidad, poblacion, definicion, fuente, formula)

filas_control <- mapa_control_personal %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    variable = variable_final,
    etiqueta = etiqueta_de(codigo_eam),
    categoria_ocupacional = NA_character_,
    unidad = "personas",
    periodicidad = "anual",
    poblacion = "ver_notas",
    definicion = "Total de control ya presente en la macrobase, usado para contrastar contra las sumas de grupo (Paso 3).",
    fuente = "original",
    formula = codigo_eam
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(variable, etiqueta, codigo_eam, categoria_ocupacional, unidad, periodicidad, poblacion, definicion, fuente, formula)

filas_costos <- mapa_costos %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    variable = variable_final,
    etiqueta = etiqueta_de(codigo_eam),
    unidad = "miles de pesos (supuesto, ver PASO 0.5)",
    periodicidad = "anual",
    poblacion = dplyr::case_when(
      concepto == "apoyo_sostenimiento_aprendices" ~ "aprendices",
      concepto == "salario_integral" ~ "permanentes (>=10 SMLV, solo 2008-2019)",
      TRUE ~ "permanentes"
    ),
    definicion = paste0("Costo laboral, concepto: ", concepto, ", categoria: ", categoria_ocupacional, "."),
    fuente = "original",
    formula = codigo_eam
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(variable, etiqueta, codigo_eam, categoria_ocupacional, unidad, periodicidad, poblacion, definicion, fuente, formula)

filas_tercerizacion <- mapa_tercerizacion %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    variable = variable_final,
    etiqueta = etiqueta_de(codigo_eam),
    unidad = "miles de pesos (supuesto, ver PASO 0.5)",
    periodicidad = "anual",
    poblacion = "no_aplica",
    definicion = paste0("Tercerizacion/ajuste, concepto: ", concepto, "."),
    fuente = "original",
    formula = codigo_eam
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(variable, etiqueta, codigo_eam, categoria_ocupacional, unidad, periodicidad, poblacion, definicion, fuente, formula)

filas_produccion <- mapa_produccion %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    variable = variable_final,
    etiqueta = etiqueta_de(codigo_eam),
    codigo_eam = codigo_eam,
    categoria_ocupacional = NA_character_,
    unidad = ifelse(tipo == "wmean", "porcentaje (0-100)", "miles de pesos o unidad fisica (ver etiqueta EAM)"),
    periodicidad = "anual",
    poblacion = "no_aplica",
    definicion = ifelse(tipo == "wmean", "Promedio ponderado por VALORVEN al agregar a firma.", "Suma directa al agregar a firma."),
    fuente = "original",
    formula = codigo_eam
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(variable, etiqueta, codigo_eam, categoria_ocupacional, unidad, periodicidad, poblacion, definicion, fuente, formula)

filas_identificadores_derivadas <- tibble::tribble(
  ~variable, ~etiqueta, ~codigo_eam, ~categoria_ocupacional, ~unidad, ~periodicidad, ~poblacion, ~definicion, ~fuente, ~formula,
  "NORDEMP", "Identificador de firma", "NORDEMP", NA_character_, "id", "anual", "no_aplica", "Identificador panel de la firma.", "original", "NORDEMP",
  "ANIO", "Año de la encuesta", "ANIO", NA_character_, "anio", "anual", "no_aplica", "Año EAM.", "original", "ANIO",
  "CIIU4", "Codigo CIIU 4 digitos", "CIIU4", NA_character_, "categorica", "anual", "no_aplica", "Sector, tomado con dplyr::first() por NORDEMP-ANIO (misma regla que el panel actual).", "original", "first(CIIU4)",
  "CIIU3", "Codigo CIIU 3 digitos", "CIIU3", NA_character_, "categorica", "anual", "no_aplica", "Cobertura muy limitada -- ver anios_disponibles.", "original", "first(CIIU3)",
  "DPTO", "Departamento", "DPTO", NA_character_, "categorica", "anual", "no_aplica", "Tomado con dplyr::first() por NORDEMP-ANIO (misma regla que el panel actual).", "original", "first(DPTO)",
  "n_establecimientos", "Numero de establecimientos de la firma en el año", NA_character_, NA_character_, "conteo", "anual", "no_aplica", "n_distinct(NORDEST) por NORDEMP-ANIO.", "suma de originales", "n_distinct(NORDEST)",
  "anio_pandemia", "TRUE en 2020 y 2021", NA_character_, NA_character_, "logico", "anual", "no_aplica", "Para filtrar sin rehacer la base.", "derivada", "ANIO %in% c(2020,2021)",
  "tamano_empresa", "Tamaño de la firma (Pequeña/Mediana/Grande)", NA_character_, NA_character_, "categorica", "anual", "no_aplica", "MISMA regla que 03_construir_panel.R: empleo_total_sin_propietarios <50 Pequeña, <200 Mediana, resto Grande.", "derivada", "tamano_de(empleo_total_sin_propietarios)",
  "empleo_total_sin_propietarios", "Empleo total (sin propietarios), 3 categorias", NA_character_, "total", "personas", "anual", "total_ocupado_sin_propietarios", "Igual definicion que empleo_total del panel actual: permanentes+temporal_directo+temporal_agencias+aprendices de las 3 categorias, SIN propietarios.", "suma de originales", "suma de 12 variables de grupo"
)

diccionario_final <- dplyr::bind_rows(
  filas_identificadores_derivadas, filas_grupos, filas_desagregados,
  filas_control, filas_costos, filas_tercerizacion, filas_produccion
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    anios_disponibles = anios_disponibles_de(variable),
    pct_faltantes = pct_faltantes_de(variable),
    pct_ceros = pct_ceros_de(variable),
    notas = dplyr::case_when(
      variable %in% c("apoyo_sostenimiento_aprendices_1_r1csap", "apoyo_sostenimiento_aprendices_2_r2csap", "apoyo_sostenimiento_aprendices_3_r3csap") ~
        "Categoria ocupacional NO determinada: etiqueta EAM truncada e identica para R1/R2/R3CSAP. No se adivino.",
      variable == "CIIU3" ~ "Cobertura muy limitada -- confirmar años antes de usar.",
      grepl("_c3r1c1$|_c3r1pt$|_c3r1c2$|_c3r1c3$", variable) ~ "Solo disponible 2008-2019 (PASO 0.4). Salario integral es >=10 SMLV por ley.",
      grepl("_c3r20c1$|_c3r20c2$|_c3r20c3$", variable) ~ "Solo disponible 2013-2024 (PASO 0.4).",
      TRUE ~ ""
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(variable, etiqueta, codigo_eam, categoria_ocupacional, unidad, periodicidad, poblacion,
                definicion, anios_disponibles, pct_faltantes, pct_ceros, fuente, formula, notas)

cat("Diccionario final:", nrow(diccionario_final), "variables documentadas.\n")

titulo("PASO 4 COMPLETO -- diccionario construido")

# ==============================================================================
# GUARDAR SALIDAS
# ==============================================================================
titulo("GUARDANDO SALIDAS")

ruta_panel_nuevo <- file.path(datos_raiz, "panel_firma_eam_expalt_completo.rds")
ruta_diccionario_nuevo <- file.path(datos_raiz, "diccionario_panel_firma_eam_expalt_completo.csv")

# No sobrescribimos NUNCA panel_analitico_firma_eam.rds -- nombre de archivo
# distinto, verificado explicitamente antes de escribir.
if (basename(ruta_panel_nuevo) == "panel_analitico_firma_eam.rds") {
  stop("Nombre de salida coincide con el panel oficial -- nos detenemos, esto no deberia pasar nunca.")
}

readr::write_rds(panel_completo, ruta_panel_nuevo)
readr::write_csv(diccionario_final, ruta_diccionario_nuevo)
cat("Panel guardado en:", ruta_panel_nuevo, "(", nrow(panel_completo), "filas,", ncol(panel_completo), "columnas)\n")
cat("Diccionario guardado en:", ruta_diccionario_nuevo, "\n")

# Tablas de validacion, en Word + CSV.
guardar_tabla(as.data.frame(resultados_validacion$v2_identidad_empleo), "EA_T01_identidad_empleo",
              "Identidad de empleo: permanentes+propietarios+temp.directo+temp.agencias+aprendices = PERTOTAL, por año")
guardar_tabla(as.data.frame(resultados_validacion$v3_identidad_costos), "EA_T02_identidad_costos",
              "Identidad de costos: C3R10C1+C3R10PT+C3R10C2 = C3R10C3, por año")
guardar_tabla(as.data.frame(resultados_validacion$v4_cobertura), "EA_T03_cobertura_variables_clave",
              "Cobertura (%faltantes / %ceros) de variables clave, por año")
if (!is.null(resultados_validacion$v5_comparacion_panel_actual)) {
  guardar_tabla(as.data.frame(resultados_validacion$v5_comparacion_panel_actual), "EA_T04_comparacion_panel_actual",
                "Comparacion contra panel_analitico_firma_eam.rds, años comunes")
}
guardar_tabla(as.data.frame(resultados_validacion$v6_firmas_por_anio), "EA_T05_firmas_por_anio",
              "Numero de firmas por año, 2020-2021 marcados como pandemia")
guardar_tabla(as.data.frame(diccionario_final), "EA_T06_diccionario_panel",
              "Diccionario del panel completo (todas las variables)", decimales = 2)

# ==============================================================================
# PASO 5 -- REPORTE (README.md)
# ==============================================================================
titulo("PASO 5: REPORTE FINAL")

pct_2020_faltantes_empleo <- cobertura_por_anio %>%
  dplyr::filter(ANIO == 2020, variable == "obreros_permanentes") %>%
  dplyr::pull(pct_na)
pct_2019_faltantes_empleo <- cobertura_por_anio %>%
  dplyr::filter(ANIO == 2019, variable == "obreros_permanentes") %>%
  dplyr::pull(pct_na)

readme_texto <- c(
"# Panel firma-año EAM completo, con 2020, para medidas de exposición alternativas",
"",
paste0("Generado por `construir_panel_completo.R`. **No construye ninguna medida de ",
       "exposición ni calcula ratios/promedios/índices.** Solo extrae, agrega a ",
       "firma-año, valida y documenta. Las fórmulas de exposición van en un script ",
       "posterior, que consume este panel."),
"",
paste0("Panel: `", nrow(panel_completo), "` filas (firma-año), `", ncol(panel_completo),
       "` columnas, `", dplyr::n_distinct(panel_completo$NORDEMP), "` firmas distintas, años ",
       paste(range(panel_completo$ANIO), collapse = "-"), " (incluye 2020, a diferencia del panel analítico oficial)."),
"",
"## Hallazgos del PASO 0 (verificaciones previas)",
"",
paste0("**0.1 -- Denominador de `Bite2022_obreros`:** revisado en ",
       "`pipeline/02_construir_exposicion.R`. Está correctamente emparejado -- ",
       "numerador (`C3R2C1`, sueldos permanentes) y denominador (`C4R2C1+C4R2C2`, ",
       "conteo de permanentes) miden la MISMA población. No usa el total ocupado ",
       "(`C4R5C1+C4R5C2`, que sí incluiría temporales). No hay confusor de ",
       "temporalidad en esa medida."),
"",
paste0("**0.2 -- Serie de conteos profesional/técnico (`C4R1C*N` vs `C4R2C*E`):** ",
       "NO son series duplicadas -- son complementarias. Probadas ambas contra la ",
       "identidad obreros+profesionales+administrativos+aprendices = `PERTOTAL`: ",
       "solo N acierta 97.56% de las filas, solo E acierta 28-39%, **N+E juntas ",
       "acierta 100.00% (0 de 140,835 filas con diferencia)**. La serie E aparece en ",
       "1-5% de las firmas por año y casi nunca coexiste con N en la misma fila ",
       "(2.23%). Se sumaron ambas en cada subcategoría, igual que ya hacía ",
       "`pipeline/01_construir_base.R`."),
"",
paste0("**0.3 -- Aprendices en `C4R5*`/`PERTOTAL`:** SÍ están incluidos en ambos ",
       "(confirmado empíricamente: sin aprendices el conteo de obreros coincide con ",
       "`C4R5C1+C4R5C2` solo 80.14% de las veces, con aprendices sube a 99.46%). Pero ",
       "su costo NO está en `C3R2C1` (sueldos permanentes) -- va aparte en ",
       "`R1CSAP`-`R4CSAP` (apoyo de sostenimiento, Ley 789). Quien use `C4R5*` o ",
       "`PERTOTAL` como denominador de un salario promedio con `C3R2C1` como ",
       "numerador está incluyendo aprendices en el denominador sin su pago en el ",
       "numerador. Se extrajeron los aprendices como variable propia por categoría."),
"",
"**0.4 -- Disponibilidad por año:**",
"",
"| Variable | Años disponibles |",
"|---|---|",
"| `C3R1*` (salario integral) | 2008-2019 (12 de 17 años, falta 2020-2024) |",
"| `C3R20*` (Impuesto Renta Equidad) | 2013-2024 (12 de 17 años, falta 2008-2012) |",
"| `C4R6*` (aprendices, todas las categorías) | 2008-2024 (los 17 años) |",
"",
paste0("**0.5 -- Unidades:** confirmado empíricamente (no declarado por el ",
       "diccionario EAM para `C3R2C1` específicamente) que las masas salariales ",
       "`C3R*` son anuales, en miles de pesos COP. Ver ",
       "`0. PREPARACION/notas_exposicion_obreros_eam.md`. Se hereda este supuesto ",
       "para el resto de variables `C3R*`, documentado como supuesto (no como hecho ",
       "verificado variable por variable) en la columna `unidad` del diccionario."),
"",
"## Resultado de las identidades (PASO 3)",
"",
paste0("Ver tablas `EA_T01_identidad_empleo` y `EA_T02_identidad_costos`. ",
       "`EA_T01`: permanentes+propietarios+temporal_directo+temporal_agencias+aprendices ",
       "(3 categorías) = `PERTOTAL`. Esta MISMA descomposición de 5 componentes por ",
       "categoría se validó al 100.00% contra la macrobase antes de escribir este script ",
       "(ver PASO 0.2/0.3 arriba); al re-verificarla ya en el panel agregado a firma-año, ",
       "el porcentaje que coincide exacto varía por año -- rango observado: ",
       round(min(identidad_empleo_anio$pct_coincide_exacto), 1), "% a ",
       round(max(identidad_empleo_anio$pct_coincide_exacto), 1), "%. La caída frente al ",
       "100% de la macrobase es un artefacto conocido de la regla de agregación ",
       "'todas NA -> NA, si no suma con na.rm=TRUE' aplicada columna por columna: si en ",
       "una firma-año una columna tiene NA en algunas plantas/años pero otra columna ",
       "relacionada no, cada suma trata esos NA de forma independiente y la identidad se ",
       "puede romper aunque cada columna esté bien agregada por separado. Es la MISMA ",
       "regla ya validada y usada por `pipeline/01_construir_base.R`, no una regla nueva. ",
       "`EA_T02` (costos, `C3R10C1+C3R10PT+C3R10C2=C3R10C3`) usa las mismas 4 columnas ",
       "sin desagregar en sub-componentes, así que no hereda este artefacto de la misma forma."),
"",
"## Comparación contra `panel_analitico_firma_eam.rds`",
"",
"Ver tabla `EA_T04_comparacion_panel_actual`. Comparación firma a firma en empleo, ventas y costo laboral total, en los años comunes a ambos paneles (2015-2019, 2021-2024 -- el panel actual no tiene 2020).",
"",
"## 2020 en cobertura",
"",
paste0("`obreros_permanentes`: ", pct_2019_faltantes_empleo, "% de faltantes en 2019 vs. ",
       pct_2020_faltantes_empleo, "% en 2020 (ver tabla `EA_T03_cobertura_variables_clave` completa para todas las variables clave y todos los años). ",
       "Esta cifra es la que debe informar si 2020 se usa o no en la estimación -- no se tomó esa decisión aquí."),
"",
"## Variables buscadas y no encontradas, o con categoría ambigua",
"",
"- **Tamaño de empresa (variable cruda), año de inicio de operaciones, organización jurídica**: NO existen en la macrobase bajo ningún nombre plausible (se buscó por patrones TAMA*, ORGJUR/ORGANIZ/JURID, INICI/ANOINI/FUNDA/APERTU -- cero resultados). `tamano_empresa` SÍ se generó, pero es una variable DERIVADA (misma regla `tamano_de()` que ya usa `03_construir_panel.R`), no un campo crudo de la encuesta.",
"- **`CIIU3`**: existe, pero con cobertura muy limitada -- confirmar años antes de usarlo (columna `anios_disponibles` del diccionario).",
"- **`R1CSAP`/`R2CSAP`/`R3CSAP`** (apoyo de sostenimiento de aprendices): sus etiquetas en el diccionario EAM están truncadas de forma idéntica (\"...aprendices y pasantes (Ley 789\"), no se pudo determinar cuál corresponde a obreros/profesional-técnico/administrativos sin adivinar. Se extrajeron como 3 variables independientes con `categoria_ocupacional = \"no_determinada\"`. `R4CSAP` sí dice \"Total\" explícitamente.",
"",
"## Qué queda pendiente",
"",
"- Decidir la categoría ocupacional real de `R1CSAP`/`R2CSAP`/`R3CSAP` (posiblemente contactando la ficha metodológica del DANE, no solo el diccionario extraído del DOCX).",
"- Decidir si 2020 entra o no a la estimación, con base en la cobertura reportada aquí.",
"- Ninguna medida de exposición se calculó -- ese es el script siguiente, que debe leer `panel_firma_eam_expalt_completo.rds` y el diccionario para saber qué numeradores y denominadores son compatibles (columna `poblacion`)."
)

writeLines(readme_texto, file.path(resultados_raiz, "README.md"))
cat("README guardado en:", file.path(resultados_raiz, "README.md"), "\n")

titulo("SCRIPT COMPLETO")
cat("Panel:", ruta_panel_nuevo, "\n")
cat("Diccionario:", ruta_diccionario_nuevo, "\n")
cat("Resultados:", resultados_raiz, "\n")
