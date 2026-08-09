# Verifica en la macrobase EAM (no solo en el diccionario maestro) la
# estabilidad anio por anio de las columnas C3R (costos de personal) y
# C4R (personal ocupado) relevantes para la medida de exposicion basada en
# "obreros y operarios".
#
# Contexto importante (ver README del commit de este script):
# - Los labels de Stata de las columnas EAM estan presentes y estables entre
#   2008 y 2018, pero DESDE 2019 el DTA ya no trae labels descriptivos (el
#   label queda igual al nombre de la variable en minusculas). Por eso el
#   mapeo variable -> categoria ocupacional -> tipo de vinculacion usado aqui
#   NO se toma de los labels del DTA ni del diccionario maestro (que solo
#   guarda la primera etiqueta no vacia por variable, sin garantizar que
#   describa el mismo concepto en todos los anios), sino del diccionario
#   oficial de DANE `1. DATOS/1. EAM/Diccionario de datos EAM2024.xlsx`,
#   contrastado contra los labels crudos 2008-2018 (coinciden exactamente).
#
# - En C3R (costos, filas R1 a R10) el patron es simple y estable:
#     C1 = Obreros y operarios | C2 = Directivos y administracion/ventas
#     C3 = Total (Obreros + Administrativos + PT) | PT = Profesional/tecnico/tecnologo
#
# - En C4R (personal ocupado) el patron NO es tan simple. Obreros (C1/C2 por
#   sexo) y Directivos-Admin (C3/C4 por sexo) si siguen una fila por tipo de
#   vinculacion (R1=Propietarios, R2=Permanente, R3=Temporal directo,
#   R4=Temporal agencia, R6=Aprendices, R5=Total). Pero Profesional-tecnico
#   NO tiene fila propia: esta empaquetado dentro de la fila 1 (sufijo "N" =
#   Nacional) y la fila 2 (sufijo "E" = Extranjero), donde el NUMERO DE
#   COLUMNA (no la fila) codifica el tipo de vinculacion:
#     C1/C2 = Propietarios | C3/C4 = Permanente | C5/C6 = Temporal directo
#     C7/C8 = Temporal agencia | C9/C10 = Total (todas las vinculaciones)
#   Ademas DANE expone columnas de TOTAL ya agregado (C4R5*, C4R1C9N/C10N,
#   C4R2C9E/C10E, C4R4*T) que en principio ya suman los componentes
#   granulares. Este script deja evidencia de si esos totales oficiales
#   coinciden con la suma manual de sus componentes, para decidir en el
#   Paso 3 si se puede confiar en ellos.
#
# Salidas (no versionadas, en 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/):
# - mapa_columnas_c3r_c4r.csv: mapeo variable -> categoria/tipo de vinculacion
# - estabilidad_columnas_c3r_c4r.csv: tabla pedida (primer/ultimo anio, % no NA)
# - validacion_totales_c4r.csv: comparacion total oficial vs suma de componentes

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "purrr", "tidyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_output_dir <- paths$bases_derivadas_exposicion

macro_path <- paths$macro_base_eam
if (!file.exists(macro_path)) {
  stop("No se encontro la macrobase EAM en: ", macro_path)
}

macro_base <- readr::read_rds(macro_path) %>%
  dplyr::mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO))

names(macro_base) <- toupper(names(macro_base))

# ------------------------------------------------------------------
# 1) Mapa de columnas, confirmado contra el diccionario oficial DANE
#    (1. DATOS/1. EAM/Diccionario de datos EAM2024.xlsx) y los labels
#    Stata 2008-2018 de la propia macrobase.
# ------------------------------------------------------------------

cat_obrero <- "Obreros y operarios"
cat_pt <- "Profesional, tecnico o tecnologo"
cat_admin <- "Directivos y empleados de administracion y ventas"
cat_total3 <- "Total (Obreros + PT + Directivos/Admin)"

tv_propietario <- "Propietarios/socios sin remuneracion fija"
tv_permanente <- "Personal permanente"
tv_temp_directo <- "Temporal contratado directamente"
tv_temp_agencia <- "Temporal contratado via agencia"
tv_aprendiz <- "Aprendices y pasantes"
tv_total <- "Total (todas las vinculaciones)"

mapa_c4r <- tibble::tribble(
  ~variable, ~categoria_ocupacional, ~tipo_vinculacion, ~sexo, ~es_total_agregado,
  "C4R1C1", cat_obrero, tv_propietario, "Mujer", FALSE,
  "C4R1C2", cat_obrero, tv_propietario, "Hombre", FALSE,
  "C4R1C3", cat_admin, tv_propietario, "Mujer", FALSE,
  "C4R1C4", cat_admin, tv_propietario, "Hombre", FALSE,
  "C4R1C1N", cat_pt, tv_propietario, "Mujer", FALSE,
  "C4R1C2N", cat_pt, tv_propietario, "Hombre", FALSE,
  "C4R2C1E", cat_pt, tv_propietario, "Mujer", FALSE,
  "C4R2C2E", cat_pt, tv_propietario, "Hombre", FALSE,
  "C4R4C1T", cat_total3, tv_propietario, "Mujer", TRUE,
  "C4R4C2T", cat_total3, tv_propietario, "Hombre", TRUE,

  "C4R2C1", cat_obrero, tv_permanente, "Mujer", FALSE,
  "C4R2C2", cat_obrero, tv_permanente, "Hombre", FALSE,
  "C4R2C3", cat_admin, tv_permanente, "Mujer", FALSE,
  "C4R2C4", cat_admin, tv_permanente, "Hombre", FALSE,
  "C4R1C3N", cat_pt, tv_permanente, "Mujer", FALSE,
  "C4R1C4N", cat_pt, tv_permanente, "Hombre", FALSE,
  "C4R2C3E", cat_pt, tv_permanente, "Mujer", FALSE,
  "C4R2C4E", cat_pt, tv_permanente, "Hombre", FALSE,
  "C4R4C3T", cat_total3, tv_permanente, "Mujer", TRUE,
  "C4R4C4T", cat_total3, tv_permanente, "Hombre", TRUE,

  "C4R3C1", cat_obrero, tv_temp_directo, "Mujer", FALSE,
  "C4R3C2", cat_obrero, tv_temp_directo, "Hombre", FALSE,
  "C4R3C3", cat_admin, tv_temp_directo, "Mujer", FALSE,
  "C4R3C4", cat_admin, tv_temp_directo, "Hombre", FALSE,
  "C4R1C5N", cat_pt, tv_temp_directo, "Mujer", FALSE,
  "C4R1C6N", cat_pt, tv_temp_directo, "Hombre", FALSE,
  "C4R2C5E", cat_pt, tv_temp_directo, "Mujer", FALSE,
  "C4R2C6E", cat_pt, tv_temp_directo, "Hombre", FALSE,
  "C4R4C5T", cat_total3, tv_temp_directo, "Mujer", TRUE,
  "C4R4C6T", cat_total3, tv_temp_directo, "Hombre", TRUE,

  "C4R4C1", cat_obrero, tv_temp_agencia, "Mujer", FALSE,
  "C4R4C2", cat_obrero, tv_temp_agencia, "Hombre", FALSE,
  "C4R4C3", cat_admin, tv_temp_agencia, "Mujer", FALSE,
  "C4R4C4", cat_admin, tv_temp_agencia, "Hombre", FALSE,
  "C4R1C7N", cat_pt, tv_temp_agencia, "Mujer", FALSE,
  "C4R1C8N", cat_pt, tv_temp_agencia, "Hombre", FALSE,
  "C4R2C7E", cat_pt, tv_temp_agencia, "Mujer", FALSE,
  "C4R2C8E", cat_pt, tv_temp_agencia, "Hombre", FALSE,
  "C4R4C7T", cat_total3, tv_temp_agencia, "Mujer", TRUE,
  "C4R4C8T", cat_total3, tv_temp_agencia, "Hombre", TRUE,

  "C4R6OM", cat_obrero, tv_aprendiz, "Mujer", FALSE,
  "C4R6OH", cat_obrero, tv_aprendiz, "Hombre", FALSE,
  "C4R6DM", cat_admin, tv_aprendiz, "Mujer", FALSE,
  "C4R6DH", cat_admin, tv_aprendiz, "Hombre", FALSE,
  "C4R6MN", cat_pt, tv_aprendiz, "Mujer", FALSE,
  "C4R6HN", cat_pt, tv_aprendiz, "Hombre", FALSE,
  "C4R6ME", cat_pt, tv_aprendiz, "Mujer", FALSE,
  "C4R6HE", cat_pt, tv_aprendiz, "Hombre", FALSE,
  "C4R6TM", cat_total3, tv_aprendiz, "Mujer", TRUE,
  "C4R6TH", cat_total3, tv_aprendiz, "Hombre", TRUE,

  "C4R5C1", cat_obrero, tv_total, "Mujer", TRUE,
  "C4R5C2", cat_obrero, tv_total, "Hombre", TRUE,
  "C4R5C3", cat_admin, tv_total, "Mujer", TRUE,
  "C4R5C4", cat_admin, tv_total, "Hombre", TRUE,
  "C4R1C9N", cat_pt, tv_total, "Mujer", TRUE,
  "C4R1C10N", cat_pt, tv_total, "Hombre", TRUE,
  "C4R2C9E", cat_pt, tv_total, "Mujer", TRUE,
  "C4R2C10E", cat_pt, tv_total, "Hombre", TRUE,
  "C4R4C9T", cat_total3, tv_total, "Mujer", TRUE,
  "C4R4C10T", cat_total3, tv_total, "Hombre", TRUE
) %>%
  dplyr::mutate(capitulo = "C4R (personal ocupado)")

mapa_c3r <- tibble::tribble(
  ~fila, ~concepto,
  "R1", "Salario integral personal permanente (discontinuado; solo 12 de 17 anios en el diccionario maestro)",
  "R2", "Sueldos y salarios del personal permanente",
  "R3", "Prestaciones sociales del personal permanente",
  "R4", "Sueldos/salarios/prestaciones del personal temporal directo",
  "R5", "Cotizaciones patronales obligatorias (salud, ARP, pension)",
  "R6", "Aportes sobre nomina (SENA, cajas de compensacion, ICBF)",
  "R7", "Aportes voluntarios a seguros de vida",
  "R8", "Valor causado por agencias de personal temporal",
  "R9", "Otros gastos del personal",
  "R10", "Total costos y gastos causados por el personal"
) %>%
  tidyr::crossing(
    tibble::tribble(
      ~sufijo, ~categoria_ocupacional, ~es_total_agregado,
      "C1", cat_obrero, FALSE,
      "C2", cat_admin, FALSE,
      "PT", cat_pt, FALSE,
      "C3", cat_total3, TRUE
    )
  ) %>%
  dplyr::mutate(
    variable = paste0("C3R", sub("^R", "", fila), sufijo),
    tipo_vinculacion = concepto,
    capitulo = "C3R (costos de personal)"
  ) %>%
  dplyr::select(variable, categoria_ocupacional, tipo_vinculacion, es_total_agregado, capitulo)

mapa_c4r_final <- mapa_c4r %>%
  dplyr::select(variable, categoria_ocupacional, tipo_vinculacion, es_total_agregado, capitulo)

mapa_columnas <- dplyr::bind_rows(mapa_c4r_final, mapa_c3r)

readr::write_csv(mapa_columnas, file.path(data_output_dir, "mapa_columnas_c3r_c4r.csv"))

# ------------------------------------------------------------------
# 2) Presencia y % no faltante por variable, verificado en la macrobase
#    (no en el diccionario maestro).
# ------------------------------------------------------------------

anios_totales <- sort(unique(macro_base$ANIO))

evaluar_variable <- function(var) {
  if (!var %in% names(macro_base)) {
    return(tibble::tibble(
      variable = var,
      existe_en_macrobase = FALSE,
      primer_anio_con_datos = NA_integer_,
      ultimo_anio_con_datos = NA_integer_,
      anios_con_datos = 0L,
      pct_nordemp_anio_no_missing = NA_real_
    ))
  }

  por_anio <- macro_base %>%
    dplyr::transmute(ANIO, valor = suppressWarnings(as.numeric(.data[[var]]))) %>%
    dplyr::group_by(ANIO) %>%
    dplyr::summarise(no_na = sum(!is.na(valor)), total = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(no_na > 0)

  if (nrow(por_anio) == 0) {
    return(tibble::tibble(
      variable = var,
      existe_en_macrobase = TRUE,
      primer_anio_con_datos = NA_integer_,
      ultimo_anio_con_datos = NA_integer_,
      anios_con_datos = 0L,
      pct_nordemp_anio_no_missing = 0
    ))
  }

  total_no_na <- sum(por_anio$no_na)
  total_filas <- nrow(macro_base)

  tibble::tibble(
    variable = var,
    existe_en_macrobase = TRUE,
    primer_anio_con_datos = min(por_anio$ANIO),
    ultimo_anio_con_datos = max(por_anio$ANIO),
    anios_con_datos = nrow(por_anio),
    pct_nordemp_anio_no_missing = round(100 * total_no_na / total_filas, 2)
  )
}

resultado <- purrr::map_dfr(mapa_columnas$variable, evaluar_variable) %>%
  dplyr::left_join(mapa_columnas, by = "variable") %>%
  dplyr::select(
    variable, capitulo, categoria_ocupacional, tipo_vinculacion, es_total_agregado,
    existe_en_macrobase, primer_anio_con_datos, ultimo_anio_con_datos,
    anios_con_datos, pct_nordemp_anio_no_missing
  ) %>%
  dplyr::arrange(capitulo, categoria_ocupacional, tipo_vinculacion, variable)

readr::write_csv(resultado, file.path(data_output_dir, "estabilidad_columnas_c3r_c4r.csv"))

# ------------------------------------------------------------------
# 3) Validacion: los totales oficiales de C4R, suman lo mismo que sus
#    componentes granulares? Esto decide si en el Paso 3 podemos usar
#    directamente C4R5C1 etc. en vez de sumar 5 columnas por categoria.
# ------------------------------------------------------------------

safe_num <- function(var) {
  if (!var %in% names(macro_base)) return(rep(NA_real_, nrow(macro_base)))
  suppressWarnings(as.numeric(macro_base[[var]]))
}

# Se compara ANIO POR ANIO (no solo agregado sobre el panel completo) para
# poder confirmar de forma explicita que la identidad "total oficial = suma
# de componentes" tambien se cumple en 2019-2024, el tramo donde el DTA ya
# no trae labels de texto que permitan leer directamente el significado de
# cada columna. Un 100% agregado ya implicaba esto logicamente (un solo
# desajuste en cualquier anio habria bajado el agregado por debajo de 100%),
# pero aqui se deja la evidencia desglosada en vez de pedir que se confie en
# esa inferencia.
comparar_total_por_anio <- function(nombre_total, var_total, vars_componentes) {
  total_oficial <- safe_num(var_total)
  suma_manual <- Reduce(`+`, lapply(vars_componentes, function(v) {
    x <- safe_num(v)
    ifelse(is.na(x), 0, x)
  }))

  diff <- total_oficial - suma_manual
  comparables <- !is.na(total_oficial) & !is.na(suma_manual)

  tibble::tibble(
    total_evaluado = nombre_total,
    variable_total = var_total,
    ANIO = macro_base$ANIO,
    comparable = comparables,
    coincide = comparables & abs(diff) < 1e-6
  ) %>%
    dplyr::filter(comparable) %>%
    dplyr::group_by(total_evaluado, variable_total, ANIO) %>%
    dplyr::summarise(
      n_comparable = dplyr::n(),
      n_coincide_exacto = sum(coincide),
      pct_coincide = round(100 * sum(coincide) / dplyr::n(), 2),
      .groups = "drop"
    )
}

validacion_totales_por_anio <- dplyr::bind_rows(
  comparar_total_por_anio("Total Obreros Mujer (C4R5C1)", "C4R5C1", c("C4R1C1", "C4R2C1", "C4R3C1", "C4R4C1", "C4R6OM")),
  comparar_total_por_anio("Total Obreros Hombre (C4R5C2)", "C4R5C2", c("C4R1C2", "C4R2C2", "C4R3C2", "C4R4C2", "C4R6OH")),
  comparar_total_por_anio("Total Admin Mujer (C4R5C3)", "C4R5C3", c("C4R1C3", "C4R2C3", "C4R3C3", "C4R4C3", "C4R6DM")),
  comparar_total_por_anio("Total Admin Hombre (C4R5C4)", "C4R5C4", c("C4R1C4", "C4R2C4", "C4R3C4", "C4R4C4", "C4R6DH")),
  comparar_total_por_anio("Total PT-Nacional Mujer (C4R1C9N)", "C4R1C9N", c("C4R1C1N", "C4R1C3N", "C4R1C5N", "C4R1C7N", "C4R6MN")),
  comparar_total_por_anio("Total PT-Nacional Hombre (C4R1C10N)", "C4R1C10N", c("C4R1C2N", "C4R1C4N", "C4R1C6N", "C4R1C8N", "C4R6HN")),
  comparar_total_por_anio("Total PT-Extranjero Mujer (C4R2C9E)", "C4R2C9E", c("C4R2C1E", "C4R2C3E", "C4R2C5E", "C4R2C7E", "C4R6ME")),
  comparar_total_por_anio("Total PT-Extranjero Hombre (C4R2C10E)", "C4R2C10E", c("C4R2C2E", "C4R2C4E", "C4R2C6E", "C4R2C8E", "C4R6HE"))
)

validacion_totales <- validacion_totales_por_anio %>%
  dplyr::group_by(total_evaluado, variable_total) %>%
  dplyr::summarise(
    n_comparable = sum(n_comparable),
    n_coincide_exacto = sum(n_coincide_exacto),
    # Importante: se cuenta ANTES de recalcular pct_coincide como agregado,
    # para no comparar el resultado agregado contra si mismo (bug detectado
    # en la primera version de este script).
    anios_con_coincidencia_perfecta = sum(pct_coincide == 100),
    anios_evaluados = dplyr::n(),
    pct_coincide_agregado = round(100 * sum(n_coincide_exacto) / sum(n_comparable), 2),
    .groups = "drop"
  )

readr::write_csv(validacion_totales_por_anio, file.path(data_output_dir, "validacion_totales_c4r_por_anio.csv"))
readr::write_csv(validacion_totales, file.path(data_output_dir, "validacion_totales_c4r.csv"))

# ------------------------------------------------------------------
# 3b) C4R4C9T / C4R4C10T: incluyen o excluyen propietarios?
#
# La descripcion oficial de DANE para estas dos variables es generica
# ("Total personal promedio ocupado en el ano - mujeres/hombres") y NO
# aclara si son la suma de las 3 categorias ocupacionales (Obreros +
# Directivos/Admin + PT), que YA incluyen propietarios como uno de sus 5
# tipos de vinculacion internos, o si excluyen aparte a los propietarios.
# Se prueban las dos hipotesis empiricamente:
#   H1 (incluye propietarios): C4R4C9T == C4R5C1 + C4R5C3 + C4R1C9N + C4R2C9E
#   H2 (excluye propietarios): C4R4C9T == H1 - C4R4C1T (resta el subtotal
#       de propietarios de las 3 categorias)
# ------------------------------------------------------------------

verificar_gran_total <- function(sexo, var_grantotal, var_obreros, var_admin, var_pt_nac, var_pt_ext, var_propietarios) {
  grantotal <- safe_num(var_grantotal)
  suma_3cat <- safe_num(var_obreros) + safe_num(var_admin) + safe_num(var_pt_nac) + safe_num(var_pt_ext)
  propietarios <- safe_num(var_propietarios)

  comparables <- !is.na(grantotal) & !is.na(suma_3cat) & !is.na(propietarios)

  diff_h1 <- grantotal - suma_3cat
  diff_h2 <- grantotal - (suma_3cat - propietarios)

  tibble::tibble(
    sexo = sexo,
    variable_grantotal = var_grantotal,
    n_comparable = sum(comparables),
    pct_coincide_H1_incluye_propietarios = round(100 * sum(comparables & abs(diff_h1) < 1e-6) / sum(comparables), 2),
    pct_coincide_H2_excluye_propietarios = round(100 * sum(comparables & abs(diff_h2) < 1e-6) / sum(comparables), 2)
  )
}

verificacion_propietarios <- dplyr::bind_rows(
  verificar_gran_total("Mujer", "C4R4C9T", "C4R5C1", "C4R5C3", "C4R1C9N", "C4R2C9E", "C4R4C1T"),
  verificar_gran_total("Hombre", "C4R4C10T", "C4R5C2", "C4R5C4", "C4R1C10N", "C4R2C10E", "C4R4C2T")
)

readr::write_csv(verificacion_propietarios, file.path(data_output_dir, "verificacion_c4r4c9t_propietarios.csv"))

# ------------------------------------------------------------------
# 4) Reporte en consola
# ------------------------------------------------------------------

script_header("Estabilidad de columnas C3R/C4R por categoria ocupacional")

message("Variables mapeadas: ", nrow(mapa_columnas))
message("Variables NO encontradas en la macrobase: ", sum(!resultado$existe_en_macrobase))
if (any(!resultado$existe_en_macrobase)) {
  print(resultado %>% dplyr::filter(!existe_en_macrobase) %>% dplyr::select(variable, capitulo, categoria_ocupacional, tipo_vinculacion))
}

message("")
message("Variables con cobertura parcial (no cubren 2008-2024 completo):")
parciales <- resultado %>%
  dplyr::filter(existe_en_macrobase, anios_con_datos > 0, anios_con_datos < length(anios_totales))
print(parciales %>% dplyr::select(variable, capitulo, categoria_ocupacional, tipo_vinculacion, primer_anio_con_datos, ultimo_anio_con_datos, anios_con_datos), n = Inf)

message("")
message("Validacion de totales oficiales C4R vs. suma manual de componentes (agregada 2008-2024):")
print(validacion_totales, n = Inf, width = Inf)

message("")
message("Misma validacion, DESGLOSADA POR ANIO (2008-2018 = con labels de texto en el DTA; 2019-2024 = sin labels, solo verificable por esta identidad aritmetica):")
validacion_con_periodo <- validacion_totales_por_anio %>%
  dplyr::mutate(periodo_labels = ifelse(ANIO <= 2018, "2008-2018 (con labels)", "2019-2024 (sin labels)"))
print(validacion_con_periodo %>% dplyr::select(total_evaluado, ANIO, periodo_labels, n_comparable, pct_coincide), n = Inf, width = Inf)

message("")
message("Resumen: anios con coincidencia perfecta (100%) dentro de cada periodo:")
resumen_periodo <- validacion_con_periodo %>%
  dplyr::group_by(total_evaluado, periodo_labels) %>%
  dplyr::summarise(
    anios_evaluados = dplyr::n(),
    anios_coincidencia_perfecta = sum(pct_coincide == 100),
    .groups = "drop"
  )
print(resumen_periodo, n = Inf, width = Inf)

message("")
message("Verificacion C4R4C9T/C4R4C10T: incluyen o excluyen propietarios?")
message("Descripcion oficial DANE (generica, no distingue): 'Total personal promedio ocupado en el ano - mujeres/hombres'")
print(verificacion_propietarios, n = Inf, width = Inf)

message("")
message("Tabla completa exportada en: ", file.path(data_output_dir, "estabilidad_columnas_c3r_c4r.csv"))
message("Mapa de columnas exportado en: ", file.path(data_output_dir, "mapa_columnas_c3r_c4r.csv"))
message("Validacion de totales exportada en: ", file.path(data_output_dir, "validacion_totales_c4r.csv"))
