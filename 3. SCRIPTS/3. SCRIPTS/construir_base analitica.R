setwd("C:/Users/njaco/OneDrive/Documentos/Mestría en Economía Aplicada/Semestre 3/Big Data y Machine Learning/Repositorios/TESIS_MECA")

getwd()
file.exists("3. SCRIPTS/_utils_proyecto.R")


# ============================================================
# 1. CARGAR MACROBASE EAM 2008-2024
# ============================================================

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tidyr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()

macro_base <- readr::read_rds(paths$macro_base_eam)

# Normalizar nombres
names(macro_base) <- toupper(names(macro_base))

# Verificaciones iniciales
check_required_vars(
  macro_base,
  c("NORDEMP", "NORDEST", "ANIO")
)

dim(macro_base)
sort(unique(macro_base$ANIO))

# ============================================================
# 2. REVISAR IDENTIFICADORES Y UNIDAD DE OBSERVACIÓN
# ============================================================

macro_base <- macro_base |>
  dplyr::mutate(
    ANIO = as.integer(ANIO),
    NORDEMP = as.character(NORDEMP),
    NORDEST = as.character(NORDEST)
  )

resumen_identificadores <- macro_base |>
  dplyr::summarise(
    filas = dplyr::n(),
    empresas = dplyr::n_distinct(NORDEMP, na.rm = TRUE),
    establecimientos = dplyr::n_distinct(NORDEST, na.rm = TRUE),
    faltantes_nordemp = sum(is.na(NORDEMP) | NORDEMP == ""),
    faltantes_nordest = sum(is.na(NORDEST) | NORDEST == "")
  )

duplicados_establecimiento_anio <- macro_base |>
  dplyr::filter(
    !is.na(NORDEST), NORDEST != "",
    !is.na(ANIO)
  ) |>
  dplyr::count(NORDEST, ANIO, name = "n_obs") |>
  dplyr::filter(n_obs > 1)

resumen_identificadores
nrow(duplicados_establecimiento_anio)

# ============================================================
# 3. COBERTURA POR AÑO
# ============================================================

cobertura_por_anio <- macro_base |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    filas = dplyr::n(),
    empresas = dplyr::n_distinct(NORDEMP),
    establecimientos = dplyr::n_distinct(NORDEST),
    .groups = "drop"
  ) |>
  dplyr::arrange(ANIO)

print(cobertura_por_anio, n = Inf)


# ============================================================
# 4. DEFINIR COLUMNAS DE EMPLEO POR CATEGORÍA
# ============================================================

cols_obreros <- c(
  "C4R2C1", "C4R2C2",       # Permanentes
  "C4R3C1", "C4R3C2",       # Temporales directos
  "C4R4C1", "C4R4C2",       # Temporales de agencia
  "C4R6OM", "C4R6OH"        # Aprendices
)

cols_administrativos <- c(
  "C4R2C3", "C4R2C4",
  "C4R3C3", "C4R3C4",
  "C4R4C3", "C4R4C4",
  "C4R6DM", "C4R6DH"
)

cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)

columnas_empleo <- c(
  cols_obreros,
  cols_administrativos,
  cols_prof_tecnico
)

setdiff(columnas_empleo, names(macro_base))

# ============================================================
# 5. CONSTRUIR EMPLEO POR CATEGORÍA
# ============================================================

sumar_componentes <- function(datos, columnas) {
  valores <- datos |>
    dplyr::select(dplyr::all_of(columnas)) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ suppressWarnings(as.numeric(.x))
      )
    )
  
  suma <- rowSums(valores, na.rm = TRUE)
  suma[rowSums(!is.na(valores)) == 0] <- NA_real_
  
  suma
}

base_analitica <- macro_base

base_analitica$total_obreros <-
  sumar_componentes(base_analitica, cols_obreros)

base_analitica$total_prof_tecnico <-
  sumar_componentes(base_analitica, cols_prof_tecnico)

base_analitica$total_administrativos <-
  sumar_componentes(base_analitica, cols_administrativos)

base_analitica <- base_analitica |>
  dplyr::mutate(
    empleo_total_categorias =
      total_obreros +
      total_prof_tecnico +
      total_administrativos
  )

base_analitica |>
  dplyr::select(
    NORDEMP, NORDEST, ANIO,
    total_obreros,
    total_prof_tecnico,
    total_administrativos,
    empleo_total_categorias
  ) |>
  head()

# ============================================================
# 6. VALIDAR EMPLEO CONTRA EL TOTAL OFICIAL DEL DANE
# ============================================================

cols_propietarios <- c(
  "C4R1C1", "C4R1C2",
  "C4R1C3", "C4R1C4",
  "C4R1C1N", "C4R1C2N",
  "C4R2C1E", "C4R2C2E"
)

base_analitica$total_propietarios <-
  sumar_componentes(base_analitica, cols_propietarios)

base_analitica <- base_analitica |>
  dplyr::mutate(
    empleo_total_con_propietarios =
      empleo_total_categorias + total_propietarios,
    
    empleo_total_oficial =
      as.numeric(C4R4C9T) + as.numeric(C4R4C10T)
  )

validacion_empleo <- base_analitica |>
  dplyr::summarise(
    pct_coincide_sin_propietarios = round(
      100 * mean(
        empleo_total_categorias == empleo_total_oficial,
        na.rm = TRUE
      ),
      2
    ),
    pct_coincide_con_propietarios = round(
      100 * mean(
        empleo_total_con_propietarios == empleo_total_oficial,
        na.rm = TRUE
      ),
      2
    )
  )

validacion_empleo


# ============================================================
# 7. CONSTRUIR EXPOSICIÓN DE OBREROS EN 2022
# ============================================================

exposicion_2022 <- base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::transmute(
    NORDEST,
    Exposure2022_obreros = dplyr::if_else(
      !is.na(empleo_total_categorias) &
        empleo_total_categorias > 0,
      total_obreros / empleo_total_categorias,
      NA_real_
    )
  )

base_analitica <- base_analitica |>
  dplyr::left_join(exposicion_2022, by = "NORDEST")

base_analitica |>
  dplyr::summarise(
    establecimientos_con_exposicion =
      dplyr::n_distinct(
        NORDEST[!is.na(Exposure2022_obreros)]
      ),
    exposicion_minima = min(Exposure2022_obreros, na.rm = TRUE),
    exposicion_maxima = max(Exposure2022_obreros, na.rm = TRUE)
  )


# ============================================================
# 8. REVISAR EXPOSICIONES NO CALCULABLES EN 2022
# ============================================================

base_analitica |>
  dplyr::filter(
    ANIO == 2022,
    is.na(Exposure2022_obreros)
  ) |>
  dplyr::count(
    sin_datos = is.na(empleo_total_categorias),
    empleo_cero = empleo_total_categorias == 0,
    name = "establecimientos"
  )


# ============================================================
# 9. DISTRIBUCIÓN DE LA EXPOSICIÓN EN 2022
# ============================================================

resumen_exposicion <- exposicion_2022 |>
  dplyr::filter(!is.na(Exposure2022_obreros)) |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    promedio = mean(Exposure2022_obreros),
    p10 = quantile(Exposure2022_obreros, 0.10),
    p25 = quantile(Exposure2022_obreros, 0.25),
    mediana = median(Exposure2022_obreros),
    p75 = quantile(Exposure2022_obreros, 0.75),
    p90 = quantile(Exposure2022_obreros, 0.90),
    proporcion_en_cero = mean(Exposure2022_obreros == 0),
    proporcion_en_uno = mean(Exposure2022_obreros == 1)
  )

resumen_exposicion

# ============================================================
# 10. EMPLEO POR TIPO DE VINCULACIÓN
# ============================================================

cols_permanentes <- c(
  "C4R2C1", "C4R2C2", "C4R2C3", "C4R2C4",
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E"
)

cols_temporales_directos <- c(
  "C4R3C1", "C4R3C2", "C4R3C3", "C4R3C4",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E"
)

cols_temporales_agencia <- c(
  "C4R4C1", "C4R4C2", "C4R4C3", "C4R4C4",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E"
)

cols_aprendices <- c(
  "C4R6OM", "C4R6OH",
  "C4R6DM", "C4R6DH",
  "C4R6MN", "C4R6HN",
  "C4R6ME", "C4R6HE"
)

base_analitica$empleo_permanente <-
  sumar_componentes(base_analitica, cols_permanentes)

base_analitica$empleo_temporal_directo <-
  sumar_componentes(base_analitica, cols_temporales_directos)

base_analitica$empleo_temporal_agencia <-
  sumar_componentes(base_analitica, cols_temporales_agencia)

base_analitica$empleo_aprendices <-
  sumar_componentes(base_analitica, cols_aprendices)

validacion_vinculacion <- base_analitica |>
  dplyr::mutate(
    suma_vinculaciones =
      empleo_permanente +
      empleo_temporal_directo +
      empleo_temporal_agencia +
      empleo_aprendices
  ) |>
  dplyr::summarise(
    pct_coincide = round(
      100 * mean(
        suma_vinculaciones == empleo_total_categorias,
        na.rm = TRUE
      ),
      2
    )
  )

validacion_vinculacion

# ============================================================
# 12. SALARIO PROMEDIO POR CATEGORÍA
# ============================================================

cols_permanentes_obreros <- c(
  "C4R2C1", "C4R2C2"
)

cols_permanentes_administrativos <- c(
  "C4R2C3", "C4R2C4"
)

cols_permanentes_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N",
  "C4R2C3E", "C4R2C4E"
)

base_analitica$personal_permanente_obrero <-
  sumar_componentes(base_analitica, cols_permanentes_obreros)

base_analitica$personal_permanente_administrativo <-
  sumar_componentes(base_analitica, cols_permanentes_administrativos)

base_analitica$personal_permanente_prof_tecnico <-
  sumar_componentes(base_analitica, cols_permanentes_prof_tecnico)

base_analitica <- base_analitica |>
  dplyr::mutate(
    salario_promedio_obrero =
      dividir_seguro(
        as.numeric(C3R2C1),
        personal_permanente_obrero
      ),
    
    salario_promedio_administrativo =
      dividir_seguro(
        as.numeric(C3R2C2),
        personal_permanente_administrativo
      ),
    
    salario_promedio_prof_tecnico =
      dividir_seguro(
        as.numeric(C3R2PT),
        personal_permanente_prof_tecnico
      )
  )

base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::summarise(
    mediana_obreros =
      median(salario_promedio_obrero, na.rm = TRUE),
    
    mediana_administrativos =
      median(salario_promedio_administrativo, na.rm = TRUE),
    
    mediana_prof_tecnico =
      median(salario_promedio_prof_tecnico, na.rm = TRUE)
  )

# ============================================================
# 13. COBERTURA DE SALARIOS EN 2022
# ============================================================

base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    con_salario_obrero = sum(!is.na(salario_promedio_obrero)),
    con_salario_administrativo =
      sum(!is.na(salario_promedio_administrativo)),
    con_salario_prof_tecnico =
      sum(!is.na(salario_promedio_prof_tecnico))
  )

# ============================================================
# 14. COBERTURA DE RESULTADOS ECONÓMICOS
# ============================================================

variables_economicas <- c(
  "VALAGRI",   # Valor agregado
  "PRODBIND",  # Producción bruta industrial
  "VALORVEN",  # Ventas
  "VALVFAB"    # Ventas de productos fabricados
)

setdiff(variables_economicas, names(base_analitica))

cobertura_variables_economicas <- base_analitica |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(variables_economicas),
      ~ round(100 * mean(!is.na(.x)), 2)
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = -ANIO,
    names_to = "variable",
    values_to = "porcentaje_con_datos"
  )

print(cobertura_variables_economicas, n = Inf)

# ============================================================
# 15. DIAGNÓSTICO DE RESULTADOS ECONÓMICOS
# ============================================================

diagnostico_economico <- base_analitica |>
  dplyr::select(
    ANIO,
    VALAGRI,
    PRODBIND,
    VALORVEN
  ) |>
  tidyr::pivot_longer(
    cols = -ANIO,
    names_to = "variable",
    values_to = "valor"
  ) |>
  dplyr::group_by(ANIO, variable) |>
  dplyr::summarise(
    porcentaje_ceros =
      round(100 * mean(valor == 0, na.rm = TRUE), 2),
    
    porcentaje_negativos =
      round(100 * mean(valor < 0, na.rm = TRUE), 2),
    
    mediana =
      median(valor, na.rm = TRUE),
    
    percentil_99 =
      quantile(valor, 0.99, na.rm = TRUE),
    
    .groups = "drop"
  )

print(diagnostico_economico, n = Inf)

# ============================================================
# 16. CASOS ECONÓMICOS NO POSITIVOS
# ============================================================

casos_no_positivos <- base_analitica |>
  dplyr::select(
    NORDEST,
    ANIO,
    VALAGRI,
    PRODBIND,
    VALORVEN
  ) |>
  tidyr::pivot_longer(
    cols = c(VALAGRI, PRODBIND, VALORVEN),
    names_to = "variable",
    values_to = "valor"
  ) |>
  dplyr::filter(valor <= 0) |>
  dplyr::group_by(ANIO, variable) |>
  dplyr::summarise(
    casos = dplyr::n(),
    valor_minimo = min(valor),
    .groups = "drop"
  )

print(casos_no_positivos, n = Inf)


# ============================================================
# 17. INVENTARIO DE POSIBLES RESULTADOS Y CONTROLES
# ============================================================

ruta_codebook <- file.path(
  dirname(paths$macro_base_eam),
  "macro_base_eam_codebook.csv"
)

diccionario <- readr::read_csv(
  ruta_codebook,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    variable = toupper(variable),
    descripcion = dplyr::coalesce(
      descripcion_final,
      descripcion_diccionario,
      label_dta
    )
  )

# Resultados que ya identificamos
resultados_identificados <- diccionario |>
  dplyr::filter(
    variable %in% c(
      "VALAGRI",
      "PRODBIND",
      "VALORVEN",
      "C3R10C3"
    )
  ) |>
  dplyr::select(
    variable,
    descripcion,
    aparece_en_anios
  )

resultados_identificados

# Buscar posibles controles
posibles_controles <- diccionario |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(
        dplyr::coalesce(descripcion, "")
      ),
      paste(
        c(
          "export",
          "invers",
          "activo",
          "capital",
          "energ",
          "import",
          "materia prima",
          "departamento",
          "ciiu"
        ),
        collapse = "|"
      )
    )
  ) |>
  dplyr::select(
    variable,
    descripcion,
    aparece_en_anios
  ) |>
  dplyr::distinct()

View(posibles_controles)


# ============================================================
# 18. INVENTARIO DE VARIABLES DE RESULTADO DISPONIBLES
# ============================================================

# Busca resultados económicos, empleo, composición y salarios
variables_y_disponibles <- names(base_analitica)[
  stringr::str_detect(
    stringr::str_to_lower(names(base_analitica)),
    paste(
      "valagri",
      "prodbind",
      "valorven",
      "empleo",
      "obrero",
      "prof_tecnico",
      "administrativo",
      "permanente",
      "temporal",
      "agencia",
      "aprendiz",
      "salario_promedio",
      sep = "|"
    )
  )
]

tibble::tibble(variable = variables_y_disponibles)

print(
  tibble::tibble(variable = variables_y_disponibles),
  n = Inf
)

# ============================================================
# 19. COBERTURA DE LAS VARIABLES DE RESULTADO (Y)
# ============================================================

division_segura <- function(numerador, denominador) {
  ifelse(
    is.na(numerador) | is.na(denominador) | denominador <= 0,
    NA_real_,
    numerador / denominador
  )
}

base_analitica <- base_analitica |>
  dplyr::mutate(
    # Productividad provisional: todavía en valores nominales
    productividad_valor_agregado =
      division_segura(VALAGRI, empleo_total_categorias),
    
    produccion_por_trabajador =
      division_segura(PRODBIND, empleo_total_categorias),
    
    # Composición ocupacional
    participacion_obreros =
      division_segura(total_obreros, empleo_total_categorias),
    
    participacion_prof_tecnico =
      division_segura(total_prof_tecnico, empleo_total_categorias),
    
    participacion_administrativos =
      division_segura(total_administrativos, empleo_total_categorias),
    
    # Participación de aprendices, que faltaba construir
    participacion_aprendices =
      division_segura(empleo_aprendices, empleo_total_categorias)
  )

nombres_y <- c(
  productividad_valor_agregado = "Productividad: valor agregado por trabajador",
  produccion_por_trabajador = "Producción industrial por trabajador",
  VALAGRI = "Valor agregado total",
  PRODBIND = "Producción industrial total",
  VALORVEN = "Ventas totales",
  empleo_total_categorias = "Empleo total sin propietarios",
  total_obreros = "Empleo de obreros",
  total_prof_tecnico = "Empleo profesional y técnico",
  total_administrativos = "Empleo administrativo",
  empleo_permanente = "Empleo permanente",
  empleo_temporal_directo = "Empleo temporal directo",
  empleo_temporal_agencia = "Empleo temporal por agencia",
  empleo_aprendices = "Aprendices",
  participacion_obreros = "Participación de obreros",
  participacion_prof_tecnico = "Participación profesional y técnica",
  participacion_administrativos = "Participación administrativa",
  participacion_permanentes = "Participación de permanentes",
  participacion_temporales_directos = "Participación temporal directa",
  participacion_temporales_agencia = "Participación temporal por agencia",
  participacion_aprendices = "Participación de aprendices",
  salario_promedio_obrero = "Salario promedio de obreros",
  salario_promedio_administrativo = "Salario promedio administrativo",
  salario_promedio_prof_tecnico = "Salario promedio profesional y técnico"
)

cobertura_y_anual <- base_analitica |>
  dplyr::select(
    ANIO,
    dplyr::all_of(names(nombres_y))
  ) |>
  tidyr::pivot_longer(
    cols = -ANIO,
    names_to = "variable",
    values_to = "valor"
  ) |>
  dplyr::group_by(ANIO, variable) |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    con_datos = sum(!is.na(valor)),
    porcentaje_con_datos =
      round(100 * mean(!is.na(valor)), 2),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    descripcion = unname(nombres_y[variable])
  )

resumen_cobertura_y <- cobertura_y_anual |>
  dplyr::group_by(variable, descripcion) |>
  dplyr::summarise(
    cobertura_minima = min(porcentaje_con_datos),
    anio_cobertura_minima = ANIO[which.min(porcentaje_con_datos)],
    cobertura_2022 = porcentaje_con_datos[ANIO == 2022],
    anios_con_cobertura_completa =
      sum(porcentaje_con_datos == 100),
    .groups = "drop"
  ) |>
  dplyr::arrange(cobertura_minima)

print(resumen_cobertura_y, n = Inf)

cobertura_y_anual |>
  dplyr::filter(
    variable %in% c(
      "salario_promedio_obrero",
      "salario_promedio_administrativo",
      "salario_promedio_prof_tecnico"
    )
  ) |>
  dplyr::select(
    ANIO,
    descripcion,
    porcentaje_con_datos
  ) |>
  tidyr::pivot_wider(
    names_from = descripcion,
    values_from = porcentaje_con_datos
  ) |>
  print(n = Inf, width = Inf)

# ============================================================
# 20. CAUSA DE LA MENOR COBERTURA SALARIAL
# ============================================================

diagnostico_salarios <- dplyr::bind_rows(
  base_analitica |>
    dplyr::transmute(
      ANIO,
      categoria = "Obreros",
      personal = personal_permanente_obrero,
      costo_salarial = suppressWarnings(as.numeric(C3R2C1))
    ),
  
  base_analitica |>
    dplyr::transmute(
      ANIO,
      categoria = "Administrativos",
      personal = personal_permanente_administrativo,
      costo_salarial = suppressWarnings(as.numeric(C3R2C2))
    ),
  
  base_analitica |>
    dplyr::transmute(
      ANIO,
      categoria = "Profesionales y técnicos",
      personal = personal_permanente_prof_tecnico,
      costo_salarial = suppressWarnings(as.numeric(C3R2PT))
    )
) |>
  dplyr::mutate(
    estado = dplyr::case_when(
      is.na(personal) ~ "Personal no reportado",
      personal == 0 ~ "Sin trabajadores de la categoría",
      personal > 0 & is.na(costo_salarial) ~ "Con trabajadores, salario faltante",
      personal > 0 & costo_salarial <= 0 ~ "Con trabajadores, salario cero o negativo",
      personal > 0 & costo_salarial > 0 ~ "Salario calculable",
      TRUE ~ "Otro caso"
    )
  )

resumen_diagnostico_salarios <- diagnostico_salarios |>
  dplyr::count(ANIO, categoria, estado, name = "establecimientos") |>
  dplyr::group_by(ANIO, categoria) |>
  dplyr::mutate(
    porcentaje = round(
      100 * establecimientos / sum(establecimientos),
      2
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(categoria, ANIO, estado)

print(resumen_diagnostico_salarios, n = Inf, width = Inf)

# ============================================================
# 21. SALARIOS VÁLIDOS Y PRESENCIA DE CADA CATEGORÍA
# ============================================================

base_analitica <- base_analitica |>
  dplyr::mutate(
    # Indica si el establecimiento tiene personal permanente
    tiene_obreros_permanentes =
      personal_permanente_obrero > 0,
    
    tiene_administrativos_permanentes =
      personal_permanente_administrativo > 0,
    
    tiene_prof_tecnico_permanentes =
      personal_permanente_prof_tecnico > 0,
    
    # Salarios válidos: requieren personal y gasto salarial positivo
    salario_obrero_valido = dplyr::if_else(
      personal_permanente_obrero > 0 & C3R2C1 > 0,
      C3R2C1 / personal_permanente_obrero,
      NA_real_
    ),
    
    salario_administrativo_valido = dplyr::if_else(
      personal_permanente_administrativo > 0 & C3R2C2 > 0,
      C3R2C2 / personal_permanente_administrativo,
      NA_real_
    ),
    
    salario_prof_tecnico_valido = dplyr::if_else(
      personal_permanente_prof_tecnico > 0 & C3R2PT > 0,
      C3R2PT / personal_permanente_prof_tecnico,
      NA_real_
    )
  )

# Crear carpeta de deflactores
carpeta_deflactores <- file.path(
  "1. DATOS",
  "7. DEFLACTORES"
)

dir.create(
  carpeta_deflactores,
  recursive = TRUE,
  showWarnings = FALSE
)

# Elegir el Excel desde Descargas
archivo_seleccionado <- file.choose()

# Copiarlo dentro del proyecto
ruta_ipp <- file.path(
  carpeta_deflactores,
  "anex-IPP-historicos-jul2026.xlsx"
)

file.copy(
  from = archivo_seleccionado,
  to = ruta_ipp,
  overwrite = FALSE
)

file.exists(ruta_ipp)

# ============================================================
# 22. CONSTRUIR DEFLACTOR IPP INDUSTRIAL ANUAL
# ============================================================

dir.create(
  file.path("1. DATOS", "7. DEFLACTORES"),
  recursive = TRUE,
  showWarnings = FALSE
)

ruta_ipp <- file.path(
  "1. DATOS",
  "7. DEFLACTORES",
  "anex-IPP-historicos-jul2026.xlsx"
)

if (!file.exists(ruta_ipp)) {
  stop("No se encontró el archivo IPP en: ", ruta_ipp)
}

ipp_mensual <- readxl::read_excel(
  ruta_ipp,
  sheet = "IPP Histórico",
  skip = 5,
  col_names = FALSE
) |>
  dplyr::select(1:6) |>
  rlang::set_names(
    c(
      "ANIO",
      "MES",
      "ipp_produccion_nacional",
      "ipp_agricultura",
      "ipp_mineria",
      "ipp_industria"
    )
  ) |>
  tidyr::fill(ANIO) |>
  dplyr::mutate(
    ANIO = suppressWarnings(as.integer(ANIO)),
    ipp_industria =
      suppressWarnings(as.numeric(ipp_industria))
  ) |>
  dplyr::filter(ANIO %in% 2008:2024)

ipp_industrial_anual <- ipp_mensual |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    meses_disponibles = sum(!is.na(ipp_industria)),
    ipp_industria_promedio = mean(
      ipp_industria,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ipp_industria_2022 = 100 *
      ipp_industria_promedio /
      ipp_industria_promedio[ANIO == 2022]
  )

print(ipp_industrial_anual, n = Inf)

# ============================================================
# 23. CONSTRUIR RESULTADOS ECONÓMICOS REALES
# ============================================================

# Guardar la serie anual ya validada
readr::write_csv(
  ipp_industrial_anual,
  file.path(
    "1. DATOS",
    "7. DEFLACTORES",
    "ipp_industrial_anual_base_2022.csv"
  )
)

# Incorporar el IPP por año
base_analitica <- base_analitica |>
  dplyr::left_join(
    ipp_industrial_anual |>
      dplyr::select(
        ANIO,
        ipp_industria_2022
      ),
    by = "ANIO"
  ) |>
  dplyr::mutate(
    valor_agregado_real =
      VALAGRI / (ipp_industria_2022 / 100),
    
    produccion_industrial_real =
      PRODBIND / (ipp_industria_2022 / 100),
    
    ventas_reales =
      VALORVEN / (ipp_industria_2022 / 100),
    
    productividad_laboral_real =
      division_segura(
        valor_agregado_real,
        empleo_total_categorias
      )
  )

# Verificar que todas las observaciones recibieron deflactor
base_analitica |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    sin_deflactor = sum(is.na(ipp_industria_2022)),
    con_valor_agregado_real =
      sum(!is.na(valor_agregado_real)),
    con_productividad_real =
      sum(!is.na(productividad_laboral_real))
  )

# ============================================================
# 24. TRANSFORMACIONES PARA LOS RESULTADOS ECONÓMICOS
# ============================================================

log_si_positivo <- function(x) {
  resultado <- rep(NA_real_, length(x))
  
  validos <- !is.na(x) &
    is.finite(x) &
    x > 0
  
  resultado[validos] <- log(x[validos])
  resultado
}

base_analitica <- base_analitica |>
  dplyr::mutate(
    # Transformaciones principales
    log_productividad_laboral =
      log_si_positivo(productividad_laboral_real),
    
    log_valor_agregado_real =
      log_si_positivo(valor_agregado_real),
    
    log_produccion_industrial_real =
      log_si_positivo(produccion_industrial_real),
    
    log_ventas_reales =
      log_si_positivo(ventas_reales),
    
    # Empleo: versión logarítmica tradicional
    log_empleo_total =
      log_si_positivo(empleo_total_categorias),
    
    # Transformaciones que conservan ceros y negativos
    asinh_empleo_total =
      asinh(empleo_total_categorias),
    
    asinh_valor_agregado_real =
      asinh(valor_agregado_real)
  )

variables_transformadas <- c(
  "log_productividad_laboral",
  "log_valor_agregado_real",
  "log_produccion_industrial_real",
  "log_ventas_reales",
  "log_empleo_total",
  "asinh_empleo_total",
  "asinh_valor_agregado_real"
)

cobertura_transformaciones <- base_analitica |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(variables_transformadas),
      list(
        con_datos = ~sum(!is.na(.x)),
        porcentaje = ~round(100 * mean(!is.na(.x)), 2)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "medida",
    values_to = "valor"
  )

print(cobertura_transformaciones, n = Inf)


# ============================================================
# 25. TRANSFORMAR LOS MECANISMOS LABORALES
# ============================================================

base_analitica <- base_analitica |>
  dplyr::mutate(
    # Empleo por categoría ocupacional
    asinh_empleo_obreros =
      asinh(total_obreros),
    
    asinh_empleo_prof_tecnico =
      asinh(total_prof_tecnico),
    
    asinh_empleo_administrativos =
      asinh(total_administrativos),
    
    # Empleo por tipo de contratación
    asinh_empleo_permanente =
      asinh(empleo_permanente),
    
    asinh_empleo_temporal_directo =
      asinh(empleo_temporal_directo),
    
    asinh_empleo_temporal_agencia =
      asinh(empleo_temporal_agencia),
    
    asinh_empleo_aprendices =
      asinh(empleo_aprendices),
    
    # Indicadores binarios: 1 tiene la categoría, 0 no la tiene
    tiene_obreros_permanentes =
      as.integer(tiene_obreros_permanentes),
    
    tiene_administrativos_permanentes =
      as.integer(tiene_administrativos_permanentes),
    
    tiene_prof_tecnico_permanentes =
      as.integer(tiene_prof_tecnico_permanentes)
  )

base_analitica |>
  dplyr::summarise(
    dplyr::across(
      dplyr::starts_with("asinh_empleo_"),
      ~round(100 * mean(!is.na(.x)), 2)
    )
  )

# ============================================================
# 26. CONSTRUIR IPC ANUAL CON BASE 2022 = 100
# ============================================================

ipc_mensual <- readr::read_csv2(
  file.path("1. DATOS", "7. DEFLACTORES", "anex-IPC-historicos-BanRep.csv"),
  show_col_types = FALSE
) |>
  dplyr::rename(
    fecha = DateTime,
    ipc = `Índice de Precios al Consumidor (IPC)`
  ) |>
  dplyr::mutate(
    fecha = as.Date(fecha, format = "%Y/%m/%d"),
    ANIO = as.integer(format(fecha, "%Y")),
    ipc = as.numeric(ipc)
  )

ipc_anual <- ipc_mensual |>
  dplyr::filter(dplyr::between(ANIO, 2008, 2024)) |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    meses_disponibles = dplyr::n(),
    ipc_promedio = mean(ipc, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ipc_2022 = 100 * ipc_promedio / ipc_promedio[ANIO == 2022]
  )

print(ipc_anual, n = Inf)

# ============================================================
# 27. CONSTRUIR SALARIOS REALES CON IPC BASE 2022
# ============================================================

base_analitica <- base_analitica |>
  dplyr::left_join(
    ipc_anual |>
      dplyr::select(ANIO, ipc_2022),
    by = "ANIO"
  ) |>
  dplyr::mutate(
    salario_obrero_real_ipc =
      salario_obrero_valido / (ipc_2022 / 100),
    
    salario_administrativo_real_ipc =
      salario_administrativo_valido / (ipc_2022 / 100),
    
    salario_prof_tecnico_real_ipc =
      salario_prof_tecnico_valido / (ipc_2022 / 100),
    
    log_salario_obrero_real_ipc =
      dplyr::if_else(
        salario_obrero_real_ipc > 0,
        log(salario_obrero_real_ipc),
        NA_real_
      ),
    
    log_salario_administrativo_real_ipc =
      dplyr::if_else(
        salario_administrativo_real_ipc > 0,
        log(salario_administrativo_real_ipc),
        NA_real_
      ),
    
    log_salario_prof_tecnico_real_ipc =
      dplyr::if_else(
        salario_prof_tecnico_real_ipc > 0,
        log(salario_prof_tecnico_real_ipc),
        NA_real_
      )
  )

base_analitica |>
  dplyr::summarise(
    obreros_con_salario_real =
      sum(!is.na(salario_obrero_real_ipc)),
    
    administrativos_con_salario_real =
      sum(!is.na(salario_administrativo_real_ipc)),
    
    profesionales_con_salario_real =
      sum(!is.na(salario_prof_tecnico_real_ipc))
  )

# ============================================================
# PASO 28. CONSTRUIR CARACTERÍSTICAS PRECHOQUE (2022)
# ============================================================

# Número de establecimientos que tiene cada empresa en 2022
estructura_empresa_2022 <- macro_base |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::group_by(NORDEMP) |>
  dplyr::summarise(
    numero_establecimientos_empresa_2022 =
      dplyr::n_distinct(NORDEST),
    empresa_multiestablecimiento_2022 =
      as.integer(numero_establecimientos_empresa_2022 > 1),
    .groups = "drop"
  )

# Características del establecimiento antes del choque
caracteristicas_2022 <- base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::left_join(
    estructura_empresa_2022,
    by = "NORDEMP"
  ) |>
  dplyr::transmute(
    NORDEST,
    NORDEMP,
    
    tamano_2022 = empleo_total_categorias,
    asinh_tamano_2022 = asinh(empleo_total_categorias),
    
    activos_fijos_2022 =
      suppressWarnings(as.numeric(ACTIVFI)),
    asinh_activos_fijos_2022 =
      asinh(suppressWarnings(as.numeric(ACTIVFI))),
    
    inversion_bruta_2022 =
      suppressWarnings(as.numeric(INVEBRTA)),
    asinh_inversion_bruta_2022 =
      asinh(suppressWarnings(as.numeric(INVEBRTA))),
    
    usa_insumos_importados_2022 =
      as.integer(suppressWarnings(as.numeric(VALORCX)) > 0),
    
    proporcion_insumos_importados_2022 =
      dplyr::if_else(
        suppressWarnings(as.numeric(VALORCOM)) > 0,
        suppressWarnings(as.numeric(VALORCX)) /
          suppressWarnings(as.numeric(VALORCOM)),
        NA_real_
      ),
    
    numero_establecimientos_empresa_2022,
    empresa_multiestablecimiento_2022
  )

caracteristicas_2022 |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    cobertura_tamano = mean(!is.na(tamano_2022)) * 100,
    cobertura_activos = mean(!is.na(activos_fijos_2022)) * 100,
    cobertura_inversion = mean(!is.na(inversion_bruta_2022)) * 100,
    porcentaje_usa_insumos_importados =
      mean(usa_insumos_importados_2022, na.rm = TRUE) * 100,
    porcentaje_multiestablecimiento =
      mean(empresa_multiestablecimiento_2022, na.rm = TRUE) * 100
  )


# ============================================================
# PASO 29. DIAGNOSTICAR LAS X PRECHOQUE
# ============================================================

diagnostico_x_2022 <- caracteristicas_2022 |>
  dplyr::select(
    tamano_2022,
    activos_fijos_2022,
    inversion_bruta_2022,
    proporcion_insumos_importados_2022,
    numero_establecimientos_empresa_2022
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "valor"
  ) |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    porcentaje_con_datos =
      round(100 * mean(!is.na(valor)), 2),
    porcentaje_ceros =
      round(100 * mean(valor == 0, na.rm = TRUE), 2),
    porcentaje_negativos =
      round(100 * mean(valor < 0, na.rm = TRUE), 2),
    minimo = min(valor, na.rm = TRUE),
    percentil_1 = quantile(valor, 0.01, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    percentil_99 = quantile(valor, 0.99, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  )

print(diagnostico_x_2022, n = Inf, width = Inf)