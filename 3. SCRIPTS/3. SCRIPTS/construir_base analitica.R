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
# 11. PARTICIPACIÓN POR TIPO DE VINCULACIÓN
# ============================================================

dividir_seguro <- function(numerador, denominador) {
  dplyr::if_else(
    !is.na(numerador) &
      !is.na(denominador) &
      denominador > 0,
    numerador / denominador,
    NA_real_
  )
}

base_analitica <- base_analitica |>
  dplyr::mutate(
    participacion_permanentes =
      dividir_seguro(empleo_permanente, empleo_total_categorias),
    
    participacion_temporales_directos =
      dividir_seguro(empleo_temporal_directo, empleo_total_categorias),
    
    participacion_temporales_agencia =
      dividir_seguro(empleo_temporal_agencia, empleo_total_categorias),
    
    participacion_aprendices =
      dividir_seguro(empleo_aprendices, empleo_total_categorias)
  )

base_analitica |>
  dplyr::filter(empleo_total_categorias > 0) |>
  dplyr::summarise(
    suma_promedio_participaciones = mean(
      participacion_permanentes +
        participacion_temporales_directos +
        participacion_temporales_agencia +
        participacion_aprendices,
      na.rm = TRUE
    )
  )


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

# ============================================================
# PASO 30. REVISAR VALORES NEGATIVOS DE INVERSIÓN BRUTA
# ============================================================

diagnostico_inversion_por_anio <- base_analitica |>
  dplyr::mutate(
    inversion_bruta =
      suppressWarnings(as.numeric(INVEBRTA))
  ) |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    
    casos_negativos =
      sum(inversion_bruta < 0, na.rm = TRUE),
    
    porcentaje_negativos =
      round(100 * mean(inversion_bruta < 0, na.rm = TRUE), 2),
    
    casos_en_cero =
      sum(inversion_bruta == 0, na.rm = TRUE),
    
    porcentaje_en_cero =
      round(100 * mean(inversion_bruta == 0, na.rm = TRUE), 2),
    
    valor_minimo =
      min(inversion_bruta, na.rm = TRUE),
    
    mediana =
      median(inversion_bruta, na.rm = TRUE),
    
    .groups = "drop"
  )

print(diagnostico_inversion_por_anio, n = Inf, width = Inf)


# ============================================================
# PASO 31. VALIDAR LA FÓRMULA DE INVERSIÓN BRUTA
# ============================================================

columnas_inversion <- c(
  "INVEBRTA",
  "C7C7R2",   # Compras de activos nuevos
  "C7C7R3",   # Compras de activos usados
  "C7C7R7",   # Mejoras y reformas
  "C7C7R12"   # Valor en libros de activos vendidos
)

faltantes_inversion <- setdiff(
  columnas_inversion,
  names(base_analitica)
)

if (length(faltantes_inversion) > 0) {
  stop(
    "Faltan estas columnas: ",
    paste(faltantes_inversion, collapse = ", ")
  )
}

validacion_inversion <- base_analitica |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(columnas_inversion),
      ~ suppressWarnings(as.numeric(.x))
    ),
    
    inversion_calculada =
      C7C7R2 +
      C7C7R3 +
      C7C7R7 -
      C7C7R12,
    
    diferencia_inversion =
      INVEBRTA - inversion_calculada
  ) |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    casos_comparables =
      sum(!is.na(INVEBRTA) & !is.na(inversion_calculada)),
    
    casos_coincidentes =
      sum(
        abs(diferencia_inversion) < 1,
        na.rm = TRUE
      ),
    
    porcentaje_coincide =
      round(
        100 * casos_coincidentes / casos_comparables,
        2
      ),
    
    diferencia_mediana =
      median(diferencia_inversion, na.rm = TRUE),
    
    .groups = "drop"
  )

print(validacion_inversion, n = Inf, width = Inf)

# ============================================================
# PASO 32. COMPARAR FÓRMULAS ALTERNATIVAS DE INVERSIÓN
# ============================================================

columnas_formula_inversion <- c(
  "INVEBRTA",
  "C7C7R2", "C7C7R3", "C7C7R7", "C7C7R12",
  "C7R10C2",
  "C7R17C7", "C7R18C7"
)

comparacion_formulas_inversion <- base_analitica |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(columnas_formula_inversion),
      ~ suppressWarnings(as.numeric(.x))
    ),
    
    formula_1 =
      C7C7R2 + C7C7R3 + C7C7R7 - C7C7R12,
    
    formula_2 =
      C7R10C2 - C7C7R12,
    
    formula_3 =
      C7C7R2 + C7C7R3 + C7C7R7 -
      C7C7R12 + C7R18C7 - C7R17C7
  ) |>
  dplyr::select(
    INVEBRTA,
    dplyr::starts_with("formula_")
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("formula_"),
    names_to = "formula",
    values_to = "inversion_calculada"
  ) |>
  dplyr::group_by(formula) |>
  dplyr::summarise(
    casos_comparables =
      sum(!is.na(INVEBRTA) & !is.na(inversion_calculada)),
    
    porcentaje_coincide =
      round(
        100 * mean(
          abs(INVEBRTA - inversion_calculada) < 1,
          na.rm = TRUE
        ),
        2
      ),
    
    diferencia_mediana =
      median(
        INVEBRTA - inversion_calculada,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

print(comparacion_formulas_inversion, n = Inf)

# ============================================================
# PASO 33. VERIFICAR LA VARIABLE DE VENTAS AL EXTERIOR
# ============================================================

if (!"PORCVT" %in% names(base_analitica)) {
  stop("PORCVT no está presente en la macrobase.")
}

diagnostico_exportaciones_2022 <- base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::mutate(
    ventas_exterior =
      suppressWarnings(as.numeric(PORCVT))
  ) |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    
    cobertura =
      round(100 * mean(!is.na(ventas_exterior)), 2),
    
    porcentaje_en_cero =
      round(100 * mean(ventas_exterior == 0, na.rm = TRUE), 2),
    
    porcentaje_positivo =
      round(100 * mean(ventas_exterior > 0, na.rm = TRUE), 2),
    
    porcentaje_negativo =
      round(100 * mean(ventas_exterior < 0, na.rm = TRUE), 2),
    
    minimo =
      min(ventas_exterior, na.rm = TRUE),
    
    mediana_positivos =
      median(
        ventas_exterior[ventas_exterior > 0],
        na.rm = TRUE
      ),
    
    percentil_99 =
      quantile(ventas_exterior, 0.99, na.rm = TRUE),
    
    maximo =
      max(ventas_exterior, na.rm = TRUE)
  )

print(diagnostico_exportaciones_2022, width = Inf)

# ============================================================
# PASO 34. CONSTRUIR CONDICIÓN E INTENSIDAD EXPORTADORA
# ============================================================

exportaciones_2022 <- base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::transmute(
    NORDEST,
    
    ventas_exterior_2022 =
      suppressWarnings(as.numeric(PORCVT)),
    
    exportador_2022 =
      as.integer(ventas_exterior_2022 > 0),
    
    participacion_exportaciones_2022 =
      dplyr::if_else(
        suppressWarnings(as.numeric(VALORVEN)) > 0,
        ventas_exterior_2022 /
          suppressWarnings(as.numeric(VALORVEN)),
        NA_real_
      )
  )

caracteristicas_2022 <- caracteristicas_2022 |>
  dplyr::left_join(
    exportaciones_2022,
    by = "NORDEST"
  )

caracteristicas_2022 |>
  dplyr::summarise(
    porcentaje_exportadores =
      round(100 * mean(exportador_2022, na.rm = TRUE), 2),
    
    cobertura_participacion =
      round(
        100 * mean(!is.na(participacion_exportaciones_2022)),
        2
      ),
    
    participacion_mediana_exportadores =
      median(
        participacion_exportaciones_2022[
          exportador_2022 == 1
        ],
        na.rm = TRUE
      ),
    
    participacion_maxima =
      max(
        participacion_exportaciones_2022,
        na.rm = TRUE
      ),
    
    porcentaje_superior_a_uno =
      round(
        100 * mean(
          participacion_exportaciones_2022 > 1,
          na.rm = TRUE
        ),
        2
      )
  )

# ============================================================
# PASO 35. CONSTRUIR SECTOR Y DEPARTAMENTO PRECHOQUE
# ============================================================

columnas_geograficas_sectoriales <- c(
  "NORDEST",
  "ANIO",
  "CIIU4",
  "DPTO"
)

faltantes <- setdiff(
  columnas_geograficas_sectoriales,
  names(base_analitica)
)

if (length(faltantes) > 0) {
  stop(
    "Faltan estas columnas: ",
    paste(faltantes, collapse = ", ")
  )
}

sector_departamento_2022 <- base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::transmute(
    NORDEST,
    
    ciiu4_2022 =
      as.character(CIIU4),
    
    division_ciiu_2022 =
      substr(as.character(CIIU4), 1, 2),
    
    departamento_2022 =
      as.character(DPTO)
  )

caracteristicas_2022 <- caracteristicas_2022 |>
  dplyr::left_join(
    sector_departamento_2022,
    by = "NORDEST"
  )

caracteristicas_2022 |>
  dplyr::summarise(
    cobertura_ciiu4 =
      round(100 * mean(!is.na(ciiu4_2022)), 2),
    
    actividades_ciiu4 =
      dplyr::n_distinct(ciiu4_2022, na.rm = TRUE),
    
    divisiones_industriales =
      dplyr::n_distinct(division_ciiu_2022, na.rm = TRUE),
    
    cobertura_departamento =
      round(100 * mean(!is.na(departamento_2022)), 2),
    
    departamentos =
      dplyr::n_distinct(departamento_2022, na.rm = TRUE)
  )


# ============================================================
# 36. ASIGNAR NOMBRES OFICIALES A LAS DIVISIONES CIIU
# ============================================================
# Fuente: DANE, CIIU Rev. 4 A.C. (2022).
# La división corresponde a los primeros dos dígitos de la clase CIIU4.

nombres_divisiones_ciiu <- tibble::tribble(
  ~division_ciiu_2022, ~nombre_division,
  "10", "Elaboración de productos alimenticios",
  "11", "Elaboración de bebidas",
  "12", "Elaboración de productos de tabaco",
  "13", "Fabricación de productos textiles",
  "14", "Confección de prendas de vestir",
  "15", "Curtido y recurtido de cueros; fabricación de calzado; fabricación de artículos de viaje, maletas, bolsos de mano y artículos similares, y fabricación de artículos de talabartería y guarnicionería; adobo y teñido de pieles",
  "16", "Transformación de la madera y fabricación de productos de madera y de corcho, excepto muebles; fabricación de artículos de cestería y espartería",
  "17", "Fabricación de papel, cartón y productos de papel y cartón",
  "18", "Actividades de impresión y de producción de copias a partir de grabaciones originales",
  "19", "Coquización, fabricación de productos de la refinación del petróleo y actividad de mezcla de combustibles",
  "20", "Fabricación de sustancias y productos químicos",
  "21", "Fabricación de productos farmacéuticos, sustancias químicas medicinales y productos botánicos de uso farmacéutico",
  "22", "Fabricación de productos de caucho y de plástico",
  "23", "Fabricación de otros productos minerales no metálicos",
  "24", "Fabricación de productos metalúrgicos básicos",
  "25", "Fabricación de productos elaborados de metal, excepto maquinaria y equipo",
  "26", "Fabricación de productos informáticos, electrónicos y ópticos",
  "27", "Fabricación de aparatos y equipo eléctrico",
  "28", "Fabricación de maquinaria y equipo n.c.p.",
  "29", "Fabricación de vehículos automotores, remolques y semirremolques",
  "30", "Fabricación de otros tipos de equipo de transporte",
  "31", "Fabricación de muebles, colchones y somieres",
  "32", "Otras industrias manufactureras",
  "33", "Instalación, mantenimiento y reparación especializado de maquinaria y equipo"
)

# Elimina la etiqueta anterior, si ya se había creado, y agrega la oficial.
caracteristicas_2022 <- caracteristicas_2022 |>
  dplyr::select(-dplyr::any_of("nombre_division")) |>
  dplyr::left_join(
    nombres_divisiones_ciiu,
    by = "division_ciiu_2022"
  )

# Distribución y verificación
distribucion_divisiones <- caracteristicas_2022 |>
  dplyr::count(
    division_ciiu_2022,
    nombre_division,
    name = "establecimientos"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      100 * establecimientos / sum(establecimientos),
      2
    )
  ) |>
  dplyr::arrange(dplyr::desc(establecimientos))

print(distribucion_divisiones, n = Inf)

sum(is.na(caracteristicas_2022$nombre_division))

# ============================================================
# 37. REVISAR CÓDIGOS DE DEPARTAMENTO EN 2022
# ============================================================
# Objetivo: identificar los códigos realmente presentes antes
# de relacionarlos con los nombres oficiales del DIVIPOLA.

distribucion_codigos_departamento <- caracteristicas_2022 |>
  dplyr::count(
    departamento_2022,
    name = "establecimientos"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      100 * establecimientos / sum(establecimientos),
      2
    )
  ) |>
  dplyr::arrange(departamento_2022)

print(distribucion_codigos_departamento, n = Inf)

# ============================================================
# 38. ASIGNAR NOMBRES OFICIALES A LOS DEPARTAMENTOS
# ============================================================
# Objetivo: normalizar los códigos DPTO a dos dígitos y
# relacionarlos con los nombres oficiales de DIVIPOLA.

nombres_departamentos <- tibble::tribble(
  ~codigo_departamento, ~nombre_departamento,
  "05", "Antioquia",
  "08", "Atlántico",
  "11", "Bogotá, D. C.",
  "13", "Bolívar",
  "15", "Boyacá",
  "17", "Caldas",
  "19", "Cauca",
  "20", "Cesar",
  "23", "Córdoba",
  "25", "Cundinamarca",
  "41", "Huila",
  "47", "Magdalena",
  "50", "Meta",
  "52", "Nariño",
  "54", "Norte de Santander",
  "63", "Quindío",
  "66", "Risaralda",
  "68", "Santander",
  "70", "Sucre",
  "73", "Tolima",
  "76", "Valle del Cauca",
  "85", "Casanare",
  "99", "Vichada"
)

caracteristicas_2022 <- caracteristicas_2022 |>
  dplyr::mutate(
    codigo_departamento_2022 =
      stringr::str_pad(
        departamento_2022,
        width = 2,
        side = "left",
        pad = "0"
      )
  ) |>
  dplyr::left_join(
    nombres_departamentos,
    by = c(
      "codigo_departamento_2022" = "codigo_departamento"
    )
  )

distribucion_departamentos <- caracteristicas_2022 |>
  dplyr::count(
    codigo_departamento_2022,
    nombre_departamento,
    name = "establecimientos"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      100 * establecimientos / sum(establecimientos),
      2
    )
  ) |>
  dplyr::arrange(dplyr::desc(establecimientos))

print(distribucion_departamentos, n = Inf)

sum(is.na(caracteristicas_2022$nombre_departamento))

# ============================================================
# 39. CONSOLIDAR LAS X PRECHOQUE DE 2022
# ============================================================
# Objetivo: crear una tabla única por establecimiento con la
# exposición principal y las características prechoque.

variables_controles_2022 <- c(
  "NORDEST",
  "tamano_2022",
  "asinh_tamano_2022",
  "activos_fijos_2022",
  "asinh_activos_fijos_2022",
  "inversion_bruta_2022",
  "asinh_inversion_bruta_2022",
  "usa_insumos_importados_2022",
  "proporcion_insumos_importados_2022",
  "numero_establecimientos_empresa_2022",
  "empresa_multiestablecimiento_2022",
  "ventas_exterior_2022",
  "exportador_2022",
  "participacion_exportaciones_2022",
  "ciiu4_2022",
  "division_ciiu_2022",
  "nombre_division",
  "codigo_departamento_2022",
  "nombre_departamento"
)

# Verificar que no estemos usando nombres inexistentes.
faltantes_controles <- setdiff(
  variables_controles_2022,
  names(caracteristicas_2022)
)

if (length(faltantes_controles) > 0) {
  stop(
    "Faltan variables en caracteristicas_2022: ",
    paste(faltantes_controles, collapse = ", ")
  )
}

# Extraer la exposición directamente de la observación de 2022.
exposicion_establecimiento_2022 <- base_analitica |>
  dplyr::filter(ANIO == 2022) |>
  dplyr::select(
    NORDEST,
    Exposure2022_obreros
  )

# No escoger arbitrariamente una fila si hubiera duplicados.
duplicados_exposicion <- exposicion_establecimiento_2022 |>
  dplyr::count(NORDEST) |>
  dplyr::filter(n > 1)

if (nrow(duplicados_exposicion) > 0) {
  stop("Hay establecimientos duplicados en la exposición de 2022.")
}

# Tabla maestra de X prechoque.
x_prechoque_2022 <- exposicion_establecimiento_2022 |>
  dplyr::left_join(
    caracteristicas_2022 |>
      dplyr::select(dplyr::all_of(variables_controles_2022)),
    by = "NORDEST"
  )

# Diagnóstico general.
resumen_x_prechoque <- x_prechoque_2022 |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    establecimientos_unicos = dplyr::n_distinct(NORDEST),
    cobertura_exposicion = round(
      100 * mean(!is.na(Exposure2022_obreros)), 2
    ),
    cobertura_tamano = round(
      100 * mean(!is.na(tamano_2022)), 2
    ),
    cobertura_activos = round(
      100 * mean(!is.na(activos_fijos_2022)), 2
    ),
    cobertura_inversion = round(
      100 * mean(!is.na(inversion_bruta_2022)), 2
    ),
    cobertura_sector = round(
      100 * mean(!is.na(nombre_division)), 2
    ),
    cobertura_departamento = round(
      100 * mean(!is.na(nombre_departamento)), 2
    ),
    cobertura_exportador = round(
      100 * mean(!is.na(exportador_2022)), 2
    ),
    cobertura_insumos_importados = round(
      100 * mean(!is.na(usa_insumos_importados_2022)), 2
    )
  )

print(resumen_x_prechoque)

# ============================================================
# 40. COBERTURA COMPLETA DE LAS X PRECHOQUE
# ============================================================

variables_x_diagnostico <- c(
  "Exposure2022_obreros",
  "tamano_2022",
  "activos_fijos_2022",
  "inversion_bruta_2022",
  "usa_insumos_importados_2022",
  "proporcion_insumos_importados_2022",
  "empresa_multiestablecimiento_2022",
  "exportador_2022",
  "participacion_exportaciones_2022",
  "division_ciiu_2022",
  "codigo_departamento_2022"
)

cobertura_x_prechoque <- x_prechoque_2022 |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(variables_x_diagnostico),
      list(
        con_datos = ~sum(!is.na(.x)),
        cobertura = ~round(100 * mean(!is.na(.x)), 2)
      ),
      .names = "{.col}__{.fn}"
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)__(con_datos|cobertura)"
  ) |>
  dplyr::mutate(
    sin_datos = nrow(x_prechoque_2022) - con_datos
  ) |>
  dplyr::select(
    variable,
    con_datos,
    sin_datos,
    cobertura
  )

print(cobertura_x_prechoque, n = Inf)

# ============================================================
# 41. UNIR LAS X PRECHOQUE AL PANEL ESTABLECIMIENTO-AÑO
# ============================================================

# Verificar nuevamente que la tabla de X sea única.
duplicados_x_2022 <- x_prechoque_2022 |>
  dplyr::count(NORDEST) |>
  dplyr::filter(n > 1)

if (nrow(duplicados_x_2022) > 0) {
  stop("La tabla de X contiene establecimientos duplicados.")
}

# Variables que se agregarán desde la tabla prechoque.
variables_x_2022 <- setdiff(
  names(x_prechoque_2022),
  "NORDEST"
)

filas_antes_union <- nrow(base_analitica)

base_panel_modelo <- base_analitica |>
  dplyr::mutate(
    NORDEST = as.character(NORDEST)
  ) |>
  # Evita crear columnas .x y .y si alguna X ya estaba en la base.
  dplyr::select(
    -dplyr::any_of(variables_x_2022)
  ) |>
  dplyr::left_join(
    x_prechoque_2022 |>
      dplyr::mutate(NORDEST = as.character(NORDEST)),
    by = "NORDEST"
  )

# La unión no debe modificar el número original de filas.
if (nrow(base_panel_modelo) != filas_antes_union) {
  stop("La unión modificó el número de filas del panel.")
}

# Identificar observaciones que tienen todas las X principales.
base_panel_modelo <- base_panel_modelo |>
  dplyr::mutate(
    muestra_x_principal =
      !is.na(Exposure2022_obreros) &
      !is.na(asinh_tamano_2022) &
      !is.na(asinh_activos_fijos_2022) &
      !is.na(asinh_inversion_bruta_2022) &
      !is.na(usa_insumos_importados_2022) &
      !is.na(empresa_multiestablecimiento_2022) &
      !is.na(exportador_2022) &
      !is.na(division_ciiu_2022) &
      !is.na(codigo_departamento_2022)
  )

# Cobertura potencial del modelo por año.
cobertura_panel_modelo <- base_panel_modelo |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    observaciones_totales = dplyr::n(),
    con_exposicion_2022 = sum(
      !is.na(Exposure2022_obreros)
    ),
    muestra_x_principal = sum(
      muestra_x_principal
    ),
    porcentaje_muestra_principal = round(
      100 * mean(muestra_x_principal),
      2
    ),
    .groups = "drop"
  )

print(cobertura_panel_modelo, n = Inf)

# ============================================================
# 41B. CORREGIR PORCENTAJES DE COBERTURA DEL PANEL
# ============================================================

cobertura_panel_modelo <- base_panel_modelo |>
  dplyr::group_by(ANIO) |>
  dplyr::summarise(
    observaciones_totales = dplyr::n(),
    con_exposicion_2022 = sum(
      !is.na(Exposure2022_obreros)
    ),
    n_muestra_x_principal = sum(
      muestra_x_principal
    ),
    porcentaje_muestra_principal = round(
      100 * sum(muestra_x_principal) / dplyr::n(),
      2
    ),
    .groups = "drop"
  )

print(cobertura_panel_modelo, n = Inf)

# ============================================================
# 42. COMPARAR PERMANENCIA EN DISTINTAS VENTANAS
# ============================================================

evaluar_ventana <- function(anio_inicio, anio_fin = 2024) {
  
  anios_ventana <- anio_inicio:anio_fin
  numero_anios <- length(anios_ventana)
  
  presencia <- base_panel_modelo |>
    dplyr::filter(
      muestra_x_principal,
      ANIO %in% anios_ventana
    ) |>
    dplyr::distinct(NORDEST, ANIO) |>
    dplyr::count(
      NORDEST,
      name = "anios_observados"
    )
  
  tibble::tibble(
    ventana = paste0(anio_inicio, "-", anio_fin),
    numero_anios = numero_anios,
    establecimientos_con_alguna_observacion = nrow(presencia),
    establecimientos_todos_los_anios =
      sum(presencia$anios_observados == numero_anios),
    porcentaje_balanceado = round(
      100 * mean(presencia$anios_observados == numero_anios),
      2
    )
  )
}

comparacion_ventanas <- purrr::map_dfr(
  2017:2020,
  evaluar_ventana
)

print(comparacion_ventanas, n = Inf)

# ============================================================
# 43. PERMANENCIA POSTCHOQUE SEGÚN EXPOSICIÓN
# ============================================================

presencia_post <- base_panel_modelo |>
  dplyr::filter(
    ANIO %in% c(2023, 2024)
  ) |>
  dplyr::distinct(NORDEST, ANIO) |>
  dplyr::mutate(presente = 1L) |>
  tidyr::pivot_wider(
    names_from = ANIO,
    values_from = presente,
    names_prefix = "presente_",
    values_fill = 0L
  )

permanencia_por_exposicion <- x_prechoque_2022 |>
  dplyr::filter(
    !is.na(Exposure2022_obreros)
  ) |>
  dplyr::left_join(
    presencia_post,
    by = "NORDEST"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(presente_2023, presente_2024),
      ~tidyr::replace_na(.x, 0L)
    ),
    quintil_exposicion = dplyr::ntile(
      Exposure2022_obreros,
      5
    )
  ) |>
  dplyr::group_by(quintil_exposicion) |>
  dplyr::summarise(
    establecimientos = dplyr::n(),
    exposicion_promedio = round(
      mean(Exposure2022_obreros),
      3
    ),
    porcentaje_presente_2023 = round(
      100 * mean(presente_2023),
      2
    ),
    porcentaje_presente_2024 = round(
      100 * mean(presente_2024),
      2
    ),
    .groups = "drop"
  )

print(permanencia_por_exposicion, n = Inf)