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