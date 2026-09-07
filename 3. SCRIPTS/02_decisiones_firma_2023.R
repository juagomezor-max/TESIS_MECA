# ============================================================
# TESIS MECA
# DECISIONES DE FIRMA ANTE EL AUMENTO DEL SML DE 2023
#
# Este script parte de las bases procesadas en el pipeline
# anterior. No reconstruye la macrobase EAM.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

# ------------------------------------------------------------
# 1. CARGAR PUNTO DE PARTIDA
# ------------------------------------------------------------

ruta_checkpoint <- file.path(
  "1. DATOS",
  "6. BASES_DERIVADAS",
  "checkpoint_decisiones_firma_2023.rds"
)

if (!file.exists(ruta_checkpoint)) {
  stop(
    "No existe el checkpoint. Ejecute primero la sección 54 ",
    "del script anterior."
  )
}

checkpoint <- readRDS(ruta_checkpoint)

base_analitica <- checkpoint$base_analitica
panel_principal <- checkpoint$panel_principal
panel_balanceado <- checkpoint$panel_balanceado

rm(checkpoint)

# ------------------------------------------------------------
# 1.1. VERIFICACIÓN MÍNIMA
# ------------------------------------------------------------

variables_minimas <- c(
  "NORDEMP",
  "NORDEST",
  "ANIO",
  "Exposure2022_obreros"
)

faltantes <- setdiff(
  variables_minimas,
  names(panel_principal)
)

if (length(faltantes) > 0) {
  stop(
    "Faltan estas variables: ",
    paste(faltantes, collapse = ", ")
  )
}

message("Base cargada correctamente.")

# ============================================================
# 1.1. CONSTRUCCIÓN Y REVISIÓN DEL EMPLEO ASALARIADO
# ============================================================
#
# En esta sección se construye el empleo asalariado total como
# la suma del personal permanente, temporal directo, temporal
# contratado por agencias y aprendices. Luego se identifica
# cuántas observaciones tienen empleo asalariado igual a cero,
# pues en esos casos no puede calcularse el logaritmo.
# ============================================================

panel_principal <- panel_principal |>
  dplyr::mutate(
    empleo_asalariado =
      empleo_permanente +
      empleo_temporal_directo +
      empleo_temporal_agencia +
      empleo_aprendices
  )

# ------------------------------------------------------------
# REVISIÓN DE CEROS Y DATOS FALTANTES
# ------------------------------------------------------------

diagnostico_empleo_asalariado <- panel_principal |>
  dplyr::summarise(
    observaciones_totales = dplyr::n(),
    
    empleo_asalariado_cero = sum(
      empleo_asalariado == 0,
      na.rm = TRUE
    ),
    
    porcentaje_cero = round(
      100 * mean(empleo_asalariado == 0, na.rm = TRUE),
      2
    ),
    
    empleo_asalariado_faltante = sum(
      is.na(empleo_asalariado)
    ),
    
    porcentaje_faltante = round(
      100 * mean(is.na(empleo_asalariado)),
      2
    ),
    
    empleo_asalariado_positivo = sum(
      empleo_asalariado > 0,
      na.rm = TRUE
    )
  )

diagnostico_empleo_asalariado

##Revisando los datos del panel principal solo hay 31 datos con 0 en el nivel de empleo. Entonces 
##no hay problema para volver log la vaina. 

range(panel_principal$ANIO, na.rm = TRUE)
table(panel_principal$ANIO)


# ============================================================
# 2. CONSTRUCCIÓN DE LAS VARIABLES DEPENDIENTES
# ============================================================
#
# Esta sección construye las dos variables dependientes principales:
# el logaritmo del empleo asalariado y la proporción de trabajadores
# temporales dentro del empleo asalariado total.
# ============================================================

panel_principal <- panel_principal |>
  dplyr::mutate(
    
    empleo_asalariado =
      empleo_permanente +
      empleo_temporal_directo +
      empleo_temporal_agencia +
      empleo_aprendices,
    
    empleo_temporal =
      empleo_temporal_directo +
      empleo_temporal_agencia,
    
    Y1_log_empleo_asalariado = dplyr::if_else(
      empleo_asalariado > 0,
      log(empleo_asalariado),
      NA_real_
    ),
    
    Y2_proporcion_temporales = dplyr::if_else(
      empleo_asalariado > 0,
      empleo_temporal / empleo_asalariado,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 2.1. VALIDACIÓN DE LAS VARIABLES DEPENDIENTES
# ------------------------------------------------------------
#
# Esta sección revisa la cobertura y distribución de las dos
# variables dependientes. También confirma que la proporción de
# empleo temporal se encuentre entre cero y uno.
# ------------------------------------------------------------

diagnostico_Y <- panel_principal |>
  dplyr::summarise(
    con_Y1 = sum(!is.na(Y1_log_empleo_asalariado)),
    faltantes_Y1 = sum(is.na(Y1_log_empleo_asalariado)),
    minimo_empleo = min(empleo_asalariado, na.rm = TRUE),
    mediana_empleo = median(empleo_asalariado, na.rm = TRUE),
    maximo_empleo = max(empleo_asalariado, na.rm = TRUE),
    
    con_Y2 = sum(!is.na(Y2_proporcion_temporales)),
    faltantes_Y2 = sum(is.na(Y2_proporcion_temporales)),
    minimo_Y2 = min(Y2_proporcion_temporales, na.rm = TRUE),
    mediana_Y2 = median(Y2_proporcion_temporales, na.rm = TRUE),
    maximo_Y2 = max(Y2_proporcion_temporales, na.rm = TRUE),
    
    fuera_de_rango_Y2 = sum(
      Y2_proporcion_temporales < 0 |
        Y2_proporcion_temporales > 1,
      na.rm = TRUE
    )
  )

diagnostico_Y


# ============================================================
# 3. MUESTRA PRINCIPAL: EMPRESAS DE UN SOLO ESTABLECIMIENTO
# ============================================================
#
# Esta sección selecciona las empresas que tenían un único
# establecimiento en 2022. También define el periodo posterior
# al choque y expresa la exposición en unidades de 10 puntos
# porcentuales para facilitar la interpretación de los modelos.
# ============================================================

variables_necesarias_3 <- c(
  "NORDEMP",
  "NORDEST",
  "ANIO",
  "empresa_multiestablecimiento_2022",
  "Exposure2022_obreros",
  "Y1_log_empleo_asalariado",
  "Y2_proporcion_temporales"
)

faltantes_3 <- setdiff(
  variables_necesarias_3,
  names(panel_principal)
)

if (length(faltantes_3) > 0) {
  stop(
    "Faltan estas variables: ",
    paste(faltantes_3, collapse = ", ")
  )
}

muestra_principal <- panel_principal |>
  dplyr::filter(
    ANIO >= 2017,
    ANIO <= 2024,
    empresa_multiestablecimiento_2022 == 0
  ) |>
  dplyr::mutate(
    post_2023 = as.integer(ANIO >= 2023),
    exposicion_10pp = Exposure2022_obreros * 10
  ) |>
  dplyr::filter(
    !is.na(Y1_log_empleo_asalariado),
    !is.na(Y2_proporcion_temporales),
    !is.na(exposicion_10pp)
  )

# ------------------------------------------------------------
# 3.1. VERIFICACIÓN DE LA MUESTRA PRINCIPAL
# ------------------------------------------------------------

resumen_muestra_principal <- muestra_principal |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    empresas = dplyr::n_distinct(NORDEMP),
    establecimientos = dplyr::n_distinct(NORDEST),
    anio_inicial = min(ANIO),
    anio_final = max(ANIO)
  )

resumen_muestra_principal

table(muestra_principal$ANIO)

#En 2022 ambos son 5.918, porque seleccionamos firmas con un solo establecimiento. 
#Pero entre 2017–2024 aparecen 5.929 códigos de empresa asociados con esos 5.918 
#establecimientos.Esto sugiere que algunos establecimientos cambiaron de empresa 
#o de código NORDEMP durante el periodo.

# ------------------------------------------------------------
# 3.2. REVISIÓN DE CAMBIOS DE EMPRESA POR ESTABLECIMIENTO
# ------------------------------------------------------------
#
# Esta sección identifica establecimientos asociados con más de
# una empresa durante 2017-2024 y muestra cómo cambia su código
# de empresa a través del tiempo.
# ------------------------------------------------------------

establecimientos_con_cambio <- muestra_principal |>
  dplyr::group_by(NORDEST) |>
  dplyr::summarise(
    numero_empresas = dplyr::n_distinct(NORDEMP),
    empresas_asociadas = paste(
      sort(unique(NORDEMP)),
      collapse = ", "
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(numero_empresas > 1)

establecimientos_con_cambio

historial_cambios_empresa <- muestra_principal |>
  dplyr::semi_join(
    establecimientos_con_cambio,
    by = "NORDEST"
  ) |>
  dplyr::select(
    NORDEST,
    NORDEMP,
    ANIO,
    empleo_asalariado,
    Exposure2022_obreros
  ) |>
  dplyr::arrange(NORDEST, ANIO)

historial_cambios_empresa

nrow(establecimientos_con_cambio)

# ------------------------------------------------------------
# 3.3. EXCLUSIÓN DE ESTABLECIMIENTOS QUE CAMBIARON DE EMPRESA
# ------------------------------------------------------------
#
# Esta sección excluye los establecimientos asociados con más de
# una empresa durante el periodo. Así, en la muestra principal,
# cada establecimiento corresponde siempre a la misma empresa.
# ------------------------------------------------------------

muestra_principal_limpia <- muestra_principal |>
  dplyr::anti_join(
    establecimientos_con_cambio |>
      dplyr::select(NORDEST),
    by = "NORDEST"
  )

resumen_muestra_limpia <- muestra_principal_limpia |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    empresas = dplyr::n_distinct(NORDEMP),
    establecimientos = dplyr::n_distinct(NORDEST)
  )

resumen_muestra_limpia


# ============================================================
# 4. MODELOS PRINCIPALES: FIRMAS DE UN SOLO ESTABLECIMIENTO
# ============================================================
#
# Esta sección estima el cambio posterior asociado con una mayor
# exposición al choque salarial. Los modelos incluyen efectos
# fijos de establecimiento y año, y errores agrupados por empresa.
# ============================================================

modelo_4_Y1_empleo <- fixest::feols(
  Y1_log_empleo_asalariado ~
    exposicion_10pp:post_2023 |
    NORDEST + ANIO,
  data = muestra_principal_limpia,
  cluster = ~NORDEMP
)

modelo_4_Y2_temporales <- fixest::feols(
  Y2_proporcion_temporales ~
    exposicion_10pp:post_2023 |
    NORDEST + ANIO,
  data = muestra_principal_limpia,
  cluster = ~NORDEMP
)

# ------------------------------------------------------------
# 4.1. TABLA DE RESULTADOS
# ------------------------------------------------------------

tabla_modelos_4 <- fixest::etable(
  modelo_4_Y1_empleo,
  modelo_4_Y2_temporales,
  headers = c(
    "Log empleo asalariado",
    "Proporción temporal"
  ),
  dict = c(
    "exposicion_10pp:post_2023" =
      "Exposición obrera (10 pp) × Post 2023"
  ),
  fitstat = ~n + r2,
  se.below = TRUE
)

tabla_modelos_4
