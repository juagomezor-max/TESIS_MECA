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


# ============================================================
# 5. TENDENCIAS DE LAS Y POR QUINTILES DE EXPOSICIÓN
# ============================================================
#
# Esta sección divide los establecimientos en cinco grupos según
# su exposición obrera de 2022. Luego muestra la evolución promedio
# de cada resultado, normalizada respecto al nivel de cada quintil
# en 2022.
# ============================================================

quintiles_exposicion <- muestra_principal_limpia |>
  dplyr::distinct(NORDEST, Exposure2022_obreros) |>
  dplyr::mutate(
    quintil_exposicion = dplyr::ntile(
      Exposure2022_obreros,
      5
    ),
    quintil_exposicion = factor(
      quintil_exposicion,
      levels = 1:5,
      labels = paste0("Quintil ", 1:5)
    )
  )

tendencias_quintiles <- muestra_principal_limpia |>
  dplyr::left_join(
    quintiles_exposicion,
    by = c("NORDEST", "Exposure2022_obreros")
  ) |>
  dplyr::group_by(ANIO, quintil_exposicion) |>
  dplyr::summarise(
    promedio_log_empleo =
      mean(Y1_log_empleo_asalariado, na.rm = TRUE),
    promedio_temporales =
      mean(Y2_proporcion_temporales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(quintil_exposicion) |>
  dplyr::mutate(
    cambio_empleo_2022 =
      100 * (
        promedio_log_empleo -
          promedio_log_empleo[ANIO == 2022]
      ),
    cambio_temporales_2022 =
      100 * (
        promedio_temporales -
          promedio_temporales[ANIO == 2022]
      )
  ) |>
  dplyr::ungroup()

# ------------------------------------------------------------
# 5.1. GRÁFICA DEL NIVEL DE EMPLEO
# ------------------------------------------------------------

grafica_quintiles_empleo <- ggplot2::ggplot(
  tendencias_quintiles,
  ggplot2::aes(
    x = ANIO,
    y = cambio_empleo_2022,
    color = quintil_exposicion,
    group = quintil_exposicion
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_vline(
    xintercept = 2022.5,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Cambio del empleo asalariado por quintil de exposición",
    x = "Año",
    y = "Cambio aproximado respecto a 2022 (%)",
    color = "Exposición"
  ) +
  ggplot2::theme_minimal()

grafica_quintiles_empleo

# ------------------------------------------------------------
# 5.2. GRÁFICA DE LA PROPORCIÓN DE TEMPORALES
# ------------------------------------------------------------

grafica_quintiles_temporales <- ggplot2::ggplot(
  tendencias_quintiles,
  ggplot2::aes(
    x = ANIO,
    y = cambio_temporales_2022,
    color = quintil_exposicion,
    group = quintil_exposicion
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_vline(
    xintercept = 2022.5,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Cambio de la proporción temporal por quintil de exposición",
    x = "Año",
    y = "Cambio respecto a 2022 (puntos porcentuales)",
    color = "Exposición"
  ) +
  ggplot2::theme_minimal()

grafica_quintiles_temporales

# ------------------------------------------------------------
# 5.1. EMPLEO ASALARIADO EN NIVELES
# ------------------------------------------------------------
#
# Esta gráfica muestra la evolución del empleo asalariado típico
# de cada quintil de exposición, sin normalizar respecto a 2022.
# ------------------------------------------------------------

grafica_quintiles_empleo <- ggplot2::ggplot(
  tendencias_quintiles,
  ggplot2::aes(
    x = ANIO,
    y = exp(promedio_log_empleo),
    color = quintil_exposicion,
    group = quintil_exposicion
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_vline(
    xintercept = 2022.5,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Empleo asalariado por quintil de exposición",
    x = "Año",
    y = "Empleo asalariado típico",
    color = "Exposición"
  ) +
  ggplot2::theme_minimal()

grafica_quintiles_empleo

# ------------------------------------------------------------
# 5.2. PROPORCIÓN TEMPORAL EN NIVELES
# ------------------------------------------------------------
#
# Esta gráfica muestra la proporción temporal promedio real de
# cada quintil, sin normalizar los valores respecto a 2022.
# ------------------------------------------------------------

grafica_quintiles_temporales <- ggplot2::ggplot(
  tendencias_quintiles,
  ggplot2::aes(
    x = ANIO,
    y = 100 * promedio_temporales,
    color = quintil_exposicion,
    group = quintil_exposicion
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_vline(
    xintercept = 2022.5,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Proporción temporal por quintil de exposición",
    x = "Año",
    y = "Proporción de temporales (%)",
    color = "Exposición"
  ) +
  ggplot2::theme_minimal()

grafica_quintiles_temporales

# ============================================================
# 6. ESTUDIOS DE EVENTO: FIRMAS DE UN SOLO ESTABLECIMIENTO
# ============================================================
#
# Esta sección estima la relación entre exposición y cada Y año
# por año. Se utiliza 2022 como referencia. Los años anteriores
# permiten evaluar tendencias previas y 2023-2024 muestran la
# respuesta posterior al aumento del salario mínimo.
# ============================================================

modelo_6_evento_Y1 <- fixest::feols(
  Y1_log_empleo_asalariado ~
    i(ANIO, exposicion_10pp, ref = 2022) |
    NORDEST + ANIO,
  data = muestra_principal_limpia,
  cluster = ~NORDEMP
)

modelo_6_evento_Y2 <- fixest::feols(
  Y2_proporcion_temporales ~
    i(ANIO, exposicion_10pp, ref = 2022) |
    NORDEST + ANIO,
  data = muestra_principal_limpia,
  cluster = ~NORDEMP
)

# ------------------------------------------------------------
# 6.1. GRÁFICAS
# ------------------------------------------------------------

fixest::iplot(
  modelo_6_evento_Y1,
  ref.line = 0,
  main = "Estudio de evento: empleo asalariado",
  xlab = "Año",
  ylab = "Efecto por 10 pp adicionales de exposición",
  ci_level = 0.95
)

fixest::iplot(
  modelo_6_evento_Y2,
  ref.line = 0,
  main = "Estudio de evento: proporción temporal",
  xlab = "Año",
  ylab = "Efecto por 10 pp adicionales de exposición",
  ci_level = 0.95
)

# ------------------------------------------------------------
# 6.2. PRUEBAS CONJUNTAS DE TENDENCIAS PREVIAS
# ------------------------------------------------------------

prueba_previa_Y1 <- fixest::wald(
  modelo_6_evento_Y1,
  keep = "ANIO::(2017|2018|2019|2020|2021)"
)

prueba_previa_Y2 <- fixest::wald(
  modelo_6_evento_Y2,
  keep = "ANIO::(2017|2018|2019|2020|2021)"
)

prueba_previa_Y1
prueba_previa_Y2


# ============================================================
# 7. ROBUSTEZ: PANEL BALANCEADO 2017-2024
# ============================================================
#
# Esta sección repite la especificación principal usando únicamente
# establecimientos observados durante los ocho años. Esto evita que
# los resultados reflejen cambios en la composición de la muestra.
# ============================================================

muestra_balanceada <- panel_balanceado |>
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
    ),
    
    post_2023 = as.integer(ANIO >= 2023),
    exposicion_10pp = Exposure2022_obreros * 10
  ) |>
  dplyr::filter(
    ANIO >= 2017,
    ANIO <= 2024,
    empresa_multiestablecimiento_2022 == 0,
    !NORDEST %in% establecimientos_con_cambio$NORDEST,
    !is.na(Y1_log_empleo_asalariado),
    !is.na(Y2_proporcion_temporales),
    !is.na(exposicion_10pp)
  )

# Verificación de la muestra

resumen_muestra_balanceada <- muestra_balanceada |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    empresas = dplyr::n_distinct(NORDEMP),
    establecimientos = dplyr::n_distinct(NORDEST),
    observaciones_por_establecimiento =
      observaciones / establecimientos
  )

resumen_muestra_balanceada

# ------------------------------------------------------------
# 7.1. MODELOS PRINCIPALES
# ------------------------------------------------------------

modelo_7_Y1 <- fixest::feols(
  Y1_log_empleo_asalariado ~
    exposicion_10pp:post_2023 |
    NORDEST + ANIO,
  data = muestra_balanceada,
  cluster = ~NORDEMP
)

modelo_7_Y2 <- fixest::feols(
  Y2_proporcion_temporales ~
    exposicion_10pp:post_2023 |
    NORDEST + ANIO,
  data = muestra_balanceada,
  cluster = ~NORDEMP
)

fixest::etable(
  modelo_7_Y1,
  modelo_7_Y2,
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

# ------------------------------------------------------------
# 7.2. ESTUDIOS DE EVENTO
# ------------------------------------------------------------

modelo_7_evento_Y1 <- fixest::feols(
  Y1_log_empleo_asalariado ~
    i(ANIO, exposicion_10pp, ref = 2022) |
    NORDEST + ANIO,
  data = muestra_balanceada,
  cluster = ~NORDEMP
)

modelo_7_evento_Y2 <- fixest::feols(
  Y2_proporcion_temporales ~
    i(ANIO, exposicion_10pp, ref = 2022) |
    NORDEST + ANIO,
  data = muestra_balanceada,
  cluster = ~NORDEMP
)

fixest::iplot(
  modelo_7_evento_Y1,
  ref.line = 0,
  main = "Estudio de evento: empleo — panel balanceado",
  xlab = "Año",
  ylab = "Efecto por 10 pp adicionales de exposición"
)

fixest::iplot(
  modelo_7_evento_Y2,
  ref.line = 0,
  main = "Estudio de evento: temporalidad — panel balanceado",
  xlab = "Año",
  ylab = "Efecto por 10 pp adicionales de exposición"
)

# ------------------------------------------------------------
# 7.3. PRUEBAS DE TENDENCIAS PREVIAS
# ------------------------------------------------------------

prueba_balanceada_Y1 <- fixest::wald(
  modelo_7_evento_Y1,
  keep = "ANIO::(2017|2018|2019|2020|2021)"
)

prueba_balanceada_Y2 <- fixest::wald(
  modelo_7_evento_Y2,
  keep = "ANIO::(2017|2018|2019|2020|2021)"
)

prueba_balanceada_Y1
prueba_balanceada_Y2


# ============================================================
# 8. MODELOS CON EFECTOS FIJOS SECTOR-AÑO Y DEPARTAMENTO-AÑO
# ============================================================
#
# Esta sección reestima los modelos principales para firmas de un
# solo establecimiento. Además de los efectos fijos de establecimiento,
# se controlan choques específicos de cada sector y departamento en
# cada año.
# ============================================================

variables_necesarias_8 <- c(
  "division_ciiu_2022",
  "codigo_departamento_2022"
)

faltantes_8 <- setdiff(
  variables_necesarias_8,
  names(muestra_principal_limpia)
)

if (length(faltantes_8) > 0) {
  stop(
    "Faltan estas variables: ",
    paste(faltantes_8, collapse = ", ")
  )
}

muestra_principal_ajustada <- muestra_principal_limpia |>
  dplyr::filter(
    !is.na(division_ciiu_2022),
    !is.na(codigo_departamento_2022)
  ) |>
  dplyr::mutate(
    division_ciiu_2022 = factor(division_ciiu_2022),
    codigo_departamento_2022 =
      factor(codigo_departamento_2022)
  )

# ------------------------------------------------------------
# 8.1. MODELOS PRINCIPALES PARA Y1 Y Y2
# ------------------------------------------------------------

modelo_8_Y1 <- fixest::feols(
  Y1_log_empleo_asalariado ~
    exposicion_10pp:post_2023 |
    NORDEST +
    division_ciiu_2022^ANIO +
    codigo_departamento_2022^ANIO,
  data = muestra_principal_ajustada,
  cluster = ~NORDEMP
)

modelo_8_Y2 <- fixest::feols(
  Y2_proporcion_temporales ~
    exposicion_10pp:post_2023 |
    NORDEST +
    division_ciiu_2022^ANIO +
    codigo_departamento_2022^ANIO,
  data = muestra_principal_ajustada,
  cluster = ~NORDEMP
)

tabla_modelos_8 <- fixest::etable(
  modelo_8_Y1,
  modelo_8_Y2,
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

tabla_modelos_8

# ------------------------------------------------------------
# 8.2. ESTUDIOS DE EVENTO
# ------------------------------------------------------------

modelo_8_evento_Y1 <- fixest::feols(
  Y1_log_empleo_asalariado ~
    i(ANIO, exposicion_10pp, ref = 2022) |
    NORDEST +
    division_ciiu_2022^ANIO +
    codigo_departamento_2022^ANIO,
  data = muestra_principal_ajustada,
  cluster = ~NORDEMP
)

modelo_8_evento_Y2 <- fixest::feols(
  Y2_proporcion_temporales ~
    i(ANIO, exposicion_10pp, ref = 2022) |
    NORDEST +
    division_ciiu_2022^ANIO +
    codigo_departamento_2022^ANIO,
  data = muestra_principal_ajustada,
  cluster = ~NORDEMP
)

fixest::iplot(
  modelo_8_evento_Y1,
  ref.line = 0,
  main = "Estudio de evento ajustado: empleo asalariado",
  xlab = "Año",
  ylab = "Efecto por 10 pp adicionales de exposición"
)

fixest::iplot(
  modelo_8_evento_Y2,
  ref.line = 0,
  main = "Estudio de evento ajustado: proporción temporal",
  xlab = "Año",
  ylab = "Efecto por 10 pp adicionales de exposición"
)

# ------------------------------------------------------------
# 8.3. PRUEBAS CONJUNTAS DE TENDENCIAS PREVIAS
# ------------------------------------------------------------

prueba_ajustada_Y1 <- fixest::wald(
  modelo_8_evento_Y1,
  keep = "ANIO::(2017|2018|2019|2020|2021)"
)

prueba_ajustada_Y2 <- fixest::wald(
  modelo_8_evento_Y2,
  keep = "ANIO::(2017|2018|2019|2020|2021)"
)

prueba_ajustada_Y1
prueba_ajustada_Y2


# ============================================================
# 9. MUESTRA BALANCEADA COMPLETA
# ============================================================
#
# Esta sección construye una muestra con establecimientos observados
# continuamente entre 2017 y 2024. Incluye firmas simples y múltiples,
# y excluye establecimientos que cambiaron de empresa durante el periodo.
# ============================================================

variables_necesarias_9 <- c(
  "NORDEMP",
  "NORDEST",
  "ANIO",
  "empresa_multiestablecimiento_2022",
  "Exposure2022_obreros",
  "division_ciiu_2022",
  "codigo_departamento_2022",
  "empleo_permanente",
  "empleo_temporal_directo",
  "empleo_temporal_agencia",
  "empleo_aprendices"
)

faltantes_9 <- setdiff(
  variables_necesarias_9,
  names(panel_balanceado)
)

if (length(faltantes_9) > 0) {
  stop(
    "Faltan estas variables: ",
    paste(faltantes_9, collapse = ", ")
  )
}

muestra_balanceada_completa <- panel_balanceado |>
  dplyr::filter(
    ANIO >= 2017,
    ANIO <= 2024
  ) |>
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
    ),
    
    post_2023 = as.integer(ANIO >= 2023),
    exposicion_10pp = Exposure2022_obreros * 10,
    
    division_ciiu_2022 = factor(division_ciiu_2022),
    
    codigo_departamento_2022 =
      factor(codigo_departamento_2022)
  ) |>
  dplyr::filter(
    !is.na(Y1_log_empleo_asalariado),
    !is.na(Y2_proporcion_temporales),
    !is.na(exposicion_10pp),
    !is.na(empresa_multiestablecimiento_2022),
    !is.na(division_ciiu_2022),
    !is.na(codigo_departamento_2022)
  ) |>
  dplyr::group_by(NORDEST) |>
  dplyr::filter(
    dplyr::n_distinct(ANIO) == 8,
    dplyr::n_distinct(NORDEMP) == 1
  ) |>
  dplyr::ungroup()

# ------------------------------------------------------------
# 9.1. VERIFICACIÓN DE LA MUESTRA
# ------------------------------------------------------------

resumen_muestra_balanceada_completa <-
  muestra_balanceada_completa |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    establecimientos = dplyr::n_distinct(NORDEST),
    empresas = dplyr::n_distinct(NORDEMP),
    
    firmas_simples = dplyr::n_distinct(
      NORDEMP[empresa_multiestablecimiento_2022 == 0]
    ),
    
    firmas_multi = dplyr::n_distinct(
      NORDEMP[empresa_multiestablecimiento_2022 == 1]
    ),
    
    observaciones_por_establecimiento =
      observaciones / establecimientos
  )

resumen_muestra_balanceada_completa

table(
  muestra_balanceada_completa$ANIO
)


# ============================================================
# 10. COMPARACIÓN ENTRE FIRMAS SIMPLES Y MULTIESTABLECIMIENTO
# ============================================================
#
# Esta sección estima si la respuesta posterior asociada con la
# exposición difiere entre firmas simples y multiestablecimiento.
# Se utiliza incluye la muestra balanceada completa y efectos fijos
# de establecimiento, sector-año y departamento-año.
# ============================================================

muestra_balanceada_completa <- muestra_balanceada_completa |>
  dplyr::mutate(
    multi_2022 = as.integer(
      empresa_multiestablecimiento_2022 == 1
    )
  )

# ------------------------------------------------------------
# 10.1. MODELO PARA Y1: EMPLEO ASALARIADO
# ------------------------------------------------------------

modelo_10_Y1 <- fixest::feols(
  Y1_log_empleo_asalariado ~
    exposicion_10pp:post_2023 +
    exposicion_10pp:post_2023:multi_2022 +
    post_2023:multi_2022 |
    NORDEST +
    division_ciiu_2022^ANIO +
    codigo_departamento_2022^ANIO,
  data = muestra_balanceada_completa,
  cluster = ~NORDEMP
)

# ------------------------------------------------------------
# 10.2. MODELO PARA Y2: PROPORCIÓN TEMPORAL
# ------------------------------------------------------------

modelo_10_Y2 <- fixest::feols(
  Y2_proporcion_temporales ~
    exposicion_10pp:post_2023 +
    exposicion_10pp:post_2023:multi_2022 +
    post_2023:multi_2022 |
    NORDEST +
    division_ciiu_2022^ANIO +
    codigo_departamento_2022^ANIO,
  data = muestra_balanceada_completa,
  cluster = ~NORDEMP
)

# ------------------------------------------------------------
# 10.3. TABLA DE RESULTADOS
# ------------------------------------------------------------

tabla_modelos_10 <- fixest::etable(
  modelo_10_Y1,
  modelo_10_Y2,
  headers = c(
    "Log empleo asalariado",
    "Proporción temporal"
  ),
  dict = c(
    "exposicion_10pp:post_2023" =
      "β1: Exposición × Post",
    
    "exposicion_10pp:post_2023:multi_2022" =
      "β2: Exposición × Post × Multi",
    
    "post_2023:multi_2022" =
      "β3: Post × Multi"
  ),
  fitstat = ~n + r2,
  se.below = TRUE
)

tabla_modelos_10