# verificar_cifras_clave.R -- Pipeline simplificado (rama `simplificacion`).
#
# Control de calidad de la simplificacion: compara cada cifra clave YA
# REPORTADA en `main` (rama feature/panel-establecimiento, tag
# panel-establecimiento-v1, y feature/estimacion-preliminar para la
# correccion de clustering de Bite) contra la cifra que produce el
# pipeline simplificado (01-05 + opcional_establecimiento.R). Los
# valores esperados se hardcodean aqui con su fuente exacta (archivo y
# script de origen) -- no se leen dinamicamente de main para que este
# control sea independiente de que main cambie despues.
#
# REGLA DE TOLERANCIA (2026-09-01, documentada explicitamente para que el
# criterio de "coincide" sea reproducible, no quede a criterio del lector):
# se usa `all.equal(esperado, producido, tolerance = 0.01)`, que en R es
# una TOLERANCIA RELATIVA del 1% (diferencia absoluta media / escala del
# valor), NO redondeo a un numero fijo de decimales ni tolerancia
# absoluta. En la practica, todas las 33 filas de este control coinciden
# de forma casi exacta (diferencias de <=0.005 en valores tipicos de
# orden 1, atribuibles solo a redondeo de presentacion en los CSV de
# origen) -- el 1% de tolerancia no se uso para "forzar" ninguna
# coincidencia marginal, se documenta el criterio real usado, no uno
# ilustrativo.
#
# VERIFICACION DE PARIDAD Exposure vs. Bite (para que la comparacion sea
# equivalente en controles, no solo en clustering, ver columna
# "especificacion" de cada fila): ambas especificaciones usan los
# MISMOS efectos fijos (NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F) y el MISMO
# cluster (~NORDEMP) -- 04_validaciones.R elige deliberadamente la
# version "CON controles" de Bite (nunca "sin controles") para que sea
# comparable con Exposure (que SIEMPRE incluye esos controles, no tiene
# version "sin controles" en ningun script validado de main). No hacen
# falta filas adicionales para emparejar la comparacion -- ya esta
# emparejada. La UNICA diferencia real entre ambas es la forma funcional
# de la exposicion (Exposure: continua, event-study año-a-año: Bite:
# quintiles, tendencia lineal), no los controles ni el cluster -- se
# documenta explicitamente en la columna "especificacion" para que no
# quede implicita.
#
# Requiere que run_all.R Y opcional_establecimiento.R ya se hayan corrido.
#
# Salida (versionada, 4. RESULTADOS/Validaciones/):
# - CIFRAS_CLAVE.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
out_dir <- paths$resultados_validaciones

leer <- function(archivo) readr::read_csv(file.path(out_dir, archivo), show_col_types = FALSE)

denom_firma <- leer("simplificado_denominadores_firma.csv")
correlacion_bite <- readr::read_csv(file.path(paths$bases_derivadas_exposicion, "correlacion_exposure_bite.csv"), show_col_types = FALSE)
atricion_real <- leer("simplificado_atricion_real_2022_2023_2024.csv") %>% dplyr::distinct(anio_seguimiento, brecha_q5_q1_pp)
atricion_placebo <- leer("simplificado_atricion_placebo_2017_2018_2019.csv") %>% dplyr::distinct(anio_seguimiento, brecha_q5_q1_pp)
pretend_exposure <- leer("simplificado_pretendencias_exposure.csv")
pretend_bite <- leer("simplificado_pretendencias_bite.csv")
denom_est <- leer("simplificado_establecimiento_denominadores.csv")
corr_planta_firma <- leer("simplificado_establecimiento_correlacion_planta_firma.csv")
multiplanta <- leer("simplificado_establecimiento_multiplanta.csv")
umbral <- leer("simplificado_establecimiento_umbral_variacion.csv")
cohorte <- leer("simplificado_establecimiento_cohorte_balanceada.csv")

obtener <- function(tabla, filtro_expr, columna) {
  mascara <- eval(parse(text = filtro_expr), envir = tabla)
  filas <- tabla[mascara, ]
  if (nrow(filas) != 1) stop("Filtro no devolvio exactamente 1 fila (dio ", nrow(filas), "): ", filtro_expr)
  filas[[columna]]
}

formatear <- function(x) {
  x <- as.numeric(x)
  if (!is.na(x) && x == round(x)) return(as.character(as.integer(round(x))))
  as.character(signif(x, 6))
}

fila <- function(nombre, esperado, producido, fuente, especificacion) {
  tibble::tibble(
    nombre = nombre,
    valor_esperado = formatear(esperado),
    valor_producido = formatear(producido),
    coincide = isTRUE(all.equal(as.numeric(esperado), as.numeric(producido), tolerance = 0.01)),
    fuente_del_valor_esperado = fuente,
    especificacion = especificacion
  )
}

# ------------------------------------------------------------------
# Especificaciones documentadas una sola vez, reusadas por fila (evita
# que el texto se desincronice entre filas de la misma medida).
# ------------------------------------------------------------------

ESPEC_NA <- "N/A -- conteo o agregado descriptivo, no es un modelo de regresion"
ESPEC_CORRELACION <- "N/A -- correlacion bivariada (Pearson/Spearman), no es un modelo de regresion"
ESPEC_ATRICION <- "Proporcion binomial por celda quintil-anio; SE analitico sqrt(p(1-p)/n); NO es un modelo de regresion -- no aplica variable de cluster (celdas independientes por construccion)"
ESPEC_EXPOSURE <- "Y ~ i(ANIO_F, exposicion_10pp, ref='2015') | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F; cluster=~NORDEMP; exposicion CONTINUA (10pp), estudio de evento anio-a-anio"
ESPEC_BITE <- "Y ~ anio_lineal + i(quintil_bite, anio_lineal, ref='Q1') | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F; cluster=~NORDEMP; exposicion en QUINTILES, tendencia LINEAL (no anio-a-anio) -- MISMOS FE/controles/cluster que Exposure (verificado), forma funcional distinta"

cifras <- dplyr::bind_rows(
  fila("Establecimientos activos 2022", 6775, obtener(denom_est, "metrica == 'Establecimientos activos 2022'", "valor"),
       "verificacion_consistencia_cruzada_multiplanta_2022.csv (main, script verificar_consistencia_cruzada_multiplanta_2022.R)", ESPEC_NA),
  fila("Establecimientos con Exposure2022_obreros_est valida (2022)", 6761, obtener(denom_est, "metrica == 'Establecimientos con Exposure2022_obreros_est valida en 2022'", "valor"),
       "verificacion_consistencia_cruzada_multiplanta_2022.csv (main)", ESPEC_NA),
  fila("Firmas del panel 2022", 6186, obtener(denom_firma, "metrica == 'Firmas del panel 2022 (todas)'", "valor"),
       "descriptivos_multiplanta_2022_peso_firmas_empleo.csv (main, script descriptivos_estructura_multiplanta_2022_parte2.R)", ESPEC_NA),
  fila("Firmas con Exposure2022_obreros valida (2022)", 6180, obtener(denom_firma, "metrica == 'Firmas con Exposure2022_obreros valida en 2022'", "valor"),
       "descriptivos_comparacion_exposure_establecimiento_vs_firma_2022.csv (main, script comparar_distribucion_exposure_establecimiento_vs_firma.R)", ESPEC_NA),

  fila("Correlacion Exposure-Bite (Pearson)", 0.124, correlacion_bite$correlacion_pearson,
       "tabla_correlacion_bite_exposure_obreros.csv (main, script diagnosticos_validacion_bite_obreros_eam.R)", ESPEC_CORRELACION),
  fila("Correlacion Exposure-Bite (Spearman)", 0.159, correlacion_bite$correlacion_spearman,
       "tabla_correlacion_bite_exposure_obreros.csv (main)", ESPEC_CORRELACION),

  fila("Atricion: brecha Q5-Q1 2023 (real, pp)", 0.57, obtener(atricion_real, "anio_seguimiento == 2023", "brecha_q5_q1_pp"),
       "atricion_a_tasa_por_quintil_con_se.csv (feature/estimacion-preliminar, script extender_diagnostico_atricion_diferencial.R)", ESPEC_ATRICION),
  fila("Atricion: brecha Q5-Q1 2024 (real, pp)", 1.70, obtener(atricion_real, "anio_seguimiento == 2024", "brecha_q5_q1_pp"),
       "atricion_a_tasa_por_quintil_con_se.csv (feature/estimacion-preliminar)", ESPEC_ATRICION),
  fila("Atricion: brecha Q5-Q1 2019 (placebo, pp)", 3.93, obtener(atricion_placebo, "anio_seguimiento == 2019", "brecha_q5_q1_pp"),
       "atricion_c_placebo_2017_2018_2019.csv (feature/estimacion-preliminar)", ESPEC_ATRICION),

  fila("Tendencias paralelas Exposure -- empleo_total F", 0.767, obtener(pretend_exposure, "variable == 'empleo_total'", "f_stat"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main, script validar_tendencias_paralelas_empleo_exposure_grafico.R)", ESPEC_EXPOSURE),
  fila("Tendencias paralelas Exposure -- empleo_total p", 0.5463, obtener(pretend_exposure, "variable == 'empleo_total'", "p_value"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main)", ESPEC_EXPOSURE),
  fila("Tendencias paralelas Exposure -- empleo_permanente F", 1.031, obtener(pretend_exposure, "variable == 'empleo_permanente'", "f_stat"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main)", ESPEC_EXPOSURE),
  fila("Tendencias paralelas Exposure -- empleo_permanente p", 0.3898, obtener(pretend_exposure, "variable == 'empleo_permanente'", "p_value"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main)", ESPEC_EXPOSURE),
  fila("Tendencias paralelas Exposure -- empleo_temporal F", 1.556, obtener(pretend_exposure, "variable == 'empleo_temporal'", "f_stat"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main)", ESPEC_EXPOSURE),
  fila("Tendencias paralelas Exposure -- empleo_temporal p", 0.1832, obtener(pretend_exposure, "variable == 'empleo_temporal'", "p_value"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main)", ESPEC_EXPOSURE),
  fila("Tendencias paralelas Exposure -- participacion_permanente F", 0.308, obtener(pretend_exposure, "variable == 'participacion_permanente'", "f_stat"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main)", ESPEC_EXPOSURE),
  fila("Tendencias paralelas Exposure -- participacion_permanente p", 0.8729, obtener(pretend_exposure, "variable == 'participacion_permanente'", "p_value"),
       "tabla_evento_tendencias_2015_2019_exposure.csv (main)", ESPEC_EXPOSURE),

  fila("Tendencias paralelas Bite (CLUSTERIZADO, con controles) -- empleo_total F", 2.308, obtener(pretend_bite, "variable == 'empleo_total'", "f_stat"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar commit 397e349 -- CLUSTERIZADO, NO main que aun tiene IID)", ESPEC_BITE),
  fila("Tendencias paralelas Bite -- empleo_total p", 0.05573, obtener(pretend_bite, "variable == 'empleo_total'", "p_value"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar, clusterizado)", ESPEC_BITE),
  fila("Tendencias paralelas Bite -- empleo_permanente F", 1.151, obtener(pretend_bite, "variable == 'empleo_permanente'", "f_stat"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar, clusterizado)", ESPEC_BITE),
  fila("Tendencias paralelas Bite -- empleo_permanente p", 0.3305, obtener(pretend_bite, "variable == 'empleo_permanente'", "p_value"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar, clusterizado)", ESPEC_BITE),
  fila("Tendencias paralelas Bite -- empleo_temporal F", 1.749, obtener(pretend_bite, "variable == 'empleo_temporal'", "f_stat"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar, clusterizado)", ESPEC_BITE),
  fila("Tendencias paralelas Bite -- empleo_temporal p", 0.1363, obtener(pretend_bite, "variable == 'empleo_temporal'", "p_value"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar, clusterizado)", ESPEC_BITE),
  fila("Tendencias paralelas Bite -- participacion_permanente F", 7.709, obtener(pretend_bite, "variable == 'participacion_permanente'", "f_stat"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar, clusterizado)", ESPEC_BITE),
  fila("Tendencias paralelas Bite -- participacion_permanente p", 0.000003446, obtener(pretend_bite, "variable == 'participacion_permanente'", "p_value"),
       "validacion_tendencias_paralelas_empleo_bite.csv (feature/estimacion-preliminar, clusterizado)", ESPEC_BITE),

  fila("Establecimientos unicos 2008-2024", 12621, obtener(denom_est, "metrica == 'Establecimientos unicos 2008-2024 (panel completo)'", "valor"),
       "auditar_confiabilidad_nordest.R (main)", ESPEC_NA),
  fila("Firmas multiplanta 2022 (Multi_f)", 262, obtener(multiplanta, "metrica == 'Firmas multiplanta EN 2022 (Multi_f)'", "valor"),
       "descriptivos_multiplanta_2022_reconciliacion.csv (main)", ESPEC_NA),
  fila("Firmas con 2+ establecimientos con exposicion valida", 260, obtener(multiplanta, "grepl('2\\\\+ establecimientos', metrica)", "valor"),
       "descriptivos_multiplanta_2022_reconciliacion.csv (main)", ESPEC_NA),
  fila("% firmas multiplanta que supera brecha >=15pp", 64.6, obtener(umbral, "umbral_pp == 15", "pct_supera_umbral"),
       "descriptivos_multiplanta_2022_umbral_variacion.csv (main)", ESPEC_NA),
  fila("% firmas multiplanta que supera brecha >=20pp", 52.7, obtener(umbral, "umbral_pp == 20", "pct_supera_umbral"),
       "descriptivos_multiplanta_2022_umbral_variacion.csv (main)", ESPEC_NA),
  fila("% del empleo total 2022 en firmas multiplanta", 21.44, obtener(multiplanta, "grepl('empleo', metrica)", "valor"),
       "descriptivos_multiplanta_2022_peso_firmas_empleo.csv (main)", ESPEC_NA),
  fila("Firmas cohorte balanceada (>=2 plantas, 9 anios)", 181, obtener(cohorte, "grepl('mantienen', metrica)", "valor"),
       "descriptivos_panel_efectivo_especificacion_b_persistencia_262.csv (main)", ESPEC_NA),
  fila("Correlacion Exposure2022_obreros_est (planta) vs Exposure2022_obreros (firma)", 0.964, corr_planta_firma$correlacion_pearson,
       "Valor esperado ORIGINAL provino de CONSOLA (mensaje de construir_exposicion_obreros_establecimiento_eam.R en main, NUNCA se guardo como archivo -- laguna de documentacion de esa epoca). El pipeline nuevo SI lo guarda: simplificado_establecimiento_correlacion_planta_firma.csv (opcional_establecimiento.R), respaldo auditable a partir de ahora.", ESPEC_CORRELACION)
)

readr::write_csv(cifras, file.path(out_dir, "CIFRAS_CLAVE.csv"))

script_header("CIFRAS_CLAVE.csv -- control de calidad de la simplificacion")
message("")
message("Regla de tolerancia: all.equal(esperado, producido, tolerance=0.01) -- tolerancia RELATIVA del 1%, no redondeo fijo. Ver cabecera del script.")
message("")
print(cifras %>% dplyr::select(nombre, valor_esperado, valor_producido, coincide), n = Inf, width = Inf)
message("")
n_false <- sum(!cifras$coincide)
if (n_false == 0) {
  message(">>> TODAS las filas coinciden (", nrow(cifras), "/", nrow(cifras), "). <<<")
} else {
  message(">>> ", n_false, " de ", nrow(cifras), " filas NO coinciden. Revisar antes de comitear. <<<")
  print(cifras %>% dplyr::filter(!coincide), n = Inf, width = Inf)
}
message("")
message("Tabla exportada en: ", file.path(out_dir, "CIFRAS_CLAVE.csv"))
