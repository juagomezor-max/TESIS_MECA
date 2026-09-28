# ==============================================================================
# 05_resultados_y_mecanismos.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# Corre la especificación del póster de punta a punta:
#
#   1. PRIMER ESLABÓN: ¿subió más el costo laboral de las firmas expuestas?
#      Con las cinco medidas de exposición, no solo con Bite.
#   2. SEGUNDO ESLABÓN: ¿ajustaron el empleo?
#   3. MECANISMOS DE AJUSTE: si el empleo no se mueve, ¿por dónde absorben
#      el choque?
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/exposicion_alternativa_2022.rds
#           1. DATOS/panel_analitico_firma_eam.rds
# Salidas:  4. RESULTADOS/05_resultados_y_mecanismos/
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulo:     5. Resultados (primer eslabón, segundo eslabón, mecanismos,
#               heterogeneidad)
# Por qué Bite: la medida principal usada en las secciones 5, 6 y 7 (donde
#               solo se corre una) es Bite2022_obreros porque es la que gana
#               la celda limpia en 04_decision_medida.R (+0,82%, p=0,023 con
#               la especificación sin tamaño) -- ver ese script para la regla
#               de decisión completa. No se re-justifica aquí, solo se usa.
#
# ESPECIFICACIÓN: por decisión de los autores, efectos fijos de firma, año,
# sector x año y departamento x año (fijados en 2022), SIN tamaño x año
# (igual que 01, 03 y 04).
#
# LIMITACIÓN DEL DISEÑO: la exposición se mide con el salario de 2022, el año
# base del cambio 2022->2023. Ese traslape se estudia en 03 y 04; aquí se
# declara como limitación y los resultados se leen como asociaciones con la
# exposición.
#
# CAPÍTULO 6 -- LA TENDENCIA PREVIA DEL COSTO LABORAL: el hallazgo de que las
# firmas expuestas venían con su costo laboral relativo cayendo 2015-2022
# (sección 3, gráfico G02, lectura B) es material de amenazas, no de
# resultado limpio. Es esperable por construcción, no un defecto de
# implementación: Kaitz se mide con el salario de 2022, así que las firmas de
# exposición alta son por definición las que llegaron a 2022 con el costo más
# bajo. Documentarlo en el capítulo 6 con esa explicación, no ocultarlo ni
# presentarlo como sorpresa.
#
# MECANISMOS Y TENDENCIAS PREVIAS: si al revisar p_previos (sección 5) resulta
# que la mayoría de los mecanismos tiene tendencias diferenciales antes del
# choque, su coeficiente de 2023 no es interpretable como efecto tal como se
# calcula aquí (solo lectura A). 06_mecanismos_por_grupo.R (archivado en
# descartado/) resuelve
# ese problema usando la lectura B para cada mecanismo (el salto contra la
# trayectoria previa de esa variable, no contra cero) -- remitir ahí cuando
# ese sea el caso. La advertencia de que los mecanismos siguen siendo
# exploratorios (circularidad, pruebas múltiples) se mantiene en los dos
# scripts.
# ------------------------------------------------------------------------------


# ==============================================================================
# LA ESPECIFICACIÓN Y LAS TRES LECTURAS
# ==============================================================================
#
# Estudio de evento a nivel firma-año, ventana 2015-2024 sin 2020:
#
#   Y_ft = Σ_s β_s · 1{año = s} · Kaitz_f + γ_f + λ_t + controles×año + ε_ft
#
# con 2022 como año de referencia, efectos fijos de firma y año, controles de
# sector (CIIU4) y departamento interactuados con año, fijados en 2022, y
# errores agrupados por firma.
#
# De ahí salen cuatro lecturas del primer eslabón, que NO son lo mismo:
#
#   A. Coeficiente de 2023. Cuánto subió el costo laboral de las firmas
#      expuestas en el año del choque, frente a 2022. Es la cifra del póster.
#
#   B. Salto de 2023 ajustado por la tendencia previa. Las firmas expuestas
#      venían con su costo laboral relativo cayendo alrededor de 0,8 puntos por
#      año entre 2016 y 2019. El contrafactual correcto para 2023 no es "cero"
#      sino "seguir cayendo". Por eso B = A + (pendiente anual previa), y sale
#      MAYOR que A, no menor.
#
#   C. DiD simple: promedio de 2023-2024 contra promedio de 2015-2022. Mezcla el
#      nivel post con toda la trayectoria previa, así que cuando hay tendencia
#      descendente sale negativo aunque A y B sean positivos. No es la lectura
#      principal.
#
#   D. 2023 frente a 2021. Compara el año del choque con el año previo a la
#      referencia. Es una lectura de apoyo: el período incluye también el
#      aumento del mínimo de 2022.
#
# POR QUÉ IMPORTA LA DISTINCIÓN: en una versión anterior de esta revisión se
# comparó la lectura A del póster contra un DiD en panel (que es la lectura C) y
# se concluyó erróneamente que había una contradicción. No la hay: son
# estimandos distintos del mismo event study.
#
# LA TENDENCIA PREVIA, DECLARADA DE FRENTE. El event study muestra que las
# firmas de Kaitz alto venían convergiendo hacia abajo en costo laboral relativo
# durante todo 2015-2022. Eso es esperable por construcción: Kaitz se mide con
# el salario de 2022, así que las firmas de exposición alta son por definición
# las que llegaron a 2022 con el costo más bajo, y su trayectoria previa se ve
# descendente. Es una propiedad conocida de los diseños con bite salarial, no un
# defecto de implementación. La lectura B existe para corregirla, y la
# sensibilidad al año en que se mide la exposición se documenta en 04.
# ==============================================================================


# ==============================================================================
# 0. PREPARACIÓN
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "05_resultados_y_mecanismos")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 25) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 4, carpeta = CARPETA) {
  tabla_word <- flextable(tabla)
  tabla_word <- colformat_double(tabla_word, digits = decimales)
  tabla_word <- set_caption(tabla_word, caption = titulo_tabla)
  tabla_word <- autofit(tabla_word)
  save_as_docx(tabla_word, path = file.path(carpeta, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(carpeta, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tabla_word
  cat("Tabla guardada:", nombre_archivo, "\n")
}

graficos <- list()
guardar_grafico <- function(grafico, nombre_archivo, carpeta = CARPETA, ancho = 9, alto = 5.5) {
  print(grafico)
  ggsave(file.path(carpeta, "figuras", paste0(nombre_archivo, ".png")),
         grafico, width = ancho, height = alto, dpi = 200, bg = "white")
  graficos[[nombre_archivo]] <<- grafico
  cat("Gráfico guardado:", nombre_archivo, "\n")
}

estrellas <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                                                      ifelse(p < 0.10, "*", ""))))
}

tema_tesis <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey35"),
        plot.caption = element_text(color = "grey45", hjust = 0),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"


# ==============================================================================
# 1. DATOS Y VARIABLES
# ==============================================================================
titulo("1. DATOS Y CONSTRUCCIÓN DE VARIABLES")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

alternativas <- read_rds(file.path("1. DATOS", "exposicion_alternativa_2022.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP)) %>%
  select(NORDEMP, any_of(c("golpe_c", "golpe_a", "golpe_costo")))

viejas <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO))) %>%
  filter(ANIO == 2022) %>%
  select(NORDEMP, any_of(c("Bite2022_obreros", "Exposure2022_obreros")))

base <- panel %>%
  left_join(alternativas, by = "NORDEMP") %>%
  left_join(viejas, by = "NORDEMP")

# --- Outcomes -------------------------------------------------------------------
# empleo_total sigue la definición del póster: permanentes + temporal directo +
# agencias + aprendices, sin propietarios (que no tienen remuneración fija).
base <- base %>%
  mutate(
    empleo_total = empleo_total_sin_propietarios,
    
    # Costo laboral por trabajador: numerador y denominador sobre el personal
    # ocupado. C3R10 incluye el apoyo a aprendices (R4CSAP), y los aprendices
    # están en el denominador, así que la inclusión es consistente.
    costo_trabajador = ifelse(empleo_total > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo_total, NA_real_),
    
    # --- Composición contractual ---
    empleo_permanente = obreros_permanentes + administrativos_permanentes +
      profesional_tecnico_permanentes,
    empleo_temporal_directo = obreros_temporal_directo + administrativos_temporal_directo +
      profesional_tecnico_temporal_directo,
    empleo_temporal_agencias = obreros_temporal_agencias + administrativos_temporal_agencias +
      profesional_tecnico_temporal_agencias,
    empleo_aprendices = obreros_aprendices + administrativos_aprendices +
      profesional_tecnico_aprendices,
    empleo_temporal = empleo_temporal_directo + empleo_temporal_agencias,
    
    participacion_permanente = ifelse(empleo_total > 0, empleo_permanente / empleo_total, NA_real_),
    participacion_temporal = ifelse(empleo_total > 0, empleo_temporal / empleo_total, NA_real_),
    participacion_agencias = ifelse(empleo_total > 0, empleo_temporal_agencias / empleo_total, NA_real_),
    
    # --- Composición ocupacional ---
    # ADVERTENCIA DE CIRCULARIDAD: la participación de obreros está
    # mecánicamente emparentada con la construcción de Exposure (que ES esa
    # participación) y, de forma más indirecta, con Kaitz. Cualquier resultado
    # sobre estas variables debe discutir ese punto explícitamente.
    obreros_total = obreros_total_ocupado,
    participacion_obreros = ifelse(empleo_total > 0, obreros_total / empleo_total, NA_real_),
    
    # --- Salarios por categoría (compresión salarial) ---
    w_obrero = ifelse(obreros_permanentes > 0,
                      sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA_real_),
    w_admin = ifelse(administrativos_permanentes > 0,
                     sueldos_permanentes_administrativos_c3r2c2 / administrativos_permanentes, NA_real_),
    brecha_obrero_admin = ifelse(!is.na(w_obrero) & !is.na(w_admin) & w_admin > 0,
                                 w_obrero / w_admin, NA_real_),
    
    # --- Tercerización ---
    outsourcing = outsourcing_total_c3r41c3,
    honorarios = honorarios_servicios_tecnicos_total_c3r15c3,
    servicios_terceros = productos_servicios_terceros_total_c3r14c3,
    pago_agencias = pago_agencias_temporales_total_c3r8c3,
    
    # --- Producción y capital ---
    ventas = valor_ventas_valorven,
    produccion = produccion_bruta_prodbr2,
    valor_agregado = valor_agregado_valagri,
    inversion = inversion_bruta_invebrta,
    maquinaria_nueva = compra_maquinaria_nueva_c7c3r2,
    productividad = ifelse(empleo_total > 0 & valor_agregado_valagri > 0,
                           valor_agregado_valagri / empleo_total, NA_real_),
    
    # --- Mecanismos adicionales (revisión del diccionario, 2026-09) ---
    # Aprendices: su apoyo de sostenimiento está atado por ley a una fracción
    # del mínimo, así que son un margen de sustitución más barato.
    participacion_aprendices = ifelse(empleo_total > 0, empleo_aprendices / empleo_total, NA_real_),
    # Costo por temporal directo: otros trabajadores y otros campos del
    # formulario que los del Kaitz (no comparten el dato de 2022).
    costo_temporal = ifelse(empleo_temporal_directo > 0 &
                              sueldos_prest_temporal_directo_total_c3r4c3 > 0,
                            sueldos_prest_temporal_directo_total_c3r4c3 / empleo_temporal_directo,
                            NA_real_),
    # Intensidad de capital física: energía eléctrica por trabajador
    energia_trabajador = ifelse(empleo_total > 0 & energia_electrica_kw_eelec > 0,
                                energia_electrica_kw_eelec / empleo_total, NA_real_),
    # Intensidad laboral: trabajadores por unidad de ventas
    empleo_por_ventas = ifelse(empleo_total > 0 & valor_ventas_valorven > 0,
                               empleo_total / valor_ventas_valorven, NA_real_),
    # Rentabilidad (Draca, Machin y Van Reenen, 2011): margen y peso del costo
    # laboral sobre la producción. Se winsorizan más abajo.
    margen = ifelse(produccion_bruta_prodbr2 > 0,
                    (valor_agregado_valagri - costos_totales_personal_total_c3r10c3) /
                      produccion_bruta_prodbr2, NA_real_),
    peso_costo = ifelse(produccion_bruta_prodbr2 > 0,
                        costos_totales_personal_total_c3r10c3 / produccion_bruta_prodbr2, NA_real_),
    # Composición por sexo: el mínimo suele pesar más en las mujeres. La EAM
    # solo tiene conteos por sexo, no salarios.
    participacion_mujeres_obreras = ifelse(obreros_total_ocupado > 0,
                                           mh_c4r5c1 / obreros_total_ocupado, NA_real_),
    
    # --- Identificadores y factores ---
    ANIO_F = factor(ANIO),
    post = as.integer(ANIO >= 2023)
  )

# CORRECCIÓN DE BUG (encontrada en la auditoría adversarial, 2026-09-26):
# sector_2022/depto_2022/tamano_2022 se construían con factor(CIIU4) etc.
# directamente sobre el panel completo (sin filtrar a 2022 primero), así que
# el valor no era el de 2022 sino el del año propio de cada fila. Entre 7% y
# 39% de las firmas-año tenían al menos una de las tres clasificaciones
# distinta a la de 2022 -- el tamaño contemporáneo es justo el "bad control"
# que esta variable se creó para evitar. Se corrige fijando de verdad la
# clasificación en 2022, con un join, igual que ya hacen
# 01_descriptivos_y_contexto.R y 06_tratamiento_continuo.R.
clasificacion_2022 <- panel %>%
  filter(ANIO == 2022) %>%
  transmute(
    NORDEMP,
    sector_2022 = factor(CIIU4),
    depto_2022  = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

n_antes_join <- n_distinct(base$NORDEMP)
base <- base %>%
  select(-any_of(c("sector_2022", "depto_2022", "tamano_2022"))) %>%
  left_join(clasificacion_2022, by = "NORDEMP")

cat("\nCORRECCIÓN DE CONTROLES FIJOS EN 2022:\n")
cat("  Firmas en el panel:", n_antes_join, "\n")
cat("  Firmas con clasificación de 2022 (no quedan NA en los controles):",
    nrow(clasificacion_2022), "\n")
cat("  Firmas SIN clasificación de 2022 (año NORDEMP no observado en 2022):",
    n_antes_join - nrow(clasificacion_2022), "\n")

cat("\n  Filas firma-año que quedan con NA en sector_2022 (se caen de cualquier\n",
    "  regresión con esos controles), por año:\n")
base %>% group_by(ANIO) %>%
  summarise(filas = n(), filas_sin_control_2022 = sum(is.na(sector_2022)),
            pct = round(100*mean(is.na(sector_2022)), 2), .groups = "drop") %>%
  print(n = 20)

# Logaritmos. Los ceros y negativos pasan a NA antes de tomar log, así que las
# variables con muchos ceros (inversión, outsourcing) pierden observaciones. Eso
# cambia el estimando: pasa a ser el efecto sobre el margen intensivo,
# condicional a que la firma ya hacía esa actividad. Por eso para esas variables
# estimamos también un indicador de si la hace o no (margen extensivo).
log_seguro <- function(x) log(ifelse(!is.na(x) & x > 0, x, NA_real_))

base <- base %>%
  mutate(
    log_costo = log_seguro(costo_trabajador),
    log_empleo = log_seguro(empleo_total),
    log_permanente = log_seguro(empleo_permanente),
    log_temporal = log_seguro(empleo_temporal),
    log_temporal_directo = log_seguro(empleo_temporal_directo),
    log_agencias = log_seguro(empleo_temporal_agencias),
    log_obreros = log_seguro(obreros_total),
    log_ventas = log_seguro(ventas),
    log_produccion = log_seguro(produccion),
    log_valor_agregado = log_seguro(valor_agregado),
    log_productividad = log_seguro(productividad),
    log_inversion = log_seguro(inversion),
    log_outsourcing = log_seguro(outsourcing),
    log_honorarios = log_seguro(honorarios),
    log_pago_agencias = log_seguro(pago_agencias),
    log_brecha_obrero_admin = log_seguro(brecha_obrero_admin),
    log_aprendices = log_seguro(empleo_aprendices),
    log_w_obrero = log_seguro(w_obrero),
    log_w_admin = log_seguro(w_admin),
    log_costo_temporal = log_seguro(costo_temporal),
    log_servicios_terceros = log_seguro(servicios_terceros),
    log_maquinaria_nueva = log_seguro(maquinaria_nueva),
    log_energia_trabajador = log_seguro(energia_trabajador),
    log_empleo_por_ventas = log_seguro(empleo_por_ventas),
    # Indicadores de margen extensivo
    hace_outsourcing = as.integer(!is.na(outsourcing) & outsourcing > 0),
    usa_agencias = as.integer(!is.na(empleo_temporal_agencias) & empleo_temporal_agencias > 0),
    invierte = as.integer(!is.na(inversion) & inversion > 0),
    usa_aprendices = as.integer(!is.na(empleo_aprendices) & empleo_aprendices > 0),
    compra_maquinaria = as.integer(!is.na(maquinaria_nueva) & maquinaria_nueva > 0)
  )

# --- Medidas de exposición, winsorizadas y estandarizadas ------------------------
# Winsorización 1/99, igual que en el póster. Conserva las firmas extremas con
# un valor recortado en vez de eliminarlas.
winsorizar <- function(x) {
  lim <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, lim[1]), lim[2])
}

# Margen y peso del costo: pueden tomar valores extremos cuando la producción
# es muy pequeña; se winsorizan al 1% y 99%.
base <- base %>% mutate(margen = winsorizar(margen), peso_costo = winsorizar(peso_costo))

MEDIDAS <- c("Bite2022_obreros", "Exposure2022_obreros", "golpe_c", "golpe_a", "golpe_costo")
ETIQUETAS <- c(Bite2022_obreros = "Bite (Kaitz de obreros)",
               Exposure2022_obreros = "Exposure (proporción de obreros)",
               golpe_c = "Golpe C (nivel salarial, 3 categorías)",
               golpe_a = "Golpe A (armónica ponderada)",
               golpe_costo = "Golpe costo (costo laboral total)")

for (m in MEDIDAS) {
  if (m %in% names(base)) {
    base[[paste0(m, "_de")]] <- {
      w <- winsorizar(base[[m]])
      w / sd(w, na.rm = TRUE)
    }
  }
}

# Ventana de estimación: 2015-2024 sin 2020 (pandemia)
datos <- base %>% filter(ANIO %in% c(2015:2019, 2021:2024))

cat("Firmas-año en la ventana de estimación:", nrow(datos), "\n")
cat("Firmas distintas:", n_distinct(datos$NORDEMP), "\n")
cat("Años:", paste(sort(unique(datos$ANIO)), collapse = ", "), "\n")

cobertura <- tibble(medida = MEDIDAS) %>%
  rowwise() %>%
  mutate(firmas_con_medida = if (medida %in% names(base))
    n_distinct(base$NORDEMP[!is.na(base[[medida]])]) else NA_integer_) %>%
  ungroup()
ver(cobertura)


# ==============================================================================
# 2. FUNCIONES DE ESTIMACIÓN
# ==============================================================================
titulo("2. ESPECIFICACIÓN")

# Por decisión de los autores no se controla por tamaño x año (igual que 01, 03 y 04)
EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F"
cat("Efectos fijos:", EFECTOS, "\n")
cat("Errores agrupados por firma (NORDEMP).\n")

# estudio_evento(): coeficiente año por año frente a 2022, más la prueba
# conjunta de los años previos al choque.
# previos: años de la prueba conjunta de tendencias previas.
estudio_evento <- function(outcome, tratamiento, base_datos = datos,
                           previos = "2015|2016|2017|2018|2019|2021",
                           efectos = EFECTOS) {
  v <- paste0(tratamiento, "_de")
  if (!v %in% names(base_datos) || !outcome %in% names(base_datos)) return(NULL)
  
  formula <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", v, ", ref = '2022') | ", efectos))
  modelo <- tryCatch(feols(formula, data = base_datos, cluster = ~NORDEMP),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  
  coefs <- as.data.frame(coeftable(modelo))
  tabla <- tibble(
    anio = as.integer(gsub(paste0("ANIO_F::|:", v), "", rownames(coefs))),
    coeficiente = coefs[["Estimate"]],
    error_estandar = coefs[["Std. Error"]],
    p_valor = coefs[["Pr(>|t|)"]]
  ) %>%
    bind_rows(tibble(anio = 2022L, coeficiente = 0, error_estandar = 0, p_valor = NA_real_)) %>%
    arrange(anio)
  
  p_previos <- tryCatch(
    wald(modelo, keep = paste0("ANIO_F::(", previos, "):"), print = FALSE)$p,
    error = function(e) NA_real_)
  
  list(tabla = tabla, p_previos = p_previos, modelo = modelo, variable = v)
}

# contraste(): combina coeficientes del event study con pesos y calcula su error
# estándar por la fórmula de la varianza de una combinación lineal.
contraste <- function(evento, pesos, etiqueta) {
  if (is.null(evento)) return(NULL)
  nombres <- paste0("ANIO_F::", names(pesos), ":", evento$variable)
  disponibles <- nombres %in% names(coef(evento$modelo))
  if (!all(disponibles)) return(NULL)
  
  b <- coef(evento$modelo)[nombres]
  V <- vcov(evento$modelo)[nombres, nombres]
  estimado <- sum(pesos * b)
  error <- sqrt(as.numeric(t(pesos) %*% V %*% pesos))
  p <- 2 * pnorm(-abs(estimado / error))
  
  tibble(lectura = etiqueta, coeficiente = estimado, error_estandar = error,
         p_valor = p, significancia = estrellas(p),
         efecto_pct = 100 * estimado,
         ic95_inf_pct = 100 * (estimado - 1.96 * error),
         ic95_sup_pct = 100 * (estimado + 1.96 * error))
}

# tres_lecturas(): A, B, C y D para un outcome y una medida (se conserva el
# nombre por continuidad con versiones anteriores).
tres_lecturas <- function(outcome, tratamiento, base_datos = datos, efectos = EFECTOS) {
  evento <- estudio_evento(outcome, tratamiento, base_datos, efectos = efectos)
  if (is.null(evento)) return(NULL)
  
  # A: coeficiente de 2023
  a <- contraste(evento, c(`2023` = 1), "A. Cambio 2022 -> 2023")
  
  # B: salto de 2023 ajustado por la pendiente anual previa.
  # (coef2016 - coef2019)/3 es el cambio anual promedio entre 2016 y 2019. Como
  # los coeficientes vienen cayendo, esa cantidad es POSITIVA y al sumarla el
  # salto de 2023 queda ajustado hacia arriba: el contrafactual no es "cero"
  # sino "seguir cayendo".
  b <- contraste(evento, c(`2023` = 1, `2016` = 1/3, `2019` = -1/3),
                 "B. Salto 2023 ajustado por tendencia previa")
  
  # C: DiD simple, promedio post menos promedio pre
  v <- evento$variable
  modelo_c <- tryCatch(
    feols(as.formula(paste0(outcome, " ~ post:", v, " | ", efectos)),
          data = base_datos, cluster = ~NORDEMP),
    error = function(e) NULL)
  c_fila <- if (!is.null(modelo_c)) {
    f <- coeftable(modelo_c)[paste0("post:", v), ]
    tibble(lectura = "C. DiD simple (promedio post - pre)",
           coeficiente = f[["Estimate"]], error_estandar = f[["Std. Error"]],
           p_valor = f[["Pr(>|t|)"]], significancia = estrellas(f[["Pr(>|t|)"]]),
           efecto_pct = 100 * f[["Estimate"]],
           ic95_inf_pct = 100 * (f[["Estimate"]] - 1.96 * f[["Std. Error"]]),
           ic95_sup_pct = 100 * (f[["Estimate"]] + 1.96 * f[["Std. Error"]]))
  } else NULL
  
  # D: 2023 frente a 2021 (lectura de apoyo; ver encabezado)
  d <- contraste(evento, c(`2023` = 1, `2021` = -1), "D. 2023 frente a 2021")
  
  bind_rows(a, b, c_fila, d) %>%
    mutate(outcome = outcome, medida = ETIQUETAS[[tratamiento]],
           p_previos = evento$p_previos,
           observaciones = nobs(evento$modelo))
}

grafico_evento <- function(evento, titulo_g, eje_y, nota = NULL) {
  if (is.null(evento)) return(NULL)
  ggplot(evento$tabla, aes(x = anio, y = 100 * coeficiente)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                      ymax = 100 * (coeficiente + 1.96 * error_estandar)),
                  width = 0.2, color = COLOR_BAJA) +
    geom_point(size = 2.5, color = ifelse(evento$tabla$anio >= 2023, COLOR_ALTA, COLOR_BAJA)) +
    scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
    labs(title = titulo_g,
         subtitle = paste0("Efecto de una DE más de exposición, frente a 2022. ",
                           "Prueba conjunta de años previos: p = ",
                           round(evento$p_previos, 3)),
         x = NULL, y = eje_y,
         caption = nota %||% NOTA) +
    tema_tesis
}

`%||%` <- function(a, b) if (is.null(a)) b else a

NOTA <- paste("Controles: firma, año, sector x año y departamento x año (fijados en 2022).",
              "\nIntervalos al 95%, errores agrupados por firma. 2020 excluido.")


# ==============================================================================
# 3. PRIMER ESLABÓN CON LAS CINCO MEDIDAS
# ==============================================================================
titulo("3. PRIMER ESLABÓN: COSTO LABORAL POR TRABAJADOR")

primer_eslabon <- bind_rows(lapply(MEDIDAS, function(m) tres_lecturas("log_costo", m))) %>%
  select(medida, lectura, efecto_pct, ic95_inf_pct, ic95_sup_pct, p_valor,
         significancia, p_previos, observaciones)

ver(primer_eslabon, filas = 20)
guardar_tabla(primer_eslabon, "T01_primer_eslabon_cinco_medidas",
              "Tabla 1. Primer eslabón: efecto sobre el costo laboral por trabajador, tres lecturas y cinco medidas",
              decimales = 3)

cat("\nCÓMO LEER:\n",
    " A = coeficiente de 2023 (la cifra del póster).\n",
    " B = A ajustado por la pendiente previa. Como la tendencia es descendente,\n",
    "     B sale MAYOR que A: el contrafactual no es cero sino seguir cayendo.\n",
    " C = DiD simple. Mezcla el nivel post con toda la trayectoria previa, así\n",
    "     que con tendencia descendente puede salir negativo sin contradecir\n",
    "     a A ni a B. No es la lectura principal.\n",
    " D = 2023 frente a 2021. Lectura de apoyo: incluye el aumento de 2022.\n",
    " p_previos = prueba conjunta de que los años anteriores a 2023 son cero.\n")

# Gráfico comparativo de la lectura A entre medidas
grafico_medidas <- primer_eslabon %>%
  filter(lectura == "A. Cambio 2022 -> 2023") %>%
  ggplot(aes(x = reorder(medida, efecto_pct), y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct),
                  color = COLOR_BAJA, size = 0.8) +
  coord_flip() +
  labs(title = "Primer eslabón según la medida de exposición",
       subtitle = "Coeficiente de 2023 frente a 2022, por DE de exposición",
       x = NULL, y = "Efecto (%)", caption = NOTA) +
  tema_tesis
guardar_grafico(grafico_medidas, "G01_primer_eslabon_medidas")

# Event study del costo laboral con cada medida, en un solo gráfico
eventos_costo <- bind_rows(lapply(MEDIDAS, function(m) {
  e <- estudio_evento("log_costo", m)
  if (is.null(e)) return(NULL)
  mutate(e$tabla, medida = ETIQUETAS[[m]])
}))

grafico_eventos <- ggplot(eventos_costo, aes(x = anio, y = 100 * coeficiente, color = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  labs(title = "Costo laboral por trabajador: trayectoria de las firmas expuestas",
       subtitle = "Diferencia frente a 2022, por DE de exposición, con cada medida",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = paste("La pendiente descendente previa es esperable: la exposición se mide con el salario de 2022,",
                       "\nasí que las firmas expuestas son por construcción las que llegan a 2022 con el costo más bajo.")) +
  guides(color = guide_legend(nrow = 2)) +
  tema_tesis
guardar_grafico(grafico_eventos, "G02_evento_costo_todas_medidas")


# ==============================================================================
# 4. SEGUNDO ESLABÓN: EMPLEO
# ==============================================================================
titulo("4. SEGUNDO ESLABÓN: EMPLEO TOTAL")

# Dos escalas: número de trabajadores y logaritmo. El log se lee como cambio
# porcentual; el nivel dice cuántos trabajadores. Reportamos las dos porque
# pueden dar conclusiones distintas, y si es así hay que decirlo, no elegir la
# más favorable.

# OJO CON LA ESCALA: en las filas de "Trabajadores" el coeficiente ya está en
# número de trabajadores, así que NO se multiplica por 100. La columna se sigue
# llamando efecto_pct por compatibilidad, pero en esas filas son trabajadores.
corregir_escala <- function(tabla) {
  tabla %>%
    mutate(across(c(efecto_pct, ic95_inf_pct, ic95_sup_pct),
                  ~ ifelse(escala == "Trabajadores", .x / 100, .x)))
}

segundo_eslabon <- bind_rows(
  bind_rows(lapply(MEDIDAS, function(m) tres_lecturas("empleo_total", m))) %>%
    mutate(escala = "Trabajadores"),
  bind_rows(lapply(MEDIDAS, function(m) tres_lecturas("log_empleo", m))) %>%
    mutate(escala = "Log (cambio %)")
) %>%
  select(medida, escala, lectura, efecto_pct, ic95_inf_pct, ic95_sup_pct,
         p_valor, significancia, p_previos, observaciones) %>%
  corregir_escala()

ver(segundo_eslabon, filas = 40)
guardar_tabla(segundo_eslabon, "T02_segundo_eslabon_empleo",
              "Tabla 2. Segundo eslabón: efecto sobre el empleo total (trabajadores o % según la escala)",
              decimales = 3)

# Event study del empleo con la medida principal
evento_empleo <- estudio_evento("log_empleo", "Bite2022_obreros")
if (!is.null(evento_empleo)) {
  guardar_grafico(grafico_evento(evento_empleo,
                                 "Empleo total de las firmas más expuestas (log)",
                                 "Diferencia en log del empleo"),
                  "G03_evento_empleo")
  
  ver(evento_empleo$tabla)
  guardar_tabla(evento_empleo$tabla %>%
                  mutate(efecto_pct = 100 * coeficiente,
                         significancia = estrellas(p_valor)),
                "T03_evento_empleo",
                "Tabla 3. Empleo total año por año frente a 2022 (log)", decimales = 4)
}

cat("\nADVERTENCIA SOBRE EL AÑO BASE. Si el coeficiente de 2023 es parecido al\n",
    "de 2019, no hay quiebre en 2023 sino retorno al nivel previo después de\n",
    "2021-2022. En ese caso el 'efecto' depende de que 2022 sea el año base, y\n",
    "hay que decirlo. La sección 7 lo verifica con base 2019.\n")

# --- Elasticidad implícita ------------------------------------------------------
# Efecto sobre empleo dividido por efecto sobre costo laboral. Solo tiene
# interpretación limpia porque las dos piezas vienen del MISMO event study y la
# MISMA lectura.
elasticidad <- primer_eslabon %>%
  filter(lectura == "A. Cambio 2022 -> 2023") %>%
  select(medida, primer_eslabon_pct = efecto_pct) %>%
  left_join(
    segundo_eslabon %>%
      filter(lectura == "A. Cambio 2022 -> 2023", escala == "Log (cambio %)") %>%
      select(medida, empleo_pct = efecto_pct, empleo_ic_inf = ic95_inf_pct,
             empleo_ic_sup = ic95_sup_pct),
    by = "medida") %>%
  mutate(
    elasticidad = empleo_pct / primer_eslabon_pct,
    elasticidad_ic_inf = empleo_ic_inf / primer_eslabon_pct,
    elasticidad_ic_sup = empleo_ic_sup / primer_eslabon_pct
  )

ver(elasticidad)
guardar_tabla(elasticidad, "T04_elasticidad_implicita",
              "Tabla 4. Elasticidad implícita empleo-costo laboral", decimales = 3)

cat("\nADVERTENCIA: este intervalo trata el primer eslabón como conocido con\n",
    "certeza. No lo es. El intervalo correcto sale por método delta o bootstrap\n",
    "y es MÁS ancho. No citarlo como intervalo de confianza formal.\n")


# ==============================================================================
# 5. MECANISMOS DE AJUSTE
# ==============================================================================
titulo("5. MECANISMOS DE AJUSTE")

# Si el empleo total no se mueve, la pregunta es por dónde absorben las firmas
# el aumento del costo laboral. Hirsch, Kaufman y Zelenska (2015) documentan
# que las firmas usan múltiples márgenes: precios, compresión salarial,
# rotación, estándares de desempeño y ajustes operativos internos. Aquí miramos
# los que la EAM permite observar.
#
# CÓMO LEER ESTA SECCIÓN. Son resultados EXPLORATORIOS, no causales, por tres
# razones que deben quedar escritas en la tesis:
#
#   1. Muchas de estas variables están afectadas simultáneamente por factores
#      macroeconómicos y sectoriales distintos al salario mínimo.
#   2. Al estimar ~20 outcomes, algunos saldrán significativos por azar.
#      Reportamos p-valores ajustados por Benjamini-Hochberg al lado de los
#      crudos, y la lectura debe basarse en los ajustados.
#   3. La participación de obreros es mecánicamente cercana a la definición de
#      la exposición. Cualquier resultado ahí debe discutir esa circularidad.
#
# Y una distinción que importa: para variables con muchos ceros (inversión,
# outsourcing, agencias), el log solo mide el MARGEN INTENSIVO, condicional a
# que la firma ya hacía esa actividad. Por eso estimamos también el indicador de
# si la hace o no, que es el MARGEN EXTENSIVO. Las dos preguntas son distintas.

MECANISMOS <- tribble(
  ~outcome,                     ~etiqueta,                                  ~grupo,
  "log_permanente",             "Empleo permanente (log)",                  "1. Composición contractual",
  "log_temporal",               "Empleo temporal (log)",                    "1. Composición contractual",
  "log_temporal_directo",       "Temporal directo (log)",                   "1. Composición contractual",
  "log_agencias",               "Personal de agencias (log)",               "1. Composición contractual",
  "participacion_permanente",   "% permanentes",                            "1. Composición contractual",
  "participacion_temporal",     "% temporales",                             "1. Composición contractual",
  "participacion_agencias",     "% de agencias",                            "1. Composición contractual",
  "usa_agencias",               "Usa agencias (indicador)",                 "1. Composición contractual",
  
  "log_aprendices",             "Aprendices (log)",                         "1. Composición contractual",
  "participacion_aprendices",   "% aprendices",                             "1. Composición contractual",
  "usa_aprendices",             "Usa aprendices (indicador)",               "1. Composición contractual",
  
  "log_obreros",                "Obreros (log)",                            "2. Composición ocupacional",
  "participacion_obreros",      "% obreros (OJO: circular)",                "2. Composición ocupacional",
  "participacion_mujeres_obreras", "% mujeres entre obreros",               "2. Composición ocupacional",
  
  "log_brecha_obrero_admin",    "Salario obrero / salario admin (log)",     "3. Salarios y compresión",
  "log_w_obrero",               "Salario obrero (log; OJO: es la base del Kaitz)", "3. Salarios y compresión",
  "log_w_admin",                "Salario administrativo (log)",             "3. Salarios y compresión",
  "log_costo_temporal",         "Costo por temporal directo (log)",         "3. Salarios y compresión",
  
  "log_outsourcing",            "Outsourcing (log)",                        "4. Tercerización",
  "hace_outsourcing",           "Hace outsourcing (indicador)",             "4. Tercerización",
  "log_honorarios",             "Honorarios y servicios técnicos (log)",    "4. Tercerización",
  "log_pago_agencias",          "Pago a agencias temporales (log)",         "4. Tercerización",
  "log_servicios_terceros",     "Producción encargada a terceros (log)",    "4. Tercerización",
  
  "log_inversion",              "Inversión bruta (log)",                    "5. Capital",
  "invierte",                   "Invierte (indicador)",                     "5. Capital",
  "log_maquinaria_nueva",       "Compra de maquinaria nueva (log)",         "5. Capital",
  "compra_maquinaria",          "Compra maquinaria nueva (indicador)",      "5. Capital",
  "log_energia_trabajador",     "Energía eléctrica por trabajador (log)",   "5. Capital",
  
  "log_ventas",                 "Ventas (log)",                             "6. Producción",
  "log_produccion",             "Producción bruta (log)",                   "6. Producción",
  "log_valor_agregado",         "Valor agregado (log)",                     "6. Producción",
  "log_productividad",          "Productividad (VA / trabajador, log)",     "6. Producción",
  "log_empleo_por_ventas",      "Empleo por unidad de ventas (log)",        "6. Producción",
  
  "margen",                     "Margen: (VA - costo laboral) / producción", "7. Rentabilidad",
  "peso_costo",                 "Costo laboral / producción",               "7. Rentabilidad"
)

cat("Estimando", nrow(MECANISMOS), "mecanismos con la medida principal...\n")

# Los mecanismos se reportan en la especificación ESTÁNDAR (exposición 2022),
# comparable con la literatura. El traslape entre la exposición y el año base,
# documentado en las secciones 3 y 4, también afecta estas estimaciones: se
# declara como limitación del diseño y los resultados se leen como
# asociaciones con la exposición, no como efectos causales del aumento.
mecanismos <- bind_rows(lapply(seq_len(nrow(MECANISMOS)), function(i) {
  fila <- MECANISMOS[i, ]
  if (!fila$outcome %in% names(datos)) {
    cat("  AVISO: no existe la variable", fila$outcome, "\n")
    return(NULL)
  }
  r <- tres_lecturas(fila$outcome, "Bite2022_obreros")
  if (is.null(r)) {
    cat("  AVISO: no se pudo estimar", fila$outcome, "\n")
    return(NULL)
  }
  r %>%
    filter(lectura == "A. Cambio 2022 -> 2023") %>%
    mutate(mecanismo = fila$etiqueta, grupo = fila$grupo, variable = fila$outcome)
}))

# Ajuste por pruebas múltiples. Con ~35 outcomes, algunos salen significativos
# por azar: con 35 pruebas al 5% se esperan casi dos falsos positivos aunque no
# haya ningún efecto real.
mecanismos <- mecanismos %>%
  mutate(
    p_ajustado_bh = p.adjust(p_valor, method = "BH"),
    significancia_cruda = estrellas(p_valor),
    significancia_ajustada = estrellas(p_ajustado_bh)
  ) %>%
  select(grupo, mecanismo, variable, efecto_pct, ic95_inf_pct, ic95_sup_pct,
         p_valor, significancia_cruda, p_ajustado_bh, significancia_ajustada,
         p_previos, observaciones) %>%
  arrange(grupo, desc(abs(efecto_pct)))

ver(mecanismos, filas = 40)
guardar_tabla(mecanismos %>% select(-variable), "T05_mecanismos_ajuste",
              "Tabla 5. Mecanismos de ajuste: efecto en 2023 por DE de exposición (exploratorio)",
              decimales = 3)

cat("\nCÓMO LEER LA TABLA DE MECANISMOS:\n",
    " - Usar 'significancia_ajustada', no la cruda. Con 35 pruebas al 5% se\n",
    "   esperan casi dos falsos positivos aunque no haya ningún efecto real.\n",
    " - Revisar 'p_previos' de cada fila: si es bajo, ese mecanismo ya tenía\n",
    "   tendencias diferenciales antes del choque. La tabla 5c compara 2023\n",
    "   con el promedio de los años previos para esos casos.\n",
    " - Los indicadores y las participaciones (%, margen, costo/producción)\n",
    "   están en puntos porcentuales, no en cambio porcentual.\n",
    " - LIMITACIÓN: la exposición se mide con el salario de 2022, el año base\n",
    "   del cambio. Las variables que comparten ese dato (salario y número de\n",
    "   obreros, y las que se dividen por el empleo) están más expuestas a ese\n",
    "   traslape. Se reportan como asociaciones.\n")

grafico_mecanismos <- mecanismos %>%
  filter(!is.na(efecto_pct)) %>%
  mutate(significativo = ifelse(significancia_ajustada != "",
                                "Significativo (ajustado)", "No significativo (ajustado)")) %>%
  ggplot(aes(x = reorder(mecanismo, efecto_pct), y = efecto_pct, color = significativo)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct)) +
  coord_flip() +
  facet_wrap(~ grupo, scales = "free", ncol = 2) +
  scale_color_manual(values = c(`Significativo (ajustado)` = COLOR_ALTA,
                                `No significativo (ajustado)` = "grey60")) +
  labs(title = "¿Por dónde ajustan las firmas más expuestas?",
       subtitle = "Efecto en 2023 por DE de exposición. Resultados exploratorios",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = paste("p-valores ajustados por Benjamini-Hochberg.",
                       "\nIndicadores, participaciones y rentabilidad en puntos porcentuales; el resto en cambio porcentual.")) +
  tema_tesis +
  theme(axis.text.y = element_text(size = 7))
guardar_grafico(grafico_mecanismos, "G04_mecanismos", ancho = 11, alto = 14)

# Event studies de los mecanismos significativos tras el ajuste
vars_sig <- mecanismos %>% filter(significancia_ajustada != "") %>%
  select(outcome = variable, etiqueta = mecanismo)

if (nrow(vars_sig) > 0) {
  cat("\nMecanismos significativos tras el ajuste:",
      paste(vars_sig$etiqueta, collapse = ", "), "\n")
  
  eventos_sig <- lapply(seq_len(nrow(vars_sig)), function(i)
    estudio_evento(vars_sig$outcome[i], "Bite2022_obreros"))
  
  eventos_mecanismos <- bind_rows(lapply(seq_len(nrow(vars_sig)), function(i) {
    if (is.null(eventos_sig[[i]])) return(NULL)
    mutate(eventos_sig[[i]]$tabla, mecanismo = vars_sig$etiqueta[i])
  }))
  
  if (nrow(eventos_mecanismos) > 0) {
    grafico_ev_mec <- ggplot(eventos_mecanismos,
                             aes(x = anio, y = 100 * coeficiente)) +
      geom_hline(yintercept = 0, color = "grey60") +
      geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
      geom_errorbar(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                        ymax = 100 * (coeficiente + 1.96 * error_estandar)),
                    width = 0.2, color = COLOR_BAJA) +
      geom_point(size = 1.8, color = COLOR_BAJA) +
      facet_wrap(~ mecanismo, scales = "free_y") +
      scale_x_continuous(breaks = c(2015, 2019, 2021, 2023)) +
      labs(title = "Trayectoria de los mecanismos con efecto significativo",
           subtitle = "Diferencia frente a 2022, por DE de exposición",
           x = NULL, y = "Efecto (%)",
           caption = "Si el mecanismo ya venía moviéndose antes de 2023, el coeficiente post no es interpretable como efecto.") +
      tema_tesis
    guardar_grafico(grafico_ev_mec, "G05_evento_mecanismos", ancho = 12, alto = 10)
  }
  
  # 2023 y 2024 frente al PROMEDIO de los años previos (2015-2019, 2021 y 2022,
  # este último con valor 0 por ser la referencia), no solo frente a 2022.
  # Sirve para los mecanismos con tendencias previas: dice si 2023 se aparta
  # del nivel habitual y no solo del año base.
  w <- -1/7
  robustez_mecanismos <- bind_rows(lapply(seq_len(nrow(vars_sig)), function(i) {
    e <- eventos_sig[[i]]
    if (is.null(e)) return(NULL)
    bind_rows(
      contraste(e, c(`2023` = 1), "2023 vs 2022"),
      contraste(e, c(`2023` = 1, `2021` = -1), "2023 vs 2021"),
      contraste(e, c(`2023` = 1, `2015` = w, `2016` = w, `2017` = w, `2018` = w,
                     `2019` = w, `2021` = w), "2023 vs promedio previo"),
      contraste(e, c(`2024` = 1, `2015` = w, `2016` = w, `2017` = w, `2018` = w,
                     `2019` = w, `2021` = w), "2024 vs promedio previo")
    ) %>% mutate(mecanismo = vars_sig$etiqueta[i], p_previos = e$p_previos)
  })) %>%
    relocate(mecanismo)
  
  ver(robustez_mecanismos, filas = 80)
  guardar_tabla(robustez_mecanismos, "T05c_mecanismos_frente_a_promedio_previo",
                "Tabla 5c. Mecanismos significativos: 2023 y 2024 frente a 2022, a 2021 y al promedio previo",
                decimales = 4)
} else {
  cat("\nNingún mecanismo resulta significativo tras el ajuste por pruebas\n",
      "múltiples: no hay un margen de ajuste detectable en la EAM.\n")
}


# ==============================================================================
# 6. HETEROGENEIDAD POR TAMAÑO
# ==============================================================================
titulo("6. HETEROGENEIDAD POR TAMAÑO")

# Las firmas pequeñas tienen menos capacidad de absorber un aumento de costos:
# menos margen, menos acceso a crédito, menos posibilidad de sustituir capital
# por trabajo. La especificación principal ya no controla por tamaño x año,
# así que dentro de cada grupo se usan los mismos efectos fijos.

EFECTOS_SIN_TAMANO <- EFECTOS

a_estandar <- function(oc, sub) {
  r <- tres_lecturas(oc, "Bite2022_obreros", base_datos = sub)
  if (is.null(r)) return(NULL)
  filter(r, lectura == "A. Cambio 2022 -> 2023")
}

heterogeneidad <- bind_rows(lapply(c("Pequena", "Mediana", "Grande"), function(t) {
  sub <- filter(datos, tamano_2022 == t)
  if (nrow(sub) < 500) return(NULL)
  bind_rows(lapply(c("log_costo", "log_empleo"), function(oc) {
    bind_rows(a_estandar(oc, sub)) %>%
      mutate(tamano = t,
             outcome = ifelse(oc == "log_costo", "Costo laboral", "Empleo total"))
  }))
})) %>%
  select(tamano, outcome, efecto_pct, ic95_inf_pct, ic95_sup_pct,
         p_valor, significancia, p_previos, observaciones) %>%
  arrange(factor(tamano, levels = c("Pequena", "Mediana", "Grande")), outcome)

ver(heterogeneidad, filas = 12)
guardar_tabla(heterogeneidad, "T06_heterogeneidad_tamano",
              "Tabla 6. Efecto en 2023 por tamaño de firma", decimales = 3)

if (nrow(heterogeneidad) > 0) {
  grafico_het <- heterogeneidad %>%
    mutate(tamano = factor(tamano, levels = c("Pequena", "Mediana", "Grande"),
                           labels = c("Pequeña", "Mediana", "Grande"))) %>%
    ggplot(aes(x = tamano, y = efecto_pct, color = outcome)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_pointrange(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct),
                    position = position_dodge(width = 0.4)) +
    scale_color_manual(values = c(`Costo laboral` = COLOR_BAJA, `Empleo total` = COLOR_ALTA)) +
    labs(title = "Efecto en 2023 según el tamaño de la firma",
         subtitle = "Por DE de exposición, con controles de sector y departamento por año",
         x = NULL, y = "Efecto (%)", color = NULL, caption = NOTA) +
    tema_tesis
  guardar_grafico(grafico_het, "G06_heterogeneidad_tamano")
}


# ==============================================================================
# 7. ROBUSTEZ: AÑO BASE ALTERNATIVO
# ==============================================================================
titulo("7. ROBUSTEZ: 2019 COMO AÑO BASE")

# 2022 fue un año de empleo alto tras la recuperación post-pandemia. Si el
# "efecto" de 2023 depende de que el año base sea ese pico, hay que saberlo.
# Reestimamos el event study con 2019 como referencia: si el coeficiente de 2023
# se mantiene, el resultado no depende del año base; si desaparece, el quiebre
# de 2023 era un retorno al nivel previo.

evento_base_2019 <- function(outcome, tratamiento = "Bite2022_obreros") {
  v <- paste0(tratamiento, "_de")
  formula <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", v, ", ref = '2019') | ", EFECTOS))
  modelo <- tryCatch(feols(formula, data = datos, cluster = ~NORDEMP),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  nombre <- paste0("ANIO_F::2023:", v)
  if (!nombre %in% rownames(coeftable(modelo))) return(NULL)
  f <- coeftable(modelo)[nombre, ]
  tibble(outcome = outcome, base = "2019",
         efecto_pct = 100 * f[["Estimate"]],
         ic95_inf_pct = 100 * (f[["Estimate"]] - 1.96 * f[["Std. Error"]]),
         ic95_sup_pct = 100 * (f[["Estimate"]] + 1.96 * f[["Std. Error"]]),
         p_valor = f[["Pr(>|t|)"]], significancia = estrellas(f[["Pr(>|t|)"]]))
}

comparacion_base <- bind_rows(
  primer_eslabon %>%
    filter(medida == ETIQUETAS[["Bite2022_obreros"]], lectura == "A. Cambio 2022 -> 2023") %>%
    transmute(outcome = "log_costo", base = "2022", efecto_pct, ic95_inf_pct,
              ic95_sup_pct, p_valor, significancia),
  segundo_eslabon %>%
    filter(medida == ETIQUETAS[["Bite2022_obreros"]], escala == "Log (cambio %)",
           lectura == "A. Cambio 2022 -> 2023") %>%
    transmute(outcome = "log_empleo", base = "2022", efecto_pct, ic95_inf_pct,
              ic95_sup_pct, p_valor, significancia),
  evento_base_2019("log_costo"),
  evento_base_2019("log_empleo")
) %>%
  arrange(outcome, base)

ver(comparacion_base)
guardar_tabla(comparacion_base, "T07_robustez_ano_base",
              "Tabla 7. Coeficiente de 2023 con año base 2022 y 2019", decimales = 3)

cat("\nCÓMO LEER: si el coeficiente de 2023 cambia mucho al mover el año base,\n",
    "el resultado depende de que 2022 sea la referencia y hay que reportarlo\n",
    "así. Si se mantiene, es robusto a esa decisión.\n")


# ==============================================================================
# 7b. DOSIS-RESPUESTA: QUINTILES DE EXPOSICIÓN
# ==============================================================================
titulo("7b. DOSIS-RESPUESTA: QUINTILES DE EXPOSICIÓN")

# La especificación principal usa el Kaitz como variable continua, lo que
# supone que el efecto crece en línea recta con la exposición. Aquí se relaja
# ese supuesto: se divide a las firmas en quintiles del Kaitz de 2022 y se
# estima el event study de cada quintil frente al quintil 1 (el menos
# expuesto), con los mismos efectos fijos. Si el mínimo tiene efecto, debería
# verse un gradiente: mayor en los quintiles altos.

quintiles_kaitz <- base %>%
  filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
  distinct(NORDEMP, .keep_all = TRUE) %>%
  transmute(NORDEMP, Bite2022_obreros,
            quintil_kaitz = factor(ntile(Bite2022_obreros, 5)))

rangos_quintiles <- quintiles_kaitz %>%
  group_by(quintil_kaitz) %>%
  summarise(firmas = n(),
            kaitz_min = min(Bite2022_obreros), kaitz_mediano = median(Bite2022_obreros),
            kaitz_max = max(Bite2022_obreros), .groups = "drop")
ver(rangos_quintiles)
guardar_tabla(rangos_quintiles, "T09a_rangos_quintiles_kaitz",
              "Tabla 9a. Quintiles del Kaitz de obreros (2022): firmas y rangos", decimales = 3)

datos_q <- datos %>%
  inner_join(select(quintiles_kaitz, NORDEMP, quintil_kaitz), by = "NORDEMP")

RESULTADOS_QUINTILES <- c(log_costo = "Costo laboral por trabajador (log)",
                          log_empleo = "Empleo total (log)",
                          log_permanente = "Empleo permanente (log)")

evento_quintiles <- function(outcome) {
  m <- tryCatch(
    feols(as.formula(paste0(outcome, " ~ i(ANIO_F, quintil_kaitz, ref = '2022', ref2 = '1') | ",
                            EFECTOS)), data = datos_q, cluster = ~NORDEMP),
    error = function(e) NULL)
  if (is.null(m)) return(NULL)
  b <- coef(m); V <- vcov(m); nm <- names(b)
  anio <- as.integer(sub("^ANIO_F::(\\d+):.*$", "\\1", nm))
  q <- as.integer(sub("^.*::(\\d+)$", "\\1", nm))
  
  trayectoria <- tibble(anio = anio, quintil = q, coeficiente = unname(b),
                        error_estandar = sqrt(diag(V))) %>%
    bind_rows(tibble(anio = 2022L, quintil = 2:5, coeficiente = 0, error_estandar = 0)) %>%
    mutate(resultado = RESULTADOS_QUINTILES[[outcome]])
  
  p_previos <- tryCatch(
    wald(m, keep = "^ANIO_F::(2015|2016|2017|2018|2019|2021):", print = FALSE)$p,
    error = function(e) NA_real_)
  
  # Combinación lineal de coeficientes de un quintil, con su error estándar
  combinar <- function(pesos, etiqueta, qq) {
    w <- setNames(rep(0, length(b)), nm)
    for (a in names(pesos)) w[anio == as.integer(a) & q == qq] <- pesos[[a]]
    est <- sum(w * b); ee <- sqrt(as.numeric(t(w) %*% V %*% w))
    tibble(quintil = qq, lectura = etiqueta, efecto_pct = 100 * est,
           ic95_inf_pct = 100 * (est - 1.96 * ee), ic95_sup_pct = 100 * (est + 1.96 * ee),
           p_valor = 2 * pnorm(-abs(est / ee)))
  }
  pp <- -1/7   # promedio previo: 2015-2019, 2021 y 2022 (este último vale 0)
  efectos <- bind_rows(lapply(2:5, function(qq) bind_rows(
    combinar(c(`2023` = 1), "2023 vs 2022", qq),
    combinar(c(`2023` = 1, `2015` = pp, `2016` = pp, `2017` = pp, `2018` = pp,
               `2019` = pp, `2021` = pp), "2023 vs promedio previo", qq)
  ))) %>%
    mutate(resultado = RESULTADOS_QUINTILES[[outcome]], significancia = estrellas(p_valor),
           p_previos = p_previos, observaciones = nobs(m))
  
  list(trayectoria = trayectoria, efectos = efectos)
}

res_quintiles <- lapply(names(RESULTADOS_QUINTILES), evento_quintiles)

efectos_quintiles <- bind_rows(lapply(res_quintiles, `[[`, "efectos")) %>%
  select(resultado, lectura, quintil, efecto_pct, ic95_inf_pct, ic95_sup_pct,
         p_valor, significancia, p_previos, observaciones) %>%
  arrange(resultado, lectura, quintil)
ver(efectos_quintiles, filas = 30)
guardar_tabla(efectos_quintiles, "T09_quintiles_exposicion",
              "Tabla 9. Efecto en 2023 por quintil del Kaitz frente al quintil 1 (dosis-respuesta)",
              decimales = 3)

trayectoria_quintiles <- bind_rows(lapply(res_quintiles, `[[`, "trayectoria"))

grafico_quintiles <- ggplot(trayectoria_quintiles,
                            aes(x = anio, y = 100 * coeficiente,
                                color = factor(quintil), group = factor(quintil))) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.6) +
  facet_wrap(~ resultado, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  scale_color_manual(values = c(`2` = "#9DB4CE", `3` = "#5B84B1", `4` = "#E08080",
                                `5` = COLOR_ALTA)) +
  labs(title = "Dosis-respuesta: trayectoria de cada quintil de exposición frente al quintil 1",
       subtitle = "Diferencia frente a 2022. Quintiles del Kaitz de obreros de 2022",
       x = NULL, y = "Efecto (%)", color = "Quintil de Kaitz", caption = NOTA) +
  tema_tesis
guardar_grafico(grafico_quintiles, "G07_quintiles_exposicion", ancho = 10, alto = 11)

cat("\nCÓMO LEER: si el mínimo tiene efecto, el coeficiente de 2023 debería\n",
    "crecer de Q2 a Q5 (gradiente). Si los quintiles se separan desde antes de\n",
    "2023 (p_previos bajo), la diferencia es de trayectoria previa, no del\n",
    "choque; por eso se reporta también 2023 frente al promedio previo.\n")


# ==============================================================================
# 7c. SENSIBILIDAD A TENDENCIAS PARALELAS (HONESTDID)
# ==============================================================================
titulo("7c. SENSIBILIDAD A TENDENCIAS PARALELAS (HONESTDID)")

# La prueba conjunta de pre-tendencias (p_previos) solo dice si los
# coeficientes previos son cero. HonestDiD (Rambachan y Roth, 2023) pregunta
# algo más útil: cuánto tendría que violarse el supuesto de tendencias
# paralelas DESPUÉS de 2023 para que el efecto de 2023 deje de ser
# significativo. Dos supuestos:
#   - Suavidad (M): la tendencia previa puede continuar, pero su pendiente
#     solo cambia hasta M por período. M = 0 es la extrapolación lineal (la
#     versión formal de la lectura B).
#   - Magnitudes relativas (Mbar): la violación posterior no supera Mbar veces
#     la mayor variación observada entre períodos previos consecutivos.
# El "valor de quiebre" es el mayor M o Mbar con el que el intervalo sigue
# excluyendo el cero. La convención es considerar robusto un resultado que
# resiste al menos Mbar = 1.
#
# SALVEDADES que deben quedar en la tesis:
#   - Falta 2020: el paquete trata los períodos como consecutivos, así que el
#     paso 2019 -> 2021 cuenta como un período aunque sean dos años.
#   - El tratamiento es continuo: la interpretación causal requiere un
#     supuesto de tendencias paralelas más fuerte (Callaway, Goodman-Bacon y
#     Sant'Anna, 2024).
#   - Los intervalos están en puntos log (se reportan x 100 como %).

if (!requireNamespace("HonestDiD", quietly = TRUE)) {
  cat("AVISO: el paquete HonestDiD no está instalado. Se omite esta sección.\n",
      "Para instalarlo: remotes::install_github(\"asheshrambachan/HonestDiD\")\n")
} else {
  
  RESULTADOS_HONEST <- c(log_costo = "Costo laboral por trabajador (log)",
                         log_empleo = "Empleo total (log)",
                         log_permanente = "Empleo permanente (log)")
  M_GRILLA <- seq(0, 0.01, by = 0.0025)
  MBAR_GRILLA <- seq(0, 2, by = 0.25)
  
  # Ejecuta una función de HonestDiD, silencia sus avisos internos y registra
  # si el intervalo quedó abierto (se sale de la malla de búsqueda).
  con_avisos <- function(expr) {
    abierto <- FALSE
    valor <- withCallingHandlers(expr, warning = function(w) {
      if (grepl("open at one of the endpoints", conditionMessage(w))) abierto <<- TRUE
      invokeRestart("muffleWarning")
    })
    list(valor = valor, abierto = abierto)
  }
  
  honest_resultado <- function(outcome) {
    e <- estudio_evento(outcome, "Bite2022_obreros")
    if (is.null(e)) return(NULL)
    b <- coef(e$modelo); V <- vcov(e$modelo)
    anios <- as.integer(gsub(paste0("ANIO_F::|:", e$variable), "", names(b)))
    if (is.unsorted(anios) || !all(c(2023, 2024) %in% anios)) {
      cat("  AVISO: coeficientes fuera de orden para", outcome, "- se omite\n")
      return(NULL)
    }
    n_pre <- sum(anios < 2022); n_post <- sum(anios > 2022)
    l <- c(1, rep(0, n_post - 1))   # efecto de 2023
    
    orig <- con_avisos(HonestDiD::constructOriginalCS(
      betahat = b, sigma = V, numPrePeriods = n_pre, numPostPeriods = n_post, l_vec = l))
    suav <- con_avisos(HonestDiD::createSensitivityResults(
      betahat = b, sigma = V, numPrePeriods = n_pre, numPostPeriods = n_post,
      l_vec = l, Mvec = M_GRILLA))
    magn <- con_avisos(HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = b, sigma = V, numPrePeriods = n_pre, numPostPeriods = n_post,
      l_vec = l, Mbarvec = MBAR_GRILLA))
    
    bind_rows(
      tibble(supuesto = "Original (tendencias paralelas exactas)", parametro = 0,
             lb = as.numeric(orig$valor$lb), ub = as.numeric(orig$valor$ub),
             ic_abierto = orig$abierto),
      tibble(supuesto = "Suavidad (M)", parametro = suav$valor$M,
             lb = as.numeric(suav$valor$lb), ub = as.numeric(suav$valor$ub),
             ic_abierto = suav$abierto),
      tibble(supuesto = "Magnitudes relativas (Mbar)", parametro = magn$valor$Mbar,
             lb = as.numeric(magn$valor$lb), ub = as.numeric(magn$valor$ub),
             ic_abierto = magn$abierto)
    ) %>%
      mutate(resultado = RESULTADOS_HONEST[[outcome]],
             efecto_2023_pct = 100 * unname(b[anios == 2023]),
             p_previos = e$p_previos)
  }
  
  honest <- bind_rows(lapply(names(RESULTADOS_HONEST), honest_resultado)) %>%
    mutate(lb_pct = 100 * lb, ub_pct = 100 * ub,
           excluye_cero = lb > 0 | ub < 0) %>%
    select(resultado, supuesto, parametro, efecto_2023_pct, lb_pct, ub_pct,
           excluye_cero, ic_abierto, p_previos)
  
  ver(honest, filas = 60)
  guardar_tabla(honest, "T10_honestdid_sensibilidad",
                "Tabla 10. Sensibilidad del efecto de 2023 a violaciones de tendencias paralelas (HonestDiD)",
                decimales = 3)
  
  # Valor de quiebre: mayor parámetro con el que el intervalo excluye el cero
  quiebre <- honest %>%
    group_by(resultado) %>%
    mutate(significativo_original = excluye_cero[supuesto == "Original (tendencias paralelas exactas)"][1]) %>%
    filter(supuesto != "Original (tendencias paralelas exactas)") %>%
    group_by(resultado, supuesto, significativo_original) %>%
    summarise(
      valor_quiebre = if (all(!excluye_cero)) NA_real_ else max(parametro[excluye_cero]),
      maximo_grilla = max(parametro),
      .groups = "drop") %>%
    mutate(lectura = case_when(
      !significativo_original ~ "No significativo ni con tendencias paralelas exactas",
      is.na(valor_quiebre) ~ "Se pierde en cuanto se permite cualquier violación",
      valor_quiebre >= maximo_grilla ~ "Robusto en toda la grilla probada",
      TRUE ~ paste0("Robusto hasta ", valor_quiebre)
    ))
  
  ver(quiebre)
  guardar_tabla(quiebre, "T10b_honestdid_valor_quiebre",
                "Tabla 10b. Valor de quiebre de HonestDiD por resultado y supuesto",
                decimales = 4)
  
  grafico_honest <- honest %>%
    filter(supuesto != "Original (tendencias paralelas exactas)") %>%
    ggplot(aes(x = parametro, ymin = lb_pct, ymax = ub_pct)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_errorbar(aes(color = excluye_cero), width = 0) +
    geom_hline(data = honest %>% filter(supuesto != "Original (tendencias paralelas exactas)") %>%
                 distinct(resultado, supuesto, efecto_2023_pct),
               aes(yintercept = efecto_2023_pct), linetype = "dashed", color = COLOR_BAJA) +
    facet_grid(resultado ~ supuesto, scales = "free") +
    scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = "grey60"),
                       labels = c(`TRUE` = "Excluye el cero", `FALSE` = "Incluye el cero")) +
    labs(title = "Sensibilidad del efecto de 2023 a violaciones de tendencias paralelas",
         subtitle = "Intervalos robustos de HonestDiD (Rambachan y Roth, 2023). Línea punteada: estimación puntual",
         x = "Violación permitida (M en puntos log; Mbar en múltiplos de la mayor variación previa)",
         y = "Efecto en 2023 (%)", color = NULL,
         caption = "Kaitz de obreros. 2020 excluido: el paquete trata 2019 -> 2021 como un solo período.") +
    tema_tesis
  guardar_grafico(grafico_honest, "G08_honestdid", ancho = 11, alto = 10)
  
  cat("\nCÓMO LEER: un resultado es robusto si su intervalo sigue excluyendo el\n",
      "cero con Mbar >= 1 (una violación tan grande como la mayor variación\n",
      "previa). Si se pierde con Mbar < 1, el efecto depende de que las\n",
      "tendencias paralelas se cumplan casi exactamente, y así debe decirse.\n")
}


# ==============================================================================
# 8. RESUMEN
# ==============================================================================
titulo("8. RESUMEN")

principal <- bind_rows(
  primer_eslabon %>%
    filter(medida == ETIQUETAS[["Bite2022_obreros"]]) %>%
    mutate(eslabon = "1. Costo laboral por trabajador") %>%
    select(eslabon, lectura, efecto_pct, ic95_inf_pct, ic95_sup_pct, p_valor,
           significancia, p_previos),
  segundo_eslabon %>%
    filter(medida == ETIQUETAS[["Bite2022_obreros"]], escala == "Log (cambio %)") %>%
    mutate(eslabon = "2. Empleo total (log)") %>%
    select(eslabon, lectura, efecto_pct, ic95_inf_pct, ic95_sup_pct, p_valor,
           significancia, p_previos)
)

ver(principal)
guardar_tabla(principal, "T08_resumen_principal",
              "Tabla 8. Resultado principal con la medida Bite", decimales = 3)

cat("
QUÉ REPORTAR EN LA TESIS:

  1. PRIMER ESLABÓN. La lectura A es la cifra principal; B, C y D son
     lecturas de apoyo (ver encabezado). La sensibilidad al año en que se mide
     la exposición se documenta en 04 y se menciona como limitación.

  2. LAS CINCO MEDIDAS. Si todas dan el mismo signo, el resultado no depende de
     la definición de exposición, y eso es robustez real. Si Exposure se aparta,
     recordar que no usa salarios y por eso no capta el canal del piso salarial.

  3. TENDENCIA PREVIA (CAPÍTULO 6, no capítulo 5). Declararla de frente: es
     esperable por construcción de la medida y es una propiedad conocida de
     los diseños con bite salarial. La lectura B la corrige, y la celda limpia
     de 04_decision_medida.R (+0,82%, p=0,023) muestra el efecto con la
     exposición medida en 2019.

  4. SEGUNDO ESLABÓN. No escribir 'el empleo no cae'. Escribir 'no detectamos
     una caída', y reportar el intervalo completo. Con el primer eslabón de esta
     magnitud, la elasticidad implícita tiene un intervalo ancho: el nulo indica
     falta de precisión, no precisión sobre el cero.

  5. MECANISMOS. Exploratorios, en la especificación estándar, con p
     ajustados y con la advertencia de circularidad para la participación de
     obreros y el salario obrero. El traslape con el año base se declara como
     limitación: se leen como asociaciones con la exposición. Revisar
     p_previos fila por fila y la tabla 5c (2023 frente al promedio previo)
     para los mecanismos con tendencias previas.

  6. DOSIS-RESPUESTA (sección 7b). Reportar la tabla 9 como robustez de la
     forma funcional: el efecto por quintil de Kaitz frente al quintil 1. Un
     gradiente creciente respalda la especificación lineal; si el efecto se
     concentra en un quintil, decirlo.

  7. HONESTDID (sección 7c). Reportar el valor de quiebre (tabla 10b) junto
     a cada resultado principal. Un efecto que se pierde con Mbar < 1 depende
     de tendencias paralelas casi exactas: presentarlo con esa salvedad.
     Declarar que 2020 falta y que el tratamiento es continuo.

  8. LO QUE NO SE PUEDE CONCLUIR. Que el salario mínimo no afecta el empleo. Lo
     que se puede concluir es que, en manufactura formal, en el corto plazo, con
     esta medida de exposición y esta precisión, no detectamos un ajuste por la
     vía del empleo total. Posibles razones por las que un efecto real no se
     vería: subsidios al empleo con monto fijo por trabajador (PAEF, incentivo
     a nuevos empleos), efecto faro sobre toda la escala salarial, y la
     cobertura de la EAM (solo firmas formales por encima de umbrales, con
     salarios promedio por categoría).
")

save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
cat("\nCompendio guardado con", length(compendio), "tablas.\n")

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

