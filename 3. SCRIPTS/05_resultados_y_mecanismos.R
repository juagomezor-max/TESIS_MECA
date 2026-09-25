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
# Salidas:  4. RESULTADOS/Resultados_mecanismos/
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulo:     5. Resultados (primer eslabón, segundo eslabón, mecanismos,
#               heterogeneidad)
# Por qué Bite: la medida principal usada en las secciones 5, 6 y 7 (donde
#               solo se corre una) es Bite2022_obreros porque es la que gana
#               la celda limpia en 04_decision_medida.R (+1,15%,
#               p=0,008) -- ver ese script para la regla de decisión completa.
#               No se re-justifica aquí, solo se usa.
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
# calcula aquí (solo lectura A). 06_mecanismos_por_grupo.R resuelve
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
# sector (CIIU4), tamaño y departamento interactuados con año, todos fijados en
# 2022, y errores agrupados por firma.
#
# De ahí salen tres lecturas del primer eslabón, que NO son lo mismo:
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
# defecto de implementación. La lectura B existe precisamente para corregirla,
# y la validación con exposición de 2019 (04_decision_medida.R) da
# un efecto positivo y significativo sin depender del año base. Las dos cosas
# van escritas en la tesis como matices del resultado, no como su refutación.
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

CARPETA <- file.path("4. RESULTADOS", "Resultados_mecanismos")
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
    
    # --- Identificadores y factores ---
    ANIO_F = factor(ANIO),
    post = as.integer(ANIO >= 2023),
    sector_2022 = factor(CIIU4),
    depto_2022 = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

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
    # Indicadores de margen extensivo
    hace_outsourcing = as.integer(!is.na(outsourcing) & outsourcing > 0),
    usa_agencias = as.integer(!is.na(empleo_temporal_agencias) & empleo_temporal_agencias > 0),
    invierte = as.integer(!is.na(inversion) & inversion > 0)
  )

# --- Medidas de exposición, winsorizadas y estandarizadas ------------------------
# Winsorización 1/99, igual que en el póster. Conserva las firmas extremas con
# un valor recortado en vez de eliminarlas.
winsorizar <- function(x) {
  lim <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, lim[1]), lim[2])
}

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

EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
cat("Efectos fijos:", EFECTOS, "\n")
cat("Errores agrupados por firma (NORDEMP).\n")

# estudio_evento(): coeficiente año por año frente a 2022, más la prueba
# conjunta de los años previos al choque.
estudio_evento <- function(outcome, tratamiento, base_datos = datos) {
  v <- paste0(tratamiento, "_de")
  if (!v %in% names(base_datos) || !outcome %in% names(base_datos)) return(NULL)
  
  formula <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", v, ", ref = '2022') | ", EFECTOS))
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
    wald(modelo, keep = "ANIO_F::(2015|2016|2017|2018|2019|2021):", print = FALSE)$p,
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

# tres_lecturas(): A, B y C para un outcome y una medida.
tres_lecturas <- function(outcome, tratamiento, base_datos = datos) {
  evento <- estudio_evento(outcome, tratamiento, base_datos)
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
    feols(as.formula(paste0(outcome, " ~ post:", v, " | ", EFECTOS)),
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
  
  bind_rows(a, b, c_fila) %>%
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

NOTA <- paste("Controles: firma, año, sector x año, tamaño x año y departamento x año (fijados en 2022).",
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

segundo_eslabon <- bind_rows(
  bind_rows(lapply(MEDIDAS, function(m) tres_lecturas("empleo_total", m))) %>%
    mutate(escala = "Trabajadores"),
  bind_rows(lapply(MEDIDAS, function(m) tres_lecturas("log_empleo", m))) %>%
    mutate(escala = "Log (cambio %)")
) %>%
  select(medida, escala, lectura, efecto_pct, ic95_inf_pct, ic95_sup_pct,
         p_valor, significancia, p_previos, observaciones)

ver(segundo_eslabon, filas = 30)
guardar_tabla(segundo_eslabon, "T02_segundo_eslabon_empleo",
              "Tabla 2. Segundo eslabón: efecto sobre el empleo total", decimales = 3)

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
  
  "log_obreros",                "Obreros (log)",                            "2. Composición ocupacional",
  "participacion_obreros",      "% obreros (OJO: circular)",                "2. Composición ocupacional",
  "log_brecha_obrero_admin",    "Salario obrero / salario admin (log)",     "3. Compresión salarial",
  
  "log_outsourcing",            "Outsourcing (log)",                        "4. Tercerización",
  "hace_outsourcing",           "Hace outsourcing (indicador)",             "4. Tercerización",
  "log_honorarios",             "Honorarios y servicios técnicos (log)",    "4. Tercerización",
  "log_pago_agencias",          "Pago a agencias temporales (log)",         "4. Tercerización",
  
  "log_inversion",              "Inversión bruta (log)",                    "5. Capital",
  "invierte",                   "Invierte (indicador)",                     "5. Capital",
  
  "log_ventas",                 "Ventas (log)",                             "6. Producción",
  "log_produccion",             "Producción bruta (log)",                   "6. Producción",
  "log_valor_agregado",         "Valor agregado (log)",                     "6. Producción",
  "log_productividad",          "Productividad (VA / trabajador, log)",     "6. Producción"
)

cat("Estimando", nrow(MECANISMOS), "mecanismos con la medida principal...\n")

mecanismos <- bind_rows(lapply(seq_len(nrow(MECANISMOS)), function(i) {
  fila <- MECANISMOS[i, ]
  if (!fila$outcome %in% names(datos)) {
    cat("  AVISO: no existe la variable", fila$outcome, "\n")
    return(NULL)
  }
  r <- tres_lecturas(fila$outcome, "Bite2022_obreros")
  if (is.null(r)) return(NULL)
  r %>%
    filter(lectura == "A. Cambio 2022 -> 2023") %>%
    mutate(mecanismo = fila$etiqueta, grupo = fila$grupo, variable = fila$outcome)
}))

# Ajuste por pruebas múltiples. Con ~20 outcomes, algunos salen significativos
# por azar: con 20 pruebas al 5%, se espera un falso positivo aunque no haya
# ningún efecto real.
mecanismos <- mecanismos %>%
  mutate(
    p_ajustado_bh = p.adjust(p_valor, method = "BH"),
    significancia_cruda = estrellas(p_valor),
    significancia_ajustada = estrellas(p_ajustado_bh)
  ) %>%
  select(grupo, mecanismo, efecto_pct, ic95_inf_pct, ic95_sup_pct,
         p_valor, significancia_cruda, p_ajustado_bh, significancia_ajustada,
         p_previos, observaciones) %>%
  arrange(grupo, desc(abs(efecto_pct)))

ver(mecanismos, filas = 30)
guardar_tabla(mecanismos, "T05_mecanismos_ajuste",
              "Tabla 5. Mecanismos de ajuste: efecto en 2023 por DE de exposición (exploratorio)",
              decimales = 3)

cat("\nCÓMO LEER LA TABLA DE MECANISMOS:\n",
    " - Usar 'significancia_ajustada', no la cruda. Con 20 pruebas al 5% se\n",
    "   espera un falso positivo aunque no haya ningún efecto real.\n",
    " - Revisar 'p_previos' de cada fila: si es bajo, ese mecanismo ya tenía\n",
    "   tendencias diferenciales antes del choque y su coeficiente no es\n",
    "   interpretable como efecto AQUÍ (esta tabla solo usa la lectura A). Si\n",
    "   son varios los mecanismos en ese caso, ver\n",
    "   06_mecanismos_por_grupo.R (lectura B).\n",
    " - Los indicadores (usa_agencias, hace_outsourcing, invierte) están en\n",
    "   puntos porcentuales, no en cambio porcentual. No mezclar escalas.\n")

grafico_mecanismos <- mecanismos %>%
  filter(!is.na(efecto_pct)) %>%
  ggplot(aes(x = reorder(mecanismo, efecto_pct), y = efecto_pct,
             color = significancia_ajustada != "")) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct)) +
  coord_flip() +
  facet_wrap(~ grupo, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = "grey60"),
                     labels = c("No significativo (ajustado)", "Significativo (ajustado)")) +
  labs(title = "¿Por dónde ajustan las firmas más expuestas?",
       subtitle = "Efecto en 2023 por DE de exposición. Resultados exploratorios",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = paste("p-valores ajustados por Benjamini-Hochberg.",
                       "\nLos indicadores están en puntos porcentuales; el resto en cambio porcentual.")) +
  tema_tesis +
  theme(axis.text.y = element_text(size = 8))
guardar_grafico(grafico_mecanismos, "G04_mecanismos", alto = 10)

# Event studies de los mecanismos que salgan significativos tras el ajuste
significativos <- mecanismos %>% filter(significancia_ajustada != "") %>% pull(mecanismo)

if (length(significativos) > 0) {
  cat("\nMecanismos significativos tras el ajuste:",
      paste(significativos, collapse = ", "), "\n")
  
  vars_sig <- MECANISMOS %>% filter(etiqueta %in% significativos)
  
  eventos_mecanismos <- bind_rows(lapply(seq_len(nrow(vars_sig)), function(i) {
    e <- estudio_evento(vars_sig$outcome[i], "Bite2022_obreros")
    if (is.null(e)) return(NULL)
    mutate(e$tabla, mecanismo = vars_sig$etiqueta[i])
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
    guardar_grafico(grafico_ev_mec, "G05_evento_mecanismos", alto = 8)
  }
} else {
  cat("\nNingún mecanismo resulta significativo tras el ajuste por pruebas\n",
      "múltiples. Eso también es un resultado: las firmas expuestas no ajustan\n",
      "de forma detectable por ninguno de los márgenes observables en la EAM.\n")
}


# ==============================================================================
# 6. HETEROGENEIDAD POR TAMAÑO
# ==============================================================================
titulo("6. HETEROGENEIDAD POR TAMAÑO")

# Las firmas pequeñas tienen menos capacidad de absorber un aumento de costos:
# menos margen, menos acceso a crédito, menos posibilidad de sustituir capital
# por trabajo. Dentro de cada grupo de tamaño quitamos el control de tamaño×año,
# que ya no aporta variación.

EFECTOS_SIN_TAMANO <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F"

heterogeneidad <- bind_rows(lapply(c("Pequena", "Mediana", "Grande"), function(t) {
  sub <- filter(datos, tamano_2022 == t)
  if (nrow(sub) < 500) return(NULL)
  
  resultados <- bind_rows(lapply(c("log_costo", "log_empleo"), function(oc) {
    formula <- as.formula(paste0(oc, " ~ i(ANIO_F, Bite2022_obreros_de, ref = '2022') | ",
                                 EFECTOS_SIN_TAMANO))
    modelo <- tryCatch(feols(formula, data = sub, cluster = ~NORDEMP),
                       error = function(e) NULL)
    if (is.null(modelo)) return(NULL)
    nombre <- "ANIO_F::2023:Bite2022_obreros_de"
    if (!nombre %in% rownames(coeftable(modelo))) return(NULL)
    f <- coeftable(modelo)[nombre, ]
    tibble(tamano = t,
           outcome = ifelse(oc == "log_costo", "Costo laboral", "Empleo total"),
           efecto_pct = 100 * f[["Estimate"]],
           ic95_inf_pct = 100 * (f[["Estimate"]] - 1.96 * f[["Std. Error"]]),
           ic95_sup_pct = 100 * (f[["Estimate"]] + 1.96 * f[["Std. Error"]]),
           p_valor = f[["Pr(>|t|)"]],
           significancia = estrellas(f[["Pr(>|t|)"]]),
           observaciones = nobs(modelo))
  }))
  resultados
}))

ver(heterogeneidad)
guardar_tabla(heterogeneidad, "T06_heterogeneidad_tamano",
              "Tabla 6. Efecto en 2023 por tamaño de firma", decimales = 3)

if (nrow(heterogeneidad) > 0) {
  grafico_het <- heterogeneidad %>%
    mutate(tamano = factor(tamano, levels = c("Pequena", "Mediana", "Grande"))) %>%
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

  1. PRIMER ESLABÓN. La lectura A es la cifra principal. Reportar B al lado y
     explicar por qué es mayor: la tendencia previa es descendente, así que el
     contrafactual de 2023 no es cero sino seguir cayendo. Mencionar C solo si
     se explica que mezcla nivel post con trayectoria previa.

  2. LAS CINCO MEDIDAS. Si todas dan el mismo signo, el resultado no depende de
     la definición de exposición, y eso es robustez real. Si Exposure se aparta,
     recordar que no usa salarios y por eso no capta el canal del piso salarial.

  3. TENDENCIA PREVIA (CAPÍTULO 6, no capítulo 5). Declararla de frente: es
     esperable por construcción de la medida y es una propiedad conocida de
     los diseños con bite salarial. Las defensas son la lectura B y la
     validación con exposición de 2019 de 04_decision_medida.R
     (+1,15%, p=0,008), que no depende del año base.

  4. SEGUNDO ESLABÓN. No escribir 'el empleo no cae'. Escribir 'no detectamos
     una caída', y reportar el intervalo completo. Con el primer eslabón de esta
     magnitud, la elasticidad implícita tiene un intervalo ancho: el nulo indica
     falta de precisión, no precisión sobre el cero.

  5. MECANISMOS. Exploratorios, con p ajustados, y con la advertencia de
     circularidad para la participación de obreros. Un mecanismo con tendencias
     previas significativas no es interpretable como efecto tal como se
     calcula aquí (solo lectura A): revisar p_previos fila por fila. Si la
     mayoría de los mecanismos cae en ese caso, ver
     06_mecanismos_por_grupo.R, que usa la lectura B (salto contra
     la trayectoria previa de cada mecanismo, no contra cero) para no perder
     esos canales por un problema que es de la lectura, no del mecanismo.

  6. LO QUE NO SE PUEDE CONCLUIR. Que el salario mínimo no afecta el empleo. Lo
     que se puede concluir es que, en manufactura formal, en el corto plazo, con
     esta medida de exposición y esta precisión, no detectamos un ajuste por la
     vía del empleo total.
")

save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
cat("\nCompendio guardado con", length(compendio), "tablas.\n")

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")
