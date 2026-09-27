# ==============================================================================
# 12_celda_limpia_real.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# LA PRUEBA QUE FALTA: CELDA LIMPIA, TODAS LAS MEDIDAS, ORDENADAS POR EL
# AUMENTO REAL DEL SALARIO MÍNIMO
#
# QUÉ CORRIGE ESTE SCRIPT RESPECTO A 10_celda_limpia_rodante.R
# Esa versión comparó 2023 contra "los años normales" usando el aumento
# NOMINAL del mínimo (implícito: 2023 es especial porque subió más). Pero
# 11_placebo_rodante_real.R (con la especificación ESTÁNDAR, contaminada) ya
# mostró que ordenar por aumento real cambia el panorama: 2023 (+6,15% real) y
# 2024 (+6,53% real) son, por lejos, los años de mayor aumento real; 2015
# (-2,03%), 2021 (-2,01%) y 2022 (-2,70%) tuvieron aumento real NEGATIVO. Con
# la estándar, el grupo de aumento real alto tuvo el diferencial MÁS BAJO de
# los tres grupos -- el signo contrario al que predice un mecanismo real.
#
# La pregunta que falta: con la CELDA LIMPIA (que corta el traslape
# aritmético) y los años ordenados por aumento REAL, ¿el diferencial sí escala
# con el tamaño del choque? Este script reemplaza la comparación "2023 contra
# el resto" de 10_celda_limpia_rodante.R por una comparación de tres grupos
# (alto / intermedio / negativo, por aumento real), para cada una de las cinco
# medidas, y agrega la sensibilidad al rezago para 2024 además de 2023.
#
# DE DÓNDE VIENE 10_celda_limpia_rodante.R
# 09_diagnostico_reversion.R mostró que la especificación estándar produce un
# diferencial de ~4% todos los años, sin relación con el tamaño del aumento del
# mínimo. Pero esa prueba tenía una limitación que conviene tener presente al
# leerla: usaba la especificación contaminada en TODOS los años, así que medía
# lo mismo contaminado siempre. Un diseño así no puede detectar señal aunque la
# haya, porque el componente mecánico domina en cada punto de la serie.
#
# La auditoría de la cadena aportó el dato que cambia la lectura: con un outcome
# que no toca el año base de la exposición (crecimiento 2023-2024), las medidas
# basadas en salario pierden entre 34% y 63% del coeficiente PERO NINGUNA PIERDE
# LA SIGNIFICANCIA. Hay señal real debajo de la contaminación. Y golpe_c y
# golpe_a resisten mejor que Bite, que fue la medida elegida.
#
# QUÉ HACE ESTE SCRIPT
# Corre el placebo rodante sobre la especificación de CELDA LIMPIA -- exposición
# medida varios años antes del año base del outcome -- para las cinco medidas a
# la vez. Es la primera prueba que combina las tres cosas: sin contaminación en
# ningún año, comparando por GRUPOS DE AUMENTO REAL en vez de "2023 contra el
# resto", y con la sensibilidad al rezago para los dos años de aumento real alto.
#
# POR QUÉ LA CELDA LIMPIA CORTA EL PROBLEMA
# La especificación estándar mide la exposición con el salario de t-1 y el
# crecimiento de t-1 a t. El salario de t-1 está en el denominador de la
# exposición Y en la base del outcome: un error de reporte en ese número empuja
# las dos cosas en la misma dirección y produce diferencial sin economía.
#
# Con la exposición medida en t-5, ese error ya no puede estar correlacionado
# con el crecimiento de t-1 a t. Lo que queda es la parte persistente del nivel
# salarial de la firma, que es justamente lo que la exposición pretende medir.
#
# EL COSTO: atenuación. La exposición vieja predice peor la actual, así que el
# coeficiente se sesga hacia cero. Por eso la celda limpia da 1,15% y la
# estándar 2,98%: no son dos estimaciones del mismo número con distinta suerte,
# son la versión atenuada y la contaminada del mismo objeto. La limpia es cota
# inferior, y hay que reportarla como tal.
#
# DOS HUECOS QUE ESTE SCRIPT NO RESUELVE Y HAY QUE CERRAR APARTE
#   - 09_diagnostico_reversion.R no existe en ningún commit. Sus resultados
#     están en disco pero el código no está versionado, así que el diagnóstico
#     que se está usando para decidir no es reproducible hoy.
#   - La regla de decisión de NOTA_DECISIONES.md no es la que aplica
#     04_decision_medida.R, y no hay registro de cuándo cambió.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  4. RESULTADOS/ARCHIVADO_commit_89b959e/Celda_limpia_real/
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

CARPETA <- file.path("4. RESULTADOS", "ARCHIVADO_commit_89b959e", "Celda_limpia_real")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 40) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3) {
  tw <- flextable(tabla)
  tw <- colformat_double(tw, digits = decimales)
  tw <- set_caption(tw, caption = titulo_tabla)
  tw <- autofit(tw)
  save_as_docx(tw, path = file.path(CARPETA, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(CARPETA, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tw
  cat("Tabla guardada:", nombre_archivo, "\n")
}

graficos <- list()
guardar_grafico <- function(g, nombre_archivo, ancho = 10, alto = 6) {
  print(g)
  ggsave(file.path(CARPETA, "figuras", paste0(nombre_archivo, ".png")),
         g, width = ancho, height = alto, dpi = 200, bg = "white")
  graficos[[nombre_archivo]] <<- g
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
COLOR_GRIS <- "#808080"
PALETA <- c(`Bite (Kaitz de obreros)` = "#C00000",
            `Golpe C (3 categorías)`  = "#1F4E79",
            `Golpe A (ponderada)`     = "#2E8B57",
            `Golpe costo`             = "#E69F00",
            `Exposure (% obreros)`    = "#7B68EE")

# CORRECCIÓN RESPECTO A 12_celda_limpia_real.R (y su origen, 10_celda_limpia_
# rodante.R): esos scripts indexaban el rezago sobre el AÑO BASE del outcome
# (anio_base <- anio_choque - 1; anio_exp <- anio_base - REZAGO), no sobre el
# año del choque. Para el choque de 2023 eso daba exposición 2018, no 2019 --
# un año antes de lo que dice la celda limpia real de la tesis
# (04_decision_medida.R: exposición 2019 contra crecimiento 2022-2023). El
# efecto medido con esa fila mal indexada (0,14%, no significativo) no es la
# celda limpia de la tesis; la fila correcta (rezago 3 con la indexación
# vieja) daba 0,81%, significativo al 5%.
#
# REZAGO se define ahora como la distancia entre el año de la exposición y el
# AÑO DEL CHOQUE (no el año base del outcome). Es la distancia que importa
# económicamente: cuánto tiempo pasó entre cuándo se midió la exposición y
# cuándo ocurrió el aumento del mínimo que se está evaluando, que es lo que
# determina si el error de medición de la exposición puede quedar
# correlacionado con el outcome (contaminación) o no (celda limpia).
#
# anio_exp   <- anio_choque - REZAGO
# anio_base  <- anio_choque - 1                 (siempre el año inmediatamente
#                                                anterior al choque, sin importar
#                                                el rezago de la exposición)
#
# Con REZAGO <- 4, el choque de 2023 usa exposición de 2019 y outcome
# 2022-2023 -- eso SÍ reproduce la celda limpia de la tesis.
REZAGO <- 4


# ==============================================================================
# 1. DATOS Y CONSTRUCCIÓN DE LAS MEDIDAS
# ==============================================================================
titulo("1. DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

# IPC_PCT: variación anual del IPC del DANE a diciembre de cada año (el mismo
# año calendario del aumento, no el anterior). Solo hay valores para 2012-2024
# porque son los únicos años que este script usa como anio_choque (ANIOS más
# abajo empieza en 2014, y anio_choque nunca es anterior a eso); no hace falta
# el IPC de 2008-2011, que aquí solo se usan como año de exposición rezagada.
#
# DEFLACTOR -- mismo criterio y misma razón que en 11_placebo_rodante_real.R:
# se usa el IPC del año EN QUE EL SALARIO ESTÁ VIGENTE, no el del año anterior,
# porque la pregunta es sobre el costo laboral REAL de la firma (una firma que
# en 2023 paga 16% más de nómina vende a precios que subieron 9,28% ESE MISMO
# año). Deflactar por la inflación causada del año previo -la convención de la
# negociación del salario mínimo, que mide recuperación de poder adquisitivo-
# es una pregunta distinta y da un número distinto (~3% para 2023, no +6,15%).
SALARIO_MINIMO <- tibble(
  anio  = 2008:2024,
  valor = c(461500, 496900, 515000, 535600, 566700, 589500, 616000, 644350,
            689455, 737717, 781242, 828116, 877803, 908526, 1000000,
            1160000, 1300000),
  ipc_pct = c(rep(NA_real_, 4),
              2.44, 1.94, 3.66, 6.77, 5.75, 4.09, 3.18,
              3.80, 1.61, 5.62, 13.12, 9.28, 5.20)
) %>%
  mutate(
    aumento_nominal_pct = 100 * (valor / lag(valor) - 1),
    aumento_real_pct = 100 * ((1 + aumento_nominal_pct / 100) / (1 + ipc_pct / 100) - 1)
  )

ver(SALARIO_MINIMO)
guardar_tabla(SALARIO_MINIMO, "T00_salario_minimo_nominal_y_real",
              "Tabla 0. Salario mínimo, IPC del año vigente, aumento nominal y real", decimales = 2)

# Costo mínimo total de contratación, como razón del salario mínimo. Se usa para
# golpe_costo. La razón (1,531) viene de 02_medidas_exposicion.R: prestaciones,
# pensión, ARL y caja sobre el mínimo más auxilio de transporte.
RAZON_COSTO_MINIMO <- 1.531

base <- panel %>%
  mutate(
    empleo = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo, NA_real_),
    
    # Salarios mensuales por categoría (sueldos + prestaciones / personas / 12)
    w_obrero = ifelse(obreros_permanentes > 0 &
                        (sueldos_permanentes_obreros_c3r2c1 +
                           prestaciones_permanentes_obreros_c3r3c1) > 0,
                      (sueldos_permanentes_obreros_c3r2c1 +
                         prestaciones_permanentes_obreros_c3r3c1) /
                        obreros_permanentes / 12 * 1000, NA_real_),
    w_prof = ifelse(profesional_tecnico_permanentes > 0 &
                      (sueldos_permanentes_profesional_tecnico_c3r2pt +
                         prestaciones_permanentes_profesional_tecnico_c3r3pt) > 0,
                    (sueldos_permanentes_profesional_tecnico_c3r2pt +
                       prestaciones_permanentes_profesional_tecnico_c3r3pt) /
                      profesional_tecnico_permanentes / 12 * 1000, NA_real_),
    w_admin = ifelse(administrativos_permanentes > 0 &
                       (sueldos_permanentes_administrativos_c3r2c2 +
                          prestaciones_permanentes_administrativos_c3r3c2) > 0,
                     (sueldos_permanentes_administrativos_c3r2c2 +
                        prestaciones_permanentes_administrativos_c3r3c2) /
                       administrativos_permanentes / 12 * 1000, NA_real_),
    
    # w_obrero_sueldos: solo sueldos, sin prestaciones. Es la versión que usa
    # Bite en el pipeline actual (02_construir_exposicion.R). Se conserva para
    # poder reproducir la cifra de la tesis.
    w_obrero_sueldos = ifelse(obreros_permanentes > 0 &
                                sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 /
                                obreros_permanentes / 12 * 1000, NA_real_),
    
    n_categorias = (!is.na(w_obrero)) + (!is.na(w_prof)) + (!is.na(w_admin)),
    
    # w_firma: promedio simple de las categorías PRESENTES. Dividir por 3 fijo
    # haría que una firma sin profesionales saliera mecánicamente más expuesta.
    w_firma = ifelse(n_categorias > 0,
                     rowSums(cbind(w_obrero, w_prof, w_admin), na.rm = TRUE) /
                       n_categorias, NA_real_),
    
    c_firma = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                     costos_totales_personal_total_c3r10c3 / empleo / 12 * 1000,
                     NA_real_),
    
    permanentes_total = rowSums(cbind(
      ifelse(is.na(w_obrero), 0, obreros_permanentes),
      ifelse(is.na(w_prof), 0, profesional_tecnico_permanentes),
      ifelse(is.na(w_admin), 0, administrativos_permanentes)), na.rm = TRUE),
    
    exposure_obreros = ifelse(empleo > 0, obreros_total_ocupado / empleo, NA_real_),
    
    log_costo = log(ifelse(costo_trabajador > 0, costo_trabajador, NA_real_)),
    log_brecha = log(ifelse(!is.na(w_obrero) & !is.na(w_admin) & w_admin > 0,
                            w_obrero / w_admin, NA_real_)),
    
    sector_2022 = factor(CIIU4),
    depto_2022 = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

cat("Panel:", nrow(base), "filas |", n_distinct(base$NORDEMP), "firmas\n")


# construir_medidas(): las cinco medidas para un año de exposición dado, con el
# mínimo del año del choque en el numerador.
#
# Cada medida tiene un denominador distinto, y ahí está lo que las diferencia
# frente a la contaminación:
#   Bite        salario del obrero de la firma          -- el más expuesto
#   Golpe C     promedio de las tres categorías         -- menos expuesto
#   Golpe A     armónica ponderada por peso en empleo   -- menos expuesto
#   Golpe costo costo laboral total por trabajador      -- muy expuesto
#   Exposure    proporción de obreros (sin salarios)    -- inmune por diseño
construir_medidas <- function(anio_exposicion, anio_choque) {
  
  minimo <- SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == anio_choque]
  if (length(minimo) == 0) return(NULL)
  costo_minimo <- minimo * RAZON_COSTO_MINIMO
  
  medidas <- base %>%
    filter(ANIO == anio_exposicion) %>%
    transmute(
      NORDEMP,
      bite    = ifelse(!is.na(w_obrero_sueldos) & w_obrero_sueldos > 0,
                       minimo / w_obrero_sueldos, NA_real_),
      golpe_c = ifelse(!is.na(w_firma) & w_firma > 0, minimo / w_firma, NA_real_),
      golpe_a = ifelse(
        permanentes_total > 0 & n_categorias > 0,
        (ifelse(is.na(w_obrero), 0, obreros_permanentes / permanentes_total *
                  minimo / w_obrero) +
           ifelse(is.na(w_prof), 0, profesional_tecnico_permanentes / permanentes_total *
                    minimo / w_prof) +
           ifelse(is.na(w_admin), 0, administrativos_permanentes / permanentes_total *
                    minimo / w_admin)),
        NA_real_),
      golpe_costo = ifelse(!is.na(c_firma) & c_firma > 0,
                           costo_minimo / c_firma, NA_real_),
      exposure = exposure_obreros
    )
  
  # Winsorización 1/99 y estandarización a DE = 1, igual que en la
  # especificación principal. Sin estandarizar, los coeficientes de medidas con
  # escalas distintas no son comparables entre sí.
  medidas %>%
    mutate(across(c(bite, golpe_c, golpe_a, golpe_costo, exposure), ~ {
      if (all(is.na(.x))) return(.x)
      lim <- quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE)
      w <- pmin(pmax(.x, lim[1]), lim[2])
      w / sd(w, na.rm = TRUE)
    }))
}

MEDIDAS <- c(bite = "Bite (Kaitz de obreros)",
             golpe_c = "Golpe C (3 categorías)",
             golpe_a = "Golpe A (ponderada)",
             golpe_costo = "Golpe costo",
             exposure = "Exposure (% obreros)")


# ==============================================================================
# 2. FUNCIÓN DE ESTIMACIÓN
# ==============================================================================
titulo("2. ESPECIFICACIÓN")

cat("Outcome: crecimiento del costo laboral por trabajador entre el año base\n",
    "y el año del choque.\n")
cat("Exposición: medida", REZAGO, "años antes del año base.\n")
cat("Controles: sector (CIIU4), departamento y tamaño. Errores robustos.\n")

# salto(): el diferencial de crecimiento por DE de exposición, para una medida
# y una combinación de años. Cuando anio_exposicion == anio_base estamos en la
# especificación estándar (contaminada); cuando está rezagada, en la limpia.
salto <- function(medida, anio_exposicion, anio_base, anio_choque,
                  outcome = "log_costo") {
  
  medidas <- construir_medidas(anio_exposicion, anio_choque)
  if (is.null(medidas) || !medida %in% names(medidas)) return(NULL)
  
  exposicion <- medidas %>%
    select(NORDEMP, valor = all_of(medida)) %>%
    filter(!is.na(valor), is.finite(valor))
  if (nrow(exposicion) < 500) return(NULL)
  
  valores <- base %>%
    filter(ANIO %in% c(anio_base, anio_choque)) %>%
    select(NORDEMP, ANIO, all_of(outcome), sector_2022, depto_2022, tamano_2022) %>%
    pivot_wider(names_from = ANIO, values_from = all_of(outcome), names_prefix = "y_")
  
  col_base <- paste0("y_", anio_base)
  col_choque <- paste0("y_", anio_choque)
  if (!all(c(col_base, col_choque) %in% names(valores))) return(NULL)
  
  datos <- valores %>%
    mutate(crecimiento = .data[[col_choque]] - .data[[col_base]]) %>%
    inner_join(exposicion, by = "NORDEMP") %>%
    filter(is.finite(crecimiento))
  
  if (nrow(datos) < 400) return(NULL)
  
  modelo <- tryCatch(
    feols(crecimiento ~ valor | sector_2022 + depto_2022 + tamano_2022,
          data = datos, vcov = "hetero"),
    error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  
  f <- coeftable(modelo)["valor", ]
  tibble(
    medida = MEDIDAS[[medida]],
    clave = medida,
    anio_choque = anio_choque,
    anio_exposicion = anio_exposicion,
    aumento_minimo_pct = SALARIO_MINIMO$aumento_nominal_pct[SALARIO_MINIMO$anio == anio_choque],
    aumento_real_pct = SALARIO_MINIMO$aumento_real_pct[SALARIO_MINIMO$anio == anio_choque],
    efecto_pct = 100 * f[["Estimate"]],
    ee_pct = 100 * f[["Std. Error"]],
    p_valor = f[["Pr(>|t|)"]],
    significancia = estrellas(f[["Pr(>|t|)"]]),
    firmas = nobs(modelo)
  )
}

anios_panel <- sort(unique(base$ANIO))

# Años estimables: hacen falta el salario de t-1-REZAGO, y el outcome en t-1 y
# en t. 2020 queda fuera como año base y como año de exposición (pandemia), y
# 2021 como año de choque porque su base sería 2020.
ANIOS <- anios_panel[anios_panel >= 2014 & !(anios_panel %in% c(2020, 2021))]

cat("\nAños en los que se intenta estimar:", paste(ANIOS, collapse = ", "), "\n")


# ==============================================================================
# 3. CELDA LIMPIA RODANTE, LAS CINCO MEDIDAS
# ==============================================================================
titulo("3. CELDA LIMPIA RODANTE")

limpia <- bind_rows(lapply(names(MEDIDAS), function(m) {
  bind_rows(lapply(ANIOS, function(a) {
    anio_base <- a - 1
    if (anio_base == 2020) return(NULL)
    # REZAGO contado desde el año del CHOQUE (a), no desde el año base -- ver
    # la nota de la sección 0. Con REZAGO=4 y a=2023, anio_exp=2019 (celda
    # limpia real de la tesis), no 2018.
    anio_exp <- a - REZAGO
    if (anio_exp == 2020 || !(anio_exp %in% anios_panel)) return(NULL)
    if (!(anio_base %in% anios_panel)) return(NULL)
    salto(m, anio_exp, anio_base, a, "log_costo")
  }))
})) %>%
  mutate(es_choque = anio_choque == 2023,
         ic_inf = efecto_pct - 1.96 * ee_pct,
         ic_sup = efecto_pct + 1.96 * ee_pct)

ver(limpia, filas = 50)
guardar_tabla(limpia, "T01_celda_limpia_rodante",
              "Tabla 1. Celda limpia año por año, las cinco medidas")

# --- Qué año de exposición usa cada año de choque, explícito ---------------------
# anio_base no viene en la tabla `limpia` (es una variable local del lapply,
# nunca se guardó en la tibble que devuelve salto()); se reconstruye aquí
# porque siempre es anio_choque - 1, sea cual sea el rezago de la exposición.
anios_expuestos <- limpia %>%
  distinct(anio_choque, anio_exposicion) %>%
  mutate(anio_base = anio_choque - 1) %>%
  arrange(anio_choque)

cat("\nAño de exposición y año base que usa cada año de choque (REZAGO =", REZAGO, "):\n")
ver(anios_expuestos, filas = 20)
guardar_tabla(anios_expuestos, "T00b_anios_por_choque",
              "Tabla 0b. Año de exposición y año base usados para cada año de choque",
              decimales = 0)

# --- VERIFICACIÓN BLOQUEANTE: Bite 2023 contra el 1,15% de 04_decision_medida.R --
# NOTA: `clave` en salto() termina con la etiqueta bonita, no la clave corta
# ("bite"), porque tibble() evalúa `medida = MEDIDAS[[medida]]` ANTES de
# `clave = medida` dentro de la misma llamada, y para ese punto `medida` ya es
# la columna reasignada, no el parámetro de la función. Es un detalle de
# construcción heredado de 10_celda_limpia_rodante.R / 12_celda_limpia_real.R,
# no algo que se corrija aquí -- se filtra por la etiqueta real.
bite_2023 <- limpia %>% filter(medida == "Bite (Kaitz de obreros)", anio_choque == 2023)
if (nrow(bite_2023) == 1) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("VERIFICACIÓN BLOQUEANTE -- Bite, celda limpia, choque 2023\n")
  cat("Este script:            ", round(bite_2023$efecto_pct, 3), "% (p = ",
      round(bite_2023$p_valor, 4), ", exposición ", bite_2023$anio_exposicion,
      ")\n", sep = "")
  cat("04_decision_medida.R:    1,15% (p = 0,008, exposición 2019)\n")
  diferencia_pp <- abs(bite_2023$efecto_pct - 1.15)
  if (diferencia_pp <= 0.5) {
    cat("-> Cerca del 1,15% esperado (diferencia de", round(diferencia_pp, 3),
        "puntos). Consistente con tratamiento de extremos distinto\n",
        "   (winsorización 1/99 aquí, filtro de plausibilidad > 1,3 allá).\n",
        "   Se continúa con el resto del script.\n")
  } else {
    cat("-> ALERTA: la diferencia (", round(diferencia_pp, 3), " puntos) es\n",
        "   mayor a lo que explica el tratamiento de extremos. Hay otra\n",
        "   diferencia de construcción sin localizar entre este script y\n",
        "   04_decision_medida.R. Se reporta aquí, TAL COMO SALIÓ, sin\n",
        "   ajustar la cifra para que coincida -- el resto del script sigue\n",
        "   corriendo, pero esta discrepancia debe resolverse antes de usar\n",
        "   estos resultados para decidir nada.\n")
  }
  cat(strrep("=", 78), "\n", sep = "")
} else {
  cat("\nALERTA: no se encontró la fila de Bite para el choque de 2023 -- no se\n",
      "pudo hacer la verificación bloqueante contra 04_decision_medida.R.\n")
}


# ==============================================================================
# 4. ¿QUÉ MEDIDA HACE QUE EL DIFERENCIAL ESCALE CON EL AUMENTO REAL?
# ==============================================================================
titulo("4. LA COMPARACIÓN QUE DECIDE -- POR GRUPOS DE AUMENTO REAL")

# 10_celda_limpia_rodante.R comparaba "2023 contra los años normales", que es
# la pregunta correcta para el placebo rodante ESTÁNDAR pero no aprovecha que
# ya sabemos (11_placebo_rodante_real.R) que el aumento real, no el nominal,
# es lo que hay que usar para ordenar los años. Aquí se reemplaza esa
# comparación por tres grupos según el aumento REAL del año de choque:
#
#   Alto        aumento real > +4%    (2023, 2024)
#   Intermedio  0% <= aumento real <= +4%   (2016, 2017, 2018, 2019)
#   Negativo    aumento real < 0%     (2015, 2022; 2021 queda fuera porque su
#                                       año base, 2020, es pandemia)
#
# LA REGLA DE LECTURA SE FIJA AQUÍ, ANTES DE VER EL RESULTADO:
#
#   - SI EL DIFERENCIAL ESCALA CON EL AUMENTO REAL EN ALGUNA MEDIDA -grupo alto
#     por encima del intermedio, e intermedio por encima del negativo- esa
#     especificación (celda limpia + esa medida) SÍ captura el choque. La
#     tesis sigue con 2023, la estimación principal pasa a ser la celda limpia
#     con esa medida (no necesariamente Bite), y este ejercicio es la
#     validación central del capítulo 4.
#
#   - SI NO ESCALA CON NINGUNA MEDIDA -los tres grupos parecidos, o el orden
#     invertido, en las cinco- el problema no es el traslape sino que ninguna
#     de estas cinco construcciones mide exposición al mínimo. La salida es
#     construir la exposición desde la distribución salarial de la firma
#     (proporción de trabajadores cerca del mínimo), verificando primero si la
#     EAM lo permite (hoy solo hay promedios por categoría ocupacional).
#
#   - CASO INTERMEDIO: si el grupo alto supera al negativo pero el orden
#     interno es ruidoso, es señal débil -- se reporta como tal, con la serie
#     completa a la vista.

comparacion_grupos_medida <- limpia %>%
  filter(!is.na(aumento_real_pct)) %>%
  mutate(
    grupo_aumento_real = case_when(
      aumento_real_pct > 4  ~ "1. Alto (>+4%)",
      aumento_real_pct >= 0 ~ "2. Intermedio (0% a +4%)",
      TRUE                  ~ "3. Negativo (<0%)"
    )
  ) %>%
  group_by(medida, clave, grupo_aumento_real) %>%
  summarise(
    anios = paste(sort(unique(anio_choque)), collapse = ", "),
    n_anios = n(),
    efecto_promedio_pct = mean(efecto_pct),
    efecto_min_pct = min(efecto_pct),
    efecto_max_pct = max(efecto_pct),
    .groups = "drop"
  ) %>%
  arrange(medida, grupo_aumento_real)

ver(comparacion_grupos_medida, filas = 20)
guardar_tabla(comparacion_grupos_medida, "T02_comparacion_grupos_por_medida",
              "Tabla 2. Diferencial promedio por grupo de aumento real, para cada medida (celda limpia)")

# --- Años individuales, no solo el promedio de grupo ------------------------------
# El grupo "alto" promedia 2023 y 2024: si esos dos años tienen patrones
# distintos (y la sección 7 sugiere que sí), el promedio no describe a
# ninguno. El grupo "negativo" tiene solo 2015 y 2022, y 2022 es atípico por
# razones ajenas al mínimo (salida de pandemia, retiro de subsidios, IPC de
# 13,12%): con dos observaciones y una rara, ese promedio tampoco soporta
# mucho peso. Se reportan los años uno por uno, no solo el agregado.
detalle_anios_grupo <- limpia %>%
  filter(!is.na(aumento_real_pct), anio_choque %in% c(2015, 2022, 2023, 2024)) %>%
  mutate(
    grupo_aumento_real = case_when(
      aumento_real_pct > 4  ~ "1. Alto (>+4%)",
      aumento_real_pct >= 0 ~ "2. Intermedio (0% a +4%)",
      TRUE                  ~ "3. Negativo (<0%)"
    )
  ) %>%
  select(medida, clave, grupo_aumento_real, anio_choque, anio_exposicion,
         aumento_real_pct, efecto_pct, p_valor, significancia) %>%
  arrange(medida, grupo_aumento_real, anio_choque)

ver(detalle_anios_grupo, filas = 25)
guardar_tabla(detalle_anios_grupo, "T02d_detalle_anios_alto_y_negativo",
              "Tabla 2d. Años individuales de los grupos alto (2023, 2024) y negativo (2015, 2022), sin promediar")

cat("\nCÓMO LEER LA TABLA 2d: si dentro del grupo 'alto' 2023 y 2024 se parecen\n",
    "entre sí, el promedio del grupo es representativo. Si difieren mucho (ver\n",
    "también la sensibilidad al rezago, sección 7), el promedio oculta esa\n",
    "diferencia y hay que leer los años por separado. Lo mismo aplica al grupo\n",
    "'negativo': si 2015 y 2022 se parecen, el promedio sirve; si 2022 es un\n",
    "valor atípico (por su contexto de pandemia/subsidios/inflación de\n",
    "13,12%), 2015 solo es la comparación más limpia de ese grupo.\n")

correlaciones_reales <- limpia %>%
  filter(!is.na(aumento_real_pct)) %>%
  group_by(medida, clave) %>%
  summarise(
    n_anios = n(),
    correlacion_aumento_real = cor(aumento_real_pct, efecto_pct, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  arrange(desc(correlacion_aumento_real))

ver(correlaciones_reales)
guardar_tabla(correlaciones_reales, "T02b_correlaciones_aumento_real",
              "Tabla 2b. Correlación entre el diferencial (celda limpia) y el aumento real, por medida")

cat("\n", strrep("-", 78), "\n", sep = "")
cat("LECTURA (la regla se fijó antes de correr esto, arriba en esta sección):\n\n")

veredicto_por_medida <- comparacion_grupos_medida %>%
  filter(grupo_aumento_real %in% c("1. Alto (>+4%)", "2. Intermedio (0% a +4%)", "3. Negativo (<0%)")) %>%
  select(medida, clave, grupo_aumento_real, efecto_promedio_pct) %>%
  pivot_wider(names_from = grupo_aumento_real, values_from = efecto_promedio_pct,
              names_prefix = "g_") %>%
  rename(alto = `g_1. Alto (>+4%)`, intermedio = `g_2. Intermedio (0% a +4%)`,
         negativo = `g_3. Negativo (<0%)`) %>%
  filter(!is.na(alto), !is.na(intermedio), !is.na(negativo)) %>%
  mutate(escala_completa = alto > intermedio & intermedio > negativo,
         alto_supera_negativo = alto > negativo)

ver(veredicto_por_medida)
guardar_tabla(veredicto_por_medida, "T02c_veredicto_por_medida",
              "Tabla 2c. Veredicto de escalamiento por medida (celda limpia, grupos de aumento real)")

ganadoras <- veredicto_por_medida %>% filter(escala_completa)
parciales <- veredicto_por_medida %>% filter(!escala_completa, alto_supera_negativo)

if (nrow(ganadoras) > 0) {
  cat("  ESCALA EN EL ORDEN ESPERADO (alto > intermedio > negativo) CON:\n")
  for (i in seq_len(nrow(ganadoras))) {
    cat("    -", ganadoras$medida[i], ": alto =", round(ganadoras$alto[i], 2),
        "% | intermedio =", round(ganadoras$intermedio[i], 2),
        "% | negativo =", round(ganadoras$negativo[i], 2), "%\n")
  }
  cat("\n  Con la celda limpia y los años ordenados por aumento real, el\n",
      "  diferencial SÍ escala con el tamaño del choque en la(s) medida(s) de\n",
      "  arriba. La tesis sigue con 2023, la estimación principal pasa a ser\n",
      "  la celda limpia con esa medida (cota inferior por atenuación), y esta\n",
      "  serie es la validación central del capítulo 4. Si la medida ganadora\n",
      "  no es Bite, hay que rehacer resultados de empleo y mecanismos con la\n",
      "  medida que gana.\n")
} else if (nrow(parciales) > 0) {
  cat("  SEÑAL PARCIAL (alto supera a negativo, pero el orden completo no es\n",
      "  monótono) CON:\n")
  for (i in seq_len(nrow(parciales))) {
    cat("    -", parciales$medida[i], ": alto =", round(parciales$alto[i], 2),
        "% | intermedio =", round(parciales$intermedio[i], 2),
        "% | negativo =", round(parciales$negativo[i], 2), "%\n")
  }
  cat("\n  Hay señal, no es limpia. Reportar la serie completa y declarar la\n",
      "  limitación explícitamente -- no forzar una lectura limpia que los\n",
      "  datos no dan.\n")
} else {
  cat("  NINGUNA MEDIDA escala con el aumento real, ni siquiera parcialmente\n",
      "  (alto no supera a negativo en ninguna). El problema no es el traslape\n",
      "  aritmético -la celda limpia ya lo corta- sino que ninguna de estas\n",
      "  cinco construcciones mide exposición al mínimo. La salida es construir\n",
      "  la exposición desde la distribución salarial de la firma, verificando\n",
      "  primero si la EAM lo permite (ver sección 8).\n")
}
cat(strrep("-", 78), "\n", sep = "")

g1 <- ggplot(limpia, aes(x = factor(anio_choque), y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.5) +
  facet_wrap(~ medida, scales = "free_y") +
  scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                     labels = c("Año normal", "2023 (choque)")) +
  labs(title = "Celda limpia: ¿se distingue 2023, con cada medida?",
       subtitle = paste0("Exposición medida ", REZAGO,
                         " años antes del año base del outcome"),
       x = NULL, y = "Diferencial (%)", color = NULL,
       caption = "Con la exposición rezagada, el salario del año base ya no está en los dos lados de la ecuación.") +
  tema_tesis +
  theme(axis.text.x = element_text(size = 8))
guardar_grafico(g1, "G01_celda_limpia_por_medida", ancho = 12, alto = 7)


# ==============================================================================
# 5. ESTÁNDAR CONTRA LIMPIA
# ==============================================================================
titulo("5. CUÁNTO DEL COEFICIENTE ERA CONTAMINACIÓN")

# Para cada medida, el coeficiente de 2023 con la especificación estándar
# (exposición del año base) y con la limpia (exposición rezagada). La razón
# entre los dos dice cuánto sobrevive al cortar el traslape.
#
# Cuidado al interpretar la razón: parte de la caída es atenuación legítima, no
# contaminación. Lo informativo es comparar la razón ENTRE medidas: la que más
# conserva es la menos contaminada.

estandar_2023 <- bind_rows(lapply(names(MEDIDAS), function(m)
  salto(m, 2022, 2022, 2023, "log_costo")))

comparacion <- estandar_2023 %>%
  select(medida, clave, efecto_estandar = efecto_pct, p_estandar = p_valor,
         n_estandar = firmas) %>%
  left_join(
    limpia %>% filter(es_choque) %>%
      select(clave, efecto_limpio = efecto_pct, p_limpio = p_valor,
             n_limpio = firmas),
    by = "clave"
  ) %>%
  mutate(
    pct_que_sobrevive = round(100 * efecto_limpio / efecto_estandar, 1),
    sig_estandar = estrellas(p_estandar),
    sig_limpio = estrellas(p_limpio)
  ) %>%
  arrange(desc(pct_que_sobrevive))

ver(comparacion)
guardar_tabla(comparacion, "T03_estandar_vs_limpia",
              "Tabla 3. Cuánto del coeficiente de 2023 sobrevive al cortar el traslape")

cat("\nCÓMO LEER: la columna 'pct_que_sobrevive' ordena las medidas de menos a\n",
    "más contaminada. La auditoría de la cadena ya había encontrado que Bite\n",
    "no es la que mejor resiste: golpe_c y golpe_a aguantan más. Si eso se\n",
    "confirma aquí, la elección original de medida fue subóptima, y la razón\n",
    "es que se eligió por la primera etapa estándar, que premia justamente la\n",
    "contaminación.\n")

g2 <- ggplot(comparacion, aes(x = reorder(medida, pct_que_sobrevive),
                              y = pct_que_sobrevive)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  geom_col(fill = "#1F4E79", width = 0.7) +
  geom_text(aes(label = paste0(pct_que_sobrevive, "%")), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "¿Cuánto del coeficiente sobrevive al cortar el traslape aritmético?",
       subtitle = "Coeficiente de la celda limpia como porcentaje del coeficiente estándar, choque de 2023",
       x = NULL, y = "% que sobrevive",
       caption = "Parte de la caída es atenuación legítima. Lo informativo es el orden entre medidas, no el nivel.") +
  tema_tesis
guardar_grafico(g2, "G02_cuanto_sobrevive")


# ==============================================================================
# 6. COMPRESIÓN SALARIAL CON LA CELDA LIMPIA
# ==============================================================================
titulo("6. COMPRESIÓN SALARIAL, CELDA LIMPIA")

# La compresión salarial falló el placebo rodante en la especificación estándar
# por una razón aritmética directa: w_obrero está en el denominador de Bite y
# en el numerador de la brecha. Con la exposición rezagada ese traslape se
# corta. El hallazgo puede sobrevivir aquí aunque no sobreviviera allá, y su
# diagnóstico es independiente del costo laboral.
#
# Nota: para golpe_c, golpe_a y golpe_costo el traslape con la brecha es más
# indirecto, porque su denominador mezcla categorías. Por eso vale la pena
# mirar las cinco.

limpia_brecha <- bind_rows(lapply(names(MEDIDAS), function(m) {
  bind_rows(lapply(ANIOS, function(a) {
    anio_base <- a - 1
    if (anio_base == 2020) return(NULL)
    anio_exp <- a - REZAGO  # rezago contado desde el año del choque -- ver sección 0
    if (anio_exp == 2020 || !(anio_exp %in% anios_panel)) return(NULL)
    salto(m, anio_exp, anio_base, a, "log_brecha")
  }))
})) %>%
  mutate(es_choque = anio_choque == 2023,
         ic_inf = efecto_pct - 1.96 * ee_pct,
         ic_sup = efecto_pct + 1.96 * ee_pct)

if (nrow(limpia_brecha) > 0) {
  
  veredicto_brecha <- limpia_brecha %>%
    group_by(medida, clave) %>%
    filter(sum(es_choque) == 1, n() >= 4) %>%
    summarise(
      efecto_2023 = efecto_pct[es_choque],
      p_2023 = p_valor[es_choque],
      promedio_normal = mean(efecto_pct[!es_choque]),
      maximo_normal = max(efecto_pct[!es_choque]),
      exceso = efecto_2023 - mean(efecto_pct[!es_choque]),
      supera_a_todos = efecto_2023 > max(efecto_pct[!es_choque]),
      .groups = "drop"
    ) %>%
    arrange(desc(exceso))
  
  ver(veredicto_brecha)
  guardar_tabla(veredicto_brecha, "T04_compresion_veredicto",
                "Tabla 4. Compresión salarial: 2023 frente a los años normales, por medida")
  
  ganadoras_brecha <- veredicto_brecha %>%
    filter(supera_a_todos, efecto_2023 > 0, p_2023 < 0.10)
  
  if (nrow(ganadoras_brecha) > 0) {
    cat("\n-> La compresión de 2023 supera a todos los años normales con:",
        paste(ganadoras_brecha$medida, collapse = ", "), "\n")
    cat("   El hallazgo se sostiene sobre esa medida y puede ser el titular.\n")
  } else {
    cat("\n-> La compresión de 2023 no supera a los años normales con ninguna\n",
        "   medida. Pasa a ser un resultado descriptivo, no causal.\n")
  }
  
  g3 <- ggplot(limpia_brecha, aes(x = factor(anio_choque), y = efecto_pct)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.5) +
    facet_wrap(~ medida, scales = "free_y") +
    scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                       labels = c("Año normal", "2023 (choque)")) +
    labs(title = "Compresión salarial con exposición rezagada",
         subtitle = "Diferencial de la brecha obrero/administrativo, sin el traslape aritmético",
         x = NULL, y = "Diferencial (%)", color = NULL,
         caption = "Si 2023 sobresale aquí, la compresión sí es atribuible al choque.") +
    tema_tesis +
    theme(axis.text.x = element_text(size = 8))
  guardar_grafico(g3, "G03_compresion_celda_limpia", ancho = 12, alto = 7)
}


# ==============================================================================
# 7. SENSIBILIDAD AL REZAGO
# ==============================================================================
titulo("7. SENSIBILIDAD AL REZAGO")

# El rezago se cuenta ahora desde el año del CHOQUE (ver la nota de la sección
# 0): anio_exp <- anio_choque_sens - r. Con esta indexación, rezago 1 -no
# rezago 0- es la especificación ESTÁNDAR (anio_exp = anio_base = anio_choque
# - 1, exposición y outcome comparten año base) y rezago 4 es la celda limpia
# de la tesis (anio_exp = 2019 para el choque de 2023). Rezago 0 es un caso
# nuevo, no probado en 12_celda_limpia_real.R: exposición medida en el MISMO
# año del choque (post-tratamiento), se incluye por completitud pero no tiene
# la lectura de "especificación estándar".
#
# Se prueban rezagos 0 a 5, con todas las medidas, para LOS DOS años de
# aumento real alto (2023 y 2024) -- no solo 2023 como en
# 10_celda_limpia_rodante.R. Si la forma de la caída es igual en los dos
# años, es estructura de la medida, no algo específico de 2023; si difiere,
# hay información en esa diferencia.
#
# Lo que se espera si hay efecto real: caída suave al aumentar el rezago, por
# atenuación creciente, que se APLANA en un valor positivo y significativo.
# Lo que indicaría contaminación pura: desplome hacia cero o hacia valores no
# significativos según crece el rezago, sin aplanarse.

sensibilidad_rezago <- function(anio_choque_sens) {
  anio_base_sens <- anio_choque_sens - 1
  bind_rows(lapply(names(MEDIDAS), function(m) {
    bind_rows(lapply(0:5, function(r) {
      anio_exp <- anio_choque_sens - r  # rezago contado desde el año del choque
      if (anio_exp == 2020) return(NULL)
      resultado <- salto(m, anio_exp, anio_base_sens, anio_choque_sens, "log_costo")
      if (is.null(resultado)) return(NULL)
      mutate(resultado, rezago = r)
    }))
  }))
}

sensibilidad_2023 <- sensibilidad_rezago(2023)
sensibilidad_2024 <- sensibilidad_rezago(2024)

sensibilidad <- bind_rows(sensibilidad_2023, sensibilidad_2024) %>%
  mutate(anio_eval = factor(anio_choque, levels = c(2023, 2024)))

# --- Tabla lado a lado, 2023 y 2024 en columnas paralelas -------------------------
sensibilidad_lado_a_lado <- sensibilidad_2023 %>%
  select(medida, rezago, anio_exposicion,
         efecto_2023 = efecto_pct, p_2023 = p_valor) %>%
  mutate(sig_2023 = estrellas(p_2023)) %>%
  full_join(
    sensibilidad_2024 %>%
      select(medida, rezago,
             efecto_2024 = efecto_pct, p_2024 = p_valor) %>%
      mutate(sig_2024 = estrellas(p_2024)),
    by = c("medida", "rezago")
  ) %>%
  arrange(medida, rezago)

ver(sensibilidad_lado_a_lado, filas = 40)
guardar_tabla(sensibilidad_lado_a_lado, "T05b_sensibilidad_lado_a_lado",
              "Tabla 5b. Rezago 0-5, 2023 y 2024 lado a lado, por medida", decimales = 3)

if (nrow(sensibilidad) > 0) {
  ver(sensibilidad, filas = 80)
  guardar_tabla(sensibilidad, "T05_sensibilidad_rezago_2023_2024",
                "Tabla 5. Diferencial de 2023 y 2024 según el rezago de la exposición, por medida")

  cat("\nCÓMO LEER: rezago 1 es la especificación estándar, rezago 4 es la celda\n",
      "limpia de la tesis. Una caída suave y monotónica que se APLANA en\n",
      "positivo (no que sigue cayendo) es atenuación normal, compatible con un\n",
      "efecto real. Un desplome sin aplanarse indica que el coeficiente estándar\n",
      "venía del traslape. Comparar la FORMA de la caída entre medidas, y\n",
      "comparar 2023 contra 2024 para la MISMA medida en la tabla 5b: si la\n",
      "forma es igual, es estructura de la medida; si difiere marcadamente, hay\n",
      "algo específico de uno de los dos años que vale la pena investigar\n",
      "aparte -- ver la sección 8 para las dos hipótesis a distinguir.\n")

  g4 <- ggplot(sensibilidad, aes(x = rezago, y = efecto_pct, color = medida)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 1, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 4, linetype = "dashed", color = "grey40") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    facet_wrap(~ anio_eval, ncol = 2) +
    scale_color_manual(values = PALETA) +
    scale_x_continuous(breaks = 0:5) +
    labs(title = "El diferencial según cuánto se rezague la exposición -- 2023 y 2024",
         subtitle = "Línea punteada = rezago 1 (especificación estándar). Línea discontinua = rezago 4 (celda limpia de la tesis). Los dos años son de aumento real alto (>+4%).",
         x = "Años de rezago de la exposición (contado desde el año del choque)", y = "Diferencial (%)", color = NULL,
         caption = "Caída que se aplana en positivo = señal real. Desplome sin aplanarse = traslape. Si 2023 y 2024 difieren en forma, investigar aparte.") +
    tema_tesis
  guardar_grafico(g4, "G04_sensibilidad_rezago_2023_2024", ancho = 12, alto = 6)
}


# ==============================================================================
# 7b. LA REGLA: ¿2023 Y 2024 MUESTRAN EL MISMO PATRÓN?
# ==============================================================================
titulo("7b. LECTURA DE LA SENSIBILIDAD -- LA REGLA SE FIJA ANTES DE VER EL RESULTADO")

# "Se aplana en positivo" se define operacionalmente como: en rezago 4 (celda
# limpia) Y rezago 5, el coeficiente es positivo Y significativo al 5%. Es la
# definición más exigente que separa "señal real que sobrevive a la
# atenuación" de "desplome hacia cero o hacia no-significativo".
aplanamiento <- sensibilidad_lado_a_lado %>%
  filter(rezago %in% c(4, 5)) %>%
  group_by(medida) %>%
  summarise(
    aplana_2023 = all(!is.na(efecto_2023) & efecto_2023 > 0 & p_2023 < 0.05),
    aplana_2024 = all(!is.na(efecto_2024) & efecto_2024 > 0 & p_2024 < 0.05),
    .groups = "drop"
  )

ver(aplanamiento)
guardar_tabla(aplanamiento, "T05c_aplanamiento_2023_2024",
              "Tabla 5c. ¿Se aplana en positivo y significativo en rezago 4-5? (celda limpia)", decimales = 0)

medidas_ambos <- aplanamiento %>% filter(aplana_2023, aplana_2024)
medidas_solo_2024 <- aplanamiento %>% filter(!aplana_2023, aplana_2024)
medidas_ninguno <- aplanamiento %>% filter(!aplana_2023, !aplana_2024)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("LECTURA ACTIVADA:\n\n")

if (nrow(medidas_ambos) > 0) {
  cat("  CASO 1 -- 2023 Y 2024 MUESTRAN EL MISMO PATRÓN (se aplanan en positivo\n",
      "  y significativo en rezago 4-5) CON:", paste(medidas_ambos$medida, collapse = ", "), "\n\n",
      "  Hay primera etapa válida. La estimación principal pasa a ser la celda\n",
      "  limpia con esa medida, y la especificación estándar queda descartada\n",
      "  por el traslape. Esto es lo que la tesis necesita.\n", sep = "")
} else if (nrow(medidas_solo_2024) > 0) {
  cat("  CASO 2 -- SOLO 2024 MUESTRA EL PATRÓN, 2023 NO, CON:",
      paste(medidas_solo_2024$medida, collapse = ", "), "\n\n",
      "  No se concluye nada todavía. Dos hipótesis a distinguir (ninguna\n",
      "  obvia de antemano):\n\n",
      "    a. EL AÑO BASE DE LA EXPOSICIÓN. Ver la Tabla 0b: para el choque de\n",
      "       2023 la celda limpia usa exposición de 2019; para 2024, de 2020\n",
      "       (excluido, se salta) o 2019 según cómo caiga el rezago exacto. Si\n",
      "       terminan usando bases muy distintas, 2023 y 2024 no son\n",
      "       comparables entre sí.\n",
      "    b. EL AUMENTO DE 2022 (+10,07% nominal) queda DENTRO del crecimiento\n",
      "       2022-2023 (la base del outcome de 2023), pero NO dentro del\n",
      "       crecimiento 2023-2024 (la base del outcome de 2024). Eso puede\n",
      "       estar ensuciando específicamente al choque de 2023 con un segundo\n",
      "       aumento grande mezclado en su propio outcome.\n\n",
      "  Ver la Tabla 0b (año de exposición y año base por año de choque) antes\n",
      "  de avanzar.\n", sep = "")
} else {
  cat("  CASO 3 -- NINGÚN AÑO MUESTRA EL PATRÓN, en ninguna medida.\n\n",
      "  Ninguna de las cinco medidas captura el choque ni siquiera sin\n",
      "  traslape (la celda limpia ya lo corta). La salida sería construir la\n",
      "  exposición desde la distribución salarial de la firma (proporción de\n",
      "  trabajadores cerca del mínimo), verificando primero si la EAM lo\n",
      "  permite -- hoy solo hay promedios por categoría ocupacional, así que es\n",
      "  posible que no se pueda con esta fuente.\n")
}
cat(strrep("=", 78), "\n", sep = "")

# --- Para el CASO 2, la comparación de bases que pide la hipótesis (a) -----------
if (nrow(medidas_solo_2024) > 0) {
  cat("\nAños de exposición y base usados por 2023 y 2024 (celda limpia, rezago 4):\n")
  bases_2023_2024 <- limpia %>%
    filter(anio_choque %in% c(2023, 2024)) %>%
    distinct(anio_choque, anio_exposicion) %>%
    mutate(anio_base = anio_choque - 1) %>%
    arrange(anio_choque)
  print(as.data.frame(bases_2023_2024), row.names = FALSE)
}


# ==============================================================================
# 8. QUÉ SIGUE
# ==============================================================================
titulo("8. QUÉ SIGUE")

cat("
SI ALGUNA MEDIDA HACE SOBRESALIR A 2023 EN LA CELDA LIMPIA
  La tesis se reorganiza alrededor de esa medida, y queda mejor sustentada que
  antes:

    - La estimación principal del primer eslabón es la de la celda limpia con
      esa medida, declarada como cota inferior por atenuación.
    - El capítulo 4 gana su prueba central: la serie completa de la tabla 1,
      que muestra que 2023 se separa de los años normales solo cuando se corta
      la contaminación aritmética.
    - El capítulo 6 gana su pieza más honesta: la especificación estándar
      produce un diferencial parejo todos los años, por eso no se usa, y la
      regla de decisión original seleccionaba por un criterio contaminado. Muy
      pocos trabajos con bite salarial reportan ese diagnóstico.
    - Si la medida ganadora no es Bite, hay que rehacer los resultados de
      empleo y mecanismos con la nueva medida. Es trabajo, pero es mecánico:
      los scripts ya están parametrizados por medida.

SI NINGUNA LO HACE
  El problema es de diseño. La salida defendible es cambiar la pregunta a una
  de medición: documentar que las medidas tipo Kaitz producen un primer
  eslabón espurio por reversión a la media, mostrar el diagnóstico que lo
  detecta, y proponer qué haría falta para construir una medida válida.

  Todo el trabajo hecho sirve para esa versión: el panel, la construcción de
  las cinco medidas, la comparación entre ellas, los dos placebos rodantes y
  la auditoría de la cadena son exactamente el material de esa tesis.

EN CUALQUIER ESCENARIO, DOS COSAS PENDIENTES (la tercera ya se cerró)
  1. Comitear 09_diagnostico_reversion.R, 10_celda_limpia_rodante.R y los dos
     scripts que los corrigen con aumentos reales (11 y 12, este). Hoy el
     diagnóstico que sustenta la decisión no está versionado en el
     repositorio real -- solo existe en la carpeta de pruebas.
  2. Escribir la regla de decisión que efectivamente se usó, con fecha, y
     reconocer que difiere de la de NOTA_DECISIONES.md.
  3. [RESUELTO por 11_placebo_rodante_real.R, sección 1 de este script]
     Documentar con cifras oficiales si el aumento REAL de 2023 fue atípico.
     Con el IPC del DANE: 2023 (+6,15%) y 2024 (+6,53%) son, por lejos, los
     años de mayor aumento real del panel; 2015, 2021 y 2022 tuvieron aumento
     real NEGATIVO. La premisa del diseño se sostiene. Lo que queda abierto
     (ver secciones 4 y 7 de este script) es si ALGUNA medida de exposición
     captura ese choque real una vez cortado el traslape aritmético.
")

if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

