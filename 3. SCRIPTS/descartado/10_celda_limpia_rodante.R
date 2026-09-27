# ==============================================================================
# 10_celda_limpia_rodante.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# LA PRUEBA QUE FALTA: CELDA LIMPIA, TODAS LAS MEDIDAS, TODOS LOS AÑOS
#
# DE DÓNDE VIENE ESTE SCRIPT
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
# la vez. Es la primera prueba que combina las dos cosas: sin contaminación en
# ningún año, y comparando 2023 contra el resto de la serie.
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
# ---------------------------------------------------------------------------
# SUPERADO -- movido a descartado/. Ver descartado/README.md para el detalle
# completo; en resumen:
#
# BUG DE INDEXACIÓN DEL REZAGO (encontrado después, nunca corregido en este
# script): más abajo, el rezago se cuenta desde el AÑO BASE del outcome, no
# desde el año del choque --
#
#     anio_base <- a - 1
#     anio_exp  <- anio_base - REZAGO
#
# Para el choque de 2023 (a = 2023) eso da anio_base = 2022 y, con REZAGO=4,
# anio_exp = 2018 -- NO 2019, que es la celda limpia real de la tesis
# (04_decision_medida.R: exposición 2019 contra crecimiento 2022-2023). El
# efecto medido con esa fila mal indexada (Bite ~0,14%, no significativo) no
# es la celda limpia de la tesis; la fila que en verdad la reproduce es la de
# REZAGO=3 con esta indexación vieja (~0,81%, significativo al 5%). La
# indexación correcta -anio_exp <- anio_choque - REZAGO, contada desde el año
# del choque- se aplicó primero en 12_celda_limpia_real.R (que la heredó de
# aquí SIN corregirla) y se corrigió finalmente en 13_celda_limpia_corregida.R.
# La versión vigente de esta prueba es la sección 2 de 09_validez_exposicion.R.
# ---------------------------------------------------------------------------
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  4. RESULTADOS/Celda_limpia_rodante/
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

CARPETA <- file.path("4. RESULTADOS", "Celda_limpia_rodante")
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

# Rezago entre el año de la exposición y el año base del outcome.
# 4 corresponde a la celda limpia de la tesis (exposición 2019, outcome
# 2022-2023). Se deja como constante para poder mover la sensibilidad.
REZAGO <- 4


# ==============================================================================
# 1. DATOS Y CONSTRUCCIÓN DE LAS MEDIDAS
# ==============================================================================
titulo("1. DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

SALARIO_MINIMO <- tibble(
  anio  = 2008:2024,
  valor = c(461500, 496900, 515000, 535600, 566700, 589500, 616000, 644350,
            689455, 737717, 781242, 828116, 877803, 908526, 1000000,
            1160000, 1300000)
) %>%
  mutate(aumento_nominal_pct = 100 * (valor / lag(valor) - 1))

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
    anio_exp <- anio_base - REZAGO
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

cat("\nVERIFICACIÓN: la fila de Bite en 2023 debería dar cerca de 1,15%, que es\n",
    "la cifra de 04_decision_medida.R. Si difiere, la causa más probable es el\n",
    "tratamiento de extremos: aquí se winsoriza 1/99 y allá se aplica un filtro\n",
    "de plausibilidad que manda a NA los valores por encima de 1,3. No es un\n",
    "error, pero hay que tenerlo en cuenta al comparar.\n")


# ==============================================================================
# 4. ¿QUÉ MEDIDA HACE SOBRESALIR A 2023?
# ==============================================================================
titulo("4. LA COMPARACIÓN QUE DECIDE")

veredicto <- limpia %>%
  group_by(medida, clave) %>%
  filter(sum(es_choque) == 1, n() >= 4) %>%
  summarise(
    anios_normales = sum(!es_choque),
    efecto_2023 = efecto_pct[es_choque],
    p_2023 = p_valor[es_choque],
    promedio_normal = mean(efecto_pct[!es_choque]),
    maximo_normal = max(efecto_pct[!es_choque]),
    exceso = efecto_2023 - mean(efecto_pct[!es_choque]),
    supera_a_todos = efecto_2023 > max(efecto_pct[!es_choque]),
    .groups = "drop"
  ) %>%
  mutate(significancia_2023 = estrellas(p_2023)) %>%
  arrange(desc(exceso))

ver(veredicto)
guardar_tabla(veredicto, "T02_veredicto_por_medida",
              "Tabla 2. 2023 frente a los años normales, por medida")

cat("\n", strrep("-", 78), "\n", sep = "")
cat("LECTURA (la regla se fijó antes de correr esto):\n\n")

ganadoras <- veredicto %>% filter(supera_a_todos, efecto_2023 > 0, p_2023 < 0.10)

if (nrow(ganadoras) > 0) {
  cat("  MEDIDAS EN LAS QUE 2023 SUPERA A TODOS LOS AÑOS NORMALES:\n")
  for (i in seq_len(nrow(ganadoras))) {
    cat("    -", ganadoras$medida[i], ":", round(ganadoras$efecto_2023[i], 3),
        "% en 2023 contra un máximo normal de",
        round(ganadoras$maximo_normal[i], 3), "%\n")
  }
  cat("\n  El primer eslabón se sostiene sobre esa especificación. La tesis se\n",
      "  reorganiza alrededor de la medida que gana, no de la que se eligió\n",
      "  originalmente. La estimación principal es la de la celda limpia,\n",
      "  declarada como cota inferior por atenuación, y esta serie completa es\n",
      "  la validación central del capítulo 4.\n")
} else {
  con_exceso <- veredicto %>% filter(exceso > 0, efecto_2023 > 0)
  if (nrow(con_exceso) > 0) {
    cat("  Ninguna medida hace que 2023 supere a TODOS los años normales, pero\n",
        "  estas lo ponen por encima del promedio:\n")
    for (i in seq_len(nrow(con_exceso))) {
      cat("    -", con_exceso$medida[i], ": exceso de",
          round(con_exceso$exceso[i], 3), "puntos\n")
    }
    cat("\n  Hay señal, no es limpia. La magnitud defendible es el EXCESO sobre\n",
        "  el promedio normal, no el coeficiente crudo. Se reporta la serie\n",
        "  completa y se declara que el diferencial de 2023 no es único.\n")
  } else {
    cat("  Ninguna medida hace sobresalir a 2023 en la celda limpia.\n\n",
        "  El problema es de diseño y no de la elección de medida. Ver la\n",
        "  sección 8.\n")
  }
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
    anio_exp <- anio_base - REZAGO
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

# El rezago de 4 años viene de que la celda limpia de la tesis usa 2019 para el
# choque de 2023. No tiene nada de sagrado. Aquí se prueban 0 a 5 para el
# choque de 2023, con todas las medidas.
#
# Rezago 0 es la especificación estándar (exposición y outcome comparten año
# base). Lo que se espera si hay efecto real: caída suave al aumentar el
# rezago, por atenuación creciente, pero el coeficiente se mantiene positivo.
# Lo que indicaría contaminación pura: desplome al primer rezago.

sensibilidad <- bind_rows(lapply(names(MEDIDAS), function(m) {
  bind_rows(lapply(0:5, function(r) {
    anio_exp <- 2022 - r
    if (anio_exp == 2020) return(NULL)
    resultado <- salto(m, anio_exp, 2022, 2023, "log_costo")
    if (is.null(resultado)) return(NULL)
    mutate(resultado, rezago = r)
  }))
}))

if (nrow(sensibilidad) > 0) {
  ver(sensibilidad, filas = 40)
  guardar_tabla(sensibilidad, "T05_sensibilidad_rezago",
                "Tabla 5. Diferencial de 2023 según el rezago de la exposición, por medida")
  
  cat("\nCÓMO LEER: una caída suave y monotónica es atenuación normal, compatible\n",
      "con un efecto real. Un desplome entre rezago 0 y rezago 1 indica que el\n",
      "coeficiente estándar venía del traslape. Comparar la FORMA de la caída\n",
      "entre medidas: la que se aplana en un valor positivo es la que tiene\n",
      "señal económica debajo.\n")
  
  g4 <- ggplot(sensibilidad, aes(x = rezago, y = efecto_pct, color = medida)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_color_manual(values = PALETA) +
    scale_x_continuous(breaks = 0:5) +
    labs(title = "El diferencial de 2023 según cuánto se rezague la exposición",
         subtitle = "Rezago 0 es la especificación estándar; 4 es la celda limpia de la tesis",
         x = "Años de rezago de la exposición", y = "Diferencial (%)", color = NULL,
         caption = "Caída suave que se aplana en positivo = señal real. Desplome al primer rezago = traslape.") +
    tema_tesis
  guardar_grafico(g4, "G04_sensibilidad_rezago")
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

EN CUALQUIER ESCENARIO, TRES COSAS PENDIENTES
  1. Comitear 09_diagnostico_reversion.R y este script. Hoy el diagnóstico que
     sustenta la decisión no está versionado.
  2. Escribir la regla de decisión que efectivamente se usó, con fecha, y
     reconocer que difiere de la de NOTA_DECISIONES.md.
  3. Documentar con cifras oficiales si el aumento REAL de 2023 fue atípico.
     Sigue pendiente desde agosto y es la premisa del diseño.
")

if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

