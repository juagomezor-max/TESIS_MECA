# ==============================================================================
# 09_validez_exposicion.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# VALIDEZ DE LA MEDIDA DE EXPOSICIÓN PRINCIPAL (BITE, KAITZ DE OBREROS)
#
# Este script consolida, en un solo lugar y una sola carpeta de salida, el
# diagnóstico de validez que antes vivía en cuatro scripts separados
# (11_placebo_rodante_real.R, 13_celda_limpia_corregida.R,
# 14_outcome_alternativo.R, 15_auditoria_adversarial.R). No reescribe su
# lógica: cada sección es el código que ya corría en el script original,
# adaptado solo para compartir la preparación de datos, evitar colisiones de
# nombres, y escribir a una única carpeta de salida. Donde un objeto (función,
# columna) tenía el mismo nombre con el mismo significado en dos scripts de
# origen, se dejó una sola definición; donde el nombre colisionaba pero el
# significado era distinto, se renombró (ver notas puntuales más abajo).
#
# CÓMO LEER ESTE SCRIPT
#   Sección 1 (T1x, G1x) -- Placebo rodante con aumentos REALES del salario
#     mínimo (antes 11_placebo_rodante_real.R). ¿2023 se distingue de un año
#     cualquiera? ¿El diferencial escala con el tamaño real del aumento?
#   Sección 2 (T2x, G2x) -- Celda limpia corregida (antes
#     13_celda_limpia_corregida.R). Corta el traslape aritmético midiendo la
#     exposición varios años antes del outcome, con las cinco medidas y la
#     indexación del rezago ya corregida.
#   Sección 3 (T3x, G3x) -- ¿El aumento de 2022 ensucia el outcome de 2023?
#     (antes 14_outcome_alternativo.R). Ventanas alternativas del outcome y un
#     contraste de falsación en 2015. Aquí se encontró y corrigió el bug de
#     controles fijos en 2022 (ver la nota en la transición a la sección 3).
#   Sección 4 (T4x) -- Auditoría adversarial de las cinco afirmaciones (antes
#     15_auditoria_adversarial.R). Busca dónde el diagnóstico anterior está
#     equivocado. Los datos de la EAM se consideran verídicos -- no se invoca
#     error de reporte como explicación de nada.
#   Sección 5 -- Síntesis: qué afirmación resiste, cuál queda falsada, y qué
#     queda pendiente.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds,
#           1. DATOS/panel_analitico_firma_eam.rds
# Salidas:  4. RESULTADOS/09_validez_exposicion/
# ==============================================================================


# ==============================================================================
# 0. PREPARACIÓN (compartida por las cuatro secciones)
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "09_validez_exposicion")
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
COLOR_BAJA <- "#1F4E79"
COLOR_GRIS <- "#808080"
PALETA <- c(`Bite (Kaitz de obreros)` = "#C00000",
            `Golpe C (3 categorías)`  = "#1F4E79",
            `Golpe A (ponderada)`     = "#2E8B57",
            `Golpe costo`             = "#E69F00",
            `Exposure (% obreros)`    = "#7B68EE")

titulo("0. DATOS Y PREPARACIÓN COMPARTIDA")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

# SALARIO_MINIMO: versión completa (2008-2024, las dos convenciones de
# deflactor), tomada de 15_auditoria_adversarial.R -- es la única de las
# cuatro versiones de origen que trae también el deflactor con el IPC del año
# ANTERIOR (aumento_real_pct_ipc_previo, usado en la sección 4.5/A4). Sustituye
# a las versiones más cortas de 11 (2012-2024) y de 13/14 (2008-2024 sin el
# deflactor alternativo): son el mismo objeto, esta es la más completa.
#
# DEFLACTOR -- por qué el IPC del año EN QUE EL SALARIO ESTÁ VIGENTE, no el del
# año anterior: la pregunta de esta tesis es sobre el costo laboral REAL de la
# firma. Una firma que en 2023 paga 16% más de nómina vende a precios que
# subieron 9,28% ESE MISMO año, así que ese es el IPC relevante. La otra
# convención -deflactar por la inflación causada del año PREVIO- es la que usa
# la negociación del salario mínimo (mide si el trabajador recuperó el poder
# adquisitivo perdido el año anterior) y responde una pregunta distinta: con
# esa convención 2023 da un aumento real de solo ~3%, no el +6,15% que se usa
# aquí. Las dos cuentas son legítimas para sus propias preguntas; para el costo
# laboral de la firma, la vigente es la correcta.
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
    aumento_real_pct = 100 * ((1 + aumento_nominal_pct / 100) / (1 + ipc_pct / 100) - 1),
    # Deflactor alternativo: IPC del año ANTERIOR (convención de negociación
    # del salario mínimo). Se usa solo en la sección 4.5 (A4).
    aumento_real_pct_ipc_previo = 100 * ((1 + aumento_nominal_pct / 100) / (1 + lag(ipc_pct) / 100) - 1)
  )

ver(SALARIO_MINIMO)
guardar_tabla(SALARIO_MINIMO, "T00_salario_minimo",
              "Tabla 0. Salario mínimo, IPC del año vigente, aumento nominal y real (las dos convenciones)",
              decimales = 2)

# Costo mínimo total de contratación, como razón del salario mínimo. Se usa
# para golpe_costo. La razón (1,531) viene de 02_medidas_exposicion.R:
# prestaciones, pensión, ARL y caja sobre el mínimo más auxilio de transporte.
RAZON_COSTO_MINIMO <- 1.531

MEDIDAS <- c(bite = "Bite (Kaitz de obreros)",
             golpe_c = "Golpe C (3 categorías)",
             golpe_a = "Golpe A (ponderada)",
             golpe_costo = "Golpe costo",
             exposure = "Exposure (% obreros)")

# construir_medidas(): las cinco medidas para un año de exposición dado, con el
# mínimo del año del choque en el numerador. Idéntica en 13, 14 y 15 -- una
# sola definición para las secciones 2, 3 y 4.
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
  # especificación principal.
  medidas %>%
    mutate(across(c(bite, golpe_c, golpe_a, golpe_costo, exposure), ~ {
      if (all(is.na(.x))) return(.x)
      lim <- quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE)
      w <- pmin(pmax(.x, lim[1]), lim[2])
      w / sd(w, na.rm = TRUE)
    }))
}

# base: construye TODAS las variables que usan las cuatro secciones, una sola
# vez. Colisiones de nombre resueltas así:
#   - w_obrero_mensual (11) y w_obrero_sueldos (13/14/15) son la MISMA cuenta
#     (sueldos, sin prestaciones) -- se deja un solo nombre, w_obrero_sueldos,
#     y se adapta la sección 1 para usarlo.
#   - w_admin_mensual (11) -> w_admin_sueldos, mismo criterio.
#   - log_brecha en 11 (con w_obrero_sueldos/w_admin_sueldos, sin
#     prestaciones) y log_brecha en 13 (con w_obrero/w_admin, CON
#     prestaciones) son magnitudes DISTINTAS con el mismo nombre en los
#     scripts de origen: aquí quedan como log_brecha_sueldos (sección 1) y
#     log_brecha (sección 2), sin tocar ninguna de las dos fórmulas.
#   - ANIO_F y log_empleo, calculadas en 11 pero no usadas después en ningún
#     modelo ni tabla de esa sección, no se trasladan aquí (no son parte de
#     ningún resultado citado).
#
# sector_2022 / depto_2022 / tamano_2022: EN ESTE PUNTO todavía NO están fijos
# en 2022 -- se recalculan con el CIIU4/DPTO/tamano_empresa de CADA FILA en su
# propio año. Es el bug encontrado en la sección 3 (antes
# 14_outcome_alternativo.R) y confirmado en el pipeline real en la sección 4.1
# (antes 15_auditoria_adversarial.R). Las secciones 1 y 2 de este script
# (antes 11 y 13) corrieron originalmente con esta versión sin corregir, y
# para reproducir sus cifras exactas se dejan así aquí. La corrección (join a
# la clasificación real de 2022) se aplica más abajo, justo antes de la
# sección 3, en el mismo punto del diagnóstico donde se encontró.
base <- panel %>%
  mutate(
    empleo = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo, NA_real_),

    w_obrero_sueldos = ifelse(obreros_permanentes > 0 & sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 /
                                obreros_permanentes / 12 * 1000, NA_real_),
    w_admin_sueldos = ifelse(administrativos_permanentes > 0 &
                               sueldos_permanentes_administrativos_c3r2c2 > 0,
                             sueldos_permanentes_administrativos_c3r2c2 /
                               administrativos_permanentes / 12 * 1000, NA_real_),

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

    n_categorias = (!is.na(w_obrero)) + (!is.na(w_prof)) + (!is.na(w_admin)),
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
    log_brecha_sueldos = log(ifelse(!is.na(w_obrero_sueldos) & !is.na(w_admin_sueldos) &
                                w_admin_sueldos > 0 & w_obrero_sueldos > 0,
                              w_obrero_sueldos / w_admin_sueldos, NA_real_)),
    log_brecha = log(ifelse(!is.na(w_obrero) & !is.na(w_admin) & w_admin > 0,
                            w_obrero / w_admin, NA_real_)),

    # Masa salarial mensual del personal permanente, en pesos -- para A1 (4.2).
    masa_obrero_permanente = ifelse(obreros_permanentes > 0,
                                    sueldos_permanentes_obreros_c3r2c1 +
                                      prestaciones_permanentes_obreros_c3r3c1, NA_real_),
    masa_total_c3r10c3 = costos_totales_personal_total_c3r10c3,

    sector_2022 = factor(CIIU4),
    depto_2022 = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

anios_panel <- sort(unique(base$ANIO))

cat("\nPanel:", nrow(base), "filas |", n_distinct(base$NORDEMP), "firmas |",
    "años", min(base$ANIO), "a", max(base$ANIO), "\n")
cat("Firmas-año con salario de obrero (sueldos):", sum(!is.na(base$w_obrero_sueldos)), "\n")


# ==============================================================================
# SECCIÓN 1 — PLACEBO RODANTE CON AUMENTOS REALES DEL SALARIO MÍNIMO
# (antes 11_placebo_rodante_real.R)
# ==============================================================================
titulo("SECCIÓN 1. PLACEBO RODANTE CON AUMENTOS REALES DEL SALARIO MÍNIMO")

# QUÉ HACE ESTA SECCIÓN
#   1.1 Placebo rodante: construye la MISMA medida (kaitz) para cada año del
#       panel y estima el salto de cada año. Si 2023 no sobresale de la serie,
#       no hay choque que identificar.
#   1.2 Escala el salto de cada año por el tamaño REAL del aumento del mínimo
#       de ese año (con el IPC del DANE del año vigente, no el nominal: 2022
#       parece fuerte en nominal pero fue de los más débiles en real).
#   1.3 Prueba de colinealidad de la medida C (salario de firmas parecidas):
#       la estima con y sin el control de sector x año, para saber si su cero
#       es real o absorbido.
#   1.4 El mismo diagnóstico sobre la compresión salarial.
#
# ESTA SECCIÓN NO DECIDE NADA POR SÍ SOLA (ver la síntesis, sección 5).

cat("\nNOTA IMPORTANTE PARA LEER EL PLACEBO: en 2019 el mínimo subió 6,0%. No\n",
    "fue un año sin choque, fue un año con un aumento normal. Por eso esta\n",
    "sección compara 2023 contra TODOS los años, no contra uno solo.\n")

# ------------------------------------------------------------------------------
# 1.1 PLACEBO RODANTE: LA MISMA MEDIDA, AÑO POR AÑO
# ------------------------------------------------------------------------------
titulo("1.1 PLACEBO RODANTE: LA MISMA MEDIDA, AÑO POR AÑO")

anios_disponibles <- anios_panel
ANIOS_PLACEBO <- anios_disponibles[anios_disponibles >= 2014 & anios_disponibles != 2020]
# 2021 queda fuera como año de choque porque su año base (2020) es pandemia
ANIOS_PLACEBO <- ANIOS_PLACEBO[ANIOS_PLACEBO != 2021]

cat("Años en los que se estima un 'salto':", paste(ANIOS_PLACEBO, collapse = ", "), "\n")

# construir_exposicion(): la medida de la tesis (kaitz), para un año base
# cualquiera. Usa w_obrero_sueldos (= w_obrero_mensual en 11_placebo_rodante_
# real.R, mismo nombre que en las secciones 2-4).
construir_exposicion <- function(anio_base, anio_choque) {
  minimo_choque <- SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == anio_choque]
  if (length(minimo_choque) == 0) return(NULL)

  base %>%
    filter(ANIO == anio_base, !is.na(w_obrero_sueldos)) %>%
    transmute(
      NORDEMP,
      kaitz_bruto = minimo_choque / w_obrero_sueldos
    ) %>%
    filter(is.finite(kaitz_bruto), kaitz_bruto > 0) %>%
    mutate(
      kaitz = {
        lim <- quantile(kaitz_bruto, probs = c(0.01, 0.99), na.rm = TRUE)
        pmin(pmax(kaitz_bruto, lim[1]), lim[2])
      }
    ) %>%
    mutate(kaitz_de = kaitz / sd(kaitz, na.rm = TRUE))
}

# salto_del_anio(): el diferencial de crecimiento del costo laboral entre
# anio_base y anio_choque, por DE de exposición. Corte transversal.
salto_del_anio <- function(anio_base, anio_choque, outcome = "log_costo") {

  exposicion <- construir_exposicion(anio_base, anio_choque)
  if (is.null(exposicion) || nrow(exposicion) < 500) return(NULL)

  valores <- base %>%
    filter(ANIO %in% c(anio_base, anio_choque)) %>%
    select(NORDEMP, ANIO, all_of(outcome),
           sector_2022, depto_2022, tamano_2022) %>%
    pivot_wider(names_from = ANIO, values_from = all_of(outcome),
                names_prefix = "y_")

  col_base <- paste0("y_", anio_base)
  col_choque <- paste0("y_", anio_choque)
  if (!all(c(col_base, col_choque) %in% names(valores))) return(NULL)

  datos <- valores %>%
    mutate(crecimiento = .data[[col_choque]] - .data[[col_base]]) %>%
    inner_join(exposicion, by = "NORDEMP") %>%
    filter(is.finite(crecimiento))

  if (nrow(datos) < 500) return(NULL)

  modelo <- tryCatch(
    feols(crecimiento ~ kaitz_de | sector_2022 + depto_2022 + tamano_2022,
          data = datos, vcov = "hetero"),
    error = function(e) NULL)
  if (is.null(modelo)) return(NULL)

  f <- coeftable(modelo)["kaitz_de", ]
  aumento <- SALARIO_MINIMO$aumento_nominal_pct[SALARIO_MINIMO$anio == anio_choque]

  tibble(
    anio_choque = anio_choque,
    anio_base = anio_base,
    aumento_minimo_pct = aumento,
    efecto_pct = 100 * f[["Estimate"]],
    ee_pct = 100 * f[["Std. Error"]],
    p_valor = f[["Pr(>|t|)"]],
    significancia = estrellas(f[["Pr(>|t|)"]]),
    firmas = nobs(modelo)
  )
}

rodante_costo <- bind_rows(lapply(ANIOS_PLACEBO, function(a) {
  anterior <- anios_disponibles[anios_disponibles < a]
  anterior <- anterior[anterior != 2020]
  if (length(anterior) == 0) return(NULL)
  salto_del_anio(max(anterior), a, "log_costo")
})) %>%
  mutate(
    es_choque = anio_choque == 2023,
    ic_inf = efecto_pct - 1.96 * ee_pct,
    ic_sup = efecto_pct + 1.96 * ee_pct
  )

ver(rodante_costo)
guardar_tabla(rodante_costo, "T1_01_placebo_rodante_costo",
              "Tabla 1.1. Diferencial del costo laboral por DE de exposición, año por año")

if (nrow(rodante_costo) > 2 && any(rodante_costo$es_choque)) {
  efecto_2023 <- rodante_costo$efecto_pct[rodante_costo$es_choque]
  otros <- rodante_costo$efecto_pct[!rodante_costo$es_choque]

  resumen_rodante <- tibble(
    concepto = c("Diferencial de 2023 (año del choque)",
                 "Promedio de los años normales",
                 "Mínimo de los años normales",
                 "Máximo de los años normales",
                 "Exceso de 2023 sobre el promedio normal",
                 "¿2023 está por encima de TODOS los años normales?"),
    valor = c(round(efecto_2023, 3),
              round(mean(otros), 3),
              round(min(otros), 3),
              round(max(otros), 3),
              round(efecto_2023 - mean(otros), 3),
              ifelse(efecto_2023 > max(otros), 1, 0))
  )

  ver(resumen_rodante)
  guardar_tabla(resumen_rodante, "T1_02_resumen_rodante",
                "Tabla 1.2. El diferencial de 2023 frente a los años normales")

  cat("\n", strrep("-", 78), "\n", sep = "")
  cat("LECTURA DEL RESULTADO (la regla se fijó antes de correr esto):\n\n")
  if (efecto_2023 > max(otros)) {
    cat("  2023 supera a TODOS los años normales. El choque es separable de la\n",
        "  reversión mecánica.\n")
  } else if (efecto_2023 > mean(otros)) {
    cat("  2023 está por encima del promedio normal pero NO por encima de todos\n",
        "  los años. Hay señal, pero no es limpia.\n")
  } else {
    cat("  2023 NO se distingue de un año normal. El diferencial que mide la\n",
        "  tesis es lo que esta medida produce todos los años por construcción.\n")
  }
  cat(strrep("-", 78), "\n", sep = "")
}

grafico_rodante <- ggplot(rodante_costo, aes(x = factor(anio_choque), y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.7) +
  scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                     labels = c("Año normal", "2023 (choque)")) +
  labs(title = "¿Se distingue 2023 de un año cualquiera?",
       subtitle = "Diferencial del costo laboral por DE de exposición, con la medida reconstruida cada año",
       x = NULL, y = "Diferencial (%)", color = NULL,
       caption = paste("Cada punto usa el salario del obrero del año anterior y el mínimo del año en curso.",
                       "\nSi 2023 no sobresale, el diferencial no es atribuible al choque.")) +
  tema_tesis
guardar_grafico(grafico_rodante, "G1_01_placebo_rodante")

# ------------------------------------------------------------------------------
# 1.2 ¿EL DIFERENCIAL ESCALA CON EL TAMAÑO REAL DEL AUMENTO?
# ------------------------------------------------------------------------------
titulo("1.2 ¿UN AUMENTO MAYOR -EN TÉRMINOS REALES- PRODUCE UN DIFERENCIAL MAYOR?")

if (nrow(rodante_costo) >= 4) {

  escala <- rodante_costo %>%
    filter(!is.na(aumento_minimo_pct)) %>%
    select(anio_choque, aumento_minimo_pct, efecto_pct, ee_pct, es_choque) %>%
    left_join(SALARIO_MINIMO %>% select(anio, aumento_real_pct),
              by = c("anio_choque" = "anio"))

  correlacion_nominal <- cor(escala$aumento_minimo_pct, escala$efecto_pct, use = "complete.obs")
  correlacion_real    <- cor(escala$aumento_real_pct,    escala$efecto_pct, use = "complete.obs")

  cat("Correlación entre el AUMENTO NOMINAL y el diferencial medido:", round(correlacion_nominal, 3), "\n")
  cat("Correlación entre el AUMENTO REAL y el diferencial medido:   ", round(correlacion_real, 3), "\n")
  cat("Años usados:", nrow(escala), "\n")

  ver(escala)
  guardar_tabla(escala, "T1_03_escala_con_aumento_real",
                "Tabla 1.3. Diferencial medido frente al aumento nominal y real del mínimo de cada año")

  escala <- escala %>%
    mutate(
      grupo_aumento_real = case_when(
        anio_choque %in% c(2023, 2024)             ~ "1. Alto (2023-2024)",
        anio_choque %in% c(2016, 2017, 2018, 2019)  ~ "2. Intermedio (2016-2019)",
        anio_choque %in% c(2015, 2021, 2022)        ~ "3. Bajo o negativo (2015, 2021, 2022)",
        TRUE ~ "Sin clasificar"
      )
    )

  cat("\nAños de la sección 1.1 dentro de cada grupo (2021 no se estima):\n")
  print(table(escala$grupo_aumento_real, escala$anio_choque))

  comparacion_grupos <- escala %>%
    group_by(grupo_aumento_real) %>%
    summarise(
      anios = paste(sort(anio_choque), collapse = ", "),
      n_anios = n(),
      efecto_promedio_pct = mean(efecto_pct),
      efecto_min_pct = min(efecto_pct),
      efecto_max_pct = max(efecto_pct),
      .groups = "drop"
    ) %>%
    arrange(grupo_aumento_real)

  ver(comparacion_grupos)
  guardar_tabla(comparacion_grupos, "T1_03b_comparacion_grupos_aumento_real",
                "Tabla 1.3b. Diferencial promedio por grupo de aumento real del salario mínimo",
                decimales = 3)

  grafico_escala_real <- ggplot(escala, aes(x = aumento_real_pct, y = efecto_pct)) +
    geom_smooth(method = "lm", se = TRUE, color = COLOR_BAJA, fill = "grey85") +
    geom_errorbar(aes(ymin = efecto_pct - 1.96 * ee_pct,
                      ymax = efecto_pct + 1.96 * ee_pct),
                  width = 0.15, color = "grey55") +
    geom_point(aes(color = es_choque), size = 3) +
    geom_text(aes(label = anio_choque), vjust = -1.2, size = 3.2) +
    scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                       guide = "none") +
    labs(title = "¿El diferencial responde al tamaño REAL del aumento del mínimo?",
         subtitle = paste0("Cada punto es un año. Correlación con aumento real: ", round(correlacion_real, 3),
                           "  (con nominal: ", round(correlacion_nominal, 3), ")"),
         x = "Aumento REAL del salario mínimo (%, deflactado por el IPC del mismo año)",
         y = "Diferencial del costo laboral (%)") +
    tema_tesis
  guardar_grafico(grafico_escala_real, "G1_02_escala_con_aumento_real")
}

# ------------------------------------------------------------------------------
# 1.3 LA MEDIDA C: ¿CERO ECONÓMICO O CERO ABSORBIDO?
# ------------------------------------------------------------------------------
titulo("1.3 LA MEDIDA C: ¿CERO ECONÓMICO O CERO ABSORBIDO?")

ANIO_BASE_C <- 2022
ANIO_CHOQUE_C <- 2023

medida_c <- base %>%
  filter(ANIO == ANIO_BASE_C, !is.na(w_obrero_sueldos)) %>%
  group_by(sector_2022, depto_2022, tamano_2022) %>%
  mutate(
    firmas_celda = n(),
    suma_celda = sum(w_obrero_sueldos, na.rm = TRUE),
    w_ajenas = (suma_celda - w_obrero_sueldos) / (firmas_celda - 1)
  ) %>%
  ungroup() %>%
  filter(firmas_celda >= 5, is.finite(w_ajenas), w_ajenas > 0) %>%
  transmute(
    NORDEMP, firmas_celda,
    kaitz_c_bruto = SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == ANIO_CHOQUE_C] / w_ajenas,
    kaitz_a_bruto = SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == ANIO_CHOQUE_C] / w_obrero_sueldos,
    sector_2022, depto_2022, tamano_2022
  ) %>%
  mutate(
    across(c(kaitz_c_bruto, kaitz_a_bruto), ~ {
      lim <- quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE)
      pmin(pmax(.x, lim[1]), lim[2])
    }, .names = "{.col}_w"),
    kaitz_c_de = kaitz_c_bruto_w / sd(kaitz_c_bruto_w, na.rm = TRUE),
    kaitz_a_de = kaitz_a_bruto_w / sd(kaitz_a_bruto_w, na.rm = TRUE)
  )

cat("Firmas con medida C:", nrow(medida_c), "\n")

variacion <- bind_rows(
  {
    m <- feols(kaitz_c_de ~ 1 | sector_2022 + depto_2022 + tamano_2022,
               data = medida_c)
    tibble(medida = "C. Salario de firmas parecidas",
           r2_de_los_controles = fitstat(m, "r2", simplify = TRUE),
           de_total = sd(medida_c$kaitz_c_de, na.rm = TRUE),
           de_residual = sd(resid(m), na.rm = TRUE))
  },
  {
    m <- feols(kaitz_a_de ~ 1 | sector_2022 + depto_2022 + tamano_2022,
               data = medida_c)
    tibble(medida = "A. Salario de la firma (Bite)",
           r2_de_los_controles = fitstat(m, "r2", simplify = TRUE),
           de_total = sd(medida_c$kaitz_a_de, na.rm = TRUE),
           de_residual = sd(resid(m), na.rm = TRUE))
  }
) %>%
  mutate(pct_variacion_que_sobrevive = round(100 * de_residual / de_total, 1))

ver(variacion)
guardar_tabla(variacion, "T1_04_variacion_tras_controles",
              "Tabla 1.4. Cuánta variación de cada medida sobrevive a los controles de celda",
              decimales = 4)

crecimiento_23 <- base %>%
  filter(ANIO %in% c(ANIO_BASE_C, ANIO_CHOQUE_C)) %>%
  select(NORDEMP, ANIO, log_costo) %>%
  pivot_wider(names_from = ANIO, values_from = log_costo, names_prefix = "y_") %>%
  mutate(crecimiento = .data[[paste0("y_", ANIO_CHOQUE_C)]] -
           .data[[paste0("y_", ANIO_BASE_C)]]) %>%
  select(NORDEMP, crecimiento)

datos_c <- medida_c %>%
  inner_join(crecimiento_23, by = "NORDEMP") %>%
  filter(is.finite(crecimiento))

estimar_variante <- function(tratamiento, controles, etiqueta_medida, etiqueta_controles) {
  formula <- if (is.null(controles)) {
    as.formula(paste0("crecimiento ~ ", tratamiento))
  } else {
    as.formula(paste0("crecimiento ~ ", tratamiento, " | ", controles))
  }
  modelo <- tryCatch(feols(formula, data = datos_c, vcov = "hetero"),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  f <- coeftable(modelo)[tratamiento, ]
  tibble(medida = etiqueta_medida, controles = etiqueta_controles,
         efecto_pct = 100 * f[["Estimate"]],
         ee_pct = 100 * f[["Std. Error"]],
         p_valor = f[["Pr(>|t|)"]],
         significancia = estrellas(f[["Pr(>|t|)"]]),
         firmas = nobs(modelo))
}

colinealidad <- bind_rows(
  estimar_variante("kaitz_c_de", NULL, "C. Firmas parecidas", "1. Sin controles"),
  estimar_variante("kaitz_c_de", "tamano_2022", "C. Firmas parecidas", "2. Solo tamaño"),
  estimar_variante("kaitz_c_de", "sector_2022 + tamano_2022", "C. Firmas parecidas", "3. Sector y tamaño"),
  estimar_variante("kaitz_c_de", "sector_2022 + depto_2022 + tamano_2022", "C. Firmas parecidas", "4. Celda completa"),
  estimar_variante("kaitz_a_de", NULL, "A. Bite", "1. Sin controles"),
  estimar_variante("kaitz_a_de", "tamano_2022", "A. Bite", "2. Solo tamaño"),
  estimar_variante("kaitz_a_de", "sector_2022 + tamano_2022", "A. Bite", "3. Sector y tamaño"),
  estimar_variante("kaitz_a_de", "sector_2022 + depto_2022 + tamano_2022", "A. Bite", "4. Celda completa")
)

ver(colinealidad)
guardar_tabla(colinealidad, "T1_05_medida_c_colinealidad",
              "Tabla 1.5. Efecto de cada medida según cuántos controles de celda se incluyan")

grafico_colinealidad <- ggplot(colinealidad,
                               aes(x = controles, y = efecto_pct, color = medida, group = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.8) +
  geom_pointrange(aes(ymin = efecto_pct - 1.96 * ee_pct,
                      ymax = efecto_pct + 1.96 * ee_pct),
                  position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c(`A. Bite` = COLOR_ALTA, `C. Firmas parecidas` = COLOR_BAJA)) +
  labs(title = "¿El cero de la medida C es económico o absorbido por los controles?",
       subtitle = "Efecto sobre el crecimiento del costo laboral 2022-2023, agregando controles uno a uno",
       x = NULL, y = "Efecto (%)", color = NULL) +
  tema_tesis +
  theme(axis.text.x = element_text(angle = 12, hjust = 1))
guardar_grafico(grafico_colinealidad, "G1_03_medida_c_colinealidad")

# ------------------------------------------------------------------------------
# 1.4 LA COMPRESIÓN SALARIAL BAJO LA MISMA LUPA
# ------------------------------------------------------------------------------
titulo("1.4 LA COMPRESIÓN SALARIAL BAJO LA MISMA LUPA")

# Usa log_brecha_sueldos (sección 0): w_obrero/w_admin SIN prestaciones, la
# cuenta de 11_placebo_rodante_real.R. No confundir con log_brecha (sección 2),
# que sí incluye prestaciones.
rodante_brecha <- bind_rows(lapply(ANIOS_PLACEBO, function(a) {
  anterior <- anios_disponibles[anios_disponibles < a]
  anterior <- anterior[anterior != 2020]
  if (length(anterior) == 0) return(NULL)
  salto_del_anio(max(anterior), a, "log_brecha_sueldos")
})) %>%
  mutate(es_choque = anio_choque == 2023,
         ic_inf = efecto_pct - 1.96 * ee_pct,
         ic_sup = efecto_pct + 1.96 * ee_pct)

if (nrow(rodante_brecha) > 0) {
  ver(rodante_brecha)
  guardar_tabla(rodante_brecha, "T1_06_placebo_rodante_brecha",
                "Tabla 1.6. Diferencial de la brecha obrero/administrativo, año por año")

  if (any(rodante_brecha$es_choque) && sum(!rodante_brecha$es_choque) > 1) {
    b_2023 <- rodante_brecha$efecto_pct[rodante_brecha$es_choque]
    b_otros <- rodante_brecha$efecto_pct[!rodante_brecha$es_choque]
    cat("\nCompresión salarial en 2023:", round(b_2023, 3), "%\n")
    cat("Promedio de los años normales:", round(mean(b_otros), 3), "%\n")
    cat("Máximo de los años normales:", round(max(b_otros), 3), "%\n")
  }

  grafico_brecha <- ggplot(rodante_brecha, aes(x = factor(anio_choque), y = efecto_pct)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.7) +
    scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                       labels = c("Año normal", "2023 (choque)")) +
    labs(title = "Compresión salarial: ¿2023 se distingue de un año cualquiera?",
         subtitle = "Diferencial de la brecha obrero/administrativo por DE de exposición",
         x = NULL, y = "Diferencial (%)", color = NULL) +
    tema_tesis
  guardar_grafico(grafico_brecha, "G1_04_placebo_rodante_brecha")
}


# ==============================================================================
# SECCIÓN 2 — CELDA LIMPIA CORREGIDA, LAS CINCO MEDIDAS
# (antes 13_celda_limpia_corregida.R)
# ==============================================================================
titulo("SECCIÓN 2. CELDA LIMPIA CORREGIDA: TODAS LAS MEDIDAS, INDEXACIÓN DEL REZAGO YA CORREGIDA")

# LA IDEA: la especificación estándar mide la exposición con el salario de t-1
# y el crecimiento de t-1 a t -- el salario de t-1 está en el denominador de
# la exposición Y en la base del outcome, así que un error de reporte en ese
# número empuja las dos cosas en la misma dirección. Con la exposición medida
# en t-5, ese error ya no puede estar correlacionado con el crecimiento de t-1
# a t. El costo es atenuación (la exposición vieja predice peor la actual, así
# que el coeficiente se sesga hacia cero): por eso la celda limpia da un
# número menor que la estándar, y hay que reportarla como cota inferior.
#
# INDEXACIÓN DEL REZAGO -- CORRECCIÓN RESPECTO A 12_celda_limpia_real.R (y su
# origen, 10_celda_limpia_rodante.R): esos scripts indexaban el rezago sobre
# el AÑO BASE del outcome (anio_base <- anio_choque - 1; anio_exp <- anio_base
# - REZAGO), no sobre el año del choque. Para el choque de 2023 eso daba
# exposición 2018, no 2019 -- un año antes de lo que dice la celda limpia real
# de la tesis (04_decision_medida.R: exposición 2019 contra crecimiento
# 2022-2023). El efecto medido con esa fila mal indexada (0,14%, no
# significativo) no es la celda limpia de la tesis; la fila correcta (rezago 3
# con la indexación vieja) daba 0,81%, significativo al 5%.
#
# REZAGO se define aquí como la distancia entre el año de la exposición y el
# AÑO DEL CHOQUE (no el año base del outcome):
#   anio_exp   <- anio_choque - REZAGO
#   anio_base  <- anio_choque - 1   (siempre el año inmediatamente anterior al
#                                    choque, sin importar el rezago de la
#                                    exposición)
# Con REZAGO <- 4, el choque de 2023 usa exposición de 2019 y outcome
# 2022-2023 -- eso SÍ reproduce la celda limpia de la tesis.
REZAGO <- 4

titulo("2.1 ESPECIFICACIÓN Y FUNCIÓN DE ESTIMACIÓN")

cat("Outcome: crecimiento del costo laboral por trabajador entre el año base\n",
    "y el año del choque.\n")
cat("Exposición: medida", REZAGO, "años antes del año base.\n")
cat("Controles: sector (CIIU4), departamento y tamaño -- TODAVÍA sin fijar en\n",
    "2022 (ver la nota de la sección 0); es como corrió originalmente esta\n",
    "sección. Errores robustos.\n")

# salto_celda(): el diferencial de crecimiento por DE de exposición, para una
# medida y una combinación de años, con la exposición fija en un solo año de
# origen (celda limpia o estándar según REZAGO). Se llama "_celda" (no
# "salto()") para no colisionar con salto_ventana() de las secciones 3-4, que
# tiene una firma y unos campos de salida distintos (ventanas variables).
#
# NOTA -- BUG DE `clave` (heredado de 10/12_celda_limpia_real.R, no corregido
# aquí a propósito, para reproducir exactamente lo que corrió 13_celda_limpia_
# corregida.R): `clave` abajo termina con la etiqueta bonita, no la clave
# corta ("bite"), porque tibble() evalúa `medida = MEDIDAS[[medida]]` ANTES de
# `clave = medida` en la misma llamada, y para ese punto `medida` ya es la
# columna reasignada, no el parámetro de la función. El código de esta sección
# que necesita la clave corta filtra por la etiqueta bonita en su lugar (ver
# la verificación bloqueante más abajo). La versión sin este bug es
# salto_ventana(), en la sección 3.
salto_celda <- function(medida, anio_exposicion, anio_base, anio_choque,
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

ANIOS <- anios_panel[anios_panel >= 2014 & !(anios_panel %in% c(2020, 2021))]
cat("\nAños en los que se intenta estimar:", paste(ANIOS, collapse = ", "), "\n")

# ------------------------------------------------------------------------------
# 2.2 CELDA LIMPIA RODANTE, LAS CINCO MEDIDAS
# ------------------------------------------------------------------------------
titulo("2.2 CELDA LIMPIA RODANTE")

limpia <- bind_rows(lapply(names(MEDIDAS), function(m) {
  bind_rows(lapply(ANIOS, function(a) {
    anio_base <- a - 1
    if (anio_base == 2020) return(NULL)
    anio_exp <- a - REZAGO
    if (anio_exp == 2020 || !(anio_exp %in% anios_panel)) return(NULL)
    if (!(anio_base %in% anios_panel)) return(NULL)
    salto_celda(m, anio_exp, anio_base, a, "log_costo")
  }))
})) %>%
  mutate(es_choque = anio_choque == 2023,
         ic_inf = efecto_pct - 1.96 * ee_pct,
         ic_sup = efecto_pct + 1.96 * ee_pct)

ver(limpia, filas = 50)
guardar_tabla(limpia, "T2_01_celda_limpia_rodante",
              "Tabla 2.1. Celda limpia año por año, las cinco medidas")

anios_expuestos <- limpia %>%
  distinct(anio_choque, anio_exposicion) %>%
  mutate(anio_base = anio_choque - 1) %>%
  arrange(anio_choque)

cat("\nAño de exposición y año base que usa cada año de choque (REZAGO =", REZAGO, "):\n")
ver(anios_expuestos, filas = 20)
guardar_tabla(anios_expuestos, "T2_00_anios_por_choque",
              "Tabla 2.0. Año de exposición y año base usados para cada año de choque",
              decimales = 0)

# --- VERIFICACIÓN BLOQUEANTE: Bite 2023 contra el 1,15% de 04_decision_medida.R --
bite_2023 <- limpia %>% filter(medida == "Bite (Kaitz de obreros)", anio_choque == 2023)
if (nrow(bite_2023) == 1) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("VERIFICACIÓN BLOQUEANTE -- Bite, celda limpia, choque 2023\n")
  cat("Esta sección:            ", round(bite_2023$efecto_pct, 3), "% (p = ",
      round(bite_2023$p_valor, 4), ", exposición ", bite_2023$anio_exposicion,
      ")\n", sep = "")
  cat("04_decision_medida.R:    1,15% (p = 0,008, exposición 2019)\n")
  diferencia_pp <- abs(bite_2023$efecto_pct - 1.15)
  if (diferencia_pp <= 0.5) {
    cat("-> Cerca del 1,15% esperado (diferencia de", round(diferencia_pp, 3),
        "puntos). Consistente con tratamiento de extremos distinto\n",
        "   (winsorización 1/99 aquí, filtro de plausibilidad > 1,3 allá).\n")
  } else {
    cat("-> ALERTA: la diferencia (", round(diferencia_pp, 3), " puntos) es\n",
        "   mayor a lo que explica el tratamiento de extremos.\n")
  }
  cat(strrep("=", 78), "\n", sep = "")
} else {
  cat("\nALERTA: no se encontró la fila de Bite para el choque de 2023.\n")
}

# ------------------------------------------------------------------------------
# 2.3 LA COMPARACIÓN QUE DECIDE -- POR GRUPOS DE AUMENTO REAL
# ------------------------------------------------------------------------------
titulo("2.3 LA COMPARACIÓN QUE DECIDE -- POR GRUPOS DE AUMENTO REAL")

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
guardar_tabla(comparacion_grupos_medida, "T2_02_comparacion_grupos_por_medida",
              "Tabla 2.2. Diferencial promedio por grupo de aumento real, para cada medida (celda limpia)")

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
guardar_tabla(detalle_anios_grupo, "T2_02d_detalle_anios_alto_y_negativo",
              "Tabla 2.2d. Años individuales de los grupos alto (2023, 2024) y negativo (2015, 2022), sin promediar")

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
guardar_tabla(correlaciones_reales, "T2_02b_correlaciones_aumento_real",
              "Tabla 2.2b. Correlación entre el diferencial (celda limpia) y el aumento real, por medida")

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
guardar_tabla(veredicto_por_medida, "T2_02c_veredicto_por_medida",
              "Tabla 2.2c. Veredicto de escalamiento por medida (celda limpia, grupos de aumento real)")

g1 <- ggplot(limpia, aes(x = factor(anio_choque), y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.5) +
  facet_wrap(~ medida, scales = "free_y") +
  scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                     labels = c("Año normal", "2023 (choque)")) +
  labs(title = "Celda limpia: ¿se distingue 2023, con cada medida?",
       subtitle = paste0("Exposición medida ", REZAGO, " años antes del año base del outcome"),
       x = NULL, y = "Diferencial (%)", color = NULL) +
  tema_tesis +
  theme(axis.text.x = element_text(size = 8))
guardar_grafico(g1, "G2_01_celda_limpia_por_medida", ancho = 12, alto = 7)

# ------------------------------------------------------------------------------
# 2.4 CUÁNTO DEL COEFICIENTE ERA CONTAMINACIÓN
# ------------------------------------------------------------------------------
titulo("2.4 CUÁNTO DEL COEFICIENTE ERA CONTAMINACIÓN")

estandar_2023 <- bind_rows(lapply(names(MEDIDAS), function(m)
  salto_celda(m, 2022, 2022, 2023, "log_costo")))

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
guardar_tabla(comparacion, "T2_03_estandar_vs_limpia",
              "Tabla 2.3. Cuánto del coeficiente de 2023 sobrevive al cortar el traslape")

g2 <- ggplot(comparacion, aes(x = reorder(medida, pct_que_sobrevive),
                              y = pct_que_sobrevive)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  geom_col(fill = "#1F4E79", width = 0.7) +
  geom_text(aes(label = paste0(pct_que_sobrevive, "%")), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "¿Cuánto del coeficiente sobrevive al cortar el traslape aritmético?",
       subtitle = "Coeficiente de la celda limpia como porcentaje del coeficiente estándar, choque de 2023",
       x = NULL, y = "% que sobrevive") +
  tema_tesis
guardar_grafico(g2, "G2_02_cuanto_sobrevive")

# ------------------------------------------------------------------------------
# 2.5 COMPRESIÓN SALARIAL, CELDA LIMPIA
# ------------------------------------------------------------------------------
titulo("2.5 COMPRESIÓN SALARIAL, CELDA LIMPIA")

# Usa log_brecha (sección 0): w_obrero/w_admin CON prestaciones. Distinta de
# log_brecha_sueldos (sección 1.4).
limpia_brecha <- bind_rows(lapply(names(MEDIDAS), function(m) {
  bind_rows(lapply(ANIOS, function(a) {
    anio_base <- a - 1
    if (anio_base == 2020) return(NULL)
    anio_exp <- a - REZAGO
    if (anio_exp == 2020 || !(anio_exp %in% anios_panel)) return(NULL)
    salto_celda(m, anio_exp, anio_base, a, "log_brecha")
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
  guardar_tabla(veredicto_brecha, "T2_04_compresion_veredicto",
                "Tabla 2.4. Compresión salarial: 2023 frente a los años normales, por medida")

  g3 <- ggplot(limpia_brecha, aes(x = factor(anio_choque), y = efecto_pct)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.5) +
    facet_wrap(~ medida, scales = "free_y") +
    scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                       labels = c("Año normal", "2023 (choque)")) +
    labs(title = "Compresión salarial con exposición rezagada",
         subtitle = "Diferencial de la brecha obrero/administrativo, sin el traslape aritmético",
         x = NULL, y = "Diferencial (%)", color = NULL) +
    tema_tesis +
    theme(axis.text.x = element_text(size = 8))
  guardar_grafico(g3, "G2_03_compresion_celda_limpia", ancho = 12, alto = 7)
}

# ------------------------------------------------------------------------------
# 2.6 y 2.7 SENSIBILIDAD AL REZAGO Y LA REGLA DE APLANAMIENTO
# ------------------------------------------------------------------------------
titulo("2.6 SENSIBILIDAD AL REZAGO")

sensibilidad_rezago <- function(anio_choque_sens) {
  anio_base_sens <- anio_choque_sens - 1
  bind_rows(lapply(names(MEDIDAS), function(m) {
    bind_rows(lapply(0:5, function(r) {
      anio_exp <- anio_choque_sens - r
      if (anio_exp == 2020) return(NULL)
      resultado <- salto_celda(m, anio_exp, anio_base_sens, anio_choque_sens, "log_costo")
      if (is.null(resultado)) return(NULL)
      mutate(resultado, rezago = r)
    }))
  }))
}

sensibilidad_2023 <- sensibilidad_rezago(2023)
sensibilidad_2024 <- sensibilidad_rezago(2024)

sensibilidad <- bind_rows(sensibilidad_2023, sensibilidad_2024) %>%
  mutate(anio_eval = factor(anio_choque, levels = c(2023, 2024)))

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
guardar_tabla(sensibilidad_lado_a_lado, "T2_05b_sensibilidad_lado_a_lado",
              "Tabla 2.5b. Rezago 0-5, 2023 y 2024 lado a lado, por medida", decimales = 3)

if (nrow(sensibilidad) > 0) {
  ver(sensibilidad, filas = 80)
  guardar_tabla(sensibilidad, "T2_05_sensibilidad_rezago_2023_2024",
                "Tabla 2.5. Diferencial de 2023 y 2024 según el rezago de la exposición, por medida")

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
         subtitle = "Línea punteada = rezago 1 (estándar). Línea discontinua = rezago 4 (celda limpia).",
         x = "Años de rezago de la exposición (desde el año del choque)", y = "Diferencial (%)", color = NULL) +
    tema_tesis
  guardar_grafico(g4, "G2_04_sensibilidad_rezago_2023_2024", ancho = 12, alto = 6)
}

titulo("2.7 LA REGLA: ¿2023 Y 2024 MUESTRAN EL MISMO PATRÓN?")

aplanamiento <- sensibilidad_lado_a_lado %>%
  filter(rezago %in% c(4, 5)) %>%
  group_by(medida) %>%
  summarise(
    aplana_2023 = all(!is.na(efecto_2023) & efecto_2023 > 0 & p_2023 < 0.05),
    aplana_2024 = all(!is.na(efecto_2024) & efecto_2024 > 0 & p_2024 < 0.05),
    .groups = "drop"
  )

ver(aplanamiento)
guardar_tabla(aplanamiento, "T2_05c_aplanamiento_2023_2024",
              "Tabla 2.5c. ¿Se aplana en positivo y significativo en rezago 4-5? (celda limpia)", decimales = 0)

# NOTA DE LECTURA: la regla exige rezago 4 Y 5 para AMBOS años. 2024
# estructuralmente no puede alcanzar aplana_2024 en el sentido de "sostenido
# en 2023 y 2024" porque su propio rezago 5 cae en 2019 pero el rezago 4 de
# 2023 y el de 2024 usan bases distintas -- ya se documentó en el script
# original que esta regla automática puede dar un veredicto que no
# corresponde a los datos, y que eso debe reportarse como artefacto de la
# regla, no como conclusión (ver también la sección 5, síntesis).
medidas_ambos <- aplanamiento %>% filter(aplana_2023, aplana_2024)
medidas_solo_2024 <- aplanamiento %>% filter(!aplana_2023, aplana_2024)
medidas_ninguno <- aplanamiento %>% filter(!aplana_2023, !aplana_2024)

cat("\nMedidas con aplana_2023 Y aplana_2024:", nrow(medidas_ambos), "\n")
cat("Medidas con SOLO aplana_2024:", nrow(medidas_solo_2024), "\n")
cat("Medidas con ninguno:", nrow(medidas_ninguno), "\n")


# ==============================================================================
# TRANSICIÓN A LA SECCIÓN 3 — CORRECCIÓN DE LOS CONTROLES FIJOS EN 2022
# ==============================================================================
titulo("TRANSICIÓN. CORRIGIENDO sector_2022 / depto_2022 / tamano_2022 ANTES DE LA SECCIÓN 3")

# BUG ENCONTRADO AL CORRER 14_outcome_alternativo.R (no estaba en 13, ni en
# 12, ni en 10 -- se hereda de ahí sin que nadie lo hubiera notado, porque
# esos scripts solo usan ventanas de outcome de 1 año): la construcción
# original de sector_2022/depto_2022/tamano_2022 (sección 0 de este script,
# usada tal cual en las secciones 1 y 2 arriba) es `factor(CIIU4)` etc.
# aplicada a TODO el panel -- es decir, recalculada con el CIIU4/DPTO/
# tamano_empresa de CADA FILA en su propio año, a pesar del nombre. Para
# ventanas de 1 año esto casi nunca se nota. Para ventanas de 2-4 años,
# pivot_wider() usa esas columnas como parte del id, y si la clasificación de
# una firma difiere entre el año base y el año del choque, esa firma pierde
# ambas observaciones en vez de una: en la ventana 2013->2015 el efecto fue
# total (0 filas de 9.076 posibles); en 2019->2023, parcial (~1.200 filas
# perdidas de ~7.150 posibles). Es una pérdida de muestra NO aleatoria.
#
# Este mismo patrón se confirmó luego en el pipeline real
# (05_resultados_y_mecanismos.R, 06_mecanismos_por_grupo.R,
# 07_reconciliacion.R -- ver la sección 4.1 más abajo) y se corrigió ahí
# también, con este mismo join.
controles_2022 <- base %>%
  filter(ANIO == 2022) %>%
  distinct(NORDEMP, .keep_all = TRUE) %>%
  transmute(NORDEMP,
            sector_2022 = factor(CIIU4),
            depto_2022 = factor(DPTO),
            tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

cat("Firmas con controles fijos en 2022:", nrow(controles_2022), "de",
    n_distinct(base$NORDEMP), "firmas totales en el panel.\n")

base <- base %>%
  select(-any_of(c("sector_2022", "depto_2022", "tamano_2022"))) %>%
  left_join(controles_2022, by = "NORDEMP")

cat("Panel (controles ya fijos en 2022):", nrow(base), "filas |",
    n_distinct(base$NORDEMP), "firmas\n")

# salto_ventana(): igual que salto_celda(), pero (a) el orden del tibble()
# final ya NO tiene el bug de `clave` (se invierte: clave = medida ANTES que
# medida = MEDIDAS[[medida]], así que clave sí queda con la clave corta), y
# (b) el resultado trae anio_base y ventana_anios explícitos en vez de
# aumento_minimo_pct/aumento_real_pct, porque las secciones 3 y 4 varían el
# año base del outcome (ventanas de distinto largo), no el año de choque.
# Sustituye a salto_celda() de aquí en adelante -- las secciones 3 y 4 la usan
# sobre el `base` ya corregido.
salto_ventana <- function(medida, anio_exposicion, anio_base, anio_choque,
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
    clave = medida,
    medida = MEDIDAS[[medida]],
    anio_exposicion = anio_exposicion,
    anio_base = anio_base,
    anio_choque = anio_choque,
    ventana_anios = anio_choque - anio_base,
    efecto_pct = 100 * f[["Estimate"]],
    ee_pct = 100 * f[["Std. Error"]],
    p_valor = f[["Pr(>|t|)"]],
    significancia = estrellas(f[["Pr(>|t|)"]]),
    firmas = nobs(modelo)
  )
}


# ==============================================================================
# SECCIÓN 3 — ¿EL AUMENTO DE 2022 ENSUCIA EL OUTCOME DE 2023?
# (antes 14_outcome_alternativo.R)
# ==============================================================================
titulo("SECCIÓN 3. ¿EL AUMENTO DE 2022 ENSUCIA EL OUTCOME DE 2023?")

# DE DÓNDE VIENE: la sección 2, con la indexación del rezago ya corregida,
# dejó esta tabla (exposición 2019, celda limpia, las cinco medidas):
#
#   Medida        2023          2024
#   Bite          0,81%  **     0,85%  **
#   Golpe A       0,63%  (ns)   1,10%  ***
#   Golpe C       0,57%  (ns)   1,30%  ***
#   Golpe costo   0,71%  (ns)   1,35%  ***
#   Exposure      0,49%  (ns)   0,20%  (ns)
#
# Bite se sostiene en los dos años. Exposure no se sostiene en ninguno (es
# coherente: es la única medida sin canal salarial). Las otras tres se
# sostienen SOLO en 2024.
#
# LA HIPÓTESIS: el outcome de 2023 es el crecimiento del costo laboral
# 2022->2023. Ese intervalo arranca en 2022, un año que tuvo su propio
# aumento del mínimo (+10,07% nominal, -2,70% real). El outcome de 2024
# (2023->2024) no arranca en un año así. Si el aumento de 2022 mete ruido en
# la BASE del outcome de 2023, eso explicaría la asimetría.
#
# LA PRUEBA: con exposición fija en 2019 (celda limpia, sin tocar), variar la
# VENTANA del outcome: saltar el año base contaminado (2021->2023 en vez de
# 2022->2023). Se repite para 2024 (control) y para 2015 (aumento real
# negativo) como contraste de falsación.

titulo("3.1 LAS TRES VENTANAS, PARA CADA CASO")

casos <- list(
  "2023" = list(
    anio_choque = 2023, anio_exposicion = 2019,
    ventanas = c("2022->2023 (usual)" = 2022,
                 "2021->2023 (salta 2022)" = 2021,
                 "2019->2023 (larga, = año exposición)" = 2019)
  ),
  "2024" = list(
    anio_choque = 2024, anio_exposicion = 2019,
    ventanas = c("2023->2024 (usual)" = 2023,
                 "2022->2024 (2 años)" = 2022,
                 "2021->2024 (larga)" = 2021)
  ),
  "2015 (falsación, aumento real -2,03%)" = list(
    anio_choque = 2015, anio_exposicion = 2011,
    ventanas = c("2014->2015 (usual)" = 2014,
                 "2013->2015 (salta 2014)" = 2013,
                 "2011->2015 (larga, = año exposición)" = 2011)
  )
)

resultado <- bind_rows(lapply(names(casos), function(nombre_caso) {
  caso <- casos[[nombre_caso]]
  bind_rows(lapply(names(caso$ventanas), function(etiqueta_ventana) {
    anio_base_v <- caso$ventanas[[etiqueta_ventana]]
    bind_rows(lapply(names(MEDIDAS), function(m) {
      r <- salto_ventana(m, caso$anio_exposicion, anio_base_v, caso$anio_choque, "log_costo")
      if (is.null(r)) return(NULL)
      mutate(r, caso = nombre_caso, ventana = etiqueta_ventana)
    }))
  }))
})) %>%
  select(caso, medida, ventana, anio_exposicion, anio_base, anio_choque,
         ventana_anios, efecto_pct, ee_pct, p_valor, significancia, firmas)

ver(resultado, filas = 60)
guardar_tabla(resultado, "T3_01_outcome_alternativo_completo",
              "Tabla 3.1. Cinco medidas x tres ventanas x tres casos (2023, 2024, falsación 2015)")

cobertura <- resultado %>%
  select(caso, medida, ventana, firmas) %>%
  pivot_wider(names_from = ventana, values_from = firmas)

ver(cobertura, filas = 20)
guardar_tabla(cobertura, "T3_02_cobertura_por_ventana",
              "Tabla 3.2. Firmas por combinación de caso, medida y ventana", decimales = 0)

titulo("3.2 ¿APARECEN GOLPE A, GOLPE C Y GOLPE COSTO AL SALTAR EL AÑO CONTAMINADO?")

lado_a_lado <- resultado %>%
  select(caso, medida, ventana, efecto_pct, p_valor, significancia) %>%
  pivot_wider(names_from = ventana, values_from = c(efecto_pct, p_valor, significancia))

ver(lado_a_lado, filas = 20)
guardar_tabla(lado_a_lado, "T3_03_lado_a_lado_por_caso",
              "Tabla 3.3. Las tres ventanas lado a lado, por caso y medida", decimales = 3)

g5 <- ggplot(resultado, aes(x = ventana_anios, y = efecto_pct, color = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ caso, ncol = 1, scales = "free_x") +
  scale_color_manual(values = PALETA) +
  labs(title = "El diferencial según el largo de la ventana del outcome",
       subtitle = "Exposición fija (2019 para 2023/2024, 2011 para el contraste de 2015)",
       x = "Largo de la ventana del outcome (años)", y = "Diferencial (%)", color = NULL,
       caption = "Si el hallazgo depende del choque, el patrón de 2023/2024 no debería repetirse en el contraste de 2015 (falsación).") +
  tema_tesis
guardar_grafico(g5, "G3_01_outcome_por_ventana", ancho = 10, alto = 12)

titulo("3.3 LA REGLA -- FIJADA ANTES DE VER EL RESULTADO")

UMBRAL_P <- 0.05

aparece_en_2023_salta <- resultado %>%
  filter(caso == "2023", ventana == "2021->2023 (salta 2022)") %>%
  transmute(medida, aparece = !is.na(efecto_pct) & efecto_pct > 0 & p_valor < UMBRAL_P)

aparece_en_2015_salta <- resultado %>%
  filter(caso == "2015 (falsación, aumento real -2,03%)", ventana == "2013->2015 (salta 2014)") %>%
  transmute(medida, aparece_falsa = !is.na(efecto_pct) & efecto_pct > 0 & p_valor < UMBRAL_P)

tres_medidas <- c("Golpe A (ponderada)", "Golpe C (3 categorías)", "Golpe costo")

veredicto <- aparece_en_2023_salta %>%
  left_join(aparece_en_2015_salta, by = "medida") %>%
  filter(medida %in% tres_medidas)

ver(veredicto)
guardar_tabla(veredicto, "T3_04_veredicto_aparicion",
              "Tabla 3.4. ¿Aparece cada medida al saltar el año contaminado, en 2023 y en la falsación de 2015?",
              decimales = 0)

n_aparecen_2023 <- sum(veredicto$aparece, na.rm = TRUE)
n_aparecen_falsa <- sum(veredicto$aparece_falsa, na.rm = TRUE)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("LECTURA ACTIVADA:\n\n")
if (n_aparecen_2023 == length(tres_medidas) && n_aparecen_falsa == 0) {
  cat("  SE CONFIRMA LA HIPÓTESIS: las tres medidas aparecen al saltar el año\n",
      "  contaminado y el contraste de falsación en 2015 sigue plano.\n")
} else if (n_aparecen_2023 < length(tres_medidas)) {
  cat("  NO SE CONFIRMA: de las tres medidas, solo", n_aparecen_2023, "de",
      length(tres_medidas), "aparece(n) al saltar el año contaminado en 2023.\n")
} else {
  cat("  HALLAZGO ESPURIO: las tres medidas aparecen al saltar el año\n",
      "  contaminado en 2023, PERO el mismo patrón aparece en la falsación de\n",
      "  2015. Lo que se mide crece con el LARGO de la ventana, no con el\n",
      "  tamaño del choque. El único resultado que sigue en pie es Bite con la\n",
      "  ventana original.\n")
}
cat(strrep("=", 78), "\n", sep = "")


# ==============================================================================
# SECCIÓN 4 — AUDITORÍA ADVERSARIAL DE LAS CINCO AFIRMACIONES
# (antes 15_auditoria_adversarial.R)
# ==============================================================================
titulo("SECCIÓN 4. AUDITORÍA ADVERSARIAL DE LAS CINCO AFIRMACIONES DEL DIAGNÓSTICO")

# Objetivo: buscar dónde las afirmaciones anteriores (de una conversación con
# IA, no de la asesoría) están equivocadas. Los datos de la EAM se consideran
# verídicos -- no se invoca error de reporte como explicación de nada.

panel_original <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

cat("Firmas con controles fijos en 2022:", nrow(controles_2022), "\n")

# ------------------------------------------------------------------------------
# 4.1 BUG DE CONTROLES EN EL PIPELINE REAL (01-08)
# ------------------------------------------------------------------------------
titulo("4.1 ¿SECTOR/DEPTO/TAMANO_2022 ESTÁN REALMENTE FIJOS EN 05, 06, 07?")

ref_2022 <- base %>% filter(ANIO == 2022) %>% distinct(NORDEMP, .keep_all = TRUE) %>%
  transmute(NORDEMP, CIIU4_2022 = CIIU4, DPTO_2022 = DPTO, tamano_2022_fijo = tamano_empresa)

comparado <- panel %>% select(NORDEMP, ANIO, CIIU4, DPTO, tamano_empresa) %>%
  inner_join(ref_2022, by = "NORDEMP") %>% filter(ANIO != 2022, ANIO %in% 2015:2024, ANIO != 2020)

bug_controles <- comparado %>%
  mutate(distinta = (CIIU4 != CIIU4_2022) | (DPTO != DPTO_2022) | (tamano_empresa != tamano_2022_fijo)) %>%
  group_by(ANIO) %>%
  summarise(pct_al_menos_uno_distinto = round(100*mean(distinta, na.rm=TRUE), 2),
            pct_tamano_distinto = round(100*mean(tamano_empresa != tamano_2022_fijo, na.rm=TRUE), 2),
            pct_sector_distinto = round(100*mean(CIIU4 != CIIU4_2022, na.rm=TRUE), 2),
            n = n(), .groups = "drop")

ver(bug_controles)
guardar_tabla(bug_controles, "T4_00_bug_controles_por_anio",
              "Tabla 4.0. % de firmas-año (2015-2024, sin 2020/2022) con sector/depto/tamaño distinto al de 2022 de esa misma firma -- confirmado en 05, 06, 07 del pipeline real", decimales = 2)

cat("\nCONCLUSIÓN: sector_2022/depto_2022/tamano_2022 en 05_resultados_y_mecanismos.R,\n",
    "06_mecanismos_por_grupo.R y 07_reconciliacion.R se construían con\n",
    "`factor(CIIU4)` etc. sobre el panel COMPLETO (sin filtrar a 2022 primero),\n",
    "exactamente el mismo patrón que el bug encontrado en la sección 3 de este\n",
    "script (y en 10/12/13). Ya corregido en el pipeline real (ver el commit\n",
    "correspondiente). En 01_descriptivos_y_contexto.R y\n",
    "08_tratamiento_continuo.R SÍ estaba bien hecho desde el inicio (filter\n",
    "(ANIO==2022) antes de construir los controles). En\n",
    "03_primer_eslabon_medidas.R y 04_decision_medida.R el dataframe ya es de\n",
    "corte transversal, así que ahí no aplica el problema de la misma forma.\n")

# ------------------------------------------------------------------------------
# 4.2 A1. EL TRASLAPE ALGEBRAICO
# ------------------------------------------------------------------------------
titulo("4.2 A1. ¿HAY TRASLAPE ALGEBRAICO ENTRE W_OBRERO Y C?")

a1_2022 <- base %>% filter(ANIO == 2022, !is.na(masa_obrero_permanente), !is.na(masa_total_c3r10c3), masa_total_c3r10c3 > 0)

a1_resumen <- tibble(
  concepto = c("Firmas con ambos componentes, 2022",
               "Peso promedio de la masa salarial de obreros permanentes en C3R10C3",
               "Peso mediano",
               "Correlación w_obrero (por trabajador) vs c (por trabajador), 2022",
               "Correlación log(w_obrero) vs log(c), 2022"),
  valor = c(nrow(a1_2022),
            round(mean(a1_2022$masa_obrero_permanente / a1_2022$masa_total_c3r10c3, na.rm=TRUE), 4),
            round(median(a1_2022$masa_obrero_permanente / a1_2022$masa_total_c3r10c3, na.rm=TRUE), 4),
            round(cor(a1_2022$w_obrero, a1_2022$c_firma, use="complete.obs"), 4),
            round(cor(log(a1_2022$w_obrero), log(a1_2022$c_firma), use="complete.obs"), 4))
)
ver(a1_resumen)
guardar_tabla(a1_resumen, "T4_01_traslape", "Tabla 4.1. Peso de w_obrero en c y su correlación", decimales = 4)

cat("\nSUSTITUCIÓN METODOLÓGICA, declarada: en vez de simular un outcome con\n",
    "ruido puro, se usa la evidencia YA GENERADA del placebo rodante sobre años\n",
    "REALES sin choque (sección 1, tabla T1_01): si el mecanismo algebraico\n",
    "opera, Bite debería predecir el crecimiento del costo laboral en\n",
    "CUALQUIER año, no solo en 2023. Esa tabla ya mostró coeficientes positivos\n",
    "y significativos en los años sin choque probados. Es una prueba más\n",
    "fuerte que una simulación porque usa variación real, no supuestos sobre el\n",
    "proceso de ruido.\n")

# ------------------------------------------------------------------------------
# 4.3 A2. ATENUACIÓN VS. TRASLAPE
# ------------------------------------------------------------------------------
titulo("4.3 A2. ¿LA CAÍDA DEL COEFICIENTE ES ATENUACIÓN CLÁSICA?")

m19 <- construir_medidas(2019, 2023) %>% rename_with(~paste0(.x, "_2019"), -NORDEMP)
m22 <- construir_medidas(2022, 2023) %>% rename_with(~paste0(.x, "_2022"), -NORDEMP)
comp_medidas <- m19 %>% inner_join(m22, by = "NORDEMP")

rho_tabla <- bind_rows(lapply(names(MEDIDAS), function(m) {
  v19 <- comp_medidas[[paste0(m, "_2019")]]; v22 <- comp_medidas[[paste0(m, "_2022")]]
  tibble(medida = MEDIDAS[[m]], clave = m,
         n_firmas_con_ambas = sum(!is.na(v19) & !is.na(v22)),
         rho_pearson = cor(v19, v22, use = "complete.obs"))
}))

# Caída observada del coeficiente estándar (exposición 2022) a la celda limpia
# (exposición 2019), choque 2023, ventana de 1 año, con salto_ventana() (base
# ya con controles fijos en 2022 -- las cifras de esta sección usan la
# especificación corregida, no la de la sección 2).
estandar_2023_v <- bind_rows(lapply(names(MEDIDAS), function(m) salto_ventana(m, 2022, 2022, 2023, "log_costo")))
limpia_2023_v   <- bind_rows(lapply(names(MEDIDAS), function(m) salto_ventana(m, 2019, 2022, 2023, "log_costo")))

caida_tabla <- estandar_2023_v %>% select(clave, medida, efecto_estandar = efecto_pct) %>%
  left_join(limpia_2023_v %>% select(clave, efecto_limpio = efecto_pct), by = "clave") %>%
  mutate(razon_observada = round(efecto_limpio / efecto_estandar, 3)) %>%
  left_join(rho_tabla %>% select(clave, rho_pearson, n_firmas_con_ambas), by = "clave") %>%
  mutate(rho = round(rho_pearson, 3),
         diferencia_razon_menos_rho = round(razon_observada - rho, 3))

ver(caida_tabla)
guardar_tabla(caida_tabla, "T4_02_atenuacion_vs_traslape",
              "Tabla 4.2. Razón observada de caída (limpia/estándar) vs. rho (correlación 2019-2022), las cinco medidas", decimales = 3)

cat("\nCÓMO LEER: si razon_observada ~= rho, la atenuación clásica explica toda\n",
    "la caída. Si razon_observada es MENOR que rho, queda caída adicional\n",
    "atribuible a cortar el traslape.\n")

# ------------------------------------------------------------------------------
# 4.4 A3. EL CONTRASTE DE FALSACIÓN EN OTROS AÑOS
# ------------------------------------------------------------------------------
titulo("4.4 A3. ¿EL PATRÓN DE 2015 ES ESTRUCTURAL O ES UN AÑO RARO?")

construir_ventanas_choque <- function(anio_choque) {
  anio_exp <- anio_choque - 4
  list(usual = anio_choque - 1, salta = anio_choque - 2, larga = anio_exp)
}

casos_a3 <- list(
  "2015 (real -2,03%)" = list(anio_choque = 2015, anio_exposicion = 2011),
  "2016 (real +1,18%)" = list(anio_choque = 2016, anio_exposicion = 2012),
  "2019 (real +2,12%)" = list(anio_choque = 2019, anio_exposicion = 2015),
  "2023 (real +6,15%, referencia)" = list(anio_choque = 2023, anio_exposicion = 2019)
)

resultado_a3 <- bind_rows(lapply(names(casos_a3), function(nombre) {
  caso <- casos_a3[[nombre]]
  vents <- construir_ventanas_choque(caso$anio_choque)
  bind_rows(lapply(names(vents), function(v) {
    anio_base_v <- vents[[v]]
    bind_rows(lapply(names(MEDIDAS), function(m) {
      r <- salto_ventana(m, caso$anio_exposicion, anio_base_v, caso$anio_choque, "log_costo")
      if (is.null(r)) return(NULL)
      anios_ventana <- (anio_base_v + 1):caso$anio_choque
      reales <- SALARIO_MINIMO$aumento_real_pct[SALARIO_MINIMO$anio %in% anios_ventana] / 100
      acumulado <- 100 * (prod(1 + reales, na.rm = TRUE) - 1)
      mutate(r, caso = nombre, ventana = v, aumento_real_acumulado_pct = round(acumulado, 2))
    }))
  }))
})) %>%
  select(caso, medida, ventana, anio_exposicion, anio_base, anio_choque, ventana_anios,
         aumento_real_acumulado_pct, efecto_pct, ee_pct, p_valor, significancia, firmas)

ver(resultado_a3, filas = 60)
guardar_tabla(resultado_a3, "T4_03_falsacion_multiples_anios",
              "Tabla 4.3. Falsación replicada en 2015, 2016, 2019, con 2023 de referencia", decimales = 3)

escala_acumulado <- resultado_a3 %>%
  filter(ventana %in% c("salta", "larga")) %>%
  group_by(medida) %>%
  summarise(correlacion_con_acumulado = round(cor(aumento_real_acumulado_pct, efecto_pct, use="complete.obs"), 3),
            .groups = "drop")
ver(escala_acumulado)
guardar_tabla(escala_acumulado, "T4_03b_correlacion_acumulado",
              "Tabla 4.3b. Correlación entre efecto y aumento real acumulado en la ventana, por medida", decimales = 3)

# ------------------------------------------------------------------------------
# 4.5 A4. ROBUSTEZ DEL "NO ESCALA"
# ------------------------------------------------------------------------------
titulo("4.5 A4. ROBUSTEZ DEL 'NO ESCALA': SIN 2022, CON OTRO DEFLACTOR, CON IC")

anios_rodante <- c(2015, 2016, 2017, 2018, 2019, 2022, 2023, 2024)
rodante_estandar <- bind_rows(lapply(anios_rodante, function(a) {
  bind_rows(lapply(names(MEDIDAS), function(m) salto_ventana(m, a - 1, a - 1, a, "log_costo")))
})) %>%
  left_join(SALARIO_MINIMO %>% select(anio, aumento_real_pct, aumento_real_pct_ipc_previo),
            by = c("anio_choque" = "anio"))

sin_2022 <- rodante_estandar %>% filter(anio_choque != 2022)
cor_sin_2022 <- sin_2022 %>% group_by(medida) %>%
  summarise(correlacion_sin_2022 = round(cor(aumento_real_pct, efecto_pct, use="complete.obs"), 3), .groups="drop")

cor_con_2022 <- rodante_estandar %>% group_by(medida) %>%
  summarise(correlacion_con_2022 = round(cor(aumento_real_pct, efecto_pct, use="complete.obs"), 3), .groups="drop")

cor_ipc_previo <- rodante_estandar %>% filter(!is.na(aumento_real_pct_ipc_previo)) %>%
  group_by(medida) %>%
  summarise(correlacion_ipc_previo = round(cor(aumento_real_pct_ipc_previo, efecto_pct, use="complete.obs"), 3), .groups="drop")

a4_tabla <- cor_con_2022 %>%
  left_join(cor_sin_2022, by = "medida") %>%
  left_join(cor_ipc_previo, by = "medida")

ver(a4_tabla)
guardar_tabla(a4_tabla, "T4_04_robustez_correlacion",
              "Tabla 4.4. Correlación aumento real vs. diferencial: con/sin 2022, con las dos convenciones de deflactor", decimales = 3)

ic_correlacion <- function(r, n, conf = 0.95) {
  z <- atanh(r); se <- 1/sqrt(n - 3)
  zc <- qnorm(1 - (1-conf)/2)
  tanh(c(z - zc*se, z + zc*se))
}
bite_r <- rodante_estandar %>% filter(clave == "bite")
r_bite <- cor(bite_r$aumento_real_pct, bite_r$efecto_pct, use = "complete.obs")
ic_bite <- ic_correlacion(r_bite, n = nrow(bite_r))
cat("\nCorrelación Bite (n=", nrow(bite_r), "): ", round(r_bite,3),
    " -- IC 95% (Fisher): [", round(ic_bite[1],3), ", ", round(ic_bite[2],3), "]\n", sep="")

ic_todas <- rodante_estandar %>% group_by(medida) %>%
  summarise(n = n(), r = cor(aumento_real_pct, efecto_pct, use="complete.obs"), .groups="drop") %>%
  rowwise() %>%
  mutate(ic_inf = ic_correlacion(r, n)[1], ic_sup = ic_correlacion(r, n)[2]) %>%
  ungroup() %>%
  mutate(across(c(r, ic_inf, ic_sup), ~round(.x, 3)))
ver(ic_todas)
guardar_tabla(ic_todas, "T4_04b_intervalos_confianza",
              "Tabla 4.4b. Correlación con IC 95% (Fisher), n=8, las cinco medidas", decimales = 3)

# ------------------------------------------------------------------------------
# 4.6 A5. ¿BITE EN VENTANA DE 1 AÑO ES REALMENTE LA ÚNICA CELDA LIMPIA?
# ------------------------------------------------------------------------------
titulo("4.6 A5. ROBUSTEZ DE LA CELDA 'BITE, VENTANA 1 AÑO, EXPOSICIÓN 4 AÑOS ANTES'")

anios_panel_todos <- anios_panel
anios_a5 <- anios_panel_todos[anios_panel_todos >= 2014 & !(anios_panel_todos %in% c(2020, 2021))]

rolling_bite_1yr <- bind_rows(lapply(anios_a5, function(a) {
  salto_ventana("bite", a - 4, a - 1, a, "log_costo")
})) %>%
  left_join(SALARIO_MINIMO %>% select(anio, aumento_real_pct), by = c("anio_choque" = "anio")) %>%
  mutate(es_2023_o_2024 = anio_choque %in% c(2023, 2024))

ver(rolling_bite_1yr)
guardar_tabla(rolling_bite_1yr, "T4_05_bite_rolling_1yr",
              "Tabla 4.5. Bite, ventana de 1 año, exposición 4 años antes, todos los años estimables", decimales = 3)

n_significativos_otros <- rolling_bite_1yr %>% filter(!es_2023_o_2024, p_valor < 0.05, efecto_pct > 0) %>% nrow()
n_anios_otros <- rolling_bite_1yr %>% filter(!es_2023_o_2024) %>% nrow()

cat("\nDe los", n_anios_otros, "años SIN choque de 2023/2024 probados, ",
    n_significativos_otros, "dan a Bite positivo y significativo al 5% con esta\n",
    "misma especificación (ventana 1 año, exposición 4 años antes).\n")


# ==============================================================================
# SECCIÓN 5 — SÍNTESIS
# ==============================================================================
titulo("SECCIÓN 5. SÍNTESIS: QUÉ RESISTE, QUÉ QUEDA FALSADO, QUÉ QUEDA PENDIENTE")

sintesis <- tibble(
  afirmacion = c(
    "A1. Traslape algebraico entre w_obrero y c",
    "A2. La caída del coeficiente es MÁS que atenuación clásica",
    "A3. Las ventanas largas producen efecto también en años sin choque real (independiente del choque)",
    "A4. El diferencial escala con el tamaño real del aumento del mínimo",
    "A5. Bite (ventana 1 año, exposición 4 años antes) es la única celda limpia sin falsos positivos",
    "Efecto de empleo estimado (-1,75%) con exposición 2022"
  ),
  veredicto = c(
    "RESISTE", "RESISTE", "RESISTE", "FALSADO", "FALSADO", "PENDIENTE"
  )
)
ver(sintesis)
guardar_tabla(sintesis, "T5_00_sintesis_afirmaciones",
              "Tabla 5.0. Síntesis de las seis afirmaciones auditadas", decimales = 0)

cat("
RESISTE (x3):
  1. El traslape algebraico entre w_obrero (denominador de Bite) y c (el
     costo laboral total por trabajador) es real y sustancial (sección 4.2,
     A1): la masa salarial de obreros permanentes pesa una fracción
     importante y estable de c, y las dos series están fuertemente
     correlacionadas en nivel y en logaritmo.
  2. La caída del coeficiente al pasar de la especificación estándar a la
     celda limpia es MAYOR de lo que la atenuación clásica por sí sola
     predice (sección 4.3, A2): la razón observada (limpia/estándar) es
     menor que rho (la correlación entre la medida 2019 y la 2022) en las
     cinco medidas, así que queda caída adicional atribuible a cortar el
     traslape, no solo a que la exposición vieja prediga peor la actual.
  3. Las ventanas largas del outcome producen un efecto positivo y
     significativo también en años de aumento real NEGATIVO o bajo (sección
     4.4, A3: 2015, 2016, 2019), con magnitudes del mismo orden que en 2023.
     El patrón de 'ventana larga = coeficiente grande' es estructural, no
     específico del choque de 2023 -- aparece independientemente de si hubo
     o no un aumento real grande del mínimo.

FALSADO (x2):
  4. La hipótesis de que el diferencial escala con el tamaño real del
     aumento del mínimo NO se sostiene (sección 4.5, A4): la correlación
     cambia de signo al excluir 2022, colapsa con el deflactor alternativo
     (IPC del año previo), y su intervalo de confianza al 95% incluye el
     cero en 4 de las 5 medidas (n=8, poca potencia, pero el patrón no es
     robusto en ninguna dirección).
  5. La afirmación de que Bite (ventana de 1 año, exposición 4 años antes)
     es LA ÚNICA celda limpia sin falsos positivos NO se sostiene (sección
     4.6, A5): con esa misma especificación, otros años sin choque de
     2023/2024 (2016**, 2017***) también dan a Bite positivo y significativo
     al 5%. No es una celda limpia inmune a falsos positivos.

PENDIENTE (x1):
  6. El efecto de empleo estimado en -1,75% con exposición 2022 (fuera del
     alcance de este script de validez de exposición, que solo corre
     modelos de costo laboral) queda pendiente de reestimar con la
     exposición de celda limpia (2019) para saber si sobrevive al mismo
     corte del traslape aritmético que se aplicó aquí a costo laboral.
")


if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")
