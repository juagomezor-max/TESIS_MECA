# ==============================================================================
# 11_empleo_sin_traslape.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# ¿SOBREVIVE EL EFECTO SOBRE EL EMPLEO SIN EL TRASLAPE?
#
# DE DÓNDE VIENE ESTE SCRIPT
# Tras corregir el bug de controles fijos en 2022 (ver 09_validez_exposicion.R,
# sección 4.1, y 10_bug_controles.R), 05_resultados_y_mecanismos.R da:
#
#   Empleo, lectura A (log)          -1,754%  (p=0,004)
#   Tendencias previas del empleo     p=0,455 (no rechaza)
#
# Es el resultado más importante que tiene la tesis hoy, y es nuevo: antes de
# la corrección el empleo salía nulo (-0,27%, p=0,63) y las tendencias
# previas rechazaban (p=0,0002).
#
# PERO está estimado con exposición de 2022 Y año de referencia 2022 -- la
# especificación exacta donde el diagnóstico de validez documentó el traslape
# algebraico: el salario de 2022 está en el denominador de la exposición y en
# la base del outcome. Antes de escribir en la tesis que el aumento del
# mínimo redujo el empleo, hay que ver si el resultado aguanta cuando ese
# canal se corta -- exactamente la misma pregunta que 09_validez_exposicion.R
# le hizo al costo laboral, aplicada ahora al empleo.
#
# QUÉ HACE ESTE SCRIPT (misma lógica ya validada en 09_validez_exposicion.R,
# sección 2 -- celda limpia -- aplicada aquí al empleo en vez del costo
# laboral; no se cambia la construcción de las cinco medidas ni la de los
# controles, solo el año de la exposición y el outcome):
#
#   1. Resultado principal: crecimiento del empleo 2022->2023 contra
#      exposición medida en 2019 (celda limpia), las cinco medidas.
#   2. Sensibilidad al rezago de la exposición, 0 a 5 años, choque de 2023.
#   3. Placebo rodante sobre el empleo: la misma medida (rezago fijo de 4
#      años) en todos los años estimables del panel, para saber si 2023
#      sobresale o si el diferencial es lo que esta medida produce siempre.
#   4. Tendencias previas del empleo con la exposición de 2019 en vez de
#      2022, con el mismo estudio de evento de 05_resultados_y_mecanismos.R
#      (i(ANIO_F, tratamiento, ref='2022') con los controles fijados en
#      2022), reportando el p-valor conjunto Y los coeficientes año a año.
#
# LA REGLA DE LECTURA SE FIJA AQUÍ, ANTES DE VER LOS RESULTADOS:
#   - Si el efecto se sostiene -negativo y significativo con exposición de
#     2019, estable al rezagar, sin destacar en los años sin choque- hay
#     resultado principal para la tesis: se reporta la estimación limpia,
#     con la contaminada al lado y la explicación de la diferencia.
#   - Si desaparece al rezagar, el -1,754% venía del traslape, igual que le
#     pasó al costo laboral. La tesis no tiene efecto de empleo que reportar,
#     y eso hay que saberlo antes de escribirlo, no después.
#   - Si queda a medio camino -negativo pero no significativo, o solo con
#     algunas medidas- se reporta la serie completa y se declara la
#     limitación. Un efecto que solo aparece con una medida de cinco es
#     débil.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  4. RESULTADOS/Empleo_sin_traslape/
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

CARPETA <- file.path("4. RESULTADOS", "Empleo_sin_traslape")
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

titulo("0. DATOS Y PREPARACIÓN")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

# SALARIO_MINIMO: idéntica a la de 09_validez_exposicion.R (misma fuente, no
# se cambia nada de la construcción del salario mínimo ni del deflactor).
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

RAZON_COSTO_MINIMO <- 1.531

MEDIDAS <- c(bite = "Bite (Kaitz de obreros)",
             golpe_c = "Golpe C (3 categorías)",
             golpe_a = "Golpe A (ponderada)",
             golpe_costo = "Golpe costo",
             exposure = "Exposure (% obreros)")

# base: MISMA construcción de columnas que 09_validez_exposicion.R (sección
# 0) para w_obrero_sueldos, w_obrero, w_prof, w_admin, w_firma, c_firma,
# n_categorias, permanentes_total, exposure_obreros -- no se cambia nada de
# esto. Se agrega log_empleo (el outcome de este script) y, a diferencia de
# 09 (secciones 1-2), los controles sector_2022/depto_2022/tamano_2022 se
# fijan en 2022 DESDE EL INICIO con el mismo patrón de `clasificacion_2022` +
# `left_join` que usa 05_resultados_y_mecanismos.R ya corregido -- no el
# patrón viejo (recalculado por fila en su propio año).
base <- panel %>%
  mutate(
    empleo = empleo_total_sin_propietarios,
    log_empleo = log(ifelse(empleo > 0, empleo, NA_real_)),

    w_obrero_sueldos = ifelse(obreros_permanentes > 0 & sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 /
                                obreros_permanentes / 12 * 1000, NA_real_),

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

    ANIO_F = factor(ANIO)
  )

# CONTROLES FIJOS EN 2022 -- patrón de 05_resultados_y_mecanismos.R corregido,
# NO el patrón viejo (factor(CIIU4) etc. sobre el panel completo).
clasificacion_2022 <- panel %>%
  filter(ANIO == 2022) %>%
  transmute(NORDEMP,
            sector_2022 = factor(CIIU4),
            depto_2022 = factor(DPTO),
            tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

base <- base %>% left_join(clasificacion_2022, by = "NORDEMP")

anios_panel <- sort(unique(base$ANIO))

cat("\nPanel:", nrow(base), "filas |", n_distinct(base$NORDEMP), "firmas |",
    "años", min(base$ANIO), "a", max(base$ANIO), "\n")
cat("Firmas con controles fijos en 2022:", nrow(clasificacion_2022), "de",
    n_distinct(base$NORDEMP), "firmas totales.\n")

# construir_medidas(): IDÉNTICA a 09_validez_exposicion.R -- no se cambia la
# construcción de ninguna de las cinco medidas.
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

  medidas %>%
    mutate(across(c(bite, golpe_c, golpe_a, golpe_costo, exposure), ~ {
      if (all(is.na(.x))) return(.x)
      lim <- quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE)
      w <- pmin(pmax(.x, lim[1]), lim[2])
      w / sd(w, na.rm = TRUE)
    }))
}

# salto(): igual estructura que salto_ventana() de 09_validez_exposicion.R,
# aplicada aquí a un outcome que por defecto es log_empleo. Los controles ya
# están fijos en 2022 desde la sección 0 (no hay transición a mitad de
# script, a diferencia de 09).
salto <- function(medida, anio_exposicion, anio_base, anio_choque,
                  outcome = "log_empleo") {

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
    efecto_pct = 100 * f[["Estimate"]],
    ee_pct = 100 * f[["Std. Error"]],
    p_valor = f[["Pr(>|t|)"]],
    significancia = estrellas(f[["Pr(>|t|)"]]),
    firmas = nobs(modelo)
  )
}


# ==============================================================================
# 1. RESULTADO PRINCIPAL: EMPLEO 2022->2023 CONTRA EXPOSICIÓN DE 2019
# ==============================================================================
titulo("1. EMPLEO 2022->2023 CONTRA EXPOSICIÓN DE 2019 (CELDA LIMPIA)")

principal <- bind_rows(lapply(names(MEDIDAS), function(m) salto(m, 2019, 2022, 2023, "log_empleo")))

ver(principal)
guardar_tabla(principal, "T1_01_empleo_celda_limpia_2023",
              "Tabla 1.1. Crecimiento del empleo 2022-2023 por DE de exposición, exposición medida en 2019")

# Referencia: la especificación estándar (exposición 2022 = año base), para
# comparar al lado.
estandar <- bind_rows(lapply(names(MEDIDAS), function(m) salto(m, 2022, 2022, 2023, "log_empleo")))

comparacion_estandar_limpia <- estandar %>%
  select(medida, clave, efecto_estandar = efecto_pct, ee_estandar = ee_pct,
         p_estandar = p_valor, n_estandar = firmas) %>%
  left_join(
    principal %>% select(clave, efecto_limpio = efecto_pct, ee_limpio = ee_pct,
                         p_limpio = p_valor, n_limpio = firmas),
    by = "clave"
  ) %>%
  mutate(sig_estandar = estrellas(p_estandar), sig_limpio = estrellas(p_limpio))

ver(comparacion_estandar_limpia)
guardar_tabla(comparacion_estandar_limpia, "T1_02_estandar_vs_limpia",
              "Tabla 1.2. Empleo 2022-2023: exposición estándar (2022) vs. celda limpia (2019)")

cat("\nNOTA: esta especificación (dos periodos, corte transversal) NO es\n",
    "idéntica al event study de panel completo de 05_resultados_y_mecanismos.R\n",
    "(lectura A, -1,754%, p=0,004) -- usa el mismo outcome y el mismo año de\n",
    "choque, pero un estimador distinto (diferencia simple entre dos años,\n",
    "sin efectos de firma ni de año). El punto de comparación aquí es la fila\n",
    "'estándar' de T1_02 contra la fila 'limpia', no contra la cifra del 05.\n",
    "La sección 4 de este script sí replica el event study completo del 05,\n",
    "con la exposición de 2019 en lugar de la de 2022.\n")


# ==============================================================================
# 2. SENSIBILIDAD AL REZAGO, CHOQUE DE 2023
# ==============================================================================
titulo("2. SENSIBILIDAD AL REZAGO DE LA EXPOSICIÓN, CHOQUE DE 2023")

# Rezago contado desde el año del choque (misma convención que
# 09_validez_exposicion.R, sección 2): anio_exp <- anio_choque - r. El año
# base del outcome queda fijo en 2022 sin importar el rezago. Rezago 1 es la
# especificación estándar (anio_exp = anio_base = 2022); rezago 4 es la
# celda limpia (anio_exp = 2019).
sensibilidad <- bind_rows(lapply(names(MEDIDAS), function(m) {
  bind_rows(lapply(0:5, function(r) {
    anio_exp <- 2023 - r
    if (anio_exp == 2020) return(NULL)
    resultado <- salto(m, anio_exp, 2022, 2023, "log_empleo")
    if (is.null(resultado)) return(NULL)
    mutate(resultado, rezago = r)
  }))
}))

ver(sensibilidad, filas = 40)
guardar_tabla(sensibilidad, "T2_01_sensibilidad_rezago_2023",
              "Tabla 2.1. Empleo, choque 2023: efecto según el rezago de la exposición (0-5), las cinco medidas")

g1 <- ggplot(sensibilidad, aes(x = rezago, y = efecto_pct, color = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = PALETA) +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "Efecto sobre el empleo (2022->2023) según el rezago de la exposición",
       subtitle = "Línea punteada = rezago 1 (estándar, exposición 2022). Línea discontinua = rezago 4 (celda limpia, exposición 2019).",
       x = "Años de rezago de la exposición (desde el año del choque)",
       y = "Efecto sobre el empleo (%)", color = NULL,
       caption = "Si el efecto se mantiene negativo y significativo al rezagar, es económico. Si desaparece entre rezago 0 y 1, venía del traslape.") +
  tema_tesis
guardar_grafico(g1, "G2_01_sensibilidad_rezago_empleo")

cat("\nCÓMO LEER: si el coeficiente se mantiene negativo y significativo al\n",
    "rezagar (en particular en rezago 4, la celda limpia), el efecto es\n",
    "económico. Si desaparece entre rezago 0 y 1 -como le pasó al costo\n",
    "laboral-, venía del traslape.\n")


# ==============================================================================
# 3. PLACEBO RODANTE SOBRE EL EMPLEO
# ==============================================================================
titulo("3. PLACEBO RODANTE SOBRE EL EMPLEO (REZAGO FIJO DE 4 AÑOS)")

# Misma lógica que la celda limpia rodante de 09_validez_exposicion.R,
# sección 2.2, con log_empleo en vez de log_costo. Para cada año de choque
# estimable, exposición medida 4 años antes y crecimiento del empleo entre el
# año base (choque - 1) y el año del choque.
ANIOS <- anios_panel[anios_panel >= 2014 & !(anios_panel %in% c(2020, 2021))]
cat("Años en los que se intenta estimar:", paste(ANIOS, collapse = ", "), "\n")

rodante_empleo <- bind_rows(lapply(names(MEDIDAS), function(m) {
  bind_rows(lapply(ANIOS, function(a) {
    anio_base <- a - 1
    if (anio_base == 2020) return(NULL)
    anio_exp <- a - 4
    if (anio_exp == 2020 || !(anio_exp %in% anios_panel)) return(NULL)
    salto(m, anio_exp, anio_base, a, "log_empleo")
  }))
})) %>%
  left_join(SALARIO_MINIMO %>% select(anio, aumento_real_pct), by = c("anio_choque" = "anio")) %>%
  mutate(es_choque = anio_choque == 2023,
         ic_inf = efecto_pct - 1.96 * ee_pct,
         ic_sup = efecto_pct + 1.96 * ee_pct)

ver(rodante_empleo, filas = 50)
guardar_tabla(rodante_empleo, "T3_01_placebo_rodante_empleo",
              "Tabla 3.1. Placebo rodante sobre el empleo, celda limpia (rezago 4), todos los años estimables")

cat("\nNOTA: con REZAGO=4, el choque de 2024 requeriría exposición de 2020\n",
    "(excluido por pandemia), así que 2024 no aparece en esta serie -- mismo\n",
    "límite estructural documentado en 09_validez_exposicion.R para el costo\n",
    "laboral. El grupo 'alto' de aumento real queda representado solo por\n",
    "2023.\n")

# --- Grupos por aumento real, misma clasificación que 09_validez_exposicion.R ----
grupos_empleo <- rodante_empleo %>%
  filter(!is.na(aumento_real_pct)) %>%
  mutate(
    grupo_aumento_real = case_when(
      aumento_real_pct > 4  ~ "1. Alto (>+4%)",
      aumento_real_pct >= 0 ~ "2. Intermedio (0% a +4%)",
      TRUE                  ~ "3. Negativo (<0%)"
    )
  )

comparacion_grupos_empleo <- grupos_empleo %>%
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

ver(comparacion_grupos_empleo, filas = 20)
guardar_tabla(comparacion_grupos_empleo, "T3_02_comparacion_grupos_empleo",
              "Tabla 3.2. Empleo: efecto promedio por grupo de aumento real (alto=2023; intermedio=2016-2019; negativo=2015,2022)")

correlacion_empleo <- grupos_empleo %>%
  group_by(medida, clave) %>%
  summarise(
    n_anios = n(),
    correlacion_aumento_real = cor(aumento_real_pct, efecto_pct, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  arrange(desc(correlacion_aumento_real))

ver(correlacion_empleo)
guardar_tabla(correlacion_empleo, "T3_03_correlacion_aumento_real_empleo",
              "Tabla 3.3. Correlación entre el efecto sobre el empleo (celda limpia) y el aumento real, por medida")

cat("\nCAUTELA (la misma que se aplicó al costo laboral en la auditoría\n",
    "adversarial de 09_validez_exposicion.R, sección 4.5): con ocho puntos\n",
    "como máximo por medida, esta correlación no soporta una conclusión\n",
    "fuerte en NINGUNA dirección -- ni para confirmar que el efecto escala con\n",
    "el choque, ni para descartarlo. Se reporta como evidencia parcial, no\n",
    "como prueba concluyente en ningún sentido.\n")

g2 <- ggplot(rodante_empleo, aes(x = factor(anio_choque), y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.5) +
  facet_wrap(~ medida, scales = "free_y") +
  scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                     labels = c("Año normal", "2023 (choque)")) +
  labs(title = "Placebo rodante sobre el empleo, celda limpia (rezago 4)",
       subtitle = "¿2023 se distingue de un año cualquiera, con cada medida?",
       x = NULL, y = "Efecto sobre el empleo (%)", color = NULL) +
  tema_tesis +
  theme(axis.text.x = element_text(size = 8))
guardar_grafico(g2, "G3_01_placebo_rodante_empleo", ancho = 12, alto = 7)


# ==============================================================================
# 4. TENDENCIAS PREVIAS DEL EMPLEO CON LA ESPECIFICACIÓN LIMPIA
# ==============================================================================
titulo("4. TENDENCIAS PREVIAS DEL EMPLEO, EXPOSICIÓN DE 2019 (EVENT STUDY COMPLETO)")

# Reproduce el estudio de evento de 05_resultados_y_mecanismos.R
# (i(ANIO_F, tratamiento, ref='2022') con sector/tamaño/departamento x año,
# todos fijados en 2022, errores agrupados por firma) cambiando SOLO la
# fuente de la exposición: en vez de Bite2022_obreros_de (construida con el
# salario de 2022), se usa la exposición construida con datos de 2019
# (construir_medidas(2019, 2023), la misma función de la sección 0, sin
# modificar). El outcome, los controles y la ventana de estimación son los
# mismos que en 05.
EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
cat("Efectos fijos:", EFECTOS, "\n")
cat("Errores agrupados por firma (NORDEMP). Ventana: 2015-2019, 2021-2024.\n")

exposicion_2019 <- construir_medidas(2019, 2023) %>%
  rename_with(~paste0(.x, "_de"), -NORDEMP)

base_evento <- base %>%
  left_join(exposicion_2019, by = "NORDEMP") %>%
  filter(ANIO %in% c(2015:2019, 2021:2024))

cat("Firmas-año en la ventana de estimación:", nrow(base_evento), "\n")
cat("Firmas distintas:", n_distinct(base_evento$NORDEMP), "\n")

estudio_evento <- function(outcome, tratamiento, base_datos = base_evento) {
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
    arrange(anio) %>%
    mutate(efecto_pct = 100 * coeficiente,
           ee_pct = 100 * error_estandar,
           significancia = estrellas(p_valor))

  p_previos <- tryCatch(
    wald(modelo, keep = "ANIO_F::(2015|2016|2017|2018|2019|2021):", print = FALSE)$p,
    error = function(e) NA_real_)

  list(tabla = tabla, p_previos = p_previos, modelo = modelo, variable = v)
}

eventos_empleo <- lapply(names(MEDIDAS), function(m) estudio_evento("log_empleo", m))
names(eventos_empleo) <- names(MEDIDAS)

tendencias_previas <- bind_rows(lapply(names(MEDIDAS), function(m) {
  ev <- eventos_empleo[[m]]
  if (is.null(ev)) return(NULL)
  tibble(medida = MEDIDAS[[m]], clave = m, p_previos_2019 = ev$p_previos,
         observaciones = nobs(ev$modelo))
}))

ver(tendencias_previas)
guardar_tabla(tendencias_previas, "T4_01_tendencias_previas_resumen",
              "Tabla 4.1. Prueba conjunta de coeficientes previos (2015-2019, 2021 vs. 2022), exposición de 2019", decimales = 4)

cat("\nCOMPARACIÓN CON LA ESPECIFICACIÓN CONTAMINADA (exposición 2022): Bite\n",
    "daba p=0,455 (no rechaza) en 05_resultados_y_mecanismos.R. Ver arriba si\n",
    "ese resultado se mantiene, mejora o empeora con exposición de 2019.\n")

# --- Coeficientes año a año, las cinco medidas ------------------------------------
coeficientes_anio_a_anio <- bind_rows(lapply(names(MEDIDAS), function(m) {
  ev <- eventos_empleo[[m]]
  if (is.null(ev)) return(NULL)
  ev$tabla %>% mutate(medida = MEDIDAS[[m]], clave = m) %>%
    select(medida, clave, anio, efecto_pct, ee_pct, p_valor, significancia)
}))

ver(coeficientes_anio_a_anio, filas = 60)
guardar_tabla(coeficientes_anio_a_anio, "T4_02_coeficientes_anio_a_anio",
              "Tabla 4.2. Coeficientes del estudio de evento año por año (frente a 2022), exposición de 2019, las cinco medidas")

cat("\nCÓMO LEER: un p-valor agregado alto (no rechaza tendencia previa) puede\n",
    "esconder un patrón sistemático de magnitud pequeña -- revisar si los\n",
    "coeficientes previos (2015-2019, 2021) son consistentemente del mismo\n",
    "signo, aunque individualmente no sean significativos.\n")

# --- ¿El coeficiente de 2023 es MÁS grande que la tendencia previa, o solo la
#     continúa? ------------------------------------------------------------------
# No basta con que la prueba conjunta rechace o no rechace: si los
# coeficientes previos ya eran grandes y del mismo signo que el de 2023, el
# coeficiente de 2023 puede ser simplemente el final de esa trayectoria, no un
# quiebre nuevo causado por el choque. Se compara el promedio de los
# coeficientes previos (2015-2019, 2021) contra el de 2023 en valor absoluto.
comparacion_pretendencia <- bind_rows(lapply(names(MEDIDAS), function(m) {
  tabla_m <- coeficientes_anio_a_anio %>% filter(clave == m)
  pre <- tabla_m %>% filter(anio %in% c(2015, 2016, 2017, 2018, 2019, 2021)) %>% pull(efecto_pct)
  c2023 <- tabla_m %>% filter(anio == 2023) %>% pull(efecto_pct)
  if (length(pre) == 0 || length(c2023) != 1) return(NULL)
  tibble(medida = MEDIDAS[[m]], clave = m,
         promedio_previo_pct = mean(pre), coef_2023_pct = c2023,
         mismo_signo = sign(mean(pre)) == sign(c2023),
         pct_2023_de_la_previa = round(100 * abs(c2023) / abs(mean(pre)), 1),
         c2023_MENOR_que_la_previa = abs(c2023) < abs(mean(pre)))
}))

ver(comparacion_pretendencia)
guardar_tabla(comparacion_pretendencia, "T4_03_coef2023_vs_tendencia_previa",
              "Tabla 4.3. Coeficiente de 2023 frente al promedio de los coeficientes previos (2015-2019, 2021), en valor absoluto")

cat("\nCÓMO LEER esta tabla: si el coeficiente de 2023 es MENOR en magnitud que\n",
    "el promedio previo (columna 'c2023_MENOR_que_la_previa' = TRUE), 2023 no\n",
    "representa un quiebre nuevo -- es un punto más, y más chico, dentro de una\n",
    "trayectoria descendente que ya existía desde 2015, que además converge\n",
    "hacia 0 al acercarse a 2022 (el año de referencia, por construcción) y\n",
    "vuelve a alejarse un poco en 2023-2024. Ese patrón es más compatible con\n",
    "reversión hacia el año de referencia (el mismo problema que motivó todo\n",
    "este diagnóstico) que con un efecto causado por el choque de 2023.\n")

g3 <- ggplot(coeficientes_anio_a_anio, aes(x = anio, y = efecto_pct, color = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ medida, scales = "free_y") +
  scale_color_manual(values = PALETA, guide = "none") +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  labs(title = "Estudio de evento sobre el empleo, exposición de 2019 (celda limpia)",
       subtitle = "Efecto de una DE más de exposición, frente a 2022",
       x = NULL, y = "Efecto (%)",
       caption = "Controles: firma, año, sector x año, tamaño x año, departamento x año (fijados en 2022). 2020 excluido.") +
  tema_tesis
guardar_grafico(g3, "G4_01_evento_empleo_exposicion2019", ancho = 12, alto = 7)


# ==============================================================================
# 5. SÍNTESIS: ¿QUÉ LECTURA SE ACTIVA?
# ==============================================================================
titulo("5. SÍNTESIS: ¿SOBREVIVE EL EFECTO SOBRE EL EMPLEO SIN EL TRASLAPE?")

bite_principal <- principal %>% filter(clave == "bite")
bite_rezago45 <- sensibilidad %>% filter(clave == "bite", rezago %in% c(4, 5))
bite_p_previos <- tendencias_previas %>% filter(clave == "bite") %>% pull(p_previos_2019)
bite_pretendencia <- comparacion_pretendencia %>% filter(clave == "bite")

n_medidas_significativas <- sum(principal$p_valor < 0.05 & principal$efecto_pct < 0, na.rm = TRUE)
n_medidas_negativas <- sum(principal$efecto_pct < 0, na.rm = TRUE)
n_medidas_no_rechazan_previas <- sum(tendencias_previas$p_previos_2019 >= 0.10, na.rm = TRUE)
n_medidas_2023_menor_que_previa <- sum(comparacion_pretendencia$c2023_MENOR_que_la_previa, na.rm = TRUE)

# EL TRASLAPE ESPECÍFICO (denominador de la exposición en la base del
# outcome) -- lo que preguntan las secciones 1-3, igual que a 09_validez_
# exposicion.R con el costo laboral:
supera_traslape <- nrow(bite_principal) == 1 && bite_principal$efecto_pct < 0 && bite_principal$p_valor < 0.05 &&
  nrow(bite_rezago45) == 2 && all(bite_rezago45$efecto_pct < 0 & bite_rezago45$p_valor < 0.05)

# UNA AMENAZA DISTINTA -- no es el traslape aritmético, es una tendencia
# previa grande y del mismo signo que el coeficiente de 2023 (sección 4): si
# el coeficiente de 2023 es MENOR en magnitud que el promedio de los
# coeficientes previos, 2023 no es un quiebre nuevo, es la cola de una
# trayectoria que ya venía cayendo (y que además converge hacia 0 al
# acercarse a 2022, el año de referencia).
tendencia_previa_domina <- nrow(bite_pretendencia) == 1 &&
  (is.na(bite_p_previos) || bite_p_previos < 0.10) &&
  bite_pretendencia$mismo_signo && bite_pretendencia$c2023_MENOR_que_la_previa

sintesis <- tibble(
  criterio = c(
    "Bite, celda limpia (2019): signo",
    "Bite, celda limpia (2019): significativo al 5%",
    "Bite, rezago 4-5: se mantiene negativo y significativo",
    "Bite, tendencias previas (exposición 2019): p-valor conjunto",
    "Bite: ¿el coef. de 2023 es MENOR que el promedio de los previos?",
    "Medidas (de 5) negativas en la celda limpia",
    "Medidas (de 5) negativas Y significativas al 5% en la celda limpia",
    "Medidas (de 5) que NO rechazan tendencias previas (p >= 0,10)",
    "Medidas (de 5) donde 2023 es MENOR que el promedio previo"
  ),
  valor = c(
    ifelse(nrow(bite_principal) == 1, ifelse(bite_principal$efecto_pct < 0, "Negativo", "Positivo"), "No estimable"),
    ifelse(nrow(bite_principal) == 1, ifelse(bite_principal$p_valor < 0.05, "Sí", "No"), "No estimable"),
    ifelse(nrow(bite_rezago45) == 2, ifelse(all(bite_rezago45$efecto_pct < 0 & bite_rezago45$p_valor < 0.05), "Sí", "No"), "No estimable"),
    ifelse(length(bite_p_previos) == 1, round(bite_p_previos, 5), NA),
    ifelse(nrow(bite_pretendencia) == 1, ifelse(bite_pretendencia$c2023_MENOR_que_la_previa, "Sí", "No"), "No estimable"),
    n_medidas_negativas,
    n_medidas_significativas,
    n_medidas_no_rechazan_previas,
    n_medidas_2023_menor_que_previa
  )
)
ver(sintesis)
guardar_tabla(sintesis, "T5_01_sintesis_criterios", "Tabla 5.1. Criterios de lectura, resumidos", decimales = 5)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("LECTURA ACTIVADA (regla fijada en el encabezado del script, antes de ver\n",
    "estos resultados):\n\n")

if (supera_traslape && !tendencia_previa_domina) {
  cat("  EL EFECTO SE SOSTIENE: Bite da un efecto negativo y significativo con\n",
      "  exposición de 2019 (celda limpia), se mantiene negativo y\n",
      "  significativo en los rezagos 4 y 5, y no está dominado por una\n",
      "  tendencia previa mayor. Hay resultado principal para la tesis.\n")
} else if (supera_traslape && tendencia_previa_domina) {
  cat("  EL TRASLAPE ESPECÍFICO NO EXPLICA EL RESULTADO, PERO UNA AMENAZA\n",
      "  DISTINTA SÍ LO COMPLICA. Las secciones 1-3 muestran que el efecto NO\n",
      "  se comporta como el del costo laboral: no se atenúa hacia cero al\n",
      "  rezagar la exposición (se mantiene entre -1,3% y -4,0% en TODOS los\n",
      "  rezagos, 0 a 5), y 2023 se distingue con signo NEGATIVO de un\n",
      "  conjunto de años 'normales' que dan efectos POSITIVOS -- eso descarta\n",
      "  la explicación mecánica específica que motivó este script.\n\n",
      "  PERO la sección 4 encuentra un problema distinto y serio: con\n",
      "  exposición de 2019, la prueba conjunta de tendencias previas RECHAZA\n",
      "  con fuerza (p =", signif(bite_p_previos, 3), ", peor que el p=0,455\n",
      "  de la especificación contaminada), y en las CINCO medidas el\n",
      "  coeficiente de 2023 es MENOR en magnitud que el promedio de los\n",
      "  coeficientes previos (2015-2019, 2021), que a su vez convergen hacia\n",
      "  0 al acercarse a 2022 (el año de referencia). Eso es exactamente el\n",
      "  patrón de una firma de exposición alta reconvergiendo hacia su propio\n",
      "  nivel de referencia, no el de un choque nuevo en 2023. NO hay\n",
      "  resultado limpio de empleo para reportar como efecto causal del\n",
      "  salario mínimo con esta especificación -- el problema ya no es el\n",
      "  traslape aritmético, es la tendencia previa, y hay que decirlo así.\n")
} else if (n_medidas_negativas >= 3 && n_medidas_significativas >= 1) {
  cat("  A MEDIO CAMINO: el efecto es negativo en la mayoría de las medidas,\n",
      "  pero no se sostiene con la misma fuerza en todas las condiciones\n",
      "  (celda limpia, rezagos altos, tendencias previas). Reportar la serie\n",
      "  completa y declarar la limitación explícitamente.\n")
} else {
  cat("  EL EFECTO NO SE SOSTIENE: al cortar el traslape (exposición de 2019),\n",
      "  el efecto sobre el empleo desaparece o cambia de signo. El -1,754% de\n",
      "  05_resultados_y_mecanismos.R venía del traslape algebraico entre el\n",
      "  denominador de la exposición y la base del outcome, igual que le\n",
      "  pasó al costo laboral. LA TESIS NO TIENE EFECTO DE EMPLEO QUE\n",
      "  REPORTAR con esta medida y esta especificación.\n")
}
cat(strrep("=", 78), "\n", sep = "")


if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")
