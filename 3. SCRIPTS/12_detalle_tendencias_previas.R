# ==============================================================================
# 12_detalle_tendencias_previas.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# DETALLE CUANTITATIVO DE LA SECCIÓN 4 DE 11_empleo_sin_traslape.R
#
# POR QUÉ ESTE SCRIPT
# La sección 4 de 11_empleo_sin_traslape.R concluyó que las tendencias
# previas del empleo rechazan con fuerza (p=0,00046 para Bite) usando
# exposición de 2019, y que el coeficiente de 2023 es menor en magnitud que
# los previos -- esa conclusión es la que decide si hay resultado de empleo
# defendible en la tesis. Se reportó en forma resumida y hace falta el
# detalle para auditarla.
#
# NO TODO EL DETALLE PEDIDO ESTABA GUARDADO EN LOS CSV de 11: el coeficiente,
# error estándar y p-valor año a año sí estaban (T4_02), y el N de cada
# modelo estaba en T4_01 -- pero el estadístico F y los grados de libertad del
# test conjunto, el test excluyendo un año a la vez, el contraste formal
# 2023-vs-promedio-previo, y la comparación de muestras entre exposición 2019
# y 2022 NO estaban guardados en ningún CSV: sólo el p-valor agregado
# (wald()$p) se guardó, no el objeto completo. Por eso este script SÍ vuelve
# a correr los mismos modelos de 11_empleo_sin_traslape.R -- misma
# construcción de medidas, mismos controles, misma ventana, mismo cluster --
# únicamente para extraer más detalle de esos modelos, no para buscar una
# especificación distinta.
#
# QUÉ HACE
#   1. Coeficientes año a año, exposición 2019, las cinco medidas: coeficiente,
#      error estándar, p-valor individual, IC 95%, N. Incluye 2022 (ref.).
#   2. El test conjunto: años exactos, estadístico F, grados de libertad,
#      p-valor, nivel de clustering -- para exposición 2019 y, de referencia,
#      para exposición 2022 (para confirmar el p=0,455 original).
#   3. El mismo test conjunto excluyendo un año a la vez (leave-one-out),
#      exposición 2019, las cinco medidas: si el rechazo desaparece al quitar
#      un año concreto, el problema es ese año, no un patrón sistemático.
#   4. Contraste formal: coeficiente de 2023 menos el promedio de los
#      coeficientes de 2015-2019 (sin 2021), con su error estándar y p-valor,
#      igual método que la lectura B de 05_resultados_y_mecanismos.R.
#   5. Comparación de muestras: firmas con exposición válida en 2019 frente a
#      2022, cuántas están en las dos, y si las que están solo en una
#      difieren en tamaño o sector.
#   6. Las dos series de coeficientes año a año (exposición 2019 y exposición
#      2022) lado a lado, para las cinco medidas.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  4. RESULTADOS/Detalle_tendencias_previas/
# ==============================================================================


# ==============================================================================
# 0. PREPARACIÓN (idéntica a 11_empleo_sin_traslape.R -- no se cambia nada de
#    la construcción de las medidas ni de los controles)
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "Detalle_tendencias_previas")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 60) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 4) {
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
guardar_grafico <- function(g, nombre_archivo, ancho = 12, alto = 7) {
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
        legend.position = "bottom",
        panel.grid.minor = element_blank())

PALETA <- c(`Bite (Kaitz de obreros)` = "#C00000",
            `Golpe C (3 categorías)`  = "#1F4E79",
            `Golpe A (ponderada)`     = "#2E8B57",
            `Golpe costo`             = "#E69F00",
            `Exposure (% obreros)`    = "#7B68EE")

titulo("0. DATOS Y PREPARACIÓN (igual que 11_empleo_sin_traslape.R)")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

SALARIO_MINIMO <- tibble(
  anio  = 2008:2024,
  valor = c(461500, 496900, 515000, 535600, 566700, 589500, 616000, 644350,
            689455, 737717, 781242, 828116, 877803, 908526, 1000000,
            1160000, 1300000),
  ipc_pct = c(rep(NA_real_, 4),
              2.44, 1.94, 3.66, 6.77, 5.75, 4.09, 3.18,
              3.80, 1.61, 5.62, 13.12, 9.28, 5.20)
)

RAZON_COSTO_MINIMO <- 1.531

MEDIDAS <- c(bite = "Bite (Kaitz de obreros)",
             golpe_c = "Golpe C (3 categorías)",
             golpe_a = "Golpe A (ponderada)",
             golpe_costo = "Golpe costo",
             exposure = "Exposure (% obreros)")

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

clasificacion_2022 <- panel %>%
  filter(ANIO == 2022) %>%
  transmute(NORDEMP,
            sector_2022 = factor(CIIU4),
            depto_2022 = factor(DPTO),
            tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

base <- base %>% left_join(clasificacion_2022, by = "NORDEMP")

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

EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
ANIOS_PREVIOS <- c(2015, 2016, 2017, 2018, 2019, 2021)
PATRON_PREVIOS <- "ANIO_F::(2015|2016|2017|2018|2019|2021):"

cat("Efectos fijos:", EFECTOS, "\n")
cat("Años previos (test conjunto):", paste(ANIOS_PREVIOS, collapse = ", "), "\n")

exposicion_2019 <- construir_medidas(2019, 2023) %>% rename_with(~paste0(.x, "_de"), -NORDEMP)
exposicion_2022 <- construir_medidas(2022, 2023) %>% rename_with(~paste0(.x, "_de"), -NORDEMP)

base_evento_2019 <- base %>% left_join(exposicion_2019, by = "NORDEMP") %>%
  filter(ANIO %in% c(2015:2019, 2021:2024))
base_evento_2022 <- base %>% left_join(exposicion_2022, by = "NORDEMP") %>%
  filter(ANIO %in% c(2015:2019, 2021:2024))

cat("Firmas-año, ventana de estimación (2019):", nrow(base_evento_2019), "\n")
cat("Firmas-año, ventana de estimación (2022):", nrow(base_evento_2022), "\n")

# estudio_evento(): misma fórmula, mismos controles, mismo cluster que
# 11_empleo_sin_traslape.R / 05_resultados_y_mecanismos.R -- se agrega el
# objeto `modelo` y el wald() completo (no solo $p) para poder extraer F, df1
# y df2 más abajo.
estudio_evento <- function(outcome, tratamiento, base_datos) {
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
    mutate(efecto_pct = 100 * coeficiente, ee_pct = 100 * error_estandar,
           ic95_inf_pct = efecto_pct - 1.96 * ee_pct,
           ic95_sup_pct = efecto_pct + 1.96 * ee_pct,
           significancia = estrellas(p_valor))

  wald_previos <- tryCatch(wald(modelo, keep = PATRON_PREVIOS, print = FALSE), error = function(e) NULL)

  list(tabla = tabla, wald_previos = wald_previos, modelo = modelo, variable = v, n = nobs(modelo))
}


# ==============================================================================
# 1. COEFICIENTES AÑO A AÑO, EXPOSICIÓN 2019, LAS CINCO MEDIDAS
# ==============================================================================
titulo("1. COEFICIENTES AÑO A AÑO, EXPOSICIÓN DE 2019")

eventos_2019 <- lapply(names(MEDIDAS), function(m) estudio_evento("log_empleo", m, base_evento_2019))
names(eventos_2019) <- names(MEDIDAS)

coef_2019 <- bind_rows(lapply(names(MEDIDAS), function(m) {
  ev <- eventos_2019[[m]]
  if (is.null(ev)) return(NULL)
  ev$tabla %>% mutate(medida = MEDIDAS[[m]], clave = m, n = ev$n) %>%
    select(medida, clave, anio, efecto_pct, ee_pct, ic95_inf_pct, ic95_sup_pct, p_valor, significancia, n)
}))

ver(coef_2019)
guardar_tabla(coef_2019, "T1_coeficientes_anio_a_anio_exposicion2019",
              "Tabla 1. Coeficientes año a año (frente a 2022), exposición de 2019, las cinco medidas: efecto, EE, IC 95%, p-valor, N")


# ==============================================================================
# 2. EL TEST CONJUNTO: AÑOS, F, GRADOS DE LIBERTAD, P-VALOR, CLUSTERING
# ==============================================================================
titulo("2. EL TEST CONJUNTO DE TENDENCIAS PREVIAS -- DETALLE COMPLETO")

eventos_2022 <- lapply(names(MEDIDAS), function(m) estudio_evento("log_empleo", m, base_evento_2022))
names(eventos_2022) <- names(MEDIDAS)

detalle_wald <- function(eventos, etiqueta_exposicion) {
  bind_rows(lapply(names(MEDIDAS), function(m) {
    ev <- eventos[[m]]
    if (is.null(ev) || is.null(ev$wald_previos)) return(NULL)
    w <- ev$wald_previos
    tibble(exposicion = etiqueta_exposicion, medida = MEDIDAS[[m]], clave = m,
           anios_incluidos = paste(ANIOS_PREVIOS, collapse = ", "),
           n_anios = length(ANIOS_PREVIOS),
           estadistico_F = w$stat, df1 = w$df1, df2 = w$df2, p_valor = w$p,
           clustering = w$vcov, n_obs = ev$n)
  }))
}

test_conjunto <- bind_rows(
  detalle_wald(eventos_2019, "2019 (celda limpia)"),
  detalle_wald(eventos_2022, "2022 (estándar, contaminada)")
)

ver(test_conjunto)
guardar_tabla(test_conjunto, "T2_test_conjunto_detalle",
              "Tabla 2. Test conjunto de tendencias previas: años, F, grados de libertad, p-valor y clustering, exposición 2019 y 2022")

cat("\nCONFIRMACIÓN: la fila de Bite con exposición 2022 debe reproducir el\n",
    "p=0,455 citado de 05_resultados_y_mecanismos.R. Ver la tabla de arriba.\n")


# ==============================================================================
# 3. LEAVE-ONE-OUT: EL MISMO TEST EXCLUYENDO UN AÑO A LA VEZ (EXPOSICIÓN 2019)
# ==============================================================================
titulo("3. TEST CONJUNTO EXCLUYENDO UN AÑO A LA VEZ, EXPOSICIÓN DE 2019")

leave_one_out <- bind_rows(lapply(names(MEDIDAS), function(m) {
  ev <- eventos_2019[[m]]
  if (is.null(ev)) return(NULL)
  bind_rows(lapply(ANIOS_PREVIOS, function(anio_excluido) {
    anios_restantes <- setdiff(ANIOS_PREVIOS, anio_excluido)
    patron <- paste0("ANIO_F::(", paste(anios_restantes, collapse = "|"), "):")
    w <- tryCatch(wald(ev$modelo, keep = patron, print = FALSE), error = function(e) NULL)
    if (is.null(w)) return(NULL)
    tibble(medida = MEDIDAS[[m]], clave = m, anio_excluido = anio_excluido,
           anios_restantes = paste(anios_restantes, collapse = ", "),
           estadistico_F = w$stat, df1 = w$df1, df2 = w$df2, p_valor = w$p,
           sigue_rechazando_al_5pct = w$p < 0.05)
  }))
}))

ver(leave_one_out, filas = 40)
guardar_tabla(leave_one_out, "T3_leave_one_out_tendencias_previas",
              "Tabla 3. Test conjunto de tendencias previas excluyendo un año a la vez, exposición de 2019, las cinco medidas")

cat("\nCÓMO LEER: si 'sigue_rechazando_al_5pct' es TRUE para todos los años\n",
    "excluidos (para una medida dada), el rechazo NO depende de un año\n",
    "particular -- es un patrón sistemático en todo el pre-período. Si se\n",
    "vuelve FALSE al excluir un año específico, ese año concreto es el que\n",
    "produce el rechazo.\n")

resumen_leave_one_out <- leave_one_out %>%
  group_by(medida, clave) %>%
  summarise(n_exclusiones_que_siguen_rechazando = sum(sigue_rechazando_al_5pct, na.rm = TRUE),
            n_exclusiones_totales = n(), .groups = "drop")
ver(resumen_leave_one_out)
guardar_tabla(resumen_leave_one_out, "T3b_resumen_leave_one_out",
              "Tabla 3b. Resumen: en cuántas de las 6 exclusiones (una por año) se mantiene el rechazo al 5%", decimales = 0)


# ==============================================================================
# 4. CONTRASTE FORMAL: 2023 CONTRA EL PROMEDIO DE 2015-2019
# ==============================================================================
titulo("4. CONTRASTE FORMAL: COEFICIENTE DE 2023 MENOS EL PROMEDIO 2015-2019")

# Mismo método que contraste() en 05_resultados_y_mecanismos.R: combinación
# lineal de coeficientes del event study, con su error estándar por la
# fórmula de la varianza de una combinación lineal (delta method exacto para
# combinaciones lineales, no una aproximación).
contraste_2023_vs_previo <- function(evento, etiqueta) {
  if (is.null(evento)) return(NULL)
  anios_promedio <- c(2015, 2016, 2017, 2018, 2019)
  pesos <- c(`2023` = 1, setNames(rep(-1 / length(anios_promedio), length(anios_promedio)),
                                  as.character(anios_promedio)))
  nombres <- paste0("ANIO_F::", names(pesos), ":", evento$variable)
  disponibles <- nombres %in% names(coef(evento$modelo))
  if (!all(disponibles)) return(NULL)

  b <- coef(evento$modelo)[nombres]
  V <- vcov(evento$modelo)[nombres, nombres]
  estimado <- sum(pesos * b)
  error <- sqrt(as.numeric(t(pesos) %*% V %*% pesos))
  p <- 2 * pnorm(-abs(estimado / error))

  nombre_2023 <- paste0("ANIO_F::2023:", evento$variable)
  nombres_previos <- paste0("ANIO_F::", anios_promedio, ":", evento$variable)
  tibble(medida = etiqueta,
         coeficiente_2023 = 100 * b[[nombre_2023]],
         promedio_2015_2019 = 100 * mean(b[nombres_previos]),
         diferencia_pct = 100 * estimado, ee_diferencia_pct = 100 * error,
         p_valor = p, significancia = estrellas(p))
}

contraste_previo <- bind_rows(lapply(names(MEDIDAS), function(m) {
  ev <- eventos_2019[[m]]
  contraste_2023_vs_previo(ev, MEDIDAS[[m]])
}))

ver(contraste_previo)
guardar_tabla(contraste_previo, "T4_contraste_2023_vs_promedio_previo",
              "Tabla 4. Contraste formal: coeficiente de 2023 menos el promedio de 2015-2019 (exposición 2019), con error estándar y p-valor")

cat("\nCÓMO LEER: si 'diferencia_pct' no es significativa (p >= 0,05 o 0,10),\n",
    "la afirmación de que el coeficiente de 2023 es MENOR (menos negativo)\n",
    "que el promedio previo no tiene respaldo estadístico formal -- sería una\n",
    "diferencia descriptiva, no una diferencia probada. Si es significativa,\n",
    "la afirmación de la sección 4 de 11_empleo_sin_traslape.R queda\n",
    "confirmada formalmente, no solo de forma descriptiva.\n")


# ==============================================================================
# 5. COMPARACIÓN DE MUESTRAS: EXPOSICIÓN 2019 VS. EXPOSICIÓN 2022
# ==============================================================================
titulo("5. FIRMAS CON EXPOSICIÓN VÁLIDA EN 2019 FRENTE A 2022")

comparacion_muestras <- bind_rows(lapply(names(MEDIDAS), function(m) {
  firmas_2019 <- exposicion_2019 %>% filter(!is.na(.data[[paste0(m, "_de")]])) %>% pull(NORDEMP)
  firmas_2022 <- exposicion_2022 %>% filter(!is.na(.data[[paste0(m, "_de")]])) %>% pull(NORDEMP)
  tibble(medida = MEDIDAS[[m]], clave = m,
         n_2019 = length(firmas_2019), n_2022 = length(firmas_2022),
         n_en_ambas = length(intersect(firmas_2019, firmas_2022)),
         n_solo_2019 = length(setdiff(firmas_2019, firmas_2022)),
         n_solo_2022 = length(setdiff(firmas_2022, firmas_2019)))
}))

ver(comparacion_muestras)
guardar_tabla(comparacion_muestras, "T5_comparacion_muestras_2019_2022",
              "Tabla 5. N de firmas con exposición válida en 2019 vs. 2022, traslape y exclusivas, las cinco medidas", decimales = 0)

# --- Caracterización de las firmas exclusivas de cada año, para Bite --------------
firmas_bite_2019 <- exposicion_2019 %>% filter(!is.na(bite_de)) %>% pull(NORDEMP)
firmas_bite_2022 <- exposicion_2022 %>% filter(!is.na(bite_de)) %>% pull(NORDEMP)

solo_2019 <- setdiff(firmas_bite_2019, firmas_bite_2022)
solo_2022 <- setdiff(firmas_bite_2022, firmas_bite_2019)
en_ambas <- intersect(firmas_bite_2019, firmas_bite_2022)

cat("\nBite -- firmas solo en 2019:", length(solo_2019),
    "| solo en 2022:", length(solo_2022),
    "| en ambas:", length(en_ambas), "\n")

# Caracterización con la propia clasificación de la firma en el año en que SÍ
# tiene el dato (2019 para 'solo_2019', 2022 para 'solo_2022'), y con
# tamano_2022/sector_2022 (clasificación fija) cuando la firma esté en el
# panel en 2022 -- puede ser NA para 'solo_2019' si la firma no aparece en el
# panel en 2022 en absoluto.
caracterizar_grupo <- function(nordemps, etiqueta, anio_propio) {
  propia <- base %>% filter(ANIO == anio_propio, NORDEMP %in% nordemps) %>%
    distinct(NORDEMP, .keep_all = TRUE)
  tibble(
    grupo = etiqueta,
    n_firmas = length(nordemps),
    tamano_pequena_pct = round(100 * mean(propia$tamano_empresa == "Pequena", na.rm = TRUE), 1),
    tamano_mediana_pct = round(100 * mean(propia$tamano_empresa == "Mediana", na.rm = TRUE), 1),
    tamano_grande_pct = round(100 * mean(propia$tamano_empresa == "Grande", na.rm = TRUE), 1),
    n_sectores_ciiu4_distintos = n_distinct(propia$CIIU4, na.rm = TRUE),
    pct_con_clasificacion_2022 = round(100 * mean(nordemps %in% clasificacion_2022$NORDEMP), 1)
  )
}

caracterizacion_bite <- bind_rows(
  caracterizar_grupo(solo_2019, "Solo en 2019 (propio año: 2019)", 2019),
  caracterizar_grupo(solo_2022, "Solo en 2022 (propio año: 2022)", 2022),
  caracterizar_grupo(en_ambas, "En ambas (propio año: 2022)", 2022)
)

ver(caracterizacion_bite)
guardar_tabla(caracterizacion_bite, "T5b_caracterizacion_firmas_bite",
              "Tabla 5b. Caracterización por tamaño y sector de las firmas exclusivas de cada año y las que están en ambas (Bite)", decimales = 1)

# Top 5 sectores de cada grupo exclusivo, para ver si difieren cualitativamente
top_sectores <- bind_rows(
  base %>% filter(ANIO == 2019, NORDEMP %in% solo_2019, !is.na(CIIU4)) %>%
    count(CIIU4, sort = TRUE) %>% slice_max(n, n = 5) %>% mutate(grupo = "Solo en 2019"),
  base %>% filter(ANIO == 2022, NORDEMP %in% solo_2022, !is.na(CIIU4)) %>%
    count(CIIU4, sort = TRUE) %>% slice_max(n, n = 5) %>% mutate(grupo = "Solo en 2022")
) %>% select(grupo, CIIU4, n)

ver(top_sectores, filas = 12)
guardar_tabla(top_sectores, "T5c_top_sectores_firmas_exclusivas",
              "Tabla 5c. Los 5 sectores (CIIU4) más frecuentes en cada grupo exclusivo (Bite)", decimales = 0)

cat("\nCÓMO LEER: si la composición por tamaño y sector de 'solo 2019' y 'solo\n",
    "2022' es parecida entre sí y a la de 'en ambas', el cambio de muestra al\n",
    "mover la exposición no está sesgando el resultado por composición\n",
    "observable. Si difieren sistemáticamente (por ejemplo, 'solo 2019' son\n",
    "predominantemente pequeñas o concentradas en pocos sectores), parte del\n",
    "cambio en p_previos (de 0,455 a 0,00046) podría venir de la muestra, no\n",
    "solo de la especificación -- exactamente el mismo tipo de descomposición\n",
    "que se hizo con el bug de controles en 10_bug_controles.R (ahí, A=B\n",
    "exacto; aquí puede no serlo).\n")

# --- La prueba directa: mismo test, restringido a la muestra COMÚN --------------
# La comparación de arriba es descriptiva. La prueba directa es la misma
# lógica de la celda B de 10_bug_controles.R: si el rechazo de las tendencias
# previas con exposición 2019 sobrevive al restringir la muestra a las MISMAS
# firmas que tiene la especificación de 2022 (columna 'en_ambas'), el cambio
# de p_previos NO viene de la muestra -- viene de la especificación
# (exposición 2019 vs. 2022). Si el rechazo desaparece al restringir, la
# muestra sí importa.
titulo("5b. ¿EL RECHAZO SOBREVIVE EN LA MUESTRA COMÚN (RESTRINGIDA A 'EN AMBAS')?")

wald_muestra_comun <- bind_rows(lapply(names(MEDIDAS), function(m) {
  firmas_2019_m <- exposicion_2019 %>% filter(!is.na(.data[[paste0(m, "_de")]])) %>% pull(NORDEMP)
  firmas_2022_m <- exposicion_2022 %>% filter(!is.na(.data[[paste0(m, "_de")]])) %>% pull(NORDEMP)
  comunes <- intersect(firmas_2019_m, firmas_2022_m)

  base_comun <- base_evento_2019 %>% filter(NORDEMP %in% comunes)
  ev_comun <- estudio_evento("log_empleo", m, base_comun)
  if (is.null(ev_comun) || is.null(ev_comun$wald_previos)) return(NULL)
  w <- ev_comun$wald_previos

  tibble(medida = MEDIDAS[[m]], clave = m,
         n_firmas_comunes = length(comunes),
         n_obs_muestra_comun = ev_comun$n,
         estadistico_F = w$stat, df1 = w$df1, df2 = w$df2, p_valor = w$p,
         rechaza_al_5pct = w$p < 0.05)
}))

ver(wald_muestra_comun)
guardar_tabla(wald_muestra_comun, "T5d_tendencias_previas_muestra_comun",
              "Tabla 5d. Test conjunto de tendencias previas (exposición 2019), restringido a las firmas que también tienen exposición válida en 2022")

comparacion_muestra_directa <- test_conjunto %>%
  filter(exposicion == "2019 (celda limpia)") %>%
  select(medida, clave, p_valor_muestra_completa = p_valor, n_obs_completa = n_obs) %>%
  left_join(wald_muestra_comun %>% select(clave, p_valor_muestra_comun = p_valor,
                                          n_obs_muestra_comun, rechaza_al_5pct),
            by = "clave")

ver(comparacion_muestra_directa)
guardar_tabla(comparacion_muestra_directa, "T5e_comparacion_directa_muestra_vs_especificacion",
              "Tabla 5e. p-valor del test conjunto: muestra completa (todas las firmas con exposición 2019 válida) vs. muestra común (también válidas en 2022)")

cat("\nCÓMO LEER: si 'rechaza_al_5pct' sigue siendo TRUE en la muestra común\n",
    "para una medida, el rechazo de esa medida NO viene de las firmas\n",
    "exclusivas de 2019 (que son más pequeñas y muchas no llegan a 2022) --\n",
    "viene de la especificación (exposición 2019) en sí misma, sobre las\n",
    "MISMAS firmas que usa la especificación de 2022. Si se vuelve FALSE, la\n",
    "muestra sí es parte de la explicación.\n")


# ==============================================================================
# 6. LAS DOS SERIES, LADO A LADO (EXPOSICIÓN 2019 Y 2022)
# ==============================================================================
titulo("6. COEFICIENTES AÑO A AÑO, EXPOSICIÓN 2019 Y 2022, LADO A LADO")

coef_2022 <- bind_rows(lapply(names(MEDIDAS), function(m) {
  ev <- eventos_2022[[m]]
  if (is.null(ev)) return(NULL)
  ev$tabla %>% mutate(medida = MEDIDAS[[m]], clave = m, n = ev$n) %>%
    select(medida, clave, anio, efecto_pct, ee_pct, ic95_inf_pct, ic95_sup_pct, p_valor, significancia, n)
}))

guardar_tabla(coef_2022, "T6a_coeficientes_anio_a_anio_exposicion2022",
              "Tabla 6a. Coeficientes año a año (frente a 2022), exposición de 2022 (estándar, contaminada), las cinco medidas")

lado_a_lado <- coef_2019 %>%
  select(medida, clave, anio, efecto_2019 = efecto_pct, ee_2019 = ee_pct,
         p_2019 = p_valor, sig_2019 = significancia, n_2019 = n) %>%
  full_join(
    coef_2022 %>% select(medida, clave, anio, efecto_2022 = efecto_pct, ee_2022 = ee_pct,
                         p_2022 = p_valor, sig_2022 = significancia, n_2022 = n),
    by = c("medida", "clave", "anio")
  ) %>%
  arrange(medida, anio)

ver(lado_a_lado, filas = 60)
guardar_tabla(lado_a_lado, "T6b_coeficientes_lado_a_lado",
              "Tabla 6b. Coeficientes año a año, exposición 2019 y 2022 lado a lado, las cinco medidas", decimales = 3)

g1 <- ggplot(bind_rows(
  coef_2019 %>% mutate(exposicion = "2019 (celda limpia)"),
  coef_2022 %>% mutate(exposicion = "2022 (estándar)")
), aes(x = anio, y = efecto_pct, color = exposicion)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  facet_wrap(~ medida, scales = "free_y") +
  scale_color_manual(values = c(`2019 (celda limpia)` = "#C00000", `2022 (estándar)` = "#1F4E79")) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  labs(title = "Empleo: coeficientes año a año, exposición 2019 vs. 2022",
       subtitle = "Efecto de una DE más de exposición, frente a 2022 (año de referencia)",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = "Controles: firma, año, sector x año, tamaño x año, departamento x año (fijados en 2022). 2020 excluido.") +
  tema_tesis
guardar_grafico(g1, "G6_coeficientes_lado_a_lado", ancho = 12, alto = 7)


if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")
