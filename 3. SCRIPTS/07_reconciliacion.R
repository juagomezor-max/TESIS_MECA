# ==============================================================================
# 07_reconciliacion.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# ¿Por qué el primer eslabón da 4,03% en el póster (event study, panel) y
# 2,98% en 03_primer_eslabon_medidas.R (corte transversal)?
#
# No son dos resultados contradictorios: son dos especificaciones distintas del
# mismo objeto, y hasta ahora nadie ha verificado qué decisión concreta genera
# la brecha. Este script la descompone cambiando UN elemento a la vez.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/panel_analitico_firma_eam.rds
# Salidas:  4. RESULTADOS/Reconciliacion/
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulo:     3. Identificación
# Pregunta:     ¿Por qué circulan cifras distintas del primer eslabón, y cuál
#               es la principal?
# Depende de:   03_primer_eslabon_medidas.R (da 2,98%),
#               04_decision_medida.R (da 1,15%, celda limpia)
#
# GLOSARIO DE CIFRAS -- este es el script que las ordena, así que quedan aquí
# con su definición exacta, no solo el número:
#   4,03%  Coeficiente de 2023 del event study en panel (post:bite, ver
#          sección 2.A). CIFRA PRINCIPAL DE LA TESIS.
#   4,80%  El mismo salto de 2023 ajustado por la pendiente previa (lectura
#          B) -- NO es "el cambio típico de 2016-2019" como quedó escrito en
#          el póster (ver el pendiente de la sección 6, punto 6: esa
#          confusión sigue sin resolverse).
#   2,98%  Tasa de crecimiento del costo laboral 2022-2023 en corte
#          transversal, sin efectos fijos (03_primer_eslabon_medidas.R, sección 1).
#   1,15%  Celda limpia: exposición 2019 contra crecimiento 2022-2023, rompe
#          el traslape aritmético (04_decision_medida.R, sección 3). Es la
#          estimación más creíble, aunque no la más citada.
#   -1,95% Coeficiente de 2023 con exposición 2022 y el outcome que arrastra
#          año base 2019 -- exposición post-tratamiento, no interpretable
#          (04_decision_medida.R, sección 4).
# ------------------------------------------------------------------------------


# ==============================================================================
# LAS DOS ESPECIFICACIONES Y POR QUÉ IMPORTA CUÁL SE USA
# ==============================================================================
#
#                        PÓSTER (4,03%)                VALIDACION/20 (2,98%)
#   Marco                panel 2015-2024,              corte transversal de la
#                        estudio de evento             tasa de crecimiento
#   Efectos fijos        firma + año                   ninguno (es un corte)
#   Controles            sector x año, tamaño x año,   sector, depto, tamaño
#                        departamento x año            en niveles
#   Outcome              C3R10C3 / empleo_total        C3R10C3 / empleo sin
#                                                      propietarios
#   Extremos             winsorización 1/99            filtro de plausibilidad
#                                                      (Bite > 1,3 -> NA)
#   Errores              agrupados por firma           robustos
#
# Ninguna de esas diferencias es un error. Son decisiones distintas, y cada una
# es defendible por separado.
#
# HAY UN ARGUMENTO FUERTE A FAVOR DEL MARCO EN PANEL que conviene tener claro:
# la elasticidad implícita de la tesis (efecto sobre empleo dividido por efecto
# sobre costo laboral) solo tiene interpretación limpia si las dos piezas vienen
# del MISMO marco. El resultado de empleo está estimado en panel. Si el
# denominador se toma de un corte transversal, el cociente mezcla dos estimandos
# distintos y deja de ser una elasticidad bien definida.
#
# Por eso la recomendación por defecto es quedarse con la especificación en
# panel, y usar este script para poder explicar en una frase por qué el corte
# transversal da un número menor.
#
# ESTE SCRIPT NO ELIGE. Produce la descomposición para que la elección sea
# informada y quede documentada.
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

CARPETA <- file.path("4. RESULTADOS", "Reconciliacion")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 20) {
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

LIMITE_BITE <- 1.3


# ==============================================================================
# 1. DATOS
# ==============================================================================
titulo("1. DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

bite <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO))) %>%
  filter(ANIO == 2022) %>%
  select(NORDEMP, Bite2022_obreros)

# Las dos definiciones de costo laboral por trabajador que están en juego.
# Difieren solo en el denominador: si se incluyen o no los propietarios, que no
# tienen remuneración fija por definición y por tanto no deberían estar en un
# denominador cuyo numerador es la nómina.
base <- panel %>%
  mutate(
    empleo_con_propietarios = empleo_total_sin_propietarios +
      obreros_propietarios + administrativos_propietarios +
      profesional_tecnico_propietarios,
    costo_sin_prop = ifelse(empleo_total_sin_propietarios > 0 &
                              costos_totales_personal_total_c3r10c3 > 0,
                            costos_totales_personal_total_c3r10c3 /
                              empleo_total_sin_propietarios, NA_real_),
    costo_con_prop = ifelse(empleo_con_propietarios > 0 &
                              costos_totales_personal_total_c3r10c3 > 0,
                            costos_totales_personal_total_c3r10c3 /
                              empleo_con_propietarios, NA_real_)
  ) %>%
  left_join(bite, by = "NORDEMP") %>%
  mutate(
    ANIO_F = factor(ANIO),
    post = as.integer(ANIO >= 2023),
    sector_2022 = factor(CIIU4),
    depto_2022 = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")),
    log_costo_sin_prop = log(ifelse(costo_sin_prop > 0, costo_sin_prop, NA)),
    log_costo_con_prop = log(ifelse(costo_con_prop > 0, costo_con_prop, NA))
  )

# Comparación de las dos definiciones de outcome
comparacion_outcome <- base %>%
  filter(ANIO == 2022) %>%
  summarise(
    firmas = n(),
    con_propietarios = sum(obreros_propietarios + administrativos_propietarios +
                             profesional_tecnico_propietarios > 0, na.rm = TRUE),
    correlacion = cor(costo_sin_prop, costo_con_prop, use = "complete.obs"),
    diferencia_mediana_pct = 100 * median(costo_sin_prop / costo_con_prop - 1,
                                          na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "concepto", values_to = "valor")

ver(comparacion_outcome)
cat("\nLECTURA: si pocas firmas tienen propietarios y la correlación es alta,\n",
    "la definición del outcome no puede explicar la brecha entre 4,03% y 2,98%.\n")

# --- Las dos formas de tratar los extremos --------------------------------------
# El póster winsoriza (recorta al 1% y 99%, conservando la firma con un valor
# modificado). 03_primer_eslabon_medidas.R aplica un filtro de
# plausibilidad (Bite > 1,3 pasa a NA, porque implica un trabajador que
# cuesta menos del mínimo, lo cual es imposible en el sector formal salvo
# por medio tiempo o error de reporte).
#
# Son decisiones distintas y hay que ver cuánto mueven el resultado: la
# winsorización conserva firmas mal reportadas con un valor inventado; el filtro
# las elimina.

limites <- quantile(base$Bite2022_obreros, probs = c(0.01, 0.99), na.rm = TRUE)

base <- base %>%
  mutate(
    bite_winsor = pmin(pmax(Bite2022_obreros, limites[1]), limites[2]),
    bite_filtro = ifelse(!is.na(Bite2022_obreros) & Bite2022_obreros > 0 &
                           Bite2022_obreros <= LIMITE_BITE,
                         Bite2022_obreros, NA_real_),
    bite_winsor_de = bite_winsor / sd(bite_winsor, na.rm = TRUE),
    bite_filtro_de = bite_filtro / sd(bite_filtro, na.rm = TRUE)
  )

cat("\nTratamiento de extremos:\n")
cat("  Límites de winsorización (1% y 99%):", round(limites, 4), "\n")
cat("  Firmas con Bite >", LIMITE_BITE, "(pasan a NA con el filtro):",
    sum(base$Bite2022_obreros[base$ANIO == 2022] > LIMITE_BITE, na.rm = TRUE), "\n")


# ==============================================================================
# 2. LAS DOS ESPECIFICACIONES ORIGINALES
# ==============================================================================
titulo("2. REPRODUCIR LAS DOS ESPECIFICACIONES")

# --- A. Marco en panel (la del póster) -----------------------------------------
# Ventana 2015-2024 sin 2020, efectos fijos de firma y año, controles
# interactuados con año. El coeficiente de post:bite dice cuánto más creció el
# costo laboral después de 2023 en una firma con una DE más de exposición.

panel_estimacion <- base %>%
  filter(ANIO %in% c(2015:2019, 2021:2024))

EFECTOS_PANEL <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"

estimar_panel <- function(outcome, tratamiento, efectos = EFECTOS_PANEL,
                          datos = panel_estimacion, etiqueta = "") {
  formula <- as.formula(paste0(outcome, " ~ post:", tratamiento, " | ", efectos))
  modelo <- tryCatch(feols(formula, data = datos, cluster = ~NORDEMP),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  fila <- coeftable(modelo)[paste0("post:", tratamiento), ]
  tibble(especificacion = etiqueta,
         coeficiente = fila[["Estimate"]],
         error_estandar = fila[["Std. Error"]],
         p_valor = fila[["Pr(>|t|)"]],
         efecto_pct = 100 * fila[["Estimate"]],
         observaciones = nobs(modelo),
         firmas = n_distinct(datos$NORDEMP[obs(modelo)]))
}

# --- B. Corte transversal (la de 03_primer_eslabon_medidas.R) -------
# Una fila por firma: la tasa de crecimiento del costo laboral 2022-2023.

corte <- base %>%
  filter(ANIO %in% c(2022, 2023)) %>%
  select(NORDEMP, ANIO, costo_sin_prop, costo_con_prop,
         bite_winsor_de, bite_filtro_de, sector_2022, depto_2022, tamano_2022) %>%
  pivot_wider(names_from = ANIO,
              values_from = c(costo_sin_prop, costo_con_prop),
              names_sep = "_") %>%
  mutate(
    crecimiento_sin_prop = log(costo_sin_prop_2023) - log(costo_sin_prop_2022),
    crecimiento_con_prop = log(costo_con_prop_2023) - log(costo_con_prop_2022)
  )

CONTROLES_CORTE <- "sector_2022 + depto_2022 + tamano_2022"

estimar_corte <- function(outcome, tratamiento, controles = CONTROLES_CORTE,
                          datos = corte, vcov_tipo = "hetero", etiqueta = "") {
  formula <- as.formula(paste0(outcome, " ~ ", tratamiento, " | ", controles))
  modelo <- tryCatch(feols(formula, data = datos, vcov = vcov_tipo),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  fila <- coeftable(modelo)[tratamiento, ]
  tibble(especificacion = etiqueta,
         coeficiente = fila[["Estimate"]],
         error_estandar = fila[["Std. Error"]],
         p_valor = fila[["Pr(>|t|)"]],
         efecto_pct = 100 * fila[["Estimate"]],
         observaciones = nobs(modelo),
         firmas = nobs(modelo))
}

originales <- bind_rows(
  estimar_panel("log_costo_con_prop", "bite_winsor_de",
                etiqueta = "A. Panel, outcome con propietarios, winsorizado"),
  estimar_corte("crecimiento_sin_prop", "bite_filtro_de",
                etiqueta = "B. Corte, outcome sin propietarios, filtrado")
) %>%
  mutate(significancia = estrellas(p_valor))

ver(originales)
guardar_tabla(originales, "T01_especificaciones_originales",
              "Tabla 1. Las dos especificaciones tal como están en el póster y en 03_primer_eslabon_medidas.R")

cat("\nSi estos dos números reproducen 4,03% y 2,98%, la descomposición de la\n",
    "sección 3 es válida. Si no, primero hay que averiguar por qué no reproducen.\n")


# ==============================================================================
# 3. DESCOMPOSICIÓN: UN CAMBIO A LA VEZ
# ==============================================================================
titulo("3. DESCOMPOSICIÓN DE LA BRECHA")

# Partimos de la especificación del póster y vamos cambiando un elemento a la
# vez hasta llegar a la de 03_primer_eslabon_medidas.R. Cada fila aísla el efecto de UNA decisión.
# Así, en lugar de "dan distinto", tendremos "la diferencia viene de X".

pasos <- bind_rows(
  
  # Punto de partida: la del póster
  estimar_panel("log_costo_con_prop", "bite_winsor_de",
                etiqueta = "0. Póster: panel + con propietarios + winsorizado"),
  
  # Paso 1: cambiar el outcome (quitar propietarios del denominador)
  estimar_panel("log_costo_sin_prop", "bite_winsor_de",
                etiqueta = "1. + outcome sin propietarios"),
  
  # Paso 2: cambiar el tratamiento de extremos
  estimar_panel("log_costo_sin_prop", "bite_filtro_de",
                etiqueta = "2. + filtro de plausibilidad en vez de winsorización"),
  
  # Paso 3: quitar los controles interactuados con año, dejando niveles
  estimar_panel("log_costo_sin_prop", "bite_filtro_de",
                efectos = "NORDEMP + ANIO_F",
                etiqueta = "3. + sin controles interactuados con año"),
  
  # Paso 4: el corte transversal completo
  estimar_corte("crecimiento_sin_prop", "bite_filtro_de",
                etiqueta = "4. 03_primer_eslabon_medidas.R: corte transversal")
) %>%
  mutate(
    significancia = estrellas(p_valor),
    cambio_vs_anterior_pct = efecto_pct - lag(efecto_pct)
  )

ver(pasos)
guardar_tabla(pasos, "T02_descomposicion_brecha",
              "Tabla 2. De la especificación del póster a la de 03_primer_eslabon_medidas.R, un cambio a la vez")

cat("\nCÓMO LEER: 'cambio_vs_anterior_pct' dice cuánto mueve cada decisión. El\n",
    "paso con el salto más grande es el que explica la brecha. Si ningún paso\n",
    "domina, la diferencia se reparte y ninguna de las dos cifras está mal:\n",
    "simplemente responden preguntas ligeramente distintas.\n")

grafico_pasos <- ggplot(pasos, aes(x = reorder(especificacion, seq_along(especificacion)),
                                   y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_col(fill = COLOR_BAJA, width = 0.7) +
  geom_text(aes(label = round(efecto_pct, 2)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "De dónde viene la diferencia entre las dos estimaciones",
       subtitle = "Cada barra cambia un elemento respecto de la anterior",
       x = NULL, y = "Efecto por DE de exposición (%)",
       caption = "El paso con el salto más grande es el que explica la brecha.") +
  tema_tesis
guardar_grafico(grafico_pasos, "G01_descomposicion")


# ==============================================================================
# 4. MUESTRA COMÚN
# ==============================================================================
titulo("4. LAS DOS ESPECIFICACIONES SOBRE LA MISMA MUESTRA")

# Parte de la brecha puede venir de que cada especificación usa firmas distintas:
# el panel exige observar la firma en varios años, el corte solo en 2022 y 2023.
# Aquí estimamos las dos sobre las firmas que están en ambas, para separar el
# efecto de la especificación del efecto de la muestra.

firmas_panel <- panel_estimacion %>%
  filter(!is.na(log_costo_sin_prop), !is.na(bite_filtro_de)) %>%
  distinct(NORDEMP) %>% pull(NORDEMP)

firmas_corte <- corte %>%
  filter(!is.na(crecimiento_sin_prop), !is.na(bite_filtro_de)) %>%
  pull(NORDEMP)

firmas_comunes <- intersect(firmas_panel, firmas_corte)

cat("Firmas en el panel:", length(firmas_panel), "\n")
cat("Firmas en el corte:", length(firmas_corte), "\n")
cat("Firmas en ambas:", length(firmas_comunes), "\n")

muestra_comun <- bind_rows(
  estimar_panel("log_costo_sin_prop", "bite_filtro_de",
                datos = filter(panel_estimacion, NORDEMP %in% firmas_comunes),
                etiqueta = "Panel, muestra común"),
  estimar_corte("crecimiento_sin_prop", "bite_filtro_de",
                datos = filter(corte, NORDEMP %in% firmas_comunes),
                etiqueta = "Corte, muestra común")
) %>%
  mutate(significancia = estrellas(p_valor))

ver(muestra_comun)
guardar_tabla(muestra_comun, "T03_muestra_comun",
              "Tabla 3. Las dos especificaciones sobre las mismas firmas")

cat("\nCÓMO LEER: si al igualar la muestra los coeficientes convergen, la brecha\n",
    "era de composición y no de especificación. Si siguen distintos, la\n",
    "diferencia es real y viene del marco econométrico.\n")


# ==============================================================================
# 5. QUÉ ESTIMA CADA UNA
# ==============================================================================
titulo("5. ESTIMANDOS: NO MIDEN LO MISMO")

# Esta es la razón conceptual, y probablemente la principal, por la que los dos
# números difieren. Vale la pena tenerla clara antes de mirar las cifras.
#
# EL CORTE TRANSVERSAL compara el costo laboral de 2023 contra el de 2022. Mide
# el salto de un año.
#
# EL PANEL con post = (ANIO >= 2023) compara el PROMEDIO de 2023-2024 contra el
# PROMEDIO de 2015-2022. Mide cuánto se separó el nivel del costo laboral de las
# firmas expuestas después del choque, respecto de su propia trayectoria previa.
#
# Si el efecto se acumula (en 2024 la brecha es mayor que en 2023), el panel da
# un número mayor que el corte, sin que ninguno esté mal.
#
# Lo verificamos con un estudio de evento: si el coeficiente de 2024 es mayor
# que el de 2023, la acumulación existe y explica parte de la brecha.

evento <- tryCatch(
  feols(log_costo_sin_prop ~ i(ANIO_F, bite_filtro_de, ref = "2022") |
          NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F,
        data = panel_estimacion, cluster = ~NORDEMP),
  error = function(e) NULL)

if (!is.null(evento)) {
  coefs <- as.data.frame(coeftable(evento))
  tabla_evento <- tibble(
    anio = as.integer(gsub("ANIO_F::|:bite_filtro_de", "", rownames(coefs))),
    coeficiente = coefs[["Estimate"]],
    error_estandar = coefs[["Std. Error"]],
    p_valor = coefs[["Pr(>|t|)"]]
  ) %>%
    bind_rows(tibble(anio = 2022L, coeficiente = 0, error_estandar = 0,
                     p_valor = NA_real_)) %>%
    mutate(efecto_pct = 100 * coeficiente,
           significancia = estrellas(p_valor)) %>%
    arrange(anio)
  
  ver(tabla_evento)
  guardar_tabla(tabla_evento, "T04_evento_costo_laboral",
                "Tabla 4. Costo laboral por trabajador, año por año frente a 2022")
  
  grafico_evento <- ggplot(tabla_evento, aes(x = anio, y = efecto_pct)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                      ymax = 100 * (coeficiente + 1.96 * error_estandar)),
                  width = 0.2, color = COLOR_BAJA) +
    geom_point(size = 2.5, color = ifelse(tabla_evento$anio >= 2023,
                                          COLOR_ALTA, COLOR_BAJA)) +
    scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
    labs(title = "Costo laboral por trabajador de las firmas más expuestas",
         subtitle = "Diferencia frente a 2022, por DE de exposición",
         x = NULL, y = "Efecto (%)",
         caption = "Si el coeficiente de 2024 supera al de 2023, el efecto se acumula y el panel da más que el corte.") +
    tema_tesis
  guardar_grafico(grafico_evento, "G02_evento_costo_laboral")
  
  efecto_2023 <- tabla_evento$efecto_pct[tabla_evento$anio == 2023]
  efecto_2024 <- tabla_evento$efecto_pct[tabla_evento$anio == 2024]
  
  cat("\nEfecto en 2023:", round(efecto_2023, 3), "%\n")
  cat("Efecto en 2024:", round(efecto_2024, 3), "%\n")
  if (length(efecto_2024) > 0 && length(efecto_2023) > 0) {
    if (efecto_2024 > efecto_2023) {
      cat("-> El efecto SE ACUMULA. El panel promedia 2023 y 2024, así que da un\n",
          "   número mayor que el corte, que solo mira 2023. Ninguno está mal.\n")
    } else {
      cat("-> El efecto NO se acumula. La brecha entre panel y corte viene de\n",
          "   otra cosa: revisar la descomposición de la sección 3.\n")
    }
  }
  
  cat("\nOJO CON LOS AÑOS PREVIOS: si los coeficientes de 2015-2021 no son cero,\n",
      "hay tendencia diferencial en el costo laboral antes del choque, y eso\n",
      "afecta la interpretación de CUALQUIERA de las dos cifras.\n")
}


# ==============================================================================
# 6. RESUMEN Y RECOMENDACIÓN
# ==============================================================================
titulo("6. RESUMEN")

resumen <- bind_rows(
  mutate(originales, bloque = "Especificaciones originales"),
  mutate(muestra_comun, bloque = "Sobre la misma muestra")
) %>%
  select(bloque, especificacion, efecto_pct, p_valor, significancia,
         observaciones, firmas)

ver(resumen)
guardar_tabla(resumen, "T05_resumen",
              "Tabla 5. Resumen de la reconciliación")

cat("
CÓMO DECIDIR, Y QUÉ ESCRIBIR:

  1. Si la descomposición (sección 3) muestra que UN paso explica casi toda la
     brecha, la decisión es sobre ese elemento concreto y hay que justificarla
     en una frase. Por ejemplo, si todo viene del outcome: los propietarios no
     tienen remuneración fija, así que no deberían estar en un denominador cuyo
     numerador es la nómina, y la versión sin propietarios es la correcta.

  2. Si la brecha se reparte entre varios pasos, ninguna cifra está mal:
     responden preguntas ligeramente distintas. Reportar la del panel como
     principal, por coherencia con el resultado de empleo (ver el punto 4), y
     mencionar la del corte como robustez.

  3. Si el estudio de evento (sección 5) muestra acumulación, decirlo
     explícitamente: 'el efecto sobre el costo laboral crece entre 2023 y 2024;
     la estimación en panel recoge el promedio de los dos años, la de corte solo
     el primero'. Eso convierte una discrepancia aparente en un hallazgo.

  4. RAZÓN PARA PREFERIR EL PANEL: la elasticidad implícita de la tesis (efecto
     sobre empleo dividido por efecto sobre costo laboral) solo tiene
     interpretación limpia si las dos piezas vienen del mismo marco. El
     resultado de empleo está estimado en panel. Tomar el denominador de un
     corte transversal mezcla dos estimandos y el cociente deja de ser una
     elasticidad bien definida.

  5. En cualquier caso, la robustez de la celda limpia
     (04_decision_medida.R: exposición medida en 2019, efecto de
     1,15%) va reportada al lado, y también la advertencia de que la
     relación por tramos se aplana en el quintil más expuesto.
     [NOTA de esta revisión, contradicción NO resuelta por cuenta propia:
     este punto dice que la relación por tramos 'se aplana en el quintil
     más expuesto'. 04_decision_medida.R (sección 6) dice, con
     los mismos controles, que la relación es MONOTÓNICA CRECIENTE -- no
     aplanada. No se sabe cuál de las dos descripciones quedó desactualizada
     al escribir la otra. Verificar contra la tabla T04_efecto_por_tramos de
     04_decision_medida.R antes de escribir cualquiera de las dos afirmaciones en
     la tesis.]

  6. PENDIENTE DEL PÓSTER QUE ESTE SCRIPT NO RESUELVE: la casilla que dice
     '+4,03% frente al cambio típico de 2016-2019: +4,80%'. Si esas dos
     cifras miden lo mismo, el diferencial del año del choque sería MENOR
     que el de un año normal, y eso contradice el titular. Hay que aclarar
     qué mide cada una antes de llevar esa comparación al documento -- ver
     el glosario de cifras en el encabezado de este script: 4,80% se definió
     en otra parte como 'el salto ajustado por la pendiente previa', que NO
     es lo mismo que 'el cambio típico de 2016-2019'. Esta discrepancia de
     definición sigue sin resolverse.
")

save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
cat("\nCompendio guardado con", length(compendio), "tablas.\n")

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")
