# ==============================================================================
# 03_primer_eslabon.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# PRIMERA ETAPA: ¿cuál de las cinco medidas de exposición predice el aumento
# diferencial del costo laboral por trabajador en 2023?
#
# Esta es la prueba decisiva. La regla comiteada en NOTA_DECISIONES.md dice que
# la medida principal se elige por este resultado y NO se revisa después según
# los resultados de empleo. Este script produce esa decisión.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/exposicion_alternativa_2022.rds
#           1. DATOS/panel_analitico_firma_eam.rds   (para Bite y Exposure)
# Salidas:  4. RESULTADOS/Primer_eslabon/
#
# Para correrlo abrimos TESIS_MECA.Rproj.
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulo:     2. Medición
# Pregunta:     ¿Cuál de las cinco medidas de exposición predice el aumento
#               diferencial del costo laboral en 2023?
# Cifra clave:  2,98% por DE de Bite — efecto sobre la TASA DE CRECIMIENTO del
#               costo laboral 2022-2023, en corte transversal. No es la cifra
#               principal de la tesis (esa es 4,03%, del event study).
# Depende de:   construccion/exposicion_alternativa.R
# Se relaciona: validacion/21_decision_medida.R (cierra la decisión de medida)
#               validacion/22_reconciliacion.R (explica por qué 2,98 ≠ 4,03)
#
# SUPERADO POR VERSIONES POSTERIORES:
#   - El placebo 2018-2019 de la sección 7 NO es informativo. Ver la nota en esa
#     sección. La estimación se conserva; su lectura cambia.
#   - La concentración en el quintil 5 de la sección 8.3 es un hallazgo de
#     medianas SIN controles. Con controles la relación es monotónica
#     (validacion/21, sección 6).
# ------------------------------------------------------------------------------


# ==============================================================================
# LA IDEA Y LA TRAMPA
# ==============================================================================
#
# La cadena que sostiene toda la tesis es:
#
#   sube el mínimo -> a las firmas expuestas les sube más el costo laboral
#                  -> por eso ajustan empleo
#
# Si el segundo eslabón no se cumple, el tercero no significa nada. Eso es lo
# que medimos aquí:
#
#   crecimiento del costo laboral por trabajador 2022->2023 = a + b*exposicion
#
# LA TRAMPA (sesgo de división). Varias medidas tienen el costo laboral de 2022
# en el denominador, y el outcome tiene ese mismo costo de 2022 en la base:
#
#   exposicion ~ 1 / costo_2022        outcome = log(costo_2023) - log(costo_2022)
#
# Si una firma reportó por azar un costo bajo en 2022, sale "muy expuesta" Y
# además su crecimiento hacia 2023 sale alto, sin que haya pasado nada
# económico. Eso genera una correlación positiva puramente mecánica.
#
# Por eso corremos TODO dos veces: con la exposición medida en 2022 y con la
# exposición medida en 2019. Si el coeficiente sobrevive con base 2019, es
# economía. Si solo aparece con base 2022, es aritmética.
#
# El orden de vulnerabilidad al sesgo, de peor a mejor:
#   golpe_costo  (su denominador ES el costo laboral del outcome)
#   Bite, golpe_c, golpe_a  (denominador salarial, relacionado pero no idéntico)
#   Exposure2022_obreros  (composición, no usa salarios: inmune)
#
# La sección 7 corre además un placebo 2018->2019. La idea original era: si la
# exposición predice igual de bien el crecimiento en un período sin choque, no
# está capturando el de 2023. Esa idea no se sostiene: el placebo da negativo
# y significativo en las CINCO medidas, incluida Exposure (que no usa salarios
# y no debería compartir el sesgo de división de las otras cuatro) -- el signo
# sale de una cadena aritmética casi inevitable, no de un problema de diseño.
# Ver la nota completa en la sección 7. La estimación se conserva como registro
# de que se probó; ya no se lee como validación del diseño.
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

CARPETA <- file.path("4. RESULTADOS", "Primer_eslabon")
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
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
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
# 1. OUTCOME: CRECIMIENTO DEL COSTO LABORAL POR TRABAJADOR
# ==============================================================================
titulo("1. CONSTRUCCIÓN DEL OUTCOME")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

# Costo laboral por trabajador. Usamos C3R10 (costo total del personal) sobre
# empleo sin propietarios: las dos cosas cubren al personal ocupado, así que
# numerador y denominador miden la misma gente. El diagnóstico del residuo
# confirmó que C3R10 = suma(R1..R9) + R4CSAP (apoyo a aprendices), y que los
# aprendices sí están en el denominador, así que la inclusión es consistente.
costo_por_trabajador <- panel %>%
  mutate(
    costo_trabajador = ifelse(
      empleo_total_sin_propietarios > 0 & costos_totales_personal_total_c3r10c3 > 0,
      costos_totales_personal_total_c3r10c3 / empleo_total_sin_propietarios,
      NA_real_
    )
  ) %>%
  select(NORDEMP, ANIO, costo_trabajador, empleo_total_sin_propietarios) %>%
  filter(ANIO %in% c(2018, 2019, 2022, 2023))

# Pasamos a formato ancho: una fila por firma con el costo de cada año
costos_ancho <- costo_por_trabajador %>%
  select(NORDEMP, ANIO, costo_trabajador) %>%
  pivot_wider(names_from = ANIO, values_from = costo_trabajador,
              names_prefix = "costo_")

# Outcome principal y outcome del placebo
outcomes <- costos_ancho %>%
  mutate(
    crecimiento_2023 = log(costo_2023) - log(costo_2022),
    crecimiento_2019 = log(costo_2019) - log(costo_2018)
  )

cat("Firmas con crecimiento 2022->2023:", sum(!is.na(outcomes$crecimiento_2023)), "\n")
cat("Firmas con crecimiento 2018->2019:", sum(!is.na(outcomes$crecimiento_2019)), "\n")

# El salario mínimo subió 16% nominal en 2023. El crecimiento mediano del costo
# laboral debería estar en ese orden de magnitud. Si sale muy lejos, hay un
# problema con el outcome y no se debe seguir.
cat("\nCrecimiento del costo laboral por trabajador 2022->2023:\n")
print(round(quantile(outcomes$crecimiento_2023,
                     c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE), 4))
cat("En porcentaje, la mediana es:",
    round(100 * (exp(median(outcomes$crecimiento_2023, na.rm = TRUE)) - 1), 2), "%\n")

# ESTA cifra (tasa de crecimiento 2022-2023 del costo laboral, mediana del
# corte transversal) es UN estimando entre cinco que circulan para "el primer
# eslabón", no la cifra principal de la tesis. Los cinco no son
# intercambiables porque miden objetos distintos: 4,03% es el coeficiente de
# 2023 del event study (la cifra principal); 4,80% es ese mismo salto ajustado
# por la pendiente previa (lectura B); ésta (~2,98% en la corrida de
# referencia) es la mediana de corte transversal que se calcula aquí; 1,15% es
# la celda limpia con exposición medida en 2019 (sin traslape aritmético); y
# -1,95% es el coeficiente de 2023 contra año base 2019. Confundirlas es el
# tipo de error que un jurado detecta de inmediato -- validacion/22_
# reconciliacion.R explica por qué no coinciden.
cat("\nCrecimiento 2018->2019 (placebo):\n")
print(round(quantile(outcomes$crecimiento_2019,
                     c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE), 4))


# ==============================================================================
# 2. MEDIDAS DE EXPOSICIÓN
# ==============================================================================
titulo("2. CARGA DE LAS CINCO MEDIDAS")

alternativas <- read_rds(file.path("1. DATOS", "exposicion_alternativa_2022.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP))

viejas <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO))) %>%
  filter(ANIO == 2022) %>%
  select(NORDEMP, any_of(c("Bite2022_obreros", "Exposure2022_obreros")))

medidas <- alternativas %>%
  left_join(viejas, by = "NORDEMP") %>%
  left_join(outcomes, by = "NORDEMP")

cat("Firmas en la base combinada:", nrow(medidas), "\n")

# --- Filtro de plausibilidad económica -----------------------------------------
# Winsorizar al 1/99 no alcanza: el summary de golpe_c con base 2019 mostró un
# máximo de 6.960, que implica un salario de 1/6960 del mínimo. Eso no es una
# firma que paga poco, es un error de reporte, y basta un puñado para destruir
# cualquier correlación de Pearson (fue lo que hizo que golpe_c 2022 vs 2019
# diera -0,02 en Pearson y 0,72 en Spearman).
#
# Ningún trabajador formal de tiempo completo puede costar menos del mínimo, así
# que un golpe por encima de ~1,3 es imposible salvo por medio tiempo o error.
# Estos valores se vuelven NA, no se recortan: recortarlos los dejaría dentro
# de la muestra con un valor inventado.
LIMITE_GOLPE <- 1.3

plausible <- function(x, limite = LIMITE_GOLPE) {
  ifelse(!is.na(x) & x > 0 & x <= limite, x, NA_real_)
}

cat("\nFirmas excluidas por implausibilidad (golpe > ", LIMITE_GOLPE, "):\n", sep = "")
for (v in c("golpe_c", "golpe_a", "golpe_costo", "Bite2022_obreros")) {
  if (v %in% names(medidas)) {
    cat("  ", v, ": ", sum(medidas[[v]] > LIMITE_GOLPE, na.rm = TRUE), "\n", sep = "")
  }
}

medidas <- medidas %>%
  mutate(
    across(any_of(c("golpe_c", "golpe_a", "golpe_costo", "Bite2022_obreros",
                    "golpe_c_2019", "golpe_a_2019", "golpe_costo_2019")),
           plausible),
    # Exposure es una proporción entre 0 y 1: su filtro es otro
    Exposure2022_obreros = ifelse(!is.na(Exposure2022_obreros) &
                                    Exposure2022_obreros >= 0 &
                                    Exposure2022_obreros <= 1,
                                  Exposure2022_obreros, NA_real_)
  )

# --- Estandarización a desviación estándar 1 -----------------------------------
# Sin esto los coeficientes no son comparables entre medidas: cada una está en
# su propia escala. Con esto, todos los betas se leen igual: "cuánto cambia el
# crecimiento del costo laboral ante una DE más de exposición".
estandarizar <- function(x) x / sd(x, na.rm = TRUE)

MEDIDAS_2022 <- c("Bite2022_obreros", "Exposure2022_obreros",
                  "golpe_c", "golpe_a", "golpe_costo")
MEDIDAS_2019 <- c("golpe_c_2019", "golpe_a_2019", "golpe_costo_2019")

medidas <- medidas %>%
  mutate(across(any_of(c(MEDIDAS_2022, MEDIDAS_2019)), estandarizar,
                .names = "{.col}_de"))

# Controles, fijados en 2022 (el tamaño contemporáneo es bad control: el empleo
# es el outcome de la tesis)
medidas <- medidas %>%
  mutate(
    sector_2022 = factor(CIIU4),
    depto_2022  = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

cobertura_medidas <- tibble(medida = MEDIDAS_2022) %>%
  rowwise() %>%
  mutate(
    firmas_con_medida = sum(!is.na(medidas[[medida]])),
    firmas_con_medida_y_outcome = sum(!is.na(medidas[[medida]]) &
                                        !is.na(medidas$crecimiento_2023))
  ) %>%
  ungroup()

ver(cobertura_medidas)
guardar_tabla(cobertura_medidas, "T01_cobertura_medidas",
              "Tabla 1. Cobertura de cada medida y del outcome", decimales = 0)


# ==============================================================================
# 3. MUESTRA COMÚN
# ==============================================================================
titulo("3. DEFINICIÓN DE LA MUESTRA COMÚN")

# Cada medida cubre un conjunto distinto de firmas (Bite pierde el 17,6% sin
# obreros permanentes; golpe_c recupera 643). Si comparamos los coeficientes
# sobre muestras distintas, la diferencia puede venir de la muestra y no de la
# medida, y no habría forma de distinguirlo.
#
# Por eso reportamos DOS versiones de todo:
#   - muestra propia: cada medida sobre todas las firmas donde está definida
#   - muestra común: solo firmas donde están definidas las cinco
#
# La muestra común es la comparación limpia. La propia dice qué se gana en
# potencia al usar una medida de mayor cobertura.

medidas <- medidas %>%
  mutate(
    en_muestra_comun = !is.na(crecimiento_2023) &
      rowSums(is.na(across(all_of(MEDIDAS_2022)))) == 0
  )

cat("Firmas en la muestra común:", sum(medidas$en_muestra_comun), "\n")
cat("Firmas con outcome pero fuera de la muestra común:",
    sum(!is.na(medidas$crecimiento_2023) & !medidas$en_muestra_comun), "\n")


# ==============================================================================
# 4. FUNCIÓN DE ESTIMACIÓN
# ==============================================================================
titulo("4. ESPECIFICACIÓN")

# Corte transversal: una observación por firma. El outcome ya es un crecimiento,
# así que la diferencia entre firmas ya está tomada; no hacen falta efectos
# fijos de firma ni de año.
#
# Controles: sector (CIIU4), departamento y tamaño, todos fijados en 2022. Son
# los mismos de la especificación principal de la tesis, donde ya se estableció
# que la credibilidad del diseño descansa en ellos y no en la exogeneidad de la
# exposición cruda.
#
# Errores estándar robustos a heterocedasticidad. Al ser corte transversal no
# hay estructura de panel que clusterizar; agrupamos por sector como robustez
# en la sección 8.

CONTROLES <- "sector_2022 + depto_2022 + tamano_2022"

estimar_primer_eslabon <- function(medida, outcome = "crecimiento_2023",
                                   base = medidas, solo_comun = FALSE,
                                   etiqueta = medida, con_controles = TRUE) {
  
  datos <- base
  if (solo_comun) datos <- filter(datos, en_muestra_comun)
  
  variable <- paste0(medida, "_de")
  if (!variable %in% names(datos)) return(NULL)
  
  formula_texto <- if (con_controles) {
    paste0(outcome, " ~ ", variable, " | ", CONTROLES)
  } else {
    paste0(outcome, " ~ ", variable)
  }
  
  modelo <- tryCatch(feols(as.formula(formula_texto), data = datos, vcov = "hetero"),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  
  fila <- coeftable(modelo)[variable, ]
  
  tibble(
    medida          = etiqueta,
    muestra         = if (solo_comun) "Común" else "Propia",
    controles       = if (con_controles) "Sí" else "No",
    coeficiente     = fila[["Estimate"]],
    error_estandar  = fila[["Std. Error"]],
    p_valor         = fila[["Pr(>|t|)"]],
    significancia   = estrellas(fila[["Pr(>|t|)"]]),
    ic95_inferior   = fila[["Estimate"]] - 1.96 * fila[["Std. Error"]],
    ic95_superior   = fila[["Estimate"]] + 1.96 * fila[["Std. Error"]],
    efecto_pct      = 100 * fila[["Estimate"]],
    observaciones   = nobs(modelo),
    r2_ajustado     = fitstat(modelo, "ar2", simplify = TRUE)
  )
}


# ==============================================================================
# 5. RESULTADO PRINCIPAL: LAS CINCO MEDIDAS
# ==============================================================================
titulo("5. PRIMER ESLABÓN: LAS CINCO MEDIDAS")

etiquetas <- c(
  Bite2022_obreros     = "Bite (Kaitz de obreros)",
  Exposure2022_obreros = "Exposure (proporción de obreros)",
  golpe_c              = "Golpe C (nivel salarial, 3 categorías)",
  golpe_a              = "Golpe A (armónica ponderada)",
  golpe_costo          = "Golpe costo (costo laboral total)"
)

resultado_propia <- bind_rows(lapply(MEDIDAS_2022, function(m)
  estimar_primer_eslabon(m, solo_comun = FALSE, etiqueta = etiquetas[[m]])))

resultado_comun <- bind_rows(lapply(MEDIDAS_2022, function(m)
  estimar_primer_eslabon(m, solo_comun = TRUE, etiqueta = etiquetas[[m]])))

primer_eslabon <- bind_rows(resultado_propia, resultado_comun) %>%
  arrange(muestra, desc(abs(coeficiente)))

ver(primer_eslabon, filas = 12)
guardar_tabla(primer_eslabon, "T02_primer_eslabon_cinco_medidas",
              "Tabla 2. Primer eslabón: efecto de una DE de exposición sobre el crecimiento del costo laboral 2022-2023")

cat("\nCÓMO LEER: el coeficiente dice en cuántos puntos log creció más el costo\n",
    "laboral por trabajador en una firma con una desviación estándar más de\n",
    "exposición. Multiplicado por 100, es el cambio porcentual aproximado.\n",
    "La medida que gana es la que combina coeficiente grande, p-valor bajo y,\n",
    "sobre todo, que sobreviva la prueba de la sección 6.\n")

# Gráfico comparativo
grafico_comparacion <- ggplot(primer_eslabon,
                              aes(x = reorder(medida, coeficiente),
                                  y = 100 * coeficiente, color = muestra)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = 100 * ic95_inferior, ymax = 100 * ic95_superior),
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_manual(values = c(`Propia` = COLOR_BAJA, `Común` = COLOR_ALTA)) +
  labs(title = "Primer eslabón: ¿qué medida predice el aumento del costo laboral?",
       subtitle = "Cambio % en el costo laboral por trabajador 2022-2023, por DE de exposición",
       x = NULL, y = "Efecto (%)", color = "Muestra",
       caption = "Controles: sector (CIIU4), departamento y tamaño, fijados en 2022. Errores robustos. IC al 95%.") +
  tema_tesis
guardar_grafico(grafico_comparacion, "G01_primer_eslabon_comparacion")


# ==============================================================================
# 6. LA PRUEBA QUE IMPORTA: BASE 2019
# ==============================================================================
titulo("6. SESGO DE DIVISIÓN: MEDIDAS CON BASE 2019")

# Aquí se separa la economía de la aritmética. Si el coeficiente de la sección 5
# aparece porque el costo de 2022 está en los dos lados de la ecuación, entonces
# al medir la exposición en 2019 debería caer mucho o desaparecer.
#
# Si sobrevive, la medida está capturando una característica real y persistente
# de la firma. Recordar que golpe_c tiene Spearman 0,72 entre 2019 y 2022, así
# que la persistencia existe: el test es informativo.
#
# Bite y Exposure no tienen versión 2019 construida. Si se quiere comparación
# completa, hay que construirlas con la misma lógica del script 02.

etiquetas_2019 <- c(
  golpe_c_2019     = "Golpe C (base 2019)",
  golpe_a_2019     = "Golpe A (base 2019)",
  golpe_costo_2019 = "Golpe costo (base 2019)"
)

disponibles_2019 <- MEDIDAS_2019[MEDIDAS_2019 %in% names(medidas)]

if (length(disponibles_2019) > 0) {
  
  resultado_2019 <- bind_rows(lapply(disponibles_2019, function(m)
    estimar_primer_eslabon(m, solo_comun = FALSE, etiqueta = etiquetas_2019[[m]])))
  
  # Comparación lado a lado con la versión 2022
  comparacion_base <- primer_eslabon %>%
    filter(muestra == "Propia",
           medida %in% c("Golpe C (nivel salarial, 3 categorías)",
                         "Golpe A (armónica ponderada)",
                         "Golpe costo (costo laboral total)")) %>%
    select(medida, coef_base_2022 = coeficiente, p_base_2022 = p_valor) %>%
    mutate(pareja = c("Golpe C", "Golpe A", "Golpe costo")[
      match(medida, c("Golpe C (nivel salarial, 3 categorías)",
                      "Golpe A (armónica ponderada)",
                      "Golpe costo (costo laboral total)"))]) %>%
    left_join(
      resultado_2019 %>%
        mutate(pareja = c("Golpe C", "Golpe A", "Golpe costo")[
          match(medida, c("Golpe C (base 2019)", "Golpe A (base 2019)",
                          "Golpe costo (base 2019)"))]) %>%
        select(pareja, coef_base_2019 = coeficiente, p_base_2019 = p_valor),
      by = "pareja"
    ) %>%
    mutate(
      porcentaje_que_sobrevive = round(100 * coef_base_2019 / coef_base_2022, 1)
    ) %>%
    select(pareja, coef_base_2022, p_base_2022, coef_base_2019, p_base_2019,
           porcentaje_que_sobrevive)
  
  ver(comparacion_base)
  guardar_tabla(comparacion_base, "T03_sesgo_division_base_2019",
                "Tabla 3. Coeficiente del primer eslabón con exposición medida en 2022 y en 2019")
  
  cat("\nCÓMO LEER: si 'porcentaje_que_sobrevive' está cerca de 100, el efecto es\n",
      "económico. Si cae muy por debajo, buena parte del coeficiente de la\n",
      "sección 5 era sesgo de división y no evidencia del choque.\n",
      "Esperamos que golpe_costo caiga más que golpe_c: su denominador ES el\n",
      "costo laboral que aparece en la base del outcome.\n")
  
} else {
  cat("AVISO: no hay medidas con base 2019 en el archivo. Se omite esta prueba,\n",
      "que es la más importante del script.\n")
}


# ==============================================================================
# 7. PLACEBO: 2018 -> 2019
# ==============================================================================
titulo("7. PLACEBO: CRECIMIENTO DEL COSTO LABORAL 2018-2019")

# ESTE PLACEBO NO ES INFORMATIVO. Se conserva la estimación -- va al capítulo 6
# de amenazas a la validez -- pero no se lee como evidencia a favor del diseño.
# Una versión anterior de este script sí lo reportaba como validación; se
# corrige aquí porque el argumento que la sostenía no se sostiene.
#
# EL ARGUMENTO ORIGINAL (ya no vale): si la exposición predice igual de bien el
# crecimiento del costo laboral en un período sin choque, no está capturando el
# aumento de 2023 sino una tendencia preexistente. Eso supone que un resultado
# "limpio" (coeficiente chico o no significativo) era posible aquí. No lo es.
#
# POR QUÉ EL SIGNO NEGATIVO ESTÁ CASI GARANTIZADO (aritmética, no diseño).
# Exposición alta significa costo laboral bajo en 2022. Por la persistencia del
# costo de una firma en el tiempo (golpe_c tiene Spearman 0,72 entre 2019 y
# 2022, sección 6), un costo bajo en 2022 implica uno bajo también en 2019. Y
# dado el costo de 2018, un costo 2019 bajo implica un crecimiento
# 2018->2019 = log(costo_2019) - log(costo_2018) bajo. El signo negativo sale
# de esa cadena, no de una pre-tendencia real ni de reversión a la media.
#
# LA PRUEBA DE QUE ES ESTO Y NO OTRA COSA: el placebo da coeficiente negativo y
# significativo en las CINCO medidas, incluida Exposure2022_obreros, que es
# composición ocupacional, no usa salarios, y por tanto no debería compartir
# ningún sesgo de división con las otras cuatro. Un placebo que da el mismo
# resultado para una medida que no comparte el mecanismo de las demás no está
# midiendo una propiedad de las medidas -- está midiendo algo que comparten
# todas por construcción del ejercicio, no por economía.
#
# OJO -- no confundir con el placebo de 2018 sobre EMPLEO (otro script, p=0,22,
# no rechaza), que sí es informativo. Son pruebas distintas sobre outcomes
# distintos.

placebo <- bind_rows(lapply(MEDIDAS_2022, function(m)
  estimar_primer_eslabon(m, outcome = "crecimiento_2019",
                         solo_comun = FALSE, etiqueta = etiquetas[[m]]))) %>%
  mutate(periodo = "Placebo 2018-2019")

principal_para_comparar <- primer_eslabon %>%
  filter(muestra == "Propia") %>%
  mutate(periodo = "Choque 2022-2023")

comparacion_placebo <- bind_rows(principal_para_comparar, placebo) %>%
  select(medida, periodo, coeficiente, error_estandar, p_valor, significancia,
         observaciones) %>%
  arrange(medida, periodo)

ver(comparacion_placebo, filas = 12)
guardar_tabla(comparacion_placebo, "T04_placebo_2018_2019",
              "Tabla 4. Primer eslabón en el año del choque y en el placebo 2018-2019 (placebo no informativo, ver nota en el script)")

cat("\nCÓMO LEER esta tabla: NO como 'si el placebo es distinto del choque, la\n",
    "medida es válida'. El placebo da negativo y significativo en las cinco\n",
    "medidas por la razón aritmética explicada arriba, así que un placebo\n",
    "'parecido' al choque no descarta nada y uno 'distinto' tampoco confirma\n",
    "nada. Se reporta para dejar registro de que se probó, no como evidencia a\n",
    "favor del diseño.\n")

grafico_placebo <- ggplot(comparacion_placebo,
                          aes(x = reorder(medida, coeficiente),
                              y = 100 * coeficiente, color = periodo)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                      ymax = 100 * (coeficiente + 1.96 * error_estandar)),
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_manual(values = c(`Choque 2022-2023` = COLOR_ALTA,
                                `Placebo 2018-2019` = COLOR_BAJA)) +
  labs(title = "Costo laboral: choque 2022-2023 vs. placebo 2018-2019 (placebo no informativo)",
       subtitle = "El placebo da negativo en las 5 medidas por construcción -- no valida ni invalida el diseño",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = "La exposición se mide en 2022 en los dos casos. Ver la nota completa antes de esta sección.") +
  tema_tesis
guardar_grafico(grafico_placebo, "G02_placebo")


# ==============================================================================
# 8. ROBUSTEZ
# ==============================================================================
titulo("8. ROBUSTEZ")

# 8.1 Sin controles: ¿cuánto del coeficiente viene de los controles?
sin_controles <- bind_rows(lapply(MEDIDAS_2022, function(m)
  estimar_primer_eslabon(m, solo_comun = FALSE, etiqueta = etiquetas[[m]],
                         con_controles = FALSE))) %>%
  mutate(version = "Sin controles")

con_controles <- primer_eslabon %>%
  filter(muestra == "Propia") %>%
  mutate(version = "Con controles")

robustez_controles <- bind_rows(con_controles, sin_controles) %>%
  select(medida, version, coeficiente, p_valor, significancia, observaciones) %>%
  arrange(medida, version)

ver(robustez_controles, filas = 12)
guardar_tabla(robustez_controles, "T05_robustez_controles",
              "Tabla 5. Primer eslabón con y sin controles")

cat("\nCÓMO LEER: si el coeficiente cambia mucho al quitar los controles, buena\n",
    "parte de la variación de la exposición es composición sectorial, regional\n",
    "o de tamaño, y no exposición al mínimo propiamente.\n")

# 8.2 Errores agrupados por sector
agrupado_sector <- bind_rows(lapply(MEDIDAS_2022, function(m) {
  variable <- paste0(m, "_de")
  if (!variable %in% names(medidas)) return(NULL)
  modelo <- tryCatch(
    feols(as.formula(paste0("crecimiento_2023 ~ ", variable, " | ", CONTROLES)),
          data = medidas, cluster = ~sector_2022),
    error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  fila <- coeftable(modelo)[variable, ]
  tibble(medida = etiquetas[[m]],
         coeficiente = fila[["Estimate"]],
         error_agrupado_sector = fila[["Std. Error"]],
         p_agrupado = fila[["Pr(>|t|)"]],
         significancia = estrellas(fila[["Pr(>|t|)"]]))
}))

ver(agrupado_sector)
guardar_tabla(agrupado_sector, "T06_errores_agrupados_sector",
              "Tabla 6. Primer eslabón con errores agrupados por sector")

# 8.3 Por tramos de exposición: ¿la relación es monotónica?
# La especificación lineal supone que el efecto es proporcional. Si no lo es,
# el coeficiente lineal puede esconder el patrón real. Lo miramos con quintiles.
#
# OJO: esto usa MEDIANAS CRUDAS, sin los controles de sector/departamento/
# tamaño de la sección 4. Lo que sale aquí es que el efecto se concentra en el
# quintil 5 -- eso NO es la última palabra sobre la forma de la relación. Con
# los mismos controles de la especificación principal, la relación por
# quintiles resulta monotónica creciente (validacion/21_decision_medida.R,
# sección 6). No es una contradicción: son dos especificaciones distintas
# (con y sin controles) que pueden mostrar formas distintas. No generalizar
# a partir de esta tabla sola.
if ("golpe_c_de" %in% names(medidas) && "Bite2022_obreros_de" %in% names(medidas)) {
  
  tramos <- medidas %>%
    filter(!is.na(crecimiento_2023)) %>%
    mutate(
      quintil_bite = ntile(Bite2022_obreros, 5),
      quintil_golpe_c = ntile(golpe_c, 5)
    ) %>%
    select(crecimiento_2023, quintil_bite, quintil_golpe_c) %>%
    pivot_longer(starts_with("quintil_"), names_to = "medida",
                 values_to = "quintil") %>%
    filter(!is.na(quintil)) %>%
    group_by(medida, quintil) %>%
    summarise(firmas = n(),
              crecimiento_mediano_pct = 100 * (exp(median(crecimiento_2023)) - 1),
              crecimiento_medio_pct = 100 * (exp(mean(crecimiento_2023)) - 1),
              .groups = "drop")
  
  ver(tramos, filas = 12)
  guardar_tabla(tramos, "T07_crecimiento_por_quintil",
                "Tabla 7. Crecimiento del costo laboral por quintil de exposición",
                decimales = 2)
  
  grafico_tramos <- ggplot(tramos, aes(x = factor(quintil),
                                       y = crecimiento_mediano_pct,
                                       color = medida, group = medida)) +
    geom_line(linewidth = 1) + geom_point(size = 2.5) +
    scale_color_manual(values = c(quintil_bite = COLOR_BAJA,
                                  quintil_golpe_c = COLOR_ALTA),
                       labels = c("Bite (Kaitz de obreros)", "Golpe C")) +
    labs(title = "Crecimiento del costo laboral 2022-2023 por quintil de exposición (sin controles)",
         subtitle = "Medianas crudas -- con controles la relación es monotónica (validacion/21, sección 6)",
         x = "Quintil de exposición (1 = menos expuesta)",
         y = "Crecimiento mediano (%)", color = NULL,
         caption = "Medianas sin controles. Sirve para ver la forma de la relación, no para medir el efecto.") +
    tema_tesis
  guardar_grafico(grafico_tramos, "G03_crecimiento_por_quintil")
}


# ==============================================================================
# 9. RESUMEN Y DECISIÓN
# ==============================================================================
titulo("9. RESUMEN")

resumen <- primer_eslabon %>%
  filter(muestra == "Común") %>%
  select(medida, coeficiente, efecto_pct, p_valor, significancia, observaciones) %>%
  arrange(desc(coeficiente))

ver(resumen)
guardar_tabla(resumen, "T08_resumen_decision",
              "Tabla 8. Resumen del primer eslabón en la muestra común")

# La columna efecto_pct de esta tabla es "cuánto cambia el crecimiento del
# costo laboral por una DE más de exposición, en esta medida y esta muestra" --
# tampoco es la cifra principal de la tesis (4,03%, event study). Es el
# insumo para DECIDIR qué medida usar, no un resultado para citar como "el"
# primer eslabón. Ver la nota de la sección 1 sobre las cinco cifras.
cat("
CÓMO SE TOMA LA DECISIÓN (regla comiteada en NOTA_DECISIONES.md):

La medida principal es la que predice el aumento diferencial del costo laboral
en 2023, y NO se revisa después según los resultados de empleo. Los criterios,
en orden:

  1. Que el coeficiente sea positivo y significativo en la MUESTRA COMÚN
     (sección 5). En muestras distintas los coeficientes no son comparables.
  2. Que SOBREVIVA con base 2019 (sección 6). Si solo aparece con base 2022,
     es sesgo de división. Este criterio pesa más que la magnitud.
  3. Que el placebo de 2018-2019 sea claramente menor (sección 7).
     [NOTA de esta revisión: este criterio, tal como está escrito, depende de
     un placebo que la sección 7 determinó que NO es informativo -- da
     negativo en las cinco medidas por construcción, así que no distingue
     entre ellas. No se reescribe este criterio aquí porque hacerlo cambiaría
     la regla de decisión, y esta tarea solo corrige comentarios. Queda
     reportado como algo que hay que decidir: cómo ajustar o reemplazar el
     criterio 3, o si la decisión debe apoyarse solo en 1, 2 y 4.]
  4. Que no dependa enteramente de los controles (sección 8.1).

Si ninguna medida pasa los criterios 1 y 2, el problema NO es de robustez sino
de diseño. Está registrado como riesgo desde septiembre: en ese caso habría que
construir la exposición desde la distribución salarial de la firma en lugar de
la composición ocupacional o el salario promedio. Reportarlo así, sin
maquillaje: una primera etapa que falla es un resultado, y decirlo es mejor que
estimar efectos sobre empleo que no se pueden interpretar.

Si dos medidas pasan, elegir la más simple de explicar en una defensa y reportar
la otra como robustez.
")

save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
cat("\nCompendio guardado con", length(compendio), "tablas.\n")

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")
