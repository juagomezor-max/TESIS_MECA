# ==============================================================================
# 08_tratamiento_continuo.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# QUÉ HACE ESTE SCRIPT
# El modelo principal supone que el efecto crece de forma pareja con el Kaitz:
# un solo coeficiente para todos. La literatura reciente sobre diferencias en
# diferencias con tratamiento continuo (Callaway, Goodman-Bacon y Sant'Anna,
# 2024, NBER WP 32117) muestra que ese supuesto puede ser fuerte y que el
# coeficiente único es difícil de interpretar.
#
# Aquí revisamos eso con seis ejercicios:
#   C1. Cómo se reparten las firmas a lo largo del Kaitz.
#   C2. Efecto por quintiles de Kaitz (sin imponer que el efecto sea una recta).
#   C3. Comparación de formas funcionales: lineal, cuadrática y por tramos.
#   C4. Estudio de evento separado por nivel de exposición.
#   C5. El resultado sin el quintil más expuesto.
#   C6. Contraste con el paquete contdid de los autores (opcional).
#
# C1 a C5 usan LOS MISMOS controles del script principal. C6 no puede usarlos
# (ver la nota en esa sección) y se reporta solo como contraste.
#
# Se corre desde la raíz del repositorio (abriendo TESIS_MECA.Rproj).
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS -- este script alimenta DOS capítulos, marcado sección por
# sección abajo, no en bloque.
#
# Capítulo 4 (Validación de la identificación -- pruebas que el diseño PASA):
#   C1. Distribución de la exposición.
#   C2 / C2b / C2c. Efecto por quintiles, el salto de 2023 por quintil, y el
#       mismo ejercicio contra 2015-2019.
#   C3. Comparación de formas funcionales (lineal, cuadrática, por tramos).
#   C4. Estudio de evento por nivel de exposición.
#   C5. El resultado sin el quintil más expuesto.
#   C6. Contraste con el paquete contdid.
#
# Capítulo 6 (Validaciones adicionales y amenazas -- lo que NO cierra):
#   C7 (C7.1/C7.2/C7.3). ¿El salto del salario es el choque o reversión a la
#       media? C7.2 en particular es un placebo que SÍ es informativo (a
#       diferencia del placebo del primer eslabón en 05_primer_eslabon_medidas.R): aquí
#       detecta que la medida actual produce un salto salarial falso en 2019,
#       un año sin choque -- es la amenaza que motiva C8.
#   C8 (C8.1/C8.2/C8.3). Tres formas de construir la exposición, probadas
#       contra el mismo placebo que C7.2. AÚN NO TIENE UN VEREDICTO FIJADO EN
#       ESTE SCRIPT: su lugar en la tesis depende de cuál medida (A, B o C)
#       termine pasando la comparación real-vs-placebo de C8.3. Mientras eso
#       no se decida, C8 completo queda en el capítulo 6 junto con C7, porque
#       es la misma investigación de una amenaza sin resolver, no una prueba
#       que el diseño ya pasó. Si una medida concreta resuelve el problema,
#       ESE hallazgo puntual pasaría al capítulo 4 -- no se anticipa aquí.
#
# Panel usado: panel_analitico_firma_eam.rds (el ORIGINAL, sección 1), NO
# panel_firma_eam_expalt_completo.rds (el ampliado, con 2020 y categorías
# ocupacionales separadas). Las cifras de este script NO son directamente
# comparables con las de 05_primer_eslabon_medidas.R, 06_decision_medida.R, 07_reconciliacion.R ni con
# 03_resultados_y_mecanismos.R y 04_mecanismos_por_grupo.R, que sí usan el panel ampliado.
# ------------------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

# Carpeta de salida propia, para no pisar nada de lo que ya está validado
CARPETA <- file.path("4. RESULTADOS", "Continuo")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

# Si el paquete contdid no está instalado, el script corre igual y se salta C6.
# Para instalarlo: remotes::install_github("bcallaway11/contdid")
CORRER_CONTDID <- TRUE

# --- Funciones de apoyo (iguales a las de los otros scripts) -------------------
titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 20) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3) {
  tabla_word <- flextable(tabla)
  tabla_word <- colformat_double(tabla_word, digits = decimales)
  tabla_word <- set_caption(tabla_word, caption = titulo_tabla)
  tabla_word <- autofit(tabla_word)
  save_as_docx(tabla_word, path = file.path(CARPETA, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(CARPETA, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tabla_word
  cat("Tabla guardada:", nombre_archivo, "\n")
}

graficos <- list()
guardar_grafico <- function(grafico, nombre_archivo, ancho = 9, alto = 5.5) {
  print(grafico)
  ggsave(file.path(CARPETA, "figuras", paste0(nombre_archivo, ".png")),
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
# 1. DATOS Y EXPOSICIÓN (igual que en el script principal)
# ==============================================================================
titulo("1. DATOS Y EXPOSICIÓN")

panel <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)),
         salario_promedio = ifelse(empleo_total > 0,
                                   costos_totales_personal_total_c3r10c3 / empleo_total, NA))

cat("Panel:", nrow(panel), "filas,", n_distinct(panel$NORDEMP), "firmas\n")

# Kaitz de 2022, recortado al 1% y 99% y estandarizado (igual que en el principal)
firmas_2022 <- panel %>%
  filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
  select(NORDEMP, Bite2022_obreros, CIIU4, DPTO, tamano_empresa)

limites <- quantile(firmas_2022$Bite2022_obreros, probs = c(0.01, 0.99))

firmas_2022 <- firmas_2022 %>%
  mutate(
    kaitz       = pmin(pmax(Bite2022_obreros, limites[1]), limites[2]),
    kaitz_de    = kaitz / sd(kaitz),
    # Quintiles de exposición: Q1 son las firmas menos expuestas
    quintil     = cut(kaitz, breaks = quantile(kaitz, probs = seq(0, 1, 0.2)),
                      labels = c("Q1 (menos expuestas)", "Q2", "Q3", "Q4", "Q5 (más expuestas)"),
                      include.lowest = TRUE),
    exposicion  = factor(ifelse(kaitz > median(kaitz), "Alta exposición", "Baja exposición"),
                         levels = c("Baja exposición", "Alta exposición")),
    sector_2022 = factor(CIIU4),
    depto_2022  = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  ) %>%
  select(NORDEMP, kaitz, kaitz_de, quintil, exposicion, sector_2022, depto_2022, tamano_2022)

datos <- panel %>%
  inner_join(firmas_2022, by = "NORDEMP") %>%
  mutate(
    ANIO_F      = factor(ANIO),
    post        = as.integer(ANIO >= 2023),
    log_empleo  = log(ifelse(empleo_total > 0, empleo_total, NA)),
    log_salario = log(ifelse(salario_promedio > 0, salario_promedio, NA))
  )

EFECTOS_FIJOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
cat("Firmas con Kaitz:", n_distinct(datos$NORDEMP), "| Observaciones:", nrow(datos), "\n")
cat("Efectos fijos:", EFECTOS_FIJOS, "\n")


# ==============================================================================
# C1. CÓMO SE REPARTEN LAS FIRMAS A LO LARGO DEL KAITZ
# ==============================================================================
titulo("C1. DISTRIBUCIÓN DE LA EXPOSICIÓN")

# Si el efecto no fuera lineal, necesitamos suficientes firmas en cada tramo
# para poder estimarlo por separado. Aquí revisamos eso.
distribucion <- datos %>%
  filter(ANIO == 2022) %>%
  group_by(quintil) %>%
  summarise(
    firmas            = n(),
    kaitz_minimo      = min(kaitz),
    kaitz_promedio    = mean(kaitz),
    kaitz_maximo      = max(kaitz),
    empleo_promedio   = mean(empleo_total, na.rm = TRUE),
    empleo_total_2022 = sum(empleo_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(porcentaje_del_empleo = round(100 * empleo_total_2022 / sum(empleo_total_2022), 1))

ver(distribucion)
guardar_tabla(distribucion, "T16_distribucion_exposicion",
              "Tabla 16. Distribución de las firmas por quintil de exposición (2022)", decimales = 2)

cat("\nLECTURA: cada quintil tiene una quinta parte de las firmas por construcción.",
    "\nLo que importa es el rango de Kaitz que cubre y cuánto empleo representa.\n")

grafico_dist <- ggplot(filter(datos, ANIO == 2022), aes(x = kaitz, fill = quintil)) +
  geom_histogram(bins = 50, color = "white") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "Distribución del índice de Kaitz por quintil (2022)",
       subtitle = "Los quintiles dividen a las firmas en cinco grupos de igual tamaño",
       x = "Índice de Kaitz", y = "Número de firmas", fill = NULL,
       caption = "Valores recortados al 1% y 99%.") +
  tema_tesis
guardar_grafico(grafico_dist, "GC01_distribucion_quintiles")


# ==============================================================================
# C2. EFECTO POR QUINTILES DE EXPOSICIÓN
# ==============================================================================
titulo("C2. EFECTO POR QUINTILES")

# En lugar de un solo coeficiente que supone que el efecto crece parejo con el
# Kaitz, estimamos uno por quintil. El primer quintil (las menos expuestas) es
# la referencia, así que cada coeficiente se lee como "cuánto más cambió este
# grupo que las firmas menos expuestas, después de 2023".
#
# Si el efecto fuera lineal, los coeficientes deberían crecer de forma pareja
# de Q2 a Q5. Si no, se ve de inmediato.

estimar_por_quintil <- function(variable, etiqueta) {
  formula_quintil <- as.formula(paste0(variable, " ~ i(quintil, post, ref = 'Q1 (menos expuestas)') | ",
                                       EFECTOS_FIJOS))
  modelo <- feols(formula_quintil, data = datos, cluster = ~NORDEMP)
  coeficientes <- as.data.frame(coeftable(modelo))
  
  tibble(
    resultado      = etiqueta,
    quintil        = gsub("quintil::|:post", "", rownames(coeficientes)),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]],
    significancia  = estrellas(coeficientes[["Pr(>|t|)"]]),
    ic95_inferior  = coeficientes[["Estimate"]] - 1.96 * coeficientes[["Std. Error"]],
    ic95_superior  = coeficientes[["Estimate"]] + 1.96 * coeficientes[["Std. Error"]]
  )
}

quintiles_salario <- estimar_por_quintil("log_salario", "Salario promedio (log)")
quintiles_empleo  <- estimar_por_quintil("log_empleo",  "Empleo total (log)")

por_quintil <- bind_rows(quintiles_salario, quintiles_empleo) %>%
  mutate(efecto_porcentual = 100 * coeficiente)

ver(por_quintil, filas = 20)
guardar_tabla(por_quintil, "T17_efecto_por_quintil",
              "Tabla 17. Efecto por quintil de exposición, frente al quintil menos expuesto",
              decimales = 4)

grafico_quintiles <- ggplot(por_quintil, aes(x = quintil, y = efecto_porcentual, color = resultado)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = 100 * ic95_inferior, ymax = 100 * ic95_superior),
                  position = position_dodge(width = 0.4), size = 0.6) +
  scale_color_manual(values = c(`Salario promedio (log)` = COLOR_BAJA,
                                `Empleo total (log)` = COLOR_ALTA)) +
  labs(title = "Efecto del aumento de 2023 por quintil de exposición",
       subtitle = "Cambio % frente al quintil menos expuesto. Si el efecto fuera lineal, crecería parejo de Q2 a Q5",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = paste("Controles: firma, año, sector x año, tamaño x año y departamento x año (fijados en 2022).",
                       "\nIntervalos de confianza al 95%, errores agrupados por firma.")) +
  tema_tesis +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
guardar_grafico(grafico_quintiles, "GC02_efecto_por_quintil")


# ==============================================================================
# C2b. EL SALTO DE 2023 POR QUINTIL
# ==============================================================================
titulo("C2b. EL SALTO DE 2023 POR QUINTIL")

# C2 compara el promedio de 2023-2024 contra el promedio de todos los años
# previos. Para el salario eso confunde el choque con la caída que esas firmas
# ya traían: por eso da negativo. Aquí medimos el salto de 2023 frente a 2022,
# que es la lectura A del primer eslabón, pero tramo por tramo.

# Dentro de un solo quintil no se puede separar el salto de 2023 del efecto
# fijo de año: el año lo absorbe todo. Así que estimamos en la muestra
# completa un conjunto de años por quintil, con el quintil menos expuesto
# como referencia, igual que en C2 pero año por año.
datos <- datos %>%
  mutate(q2 = as.integer(quintil == "Q2"),
         q3 = as.integer(quintil == "Q3"),
         q4 = as.integer(quintil == "Q4"),
         q5 = as.integer(quintil == "Q5 (más expuestas)"))

salto_2023_por_quintil <- function(variable, etiqueta) {
  formula_salto <- as.formula(paste0(
    variable, " ~ i(ANIO_F, q2, ref = '2022') + i(ANIO_F, q3, ref = '2022')",
    " + i(ANIO_F, q4, ref = '2022') + i(ANIO_F, q5, ref = '2022') | ", EFECTOS_FIJOS))
  modelo <- feols(formula_salto, data = datos, cluster = ~NORDEMP)
  coeficientes <- as.data.frame(coeftable(modelo))
  
  etiquetas_quintil <- c(q2 = "Q2", q3 = "Q3", q4 = "Q4", q5 = "Q5 (más expuestas)")
  
  tibble(
    nombre         = rownames(coeficientes),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]]
  ) %>%
    mutate(anio    = as.integer(sub("ANIO_F::([0-9]{4}):.*", "\\1", nombre)),
           clave   = sub(".*:(q[2-5])$", "\\1", nombre),
           quintil = etiquetas_quintil[clave]) %>%
    filter(anio %in% c(2023, 2024)) %>%
    transmute(
      resultado = etiqueta, anio, quintil,
      coeficiente, error_estandar, p_valor,
      significancia = estrellas(p_valor),
      ic95_inferior = coeficiente - 1.96 * error_estandar,
      ic95_superior = coeficiente + 1.96 * error_estandar
    )
}

salto_2023 <- bind_rows(
  salto_2023_por_quintil("log_salario", "Salario promedio (log)"),
  salto_2023_por_quintil("log_empleo",  "Empleo total (log)")
) %>%
  mutate(efecto_porcentual = 100 * coeficiente,
         quintil = factor(quintil, levels = c("Q2", "Q3", "Q4", "Q5 (más expuestas)")))

ver(salto_2023, filas = 12)
guardar_tabla(salto_2023, "T17b_salto_2023_por_quintil",
              "Tabla 17b. Cambio en 2023 y 2024 frente a 2022, por quintil de exposición",
              decimales = 4)

cat("\nLECTURA: aquí cada quintil se compara con el menos expuesto, pero año por año:",
    "\n2023 y 2024 frente a 2022. Si el salario sube más en los quintiles altos en 2023,",
    "\nel choque llegó como esperamos, aunque en C2 el promedio de todos los años",
    "\nprevios dé negativo por la caída que esas firmas ya traían.\n")

grafico_salto <- ggplot(filter(salto_2023, anio == 2023),
                        aes(x = quintil, y = efecto_porcentual, color = resultado)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = 100 * ic95_inferior, ymax = 100 * ic95_superior),
                  position = position_dodge(width = 0.4), size = 0.6) +
  scale_color_manual(values = c(`Salario promedio (log)` = COLOR_BAJA,
                                `Empleo total (log)` = COLOR_ALTA)) +
  labs(title = "Cambio de 2022 a 2023 por quintil de exposición",
       subtitle = "Frente al quintil menos expuesto, comparando solo 2023 con 2022",
       x = NULL, y = "Cambio (%)", color = NULL,
       caption = paste("Controles: firma, año, sector x año, tamaño x año y departamento x año (fijados en 2022).",
                       "\nIntervalos de confianza al 95%, errores agrupados por firma.")) +
  tema_tesis +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
guardar_grafico(grafico_salto, "GC02b_salto_2023_por_quintil")


# ==============================================================================
# C2c. EL MISMO EJERCICIO, PERO CONTRA 2015-2019
# ==============================================================================
titulo("C2c. POR QUINTIL, CONTRA EL PROMEDIO DE 2015-2019")

# C2b compara 2023 con 2022. El problema es que 2022 fue un año de empleo alto
# (subsidios PAEF e incentivo a nuevos empleos), así que compararse solo contra
# ese año puede exagerar la caída: es lo que ya mostró la validación V2.
# Aquí repetimos el ejercicio usando como referencia el promedio de 2015-2019,
# que es un período sin pandemia ni subsidios.
#
# La comparación se hace igual que en C2b: cada quintil frente al menos
# expuesto, con el mismo conjunto de controles.

datos <- datos %>%
  mutate(periodo_c2c = case_when(
    ANIO %in% 2015:2019 ~ "referencia (2015-2019)",
    ANIO == 2023        ~ "2023",
    ANIO == 2024        ~ "2024",
    TRUE                ~ "excluido"        # 2021 y 2022 quedan fuera
  ))

base_c2c <- datos %>%
  filter(periodo_c2c != "excluido") %>%
  mutate(periodo_c2c = factor(periodo_c2c,
                              levels = c("referencia (2015-2019)", "2023", "2024")))

cat("Observaciones usadas:", nrow(base_c2c),
    "| firmas:", n_distinct(base_c2c$NORDEMP), "\n")

contra_referencia <- function(variable, etiqueta) {
  formula_c2c <- as.formula(paste0(
    variable, " ~ i(periodo_c2c, q2, ref = 'referencia (2015-2019)')",
    " + i(periodo_c2c, q3, ref = 'referencia (2015-2019)')",
    " + i(periodo_c2c, q4, ref = 'referencia (2015-2019)')",
    " + i(periodo_c2c, q5, ref = 'referencia (2015-2019)') | ", EFECTOS_FIJOS))
  modelo <- feols(formula_c2c, data = base_c2c, cluster = ~NORDEMP)
  coeficientes <- as.data.frame(coeftable(modelo))
  
  etiquetas_quintil <- c(q2 = "Q2", q3 = "Q3", q4 = "Q4", q5 = "Q5 (más expuestas)")
  
  tibble(
    nombre         = rownames(coeficientes),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]]
  ) %>%
    mutate(anio    = sub("periodo_c2c::([^:]+):.*", "\\1", nombre),
           clave   = sub(".*:(q[2-5])$", "\\1", nombre),
           quintil = etiquetas_quintil[clave]) %>%
    transmute(
      resultado = etiqueta, anio, quintil,
      coeficiente, error_estandar, p_valor,
      significancia = estrellas(p_valor),
      ic95_inferior = coeficiente - 1.96 * error_estandar,
      ic95_superior = coeficiente + 1.96 * error_estandar
    )
}

contra_2015_2019 <- bind_rows(
  contra_referencia("log_salario", "Salario promedio (log)"),
  contra_referencia("log_empleo",  "Empleo total (log)")
) %>%
  mutate(efecto_porcentual = 100 * coeficiente,
         quintil = factor(quintil, levels = c("Q2", "Q3", "Q4", "Q5 (más expuestas)")))

ver(contra_2015_2019, filas = 16)
guardar_tabla(contra_2015_2019, "T17c_por_quintil_contra_2015_2019",
              "Tabla 17c. Cambio en 2023 y 2024 frente al promedio de 2015-2019, por quintil",
              decimales = 4)

# Las dos referencias, lado a lado, para 2023
comparacion_referencias <- bind_rows(
  salto_2023 %>% filter(anio == 2023) %>%
    transmute(resultado, quintil, referencia = "2022",
              efecto_porcentual, p_valor, significancia),
  contra_2015_2019 %>% filter(anio == "2023") %>%
    transmute(resultado, quintil, referencia = "promedio 2015-2019",
              efecto_porcentual, p_valor, significancia)
) %>%
  arrange(resultado, quintil, referencia)

ver(comparacion_referencias, filas = 16)
guardar_tabla(comparacion_referencias, "T17d_comparacion_referencias",
              "Tabla 17d. El cambio de 2023 por quintil, según el período de comparación",
              decimales = 4)

grafico_referencias <- ggplot(comparacion_referencias,
                              aes(x = quintil, y = efecto_porcentual, color = referencia)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  facet_wrap(~ resultado, scales = "free_y") +
  scale_color_manual(values = c(`2022` = COLOR_ALTA, `promedio 2015-2019` = COLOR_BAJA)) +
  labs(title = "El cambio de 2023 según con qué período se compare",
       subtitle = "2022 fue un año de empleo alto por los subsidios; 2015-2019 es un período sin pandemia ni subsidios",
       x = NULL, y = "Cambio (%)", color = "Período de comparación",
       caption = paste("Cada quintil frente al menos expuesto.",
                       "\nControles: firma, año, sector x año, tamaño x año y departamento x año (fijados en 2022).")) +
  tema_tesis +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
guardar_grafico(grafico_referencias, "GC02c_comparacion_referencias", ancho = 11)

cat("\nLECTURA: si la caída del empleo en Q4 y Q5 se mantiene con la referencia",
    "\n2015-2019, es un resultado que hay que reportar. Si se desvanece, la caída",
    "\nfrente a 2022 refleja el pico de empleo de ese año y no el choque salarial.\n")


# ==============================================================================
# C3. FORMA FUNCIONAL: LINEAL, CUADRÁTICA O POR TRAMOS
# ==============================================================================
titulo("C3. COMPARACIÓN DE FORMAS FUNCIONALES")

# Tres maneras de dejar que el efecto dependa del Kaitz:
#   lineal:     un coeficiente, el del modelo principal
#   cuadrática: agrega el Kaitz al cuadrado, para permitir curvatura
#   por tramos: un coeficiente por quintil, sin imponer ninguna forma
#
# Comparamos las tres con el AIC (menor es mejor) y probamos si los términos
# adicionales aportan algo (prueba conjunta).

datos <- datos %>% mutate(kaitz_de2 = kaitz_de^2)

comparar_formas <- function(variable, etiqueta) {
  lineal    <- feols(as.formula(paste0(variable, " ~ post:kaitz_de | ", EFECTOS_FIJOS)),
                     data = datos, cluster = ~NORDEMP)
  cuadratica <- feols(as.formula(paste0(variable, " ~ post:kaitz_de + post:kaitz_de2 | ", EFECTOS_FIJOS)),
                      data = datos, cluster = ~NORDEMP)
  tramos    <- feols(as.formula(paste0(variable, " ~ i(quintil, post, ref = 'Q1 (menos expuestas)') | ",
                                       EFECTOS_FIJOS)), data = datos, cluster = ~NORDEMP)
  
  # ¿Aporta algo el término cuadrático?
  p_cuadratico <- coeftable(cuadratica)["post:kaitz_de2", "Pr(>|t|)"]
  # ¿Aportan algo los tramos en conjunto?
  p_tramos <- wald(tramos, keep = "quintil::", print = FALSE)$p
  
  tibble(
    resultado = etiqueta,
    forma     = c("Lineal (modelo principal)", "Cuadrática", "Por tramos (quintiles)"),
    aic       = c(AIC(lineal), AIC(cuadratica), AIC(tramos)),
    observaciones = c(nobs(lineal), nobs(cuadratica), nobs(tramos)),
    prueba_de_no_linealidad = c(NA, p_cuadratico, p_tramos)
  )
}

formas <- bind_rows(
  comparar_formas("log_salario", "Salario promedio (log)"),
  comparar_formas("log_empleo",  "Empleo total (log)")
)

ver(formas, filas = 10)
guardar_tabla(formas, "T18_formas_funcionales",
              "Tabla 18. Comparación de formas funcionales del tratamiento continuo", decimales = 4)

cat("\nLECTURA: si el p-valor de la prueba de no linealidad es alto, no hay evidencia",
    "\nde que el efecto se aparte de una recta y la especificación principal es adecuada.",
    "\nSi es bajo, el efecto depende del tramo de exposición y conviene reportar los quintiles.\n")


# ==============================================================================
# C4. ESTUDIO DE EVENTO SEPARADO POR NIVEL DE EXPOSICIÓN
# ==============================================================================
titulo("C4. ESTUDIO DE EVENTO POR NIVEL DE EXPOSICIÓN")

# La versión visual de lo anterior: en lugar de un coeficiente por año,
# uno por año y grupo de exposición (alta y baja). Sirve para ver si los
# grupos venían parecidos antes de 2023 dentro de cada tramo.

evento_por_grupo <- function(variable, etiqueta) {
  resultados <- tibble()
  for (grupo in levels(datos$exposicion)) {
    base_grupo <- filter(datos, exposicion == grupo)
    modelo <- feols(as.formula(paste0(variable, " ~ i(ANIO_F, kaitz_de, ref = '2022') | ", EFECTOS_FIJOS)),
                    data = base_grupo, cluster = ~NORDEMP)
    coeficientes <- as.data.frame(coeftable(modelo))
    resultados <- bind_rows(resultados, tibble(
      resultado      = etiqueta,
      grupo          = grupo,
      anio           = as.integer(gsub("ANIO_F::|:kaitz_de", "", rownames(coeficientes))),
      coeficiente    = coeficientes[["Estimate"]],
      error_estandar = coeficientes[["Std. Error"]]
    ) %>%
      bind_rows(tibble(resultado = etiqueta, grupo = grupo, anio = 2022L,
                       coeficiente = 0, error_estandar = 0)) %>%
      arrange(anio))
  }
  resultados %>%
    mutate(ic95_inferior = coeficiente - 1.96 * error_estandar,
           ic95_superior = coeficiente + 1.96 * error_estandar)
}

evento_grupos <- evento_por_grupo("log_empleo", "Empleo total (log)")
ver(evento_grupos, filas = 20)
guardar_tabla(evento_grupos, "T19_evento_por_grupo",
              "Tabla 19. Empleo año por año, estimado por separado en cada grupo de exposición",
              decimales = 4)

grafico_evento_grupos <- ggplot(evento_grupos, aes(x = anio, y = 100 * coeficiente, color = grupo)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = 100 * ic95_inferior, ymax = 100 * ic95_superior),
                  position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(title = "Empleo año por año, estimado por separado en cada grupo de exposición",
       subtitle = "Efecto de una desviación estándar más de Kaitz dentro de cada grupo, frente a 2022",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = "Controles: firma, año, sector x año y departamento x año. Errores agrupados por firma.") +
  tema_tesis
guardar_grafico(grafico_evento_grupos, "GC03_evento_por_grupo")


# ==============================================================================
# C5. EL RESULTADO SIN EL QUINTIL MÁS EXPUESTO
# ==============================================================================
titulo("C5. SIN EL QUINTIL MÁS EXPUESTO")

# Las firmas del quintil más alto son las de salarios más bajos, y por eso
# también las más expuestas a errores de medición del salario del obrero.
# Repetimos el resultado principal sin ellas.

estimar_did <- function(variable, base, etiqueta, muestra) {
  modelo <- feols(as.formula(paste0(variable, " ~ post:kaitz_de | ", EFECTOS_FIJOS)),
                  data = base, cluster = ~NORDEMP)
  fila <- coeftable(modelo)["post:kaitz_de", ]
  tibble(
    muestra        = muestra,
    resultado      = etiqueta,
    coeficiente    = fila[["Estimate"]],
    error_estandar = fila[["Std. Error"]],
    p_valor        = fila[["Pr(>|t|)"]],
    significancia  = estrellas(fila[["Pr(>|t|)"]]),
    ic95_inferior  = fila[["Estimate"]] - 1.96 * fila[["Std. Error"]],
    ic95_superior  = fila[["Estimate"]] + 1.96 * fila[["Std. Error"]],
    observaciones  = nobs(modelo)
  )
}

sin_extremo <- filter(datos, quintil != "Q5 (más expuestas)")

recorte_extremo <- bind_rows(
  estimar_did("log_salario", datos,       "Salario promedio (log)", "Todas las firmas"),
  estimar_did("log_salario", sin_extremo, "Salario promedio (log)", "Sin el quintil más expuesto"),
  estimar_did("log_empleo",  datos,       "Empleo total (log)",     "Todas las firmas"),
  estimar_did("log_empleo",  sin_extremo, "Empleo total (log)",     "Sin el quintil más expuesto")
) %>%
  mutate(efecto_porcentual = 100 * coeficiente)

ver(recorte_extremo)
guardar_tabla(recorte_extremo, "T20_sin_quintil_extremo",
              "Tabla 20. Resultado principal con y sin el quintil más expuesto", decimales = 4)


# ==============================================================================
# C6. CONTRASTE CON EL PAQUETE contdid (OPCIONAL)
# ==============================================================================
titulo("C6. CONTRASTE CON EL PAQUETE contdid")

# NOTA IMPORTANTE SOBRE ESTE EJERCICIO
# El paquete contdid (Callaway, Goodman-Bacon y Sant'Anna) implementa el método
# de los autores, pero en su versión actual NO admite covariables ni panel
# desbalanceado, y trabaja con dos períodos. Como nuestro control de tamaño por
# año es determinante, no podemos meterlo dentro del estimador. Lo que hacemos
# es quitarle al resultado el efecto de los controles ANTES de estimar
# (residualizar) y correr el método sobre esos residuos.
#
# Esto NO es equivalente a incluir los controles dentro del estimador. Se
# reporta como contraste metodológico, no como especificación principal.

if (CORRER_CONTDID && requireNamespace("contdid", quietly = TRUE)) {
  
  library(contdid)
  
  # Paso 1: quitarle al empleo el efecto de los controles, usando solo los años
  # previos para que el choque no contamine la limpieza
  modelo_controles <- feols(log_empleo ~ 1 | NORDEMP + ANIO_F + sector_2022^ANIO_F +
                              tamano_2022^ANIO_F + depto_2022^ANIO_F,
                            data = datos, cluster = ~NORDEMP)
  # obs() devuelve las filas que el modelo usó de verdad: fixest quita las
  # vacías y las que quedan solas en una celda, así los residuos calzan
  datos_residuos <- datos[obs(modelo_controles), ] %>%
    mutate(empleo_residual = as.numeric(resid(modelo_controles)))
  
  # Paso 2: dos períodos (antes y después), con dos exigencias del paquete:
  #   - la dosis debe ser la misma en todos los años de la firma, también antes
  #     del choque (no puede ir en cero en los años previos);
  #   - hace falta un grupo de comparación, con gname = 0.
  # Como todas nuestras firmas están expuestas en 2023, usamos el quintil menos
  # expuesto como comparación. Es el mismo grupo de referencia de C2, pero hay
  # que decirlo con todas las letras: ese grupo TAMBIÉN recibió el choque
  # (su Kaitz promedio es 0,49), así que el ejercicio subestima el efecto.
  base_dos_periodos <- datos_residuos %>%
    group_by(NORDEMP, post) %>%
    summarise(empleo_residual = mean(empleo_residual, na.rm = TRUE),
              kaitz   = first(kaitz),
              quintil = first(quintil), .groups = "drop") %>%
    group_by(NORDEMP) %>%
    filter(n() == 2) %>%      # solo firmas con dato antes y después
    ungroup() %>%
    mutate(
      periodo = ifelse(post == 1, 2, 1),
      dosis   = kaitz,                                             # igual en los dos períodos
      grupo   = ifelse(quintil == "Q1 (menos expuestas)", 0, 2),   # 0 = comparación
      id      = as.integer(factor(NORDEMP))
    )
  
  cat("Firmas en el contraste:", n_distinct(base_dos_periodos$NORDEMP),
      "| de comparación (Q1):",
      n_distinct(base_dos_periodos$NORDEMP[base_dos_periodos$grupo == 0]), "\n")
  
  resultado_contdid <- try(
    cont_did(yname   = "empleo_residual",
             tname   = "periodo",
             idname  = "id",
             dname   = "dosis",
             gname   = "grupo",
             data    = as.data.frame(base_dos_periodos),
             target_parameter = "slope",   # respuesta a la dosis, comparable con nuestro beta
             aggregation      = "dose",
             treatment_type   = "continuous",
             control_group    = "nevertreated",
             biters    = 100,
             cband     = TRUE,
             num_knots = 1,
             degree    = 3),
    silent = TRUE)
  
  if (inherits(resultado_contdid, "try-error")) {
    cat("\nNo se pudo estimar con contdid. Mensaje:\n", as.character(resultado_contdid), "\n")
    cat("Los resultados C1 a C5 no dependen de este paso.\n")
  } else {
    print(summary(resultado_contdid))
    cat("\nCÓMO LEER ESTE CONTRASTE:",
        "\n  Estima la respuesta a la dosis sin imponer forma funcional, pero con",
        "\n  tres diferencias frente a C2: los controles se quitaron antes (no van",
        "\n  dentro del estimador), se promedian los años antes y después, y el",
        "\n  quintil menos expuesto se trata como si no hubiera recibido el choque.",
        "\n  Por eso es un contraste metodológico y no un resultado de la tesis.\n")
  }
  
} else {
  cat("Paquete contdid no disponible: se omite este contraste.\n")
  cat("Para instalarlo: remotes::install_github('bcallaway11/contdid')\n")
  cat("Los resultados C1 a C5 no dependen de este paso.\n")
}


# ==============================================================================
# C7. ¿EL SALTO DEL SALARIO ES EL CHOQUE O REVERSIÓN A LA MEDIA?
# ==============================================================================
titulo("C7. ¿EL SALTO DEL SALARIO ES EL CHOQUE O REVERSIÓN A LA MEDIA?")

# EL PROBLEMA
# El Kaitz lleva el salario del obrero de 2022 en el DENOMINADOR. Si una firma
# tuvo un mal 2022 por cualquier razón pasajera, su Kaitz sube por construcción,
# y al año siguiente su salario rebota sin que el salario mínimo tenga nada que
# ver. Eso se llama reversión a la media y produciría un "efecto" falso.
#
# La sospecha viene de C2c: frente a 2022 el salario de Q5 sube 10%, pero frente
# al promedio de 2015-2019 está 9% POR DEBAJO. Las dos cosas juntas significan
# que esas firmas venían cayendo y solo rebotaron.
#
# Tres ejercicios para distinguir una cosa de la otra.

# --- C7.1 La serie del salario por quintil, desde 2015 ---------------------------
# Si las firmas de Q5 vienen cayendo desde 2015 y en 2023 solo rebotan, es
# reversión. Si están planas y saltan en 2023, es el choque.
titulo("C7.1 Trayectoria del salario por quintil")

trayectoria <- datos %>%
  group_by(quintil, ANIO) %>%
  summarise(salario_medio_log = mean(log_salario, na.rm = TRUE),
            firmas = n(), .groups = "drop") %>%
  group_by(quintil) %>%
  mutate(vs_2015 = 100 * (salario_medio_log - salario_medio_log[ANIO == 2015])) %>%
  ungroup()

ver(trayectoria, filas = 50)
guardar_tabla(trayectoria, "T21_trayectoria_salario_por_quintil",
              "Tabla 21. Salario promedio por quintil de exposición, frente a su nivel de 2015 (%)",
              decimales = 2)

grafico_trayectoria <- ggplot(trayectoria, aes(x = ANIO, y = vs_2015, color = quintil)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(title = "Trayectoria del salario promedio por quintil de exposición",
       subtitle = "Diferencia frente al nivel de 2015 de cada grupo. Si Q5 viene cayendo y rebota en 2023, es reversión",
       x = NULL, y = "% frente a 2015", color = NULL,
       caption = "Promedios simples del logaritmo del salario, sin controles.") +
  tema_tesis
guardar_grafico(grafico_trayectoria, "GC04_trayectoria_salario", ancho = 10)

# --- C7.2 Placebo de reversión: Kaitz de 2018 y salto en 2019 --------------------
# Repetimos la construcción de la medida con el salario de 2018 y miramos el
# salto de 2019, un año sin choque. Si aparece un salto parecido, lo que
# medimos es reversión y no el salario mínimo.
titulo("C7.2 Placebo: la misma medida construida en 2018")

salario_minimo_2019 <- 828116   # decreto del salario mínimo de 2019

kaitz_placebo <- datos %>%
  filter(ANIO == 2018,
         obreros_permanentes > 0,
         sueldos_permanentes_obreros_c3r2c1 > 0) %>%
  mutate(salario_obrero_2018 = sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes / 12 * 1000,
         kaitz_2018_bruto    = salario_minimo_2019 / salario_obrero_2018) %>%
  filter(is.finite(kaitz_2018_bruto)) %>%
  select(NORDEMP, kaitz_2018_bruto)

limites_placebo <- quantile(kaitz_placebo$kaitz_2018_bruto, probs = c(0.01, 0.99), na.rm = TRUE)

kaitz_placebo <- kaitz_placebo %>%
  mutate(kaitz_2018 = pmin(pmax(kaitz_2018_bruto, limites_placebo[1]), limites_placebo[2]),
         quintil_2018 = cut(kaitz_2018,
                            breaks = quantile(kaitz_2018, probs = seq(0, 1, 0.2), na.rm = TRUE),
                            labels = c("Q1 (menos expuestas)", "Q2", "Q3", "Q4", "Q5 (más expuestas)"),
                            include.lowest = TRUE)) %>%
  mutate(p2 = as.integer(quintil_2018 == "Q2"),
         p3 = as.integer(quintil_2018 == "Q3"),
         p4 = as.integer(quintil_2018 == "Q4"),
         p5 = as.integer(quintil_2018 == "Q5 (más expuestas)"))

cat("Firmas con Kaitz construido en 2018:", nrow(kaitz_placebo), "\n")

base_placebo <- datos %>%
  inner_join(select(kaitz_placebo, NORDEMP, quintil_2018, p2, p3, p4, p5), by = "NORDEMP") %>%
  filter(ANIO %in% 2015:2019)   # solo el período previo, sin el choque de 2023

etiquetas_quintil <- c(p2 = "Q2", p3 = "Q3", p4 = "Q4", p5 = "Q5 (más expuestas)")

salto_placebo <- function(variable, etiqueta) {
  modelo <- feols(as.formula(paste0(
    variable, " ~ i(ANIO_F, p2, ref = '2018') + i(ANIO_F, p3, ref = '2018')",
    " + i(ANIO_F, p4, ref = '2018') + i(ANIO_F, p5, ref = '2018') | ",
    "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F")),
    data = base_placebo, cluster = ~NORDEMP)
  coeficientes <- as.data.frame(coeftable(modelo))
  
  tibble(
    nombre         = rownames(coeficientes),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]]
  ) %>%
    mutate(anio    = as.integer(sub("ANIO_F::([0-9]{4}):.*", "\\1", nombre)),
           clave   = sub(".*:(p[2-5])$", "\\1", nombre),
           quintil = etiquetas_quintil[clave]) %>%
    filter(anio == 2019) %>%
    transmute(resultado = etiqueta, quintil, coeficiente, error_estandar, p_valor,
              significancia = estrellas(p_valor),
              efecto_porcentual = 100 * coeficiente)
}

placebo_reversion <- bind_rows(
  salto_placebo("log_salario", "Salario promedio (log)"),
  salto_placebo("log_empleo",  "Empleo total (log)")
) %>%
  mutate(quintil = factor(quintil, levels = c("Q2", "Q3", "Q4", "Q5 (más expuestas)")))

ver(placebo_reversion, filas = 12)
guardar_tabla(placebo_reversion, "T22_placebo_reversion_2018",
              "Tabla 22. Placebo: salto de 2019 con la medida construida en 2018, por quintil",
              decimales = 4)

cat("\nLECTURA: en 2019 no hubo ningún choque parecido al de 2023. Si el salario",
    "\nigual salta en los quintiles altos, lo que mide la medida es reversión a la",
    "\nmedia, no el efecto del salario mínimo. Compara estas cifras con las de C2b.\n")

# --- C7.3 Kaitz con el salario promedio de 2019-2021 -----------------------------
# Si el problema es el ruido de un solo año, un denominador promediado lo
# reduce. Repetimos el salto de 2023 con esta medida alternativa.
titulo("C7.3 Kaitz con el salario promedio de 2019-2021")

kaitz_promedio <- datos %>%
  filter(ANIO %in% c(2019, 2021),
         obreros_permanentes > 0,
         sueldos_permanentes_obreros_c3r2c1 > 0) %>%
  mutate(salario_obrero = sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes / 12 * 1000) %>%
  group_by(NORDEMP) %>%
  summarise(salario_obrero_medio = mean(salario_obrero, na.rm = TRUE),
            anios_usados = n(), .groups = "drop") %>%
  filter(is.finite(salario_obrero_medio), salario_obrero_medio > 0) %>%
  mutate(kaitz_alt_bruto = 1160000 / salario_obrero_medio)   # salario mínimo de 2023

limites_alt <- quantile(kaitz_promedio$kaitz_alt_bruto, probs = c(0.01, 0.99), na.rm = TRUE)

kaitz_promedio <- kaitz_promedio %>%
  mutate(kaitz_alt = pmin(pmax(kaitz_alt_bruto, limites_alt[1]), limites_alt[2]),
         quintil_alt = cut(kaitz_alt,
                           breaks = quantile(kaitz_alt, probs = seq(0, 1, 0.2), na.rm = TRUE),
                           labels = c("Q1 (menos expuestas)", "Q2", "Q3", "Q4", "Q5 (más expuestas)"),
                           include.lowest = TRUE),
         a2 = as.integer(quintil_alt == "Q2"),
         a3 = as.integer(quintil_alt == "Q3"),
         a4 = as.integer(quintil_alt == "Q4"),
         a5 = as.integer(quintil_alt == "Q5 (más expuestas)"))

cat("Firmas con la medida alternativa:", nrow(kaitz_promedio), "\n")

base_alt <- datos %>%
  inner_join(select(kaitz_promedio, NORDEMP, quintil_alt, a2, a3, a4, a5), by = "NORDEMP")

cat("Correlación entre las dos medidas:",
    round(cor(kaitz_promedio$kaitz_alt,
              firmas_2022$kaitz[match(kaitz_promedio$NORDEMP, firmas_2022$NORDEMP)],
              use = "complete.obs"), 3), "\n")

etiquetas_alt <- c(a2 = "Q2", a3 = "Q3", a4 = "Q4", a5 = "Q5 (más expuestas)")

salto_alternativo <- function(variable, etiqueta, referencia) {
  if (referencia == "2022") {
    base_uso <- base_alt
    ref_periodo <- "2022"
    variable_periodo <- "ANIO_F"
  } else {
    base_uso <- base_alt %>%
      filter(periodo_c2c != "excluido") %>%
      mutate(periodo_c2c = factor(periodo_c2c,
                                  levels = c("referencia (2015-2019)", "2023", "2024")))
    ref_periodo <- "referencia (2015-2019)"
    variable_periodo <- "periodo_c2c"
  }
  
  modelo <- feols(as.formula(paste0(
    variable, " ~ i(", variable_periodo, ", a2, ref = '", ref_periodo, "')",
    " + i(", variable_periodo, ", a3, ref = '", ref_periodo, "')",
    " + i(", variable_periodo, ", a4, ref = '", ref_periodo, "')",
    " + i(", variable_periodo, ", a5, ref = '", ref_periodo, "') | ", EFECTOS_FIJOS)),
    data = base_uso, cluster = ~NORDEMP)
  coeficientes <- as.data.frame(coeftable(modelo))
  
  tibble(
    nombre         = rownames(coeficientes),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]]
  ) %>%
    mutate(anio    = sub(paste0(variable_periodo, "::([^:]+):.*"), "\\1", nombre),
           clave   = sub(".*:(a[2-5])$", "\\1", nombre),
           quintil = etiquetas_alt[clave]) %>%
    filter(anio == "2023") %>%
    transmute(resultado = etiqueta, referencia, quintil,
              coeficiente, error_estandar, p_valor,
              significancia = estrellas(p_valor),
              efecto_porcentual = 100 * coeficiente)
}

medida_alternativa <- bind_rows(
  salto_alternativo("log_salario", "Salario promedio (log)", "2022"),
  salto_alternativo("log_salario", "Salario promedio (log)", "promedio 2015-2019"),
  salto_alternativo("log_empleo",  "Empleo total (log)",     "2022"),
  salto_alternativo("log_empleo",  "Empleo total (log)",     "promedio 2015-2019")
) %>%
  mutate(quintil = factor(quintil, levels = c("Q2", "Q3", "Q4", "Q5 (más expuestas)")))

ver(medida_alternativa, filas = 20)
guardar_tabla(medida_alternativa, "T23_kaitz_promedio_2019_2021",
              "Tabla 23. Cambio de 2023 por quintil, con el Kaitz construido sobre el salario promedio de 2019-2021",
              decimales = 4)

cat("\nLECTURA: si con esta medida el salario sigue subiendo en los quintiles altos",
    "\ncon LAS DOS referencias, el primer eslabón es sólido y el problema era el ruido",
    "\ndel salario de 2022. Si el salto desaparece, la medida estaba capturando",
    "\nreversión a la media y hay que reconstruir la exposición desde la distribución",
    "\nsalarial de la firma.\n")


# ==============================================================================
# C8. TRES MEDIDAS DE EXPOSICIÓN Y SU PLACEBO
# ==============================================================================
titulo("C8. TRES MEDIDAS DE EXPOSICIÓN Y SU PLACEBO")

# C7.2 mostró que la medida actual produce un salto salarial falso en 2019, un
# año sin choque. La causa es mecánica: el salario de la propia firma en el año
# base está en el denominador de la exposición Y es el punto de partida contra
# el que medimos el cambio. Cualquier error de medición en ese número empuja
# las dos cosas en la misma dirección.
#
# Aquí comparamos tres formas de construir la exposición y le corremos a cada
# una el MISMO placebo. La que no produzca salto falso es la que sirve.
#
#   A. Actual: salario del obrero de la firma en el año base.
#   B. Promediada: salario del obrero de la firma, promediado en varios años.
#   C. Por celda: salario promedio de las firmas PARECIDAS (mismo sector,
#      departamento y tamaño), excluyendo a la propia firma. Así el error de
#      medición de la firma desaparece del denominador.
#
# Cada medida se construye dos veces: una para el ejercicio real (base 2022,
# choque 2023) y otra para el placebo (base 2018, año falso 2019).

# --- Función que construye las tres medidas para un año base dado --------------
construir_medidas <- function(anio_base, anios_promedio, salario_minimo_siguiente) {
  
  # Salario del obrero de cada firma, en pesos mensuales
  salarios <- datos %>%
    filter(obreros_permanentes > 0, sueldos_permanentes_obreros_c3r2c1 > 0) %>%
    mutate(salario_obrero = sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes / 12 * 1000) %>%
    select(NORDEMP, ANIO, salario_obrero, sector_2022, depto_2022, tamano_2022)
  
  # A. El salario del año base
  medida_a <- salarios %>%
    filter(ANIO == anio_base) %>%
    transmute(NORDEMP, sector_2022, depto_2022, tamano_2022,
              salario_base = salario_obrero)
  
  # B. El promedio de varios años
  medida_b <- salarios %>%
    filter(ANIO %in% anios_promedio) %>%
    group_by(NORDEMP) %>%
    summarise(salario_promediado = mean(salario_obrero, na.rm = TRUE),
              anios = n(), .groups = "drop") %>%
    filter(anios >= 2)   # al menos dos años, para que promediar sirva de algo
  
  # C. El promedio de las firmas parecidas, sin contarse a sí misma
  #    (media de la celda menos la propia firma, dividida por las demás)
  medida_c <- medida_a %>%
    group_by(sector_2022, depto_2022, tamano_2022) %>%
    mutate(firmas_en_celda = n(),
           suma_celda      = sum(salario_base, na.rm = TRUE),
           salario_ajenas  = (suma_celda - salario_base) / (firmas_en_celda - 1)) %>%
    ungroup() %>%
    filter(firmas_en_celda >= 5, is.finite(salario_ajenas), salario_ajenas > 0) %>%
    select(NORDEMP, salario_ajenas, firmas_en_celda)
  
  # Las tres medidas juntas, cada una convertida en Kaitz y en quintiles
  medidas <- medida_a %>%
    select(NORDEMP, salario_base) %>%
    left_join(medida_b, by = "NORDEMP") %>%
    left_join(medida_c, by = "NORDEMP") %>%
    mutate(kaitz_a = salario_minimo_siguiente / salario_base,
           kaitz_b = salario_minimo_siguiente / salario_promediado,
           kaitz_c = salario_minimo_siguiente / salario_ajenas)
  
  recortar_y_partir <- function(x) {
    limites <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
    x <- pmin(pmax(x, limites[1]), limites[2])
    cut(x, breaks = quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE),
        labels = c("Q1", "Q2", "Q3", "Q4", "Q5"), include.lowest = TRUE)
  }
  
  medidas %>%
    mutate(qa = recortar_y_partir(kaitz_a),
           qb = recortar_y_partir(kaitz_b),
           qc = recortar_y_partir(kaitz_c)) %>%
    select(NORDEMP, qa, qb, qc, firmas_en_celda)
}

# --- Función que estima el salto de un año, con la medida que se le pase -------
salto_con_medida <- function(base, columna_quintil, variable, anio_choque, anio_base_ref) {
  base <- base %>%
    filter(!is.na(.data[[columna_quintil]])) %>%
    mutate(g2 = as.integer(.data[[columna_quintil]] == "Q2"),
           g3 = as.integer(.data[[columna_quintil]] == "Q3"),
           g4 = as.integer(.data[[columna_quintil]] == "Q4"),
           g5 = as.integer(.data[[columna_quintil]] == "Q5"))
  
  modelo <- feols(as.formula(paste0(
    variable, " ~ i(ANIO_F, g2, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g3, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g4, ref = '", anio_base_ref, "')",
    " + i(ANIO_F, g5, ref = '", anio_base_ref, "') | ", EFECTOS_FIJOS)),
    data = base, cluster = ~NORDEMP)
  
  coeficientes <- as.data.frame(coeftable(modelo))
  etiquetas <- c(g2 = "Q2", g3 = "Q3", g4 = "Q4", g5 = "Q5 (más expuestas)")
  
  tibble(
    nombre         = rownames(coeficientes),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]],
    p_valor        = coeficientes[["Pr(>|t|)"]]
  ) %>%
    mutate(anio    = as.integer(sub("ANIO_F::([0-9]{4}):.*", "\\1", nombre)),
           clave   = sub(".*:(g[2-5])$", "\\1", nombre),
           quintil = etiquetas[clave]) %>%
    filter(anio == anio_choque) %>%
    transmute(quintil, coeficiente, error_estandar, p_valor,
              significancia = estrellas(p_valor),
              efecto_porcentual = 100 * coeficiente,
              firmas = n_distinct(base$NORDEMP))
}

# --- Ejercicio real: base 2022, choque de 2023 ---------------------------------
titulo("C8.1 Las tres medidas en el choque de 2023")

medidas_2022 <- construir_medidas(anio_base = 2022,
                                  anios_promedio = c(2019, 2021, 2022),
                                  salario_minimo_siguiente = 1160000)

cat("Firmas con cada medida (2022): A =", sum(!is.na(medidas_2022$qa)),
    "| B =", sum(!is.na(medidas_2022$qb)),
    "| C =", sum(!is.na(medidas_2022$qc)), "\n")

base_medidas_2022 <- datos %>% inner_join(medidas_2022, by = "NORDEMP")

real_2023 <- bind_rows(
  salto_con_medida(base_medidas_2022, "qa", "log_salario", 2023, "2022") %>%
    mutate(medida = "A. Salario de la firma en 2022", resultado = "Salario promedio (log)"),
  salto_con_medida(base_medidas_2022, "qb", "log_salario", 2023, "2022") %>%
    mutate(medida = "B. Salario promediado 2019-2022", resultado = "Salario promedio (log)"),
  salto_con_medida(base_medidas_2022, "qc", "log_salario", 2023, "2022") %>%
    mutate(medida = "C. Salario de firmas parecidas", resultado = "Salario promedio (log)"),
  salto_con_medida(base_medidas_2022, "qa", "log_empleo", 2023, "2022") %>%
    mutate(medida = "A. Salario de la firma en 2022", resultado = "Empleo total (log)"),
  salto_con_medida(base_medidas_2022, "qb", "log_empleo", 2023, "2022") %>%
    mutate(medida = "B. Salario promediado 2019-2022", resultado = "Empleo total (log)"),
  salto_con_medida(base_medidas_2022, "qc", "log_empleo", 2023, "2022") %>%
    mutate(medida = "C. Salario de firmas parecidas", resultado = "Empleo total (log)")
) %>%
  mutate(ejercicio = "Real (choque de 2023)")

ver(real_2023, filas = 24)

# --- Placebo: base 2018, año falso 2019 ----------------------------------------
titulo("C8.2 Las tres medidas en el placebo de 2019")

medidas_2018 <- construir_medidas(anio_base = 2018,
                                  anios_promedio = c(2015, 2016, 2017, 2018),
                                  salario_minimo_siguiente = 828116)

cat("Firmas con cada medida (2018): A =", sum(!is.na(medidas_2018$qa)),
    "| B =", sum(!is.na(medidas_2018$qb)),
    "| C =", sum(!is.na(medidas_2018$qc)), "\n")

base_medidas_2018 <- datos %>%
  inner_join(medidas_2018, by = "NORDEMP") %>%
  filter(ANIO %in% 2015:2019)      # sin el choque de 2023

placebo_2019 <- bind_rows(
  salto_con_medida(base_medidas_2018, "qa", "log_salario", 2019, "2018") %>%
    mutate(medida = "A. Salario de la firma en 2022", resultado = "Salario promedio (log)"),
  salto_con_medida(base_medidas_2018, "qb", "log_salario", 2019, "2018") %>%
    mutate(medida = "B. Salario promediado 2019-2022", resultado = "Salario promedio (log)"),
  salto_con_medida(base_medidas_2018, "qc", "log_salario", 2019, "2018") %>%
    mutate(medida = "C. Salario de firmas parecidas", resultado = "Salario promedio (log)"),
  salto_con_medida(base_medidas_2018, "qa", "log_empleo", 2019, "2018") %>%
    mutate(medida = "A. Salario de la firma en 2022", resultado = "Empleo total (log)"),
  salto_con_medida(base_medidas_2018, "qb", "log_empleo", 2019, "2018") %>%
    mutate(medida = "B. Salario promediado 2019-2022", resultado = "Empleo total (log)"),
  salto_con_medida(base_medidas_2018, "qc", "log_empleo", 2019, "2018") %>%
    mutate(medida = "C. Salario de firmas parecidas", resultado = "Empleo total (log)")
) %>%
  mutate(ejercicio = "Placebo (año sin choque: 2019)")

ver(placebo_2019, filas = 24)

# --- La comparación, que es lo que decide --------------------------------------
titulo("C8.3 Real frente a placebo")

comparacion_medidas <- bind_rows(real_2023, placebo_2019) %>%
  select(resultado, medida, ejercicio, quintil, efecto_porcentual, p_valor, significancia, firmas) %>%
  arrange(resultado, medida, quintil, ejercicio)

ver(comparacion_medidas, filas = 48)
guardar_tabla(comparacion_medidas, "T24_tres_medidas_real_vs_placebo",
              "Tabla 24. Las tres medidas de exposición: salto real de 2023 frente al placebo de 2019",
              decimales = 4)

# Resumen apretado: solo el quintil más expuesto, que es donde más se nota
resumen_medidas <- comparacion_medidas %>%
  filter(quintil == "Q5 (más expuestas)") %>%
  select(resultado, medida, ejercicio, efecto_porcentual, significancia) %>%
  pivot_wider(names_from = ejercicio, values_from = c(efecto_porcentual, significancia))

ver(resumen_medidas, filas = 12)
guardar_tabla(resumen_medidas, "T25_resumen_medidas_q5",
              "Tabla 25. El quintil más expuesto: efecto real y efecto falso, con cada medida",
              decimales = 2)

grafico_medidas <- ggplot(filter(comparacion_medidas, resultado == "Salario promedio (log)"),
                          aes(x = quintil, y = efecto_porcentual, fill = ejercicio)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ medida) +
  scale_fill_manual(values = c(`Real (choque de 2023)` = COLOR_ALTA,
                               `Placebo (año sin choque: 2019)` = "grey60")) +
  labs(title = "Salario: el salto real frente al salto falso, con cada medida de exposición",
       subtitle = "Si las dos barras son parecidas, la medida produce efectos donde no los hay",
       x = NULL, y = "Cambio (%)", fill = NULL,
       caption = paste("Cada quintil frente al menos expuesto.",
                       "\nControles: firma, año, sector x año, tamaño x año y departamento x año.")) +
  tema_tesis
guardar_grafico(grafico_medidas, "GC05_medidas_real_vs_placebo", ancho = 13, alto = 6)

cat("\nCÓMO DECIDIR:",
    "\n  Para cada medida, compara la columna real con la del placebo.",
    "\n  - Si el placebo da cerca de cero y el real da positivo: la medida SIRVE.",
    "\n  - Si las dos dan parecido: la medida produce efectos falsos y hay que",
    "\n    descartarla, por muy significativo que salga el ejercicio real.",
    "\n  La medida C no usa el salario de la propia firma, así que es la que menos",
    "\n  debería sufrir el problema; a cambio tiene menos variación y menos firmas.\n")


# ==============================================================================
# RESUMEN
# ==============================================================================
titulo("RESUMEN")

cat("\nEfecto por quintil (empleo, %):\n")
print(as.data.frame(
  por_quintil %>%
    filter(resultado == "Empleo total (log)") %>%
    transmute(quintil, efecto = round(efecto_porcentual, 2), significancia,
              p = round(p_valor, 3))
), row.names = FALSE)

cat("\nSalto de 2022 a 2023 dentro de cada quintil (%):\n")
print(as.data.frame(
  salto_2023 %>%
    filter(anio == 2023) %>%
    transmute(resultado, quintil, salto = round(efecto_porcentual, 2), significancia)
), row.names = FALSE)

cat("\nEl cambio de 2023 según el período de comparación (%):\n")
print(as.data.frame(
  comparacion_referencias %>%
    transmute(resultado, quintil, referencia,
              cambio = round(efecto_porcentual, 2), significancia)
), row.names = FALSE)

cat("\nPLACEBO de reversión: salto de 2019 con la medida construida en 2018 (%):\n")
print(as.data.frame(
  placebo_reversion %>%
    transmute(resultado, quintil, salto = round(efecto_porcentual, 2), significancia)
), row.names = FALSE)

cat("\nMedida alternativa (salario promedio 2019-2021), cambio de 2023 (%):\n")
print(as.data.frame(
  medida_alternativa %>%
    transmute(resultado, quintil, referencia,
              cambio = round(efecto_porcentual, 2), significancia)
), row.names = FALSE)

cat("\nTRES MEDIDAS: efecto real frente a efecto falso en el quintil más expuesto (%):\n")
print(as.data.frame(resumen_medidas), row.names = FALSE)

cat("\nPrueba de no linealidad (p-valor):\n")
print(as.data.frame(
  formas %>%
    filter(!is.na(prueba_de_no_linealidad)) %>%
    transmute(resultado, forma, p = round(prueba_de_no_linealidad, 4))
), row.names = FALSE)

cat("\nCÓMO LEER ESTO:",
    "\n  Si los quintiles no son significativos y la prueba de no linealidad tampoco,",
    "\n  la especificación lineal del script principal es adecuada y el resultado nulo",
    "\n  del empleo se sostiene en toda la distribución de exposición.",
    "\n  Si aparece un efecto concentrado en un tramo, hay que reportarlo por quintiles",
    "\n  y revisar si ese grupo tiene algo particular (tamaño, sector, medición).\n")

if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_continuo.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

