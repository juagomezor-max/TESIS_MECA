# ==============================================================================
# graficar_figuras_tesis.R
#
# Regenera las figuras 1 a 6 de la tesis con un estilo unificado (el mismo de
# G09c_trayectorias_q1_q5 en 05_resultados_y_mecanismos.R). Script
# independiente: NO corre ni modifica 05_resultados_y_mecanismos.R ni
# 06_tratamiento_continuo.R, y NO estima ningún modelo -- solo lee los CSV que
# esos scripts ya dejaron en 4. RESULTADOS/ y grafica. La única excepción es
# G1, que recalcula el Kaitz y los quintiles de 2022 desde el panel (una
# réplica literal de unas pocas líneas de 06, sin estimar nada), y solo si no
# existe ya su propio CSV de apoyo.
#
# Entradas:
#   4. RESULTADOS/06_tratamiento_continuo/T16_distribucion_exposicion.csv
#   4. RESULTADOS/06_tratamiento_continuo/T21_trayectoria_salario_por_quintil.csv
#   4. RESULTADOS/06_tratamiento_continuo/T24_tres_medidas_real_vs_placebo.csv
#   4. RESULTADOS/06_tratamiento_continuo/T24_tres_medidas_real_vs_placebo_tamano.csv
#   4. RESULTADOS/05_resultados_y_mecanismos/T11b_coeficientes_tendencias.csv
#   4. RESULTADOS/05_resultados_y_mecanismos/T09_quintiles_exposicion.csv
#   4. RESULTADOS/05_resultados_y_mecanismos/T09_quintiles_exposicion_tamano.csv
#   4. RESULTADOS/05_resultados_y_mecanismos/T09b_evento_quintiles.csv
#   4. RESULTADOS/05_resultados_y_mecanismos/T09b_evento_quintiles_tamano.csv
#   1. DATOS/panel_analitico_firma_eam.rds (solo si RECALCULAR_G1 o no existe
#     el CSV de apoyo de G1)
# Salidas: 4. RESULTADOS/07_figuras_tesis/ (G1 a G6, .png, y el CSV de apoyo
#   de G1)
#
# Se corre desde la raíz del repositorio (abriendo TESIS_MECA.Rproj).
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

CARPETA <- file.path("4. RESULTADOS", "07_figuras_tesis")
dir.create(CARPETA, recursive = TRUE, showWarnings = FALSE)
CARPETA_ANEXO <- file.path(CARPETA, "anexo")
dir.create(CARPETA_ANEXO, recursive = TRUE, showWarnings = FALSE)

# --- Estilo común ------------------------------------------------------------

AZUL <- "#1F4E79"   # más expuestas (Q5) / especificación principal / choque real
GRIS <- "grey45"    # menos expuestas (Q1) / especificación secundaria / placebo

COLORES_QUINTIL <- c(Q1 = "grey45", Q2 = "#C9D6E5", Q3 = "#93ADCB",
                     Q4 = "#5A7FAA", Q5 = "#1F4E79")
ETIQUETAS_QUINTIL <- c(Q1 = "Q1 (menos expuestas)", Q2 = "Q2", Q3 = "Q3",
                       Q4 = "Q4", Q5 = "Q5 (más expuestas)")

fmt_num <- scales::label_number(decimal.mark = ",", big.mark = ".")
fmt_dec <- scales::label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1)

tema_figuras <- theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 9),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

guardar_figura <- function(grafico, nombre, alto, carpeta = CARPETA) {
  ggsave(file.path(carpeta, paste0(nombre, ".png")), grafico,
         width = 16, height = alto, units = "cm", dpi = 300, bg = "white")
  cat("Guardado:", file.path(carpeta, nombre), paste0("(16 x ", alto, " cm)\n"))
}

# Extrae el código corto de quintil ("Q2", "Q5", ...) de etiquetas largas
# como "Q5 (más expuestas)".
quintil_corto <- function(x) sub(" .*", "", x)


# ==============================================================================
# G1. DISTRIBUCIÓN DEL KAITZ DE OBREROS 2022, POR QUINTIL
# ==============================================================================
cat("\n=== G1. Distribución del Kaitz por quintil ===\n")

RECALCULAR_G1 <- FALSE
RUTA_G1_CSV <- file.path(CARPETA, "G1_kaitz_firmas_2022.csv")

if (!RECALCULAR_G1 && file.exists(RUTA_G1_CSV)) {
  cat("RECALCULAR_G1 = FALSE y", RUTA_G1_CSV, "ya existe: se lee el CSV.\n")
  firmas_g1 <- read_csv(RUTA_G1_CSV, show_col_types = FALSE)
} else {
  cat("Recalculando Kaitz y quintiles 2022 desde el panel",
      "(réplica literal de 06_tratamiento_continuo.R, líneas ~138-160)...\n")

  panel_g1 <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
    mutate(NORDEMP = as.character(NORDEMP),
           ANIO = as.integer(as.character(ANIO)))

  firmas_g1 <- panel_g1 %>%
    filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
    select(NORDEMP, Bite2022_obreros)

  limites_g1 <- quantile(firmas_g1$Bite2022_obreros, probs = c(0.01, 0.99))

  firmas_g1 <- firmas_g1 %>%
    mutate(
      kaitz = pmin(pmax(Bite2022_obreros, limites_g1[1]), limites_g1[2]),
      quintil = cut(kaitz, breaks = quantile(kaitz, probs = seq(0, 1, 0.2)),
                    labels = c("Q1", "Q2", "Q3", "Q4", "Q5"), include.lowest = TRUE)
    ) %>%
    select(NORDEMP, kaitz, quintil)

  write_csv(firmas_g1, RUTA_G1_CSV)
  cat("Guardado:", RUTA_G1_CSV, "\n")
}

# --- Control: firmas por quintil contra T16 (06) -----------------------------
conteo_g1 <- firmas_g1 %>% count(quintil) %>% arrange(quintil)
t16 <- read_csv(file.path("4. RESULTADOS", "06_tratamiento_continuo",
                          "T16_distribucion_exposicion.csv"), show_col_types = FALSE)

esperado_g1 <- setNames(t16$firmas, c("Q1", "Q2", "Q3", "Q4", "Q5"))
obtenido_g1 <- setNames(conteo_g1$n, as.character(conteo_g1$quintil))[c("Q1", "Q2", "Q3", "Q4", "Q5")]

cat("Firmas por quintil (G1) vs T16 (06):\n")
print(data.frame(quintil = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                 g1 = as.integer(obtenido_g1), t16 = as.integer(esperado_g1)))

if (!identical(as.integer(obtenido_g1), as.integer(esperado_g1))) {
  stop("G1: los conteos por quintil NO coinciden con T16_distribucion_exposicion.csv. ",
       "Deteniendo el script -- revisar antes de seguir.")
}
cat("Control G1 OK: conteos idénticos a T16 (total", sum(obtenido_g1), "firmas).\n")

# Altura real del histograma (bins=50), para ubicar la anotación al ~85% del
# máximo del eje y en vez de pegarla al tope del panel.
max_y_g1 <- max(ggplot_build(
  ggplot(firmas_g1, aes(x = kaitz)) + geom_histogram(bins = 50)
)$data[[1]]$count)

grafico_g1 <- ggplot(firmas_g1, aes(x = kaitz, fill = quintil)) +
  geom_histogram(bins = 50, color = "white") +
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "grey50") +
  annotate("text", x = 1.18, y = 0.85 * max_y_g1, label = "Mínimo de 2022 (1,16)",
           hjust = 0, size = 3, color = "grey30") +
  scale_fill_manual(values = COLORES_QUINTIL, labels = ETIQUETAS_QUINTIL) +
  scale_x_continuous(labels = fmt_dec) +
  scale_y_continuous(labels = fmt_num) +
  labs(x = "Índice de Kaitz", y = "Número de firmas", fill = NULL) +
  tema_figuras

print(grafico_g1)
guardar_figura(grafico_g1, "G1_distribucion_kaitz", 9)


# ==============================================================================
# G3. TRAYECTORIA DEL COSTO LABORAL POR QUINTIL, FRENTE A 2015
# ==============================================================================
cat("\n=== G3. Trayectoria del costo por quintil ===\n")

t21 <- read_csv(file.path("4. RESULTADOS", "06_tratamiento_continuo",
                          "T21_trayectoria_salario_por_quintil.csv"), show_col_types = FALSE) %>%
  mutate(quintil_q = quintil_corto(quintil),
         tramo = ifelse(ANIO <= 2019, "2015-2019", "2021-2024"))

# Capas separadas por quintil para controlar el orden de dibujo: Q2-Q4 abajo,
# Q1 y Q5 encima (últimas capas), para que no queden tapados.
t21_medio <- filter(t21, quintil_q %in% c("Q2", "Q3", "Q4"))
t21_q1 <- filter(t21, quintil_q == "Q1")
t21_q5 <- filter(t21, quintil_q == "Q5")

# Segmento punteado 2019 -> 2021 por quintil (sin punto en 2020, que no existe
# en los datos). Punteado ("dotted"), no rayado, para no confundirlo con la
# línea vertical de 2022,5 (esa sí es "dashed"). Se dibuja ANTES que las
# líneas sólidas para que estas no queden tapadas.
segmentos_1921 <- t21 %>%
  filter(ANIO %in% c(2019, 2021)) %>%
  select(quintil_q, ANIO, vs_2015) %>%
  pivot_wider(names_from = ANIO, values_from = vs_2015, names_prefix = "y")
seg_medio <- filter(segmentos_1921, quintil_q %in% c("Q2", "Q3", "Q4"))
seg_q1 <- filter(segmentos_1921, quintil_q == "Q1")
seg_q5 <- filter(segmentos_1921, quintil_q == "Q5")

grafico_g3 <- ggplot(t21, aes(x = ANIO, y = vs_2015, color = quintil_q,
                              group = interaction(quintil_q, tramo))) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_segment(data = seg_medio, aes(x = 2019, xend = 2021, y = y2019, yend = y2021,
                                    color = quintil_q),
               linetype = "dotted", linewidth = 0.6, inherit.aes = FALSE, show.legend = FALSE) +
  geom_segment(data = seg_q1, aes(x = 2019, xend = 2021, y = y2019, yend = y2021,
                                  color = quintil_q),
               linetype = "dotted", linewidth = 1.1, inherit.aes = FALSE, show.legend = FALSE) +
  geom_segment(data = seg_q5, aes(x = 2019, xend = 2021, y = y2019, yend = y2021,
                                  color = quintil_q),
               linetype = "dotted", linewidth = 1.1, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line(data = t21_medio, linewidth = 0.6) +
  geom_point(data = t21_medio, size = 1.4) +
  geom_line(data = t21_q1, linewidth = 1.1) +
  geom_point(data = t21_q1, size = 1.4) +
  geom_line(data = t21_q5, linewidth = 1.1) +
  geom_point(data = t21_q5, size = 1.4) +
  scale_color_manual(values = COLORES_QUINTIL, labels = ETIQUETAS_QUINTIL) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  scale_y_continuous(labels = fmt_num) +
  labs(x = NULL, y = "Cambio frente a 2015 (%)", color = NULL) +
  tema_figuras

print(grafico_g3)
guardar_figura(grafico_g3, "G3_trayectoria_costo_quintil", 9)



# ==============================================================================
# G4. EVENTO POR QUINTIL (DOSIS-RESPUESTA), SIN CONTROL POR TAMAÑO
# ==============================================================================
cat("\n=== G4. Evento por quintil (dosis-respuesta, sin tamaño) ===\n")

RESULTADOS_QUINTIL_ORDEN <- c("Costo laboral por trabajador (log)", "Empleo total (log)",
                              "Empleo permanente (log)")
RESULTADOS_QUINTIL_ETIQUETAS <- c("Costo laboral por trabajador", "Empleo total",
                                  "Empleo permanente")

leer_evento_quintiles <- function(ruta) {
  read_csv(ruta, show_col_types = FALSE) %>%
    mutate(quintil_q = paste0("Q", quintil),
           tramo = ifelse(anio <= 2019, "2015-2019", "2021-2024"),
           resultado = factor(resultado, levels = RESULTADOS_QUINTIL_ORDEN,
                              labels = RESULTADOS_QUINTIL_ETIQUETAS))
}

# Un solo gráfico para G4 (sin tamaño) y para el anexo A4_4 (con tamaño): tres
# paneles en una columna, Q2-Q5 (Q1 es la referencia, no tiene coeficiente
# propio), Q5 más grueso y con ribbon de IC95, tramo 2019-2021 punteado y sin
# leyenda (no hay dato en 2020).
graficar_evento_quintiles <- function(datos_evento) {
  datos_resto <- filter(datos_evento, quintil_q != "Q5")
  datos_q5 <- filter(datos_evento, quintil_q == "Q5")

  seg <- datos_evento %>%
    filter(anio %in% c(2019, 2021)) %>%
    select(resultado, quintil_q, anio, efecto_pct) %>%
    pivot_wider(names_from = anio, values_from = efecto_pct, names_prefix = "y")
  seg_resto <- filter(seg, quintil_q != "Q5")
  seg_q5 <- filter(seg, quintil_q == "Q5")

  ggplot(datos_evento, aes(x = anio, y = efecto_pct, color = quintil_q,
                           group = interaction(quintil_q, tramo))) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_segment(data = seg_resto, aes(x = 2019, xend = 2021, y = y2019, yend = y2021,
                                       color = quintil_q),
                 linetype = "dotted", linewidth = 0.6, inherit.aes = FALSE, show.legend = FALSE) +
    geom_segment(data = seg_q5, aes(x = 2019, xend = 2021, y = y2019, yend = y2021,
                                    color = quintil_q),
                 linetype = "dotted", linewidth = 1.1, inherit.aes = FALSE, show.legend = FALSE) +
    geom_ribbon(data = datos_q5, aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct, fill = quintil_q,
                                     group = tramo),
                alpha = 0.15, color = NA, show.legend = FALSE) +
    geom_line(data = datos_resto, linewidth = 0.6) +
    geom_point(data = datos_resto, size = 1.4) +
    geom_line(data = datos_q5, linewidth = 1.1) +
    geom_point(data = datos_q5, size = 1.4) +
    facet_wrap(~ resultado, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
    scale_y_continuous(labels = fmt_num) +
    scale_color_manual(values = COLORES_QUINTIL, labels = ETIQUETAS_QUINTIL) +
    scale_fill_manual(values = COLORES_QUINTIL, guide = "none") +
    labs(x = NULL, y = "Diferencia frente a Q1, relativa a 2022 (%)", color = NULL) +
    tema_figuras
}

t09b_sin <- leer_evento_quintiles(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                                            "T09b_evento_quintiles.csv"))

cat("Control impreso (Q5, 2023):\n")
t09b_sin %>%
  filter(quintil_q == "Q5", anio == 2023) %>%
  select(resultado, efecto_pct) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

grafico_g4 <- graficar_evento_quintiles(t09b_sin)
print(grafico_g4)
guardar_figura(grafico_g4, "G4_evento_quintiles", 16)


# ==============================================================================
# G5. SALTO REAL (2023) FRENTE AL PLACEBO (2019), POR QUINTIL -- SIN TAMAÑO
# ==============================================================================
cat("\n=== G5. Real vs placebo por quintil (sin tamaño) ===\n")

t24_sin <- read_csv(file.path("4. RESULTADOS", "06_tratamiento_continuo",
                              "T24_tres_medidas_real_vs_placebo.csv"), show_col_types = FALSE) %>%
  mutate(control = "Sin control por tamaño")
t24_con <- read_csv(file.path("4. RESULTADOS", "06_tratamiento_continuo",
                              "T24_tres_medidas_real_vs_placebo_tamano.csv"), show_col_types = FALSE) %>%
  mutate(control = "Con control por tamaño")

# --- Control de escala del error estándar: T24.error_estandar (crudo) x 100
# debe coincidir con T25.error_estandar_* (ya en puntos porcentuales).
t25 <- read_csv(file.path("4. RESULTADOS", "06_tratamiento_continuo",
                          "T25_resumen_medidas_q5.csv"), show_col_types = FALSE)
chequeo_ee <- t24_sin %>%
  filter(resultado == "Costo laboral por trabajador (log)",
         medida == "A. Salario de la firma en el año base",
         quintil == "Q5 (más expuestas)")
ee_t24_x100 <- chequeo_ee$error_estandar * 100
ee_t25 <- t25$`error_estandar_Real (choque de 2023)`[
  t25$resultado == "Costo laboral por trabajador (log)" &
    t25$medida == "A. Salario de la firma en el año base"]
cat("Chequeo de escala del error estándar (Q5, costo, medida A, real):\n")
cat("  T24.error_estandar x 100 =", ee_t24_x100[chequeo_ee$ejercicio == "Real (choque de 2023)"], "\n")
cat("  T25.error_estandar_Real  =", ee_t25, "\n")
if (abs(ee_t24_x100[chequeo_ee$ejercicio == "Real (choque de 2023)"] - ee_t25) > 1e-6) {
  stop("G5: la escala del error estándar de T24 no cuadra con T25. Deteniendo -- revisar.")
}
cat("Control de escala OK: T24.error_estandar esta en escala cruda (x100 = T25).\n")

t24_todo <- bind_rows(t24_sin, t24_con) %>%
  filter(medida == "A. Salario de la firma en el año base",
         resultado %in% c("Costo laboral por trabajador (log)", "Empleo total (log)")) %>%
  mutate(
    quintil_q = quintil_corto(quintil),
    ic95_inf = efecto_porcentual - 1.96 * 100 * error_estandar,
    ic95_sup = efecto_porcentual + 1.96 * 100 * error_estandar,
    control = factor(control, levels = c("Sin control por tamaño", "Con control por tamaño")),
    resultado = factor(resultado,
                       levels = c("Costo laboral por trabajador (log)", "Empleo total (log)"),
                       labels = c("Costo laboral por trabajador", "Empleo total")),
    # Orden de la leyenda: Real primero, Placebo después.
    ejercicio = factor(ejercicio, levels = c("Real (choque de 2023)",
                                             "Placebo (año sin choque: 2019)"))
  )

cat("\nControl impreso (Q5, medida A):\n")
t24_todo %>%
  filter(quintil_q == "Q5") %>%
  select(control, resultado, ejercicio, efecto_porcentual) %>%
  arrange(resultado, control, ejercicio) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

t24_sin_solo <- filter(t24_todo, control == "Sin control por tamaño")

grafico_g5 <- ggplot(t24_sin_solo, aes(x = quintil_q, y = efecto_porcentual,
                                       color = ejercicio, shape = ejercicio)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_errorbar(aes(ymin = ic95_inf, ymax = ic95_sup),
                position = position_dodge(width = 0.4), width = 0.15) +
  geom_point(position = position_dodge(width = 0.4), size = 1.8) +
  facet_wrap(~ resultado, scales = "free_y") +
  scale_color_manual(values = c(`Real (choque de 2023)` = AZUL,
                                `Placebo (año sin choque: 2019)` = GRIS)) +
  scale_shape_manual(values = c(`Real (choque de 2023)` = 17,
                                `Placebo (año sin choque: 2019)` = 16)) +
  scale_y_continuous(labels = fmt_num) +
  labs(x = NULL, y = "Cambio frente a Q1 (%)", color = NULL, shape = NULL) +
  tema_figuras

print(grafico_g5)
guardar_figura(grafico_g5, "G5_real_vs_placebo_quintil", 8)


# ==============================================================================
# G6. QUINTILES: FRENTE A 2022 Y FRENTE AL PROMEDIO PREVIO -- SIN TAMAÑO
# ==============================================================================
cat("\n=== G6. Quintiles, dos referencias (sin tamaño) ===\n")

t09_sin <- read_csv(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                              "T09_quintiles_exposicion.csv"), show_col_types = FALSE) %>%
  mutate(control = "Sin control por tamaño")
t09_con <- read_csv(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                              "T09_quintiles_exposicion_tamano.csv"), show_col_types = FALSE) %>%
  mutate(control = "Con control por tamaño")

t09_todo <- bind_rows(t09_sin, t09_con) %>%
  # T09 ahora también trae "2023 vs 2021" y "2024 vs 2022" (nuevas lecturas
  # agregadas en 05); G6/A4_3 solo muestran las dos de siempre.
  filter(lectura %in% c("2023 vs 2022", "2023 vs promedio previo")) %>%
  mutate(
    quintil_q = paste0("Q", quintil),
    control = factor(control, levels = c("Sin control por tamaño", "Con control por tamaño")),
    resultado = factor(resultado,
                       levels = c("Costo laboral por trabajador (log)", "Empleo total (log)",
                                 "Empleo permanente (log)"),
                       labels = c("Costo laboral", "Empleo total", "Empleo permanente")),
    lectura = factor(lectura, levels = c("2023 vs 2022", "2023 vs promedio previo"),
                     labels = c("Frente a 2022", "Frente al promedio previo"))
  )

cat("Control impreso (Q5, sin control por tamaño):\n")
t09_todo %>%
  filter(quintil_q == "Q5", control == "Sin control por tamaño") %>%
  select(resultado, lectura, efecto_pct) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

t09_sin_solo <- filter(t09_todo, control == "Sin control por tamaño")

grafico_g6 <- ggplot(t09_sin_solo, aes(x = quintil_q, y = efecto_pct,
                                       color = lectura, shape = lectura)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_errorbar(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct),
                position = position_dodge(width = 0.4), width = 0.15) +
  geom_point(position = position_dodge(width = 0.4), size = 1.8) +
  facet_wrap(~ resultado, scales = "free_y") +
  scale_color_manual(values = c(`Frente a 2022` = AZUL, `Frente al promedio previo` = GRIS)) +
  scale_shape_manual(values = c(`Frente a 2022` = 17, `Frente al promedio previo` = 16)) +
  scale_y_continuous(labels = fmt_num) +
  labs(x = NULL, y = "Cambio frente a Q1 (%)", color = NULL, shape = NULL) +
  tema_figuras

print(grafico_g6)
guardar_figura(grafico_g6, "G6_quintiles_referencias", 8)


# ==============================================================================
# ANEXO: CONTROL POR TAMAÑO x AÑO
# ==============================================================================
cat("\n=== Anexo: figuras con control por tamaño ===\n")

# --- A4_1: evento de empleo, con y sin control por tamaño (antes G5) ------------
t11b <- read_csv(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                           "T11b_coeficientes_tendencias.csv"), show_col_types = FALSE) %>%
  filter(variable == "log_empleo") %>%
  mutate(control = factor(control,
                          levels = c("Sin control por tamaño", "Con control por tamaño"),
                          labels = c("Sin control por tamaño (principal)", "Con control por tamaño")))

cat("Control impreso (log_empleo, 2023, y p de años previos):\n")
t11b %>%
  filter(anio == 2023) %>%
  select(control, efecto_pct, p_previos) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

grafico_a4_1 <- ggplot(t11b, aes(x = anio, y = efecto_pct, color = control, shape = control)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct),
                position = position_dodge(width = 0.35), width = 0) +
  geom_point(position = position_dodge(width = 0.35), size = 1.6) +
  scale_color_manual(values = c(`Sin control por tamaño (principal)` = AZUL,
                                `Con control por tamaño` = GRIS)) +
  scale_shape_manual(values = c(`Sin control por tamaño (principal)` = 17,
                                `Con control por tamaño` = 16)) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  scale_y_continuous(breaks = seq(-5, 10, 2.5), labels = fmt_num) +
  labs(x = NULL, y = "Efecto por DE de exposición (%)",
       color = NULL, shape = NULL) +
  tema_figuras

print(grafico_a4_1)
guardar_figura(grafico_a4_1, "A4_1_evento_empleo_tamano", 9, carpeta = CARPETA_ANEXO)

# --- A4_2: real vs placebo por quintil, con y sin tamaño (antes G4) -------------
# facet_grid(..., scales = "free_y") deja CADA panel libre, no cada columna.
# Para que las dos filas (sin/con tamaño) de una misma columna (resultado)
# compartan escala -- sin instalar ggh4x ni patchwork -- se agregan puntos
# invisibles con el rango (mínimo y máximo) de cada columna, repetidos en
# las dos filas: al incluirlos, ggplot expande cada panel de esa columna al
# mismo rango, y las columnas entre sí siguen siendo libres.
rango_a4_2 <- t24_todo %>%
  group_by(resultado) %>%
  summarise(ymin = min(ic95_inf), ymax = max(ic95_sup), .groups = "drop") %>%
  tidyr::crossing(control = levels(t24_todo$control)) %>%
  tidyr::pivot_longer(c(ymin, ymax), values_to = "y") %>%
  mutate(control = factor(control, levels = levels(t24_todo$control)),
         quintil_q = t24_todo$quintil_q[1])

grafico_a4_2 <- ggplot(t24_todo, aes(x = quintil_q, y = efecto_porcentual,
                                     color = ejercicio, shape = ejercicio)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_point(data = rango_a4_2, aes(x = quintil_q, y = y), alpha = 0, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin = ic95_inf, ymax = ic95_sup),
                position = position_dodge(width = 0.4), width = 0.15) +
  geom_point(position = position_dodge(width = 0.4), size = 1.8) +
  facet_grid(control ~ resultado, scales = "free_y") +
  scale_color_manual(values = c(`Real (choque de 2023)` = AZUL,
                                `Placebo (año sin choque: 2019)` = GRIS)) +
  scale_shape_manual(values = c(`Real (choque de 2023)` = 17,
                                `Placebo (año sin choque: 2019)` = 16)) +
  scale_y_continuous(labels = fmt_num) +
  labs(x = NULL, y = "Cambio frente a Q1 (%)", color = NULL, shape = NULL) +
  tema_figuras

print(grafico_a4_2)
guardar_figura(grafico_a4_2, "A4_2_placebo_quintil_tamano", 13, carpeta = CARPETA_ANEXO)

# --- A4_3: quintiles, dos referencias, con y sin tamaño (antes G6) --------------
grafico_a4_3 <- ggplot(t09_todo, aes(x = quintil_q, y = efecto_pct,
                                     color = lectura, shape = lectura)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_errorbar(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct),
                position = position_dodge(width = 0.4), width = 0.15) +
  geom_point(position = position_dodge(width = 0.4), size = 1.8) +
  facet_grid(control ~ resultado, scales = "free_y") +
  scale_color_manual(values = c(`Frente a 2022` = AZUL, `Frente al promedio previo` = GRIS)) +
  scale_shape_manual(values = c(`Frente a 2022` = 17, `Frente al promedio previo` = 16)) +
  scale_y_continuous(labels = fmt_num) +
  labs(x = NULL, y = "Cambio frente a Q1 (%)", color = NULL, shape = NULL) +
  tema_figuras

print(grafico_a4_3)
guardar_figura(grafico_a4_3, "A4_3_quintiles_referencias_tamano", 12, carpeta = CARPETA_ANEXO)

# --- A4_4: evento por quintil (dosis-respuesta), con control por tamaño --------
t09b_con <- leer_evento_quintiles(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                                            "T09b_evento_quintiles_tamano.csv"))

grafico_a4_4 <- graficar_evento_quintiles(t09b_con)
print(grafico_a4_4)
guardar_figura(grafico_a4_4, "A4_4_evento_quintiles_tamano", 16, carpeta = CARPETA_ANEXO)

cat("\nFIN: figuras G1, G3, G4, G5 y G6 guardadas en", CARPETA, "\n")
cat("Anexo (control por tamaño) guardado en", CARPETA_ANEXO, "\n")
cat("(G2 no se pidió en esta tanda; G9 se actualiza aparte en",
    "herramientas/graficar_tendencias_paralelas.R)\n")
