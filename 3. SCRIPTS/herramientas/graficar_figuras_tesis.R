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

guardar_figura <- function(grafico, nombre, alto) {
  ggsave(file.path(CARPETA, paste0(nombre, ".png")), grafico,
         width = 16, height = alto, units = "cm", dpi = 300, bg = "white")
  cat("Guardado:", nombre, paste0("(16 x ", alto, " cm)\n"))
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

grafico_g3 <- ggplot(t21, aes(x = ANIO, y = vs_2015, color = quintil_q,
                              group = interaction(quintil_q, tramo))) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
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
# G4. SALTO REAL (2023) FRENTE AL PLACEBO (2019), POR QUINTIL
# ==============================================================================
cat("\n=== G4. Real vs placebo por quintil ===\n")

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
  stop("G4: la escala del error estándar de T24 no cuadra con T25. Deteniendo -- revisar.")
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

# facet_grid(..., scales = "free_y") deja CADA panel libre, no cada columna.
# Para que las dos filas (sin/con tamaño) de una misma columna (resultado)
# compartan escala -- sin instalar ggh4x ni patchwork -- se agregan puntos
# invisibles con el rango (mínimo y máximo) de cada columna, repetidos en
# las dos filas: al incluirlos, ggplot expande cada panel de esa columna al
# mismo rango, y las columnas entre sí siguen siendo libres.
rango_g4 <- t24_todo %>%
  group_by(resultado) %>%
  summarise(ymin = min(ic95_inf), ymax = max(ic95_sup), .groups = "drop") %>%
  tidyr::crossing(control = levels(t24_todo$control)) %>%
  tidyr::pivot_longer(c(ymin, ymax), values_to = "y") %>%
  mutate(control = factor(control, levels = levels(t24_todo$control)),
         quintil_q = t24_todo$quintil_q[1])

cat("\nControl impreso (Q5, medida A):\n")
t24_todo %>%
  filter(quintil_q == "Q5") %>%
  select(control, resultado, ejercicio, efecto_porcentual) %>%
  arrange(resultado, control, ejercicio) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

grafico_g4 <- ggplot(t24_todo, aes(x = quintil_q, y = efecto_porcentual,
                                   color = ejercicio, shape = ejercicio)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_point(data = rango_g4, aes(x = quintil_q, y = y), alpha = 0, inherit.aes = FALSE) +
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

print(grafico_g4)
guardar_figura(grafico_g4, "G4_real_vs_placebo_quintil", 13)


# ==============================================================================
# G5. EVENTO DE EMPLEO, CON Y SIN CONTROL POR TAMAÑO
# ==============================================================================
cat("\n=== G5. Evento empleo con/sin tamaño ===\n")

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

grafico_g5 <- ggplot(t11b, aes(x = anio, y = efecto_pct, color = control, shape = control)) +
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

print(grafico_g5)
guardar_figura(grafico_g5, "G5_evento_empleo_tamano", 9)


# ==============================================================================
# G6. QUINTILES: FRENTE A 2022 Y FRENTE AL PROMEDIO PREVIO, CON Y SIN TAMAÑO
# ==============================================================================
cat("\n=== G6. Quintiles, dos referencias ===\n")

t09_sin <- read_csv(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                              "T09_quintiles_exposicion.csv"), show_col_types = FALSE) %>%
  mutate(control = "Sin control por tamaño")
t09_con <- read_csv(file.path("4. RESULTADOS", "05_resultados_y_mecanismos",
                              "T09_quintiles_exposicion_tamano.csv"), show_col_types = FALSE) %>%
  mutate(control = "Con control por tamaño")

t09_todo <- bind_rows(t09_sin, t09_con) %>%
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

grafico_g6 <- ggplot(t09_todo, aes(x = quintil_q, y = efecto_pct,
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

print(grafico_g6)
guardar_figura(grafico_g6, "G6_quintiles_referencias", 12)

cat("\nFIN: figuras G1, G3, G4, G5 y G6 guardadas en", CARPETA, "\n")
cat("(G2 no se pidió en esta tanda; G9 se actualiza aparte en",
    "herramientas/graficar_tendencias_paralelas.R)\n")
