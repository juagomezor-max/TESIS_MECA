# ==============================================================================
# 01_exposicion_alternativa.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# Construye las medidas alternativas de exposición al aumento del salario mínimo
# de 2023 y las valida. NO estima efectos sobre empleo: la medida principal se
# elige por el primer eslabón, según la regla ya comiteada en NOTA_DECISIONES.md.
#
# Entrada:  1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  1. DATOS/exposicion_alternativa_2022.rds   (una fila por firma)
#           4. RESULTADOS/Exposicion_alternativa/       (tablas y figuras)
#
# Para correrlo abrimos TESIS_MECA.Rproj, así R trabaja desde la raíz del
# repositorio y encuentra las carpetas.
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulo:     2. Medición
# Pregunta:     ¿Cómo se construyen las medidas alternativas de exposición
#               (golpe_c, golpe_a, golpe_costo, brecha) al aumento del mínimo
#               de 2023?
# Cifra clave:  5.742 firmas con golpe_c definido, frente a 5.099 con el Kaitz
#               de obreros (Bite2022_obreros) -- 643 recuperadas (sección 7,
#               tabla T06).
# Depende de:   (nada propio del proyecto -- lee directamente
#               1. DATOS/panel_firma_eam_expalt_completo.rds)
# Alimenta a:   05_primer_eslabon_medidas.R (prueba cuál medida
#               predice el choque), 06_decision_medida.R (cierra
#               la decisión de medida principal)
#
# OJO -- hay material de otro capítulo dentro de este script:
#   La sección 8 (tabla T09_cerca_del_minimo_por_categoria, gráfico G02)
#   reporta que el obrero permanente típico gana ~1,44 salarios mínimos. Esa
#   cifra es un DESCRIPTIVO de la distribución salarial -- capítulo 1 (quién
#   está expuesto), no un resultado de medición (capítulo 2). Se calcula aquí
#   porque sostiene el argumento metodológico de usar las tres categorías en
#   vez de solo obreros, pero si se cita en la tesis como hecho descriptivo,
#   corresponde al capítulo 1, no a este.
# ------------------------------------------------------------------------------


# ==============================================================================
# LA IDEA
# ==============================================================================
#
# El Kaitz que usamos hasta ahora (Bite2022_obreros) mira solo a los obreros:
#
#     Bite = salario mínimo anual 2023 / salario promedio del obrero en 2022
#
# Tiene dos problemas que este script intenta resolver.
#
# 1. La categoría ocupacional no dice por sí sola qué tan cerca del mínimo está
#    un trabajador. Hay administrativos ganando cerca del mínimo y obreros muy
#    por encima. Mirar solo obreros deja fuera exposición real.
#
# 2. El 20,5% de las firmas-año no tiene obreros permanentes: su producción la
#    hacen temporales o personal de agencias. El Kaitz las deja sin denominador
#    y las bota de la muestra. Son justo las firmas que ya usan la temporalidad
#    como margen de ajuste, que es uno de los mecanismos que queremos estudiar.
#
# Construimos cuatro objetos. Tres son candidatos a tratamiento y uno es
# descriptivo:
#
#   golpe_c      Salario mínimo sobre el promedio SIMPLE de los salarios de las
#                tres categorías. Mide el nivel salarial de la firma sin que
#                influya su composición ocupacional: dos firmas que pagan igual
#                salen iguales aunque una sea 90% obreros y la otra 90%
#                administrativos.
#
#   golpe_a      Lo mismo pero ponderando cada categoría por su peso en el
#                empleo. Se diferencia de golpe_c en qué categoría "manda": el
#                promedio simple se deja arrastrar por la categoría cara, el
#                ponderado (armónico) por la barata. Cuál representa mejor la
#                exposición a un piso salarial es una pregunta empírica.
#
#   golpe_costo  Sobre el costo laboral completo (con prestaciones, cotizaciones
#                y parafiscales) en lugar del salario contractual.
#
#   brecha       Salario promedio de la firma sobre el promedio simple de las
#                categorías. Mide COMPOSICIÓN ocupacional. NO es candidata a
#                tratamiento (ver la advertencia en la sección 5).
#
# Restricciones que vienen del trabajo de construcción del panel:
#
#   - Sin salario integral (C3R1): no existe en 2022. Los trabajadores con
#     salario integral (>=10 SMLV por ley) quedan en el denominador sin su
#     remuneración en el numerador, así que los salarios promedio están
#     subestimados justo en las firmas de personal caro. Es una limitación de
#     medición que hay que declarar en la tesis, no un problema que este script
#     pueda arreglar.
#
#   - Numerador y denominador sobre la misma población, siempre. Las
#     cotizaciones y los parafiscales no están desagregados por tipo de vínculo,
#     así que solo hay dos combinaciones consistentes: salarial sobre
#     permanentes, o costo total sobre ocupados sin propietarios. No hay punto
#     intermedio.
# ==============================================================================


# ==============================================================================
# 0. PREPARACIÓN
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "Exposicion_alternativa")
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
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3, carpeta = CARPETA) {
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

tema_tesis <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey35"),
        plot.caption = element_text(color = "grey45", hjust = 0),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"

# --- Parámetros legales --------------------------------------------------------
# Las masas salariales de la EAM están en MILES de pesos y son ANUALES, así que
# los referentes tienen que estar en las mismas unidades.
#
# AVISO: el referente entra igual en todas las firmas, así que un error aquí NO
# cambia el ordenamiento de la exposición ni la identificación. Solo cambia la
# lectura en "veces el mínimo". Aun así, verificar las cifras marcadas antes de
# reportarlas en la tesis.

SMLV_2023_MENSUAL <- 1160000                            # decreto de salario mínimo
SMLV_2023_ANUAL_MILES <- SMLV_2023_MENSUAL * 12 / 1000  # = 13.920 miles de pesos

# Costo mínimo total de contratación 2023: qué le cuesta al empleador un
# trabajador de salario mínimo, con todo incluido. Desde la Ley 1607, los
# trabajadores que ganan menos de 10 SMLV están exentos de SENA, ICBF y del
# aporte de salud del empleador; la caja de compensación sí se paga.
#
# VERIFICAR antes de citar en la tesis, en especial el auxilio de transporte.
AUXILIO_TRANSPORTE_2023 <- 140606   # VERIFICAR con el decreto
TASA_PRESTACIONES <- 0.2183         # cesantías 8,33 + intereses 1 + prima 8,33 + vacaciones 4,17
TASA_PENSION_EMPLEADOR <- 0.12
TASA_ARL <- 0.00522                 # clase I; varía por clase de riesgo
TASA_CAJA <- 0.04

base_mensual <- SMLV_2023_MENSUAL + AUXILIO_TRANSPORTE_2023
COSTO_MINIMO_2023_ANUAL_MILES <- (
  base_mensual +
    SMLV_2023_MENSUAL * (TASA_PENSION_EMPLEADOR + TASA_ARL + TASA_CAJA) +
    base_mensual * TASA_PRESTACIONES
) * 12 / 1000

cat("Salario mínimo anual 2023 (miles):", round(SMLV_2023_ANUAL_MILES, 1), "\n")
cat("Costo mínimo total anual 2023 (miles):", round(COSTO_MINIMO_2023_ANUAL_MILES, 1), "\n")
cat("Razón costo total / salario:", round(COSTO_MINIMO_2023_ANUAL_MILES / SMLV_2023_ANUAL_MILES, 3), "\n")


# ==============================================================================
# 1. DATOS
# ==============================================================================
titulo("1. CARGA DE DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

cat("Panel:", nrow(panel), "filas,", ncol(panel), "columnas,",
    n_distinct(panel$NORDEMP), "firmas\n")
cat("Años:", paste(sort(unique(panel$ANIO)), collapse = ", "), "\n")

# Columnas que necesita este script. Si falta alguna, paramos: es preferible un
# error claro a una medida construida sobre una columna equivocada.
columnas_necesarias <- c(
  "NORDEMP", "ANIO", "CIIU4", "DPTO", "tamano_empresa",
  "obreros_permanentes", "profesional_tecnico_permanentes", "administrativos_permanentes",
  "sueldos_permanentes_obreros_c3r2c1",
  "sueldos_permanentes_profesional_tecnico_c3r2pt",
  "sueldos_permanentes_administrativos_c3r2c2",
  "prestaciones_permanentes_obreros_c3r3c1",
  "prestaciones_permanentes_profesional_tecnico_c3r3pt",
  "prestaciones_permanentes_administrativos_c3r3c2",
  "costos_totales_personal_total_c3r10c3",
  "empleo_total_sin_propietarios"
)
faltan <- setdiff(columnas_necesarias, names(panel))
if (length(faltan) > 0) stop("Faltan columnas en el panel: ", paste(faltan, collapse = ", "))
cat("Todas las columnas necesarias están en el panel.\n")


# ==============================================================================
# 2. SALARIO PROMEDIO POR CATEGORÍA
# ==============================================================================
titulo("2. SALARIO PROMEDIO POR CATEGORÍA")

# Regla que no se negocia: numerador y denominador sobre la misma población.
# C3R2 (sueldos) y C3R3 (prestaciones) son del personal PERMANENTE, así que el
# denominador son los permanentes de la categoría. Propietarios fuera (no tienen
# remuneración fija por definición). Aprendices fuera (su pago va en R4CSAP, en
# fila aparte).
#
# Sin C3R1 (salario integral): no existe en 2022. Ver la advertencia del
# encabezado.
#
# salario_categoria(): devuelve NA cuando la categoría no tiene permanentes. Eso
# es lo correcto: la categoría simplemente no existe en esa firma y no debe
# entrar al promedio ni al divisor.
salario_categoria <- function(sueldos, prestaciones, personas) {
  ifelse(personas > 0 & (sueldos + prestaciones) > 0,
         (sueldos + prestaciones) / personas,
         NA_real_)
}

panel <- panel %>%
  mutate(
    w_obreros = salario_categoria(sueldos_permanentes_obreros_c3r2c1,
                                  prestaciones_permanentes_obreros_c3r3c1,
                                  obreros_permanentes),
    w_profesional = salario_categoria(sueldos_permanentes_profesional_tecnico_c3r2pt,
                                      prestaciones_permanentes_profesional_tecnico_c3r3pt,
                                      profesional_tecnico_permanentes),
    w_administrativo = salario_categoria(sueldos_permanentes_administrativos_c3r2c2,
                                         prestaciones_permanentes_administrativos_c3r3c2,
                                         administrativos_permanentes),
    
    # Cuántas de las tres categorías tienen dato utilizable
    n_categorias = (!is.na(w_obreros)) + (!is.na(w_profesional)) + (!is.na(w_administrativo)),
    
    # w_firma: promedio SIMPLE de los salarios de categoría, dividido por el
    # número de categorías PRESENTES (nunca por 3 fijo). Si se dividiera por 3,
    # una firma sin profesionales tendría un numerador de dos términos sobre un
    # divisor de tres y saldría mecánicamente más expuesta que una firma
    # idéntica con las tres categorías.
    w_firma = ifelse(
      n_categorias > 0,
      rowSums(cbind(w_obreros, w_profesional, w_administrativo), na.rm = TRUE) / n_categorias,
      NA_real_
    ),
    
    # Costo laboral por trabajador: C3R10 cubre a TODO el personal ocupado, así
    # que el denominador son los ocupados sin propietarios. No se puede mezclar
    # con los salarios por categoría de arriba: son poblaciones distintas.
    c_firma = ifelse(empleo_total_sin_propietarios > 0,
                     costos_totales_personal_total_c3r10c3 / empleo_total_sin_propietarios,
                     NA_real_),
    
    # Permanentes totales, para los pesos de golpe_a
    permanentes_total = rowSums(cbind(
      ifelse(is.na(w_obreros), 0, obreros_permanentes),
      ifelse(is.na(w_profesional), 0, profesional_tecnico_permanentes),
      ifelse(is.na(w_administrativo), 0, administrativos_permanentes)
    ), na.rm = TRUE)
  )

# Cobertura de cada categoría en 2022
cobertura <- panel %>%
  filter(ANIO == 2022) %>%
  summarise(
    firmas = n(),
    con_obreros = sum(!is.na(w_obreros)),
    con_profesional = sum(!is.na(w_profesional)),
    con_administrativo = sum(!is.na(w_administrativo)),
    con_alguna = sum(n_categorias > 0),
    solo_una = sum(n_categorias == 1),
    dos = sum(n_categorias == 2),
    las_tres = sum(n_categorias == 3),
    sin_ninguna = sum(n_categorias == 0)
  ) %>%
  pivot_longer(everything(), names_to = "concepto", values_to = "firmas_2022") %>%
  mutate(porcentaje = round(100 * firmas_2022 / firmas_2022[concepto == "firmas"], 2))

ver(cobertura, filas = 15)
guardar_tabla(cobertura, "T01_cobertura_categorias_2022",
              "Tabla 1. Cobertura de las categorías ocupacionales en 2022", decimales = 2)

cat("\nLECTURA: la diferencia entre 'con_obreros' y 'con_alguna' es la muestra\n",
    "que recuperamos frente al Kaitz de obreros.\n")


# ==============================================================================
# 3. MEDIDAS DE EXPOSICIÓN
# ==============================================================================
titulo("3. MEDIDAS DE EXPOSICIÓN")

# construir_medidas(): arma las cuatro medidas para un año base dado. Lo hacemos
# función porque además de 2022 necesitamos 2019 como robustez (ver sección 6).
construir_medidas <- function(base, anio_base) {
  
  base %>%
    filter(ANIO == anio_base) %>%
    mutate(
      # --- golpe_c: nivel salarial sin composición ---------------------------
      golpe_c = ifelse(!is.na(w_firma) & w_firma > 0,
                       SMLV_2023_ANUAL_MILES / w_firma, NA_real_),
      
      # --- golpe_a: armónica ponderada por peso en el empleo -----------------
      # Cada categoría aporta su propio bite, ponderado por su participación.
      # Las categorías ausentes tienen peso cero y no entran.
      peso_obreros = ifelse(is.na(w_obreros) | permanentes_total == 0, 0,
                            obreros_permanentes / permanentes_total),
      peso_profesional = ifelse(is.na(w_profesional) | permanentes_total == 0, 0,
                                profesional_tecnico_permanentes / permanentes_total),
      peso_administrativo = ifelse(is.na(w_administrativo) | permanentes_total == 0, 0,
                                   administrativos_permanentes / permanentes_total),
      
      golpe_a = ifelse(
        permanentes_total > 0 & n_categorias > 0,
        peso_obreros * ifelse(is.na(w_obreros), 0, SMLV_2023_ANUAL_MILES / w_obreros) +
          peso_profesional * ifelse(is.na(w_profesional), 0, SMLV_2023_ANUAL_MILES / w_profesional) +
          peso_administrativo * ifelse(is.na(w_administrativo), 0, SMLV_2023_ANUAL_MILES / w_administrativo),
        NA_real_
      ),
      
      # --- golpe_costo: sobre costo laboral completo -------------------------
      golpe_costo = ifelse(!is.na(c_firma) & c_firma > 0,
                           COSTO_MINIMO_2023_ANUAL_MILES / c_firma, NA_real_),
      
      # --- brecha: composición ocupacional (NO es tratamiento) ---------------
      brecha = ifelse(!is.na(c_firma) & !is.na(w_firma) & w_firma > 0,
                      c_firma / w_firma, NA_real_),
      
      # golpe sobre el salario promedio de firma: el Kaitz "ingenuo", que sirve
      # para la descomposición de la sección 5
      golpe_promedio = ifelse(!is.na(c_firma) & c_firma > 0,
                              SMLV_2023_ANUAL_MILES / c_firma, NA_real_)
    ) %>%
    select(NORDEMP, CIIU4, DPTO, tamano_empresa,
           w_obreros, w_profesional, w_administrativo, w_firma, c_firma,
           n_categorias, permanentes_total, empleo_total_sin_propietarios,
           golpe_c, golpe_a, golpe_costo, brecha, golpe_promedio)
}

exposicion <- construir_medidas(panel, 2022)

cat("Firmas en 2022:", nrow(exposicion), "\n")
cat("Con golpe_c:", sum(!is.na(exposicion$golpe_c)), "\n")
cat("Con golpe_a:", sum(!is.na(exposicion$golpe_a)), "\n")
cat("Con golpe_costo:", sum(!is.na(exposicion$golpe_costo)), "\n")

# --- Winsorización y estandarización -------------------------------------------
# Recortamos al 1% y 99% igual que el Kaitz actual, y estandarizamos a
# desviación estándar 1 para que los coeficientes de las regresiones se lean
# como "efecto de una DE más de exposición" y sean comparables entre medidas.
winsorizar <- function(x, p = c(0.01, 0.99)) {
  lim <- quantile(x, probs = p, na.rm = TRUE)
  pmin(pmax(x, lim[1]), lim[2])
}

exposicion <- exposicion %>%
  mutate(
    across(c(golpe_c, golpe_a, golpe_costo, brecha),
           winsorizar, .names = "{.col}_w"),
    across(ends_with("_w"),
           ~ .x / sd(.x, na.rm = TRUE), .names = "{.col}_de")
  ) %>%
  rename_with(~ gsub("_w_de$", "_de", .x), ends_with("_w_de"))

# Descriptivos de cada medida
descriptivos <- exposicion %>%
  select(golpe_c, golpe_a, golpe_costo, brecha) %>%
  pivot_longer(everything(), names_to = "medida", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  group_by(medida) %>%
  summarise(
    firmas = n(),
    media = mean(valor),
    de = sd(valor),
    p1 = quantile(valor, 0.01),
    p10 = quantile(valor, 0.10),
    mediana = median(valor),
    p90 = quantile(valor, 0.90),
    p99 = quantile(valor, 0.99),
    pct_en_cero = 100 * mean(valor == 0),
    .groups = "drop"
  )

ver(descriptivos)
guardar_tabla(descriptivos, "T02_descriptivos_medidas",
              "Tabla 2. Distribución de las medidas de exposición (2022)", decimales = 4)

# Lectura en "veces el mínimo": el inverso del golpe dice cuántos salarios
# mínimos cuesta el trabajador típico. Sirve para verificar que las unidades
# están bien: si sale un número absurdo, hay un error de escala en alguna parte.
veces_minimo <- exposicion %>%
  summarise(
    mediana_veces_minimo_golpe_c = median(1 / golpe_c, na.rm = TRUE),
    mediana_veces_minimo_golpe_costo = median(1 / golpe_costo, na.rm = TRUE),
    mediana_w_firma_miles = median(w_firma, na.rm = TRUE),
    mediana_c_firma_miles = median(c_firma, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "concepto", values_to = "valor")

ver(veces_minimo)
cat("\nCONTROL DE UNIDADES: las dos primeras filas deben dar un número de veces\n",
    "el salario mínimo económicamente plausible (del orden de 1,5 a 4). Si no,\n",
    "hay un error de escala y NO se debe seguir.\n")


# ==============================================================================
# 4. ¿ESTO ES UN PROXY DE TAMAÑO? (chequeo bloqueante)
# ==============================================================================
titulo("4. CHEQUEO: ¿LAS MEDIDAS SON UN PROXY DE TAMAÑO?")

# Una versión anterior de esta medida dividía la suma de salarios por el empleo
# total de la firma. Eso la volvía esencialmente el inverso del tamaño: los
# salarios promedio varían entre firmas por un factor de dos o tres, mientras
# que el empleo varía por un factor de cien.
#
# Importa porque la identificación de la tesis descansa en los controles de
# tamaño x año. Una exposición casi colineal con el tamaño dejaría al diseño sin
# variación con la cual identificar el efecto.

proxy_tamano <- exposicion %>%
  mutate(menos_log_empleo = -log(ifelse(empleo_total_sin_propietarios > 0,
                                        empleo_total_sin_propietarios, NA))) %>%
  summarise(
    across(c(golpe_c, golpe_a, golpe_costo, brecha),
           ~ cor(log(ifelse(.x > 0, .x, NA)), menos_log_empleo, use = "complete.obs"))
  ) %>%
  pivot_longer(everything(), names_to = "medida", values_to = "correlacion_con_menos_log_empleo")

ver(proxy_tamano)
guardar_tabla(proxy_tamano, "T03_chequeo_proxy_tamano",
              "Tabla 3. Correlación de cada medida con el inverso del tamaño", decimales = 4)

if (any(abs(proxy_tamano$correlacion_con_menos_log_empleo) > 0.5, na.rm = TRUE)) {
  warning("ALERTA: alguna medida correlaciona por encima de 0,5 con el inverso ",
          "del tamaño. Revisar antes de usarla como tratamiento: puede quedar ",
          "colineal con el control de tamaño x año.")
}


# ==============================================================================
# 5. DESCOMPOSICIÓN: NIVEL Y COMPOSICIÓN
# ==============================================================================
titulo("5. DESCOMPOSICIÓN DEL KAITZ EN NIVEL Y COMPOSICIÓN")

# El Kaitz calculado sobre el salario promedio de la firma se parte de forma
# exacta en dos piezas:
#
#     log(golpe_promedio) = log(golpe_c) - log(brecha)
#
# golpe_c es el NIVEL salarial (qué tan bien paga la firma) y brecha es la
# COMPOSICIÓN ocupacional (en qué categorías está concentrado su empleo).
#
# Esto explica con aritmética, no con correlaciones, por qué Exposure2022_obreros
# (composición) y Bite2022_obreros (nivel) correlacionan solo 0,124: son
# proyecciones sobre ejes distintos del mismo objeto.
#
# ADVERTENCIA: brecha NO es candidata a variable de tratamiento. La composición
# ocupacional es uno de los márgenes de ajuste que la tesis estudia como
# RESULTADO. Usarla en los dos lados sería circular, igual que pasa con la
# participación de obreros.

descomposicion <- exposicion %>%
  filter(!is.na(golpe_promedio), !is.na(golpe_c), !is.na(brecha),
         golpe_promedio > 0, golpe_c > 0, brecha > 0) %>%
  mutate(
    lado_izquierdo = log(golpe_promedio),
    lado_derecho = log(golpe_c) - log(brecha),
    error = abs(lado_izquierdo - lado_derecho)
  )

cat("Firmas en la descomposición:", nrow(descomposicion), "\n")
cat("La identidad cierra con error < 0,001 en:",
    round(100 * mean(descomposicion$error < 0.001), 2), "% de las firmas\n")

varianzas <- descomposicion %>%
  summarise(
    var_nivel = var(log(golpe_c)),
    var_composicion = var(log(brecha)),
    covarianza = cov(log(golpe_c), log(brecha)),
    var_kaitz_ingenuo = var(lado_izquierdo)
  ) %>%
  pivot_longer(everything(), names_to = "componente", values_to = "valor") %>%
  mutate(porcentaje_de_la_varianza_total = round(
    100 * valor / valor[componente == "var_kaitz_ingenuo"], 1))

ver(varianzas)
guardar_tabla(varianzas, "T04_descomposicion_varianza",
              "Tabla 4. Descomposición de la varianza del Kaitz en nivel y composición",
              decimales = 4)

cat("\nLECTURA: si var_nivel domina, el Kaitz está capturando sobre todo cuánto\n",
    "paga la firma. Si domina var_composicion, está capturando sobre todo en\n",
    "qué categorías tiene su empleo, que es una variable de resultado.\n")


# ==============================================================================
# 6. AÑO BASE ALTERNATIVO (2019)
# ==============================================================================
titulo("6. MEDIDAS CON AÑO BASE 2019")

# Robustez frente al sesgo de división. El error de medición del salario de 2022
# está en el denominador de la exposición y también en la base del crecimiento
# del costo laboral 2022->2023, que es el outcome del primer eslabón. Parte de
# la correlación entre ambos es por tanto mecánica, no económica.
#
# Construir la medida con 2019 rompe ese traslape: el error de medición de 2019
# no tiene por qué estar en la base de 2022.
#
# golpe_costo es MÁS vulnerable a este problema que golpe_c, porque su
# denominador es casi el mismo objeto que el outcome del primer eslabón.

exposicion_2019 <- construir_medidas(panel, 2019) %>%
  mutate(across(c(golpe_c, golpe_a, golpe_costo, brecha), winsorizar,
                .names = "{.col}_w")) %>%
  mutate(across(ends_with("_w"), ~ .x / sd(.x, na.rm = TRUE), .names = "{.col}_de")) %>%
  rename_with(~ gsub("_w_de$", "_de", .x), ends_with("_w_de")) %>%
  select(NORDEMP, golpe_c_2019 = golpe_c, golpe_a_2019 = golpe_a,
         golpe_costo_2019 = golpe_costo, brecha_2019 = brecha,
         golpe_c_2019_de = golpe_c_de, golpe_a_2019_de = golpe_a_de,
         golpe_costo_2019_de = golpe_costo_de)

exposicion <- left_join(exposicion, exposicion_2019, by = "NORDEMP")

estabilidad <- exposicion %>%
  summarise(
    firmas_con_ambos_anios = sum(!is.na(golpe_c) & !is.na(golpe_c_2019)),
    cor_golpe_c_2022_2019 = cor(golpe_c, golpe_c_2019, use = "complete.obs"),
    cor_golpe_a_2022_2019 = cor(golpe_a, golpe_a_2019, use = "complete.obs"),
    cor_golpe_costo_2022_2019 = cor(golpe_costo, golpe_costo_2019, use = "complete.obs")
  ) %>%
  pivot_longer(everything(), names_to = "concepto", values_to = "valor")

ver(estabilidad)
guardar_tabla(estabilidad, "T05_estabilidad_base_2019",
              "Tabla 5. Estabilidad de las medidas entre 2019 y 2022", decimales = 4)


# ==============================================================================
# 7. COMPARACIÓN CON LAS MEDIDAS EXISTENTES
# ==============================================================================
titulo("7. COMPARACIÓN CON EL KAITZ ACTUAL")

# Traemos Bite2022_obreros y la proporción de obreros desde el panel analítico
# que ya está en uso. Si el archivo no está, seguimos sin la comparación en vez
# de detener el script.
ruta_panel_viejo <- file.path("1. DATOS", "panel_analitico_firma_eam.rds")

if (file.exists(ruta_panel_viejo)) {
  
  medidas_viejas <- read_rds(ruta_panel_viejo) %>%
    mutate(NORDEMP = as.character(NORDEMP),
           ANIO = as.integer(as.character(ANIO))) %>%
    filter(ANIO == 2022) %>%
    select(NORDEMP, any_of(c("Bite2022_obreros", "Exposure2022_obreros")))
  
  comparacion <- left_join(exposicion, medidas_viejas, by = "NORDEMP")
  
  # Cuántas firmas recuperamos frente al Kaitz de obreros
  recuperacion <- comparacion %>%
    summarise(
      firmas_2022 = n(),
      con_bite_obreros = sum(!is.na(Bite2022_obreros)),
      con_golpe_c = sum(!is.na(golpe_c)),
      recuperadas = sum(is.na(Bite2022_obreros) & !is.na(golpe_c)),
      perdidas = sum(!is.na(Bite2022_obreros) & is.na(golpe_c)),
      sin_ninguna = sum(is.na(Bite2022_obreros) & is.na(golpe_c))
    ) %>%
    pivot_longer(everything(), names_to = "concepto", values_to = "firmas")
  
  ver(recuperacion)
  guardar_tabla(recuperacion, "T06_recuperacion_muestra",
                "Tabla 6. Firmas recuperadas frente al Kaitz de obreros", decimales = 0)
  
  # Correlaciones entre todas las medidas
  matriz <- comparacion %>%
    select(any_of(c("golpe_c", "golpe_a", "golpe_costo", "brecha",
                    "Bite2022_obreros", "Exposure2022_obreros")))
  
  correlaciones <- expand_grid(medida_1 = names(matriz), medida_2 = names(matriz)) %>%
    rowwise() %>%
    mutate(
      pearson = cor(matriz[[medida_1]], matriz[[medida_2]],
                    use = "complete.obs", method = "pearson"),
      spearman = cor(matriz[[medida_1]], matriz[[medida_2]],
                     use = "complete.obs", method = "spearman"),
      n = sum(!is.na(matriz[[medida_1]]) & !is.na(matriz[[medida_2]]))
    ) %>%
    ungroup() %>%
    filter(medida_1 < medida_2)
  
  ver(correlaciones, filas = 20)
  guardar_tabla(correlaciones, "T07_correlaciones_entre_medidas",
                "Tabla 7. Correlaciones entre las medidas de exposición", decimales = 4)
  
  cat("\nLECTURA CLAVE: la correlación entre golpe_c y golpe_a dice cuánto aporta\n",
      "ponderar por composición. Si es muy alta (>0,9), la ponderación no agrega\n",
      "nada y conviene quedarse con la medida más simple de explicar.\n")
  
  # Transición por quintiles: cuántas firmas cambian de grupo al cambiar de medida
  if (sum(!is.na(comparacion$Bite2022_obreros) & !is.na(comparacion$golpe_c)) > 100) {
    transicion <- comparacion %>%
      filter(!is.na(Bite2022_obreros), !is.na(golpe_c)) %>%
      mutate(
        quintil_bite = ntile(Bite2022_obreros, 5),
        quintil_golpe_c = ntile(golpe_c, 5)
      ) %>%
      count(quintil_bite, quintil_golpe_c) %>%
      pivot_wider(names_from = quintil_golpe_c, values_from = n,
                  names_prefix = "golpe_c_q", values_fill = 0)
    
    ver(transicion)
    guardar_tabla(transicion, "T08_transicion_quintiles",
                  "Tabla 8. Transición de quintiles: Kaitz de obreros frente a golpe_c",
                  decimales = 0)
  }
  
} else {
  cat("AVISO: no se encontró", ruta_panel_viejo, "- se omite la comparación.\n")
  comparacion <- exposicion
}


# ==============================================================================
# 8. GRÁFICOS
# ==============================================================================
titulo("8. GRÁFICOS")

# Distribución de cada medida
para_graficar <- exposicion %>%
  select(golpe_c, golpe_a, golpe_costo) %>%
  pivot_longer(everything(), names_to = "medida", values_to = "valor") %>%
  filter(!is.na(valor))

grafico_distribuciones <- ggplot(para_graficar, aes(x = valor)) +
  geom_histogram(bins = 50, fill = COLOR_BAJA, color = "white") +
  facet_wrap(~ medida, scales = "free") +
  labs(title = "Distribución de las medidas de exposición (2022)",
       subtitle = "Un valor más alto indica una firma más expuesta al aumento del mínimo",
       x = "Valor de la medida", y = "Número de firmas",
       caption = "Valores recortados al 1% y 99%.") +
  tema_tesis
guardar_grafico(grafico_distribuciones, "G01_distribucion_medidas")

# Salario promedio por categoría: la evidencia de si la categoría ocupacional
# informa o no sobre cercanía al mínimo
salarios_categoria <- exposicion %>%
  select(w_obreros, w_profesional, w_administrativo) %>%
  pivot_longer(everything(), names_to = "categoria", values_to = "salario") %>%
  filter(!is.na(salario), salario > 0) %>%
  mutate(veces_el_minimo = salario / SMLV_2023_ANUAL_MILES)

grafico_categorias <- ggplot(salarios_categoria, aes(x = veces_el_minimo, fill = categoria)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 1, linetype = "dashed", color = COLOR_ALTA) +
  scale_x_continuous(limits = c(0, 8)) +
  labs(title = "Salario promedio por categoría ocupacional (2022)",
       subtitle = "En veces el salario mínimo anual. La línea roja marca el mínimo",
       x = "Veces el salario mínimo", y = "Densidad", fill = NULL,
       caption = "Si las distribuciones se traslapan, la categoría ocupacional no informa por sí sola sobre cercanía al mínimo.") +
  tema_tesis
guardar_grafico(grafico_categorias, "G02_salarios_por_categoria")

# Cuántos administrativos están cerca del mínimo: la prueba directa del
# argumento de que la categoría ocupacional no basta.
#
# NOTA DE UBICACIÓN: la mediana de "veces_el_minimo" que sale aquí para
# obreros (~1,44) es una cifra DESCRIPTIVA de la distribución salarial --
# material del capítulo 1 (quién está expuesto), no del capítulo 2. Se usa
# en este script como evidencia de apoyo al argumento metodológico de abajo,
# no como resultado propio de esta sección.
cerca_del_minimo <- salarios_categoria %>%
  group_by(categoria) %>%
  summarise(
    firmas = n(),
    pct_bajo_1_5_minimos = 100 * mean(veces_el_minimo < 1.5),
    pct_bajo_2_minimos = 100 * mean(veces_el_minimo < 2),
    mediana_veces_minimo = median(veces_el_minimo),
    .groups = "drop"
  )

ver(cerca_del_minimo)
guardar_tabla(cerca_del_minimo, "T09_cerca_del_minimo_por_categoria",
              "Tabla 9. Qué tan cerca del mínimo está cada categoría ocupacional",
              decimales = 2)

cat("\nLECTURA: si el porcentaje de firmas con administrativos por debajo de 1,5\n",
    "mínimos es apreciable, queda demostrado empíricamente que la categoría\n",
    "ocupacional no informa por sí sola sobre cercanía al piso salarial. Es la\n",
    "justificación económica de usar las tres categorías y no solo obreros.\n")

# golpe_c contra el Kaitz actual
if (exists("comparacion") && "Bite2022_obreros" %in% names(comparacion)) {
  grafico_scatter <- comparacion %>%
    filter(!is.na(golpe_c), !is.na(Bite2022_obreros)) %>%
    ggplot(aes(x = Bite2022_obreros, y = golpe_c)) +
    geom_point(alpha = 0.25, color = COLOR_BAJA) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = COLOR_ALTA) +
    labs(title = "golpe_c frente al Kaitz de obreros",
         subtitle = "Cada punto es una firma. La línea punteada es la igualdad",
         x = "Bite2022_obreros (solo obreros)", y = "golpe_c (tres categorías)",
         caption = "Los puntos alejados de la línea son firmas que cambian de nivel de exposición al usar las tres categorías.") +
    tema_tesis
  guardar_grafico(grafico_scatter, "G03_golpe_c_vs_kaitz")
}


# ==============================================================================
# 9. GUARDAR
# ==============================================================================
titulo("9. GUARDAR RESULTADOS")

write_rds(exposicion, file.path("1. DATOS", "exposicion_alternativa_2022.rds"))
cat("Guardado: 1. DATOS/exposicion_alternativa_2022.rds con", nrow(exposicion), "firmas\n")

save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
cat("Compendio guardado con", length(compendio), "tablas.\n")

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")


# ==============================================================================
# 10. QUÉ SIGUE
# ==============================================================================
titulo("10. QUÉ SIGUE")

cat("
Este script NO elige la medida principal. Esa decisión se toma con el primer
eslabón, según la regla comiteada en NOTA_DECISIONES.md: gana la medida que
prediga el aumento diferencial del costo laboral por trabajador en 2023, y no
se revisa según los resultados de empleo.

Antes de estimar, revisar:

  1. El control de unidades de la sección 3 (veces el mínimo plausible).
  2. El chequeo de proxy de tamaño de la sección 4. Si alguna medida supera
     0,5 de correlación con el inverso del tamaño, no usarla como tratamiento.
  3. La correlación entre golpe_c y golpe_a de la sección 7. Si es muy alta,
     quedarse con golpe_c, que es más fácil de explicar.

Limitaciones que deben quedar declaradas en la tesis:

  - Sin salario integral (C3R1 no existe en 2022). Los trabajadores con salario
    integral quedan en el denominador sin su remuneración en el numerador, así
    que los salarios promedio están subestimados en las firmas de personal caro,
    que aparecen como más expuestas de lo que son.
  - El 20,5% de las firmas-año no tiene obreros permanentes. golpe_c y golpe_a
    las recuperan si tienen alguna otra categoría; el Kaitz de obreros no. Esa
    exclusión es selección sobre el mecanismo mismo que la tesis estudia.
  - brecha no es candidata a tratamiento: es composición ocupacional, que es un
    resultado, no una causa.
")

titulo("FIN DEL SCRIPT")
