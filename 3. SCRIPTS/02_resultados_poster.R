# ==============================================================================
# 02_resultados_poster.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
# Fecha:   2026-09-16
#
# Este script genera los resultados principales de la tesis.
# Todo se muestra en pantalla mientras corre, las tablas se guardan en Word
# y los gráficos en PNG. Las validaciones adicionales van en otro script.
#
# Para correrlo abrimos TESIS_MECA.Rproj, así R trabaja desde la raíz del
# repositorio y encuentra las carpetas.
#
# Resultados (tablas en Word y CSV; gráficos en la subcarpeta "figuras"):
#   4. RESULTADOS/Descriptivos/   salario mínimo, Kaitz, perfil de firmas, comparación simple, brechas
#   4. RESULTADOS/Principal/      primer eslabón y empleo total (+ compendio con todas las tablas)
#   4. RESULTADOS/Estimacion/     resultados exploratorios, compresión salarial, tamaño
#   4. RESULTADOS/Validaciones/   revisión de carga de datos y supuestos del modelo
#   4. RESULTADOS/Robustez/       reservada para el script de validaciones
#
# Las bases de 1. DATOS/ se amplían antes con
# 0. ANALISIS INICIAL IA/3. SCRIPTS/construccion/ampliar_variables_paneles.R
# (agrega costos laborales por categoría, tipos de contrato, inversión,
# producción y otras variables). Si no se ha corrido, la sección 1 lo avisa.
#
# El script anterior (99_script_base_historico.R) queda en el repositorio
# como registro histórico de la revisión que hicimos -- no correrlo, su
# carpeta de salida colisiona con la de este.
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS -- este script alimenta TRES capítulos, marcado sección
# por sección.
#
# Capítulo 1 (Análisis inicial y descriptivos): secciones 1-4 (carga de
#   datos, Kaitz 2022, estadísticas descriptivas, comparación simple
#   antes/después). La sección 5 (controles de pandemia/subsidios) es de
#   transición: metodológicamente sostiene la identificación (capítulo 3),
#   pero se deja aquí porque es donde se calcula.
#
# Capítulo 5 (Resultados): secciones 6-9 (primer eslabón/salario promedio,
#   resultado principal de empleo, resultados exploratorios, heterogeneidad
#   por tamaño).
#
# Capítulo 4 (Validación de la identificación): sección 10 (supuestos del
#   modelo) -- es un mapa de qué evidencia de ESTE script respalda cada
#   supuesto y qué queda pendiente en el script de validaciones. No es en sí
#   misma una prueba nueva.
#
# Sección 11 (resumen): transversal a los tres capítulos, solo reordena
# cifras ya calculadas arriba.
#
# YA VERIFICADO EN ESTE SCRIPT (no requirió corrección):
#   - No se encontró la frase "el empleo no cae" en ninguna parte. El texto
#     ya usa consistentemente "no se detecta un efecto distinto de cero" /
#     "no alcanzamos a distinguirlo de cero", con el intervalo de confianza
#     completo al lado (secciones 7 y 11).
#   - Se buscó un "placebo de 2018 sobre empleo (p=0,22)" como prueba
#     puntual y NO se encontró tal cual en este script. Lo que SÍ existe es
#     una prueba CONJUNTA de todos los años previos (2015-2019, 2021) contra
#     2022, vía estudio_evento()$p_antes (sección 7, líneas ~774-784) --
#     esa es la evidencia de "sin anticipación"/tendencias paralelas que
#     corresponde al capítulo 4. Si el placebo específico de 2018 con p=0,22
#     existe en otro script no revisado en esta tarea, no se pudo verificar
#     aquí -- no se inventó la cifra.
#
# PENDIENTE CONECTADO A 07_reconciliacion.R: la "Lectura B" del
# primer eslabón (sección 11, línea ~1035: "salto 2023 frente al cambio
# típico 2016-2019") se calcula con contraste() (línea ~665), que resta el
# promedio de los coeficientes 2016-2019 al de 2023 -- es decir, SÍ parece
# ser "el salto ajustado por la pendiente previa" tal como se define en el
# glosario de 07_reconciliacion.R, no "la pendiente previa" en sí misma. Esto es
# evidencia a favor de que la nota pendiente de 07_reconciliacion.R (punto 6) se
# origina en una descripción imprecisa del póster, no en dos cálculos
# distintos -- pero no se confirmó corriendo el número, así que la nota de
# 07_reconciliacion.R se deja como está, sin marcarla resuelta.
# ------------------------------------------------------------------------------


# ==============================================================================
# LA IDEA DEL ESTUDIO
# ==============================================================================
#
# 1. En 2023 el salario mínimo subió 16%. El aumento fue igual para todas las
#    firmas del país.
#
# 2. Pero no afectó a todas por igual. Si los obreros de una firma ganan cerca
#    del mínimo, el aumento le pesa más que a una firma que les paga bastante
#    más. Eso lo medimos con el índice de Kaitz:
#
#        Kaitz = salario mínimo 2023 / salario promedio del obrero en 2022
#
#    Un Kaitz alto quiere decir que la firma paga cerca del mínimo, o sea que
#    está más expuesta. Lo calculamos con datos de 2022 para que el aumento de
#    2023 no afecte la medida.
#
# 3. Comparamos qué pasó antes y después de 2023 entre firmas más y menos
#    expuestas (diferencias en diferencias). Si las más expuestas cambian más,
#    esa diferencia la atribuimos al aumento del mínimo.
#
# 4. Entre 2015 y 2024 pasaron muchas cosas: pandemia, inflación, subsidios.
#    Para no confundirlas con el efecto del mínimo usamos efectos fijos, que
#    descuentan lo que afecta igual a:
#      - cada firma en todos los años
#      - todas las firmas en un mismo año (pandemia, inflación)
#      - las firmas de un mismo sector en un mismo año (subsidios, choques del sector)
#      - las firmas del mismo tamaño en un mismo año
#      - las firmas del mismo departamento en un mismo año
#    Sector, tamaño y departamento los tomamos de 2022.
#
# 5. Antes de mirar el empleo revisamos que las firmas con Kaitz alto sí
#    hayan tenido un aumento mayor de salarios en 2023. Si no fuera así, Kaitz
#    no estaría midiendo la exposición.
#
# Resultado principal: empleo total.
# Resultados exploratorios: empleo permanente, empleo temporal, participación
# de permanentes y ventas.
# ==============================================================================


# ==============================================================================
# 0. PREPARACIÓN
# ==============================================================================

# Limpiamos el entorno para empezar desde cero
rm(list = ls())

# Cargamos los paquetes que vamos a usar
library(dplyr)      # para manipular los datos
library(tidyr)      # para reorganizar tablas
library(readr)      # para leer y guardar archivos
library(fixest)     # para las regresiones con efectos fijos
library(ggplot2)    # para los gráficos
library(flextable)  # para las tablas en Word (si falta: renv::install("flextable"))

# Definimos las carpetas de salida. Cada una tiene una subcarpeta "figuras"
# para los gráficos. Las creamos si no existen.
CARPETA_DESCRIPTIVOS <- file.path("4. RESULTADOS", "Descriptivos")
CARPETA_PRINCIPAL    <- file.path("4. RESULTADOS", "Principal")
CARPETA_ESTIMACION   <- file.path("4. RESULTADOS", "Estimacion")
CARPETA_VALIDACIONES <- file.path("4. RESULTADOS", "Validaciones")
CARPETA_ROBUSTEZ     <- file.path("4. RESULTADOS", "Robustez")

for (carpeta in c(CARPETA_DESCRIPTIVOS, CARPETA_PRINCIPAL, CARPETA_ESTIMACION,
                  CARPETA_VALIDACIONES, CARPETA_ROBUSTEZ)) {
  dir.create(file.path(carpeta, "figuras"), recursive = TRUE, showWarnings = FALSE)
}

# --- Funciones de apoyo --------------------------------------------------------
# Las definimos una vez para no repetir el mismo código muchas veces.

# titulo(): escribe un encabezado en la consola para ubicar cada sección
titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

# ver(): muestra en la consola una tabla recién creada: cuántas filas y
# columnas tiene y sus primeras filas. Toma el nombre del objeto
# automáticamente. Con "filas" elegimos cuántas filas mostrar (por defecto 20).
ver <- function(tabla, filas = 20) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

# guardar_tabla(): guarda la tabla en Word y en CSV, y la agrega a la lista
# "compendio" para juntarlas todas al final. La tabla ya la mostramos en
# pantalla con ver() cuando la creamos.
# El "<<-" hace que la tabla se guarde en la lista que está fuera de la función.
compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3, carpeta = CARPETA_PRINCIPAL) {
  
  tabla_word <- flextable(tabla)
  tabla_word <- colformat_double(tabla_word, digits = decimales)
  tabla_word <- set_caption(tabla_word, caption = titulo_tabla)
  tabla_word <- autofit(tabla_word)
  
  save_as_docx(tabla_word, path = file.path(carpeta, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(carpeta, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tabla_word
  cat("Tabla guardada en", carpeta, ":", nombre_archivo, "\n")
}

# guardar_grafico(): muestra el gráfico en el panel Plots, lo guarda en PNG y
# lo agrega a la lista "graficos" para poder volver a verlo.
# No hacemos pausas: al correr el script por partes, la pausa "se comía" la
# siguiente línea de código. Para revisar los gráficos uno por uno usamos
# revisar_graficos() al final (sección 11).
graficos <- list()
guardar_grafico <- function(grafico, nombre_archivo, carpeta = CARPETA_PRINCIPAL, ancho = 9, alto = 5.5) {
  print(grafico)
  
  ggsave(file.path(carpeta, "figuras", paste0(nombre_archivo, ".png")),
         grafico, width = ancho, height = alto, dpi = 200, bg = "white")
  graficos[[nombre_archivo]] <<- grafico
  cat("Gráfico guardado en", file.path(carpeta, "figuras"), ":", nombre_archivo, "\n")
}

# estrellas(): convierte el p-valor en estrellas de significancia
estrellas <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
}

# Estilo común para todos los gráficos
tema_tesis <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey35"),
        plot.caption = element_text(color = "grey45", hjust = 0),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

# Colores: rojo para alta exposición, azul para baja
COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"


# ==============================================================================
# 1. DATOS
# ==============================================================================
titulo("1. CARGA DE DATOS")

# --- 1.1 Cargamos las dos bases ------------------------------------------------
# Panel de firmas: una fila por firma y año (2020 no está)
panel <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

# Panel de plantas: solo lo usamos para comprobar el salario promedio (1.4)
plantas <- read_rds(file.path("1. DATOS", "panel_establecimiento_formal.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

cat("Panel de firmas:", nrow(panel), "filas,", ncol(panel), "columnas,",
    n_distinct(panel$NORDEMP), "firmas\n")
cat("Panel de plantas:", nrow(plantas), "filas,", ncol(plantas), "columnas\n")
cat("Años:", paste(sort(unique(panel$ANIO)), collapse = ", "), "\n")

# --- 1.2 Revisamos que estén las columnas que usa este script --------------------
# Las primeras vienen del panel original; las demás las agrega
# ampliar_variables_paneles.R. Si falta alguna, el script se detiene.
columnas_originales <- c("NORDEMP", "ANIO", "CIIU4", "DPTO", "tamano_empresa",
                         "empleo_total", "empleo_permanente", "empleo_temporal",
                         "participacion_permanente", "Bite2022_obreros", "VALORVEN")
columnas_ampliadas <- c("costos_totales_personal_total_c3r10c3",
                        "sueldos_permanentes_obreros_c3r2c1", "sueldos_permanentes_administrativos_c3r2c2", "sueldos_permanentes_profesionales_c3r2pt",
                        "obreros_permanentes", "administrativos_permanentes", "profesionales_permanentes",
                        "temporal_directo", "temporal_agencias", "aprendices",
                        "n_establecimientos")

faltan_originales <- setdiff(columnas_originales, names(panel))
faltan_ampliadas  <- setdiff(columnas_ampliadas, names(panel))

if (length(faltan_originales) > 0) {
  stop("Faltan columnas del panel original: ", paste(faltan_originales, collapse = ", "))
}
if (length(faltan_ampliadas) > 0) {
  stop("Faltan columnas ampliadas: ", paste(faltan_ampliadas, collapse = ", "),
       "\nCorre primero 0. ANALISIS INICIAL IA/3. SCRIPTS/construccion/ampliar_variables_paneles.R")
}
cat("Todas las columnas que usa el script están en el panel.\n")

# Cada firma debe aparecer una sola vez por año
repetidas <- panel %>% count(NORDEMP, ANIO) %>% filter(n > 1) %>% nrow()
if (repetidas > 0) stop("Hay ", repetidas, " firmas-año repetidas en el panel.")
cat("No hay firmas-año repetidas.\n")

# --- 1.3 Qué tanto dato tienen las columnas que usamos (año 2022) -----------------
cobertura_carga <- tibble(columna = c(columnas_originales, columnas_ampliadas)) %>%
  mutate(
    porcentaje_vacios_2022 = sapply(columna, function(col) round(100 * mean(is.na(panel[[col]][panel$ANIO == 2022])), 1)),
    porcentaje_ceros_2022  = sapply(columna, function(col) {
      x <- panel[[col]][panel$ANIO == 2022]
      if (is.numeric(x)) round(100 * mean(x == 0, na.rm = TRUE), 1) else NA
    })
  )
ver(cobertura_carga, filas = 30)
guardar_tabla(cobertura_carga, "T00_revision_carga_datos", carpeta = CARPETA_VALIDACIONES,
              "Tabla 0. Columnas usadas: porcentaje de vacíos y ceros en 2022", decimales = 1)

# Identidad del empleo: el empleo total debería ser la suma de permanentes,
# temporales directos, temporales de agencias y aprendices
identidad_empleo <- panel %>%
  mutate(suma_partes = empleo_permanente + temporal_directo + temporal_agencias + aprendices,
         cumple = abs(empleo_total - suma_partes) < 0.5)
cat("\nFirmas-año donde empleo total = permanentes + temporales directos + agencias + aprendices:",
    sum(identidad_empleo$cumple, na.rm = TRUE), "de", nrow(identidad_empleo), "\n")

# Cuántas firmas tienen más de una planta
cat("Firmas-año con más de una planta:", sum(panel$n_establecimientos > 1, na.rm = TRUE), "\n")

# --- 1.4 Salario promedio ------------------------------------------------------------
# Salario promedio de cada firma en cada año: costo total del personal
# (sueldos, prestaciones y aportes) dividido por el número de trabajadores.
panel <- panel %>%
  mutate(salario_promedio = ifelse(empleo_total > 0, costos_totales_personal_total_c3r10c3 / empleo_total, NA))

# Comprobación: lo calculamos también sumando las plantas del panel de plantas
# (como lo hacíamos antes) y comparamos. Deberían dar casi lo mismo.
salario_desde_plantas <- plantas %>%
  group_by(NORDEMP, ANIO) %>%
  summarise(costo_laboral = sum(costo_laboral_total),
            trabajadores  = sum(empleo_total),
            .groups = "drop") %>%
  mutate(salario_plantas = ifelse(trabajadores > 0, costo_laboral / trabajadores, NA))

comparacion_salario <- panel %>%
  select(NORDEMP, ANIO, salario_promedio) %>%
  inner_join(salario_desde_plantas, by = c("NORDEMP", "ANIO")) %>%
  mutate(diferencia_pct = 100 * (salario_promedio / salario_plantas - 1))

cat("\nComparación del salario promedio (panel de firmas frente a suma de plantas):\n")
cat("  Firmas-año comparadas:", sum(!is.na(comparacion_salario$diferencia_pct)), "\n")
cat("  Correlación:", round(cor(comparacion_salario$salario_promedio, comparacion_salario$salario_plantas,
                                use = "complete.obs"), 4), "\n")
cat("  Firmas-año con diferencia mayor a 1%:",
    sum(abs(comparacion_salario$diferencia_pct) > 1, na.rm = TRUE), "\n")

muestra_salario <- select(panel, NORDEMP, ANIO, empleo_total, costos_totales_personal_total_c3r10c3, salario_promedio)
ver(muestra_salario, filas = 10)


# ==============================================================================
# 2. EXPOSICIÓN: ÍNDICE DE KAITZ 2022
# ==============================================================================
titulo("2. EXPOSICIÓN: ÍNDICE DE KAITZ 2022")

# Tomamos las firmas de 2022 que tienen Kaitz, junto con su sector,
# departamento y tamaño de ese año
firmas_2022 <- panel %>%
  filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
  select(NORDEMP, Bite2022_obreros, CIIU4, DPTO, tamano_empresa)

ver(firmas_2022, filas = 10)

# Algunas firmas reportan muy pocos obreros y su Kaitz queda en valores
# extremos. Recortamos al 1% y al 99% para que no distorsionen los resultados.
limites <- quantile(firmas_2022$Bite2022_obreros, probs = c(0.01, 0.99))
cat("Límites para recortar Kaitz (1% y 99%):\n")
print(limites)

# Creamos las variables de exposición:
#   kaitz       Kaitz recortado
#   kaitz_de    Kaitz en desviaciones estándar (para interpretar los coeficientes)
#   exposicion  alta o baja según si está por encima o por debajo de la mediana
#   sector, departamento y tamaño de 2022, que usamos como controles
firmas_2022 <- firmas_2022 %>%
  mutate(
    kaitz       = pmin(pmax(Bite2022_obreros, limites[1]), limites[2]),
    kaitz_de    = kaitz / sd(kaitz),
    exposicion  = ifelse(kaitz > median(kaitz), "Alta exposición", "Baja exposición"),
    exposicion  = factor(exposicion, levels = c("Baja exposición", "Alta exposición")),
    sector_2022 = factor(CIIU4),
    depto_2022  = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  ) %>%
  select(NORDEMP, kaitz, kaitz_de, exposicion, sector_2022, depto_2022, tamano_2022)

ver(firmas_2022, filas = 10)
cat("Firmas con Kaitz 2022:", nrow(firmas_2022), "\n")
cat("Mediana de Kaitz:", round(median(firmas_2022$kaitz), 3), "\n")
cat("Desviación estándar de Kaitz:", round(sd(firmas_2022$kaitz), 3), "\n")

# Armamos la base de análisis: dejamos solo las firmas que tienen Kaitz y
# creamos las variables que usamos en las regresiones
#   ANIO_F       el año como categoría (para los efectos fijos)
#   post         1 en 2023 y 2024, 0 antes
#   periodo      lo mismo, en texto, para las tablas
#   log_...      logaritmos, para leer los efectos como porcentajes
# Primero cambiamos a vacío (NA) los valores de cero o negativos y después
# sacamos el logaritmo. Así R no intenta calcular logaritmos de negativos.
datos <- panel %>%
  inner_join(firmas_2022, by = "NORDEMP") %>%
  mutate(
    ANIO_F      = factor(ANIO),
    post        = as.integer(ANIO >= 2023),
    periodo     = ifelse(post == 1, "Después (2023-2024)", "Antes (2015-2022)"),
    log_empleo  = log(ifelse(empleo_total > 0, empleo_total, NA)),
    log_salario = log(ifelse(salario_promedio > 0, salario_promedio, NA)),
    log_ventas  = log(ifelse(VALORVEN > 0, VALORVEN, NA)),
    # Salario promedio de los permanentes de cada categoría (sueldos / personas)
    salario_obrero         = ifelse(obreros_permanentes > 0, sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA),
    salario_administrativo = ifelse(administrativos_permanentes > 0, sueldos_permanentes_administrativos_c3r2c2 / administrativos_permanentes, NA),
    salario_profesional    = ifelse(profesionales_permanentes > 0, sueldos_permanentes_profesionales_c3r2pt / profesionales_permanentes, NA),
    log_salario_obrero         = log(ifelse(salario_obrero > 0, salario_obrero, NA)),
    log_salario_administrativo = log(ifelse(salario_administrativo > 0, salario_administrativo, NA)),
    log_salario_profesional    = log(ifelse(salario_profesional > 0, salario_profesional, NA))
  )

ver(datos, filas = 5)

# Revisamos cuántos datos quedan vacíos por tener valores de cero o negativos
cat("Filas con empleo total en cero:", sum(datos$empleo_total <= 0, na.rm = TRUE), "\n")
cat("Filas con salario promedio en cero o negativo:", sum(datos$salario_promedio <= 0, na.rm = TRUE), "\n")
cat("Filas con ventas en cero:", sum(datos$VALORVEN == 0, na.rm = TRUE), "\n")
cat("Filas con ventas negativas:", sum(datos$VALORVEN < 0, na.rm = TRUE), "\n")

# Guardamos en una variable los efectos fijos que usamos en todas las
# regresiones: firma, año, y sector, tamaño y departamento por año
EFECTOS_FIJOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
cat("Efectos fijos:", EFECTOS_FIJOS, "\n")


# ==============================================================================
# 3. ESTADÍSTICAS DESCRIPTIVAS
# ==============================================================================
titulo("3. ESTADÍSTICAS DESCRIPTIVAS")

# --- 3.1 El salario mínimo ------------------------------------------------------
# Escribimos a mano el salario mínimo mensual de cada año (según los decretos)
# y calculamos el aumento porcentual frente al año anterior.
# Pendiente para validaciones: agregar la inflación del DANE para ver el
# aumento real.
salario_minimo <- tibble(
  anio  = 2015:2024,
  valor = c(644350, 689455, 737717, 781242, 828116, 877803, 908526, 1000000, 1160000, 1300000)
) %>%
  mutate(aumento_porcentual = round(100 * (valor / lag(valor) - 1), 2))

ver(salario_minimo)
guardar_tabla(salario_minimo, "T01_salario_minimo", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 1. Salario mínimo mensual y aumento anual (%)", decimales = 2)

# Gráfico de barras del aumento de cada año; 2023 va en rojo
grafico_sm <- ggplot(filter(salario_minimo, !is.na(aumento_porcentual)),
                     aes(x = factor(anio), y = aumento_porcentual,
                         fill = anio == 2023)) +
  geom_col() +
  geom_text(aes(label = paste0(aumento_porcentual, "%")), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = "grey65"), guide = "none") +
  labs(title = "Aumento anual del salario mínimo en Colombia",
       subtitle = "El aumento de 2023 es el choque que estudia la tesis",
       x = NULL, y = "Aumento nominal (%)",
       caption = "Fuente: decretos de salario mínimo. Valores nominales.") +
  tema_tesis
guardar_grafico(grafico_sm, "G01_aumento_salario_minimo", carpeta = CARPETA_DESCRIPTIVOS)

# --- 3.2 Firmas por año y grupo de exposición --------------------------------------
# Contamos cuántas firmas hay cada año y cuántas están en cada grupo
firmas_por_anio <- datos %>%
  group_by(ANIO) %>%
  summarise(firmas = n(),
            alta_exposicion = sum(exposicion == "Alta exposición"),
            baja_exposicion = sum(exposicion == "Baja exposición"))

ver(firmas_por_anio)
guardar_tabla(firmas_por_anio, "T02_firmas_por_anio", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 2. Número de firmas por año y grupo de exposición", decimales = 0)

# --- 3.3 Cómo son las firmas de cada grupo en 2022 ----------------------------------
# Comparamos los dos grupos en 2022: tamaño, salario y participación de
# permanentes. Esperamos que las firmas de alta exposición paguen menos.
perfil_2022 <- datos %>%
  filter(ANIO == 2022) %>%
  group_by(exposicion) %>%
  summarise(firmas = n(),
            kaitz_promedio = mean(kaitz),
            empleo_promedio = mean(empleo_total, na.rm = TRUE),
            empleo_mediana = median(empleo_total, na.rm = TRUE),
            salario_promedio_anual_miles = mean(salario_promedio, na.rm = TRUE),
            participacion_permanentes = mean(participacion_permanente, na.rm = TRUE))

ver(perfil_2022)
guardar_tabla(perfil_2022, "T03_perfil_firmas_2022", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 3. Características de las firmas por grupo de exposición (2022)", decimales = 2)

# Histograma de Kaitz con una línea en la mediana, que separa los dos grupos
grafico_kaitz <- ggplot(firmas_2022, aes(x = kaitz)) +
  geom_histogram(bins = 40, fill = COLOR_BAJA, color = "white") +
  geom_vline(xintercept = median(firmas_2022$kaitz), linetype = "dashed", color = COLOR_ALTA) +
  annotate("text", x = median(firmas_2022$kaitz), y = Inf, vjust = 1.5, hjust = -0.05,
           label = "Mediana: separa alta y baja exposición", color = COLOR_ALTA, size = 3.5) +
  labs(title = "Distribución del índice de Kaitz (2022)",
       subtitle = "Kaitz = salario mínimo 2023 / salario promedio del obrero en 2022",
       x = "Índice de Kaitz", y = "Número de firmas",
       caption = "Valores recortados al 1% y 99%.") +
  tema_tesis
guardar_grafico(grafico_kaitz, "G02_distribucion_kaitz", carpeta = CARPETA_DESCRIPTIVOS)

# --- Salario promedio frente al salario mínimo ------------------------------------
# Pasamos el salario promedio (miles de pesos al año) a pesos mensuales y lo
# dividimos por el salario mínimo del año. Si esa razón baja, el mínimo se está
# acercando al costo típico de un trabajador.
veces_minimo <- datos %>%
  left_join(salario_minimo, by = c("ANIO" = "anio")) %>%
  mutate(salario_mensual_pesos = salario_promedio * 1000 / 12,
         veces_el_minimo       = salario_mensual_pesos / valor) %>%
  group_by(ANIO, exposicion) %>%
  summarise(mediana_veces_minimo = median(veces_el_minimo, na.rm = TRUE),
            debajo_del_minimo    = sum(veces_el_minimo < 1, na.rm = TRUE),
            mas_de_10_minimos    = sum(veces_el_minimo > 10, na.rm = TRUE),
            .groups = "drop")

ver(veces_minimo)
guardar_tabla(veces_minimo, "T03b_salario_en_veces_el_minimo", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 3b. Costo laboral por trabajador en veces el salario mínimo", decimales = 2)

grafico_veces_minimo <- ggplot(veces_minimo, aes(x = ANIO, y = mediana_veces_minimo, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  labs(title = "Costo laboral por trabajador frente al salario mínimo",
       subtitle = "Mediana de cuántas veces el salario mínimo cuesta un trabajador. Si baja, el mínimo se acerca al costo típico",
       x = NULL, y = "Veces el salario mínimo", color = NULL,
       caption = "Costo total del personal (sueldos, prestaciones y aportes) dividido por el número de trabajadores.") +
  tema_tesis
guardar_grafico(grafico_veces_minimo, "G02b_salario_en_veces_el_minimo", carpeta = CARPETA_DESCRIPTIVOS)

# --- 3.4 Tamaño y sector por grupo ---------------------------------------------------
# Contamos las firmas de cada tamaño en cada grupo de exposición
tamano_por_grupo <- firmas_2022 %>%
  count(tamano_2022, exposicion) %>%
  pivot_wider(names_from = exposicion, values_from = n, values_fill = 0)

ver(tamano_por_grupo)
guardar_tabla(tamano_por_grupo, "T04_tamano_por_grupo", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 4. Firmas por tamaño y grupo de exposición (2022)", decimales = 0)

# Los 10 sectores con más firmas y qué porcentaje de ellas está en alta exposición
sectores <- firmas_2022 %>%
  group_by(sector_2022) %>%
  summarise(firmas = n(),
            porcentaje_alta_exposicion = 100 * mean(exposicion == "Alta exposición")) %>%
  arrange(desc(firmas)) %>%
  head(10)

ver(sectores)
guardar_tabla(sectores, "T05_sectores_principales", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 5. Diez sectores (CIIU4) con más firmas y % en alta exposición", decimales = 1)


# ==============================================================================
# 4. COMPARACIÓN SIMPLE: ANTES Y DESPUÉS, ALTA Y BAJA EXPOSICIÓN
# ==============================================================================
titulo("4. COMPARACIÓN SIMPLE: ANTES Y DESPUÉS, ALTA Y BAJA EXPOSICIÓN")

# Antes de las regresiones miramos los promedios. Si el aumento del mínimo
# pesó más en las firmas expuestas, su salario debería subir más después de 2023.

# --- 4.1 Tabla antes y después --------------------------------------------------------
# Calculamos el salario y el empleo promedio de cada grupo antes y después
# de 2023, y el cambio porcentual entre los dos periodos
tabla_2x2 <- datos %>%
  group_by(exposicion, periodo) %>%
  summarise(salario_promedio = mean(salario_promedio, na.rm = TRUE),
            empleo_promedio  = mean(empleo_total, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = c(salario_promedio, empleo_promedio)) %>%
  mutate(
    cambio_salario_pct = 100 * (`salario_promedio_Después (2023-2024)` / `salario_promedio_Antes (2015-2022)` - 1),
    cambio_empleo_pct  = 100 * (`empleo_promedio_Después (2023-2024)` / `empleo_promedio_Antes (2015-2022)` - 1)
  )

ver(tabla_2x2)
guardar_tabla(tabla_2x2, "T06_antes_despues_por_grupo", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 6. Promedios antes y después de 2023 por grupo de exposición", decimales = 1)

# Restamos el cambio de las firmas de baja exposición al de alta exposición.
# Esa resta es la idea de "diferencias en diferencias", pero sin controles.
diferencia_simple <- tibble(
  indicador = c("Cambio % del salario promedio", "Cambio % del empleo promedio"),
  alta_exposicion = c(tabla_2x2$cambio_salario_pct[tabla_2x2$exposicion == "Alta exposición"],
                      tabla_2x2$cambio_empleo_pct[tabla_2x2$exposicion == "Alta exposición"]),
  baja_exposicion = c(tabla_2x2$cambio_salario_pct[tabla_2x2$exposicion == "Baja exposición"],
                      tabla_2x2$cambio_empleo_pct[tabla_2x2$exposicion == "Baja exposición"])
) %>%
  mutate(diferencia_alta_menos_baja = alta_exposicion - baja_exposicion)

ver(diferencia_simple)
guardar_tabla(diferencia_simple, "T07_diferencia_simple", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 7. Diferencia simple: cambio en firmas de alta menos baja exposición (puntos porcentuales)",
              decimales = 2)
cat("\nNOTA: esta tabla es solo descriptiva (promedios sin controles).",
    "\nLos resultados formales están en las secciones 6 y 7.\n")

# --- 4.2 Evolución en el tiempo por grupo --------------------------------------------
# Para cada grupo y año calculamos el promedio del logaritmo del salario y del
# empleo. Luego le restamos el valor de 2022 del mismo grupo y multiplicamos
# por 100. Así cada punto se lee como "% por encima o por debajo de 2022".
evolucion <- datos %>%
  group_by(exposicion, ANIO) %>%
  summarise(log_salario = mean(log_salario, na.rm = TRUE),
            log_empleo  = mean(log_empleo, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(exposicion) %>%
  mutate(salario_vs_2022 = 100 * (log_salario - log_salario[ANIO == 2022]),
         empleo_vs_2022  = 100 * (log_empleo  - log_empleo[ANIO == 2022])) %>%
  ungroup()

ver(evolucion)

# Gráfico de líneas del salario por grupo
grafico_evol_salario <- ggplot(evolucion, aes(x = ANIO, y = salario_vs_2022, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  labs(title = "Salario promedio por grupo de exposición",
       subtitle = "Diferencia porcentual frente a 2022. La línea punteada marca el aumento de 2023",
       x = NULL, y = "% frente a 2022", color = NULL,
       caption = "Promedios simples, sin controles. 2020 no está en la base.") +
  tema_tesis
guardar_grafico(grafico_evol_salario, "G03_evolucion_salario_por_grupo", carpeta = CARPETA_DESCRIPTIVOS)

# Gráfico de líneas del empleo por grupo
grafico_evol_empleo <- ggplot(evolucion, aes(x = ANIO, y = empleo_vs_2022, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
  labs(title = "Empleo por grupo de exposición",
       subtitle = "Diferencia porcentual frente a 2022. La línea punteada marca el aumento de 2023",
       x = NULL, y = "% frente a 2022", color = NULL,
       caption = "Promedios simples, sin controles. 2020 no está en la base.") +
  tema_tesis
guardar_grafico(grafico_evol_empleo, "G04_evolucion_empleo_por_grupo", carpeta = CARPETA_DESCRIPTIVOS)


# ==============================================================================
# 5. CONTROLES: PANDEMIA Y SUBSIDIOS
# ==============================================================================
titulo("5. CONTROLES: QUÉ ABSORBEN LOS EFECTOS FIJOS")

# La pandemia y los subsidios (PAEF en 2020-2021 e incentivo al empleo entre
# 2021 y agosto de 2023) afectaron a toda la economía, así que deben quedar
# controlados. Aquí comprobamos que ya lo están.
#
# Creamos una dummy de pandemia (2021, porque 2020 no está en la base) y otra
# de subsidios (2021 a 2023) y las metemos en un modelo con efecto fijo de año.
# R las elimina porque el efecto fijo de año ya recoge todo lo que pasó a
# todas las firmas en cada año. El efecto de los subsidios sobre cada sector lo
# recoge el control de sector por año.

datos_demostracion <- datos %>%
  mutate(pandemia  = as.integer(ANIO == 2021),
         subsidios = as.integer(ANIO %in% 2021:2023))

# Vemos cómo quedan las dummies por año
ver(datos_demostracion %>% count(ANIO, pandemia, subsidios))

modelo_demostracion <- feols(log_empleo ~ post:kaitz_de + pandemia + subsidios | NORDEMP + ANIO_F,
                             data = datos_demostracion, cluster = ~NORDEMP)

# Mostramos qué variables quedaron en el modelo y cuáles eliminó R
cat("\nCoeficientes que quedan en el modelo:\n")
print(coeftable(modelo_demostracion))
cat("\nEliminadas por colinealidad:",
    paste(modelo_demostracion$collin.var, collapse = ", "), "\n")
cat("LECTURA: 'pandemia' y 'subsidios' no aparecen porque el efecto fijo de año",
    "\nya contiene esa información. Los controles de pandemia y subsidios están",
    "\nincluidos en todas las estimaciones a través de los efectos fijos.\n")


# ==============================================================================
# FUNCIONES DE ESTIMACIÓN
# ==============================================================================
# Definimos tres funciones que usamos en las secciones siguientes.

# estimar_did(): corre la regresión de diferencias en diferencias.
# El coeficiente de "post:kaitz_de" dice cuánto cambió el resultado después de
# 2023 en una firma con una desviación estándar más de Kaitz, frente a antes
# de 2023. La función devuelve el coeficiente, su error estándar, el p-valor,
# las estrellas, el intervalo de confianza al 95% y el número de datos.
estimar_did <- function(variable, base = datos, efectos = EFECTOS_FIJOS, etiqueta = variable) {
  formula_did <- as.formula(paste0(variable, " ~ post:kaitz_de | ", efectos))
  modelo <- feols(formula_did, data = base, cluster = ~NORDEMP)
  fila <- coeftable(modelo)["post:kaitz_de", ]
  tibble(
    resultado      = etiqueta,
    coeficiente    = fila[["Estimate"]],
    error_estandar = fila[["Std. Error"]],
    p_valor        = fila[["Pr(>|t|)"]],
    significancia  = estrellas(fila[["Pr(>|t|)"]]),
    ic95_inferior  = fila[["Estimate"]] - 1.96 * fila[["Std. Error"]],
    ic95_superior  = fila[["Estimate"]] + 1.96 * fila[["Std. Error"]],
    observaciones  = nobs(modelo),
    firmas         = n_distinct(base$NORDEMP[obs(modelo)])
  )
}

# estudio_evento(): hace lo mismo pero año por año, comparando cada año con 2022.
# Sirve para ver si los dos grupos venían parecidos antes de 2023.
# Además calcula una prueba conjunta: si el p-valor es alto, no hay evidencia
# de que los años anteriores fueran distintos de 2022.
estudio_evento <- function(variable) {
  formula_evento <- as.formula(paste0(variable, " ~ i(ANIO_F, kaitz_de, ref = '2022') | ", EFECTOS_FIJOS))
  modelo <- feols(formula_evento, data = datos, cluster = ~NORDEMP)
  coeficientes <- as.data.frame(coeftable(modelo))
  
  # Armamos una tabla con un coeficiente por año y agregamos 2022 con valor 0
  tabla <- tibble(
    anio           = as.integer(gsub("ANIO_F::|:kaitz_de", "", rownames(coeficientes))),
    coeficiente    = coeficientes[["Estimate"]],
    error_estandar = coeficientes[["Std. Error"]]
  ) %>%
    bind_rows(tibble(anio = 2022L, coeficiente = 0, error_estandar = 0)) %>%
    mutate(ic95_inferior = coeficiente - 1.96 * error_estandar,
           ic95_superior = coeficiente + 1.96 * error_estandar) %>%
    arrange(anio)
  
  # Prueba conjunta de los años anteriores a 2022
  p_antes <- wald(modelo, keep = "ANIO_F::(2015|2016|2017|2018|2019|2021):", print = FALSE)$p
  
  list(tabla = tabla, p_antes = p_antes, modelo = modelo)
}

# contraste(): combina coeficientes de un estudio de evento con pesos y
# calcula su error estándar. Ejemplo: 2023 menos el promedio de los cambios
# anuales 2016-2019. Devuelve una fila con estimado, error, p-valor e IC.
contraste <- function(modelo, pesos, etiqueta) {
  nombres <- paste0("ANIO_F::", names(pesos), ":kaitz_de")
  b <- coef(modelo)[nombres]
  V <- vcov(modelo)[nombres, nombres]
  estimado <- sum(pesos * b)
  error <- sqrt(as.numeric(t(pesos) %*% V %*% pesos))
  p <- 2 * pnorm(-abs(estimado / error))
  tibble(medida = etiqueta, coeficiente = estimado, error_estandar = error,
         p_valor = p, significancia = estrellas(p),
         ic95_inferior = estimado - 1.96 * error, ic95_superior = estimado + 1.96 * error)
}

# grafico_evento(): dibuja los coeficientes año por año con su intervalo de
# confianza. Los años después de 2023 van en rojo.
grafico_evento <- function(evento, titulo_grafico, eje_y, nota) {
  ggplot(evento$tabla, aes(x = anio, y = coeficiente)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ic95_inferior, ymax = ic95_superior), width = 0.2, color = COLOR_BAJA) +
    geom_point(size = 2.5, color = ifelse(evento$tabla$anio >= 2023, COLOR_ALTA, COLOR_BAJA)) +
    scale_x_continuous(breaks = c(2015:2019, 2021:2024)) +
    labs(title = titulo_grafico,
         subtitle = paste0("Efecto de una desviación estándar más de Kaitz, frente a 2022\n",
                           "Prueba conjunta de los años previos: p = ", round(evento$p_antes, 3)),
         x = NULL, y = eje_y, caption = nota) +
    tema_tesis
}

# Nota que ponemos debajo de los gráficos de regresión
NOTA_MODELO <- paste("Controles: firma, año, sector x año, tamaño x año y departamento x año (fijados en 2022).",
                     "\nIntervalos de confianza al 95%, errores agrupados por firma.")


# ==============================================================================
# 6. PRIMER ESLABÓN: SALARIO PROMEDIO
# ==============================================================================
titulo("6. PRIMER ESLABÓN: SALARIO PROMEDIO")

# Revisamos si a las firmas con Kaitz alto les subió más el salario promedio
# en 2023. El salario promedio es el costo total del personal por trabajador
# (sección 1.4). Como está en logaritmo, coeficiente x 100 = % adicional por
# cada desviación estándar de Kaitz.
#
# Lo leemos de tres formas, porque dan cosas distintas:
#   A. Cambio 2022 -> 2023: cuánto subió el salario de las firmas más expuestas
#      en el año del aumento, frente a 2022.
#   B. Salto de 2023 frente al cambio anual típico de 2016-2019: descuenta lo
#      que ya venía pasando antes.
#   C. Promedio después (2023-2024) menos promedio antes (2015-2022): la
#      regresión de diferencias en diferencias simple.
# Las firmas con Kaitz alto venían con salarios relativamente cada vez más
# bajos hasta 2022 (por eso tienen Kaitz alto en 2022). Eso hace que C salga
# negativo aunque en 2023 su salario sí sube más. Lo que interesa para el
# primer eslabón es el salto de 2023 (A y B). La tendencia previa queda como
# pendiente para validaciones.

# Estudio de evento del salario (coeficientes año por año frente a 2022)
evento_salario <- estudio_evento("log_salario")
ver(evento_salario$tabla)
cat("Prueba conjunta de años previos: p =", round(evento_salario$p_antes, 3), "\n")
guardar_tabla(evento_salario$tabla, "T09_evento_salario", carpeta = CARPETA_PRINCIPAL,
              "Tabla 9. Salario promedio año por año frente a 2022 (log)", decimales = 4)

# A y B salen del estudio de evento; C de la regresión simple
lectura_a <- contraste(evento_salario$modelo, c(`2023` = 1),
                       "A. Cambio 2022 -> 2023")
lectura_b <- contraste(evento_salario$modelo, c(`2023` = 1, `2016` = 1/3, `2019` = -1/3),
                       "B. Salto 2023 frente al cambio anual típico 2016-2019")
lectura_c <- estimar_did("log_salario", etiqueta = "Salario promedio (log)") %>%
  transmute(medida = "C. Promedio después menos promedio antes (DiD simple)",
            coeficiente, error_estandar, p_valor, significancia, ic95_inferior, ic95_superior)

primer_eslabon <- bind_rows(lectura_a, lectura_b, lectura_c) %>%
  mutate(efecto_porcentual = 100 * coeficiente)

ver(primer_eslabon)
guardar_tabla(primer_eslabon, "T08_primer_eslabon_salario", carpeta = CARPETA_PRINCIPAL,
              "Tabla 8. Primer eslabón: aumento del salario promedio en firmas más expuestas (tres lecturas)",
              decimales = 4)

guardar_grafico(grafico_evento(evento_salario,
                               "Primer eslabón: salario promedio de las firmas más expuestas",
                               "Diferencia en log del salario", NOTA_MODELO),
                "G05_evento_salario", carpeta = CARPETA_PRINCIPAL)


# ==============================================================================
# 7. RESULTADO PRINCIPAL: EMPLEO TOTAL
# ==============================================================================
titulo("7. RESULTADO PRINCIPAL: EMPLEO TOTAL")

# Estimamos el efecto sobre el empleo total de dos formas:
#   - en número de trabajadores
#   - en logaritmo, que se lee como cambio porcentual (coeficiente por 100)
# Si el coeficiente no es significativo, no quiere decir que el efecto sea
# cero: quiere decir que no alcanzamos a distinguirlo de cero. El intervalo de
# confianza muestra el rango de valores posibles.

empleo_principal <- bind_rows(
  estimar_did("empleo_total", etiqueta = "Empleo total (trabajadores)"),
  estimar_did("log_empleo",   etiqueta = "Empleo total (log)")
) %>%
  mutate(efecto_porcentual = ifelse(resultado == "Empleo total (log)", 100 * coeficiente, NA))

ver(empleo_principal)
guardar_tabla(empleo_principal, "T10_resultado_principal_empleo", carpeta = CARPETA_PRINCIPAL,
              "Tabla 10. Resultado principal: efecto del aumento de 2023 sobre el empleo total", decimales = 4)

# Lo mismo, año por año, en las dos escalas
evento_empleo     <- estudio_evento("empleo_total")
evento_log_empleo <- estudio_evento("log_empleo")

ver(evento_empleo$tabla)
cat("Prueba conjunta de años previos: p =", round(evento_empleo$p_antes, 3), "\n")
guardar_tabla(evento_empleo$tabla, "T11_evento_empleo", carpeta = CARPETA_PRINCIPAL,
              "Tabla 11. Empleo total año por año frente a 2022 (trabajadores)", decimales = 3)
ver(evento_log_empleo$tabla)
cat("Prueba conjunta de años previos: p =", round(evento_log_empleo$p_antes, 3), "\n")
guardar_tabla(evento_log_empleo$tabla, "T12_evento_log_empleo", carpeta = CARPETA_PRINCIPAL,
              "Tabla 12. Empleo total año por año frente a 2022 (log)", decimales = 4)

guardar_grafico(grafico_evento(evento_empleo,
                               "Empleo total de las firmas más expuestas (trabajadores)",
                               "Diferencia en número de trabajadores", NOTA_MODELO),
                "G06_evento_empleo", carpeta = CARPETA_PRINCIPAL)
guardar_grafico(grafico_evento(evento_log_empleo,
                               "Empleo total de las firmas más expuestas (log)",
                               "Diferencia en log del empleo", NOTA_MODELO),
                "G07_evento_log_empleo", carpeta = CARPETA_PRINCIPAL)


# ==============================================================================
# 8. RESULTADOS EXPLORATORIOS
# ==============================================================================
titulo("8. RESULTADOS EXPLORATORIOS")

# Las firmas también pueden ajustar por otros lados: el tipo de contrato, la
# composición del empleo o sus ventas. Corremos la misma regresión para cada
# uno. Estos resultados los reportamos como exploratorios, no como efectos
# causales.

exploratorios <- bind_rows(
  estimar_did("empleo_permanente",        etiqueta = "Empleo permanente (trabajadores)"),
  estimar_did("empleo_temporal",          etiqueta = "Empleo temporal (trabajadores)"),
  estimar_did("participacion_permanente", etiqueta = "Participación de permanentes (puntos %)"),
  estimar_did("log_ventas",               etiqueta = "Ventas (log)")
) %>%
  mutate(lectura = "Exploratorio")

ver(exploratorios)
guardar_tabla(exploratorios, "T13_resultados_exploratorios", carpeta = CARPETA_ESTIMACION,
              "Tabla 13. Resultados exploratorios (sin lectura causal fuerte)", decimales = 4)


# ==============================================================================
# 8b. COMPRESIÓN SALARIAL (EXPLORATORIO)
# ==============================================================================
titulo("8b. COMPRESIÓN SALARIAL (EXPLORATORIO)")

# Si el salario mínimo sube, los salarios de los obreros (cerca del mínimo)
# suben más que los de administrativos y profesionales, y la brecha se achica.
# Eso es compresión salarial.
#
# Usamos el salario promedio de los permanentes de cada categoría (sección 2)
# y medimos la brecha como diferencia en logaritmos:
#   brecha = log(salario administrativo) - log(salario obrero)
# Por 100 se lee aproximadamente como "% que gana de más un administrativo".
# Si la brecha baja, hay compresión.
#
# Cuidados (se revisan en validaciones):
#   - Solo cubre trabajadores permanentes.
#   - Si una firma despide a los obreros peor pagados, el salario promedio del
#     obrero sube sin que nadie gane más (cambio de composición).
#   - El salario del obrero es el mismo con el que calculamos Kaitz 2022.

datos_compresion <- datos %>%
  mutate(brecha_admin_obrero = log_salario_administrativo - log_salario_obrero,
         brecha_prof_obrero  = log_salario_profesional - log_salario_obrero)

# Algunas firmas tienen uno o dos trabajadores en una categoría y la brecha
# queda en valores extremos. Dejamos vacías las brechas fuera del 1% y 99%.
recortar <- function(x) {
  limites_brecha <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  ifelse(x < limites_brecha[1] | x > limites_brecha[2], NA, x)
}
datos_compresion <- datos_compresion %>%
  mutate(brecha_admin_obrero = recortar(brecha_admin_obrero),
         brecha_prof_obrero  = recortar(brecha_prof_obrero))

cat("Firmas-año con brecha administrativo-obrero:", sum(!is.na(datos_compresion$brecha_admin_obrero)), "\n")
cat("Firmas-año con brecha profesional-obrero:", sum(!is.na(datos_compresion$brecha_prof_obrero)), "\n")

# Descriptivo: brecha mediana cada año, por grupo de exposición
brechas_por_anio <- datos_compresion %>%
  group_by(ANIO, exposicion) %>%
  summarise(brecha_admin_obrero_pct = 100 * median(brecha_admin_obrero, na.rm = TRUE),
            brecha_prof_obrero_pct  = 100 * median(brecha_prof_obrero, na.rm = TRUE),
            firmas_con_dato_admin   = sum(!is.na(brecha_admin_obrero)),
            firmas_con_dato_prof    = sum(!is.na(brecha_prof_obrero)),
            .groups = "drop")

ver(brechas_por_anio)
guardar_tabla(brechas_por_anio, "T13b_brechas_salariales_por_anio", carpeta = CARPETA_DESCRIPTIVOS,
              "Tabla 13b. Brecha salarial mediana frente a los obreros (%, en log x 100), por año y grupo",
              decimales = 1)

# Pasamos la tabla a formato largo para graficar las dos brechas juntas
brechas_largo <- brechas_por_anio %>%
  select(ANIO, exposicion, brecha_admin_obrero_pct, brecha_prof_obrero_pct) %>%
  pivot_longer(c(brecha_admin_obrero_pct, brecha_prof_obrero_pct),
               names_to = "brecha", values_to = "valor") %>%
  mutate(brecha = ifelse(brecha == "brecha_admin_obrero_pct",
                         "Administrativos vs. obreros", "Profesionales vs. obreros"))

grafico_brechas <- ggplot(brechas_largo, aes(x = ANIO, y = valor, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~ brecha, scales = "free_y") +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Brecha salarial frente a los obreros",
       subtitle = "Mediana de la diferencia en log x 100. Si la línea baja después de 2023, hay compresión salarial",
       x = NULL, y = "Brecha (%)", color = NULL,
       caption = "Trabajadores permanentes. Sin controles.") +
  tema_tesis
guardar_grafico(grafico_brechas, "G08b_brechas_salariales", carpeta = CARPETA_DESCRIPTIVOS)

# Regresión: ¿la brecha se achicó más en las firmas más expuestas?
# Estimamos también el salario de cada categoría para ver cuál se movió.
compresion <- bind_rows(
  estimar_did("log_salario_obrero",         base = datos_compresion, etiqueta = "Salario obrero (log)"),
  estimar_did("log_salario_administrativo", base = datos_compresion, etiqueta = "Salario administrativo (log)"),
  estimar_did("log_salario_profesional",    base = datos_compresion, etiqueta = "Salario profesional (log)"),
  estimar_did("brecha_admin_obrero",        base = datos_compresion, etiqueta = "Brecha administrativo - obrero (log)"),
  estimar_did("brecha_prof_obrero",         base = datos_compresion, etiqueta = "Brecha profesional - obrero (log)")
) %>%
  mutate(efecto_porcentual = 100 * coeficiente,
         lectura = "Exploratorio")

ver(compresion)
guardar_tabla(compresion, "T13c_compresion_salarial", carpeta = CARPETA_ESTIMACION,
              "Tabla 13c. Compresión salarial: efecto sobre salarios por categoría y brechas (exploratorio)",
              decimales = 4)

cat("\nLECTURA: un coeficiente negativo en las brechas indica que la brecha se",
    "\nachicó más en las firmas más expuestas después de 2023 (compresión).\n")


# ==============================================================================
# 9. HETEROGENEIDAD POR TAMAÑO
# ==============================================================================
titulo("9. HETEROGENEIDAD POR TAMAÑO")

# Revisamos si el efecto cambia según el tamaño de la firma. Repetimos la
# regresión por separado para pequeñas, medianas y grandes (tamaño de 2022).
# Dentro de cada grupo todas las firmas tienen el mismo tamaño, así que
# quitamos ese control.

EFECTOS_SIN_TAMANO <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F"

# Empezamos con una tabla vacía y en cada vuelta del bucle le agregamos los
# resultados de un tamaño (salario y empleo)
heterogeneidad <- tibble()
for (tamano in c("Pequena", "Mediana", "Grande")) {
  base_tamano <- filter(datos, tamano_2022 == tamano)
  cat("\nEstimando para firmas de tamaño:", tamano, "\n")
  
  salario_tamano <- estimar_did("log_salario", base = base_tamano, efectos = EFECTOS_SIN_TAMANO,
                                etiqueta = "Salario promedio (log)")
  empleo_tamano  <- estimar_did("log_empleo", base = base_tamano, efectos = EFECTOS_SIN_TAMANO,
                                etiqueta = "Empleo total (log)")
  ver(salario_tamano)
  ver(empleo_tamano)
  
  heterogeneidad <- bind_rows(heterogeneidad,
                              mutate(salario_tamano, tamano = tamano),
                              mutate(empleo_tamano, tamano = tamano))
}

# Ponemos el tamaño como primera columna y en orden de menor a mayor
heterogeneidad <- heterogeneidad %>%
  relocate(tamano) %>%
  mutate(tamano = factor(tamano, levels = c("Pequena", "Mediana", "Grande")))

ver(heterogeneidad)
guardar_tabla(heterogeneidad, "T14_heterogeneidad_tamano", carpeta = CARPETA_ESTIMACION,
              "Tabla 14. Efecto por tamaño de firma (salario y empleo, log)", decimales = 4)

# Gráfico con el efecto en % y su intervalo de confianza para cada tamaño
grafico_tamano <- ggplot(heterogeneidad, aes(x = tamano, y = 100 * coeficiente, color = resultado)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = 100 * ic95_inferior, ymax = 100 * ic95_superior),
                  position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c(`Salario promedio (log)` = COLOR_BAJA, `Empleo total (log)` = COLOR_ALTA)) +
  labs(title = "Efecto del aumento de 2023 según tamaño de la firma",
       subtitle = "Cambio % por una desviación estándar más de Kaitz",
       x = "Tamaño de la firma (2022)", y = "Efecto (%)", color = NULL, caption = NOTA_MODELO) +
  tema_tesis
guardar_grafico(grafico_tamano, "G08_heterogeneidad_tamano", carpeta = CARPETA_ESTIMACION)


# ==============================================================================
# 10. SUPUESTOS DEL MODELO
# ==============================================================================
titulo("10. SUPUESTOS DEL MODELO")

# Dejamos por escrito los supuestos en los que se apoya la estimación, qué
# evidencia de este script los respalda y qué queda para el script de
# validaciones.

supuestos <- tibble(
  supuesto = c(
    "1. Tendencias paralelas",
    "2. Kaitz mide la exposición al choque",
    "3. Choques comunes absorbidos por los controles",
    "4. Sin anticipación",
    "5. Exposición medida antes del choque",
    "6. Resultados exploratorios",
    "7. Compresión salarial (exploratorio)"
  ),
  que_significa = c(
    "Sin el aumento de 2023, el empleo de firmas con alta y baja exposición habría evolucionado de forma parecida (dentro del mismo sector, tamaño y departamento).",
    "Las firmas con Kaitz más alto enfrentaron un aumento mayor de su costo laboral en 2023.",
    "Pandemia, inflación, ciclo económico y subsidios afectaron por igual a las firmas de un mismo año, sector, tamaño y departamento.",
    "Las firmas no ajustaron su empleo antes de conocer el aumento (decretado en diciembre de 2022).",
    "El Kaitz de 2022 no está afectado por el aumento de 2023.",
    "Composición del empleo y ventas se reportan como asociaciones, no como efectos causales.",
    "La brecha entre obreros y otras categorías cambia por el aumento del mínimo y no por cambios en quién trabaja en la firma."
  ),
  evidencia_en_este_script = c(
    "Gráficos G06 y G07: coeficientes de los años previos a 2023.",
    "Tabla 8 y gráfico G05 (primer eslabón).",
    "Sección 5: dummies de pandemia y subsidios absorbidas por el efecto fijo de año.",
    "Coeficientes de 2021 y 2022 en los estudios de evento.",
    "Kaitz construido con datos de 2022.",
    "Tabla 13.",
    "Tabla 13b, gráfico G08b y tabla 13c."
  ),
  pendiente_en_validaciones = c(
    "Sensibilidad al período de comparación (2022 fue un año de alto empleo) y magnitud tolerable de desviaciones (Honest DiD).",
    "El salario relativo de las firmas expuestas venía cayendo hasta 2022 (prueba de años previos del salario). Separar el salto de 2023 de una recuperación de salarios bajos del año base.",
    "PAEF e incentivo a nuevos empleos pagaban un monto fijo por trabajador, con más peso en firmas de salarios bajos; 2024 como año sin subsidios.",
    "Revisar si hubo ajustes a finales de 2022.",
    "Sensibilidad a medir Kaitz en otros años.",
    "Patrón de las tendencias previas en composición del empleo.",
    "Separar compresión de cambios de composición; repetir con Kaitz medido en otro año."
  )
)

ver(supuestos)
guardar_tabla(supuestos, "T15_supuestos_del_modelo", carpeta = CARPETA_VALIDACIONES,
              "Tabla 15. Supuestos del modelo y validaciones pendientes")


# ==============================================================================
# 11. RESUMEN DE RESULTADOS
# ==============================================================================
titulo("11. RESUMEN DE RESULTADOS")

# leer_efecto(): arma una frase con el coeficiente, las estrellas, el
# intervalo de confianza y el p-valor de una fila de resultados
leer_efecto <- function(tabla, fila) {
  paste0(round(tabla$coeficiente[fila], 4), " ", tabla$significancia[fila],
         " (IC95%: ", round(tabla$ic95_inferior[fila], 4), " a ", round(tabla$ic95_superior[fila], 4),
         "; p = ", round(tabla$p_valor[fila], 3), ")")
}

# Mostramos en pantalla los números principales
cat("\nPRIMER ESLABÓN - salario promedio (log):")
cat("\n  A. Cambio 2022 -> 2023:", leer_efecto(primer_eslabon, 1))
cat("\n  B. Salto 2023 frente al cambio típico 2016-2019:", leer_efecto(primer_eslabon, 2))
cat("\n  C. Promedio después menos antes:", leer_efecto(primer_eslabon, 3))
cat("\n  -> Entre 2022 y 2023, el salario de una firma una desviación estándar más expuesta cambió",
    round(primer_eslabon$efecto_porcentual[1], 2), "% frente a las demás (lectura A).\n")

cat("\nRESULTADO PRINCIPAL - empleo total (trabajadores):", leer_efecto(empleo_principal, 1))
cat("\nRESULTADO PRINCIPAL - empleo total (log):", leer_efecto(empleo_principal, 2))
cat("\n  -> Por cada desviación estándar de Kaitz, el empleo cambia",
    round(empleo_principal$efecto_porcentual[2], 2), "% después de 2023.\n")

cat("\nPrueba conjunta de años previos (p-valor):",
    "\n  salario:", round(evento_salario$p_antes, 3),
    "\n  empleo (trabajadores):", round(evento_empleo$p_antes, 3),
    "\n  empleo (log):", round(evento_log_empleo$p_antes, 3), "\n")

cat("\nRecordatorio de lectura:",
    "\n  *** p < 0,01   ** p < 0,05   * p < 0,10",
    "\n  Si el intervalo de confianza incluye el cero, no se detecta un efecto",
    "\n  distinto de cero; el intervalo indica el rango de efectos posibles.\n")

# Juntamos todas las tablas en un solo documento de Word
save_as_docx(values = compendio, path = file.path(CARPETA_PRINCIPAL, "00_compendio_tablas.docx"))
cat("\nCompendio guardado: 00_compendio_tablas.docx con", length(compendio), "tablas.\n")

# Dejamos a mano la lista de gráficos para volver a verlos cuando queramos.
# Por ejemplo, en la consola: print(graficos[["G06_evento_empleo"]])
cat("\nGráficos disponibles para volver a verlos:\n")
cat(paste(" -", names(graficos)), sep = "\n")

# revisar_graficos(): muestra los gráficos uno por uno y espera Enter entre
# cada uno. Se llama a mano en la consola, NO dentro del script:
#   revisar_graficos()
revisar_graficos <- function() {
  for (nombre in names(graficos)) {
    print(graficos[[nombre]])
    readline(paste0(nombre, " - Enter para el siguiente (Esc para salir)... "))
  }
}
cat("\nPara revisar los gráficos uno por uno, escribe en la consola: revisar_graficos()\n")

titulo("FIN DEL SCRIPT")

