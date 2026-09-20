# ==============================================================================
# 04_descriptivos_evolucion.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# QUÉ HACE ESTE SCRIPT
# Describe cómo venían las firmas ANTES del choque de 2023: ventas, producción,
# valor agregado, empleo, salarios, productividad, inversión, composición del
# empleo y subcontratación. No estima nada: solo muestra las trayectorias.
#
# Sirve para dos cosas:
#   1. El capítulo descriptivo de la tesis.
#   2. Ver con los ojos si los grupos de exposición venían parecidos antes de
#      2023, que es lo que el supuesto de tendencias paralelas requiere.
#
# AVISO SOBRE LOS VALORES
# La EAM reporta valores nominales y no tenemos deflactor en la base. Por eso
# todas las series de dinero se muestran como índice frente a 2015 dentro de
# cada grupo: la inflación afecta a los dos grupos por igual, así que la
# COMPARACIÓN entre grupos es informativa aunque el nivel no lo sea.
# Las razones (salario sobre ventas, valor agregado por trabajador sobre el
# mínimo) no dependen del deflactor y se leen directo.
#
# Se corre desde la raíz del repositorio (abriendo TESIS_MECA.Rproj).
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "Descriptivos_evolucion")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

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
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 2) {
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
guardar_grafico <- function(grafico, nombre_archivo, ancho = 10, alto = 6) {
  print(grafico)
  ggsave(file.path(CARPETA, "figuras", paste0(nombre_archivo, ".png")),
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
PALETA_QUINTILES <- "RdYlBu"

NOTA_NOMINAL <- paste("Valores nominales de la EAM, sin deflactar: cada serie se muestra frente a su propio nivel de 2015.",
                      "\nLa comparación entre grupos es válida; el nivel absoluto no.")
NOTA_NOMINAL_2019 <- paste("Valores nominales de la EAM, sin deflactar: cada serie se muestra frente a su propio nivel de 2019.",
                           "\nLa comparación entre grupos es válida; el nivel absoluto no.")


# ==============================================================================
# 1. DATOS
# ==============================================================================
titulo("1. DATOS Y GRUPOS DE EXPOSICIÓN")

panel <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

cat("Panel:", nrow(panel), "filas,", n_distinct(panel$NORDEMP), "firmas\n")

# buscar_variable(): los nombres de algunas columnas cambiaron entre versiones
# de la base. Esta función toma varios nombres posibles y devuelve el primero
# que exista, o NA si no hay ninguno. Así el script no se cae por un nombre.
buscar_variable <- function(...) {
  candidatos <- c(...)
  encontrada <- candidatos[candidatos %in% names(panel)]
  if (length(encontrada) == 0) NA_character_ else encontrada[1]
}

VAR <- list(
  ventas         = buscar_variable("VALORVEN", "valor_ventas_valorven"),
  produccion     = buscar_variable("produccion_bruta_prodbr2", "produccion_industrial_prodbind"),
  valor_agregado = buscar_variable("valor_agregado_valagri"),
  consumo        = buscar_variable("consumo_intermedio_consin2"),
  costo_laboral  = buscar_variable("costos_totales_personal_total_c3r10c3"),
  inversion      = buscar_variable("inversion_total_c7r10c2", "inversion_bruta_invebrta"),
  activos        = buscar_variable("activos_fijos_activfi"),
  terceros       = buscar_variable("terceros_total_c3r41c3", "prod_terceros_total_c3r14c3"),
  agencias       = buscar_variable("agencias_personal_temporal_total_c3r8c3", "agencias_valor_total_c3r8c3"),
  energia        = buscar_variable("energia_consumida_kwh_c5r1c4", "energia_comprada_kwh_c5r1c1"),
  exportado      = buscar_variable("porcentaje_exportado_porcvt", "pct_exportado_porcvt")
)

cobertura <- tibble(concepto = names(VAR),
                    variable = unlist(VAR, use.names = FALSE)) %>%
  mutate(disponible = ifelse(is.na(variable), "NO ESTÁ EN LA BASE", "sí"))
ver(cobertura, filas = 20)
guardar_tabla(cobertura, "TD00_variables_encontradas",
              "Tabla D0. Variables usadas en los descriptivos y si están en la base", decimales = 0)

# Grupos de exposición: los mismos del script principal
firmas_2022 <- panel %>%
  filter(ANIO == 2022, !is.na(Bite2022_obreros)) %>%
  select(NORDEMP, Bite2022_obreros, CIIU4, tamano_empresa)

limites <- quantile(firmas_2022$Bite2022_obreros, probs = c(0.01, 0.99))

firmas_2022 <- firmas_2022 %>%
  mutate(kaitz      = pmin(pmax(Bite2022_obreros, limites[1]), limites[2]),
         exposicion = factor(ifelse(kaitz > median(kaitz), "Alta exposición", "Baja exposición"),
                             levels = c("Baja exposición", "Alta exposición")),
         quintil    = cut(kaitz, breaks = quantile(kaitz, probs = seq(0, 1, 0.2)),
                          labels = c("Q1 (menos expuestas)", "Q2", "Q3", "Q4", "Q5 (más expuestas)"),
                          include.lowest = TRUE),
         tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")),
         sector_2022 = factor(CIIU4)) %>%
  select(NORDEMP, kaitz, exposicion, quintil, tamano_2022, sector_2022)

# Base de trabajo, con las variables renombradas a algo legible
tomar <- function(nombre) if (is.na(VAR[[nombre]])) NA_real_ else panel[[VAR[[nombre]]]]

datos <- panel %>%
  mutate(
    ventas          = tomar("ventas"),
    produccion      = tomar("produccion"),
    valor_agregado  = tomar("valor_agregado"),
    consumo         = tomar("consumo"),
    costo_laboral   = tomar("costo_laboral"),
    inversion       = tomar("inversion"),
    activos         = tomar("activos"),
    terceros        = tomar("terceros"),
    agencias_valor  = tomar("agencias"),
    energia         = tomar("energia"),
    exportado       = tomar("exportado")
  ) %>%
  inner_join(firmas_2022, by = "NORDEMP") %>%
  mutate(
    salario_promedio      = ifelse(empleo_total > 0, costo_laboral / empleo_total, NA),
    productividad         = ifelse(empleo_total > 0, valor_agregado / empleo_total, NA),
    ventas_por_trabajador = ifelse(empleo_total > 0, ventas / empleo_total, NA),
    # Razones, que no dependen del deflactor
    costo_laboral_sobre_produccion = ifelse(produccion > 0, 100 * costo_laboral / produccion, NA),
    costo_laboral_sobre_va         = ifelse(valor_agregado > 0, 100 * costo_laboral / valor_agregado, NA),
    inversion_sobre_activos        = ifelse(activos > 0, 100 * inversion / activos, NA),
    terceros_sobre_produccion      = ifelse(produccion > 0, 100 * terceros / produccion, NA),
    agencias_sobre_costo_laboral   = ifelse(costo_laboral > 0, 100 * agencias_valor / costo_laboral, NA),
    # Composición del empleo
    pct_permanente = ifelse(empleo_total > 0, 100 * empleo_permanente / empleo_total, NA),
    pct_temporal   = ifelse(empleo_total > 0, 100 * empleo_temporal / empleo_total, NA),
    pct_agencias   = ifelse(empleo_total > 0, 100 * temporal_agencias / empleo_total, NA),
    pct_obreros    = ifelse(empleo_total > 0, 100 * obreros_permanentes / empleo_total, NA)
  )

cat("Firmas en los descriptivos:", n_distinct(datos$NORDEMP),
    "| observaciones:", nrow(datos), "\n")
cat("Años:", paste(sort(unique(datos$ANIO)), collapse = ", "), "\n")


# ==============================================================================
# D1. CÓMO VENÍA LA INDUSTRIA: SERIES AGREGADAS
# ==============================================================================
titulo("D1. SERIES AGREGADAS DE LA MUESTRA")

# Para cada año: la mediana de cada variable y el total de la muestra.
# Usamos la mediana porque unas pocas firmas muy grandes dominan los promedios.
series_agregadas <- datos %>%
  group_by(ANIO) %>%
  summarise(
    firmas                 = n(),
    empleo_total_muestra   = sum(empleo_total, na.rm = TRUE),
    empleo_mediana         = median(empleo_total, na.rm = TRUE),
    ventas_mediana         = median(ventas, na.rm = TRUE),
    produccion_mediana     = median(produccion, na.rm = TRUE),
    valor_agregado_mediana = median(valor_agregado, na.rm = TRUE),
    salario_mediana        = median(salario_promedio, na.rm = TRUE),
    productividad_mediana  = median(productividad, na.rm = TRUE),
    .groups = "drop"
  )

ver(series_agregadas, filas = 12)
guardar_tabla(series_agregadas, "TD01_series_agregadas",
              "Tabla D1. Series agregadas de la muestra, por año", decimales = 0)

# El mismo cuadro en forma de índice frente a 2015, que es como se compara
indice_frente_2015 <- series_agregadas %>%
  mutate(across(c(empleo_total_muestra, empleo_mediana, ventas_mediana, produccion_mediana,
                  valor_agregado_mediana, salario_mediana, productividad_mediana),
                ~ round(100 * .x / .x[ANIO == 2015], 1)))

ver(indice_frente_2015, filas = 12)
guardar_tabla(indice_frente_2015, "TD02_indice_frente_2015",
              "Tabla D2. Las mismas series como índice (2015 = 100)", decimales = 1)

serie_larga <- indice_frente_2015 %>%
  select(ANIO, Empleo = empleo_mediana, Ventas = ventas_mediana,
         Producción = produccion_mediana, `Valor agregado` = valor_agregado_mediana,
         `Costo laboral por trabajador` = salario_mediana) %>%
  pivot_longer(-ANIO, names_to = "variable", values_to = "indice")

grafico_series <- ggplot(serie_larga, aes(x = ANIO, y = indice, color = variable)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 100, color = "grey80") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(title = "Cómo venía la industria manufacturera de la muestra",
       subtitle = "Mediana de cada variable, con 2015 = 100. La línea punteada marca el aumento del mínimo de 2023",
       x = NULL, y = "Índice (2015 = 100)", color = NULL, caption = NOTA_NOMINAL) +
  tema_tesis
guardar_grafico(grafico_series, "GD01_series_agregadas")


# ==============================================================================
# D2. LOS DOS GRUPOS DE EXPOSICIÓN, LADO A LADO
# ==============================================================================
titulo("D2. EVOLUCIÓN POR GRUPO DE EXPOSICIÓN")

# Aquí está lo que importa para el diseño: si los dos grupos venían con
# trayectorias parecidas antes de 2023, la comparación tiene sentido.
por_grupo <- datos %>%
  group_by(exposicion, ANIO) %>%
  summarise(
    firmas         = n(),
    empleo         = median(empleo_total, na.rm = TRUE),
    ventas         = median(ventas, na.rm = TRUE),
    produccion     = median(produccion, na.rm = TRUE),
    valor_agregado = median(valor_agregado, na.rm = TRUE),
    salario        = median(salario_promedio, na.rm = TRUE),
    productividad  = median(productividad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(exposicion) %>%
  mutate(across(c(empleo, ventas, produccion, valor_agregado, salario, productividad),
                ~ round(100 * .x / .x[ANIO == 2015], 1),
                .names = "indice_{.col}")) %>%
  ungroup()

ver(por_grupo, filas = 20)
guardar_tabla(por_grupo, "TD03_evolucion_por_grupo",
              "Tabla D3. Evolución por grupo de exposición (mediana e índice 2015 = 100)",
              decimales = 1)

grupo_largo <- por_grupo %>%
  select(exposicion, ANIO,
         Empleo = indice_empleo, Ventas = indice_ventas,
         Producción = indice_produccion, `Valor agregado` = indice_valor_agregado,
         `Costo laboral por trabajador` = indice_salario,
         Productividad = indice_productividad) %>%
  pivot_longer(-c(exposicion, ANIO), names_to = "variable", values_to = "indice")

grafico_grupos <- ggplot(grupo_largo, aes(x = ANIO, y = indice, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 100, color = "grey80") +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Las firmas más y menos expuestas, antes y después del choque",
       subtitle = "Mediana de cada variable con 2015 = 100, dentro de cada grupo",
       x = NULL, y = "Índice (2015 = 100)", color = NULL, caption = NOTA_NOMINAL) +
  tema_tesis
guardar_grafico(grafico_grupos, "GD02_evolucion_por_grupo", ancho = 12, alto = 7)


# ==============================================================================
# D3. RAZONES QUE NO DEPENDEN DEL DEFLACTOR
# ==============================================================================
titulo("D3. ESTRUCTURA DE COSTOS Y SUBCONTRATACIÓN")

# Estas razones son las más informativas, porque la inflación se cancela en el
# cociente: el costo laboral y la producción suben juntos con los precios.
razones <- datos %>%
  group_by(exposicion, ANIO) %>%
  summarise(
    costo_laboral_sobre_produccion = median(costo_laboral_sobre_produccion, na.rm = TRUE),
    costo_laboral_sobre_va         = median(costo_laboral_sobre_va, na.rm = TRUE),
    inversion_sobre_activos        = median(inversion_sobre_activos, na.rm = TRUE),
    .groups = "drop"
  )

ver(razones, filas = 20)
guardar_tabla(razones, "TD04_razones_estructurales",
              "Tabla D4. Estructura de costos e inversión por grupo y año (medianas, %)", decimales = 2)

razones_largo <- razones %>%
  rename(`Costo laboral / producción` = costo_laboral_sobre_produccion,
         `Costo laboral / valor agregado` = costo_laboral_sobre_va,
         `Inversión / activos fijos` = inversion_sobre_activos) %>%
  pivot_longer(-c(exposicion, ANIO), names_to = "razon", values_to = "valor") %>%
  filter(!is.na(valor), is.finite(valor))

grafico_razones <- ggplot(razones_largo, aes(x = ANIO, y = valor, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  facet_wrap(~ razon, scales = "free_y") +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Estructura de costos e inversión",
       subtitle = "Medianas por grupo. Estas razones no dependen de la inflación, así que se leen directo",
       x = NULL, y = "%", color = NULL,
       caption = "El costo laboral sobre producción mide qué tanto pesa la nómina en lo que produce la firma.") +
  tema_tesis
guardar_grafico(grafico_razones, "GD03_razones_estructurales", ancho = 12, alto = 5)


# --- D3b. Subcontratación: cuántas firmas la usan y cuánto usan las que la usan ---
titulo("D3b. SUBCONTRATACIÓN")

# La mayoría de las firmas no subcontrata, así que la mediana da cero y no
# dice nada. Para que sirva hay que mirar dos cosas por separado:
#   1. qué porcentaje de firmas usa cada forma de subcontratación;
#   2. entre las que sí la usan, cuánto pesa.

subcontratacion <- datos %>%
  group_by(exposicion, ANIO) %>%
  summarise(
    firmas = n(),
    pct_usa_terceros   = 100 * mean(terceros > 0, na.rm = TRUE),
    pct_usa_agencias   = 100 * mean(agencias_valor > 0, na.rm = TRUE),
    pct_tiene_temporales_agencia = 100 * mean(temporal_agencias > 0, na.rm = TRUE),
    # Peso entre las que sí usan
    terceros_si_usa = median(terceros_sobre_produccion[terceros > 0], na.rm = TRUE),
    agencias_si_usa = median(agencias_sobre_costo_laboral[agencias_valor > 0], na.rm = TRUE),
    .groups = "drop"
  )

ver(subcontratacion, filas = 20)
guardar_tabla(subcontratacion, "TD04b_subcontratacion",
              "Tabla D4b. Subcontratación: porcentaje de firmas que la usan y peso entre las que la usan",
              decimales = 2)

subcontratacion_larga <- subcontratacion %>%
  select(exposicion, ANIO,
         `% de firmas que contrata trabajos de terceros` = pct_usa_terceros,
         `% de firmas que usa personal de agencias` = pct_usa_agencias,
         `Terceros / producción, entre las que usan` = terceros_si_usa,
         `Agencias / costo laboral, entre las que usan` = agencias_si_usa) %>%
  pivot_longer(-c(exposicion, ANIO), names_to = "indicador", values_to = "valor") %>%
  filter(!is.na(valor), is.finite(valor))

grafico_subcontratacion <- ggplot(subcontratacion_larga,
                                  aes(x = ANIO, y = valor, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  facet_wrap(~ indicador, scales = "free_y") +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Subcontratación: cuántas firmas la usan y cuánto pesa",
       subtitle = "Arriba, el porcentaje de firmas que subcontrata. Abajo, cuánto pesa entre las que sí lo hacen",
       x = NULL, y = "%", color = NULL,
       caption = paste("La mediana simple no sirve aquí porque la mayoría de las firmas no subcontrata y da cero.",
                       "\nSi la subcontratación sube en las firmas expuestas tras 2023, es un mecanismo de ajuste.")) +
  tema_tesis
guardar_grafico(grafico_subcontratacion, "GD03b_subcontratacion", ancho = 12, alto = 7)


# ==============================================================================
# D4. COMPOSICIÓN DEL EMPLEO
# ==============================================================================
titulo("D4. COMPOSICIÓN DEL EMPLEO")

composicion <- datos %>%
  group_by(exposicion, ANIO) %>%
  summarise(
    permanentes = median(pct_permanente, na.rm = TRUE),
    temporales  = median(pct_temporal, na.rm = TRUE),
    agencias    = median(pct_agencias, na.rm = TRUE),
    obreros     = median(pct_obreros, na.rm = TRUE),
    .groups = "drop"
  )

ver(composicion, filas = 20)
guardar_tabla(composicion, "TD05_composicion_empleo",
              "Tabla D5. Composición del empleo por grupo y año (medianas, % del empleo total)",
              decimales = 1)

composicion_larga <- composicion %>%
  rename(`Permanentes` = permanentes, `Temporales` = temporales,
         `De agencias` = agencias, `Obreros permanentes` = obreros) %>%
  pivot_longer(-c(exposicion, ANIO), names_to = "categoria", values_to = "porcentaje")

grafico_composicion <- ggplot(composicion_larga,
                              aes(x = ANIO, y = porcentaje, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  facet_wrap(~ categoria, scales = "free_y") +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Composición del empleo por grupo de exposición",
       subtitle = "Medianas del porcentaje sobre el empleo total de cada firma",
       x = NULL, y = "% del empleo total", color = NULL,
       caption = "Si las firmas expuestas sustituyeran permanentes por temporales tras 2023, se vería aquí.") +
  tema_tesis
guardar_grafico(grafico_composicion, "GD04_composicion_empleo", ancho = 12, alto = 7)


# ==============================================================================
# D5. EL COSTO LABORAL FRENTE AL SALARIO MÍNIMO
# ==============================================================================
titulo("D5. EL COSTO LABORAL EN VECES EL SALARIO MÍNIMO")

# --- El mínimo legal y el costo mínimo de contratación -------------------------
# Dos cosas distintas que conviene no confundir:
#   - el SALARIO mínimo es lo que recibe el trabajador como sueldo base;
#   - el COSTO mínimo de contratación es lo que le cuesta a la firma ese mismo
#     trabajador, con prestaciones, aportes y auxilio de transporte.
#
# El segundo es el concepto de Merchán Álvarez (2015) y es el que hay que usar
# cuando el numerador es el costo laboral total de la EAM (C3R10C3), porque ese
# también incluye prestaciones y aportes.
#
# Fuentes de los valores:
#   Salario mínimo y auxilio de transporte: decretos anuales del Gobierno.
#   Verificados contra Función Pública para 2023 (Decreto 2614 de 2022,
#   auxilio de $140.606) y 2024 (Decreto 2293 de 2023, auxilio de $162.000).
#
# Factores de la carga laboral, sobre el salario:
#   Pensión (empleador)      12,00 %   Ley 100 de 1993
#   Caja de compensación      4,00 %
#   Riesgos laborales (ARL)   ver ARL_TASA abajo; depende de la clase de riesgo
#   Vacaciones                4,17 %   (15 días hábiles al año)
#   Prima de servicios        8,33 %   sobre salario MÁS auxilio de transporte
#   Cesantías                 8,33 %   sobre salario MÁS auxilio de transporte
#   Intereses a las cesantías 1,00 %   (12 % de las cesantías)
#
# SENA (2 %), ICBF (3 %) y salud (8,5 %): las sociedades declarantes de renta
# están EXONERADAS por los trabajadores que ganan menos de 10 mínimos
# (artículo 114-1 del Estatuto Tributario, antes artículo 25 de la Ley 1607 de
# 2012). Casi todas las firmas de la EAM son sociedades, así que la exoneración
# aplica; aun así lo dejamos como interruptor para ver cuánto cambia.

EXONERADA_SENA_ICBF_SALUD <- TRUE   # artículo 114-1 del Estatuto Tributario
ARL_TASA <- 0.02436                 # clase III, típica de manufactura

salario_minimo <- tibble(
  ANIO   = 2015:2024,
  minimo = c(644350, 689455, 737717, 781242, 828116, 877803, 908526, 1000000, 1160000, 1300000),
  auxilio = c( 74000,  77700,  83140,  88211,  97032, 102854, 106454,  117172,  140606,  162000)
) %>%
  mutate(
    # Sobre el salario solamente
    carga_sobre_salario = 0.12 + 0.04 + ARL_TASA + 0.0417 +
      ifelse(EXONERADA_SENA_ICBF_SALUD, 0, 0.02 + 0.03 + 0.085),
    # Prima, cesantías e intereses: la base incluye el auxilio de transporte
    carga_sobre_salario_mas_auxilio = 0.0833 + 0.0833 + 0.01,
    costo_minimo = minimo * (1 + carga_sobre_salario) +
      (minimo + auxilio) * carga_sobre_salario_mas_auxilio + auxilio,
    factor_carga = costo_minimo / minimo
  )

ver(salario_minimo, filas = 10)
guardar_tabla(salario_minimo %>%
                select(ANIO, minimo, auxilio, costo_minimo, factor_carga),
              "TD11_costo_minimo_contratacion",
              "Tabla D11. Salario mínimo, auxilio de transporte y costo mínimo de contratación",
              decimales = 3)

cat("\nCrecimiento anual del mínimo y del costo mínimo de contratación (%):\n")
print(as.data.frame(
  salario_minimo %>%
    mutate(crece_minimo = round(100 * (minimo / lag(minimo) - 1), 2),
           crece_costo  = round(100 * (costo_minimo / lag(costo_minimo) - 1), 2),
           crece_auxilio = round(100 * (auxilio / lag(auxilio) - 1), 2)) %>%
    select(ANIO, crece_minimo, crece_auxilio, crece_costo) %>%
    filter(!is.na(crece_minimo))
), row.names = FALSE)

cat("\nLECTURA: el auxilio de transporte se decreta aparte y no sube igual que el",
    "\nmínimo, así que el costo mínimo de contratación y el salario mínimo no crecen",
    "\nal mismo ritmo. Esa diferencia es una fuente de variación que el salario",
    "\nmínimo por sí solo no captura.\n")

# Dos comparaciones, cada una con numerador y denominador del mismo tipo:
#   A. SALARIO del obrero sobre el SALARIO mínimo legal.
#      Mide qué tan cerca está el sueldo del piso salarial.
#   B. COSTO laboral por trabajador sobre el COSTO mínimo de contratación.
#      Mide qué tan cerca está la firma del costo mínimo que puede pagar.
#
# Antes este gráfico comparaba el costo total contra el salario mínimo pelado,
# lo que inflaba la razón en cerca de 50 % y hacía parecer que ninguna firma
# estaba cerca del mínimo.
veces_minimo <- datos %>%
  left_join(salario_minimo, by = "ANIO") %>%
  mutate(
    salario_obrero_mensual = ifelse(obreros_permanentes > 0,
                                    sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes / 12 * 1000,
                                    NA),
    costo_mensual_trabajador = salario_promedio * 1000 / 12,
    veces_salario = salario_obrero_mensual / minimo,
    veces_costo   = costo_mensual_trabajador / costo_minimo
  ) %>%
  group_by(quintil, ANIO) %>%
  summarise(
    salario_obrero_sobre_minimo = median(veces_salario, na.rm = TRUE),
    costo_sobre_costo_minimo    = median(veces_costo, na.rm = TRUE),
    firmas = n(), .groups = "drop")

ver(veces_minimo, filas = 20)
guardar_tabla(veces_minimo, "TD06_veces_el_minimo",
              "Tabla D6. Cercanía al mínimo por quintil: salario sobre el mínimo legal y costo laboral sobre el costo mínimo de contratación",
              decimales = 2)

veces_largo <- veces_minimo %>%
  rename(`Salario del obrero / salario mínimo` = salario_obrero_sobre_minimo,
         `Costo por trabajador / costo mínimo de contratación` = costo_sobre_costo_minimo) %>%
  pivot_longer(-c(quintil, ANIO, firmas), names_to = "comparacion", values_to = "veces") %>%
  filter(!is.na(veces), is.finite(veces))

grafico_veces <- ggplot(veces_largo, aes(x = ANIO, y = veces, color = quintil)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 1, color = "grey40", linetype = "dotted") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~ comparacion) +
  scale_color_brewer(palette = PALETA_QUINTILES, direction = -1) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Qué tan cerca del mínimo están las firmas de cada quintil",
       subtitle = "A la izquierda, el sueldo frente al piso salarial. A la derecha, lo que cuesta un trabajador frente al costo mínimo posible",
       x = NULL, y = "Veces el mínimo correspondiente", color = NULL,
       caption = paste("La línea punteada marca el mínimo: una firma en 1,0 paga exactamente el mínimo de esa comparación.",
                       "\nCosto mínimo de contratación: salario mínimo con prestaciones, aportes y auxilio de transporte (ver Tabla D11).")) +
  tema_tesis
guardar_grafico(grafico_veces, "GD05_veces_el_minimo", ancho = 12, alto = 6)

cat("\nLECTURA: las dos comparaciones deben contar la misma historia. Si la de la",
    "\nizquierda muestra a Q5 cerca de 1 y la de la derecha también, la medida de",
    "\nexposición está capturando firmas realmente pegadas al piso legal.\n")


# ==============================================================================
# D6. PERFIL DE LAS FIRMAS EN 2022, POR QUINTIL
# ==============================================================================
titulo("D6. PERFIL PRE-CHOQUE (2022)")

perfil_2022 <- datos %>%
  filter(ANIO == 2022) %>%
  group_by(quintil) %>%
  summarise(
    firmas                  = n(),
    kaitz_promedio          = mean(kaitz, na.rm = TRUE),
    empleo_mediana          = median(empleo_total, na.rm = TRUE),
    ventas_mediana          = median(ventas, na.rm = TRUE),
    valor_agregado_mediana  = median(valor_agregado, na.rm = TRUE),
    productividad_mediana   = median(productividad, na.rm = TRUE),
    costo_laboral_sobre_va  = median(costo_laboral_sobre_va, na.rm = TRUE),
    pct_permanente          = median(pct_permanente, na.rm = TRUE),
    pct_obreros             = median(pct_obreros, na.rm = TRUE),
    pct_pequenas            = 100 * mean(tamano_2022 == "Pequena", na.rm = TRUE),
    .groups = "drop"
  )

ver(perfil_2022, filas = 10)
guardar_tabla(perfil_2022, "TD07_perfil_2022_por_quintil",
              "Tabla D7. Perfil de las firmas en 2022, por quintil de exposición", decimales = 1)

cat("\nLECTURA: esta tabla muestra que la exposición está muy ligada al tamaño.",
    "\nSi el porcentaje de firmas pequeñas crece de Q1 a Q5, la comparación entre",
    "\nquintiles es en buena parte una comparación entre tamaños, y por eso el",
    "\ncontrol de tamaño por año pesa tanto en las estimaciones.\n")


# ==============================================================================
# D7. CRECIMIENTO ANUAL, AÑO POR AÑO
# ==============================================================================
titulo("D7. CRECIMIENTO ANUAL POR GRUPO")

# El crecimiento anual de la firma mediana. Sirve para ver si algún año se
# despega, sobre todo 2021 y 2022, que son los años de los subsidios.
crecimiento <- datos %>%
  arrange(NORDEMP, ANIO) %>%
  group_by(NORDEMP) %>%
  mutate(crece_empleo = 100 * (empleo_total / lag(empleo_total) - 1),
         crece_ventas = 100 * (ventas / lag(ventas) - 1),
         anio_previo  = lag(ANIO)) %>%
  ungroup() %>%
  filter(anio_previo == ANIO - 1) %>%     # solo años consecutivos
  group_by(exposicion, ANIO) %>%
  summarise(crecimiento_empleo = median(crece_empleo, na.rm = TRUE),
            crecimiento_ventas = median(crece_ventas, na.rm = TRUE),
            firmas = n(), .groups = "drop")

ver(crecimiento, filas = 20)
guardar_tabla(crecimiento, "TD08_crecimiento_anual",
              "Tabla D8. Crecimiento anual de la firma mediana, por grupo (%)", decimales = 2)

crecimiento_largo <- crecimiento %>%
  select(exposicion, ANIO, Empleo = crecimiento_empleo, Ventas = crecimiento_ventas) %>%
  pivot_longer(-c(exposicion, ANIO), names_to = "variable", values_to = "crecimiento")

grafico_crecimiento <- ggplot(crecimiento_largo, aes(x = factor(ANIO), y = crecimiento, fill = exposicion)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  labs(title = "Crecimiento anual de la firma mediana",
       subtitle = "Solo pares de años consecutivos, así que 2021 no aparece: viene de 2019, que no es consecutivo",
       x = NULL, y = "Crecimiento anual (%)", fill = NULL,
       caption = paste("Mediana del crecimiento de cada firma, no crecimiento de la mediana.",
                       "\nLas ventas están en pesos corrientes: el salto de 2022 incluye la inflación de ese año,",
                       "\nque fue la más alta del período. No es crecimiento real.")) +
  tema_tesis
guardar_grafico(grafico_crecimiento, "GD06_crecimiento_anual", ancho = 12)


# ==============================================================================
# D8. LAS MISMAS SERIES, PERO CON 2019 COMO BASE
# ==============================================================================
titulo("D8. EVOLUCIÓN DESDE 2019 (ANTES DE LA PANDEMIA)")

# Si los dos grupos vienen divergiendo desde 2015, la pregunta siguiente es si
# la brecha se abrió durante toda la década o solo en los años recientes.
# Aquí repetimos el mismo ejercicio pero con 2019 como base, que es el último
# año normal antes de la pandemia y de los subsidios.

desde_2019 <- datos %>%
  filter(ANIO >= 2019) %>%
  group_by(exposicion, ANIO) %>%
  summarise(
    empleo         = median(empleo_total, na.rm = TRUE),
    ventas         = median(ventas, na.rm = TRUE),
    valor_agregado = median(valor_agregado, na.rm = TRUE),
    salario        = median(salario_promedio, na.rm = TRUE),
    productividad  = median(productividad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(exposicion) %>%
  mutate(across(c(empleo, ventas, valor_agregado, salario, productividad),
                ~ round(100 * .x / .x[ANIO == 2019], 1))) %>%
  ungroup()

ver(desde_2019, filas = 12)
guardar_tabla(desde_2019, "TD09_evolucion_desde_2019",
              "Tabla D9. Evolución por grupo desde 2019 (2019 = 100)", decimales = 1)

# Las dos bases juntas, para ver cuánto de la brecha es reciente
brecha_por_base <- bind_rows(
  por_grupo %>%
    select(exposicion, ANIO, Empleo = indice_empleo, Ventas = indice_ventas,
           `Valor agregado` = indice_valor_agregado, Salario = indice_salario,
           Productividad = indice_productividad) %>%
    mutate(base = "2015 = 100"),
  desde_2019 %>%
    select(exposicion, ANIO, Empleo = empleo, Ventas = ventas,
           `Valor agregado` = valor_agregado, Salario = salario,
           Productividad = productividad) %>%
    mutate(base = "2019 = 100")
) %>%
  pivot_longer(-c(exposicion, ANIO, base), names_to = "variable", values_to = "indice")

grafico_desde_2019 <- ggplot(filter(brecha_por_base, base == "2019 = 100"),
                             aes(x = ANIO, y = indice, color = exposicion)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 100, color = "grey80") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_color_manual(values = c(`Baja exposición` = COLOR_BAJA, `Alta exposición` = COLOR_ALTA)) +
  scale_x_continuous(breaks = c(2019, 2021, 2022, 2023, 2024)) +
  labs(title = "La evolución desde 2019, el último año antes de la pandemia",
       subtitle = "Si la brecha entre grupos ya estaba abierta en 2015, aquí se ve cuánto se abrió en los años recientes",
       x = NULL, y = "Índice (2019 = 100)", color = NULL, caption = NOTA_NOMINAL_2019) +
  tema_tesis
guardar_grafico(grafico_desde_2019, "GD07_desde_2019", ancho = 12, alto = 7)

# Cuánta brecha hay en 2022 con cada base: el número que resume el problema
brecha_2022 <- brecha_por_base %>%
  filter(ANIO == 2022) %>%
  pivot_wider(names_from = exposicion, values_from = indice) %>%
  mutate(brecha = round(`Baja exposición` - `Alta exposición`, 1)) %>%
  arrange(variable, base)

ver(brecha_2022, filas = 12)
guardar_tabla(brecha_2022, "TD10_brecha_2022_segun_base",
              "Tabla D10. Diferencia entre grupos en 2022, según el año que se tome como base",
              decimales = 1)

cat("\nLECTURA: la columna 'brecha' dice cuántos puntos de índice separa a los dos",
    "\ngrupos en 2022. Si la brecha con base 2015 es mucho mayor que con base 2019,",
    "\nla divergencia viene de lejos y no de los años recientes. Si son parecidas,",
    "\nla brecha se abrió sobre todo después de 2019.\n")


# ==============================================================================
# RESUMEN
# ==============================================================================
titulo("RESUMEN")

cat("\nVariables que NO estaban en la base (sus gráficos salen vacíos):\n")
faltantes <- cobertura %>% filter(disponible != "sí")
if (nrow(faltantes) == 0) cat("  ninguna\n") else print(as.data.frame(faltantes), row.names = FALSE)

cat("\nÍndice 2015 = 100 en 2022, por grupo (el año previo al choque):\n")
print(as.data.frame(
  por_grupo %>%
    filter(ANIO == 2022) %>%
    select(exposicion, Empleo = indice_empleo, Ventas = indice_ventas,
           `Valor agregado` = indice_valor_agregado, Salario = indice_salario)
), row.names = FALSE)

cat("\nDiferencia entre grupos en 2022, según el año base (puntos de índice):\n")
print(as.data.frame(brecha_2022 %>% select(variable, base, brecha)), row.names = FALSE)

cat("\nQUÉ MIRAR EN LOS GRÁFICOS:",
    "\n  GD02: si las dos líneas van parejas hasta 2022, la comparación del diseño",
    "\n        tiene sentido; si se separan antes, hay que decirlo.",
    "\n  GD03b: si la subcontratación sube en las expuestas después de 2023, es un",
    "\n        mecanismo de ajuste; si ya venía subiendo, no se le puede atribuir.",
    "\n  GD07: cuánto de la brecha entre grupos es reciente y cuánto viene de lejos.",
    "\n  GD04: aquí se vería la sustitución de permanentes por temporales.",
    "\n  GD05: qué tan cerca del mínimo está cada quintil, que es la razón de ser",
    "\n        de la medida de exposición.\n")

if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_descriptivos.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

