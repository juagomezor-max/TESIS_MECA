# ==============================================================================
# 14_outcome_alternativo.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# ¿EL AUMENTO DE 2022 ENSUCIA EL OUTCOME DE 2023?
#
# DE DÓNDE VIENE ESTE SCRIPT
# 13_celda_limpia_corregida.R, con la indexación del rezago ya corregida, dejó
# esta tabla (exposición 2019, celda limpia, las cinco medidas):
#
#   Medida        2023          2024
#   Bite          0,81%  **     0,85%  **
#   Golpe A       0,63%  (ns)   1,10%  ***
#   Golpe C       0,57%  (ns)   1,30%  ***
#   Golpe costo   0,71%  (ns)   1,35%  ***
#   Exposure      0,49%  (ns)   0,20%  (ns)
#
# Bite se sostiene en los dos años. Exposure no se sostiene en ninguno (es
# coherente: es la única medida sin canal salarial). Las otras tres se
# sostienen SOLO en 2024. Esa asimetría es lo que este script investiga.
#
# LA HIPÓTESIS
# El outcome de 2023 es el crecimiento del costo laboral 2022->2023. Ese
# intervalo arranca en 2022, un año que tuvo su propio aumento del mínimo
# (+10,07% nominal, -2,70% real -- ver 11_placebo_rodante_real.R). El outcome
# de 2024 (2023->2024) no arranca en un año así. Si el aumento de 2022 mete
# ruido en la BASE del outcome de 2023 (no en la exposición, que ya está fija
# en 2019 y es limpia), eso explicaría por qué tres medidas se caen ahí y no
# en 2024.
#
# LA PRUEBA
# Con exposición fija en 2019 (celda limpia, sin tocar), variar la VENTANA del
# outcome: saltar el año base contaminado (2021->2023 en vez de 2022->2023) y
# ver si las medidas que se caían aparecen. Se repite lo mismo para 2024 (que
# no debería cambiar mucho, porque su año base 2023 no tiene el mismo
# problema) y para un año de aumento real NEGATIVO (2015) como contraste de
# falsación: si alargar la ventana también produce un efecto positivo ahí,
# donde no debería haber nada, el hallazgo es espurio -- crece con la ventana,
# no con el choque.
#
# QUÉ NO CAMBIA: la construcción de las cinco medidas (construir_medidas) y la
# función de estimación (salto) son las mismas de 13_celda_limpia_corregida.R,
# reutilizadas sin tocar su lógica. Lo único que varía en este script es qué
# año se usa como BASE del outcome (anio_base en salto()); la exposición queda
# fija en el año que corresponde a cada caso (2019 para 2023 y 2024, 2011 para
# el contraste de 2015 -- el mismo rezago de 4 años que usa la celda limpia).
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  4. RESULTADOS/Outcome_alternativo/
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

CARPETA <- file.path("4. RESULTADOS", "Outcome_alternativo")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 50) {
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

PALETA <- c(`Bite (Kaitz de obreros)` = "#C00000",
            `Golpe C (3 categorías)`  = "#1F4E79",
            `Golpe A (ponderada)`     = "#2E8B57",
            `Golpe costo`             = "#E69F00",
            `Exposure (% obreros)`    = "#7B68EE")


# ==============================================================================
# 1. DATOS Y CONSTRUCCIÓN DE LAS MEDIDAS (igual que 13_celda_limpia_corregida.R)
# ==============================================================================
titulo("1. DATOS")

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
) %>%
  mutate(
    aumento_nominal_pct = 100 * (valor / lag(valor) - 1),
    aumento_real_pct = 100 * ((1 + aumento_nominal_pct / 100) / (1 + ipc_pct / 100) - 1)
  )

RAZON_COSTO_MINIMO <- 1.531

base <- panel %>%
  mutate(
    empleo = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo, NA_real_),
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
    w_obrero_sueldos = ifelse(obreros_permanentes > 0 &
                                sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 /
                                obreros_permanentes / 12 * 1000, NA_real_),
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
    log_costo = log(ifelse(costo_trabajador > 0, costo_trabajador, NA_real_))
  )

# CORRECCIÓN ENCONTRADA AL CORRER ESTE SCRIPT (no estaba en 13_celda_limpia_
# corregida.R, ni en 12, ni en 10 -- se hereda de ahí sin que nadie lo hubiera
# notado, porque esos scripts solo usan ventanas de outcome de 1 año): la
# construcción original de sector_2022/depto_2022/tamano_2022 era
# `factor(CIIU4)` etc. aplicada a TODO el panel, es decir, recalculada con el
# CIIU4/DPTO/tamano_empresa de CADA FILA en su propio año -- a pesar del
# nombre, no estaba fijada en 2022. Para ventanas de outcome de 1 año esto casi
# nunca se nota (pocas firmas cambian de clasificación en un año). Para
# ventanas de 2-4 años, pivot_wider() usa esas columnas como parte del id, y si
# la clasificación de una firma difiere entre el año base y el año del choque,
# esa firma pierde ambas observaciones en vez de una: en la ventana 2013->2015
# el efecto fue total (0 filas de 9.076 posibles); en 2019->2023, parcial (se
# perdieron ~1.200 de ~7.150 filas posibles, comparado contra el join con
# controles genuinamente fijos). Es una pérdida de muestra NO aleatoria --
# depende de qué firmas cambiaron de sector/departamento/tamaño entre dos
# años, que puede estar correlacionado con la propia exposición al choque.
#
# Se corrige fijando los controles en 2022 de verdad, con un join, como dice
# el nombre de la variable y como hacen los demás scripts del pipeline real
# (03_primer_eslabon_medidas.R, 04_decision_medida.R, etc.).
controles_2022 <- base %>%
  filter(ANIO == 2022) %>%
  distinct(NORDEMP, .keep_all = TRUE) %>%
  transmute(NORDEMP,
            sector_2022 = factor(CIIU4),
            depto_2022 = factor(DPTO),
            tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

cat("Firmas con controles fijos en 2022:", nrow(controles_2022), "de",
    n_distinct(base$NORDEMP), "firmas totales en el panel.\n")

base <- base %>% left_join(controles_2022, by = "NORDEMP")

cat("Panel:", nrow(base), "filas |", n_distinct(base$NORDEMP), "firmas\n")

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

MEDIDAS <- c(bite = "Bite (Kaitz de obreros)",
             golpe_c = "Golpe C (3 categorías)",
             golpe_a = "Golpe A (ponderada)",
             golpe_costo = "Golpe costo",
             exposure = "Exposure (% obreros)")


# ==============================================================================
# 2. FUNCIÓN DE ESTIMACIÓN (igual que 13_celda_limpia_corregida.R, con un
#    arreglo: `clave` ahí quedaba con la etiqueta bonita, no la clave corta,
#    porque tibble() evaluaba `medida = MEDIDAS[[medida]]` ANTES de
#    `clave = medida`, y para ese punto `medida` ya era la columna reasignada.
#    Aquí se invierte el orden para que `clave` sí quede con la clave corta.)
# ==============================================================================
titulo("2. ESPECIFICACIÓN")

cat("Outcome: crecimiento del costo laboral, con VENTANA variable (no fija en\n",
    "año_choque - 1). Exposición: fija en el año que corresponde a cada caso.\n",
    "Controles: sector (CIIU4), departamento y tamaño. Errores robustos.\n")

salto <- function(medida, anio_exposicion, anio_base, anio_choque,
                  outcome = "log_costo") {

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
    ventana_anios = anio_choque - anio_base,
    efecto_pct = 100 * f[["Estimate"]],
    ee_pct = 100 * f[["Std. Error"]],
    p_valor = f[["Pr(>|t|)"]],
    significancia = estrellas(f[["Pr(>|t|)"]]),
    firmas = nobs(modelo)
  )
}


# ==============================================================================
# 3. TRES CASOS: 2023, 2024, Y EL CONTRASTE DE FALSACIÓN DE 2015
# ==============================================================================
titulo("3. LAS TRES VENTANAS, PARA CADA CASO")

# Ventanas equivalentes en las tres pruebas: usual (año inmediatamente
# anterior al choque), salta-uno (dos años antes, saltando el año
# inmediatamente anterior) y larga (el mismo año de la exposición -- la
# ventana más larga posible con esta exposición, que además de esquivar el año
# contaminado incluye pandemia y otros aumentos: no es gratis, hay que decirlo).
casos <- list(
  "2023" = list(
    anio_choque = 2023, anio_exposicion = 2019,
    ventanas = c("2022->2023 (usual)" = 2022,
                 "2021->2023 (salta 2022)" = 2021,
                 "2019->2023 (larga, = año exposición)" = 2019)
  ),
  "2024" = list(
    anio_choque = 2024, anio_exposicion = 2019,
    ventanas = c("2023->2024 (usual)" = 2023,
                 "2022->2024 (2 años)" = 2022,
                 "2021->2024 (larga)" = 2021)
  ),
  "2015 (falsación, aumento real -2,03%)" = list(
    anio_choque = 2015, anio_exposicion = 2011,
    ventanas = c("2014->2015 (usual)" = 2014,
                 "2013->2015 (salta 2014)" = 2013,
                 "2011->2015 (larga, = año exposición)" = 2011)
  )
)

resultado <- bind_rows(lapply(names(casos), function(nombre_caso) {
  caso <- casos[[nombre_caso]]
  bind_rows(lapply(names(caso$ventanas), function(etiqueta_ventana) {
    anio_base_v <- caso$ventanas[[etiqueta_ventana]]
    bind_rows(lapply(names(MEDIDAS), function(m) {
      r <- salto(m, caso$anio_exposicion, anio_base_v, caso$anio_choque, "log_costo")
      if (is.null(r)) return(NULL)
      mutate(r, caso = nombre_caso, ventana = etiqueta_ventana)
    }))
  }))
})) %>%
  select(caso, medida, ventana, anio_exposicion, anio_base, anio_choque,
         ventana_anios, efecto_pct, ee_pct, p_valor, significancia, firmas)

ver(resultado, filas = 60)
guardar_tabla(resultado, "T01_outcome_alternativo_completo",
              "Tabla 1. Cinco medidas x tres ventanas x tres casos (2023, 2024, falsación 2015)")

cat("\nCOBERTURA: cuántas firmas entran en cada combinación. Las ventanas largas\n",
    "pierden observaciones (menos firmas sobreviven varios años seguidas), así\n",
    "que la comparabilidad entre ventanas de la misma medida no es perfecta --\n",
    "parte de cualquier diferencia puede venir de la muestra, no solo de la\n",
    "ventana. Se reporta 'firmas' en la tabla 1 para poder juzgar esto.\n")

cobertura <- resultado %>%
  select(caso, medida, ventana, firmas) %>%
  pivot_wider(names_from = ventana, values_from = firmas)

ver(cobertura, filas = 20)
guardar_tabla(cobertura, "T02_cobertura_por_ventana",
              "Tabla 2. Firmas por combinación de caso, medida y ventana", decimales = 0)


# ==============================================================================
# 4. TABLA LADO A LADO: ¿APARECEN LAS TRES MEDIDAS AL SALTAR 2022?
# ==============================================================================
titulo("4. ¿APARECEN GOLPE A, GOLPE C Y GOLPE COSTO AL SALTAR EL AÑO CONTAMINADO?")

lado_a_lado <- resultado %>%
  select(caso, medida, ventana, efecto_pct, p_valor, significancia) %>%
  pivot_wider(names_from = ventana, values_from = c(efecto_pct, p_valor, significancia))

ver(lado_a_lado, filas = 20)
guardar_tabla(lado_a_lado, "T03_lado_a_lado_por_caso",
              "Tabla 3. Las tres ventanas lado a lado, por caso y medida", decimales = 3)

g1 <- ggplot(resultado, aes(x = ventana_anios, y = efecto_pct, color = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ caso, ncol = 1, scales = "free_x") +
  scale_color_manual(values = PALETA) +
  labs(title = "El diferencial según el largo de la ventana del outcome",
       subtitle = "Exposición fija (2019 para 2023/2024, 2011 para el contraste de 2015). Eje x: años que cubre la ventana del outcome.",
       x = "Largo de la ventana del outcome (años)", y = "Diferencial (%)", color = NULL,
       caption = "Si el hallazgo depende del choque, el patrón de 2023/2024 no debería repetirse en el contraste de 2015 (falsación).") +
  tema_tesis
guardar_grafico(g1, "G01_outcome_por_ventana", ancho = 10, alto = 12)


# ==============================================================================
# 5. LA REGLA -- FIJADA ANTES DE VER EL RESULTADO
# ==============================================================================
titulo("5. LECTURA -- LA REGLA SE FIJA ANTES DE VER EL RESULTADO")

# Definición operacional de "aparece": positivo Y significativo al 5% en la
# ventana que salta el año contaminado (2021->2023 para 2023; el equivalente,
# 2013->2015, para el contraste de falsación).
UMBRAL_P <- 0.05

aparece_en_2023_salta <- resultado %>%
  filter(caso == "2023", ventana == "2021->2023 (salta 2022)") %>%
  transmute(medida, aparece = !is.na(efecto_pct) & efecto_pct > 0 & p_valor < UMBRAL_P)

aparece_en_2015_salta <- resultado %>%
  filter(caso == "2015 (falsación, aumento real -2,03%)", ventana == "2013->2015 (salta 2014)") %>%
  transmute(medida, aparece_falsa = !is.na(efecto_pct) & efecto_pct > 0 & p_valor < UMBRAL_P)

tres_medidas <- c("Golpe A (ponderada)", "Golpe C (3 categorías)", "Golpe costo")

veredicto <- aparece_en_2023_salta %>%
  left_join(aparece_en_2015_salta, by = "medida") %>%
  filter(medida %in% tres_medidas)

ver(veredicto)
guardar_tabla(veredicto, "T04_veredicto_aparicion",
              "Tabla 4. ¿Aparece cada medida al saltar el año contaminado, en 2023 y en la falsación de 2015?",
              decimales = 0)

n_aparecen_2023 <- sum(veredicto$aparece, na.rm = TRUE)
n_aparecen_falsa <- sum(veredicto$aparece_falsa, na.rm = TRUE)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("LECTURA ACTIVADA:\n\n")

if (n_aparecen_2023 == length(tres_medidas) && n_aparecen_falsa == 0) {
  cat("  SE CONFIRMA LA HIPÓTESIS: Golpe A, Golpe C y Golpe costo aparecen (positivos\n",
      "  y significativos al 5%) al saltar el año base contaminado (2021->2023), y\n",
      "  el contraste de falsación en 2015 sigue plano con la misma ventana\n",
      "  equivalente. El outcome de 2023 estaba contaminado por el aumento de\n",
      "  2022, no por un problema de las medidas. Con la ventana corregida hay\n",
      "  primera etapa válida en los dos años de aumento real alto. Este es el\n",
      "  resultado que la tesis necesita.\n")
} else if (n_aparecen_2023 < length(tres_medidas)) {
  cat("  NO SE CONFIRMA: de las tres medidas (Golpe A, Golpe C, Golpe costo), solo",
      n_aparecen_2023, "de", length(tres_medidas),
      "aparece(n) al saltar el año contaminado en 2023.\n",
      "  La asimetría entre 2023 y 2024 tiene otra causa, sin identificar en esta\n",
      "  prueba. El resultado defendible se reduce a Bite, que se sostiene en los\n",
      "  dos años con la ventana original (0,81% y 0,85%, ambos p<0,05).\n")
} else {
  cat("  HALLAZGO ESPURIO: Golpe A, Golpe C y Golpe costo aparecen al saltar el año\n",
      "  contaminado en 2023, PERO el mismo patrón aparece en el contraste de\n",
      "  falsación de 2015 (aumento real negativo, no debería haber efecto). Lo que\n",
      "  se está midiendo crece con el LARGO de la ventana, no con el tamaño del\n",
      "  choque. Hay que reportar esto así y descartar la lectura optimista: el\n",
      "  único resultado que sigue en pie es Bite con la ventana original, y debe\n",
      "  revisarse si Bite también es sensible a este mismo problema con otras\n",
      "  ventanas.\n")
}
cat(strrep("=", 78), "\n", sep = "")

cat("\nDetalle de la tabla 4 (aparece = positivo y p<0,05):\n")
print(as.data.frame(veredicto), row.names = FALSE)


if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")
