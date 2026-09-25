# ==============================================================================
# 06_decision_medida.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# Cierra la decisión de qué medida de exposición usa la tesis y produce la
# estimación creíble del primer eslabón.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/exposicion_alternativa_2022.rds
#           1. DATOS/panel_analitico_firma_eam.rds
# Salidas:  4. RESULTADOS/Decision_medida/
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulos:    2 (Medición) y 6 (Validaciones adicionales y amenazas)
# Pregunta:     ¿Qué medida de exposición usa la tesis, y con qué evidencia se
#               eligió?
# Cifra clave:  Bite gana la celda limpia con +1,15% por DE (p=0,008) --
#               sección 3, tabla T01_celda_limpia.
# Depende de:   01_exposicion_alternativa.R,
#               05_primer_eslabon_medidas.R (mismo problema de
#               sesgo de división, misma solución de celda limpia)
#
# CAPÍTULO 6 -- NO SUAVIZAR: la sensibilidad al año base de exposición es una
# amenaza reconocida, no un detalle técnico. Con exposición 2022 el
# coeficiente estándar da +4,03% por DE; con exposición 2019 (celda limpia)
# da -1,95% contra el outcome que arrastra base 2019 (sección 4, fila "Exp
# 2022 / Crec 2019-23" de la matriz, y comparación de secciones 3 vs. 7). Un
# coeficiente que cambia de signo según el año en que se mide la exposición
# es exactamente el tipo de resultado que va al capítulo 6, declarado como
# tal, no minimizado en el texto.
#
# YA VERIFICADO EN ESTE SCRIPT (no requirió corrección): la "prueba del
# outcome rebasado" que una versión intermedia proponía como decisiva está
# aquí correctamente declarada INVÁLIDA (ver "EL PROBLEMA Y CÓMO SE
# RESUELVE" abajo y la sección 4) -- el outcome 2021-2023 incluye el aumento
# del mínimo de 2022, anterior al año en que se mide la exposición 2022, lo
# que deja la exposición post-tratamiento. Este script ya tiene la versión
# final correcta.
# ------------------------------------------------------------------------------


# ==============================================================================
# EL PROBLEMA Y CÓMO SE RESUELVE
# ==============================================================================
#
# La especificación estándar del primer eslabón es
#
#     log(costo 2023) - log(costo 2022)  ~  exposición medida en 2022
#
# y tiene un problema aritmético: el costo de 2022 aparece a los dos lados. Está
# en el denominador de la exposición (una firma que paga poco sale muy expuesta)
# y en la base del outcome (un costo bajo en 2022 hace grande el crecimiento
# hacia 2023). Si una firma reportó por azar un costo bajo en 2022, sale muy
# expuesta Y con crecimiento alto, sin que haya pasado nada económico. Eso es
# SESGO DE DIVISIÓN, y infla el coeficiente.
#
# Para romperlo hay que separar en el tiempo la exposición del outcome. Pero hay
# DOS formas de hacerlo y solo una funciona:
#
#   (i) Mover el outcome hacia atrás (2021->2023, 2019->2023) dejando la
#       exposición en 2022. NO SIRVE, y el script anterior se equivocó al
#       proponerlo como prueba principal: el outcome 2021->2023 contiene el
#       aumento del salario mínimo de 2022 (10,07% nominal), que es anterior al
#       año en que se mide la exposición. La exposición queda POST-TRATAMIENTO
#       respecto de parte del período del outcome: una firma muy golpeada en
#       2022 vio subir su costo ese año, así que en 2022 aparece con costo alto,
#       es decir con exposición BAJA. Eso sesga el coeficiente hacia abajo y
#       puede volverlo negativo. Con base 2019 el problema empeora, porque el
#       outcome cubre además la pandemia.
#
#   (ii) Mover la EXPOSICIÓN hacia atrás (medirla en 2019) dejando el outcome en
#       2022->2023. SÍ SIRVE. Los dos objetos no comparten ningún año, así que
#       no hay traslape aritmético; y la exposición es anterior a todo el
#       período del outcome, así que no es post-tratamiento.
#
# La combinación (ii) es la CELDA LIMPIA y es la especificación principal de
# este script. El resto se reporta como diagnóstico.
#
# Lo que (ii) no resuelve, y hay que declarar en la tesis: la exposición de 2019
# mide con error la exposición relevante en 2023 (atenuación), y entre 2019 y
# 2022 hubo una pandemia que pudo reorganizar la estructura salarial de las
# firmas. Por eso el coeficiente de la celda limpia es una COTA INFERIOR del
# efecto verdadero, no una medición exacta.
#
# ADVERTENCIA SOBRE EL PLACEBO 2018-2019 DEL SCRIPT 03: daba coeficiente
# negativo y significativo en las CINCO medidas, incluida Exposure, que no usa
# salarios. Un placebo que da lo mismo para todas no está midiendo propiedades
# de las medidas. La razón es aritmética: exposición alta significa costo 2022
# bajo, que por persistencia implica costo 2019 bajo, que dado 2018 implica
# crecimiento 2018-2019 bajo. El signo negativo está casi garantizado por
# construcción. Ese placebo NO es informativo y no debe citarse como evidencia.
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

CARPETA <- file.path("4. RESULTADOS", "Decision_medida")
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
COLOR_GRIS <- "#808080"

SMLV_2023_ANUAL_MILES <- 1160000 * 12 / 1000
LIMITE_GOLPE <- 1.3


# ==============================================================================
# 1. BITE Y EXPOSURE CON BASE 2019
# ==============================================================================
titulo("1. CONSTRUCCIÓN DE BITE Y EXPOSURE CON BASE 2019")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

# Replicamos EXACTAMENTE la definición original de Bite. El Paso 0.1 confirmó
# que usa C3R2C1 (sueldos del personal permanente, obreros) sobre
# obreros_permanentes. Bite NO incluye prestaciones; Golpe C sí. Si aquí
# agregáramos prestaciones estaríamos construyendo otra medida.
#
# El referente (mínimo de 2023) es el mismo en las dos bases: es constante y no
# cambia el ordenamiento entre firmas. Lo que cambia es el año del salario.

construir_base <- function(anio_base) {
  panel %>%
    filter(ANIO == anio_base) %>%
    transmute(
      NORDEMP,
      salario_obrero = ifelse(obreros_permanentes > 0 &
                                sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes,
                              NA_real_),
      bite = ifelse(!is.na(salario_obrero) & salario_obrero > 0,
                    SMLV_2023_ANUAL_MILES / salario_obrero, NA_real_),
      # Exposure no usa salarios: es la única medida inmune al canal mecánico,
      # y sirve de contrafactual limpio de las demás.
      exposure = ifelse(empleo_total_sin_propietarios > 0,
                        obreros_total_ocupado / empleo_total_sin_propietarios,
                        NA_real_)
    )
}

base_2019 <- construir_base(2019) %>%
  rename(bite_2019 = bite, exposure_2019 = exposure,
         salario_obrero_2019 = salario_obrero)

base_2022_replica <- construir_base(2022) %>%
  rename(bite_2022_replica = bite, exposure_2022_replica = exposure,
         salario_obrero_2022 = salario_obrero)

cat("Firmas con bite 2019:", sum(!is.na(base_2019$bite_2019)), "\n")
cat("Firmas con exposure 2019:", sum(!is.na(base_2019$exposure_2019)), "\n")

# --- Verificación de la réplica (BLOQUEANTE) ------------------------------------
viejas <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO))) %>%
  filter(ANIO == 2022) %>%
  select(NORDEMP, any_of(c("Bite2022_obreros", "Exposure2022_obreros")))

verificacion <- base_2022_replica %>%
  left_join(viejas, by = "NORDEMP") %>%
  filter(!is.na(bite_2022_replica), !is.na(Bite2022_obreros)) %>%
  mutate(diferencia_pct = 100 * (bite_2022_replica / Bite2022_obreros - 1))

cor_replica <- cor(verificacion$bite_2022_replica, verificacion$Bite2022_obreros,
                   use = "complete.obs")

cat("\nVERIFICACIÓN DE LA RÉPLICA (Bite 2022):\n")
cat("  Firmas comparadas:", nrow(verificacion), "\n")
cat("  Correlación con el original:", round(cor_replica, 6), "\n")
cat("  Firmas con diferencia mayor a 1%:",
    sum(abs(verificacion$diferencia_pct) > 1, na.rm = TRUE), "\n")

if (cor_replica < 0.99) {
  warning("La réplica de Bite NO reproduce la variable original (correlación ",
          round(cor_replica, 4), "). NO interpretar el resto del script.")
}


# ==============================================================================
# 2. BASE DE TRABAJO
# ==============================================================================
titulo("2. BASE DE TRABAJO")

alternativas <- read_rds(file.path("1. DATOS", "exposicion_alternativa_2022.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP))

outcomes <- panel %>%
  mutate(costo_trabajador = ifelse(
    empleo_total_sin_propietarios > 0 & costos_totales_personal_total_c3r10c3 > 0,
    costos_totales_personal_total_c3r10c3 / empleo_total_sin_propietarios,
    NA_real_)) %>%
  filter(ANIO %in% c(2019, 2021, 2022, 2023)) %>%
  select(NORDEMP, ANIO, costo_trabajador) %>%
  pivot_wider(names_from = ANIO, values_from = costo_trabajador,
              names_prefix = "costo_") %>%
  mutate(
    crecimiento_23_22 = log(costo_2023) - log(costo_2022),
    crecimiento_23_21 = log(costo_2023) - log(costo_2021),
    crecimiento_23_19 = log(costo_2023) - log(costo_2019)
  )

datos <- alternativas %>%
  left_join(viejas, by = "NORDEMP") %>%
  left_join(base_2019, by = "NORDEMP") %>%
  left_join(outcomes, by = "NORDEMP")

# Filtro de plausibilidad: ningún trabajador formal de tiempo completo puede
# costar menos del mínimo, así que un golpe por encima de 1,3 es error de
# reporte. Se vuelven NA, no se recortan.
plausible <- function(x, limite = LIMITE_GOLPE) {
  ifelse(!is.na(x) & x > 0 & x <= limite, x, NA_real_)
}

datos <- datos %>%
  mutate(
    across(any_of(c("golpe_c", "golpe_a", "golpe_costo", "Bite2022_obreros",
                    "golpe_c_2019", "golpe_a_2019", "golpe_costo_2019",
                    "bite_2019")),
           plausible),
    across(any_of(c("Exposure2022_obreros", "exposure_2019")),
           ~ ifelse(!is.na(.x) & .x >= 0 & .x <= 1, .x, NA_real_))
  )

estandarizar <- function(x) x / sd(x, na.rm = TRUE)

PAREJAS <- tribble(
  ~nombre,        ~var_2022,              ~var_2019,
  "Bite",         "Bite2022_obreros",     "bite_2019",
  "Exposure",     "Exposure2022_obreros", "exposure_2019",
  "Golpe C",      "golpe_c",              "golpe_c_2019",
  "Golpe A",      "golpe_a",              "golpe_a_2019",
  "Golpe costo",  "golpe_costo",          "golpe_costo_2019"
)

vars_presentes <- c(PAREJAS$var_2022, PAREJAS$var_2019)
vars_presentes <- vars_presentes[vars_presentes %in% names(datos)]

datos <- datos %>%
  mutate(across(all_of(vars_presentes), estandarizar, .names = "{.col}_de"),
         sector_2022 = factor(CIIU4),
         depto_2022  = factor(DPTO),
         tamano_2022 = factor(tamano_empresa,
                              levels = c("Pequena", "Mediana", "Grande")))

cat("Firmas en la base:", nrow(datos), "\n")
cat("Con outcome 2022->2023:", sum(!is.na(datos$crecimiento_23_22)), "\n")

CONTROLES <- "sector_2022 + depto_2022 + tamano_2022"

estimar <- function(variable, outcome = "crecimiento_23_22", base = datos) {
  v <- paste0(variable, "_de")
  if (!v %in% names(base)) return(NULL)
  modelo <- tryCatch(
    feols(as.formula(paste0(outcome, " ~ ", v, " | ", CONTROLES)),
          data = base, vcov = "hetero"),
    error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  fila <- coeftable(modelo)[v, ]
  tibble(coeficiente = fila[["Estimate"]],
         error_estandar = fila[["Std. Error"]],
         p_valor = fila[["Pr(>|t|)"]],
         observaciones = nobs(modelo))
}

# extrae(): saca un campo de una lista de resultados, devolviendo NA cuando la
# estimación falló. Evita el error de sapply() sobre listas vacías.
extrae <- function(lista, campo) {
  sapply(lista, function(x) if (length(x)) x[[campo]] else NA_real_)
}


# ==============================================================================
# 3. RESULTADO PRINCIPAL: LA CELDA LIMPIA
# ==============================================================================
titulo("3. CELDA LIMPIA: EXPOSICIÓN 2019, OUTCOME 2022->2023")

# Esta es la estimación creíble del primer eslabón, y la que debe ir en la
# tesis como resultado principal de esta sección.
#
# Por qué es limpia:
#   - exposición (2019) y outcome (2022-2023) no comparten ningún año, así que
#     no hay sesgo de división;
#   - la exposición es anterior a todo el período del outcome, así que no es
#     post-tratamiento respecto de ningún aumento del mínimo incluido en él.
#
# Por qué es una COTA INFERIOR y no una medición exacta:
#   - la exposición de 2019 mide con error la exposición relevante en 2023
#     (atenuación, que sesga hacia cero);
#   - entre 2019 y 2022 hubo una pandemia que pudo reorganizar la estructura
#     salarial de las firmas.
# Las dos cosas empujan el coeficiente hacia abajo, no hacia arriba. Así que el
# efecto verdadero es al menos el estimado aquí.

celda_limpia <- PAREJAS %>%
  rowwise() %>%
  mutate(r = list(estimar(var_2019, outcome = "crecimiento_23_22"))) %>%
  ungroup() %>%
  mutate(
    coeficiente = extrae(r, "coeficiente"),
    error_estandar = extrae(r, "error_estandar"),
    p_valor = extrae(r, "p_valor"),
    observaciones = extrae(r, "observaciones"),
    significancia = estrellas(p_valor),
    efecto_pct = 100 * coeficiente,
    ic95_inferior_pct = 100 * (coeficiente - 1.96 * error_estandar),
    ic95_superior_pct = 100 * (coeficiente + 1.96 * error_estandar)
  ) %>%
  select(nombre, efecto_pct, ic95_inferior_pct, ic95_superior_pct,
         p_valor, significancia, observaciones) %>%
  arrange(desc(efecto_pct))

ver(celda_limpia)
guardar_tabla(celda_limpia, "T01_celda_limpia",
              "Tabla 1. Primer eslabón con exposición de 2019 y crecimiento 2022-2023 (especificación principal)",
              decimales = 3)

cat("\nESTA ES LA CIFRA QUE VA EN LA TESIS como magnitud del primer eslabón.\n",
    "La especificación estándar (exposición 2022) da un número mucho mayor,\n",
    "pero contaminado por sesgo de división. Hay que reportar las dos y\n",
    "explicar la diferencia.\n")

grafico_limpia <- ggplot(celda_limpia,
                         aes(x = reorder(nombre, efecto_pct), y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = ic95_inferior_pct, ymax = ic95_superior_pct),
                  color = COLOR_BAJA, size = 0.8) +
  coord_flip() +
  labs(title = "Primer eslabón sin contaminación aritmética",
       subtitle = "Exposición medida en 2019, crecimiento del costo laboral 2022-2023",
       x = NULL, y = "Efecto por DE de exposición (%)",
       caption = "Controles: sector (CIIU4), departamento y tamaño, fijados en 2022. Errores robustos. IC al 95%.") +
  tema_tesis
guardar_grafico(grafico_limpia, "G01_celda_limpia")


# ==============================================================================
# 4. DIAGNÓSTICO: LA MATRIZ COMPLETA DE COMBINACIONES
# ==============================================================================
titulo("4. MATRIZ DE COMBINACIONES EXPOSICIÓN x OUTCOME")

# Estimamos todas las combinaciones para dejar visible por qué la celda de la
# sección 3 es la única interpretable. Cada celda tiene su problema, salvo una.

COMBINACIONES <- tribble(
  ~base_exposicion, ~outcome,             ~etiqueta,                          ~problema,
  "2022",           "crecimiento_23_22",  "Exp 2022 / Crec 2022-23",          "Sesgo de división: 2022 en los dos lados",
  "2022",           "crecimiento_23_21",  "Exp 2022 / Crec 2021-23",          "Exposición post-tratamiento: el outcome incluye el aumento de 2022",
  "2022",           "crecimiento_23_19",  "Exp 2022 / Crec 2019-23",          "Post-tratamiento + pandemia dentro del outcome",
  "2019",           "crecimiento_23_22",  "Exp 2019 / Crec 2022-23 (LIMPIA)", "Solo atenuación: sesga hacia cero, es cota inferior",
  "2019",           "crecimiento_23_21",  "Exp 2019 / Crec 2021-23",          "Limpia pero el outcome mezcla dos aumentos del mínimo",
  "2019",           "crecimiento_23_19",  "Exp 2019 / Crec 2019-23",          "Sesgo de división al revés: 2019 en los dos lados"
)

matriz <- PAREJAS %>%
  tidyr::crossing(COMBINACIONES) %>%
  rowwise() %>%
  mutate(
    variable = if (base_exposicion == "2022") var_2022 else var_2019,
    r = list(estimar(variable, outcome = outcome))
  ) %>%
  ungroup() %>%
  mutate(
    coeficiente = extrae(r, "coeficiente"),
    p_valor = extrae(r, "p_valor"),
    observaciones = extrae(r, "observaciones"),
    significancia = estrellas(p_valor),
    efecto_pct = 100 * coeficiente
  ) %>%
  select(medida = nombre, especificacion = etiqueta, efecto_pct, p_valor,
         significancia, observaciones, problema)

ver(matriz, filas = 30)
guardar_tabla(matriz, "T02_matriz_combinaciones",
              "Tabla 2. Todas las combinaciones de año base de exposición y de outcome, con su problema",
              decimales = 3)

cat("
POR QUÉ LAS OTRAS CELDAS NO SIRVEN:

  Exp 2022 / Crec 2022-23: el costo de 2022 está en el denominador de la
  exposición y en la base del outcome. Coeficiente inflado.

  Exp 2022 / Crec 2021-23 y 2019-23: el outcome incluye el aumento del salario
  mínimo de 2022 (10,07% nominal), anterior al año en que se mide la exposición.
  Una firma golpeada en 2022 vio subir su costo ese año, así que aparece con
  costo alto en 2022, es decir con exposición BAJA. La exposición es
  post-tratamiento y el coeficiente se sesga hacia abajo, hasta volverse
  negativo. Esto invalida la prueba de 'outcome rebasado' que se propuso en una
  versión anterior de este script.

  Exp 2019 / Crec 2019-23: mismo sesgo de división que la primera, con 2019 en
  los dos lados.

  Exp 2019 / Crec 2022-23: ninguno de los dos problemas. Es la celda limpia.
")

grafico_matriz <- matriz %>%
  filter(!is.na(efecto_pct)) %>%
  mutate(es_limpia = grepl("LIMPIA", especificacion)) %>%
  ggplot(aes(x = especificacion, y = efecto_pct, fill = es_limpia)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_col() +
  facet_wrap(~ medida, ncol = 2) +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = COLOR_BAJA, `FALSE` = COLOR_GRIS),
                    labels = c("Con problema de identificación", "Celda limpia")) +
  labs(title = "El primer eslabón según qué años se usan",
       subtitle = "Solo la combinación exposición 2019 / crecimiento 2022-2023 es interpretable",
       x = NULL, y = "Efecto (%)", fill = NULL,
       caption = "Ver la tabla 2 para el problema específico de cada especificación.") +
  tema_tesis +
  theme(axis.text.y = element_text(size = 8))
guardar_grafico(grafico_matriz, "G02_matriz_combinaciones", alto = 8)


# ==============================================================================
# 5. ESTABILIDAD DE CADA MEDIDA ENTRE AÑOS BASE
# ==============================================================================
titulo("5. ESTABILIDAD DE LA EXPOSICIÓN ENTRE 2019 Y 2022")

# Importa por dos razones. Primero, una medida que cambia mucho de un año a otro
# no está captando una característica estructural de la firma. Segundo, es el
# determinante de cuánta atenuación tiene la celda limpia: mientras menos
# estable la medida, más sesgado hacia cero está su coeficiente allí.
#
# NO lo usamos como benchmark para "corregir" el coeficiente: ese cálculo
# exigiría suponer cuál de las dos mediciones es la verdadera y cuál la ruidosa,
# y no tenemos base para ese supuesto.

estabilidad <- PAREJAS %>%
  rowwise() %>%
  mutate(
    disponible = var_2019 %in% names(datos) & var_2022 %in% names(datos),
    n_ambos = if (disponible) sum(!is.na(datos[[var_2022]]) & !is.na(datos[[var_2019]])) else NA_integer_,
    pearson = if (disponible) cor(datos[[var_2022]], datos[[var_2019]],
                                  use = "complete.obs", method = "pearson") else NA_real_,
    spearman = if (disponible) cor(datos[[var_2022]], datos[[var_2019]],
                                   use = "complete.obs", method = "spearman") else NA_real_
  ) %>%
  ungroup() %>%
  select(nombre, n_ambos, pearson, spearman) %>%
  arrange(desc(pearson))

ver(estabilidad)
guardar_tabla(estabilidad, "T03_estabilidad_entre_bases",
              "Tabla 3. Correlación de cada medida entre 2019 y 2022")

cat("\nLECTURA: una estabilidad baja indica más atenuación en la celda limpia,\n",
    "y también menos firmas con las dos mediciones disponibles. Las dos cosas\n",
    "restan potencia. Si una medida gana la celda limpia PESE a tener la peor\n",
    "estabilidad, su ventaja es más fuerte de lo que sugiere el coeficiente.\n")


# ==============================================================================
# 6. EFECTO POR TRAMOS DE EXPOSICIÓN
# ==============================================================================
titulo("6. EFECTO POR TRAMOS DE EXPOSICIÓN")

# 05_primer_eslabon_medidas.R sugirió, con medianas SIN controles,
# que el efecto estaba concentrado en el quintil más expuesto. Con controles
# esa lectura no se sostiene: la relación resulta monotónica creciente. Lo
# dejamos aquí para documentar la corrección, con la exposición de 2019
# (celda limpia) además de la de 2022.

estimar_tramos <- function(variable, etiqueta, outcome = "crecimiento_23_22") {
  if (!variable %in% names(datos)) return(NULL)
  base <- datos %>%
    filter(!is.na(.data[[variable]]), !is.na(.data[[outcome]])) %>%
    mutate(quintil = factor(ntile(.data[[variable]], 5)))
  modelo <- tryCatch(
    feols(as.formula(paste0(outcome, " ~ i(quintil, ref = 1) | ", CONTROLES)),
          data = base, vcov = "hetero"),
    error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  coefs <- as.data.frame(coeftable(modelo))
  tibble(
    medida = etiqueta,
    quintil = as.integer(gsub("quintil::", "", rownames(coefs))),
    coeficiente = coefs[["Estimate"]],
    error_estandar = coefs[["Std. Error"]],
    p_valor = coefs[["Pr(>|t|)"]],
    significancia = estrellas(coefs[["Pr(>|t|)"]])
  ) %>%
    bind_rows(tibble(medida = etiqueta, quintil = 1L, coeficiente = 0,
                     error_estandar = 0, p_valor = NA_real_, significancia = "ref")) %>%
    arrange(quintil)
}

tramos <- bind_rows(
  estimar_tramos("Bite2022_obreros", "Bite (exp. 2022)"),
  estimar_tramos("golpe_c", "Golpe C (exp. 2022)"),
  estimar_tramos("bite_2019", "Bite (exp. 2019, limpia)"),
  estimar_tramos("golpe_c_2019", "Golpe C (exp. 2019, limpia)")
)

ver(tramos, filas = 25)
guardar_tabla(tramos, "T04_efecto_por_tramos",
              "Tabla 4. Efecto por quintil de exposición, frente al quintil 1")

grafico_tramos <- ggplot(tramos, aes(x = quintil, y = 100 * coeficiente,
                                     color = medida, group = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.8) +
  geom_pointrange(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                      ymax = 100 * (coeficiente + 1.96 * error_estandar)),
                  position = position_dodge(width = 0.3)) +
  labs(title = "El aumento del costo laboral por tramo de exposición",
       subtitle = "Diferencia frente al quintil menos expuesto, con controles",
       x = "Quintil de exposición (1 = menos expuesta)",
       y = "Diferencia en el crecimiento (%)", color = NULL,
       caption = "Con controles la relación es monotónica. Las medianas crudas de 05_primer_eslabon_medidas.R sugerían lo contrario.") +
  tema_tesis
guardar_grafico(grafico_tramos, "G03_efecto_por_tramos")


# ==============================================================================
# 7. DECISIÓN DE MEDIDA
# ==============================================================================
titulo("7. DECISIÓN DE MEDIDA")

# Criterios, en orden de peso:
#   1. Coeficiente positivo y SIGNIFICATIVO en la celda limpia (sección 3).
#      Este es el criterio decisivo: es la única especificación interpretable.
#   2. Que también sea positivo y significativo en la especificación estándar.
#      Consistencia entre las dos, no evidencia independiente.
#   3. Cobertura de muestra y estabilidad de la medida.
#
# NOTA SOBRE UN BUG DE LA VERSIÓN ANTERIOR: la tabla de decisión verificaba el
# signo y la razón de coeficientes pero NO la significancia. Por eso Exposure
# aparecía cumpliendo un criterio con un coeficiente que era ruido (razón de
# 3,68 calculada sobre un coeficiente de 0,19% con p=0,70). Aquí todos los
# criterios exigen significancia explícita.

estandar <- PAREJAS %>%
  rowwise() %>%
  mutate(r = list(estimar(var_2022, outcome = "crecimiento_23_22"))) %>%
  ungroup() %>%
  mutate(coef_estandar = extrae(r, "coeficiente"),
         p_estandar = extrae(r, "p_valor"),
         n_estandar = extrae(r, "observaciones")) %>%
  select(nombre, coef_estandar, p_estandar, n_estandar)

limpia_para_decision <- PAREJAS %>%
  rowwise() %>%
  mutate(r = list(estimar(var_2019, outcome = "crecimiento_23_22"))) %>%
  ungroup() %>%
  mutate(coef_limpia = extrae(r, "coeficiente"),
         p_limpia = extrae(r, "p_valor"),
         n_limpia = extrae(r, "observaciones")) %>%
  select(nombre, coef_limpia, p_limpia, n_limpia)

decision <- limpia_para_decision %>%
  left_join(estandar, by = "nombre") %>%
  left_join(select(estabilidad, nombre, pearson, n_ambos), by = "nombre") %>%
  transmute(
    medida = nombre,
    # Criterio 1: decisivo
    crit_1_limpia_positiva_y_sig = ifelse(!is.na(coef_limpia) & coef_limpia > 0 &
                                            !is.na(p_limpia) & p_limpia < 0.05,
                                          "Sí", "No"),
    efecto_limpia_pct = round(100 * coef_limpia, 3),
    p_limpia = round(p_limpia, 4),
    # Criterio 2: consistencia
    crit_2_estandar_positiva_y_sig = ifelse(!is.na(coef_estandar) & coef_estandar > 0 &
                                              !is.na(p_estandar) & p_estandar < 0.05,
                                            "Sí", "No"),
    efecto_estandar_pct = round(100 * coef_estandar, 3),
    # Criterio 3: calidad de la medida
    estabilidad_pearson = round(pearson, 3),
    n_celda_limpia = n_limpia,
    # Cuánto del coeficiente estándar sobrevive en la celda limpia
    razon_limpia_sobre_estandar = round(coef_limpia / coef_estandar, 3)
  ) %>%
  arrange(desc(efecto_limpia_pct))

ver(decision)
guardar_tabla(decision, "T05_decision",
              "Tabla 5. Decisión de medida según la celda limpia", decimales = 3)

# Identificamos la ganadora de forma explícita, para que quede en el log
ganadoras <- decision %>% filter(crit_1_limpia_positiva_y_sig == "Sí")

cat("\nMEDIDAS QUE PASAN EL CRITERIO DECISIVO:",
    if (nrow(ganadoras) > 0) paste(ganadoras$medida, collapse = ", ") else "NINGUNA", "\n")

if (nrow(ganadoras) > 0) {
  cat("GANADORA (mayor efecto en la celda limpia):", ganadoras$medida[1],
      "con", ganadoras$efecto_limpia_pct[1], "% por DE (p =", ganadoras$p_limpia[1], ")\n")
} else {
  cat("\nNINGUNA MEDIDA PASA. Eso NO es un problema de robustez sino de diseño,\n",
      "y está registrado como riesgo desde septiembre. Habría que construir la\n",
      "exposición desde la distribución salarial de la firma. Reportarlo así,\n",
      "sin maquillaje: una primera etapa que falla es un resultado.\n")
}

cat("
QUÉ ESCRIBIR EN LA TESIS:

  1. MAGNITUD DEL PRIMER ESLABÓN. Reportar la cifra de la celda limpia como
     estimación principal, y la de la especificación estándar como referencia,
     explicando que la diferencia es sesgo de división. Declarar que la celda
     limpia es una cota inferior por atenuación.

  2. EXPOSURE NO PREDICE EL COSTO LABORAL. Es un resultado, no una omisión:
     valida con evidencia la decisión pre-comiteada de descartarla y muestra que
     la composición ocupacional por sí sola no mide exposición al mínimo.

  3. LA RELACIÓN ES MONOTÓNICA con controles. Las medianas crudas de
     05_primer_eslabon_medidas.R sugerían concentración en el
     quintil 5; con controles no se sostiene.
     Corregir esa afirmación donde aparezca.

  4. EL PLACEBO 2018-2019 NO ES INFORMATIVO. Ver el encabezado de este script.
     No citarlo como evidencia en ninguna dirección.

  5. ELASTICIDAD IMPLÍCITA. Con un primer eslabón menor, el mismo efecto nulo
     sobre empleo implica una elasticidad mayor en valor absoluto y un intervalo
     mucho más ancho. Recalcularla y no presentar el nulo como evidencia de
     ausencia de efecto: es falta de precisión, no precisión sobre el cero.

  6. LIMITACIÓN DE LA MEDIDA GANADORA. Si es Bite, declarar que es la menos
     estable entre años base (ver tabla 3) y la de menor cobertura en la celda
     limpia. Que gane pese a eso es informativo, pero la limitación va escrita.
")

save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
cat("\nCompendio guardado con", length(compendio), "tablas.\n")

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

