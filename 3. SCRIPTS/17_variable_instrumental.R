# ==============================================================================
# 17_variable_instrumental.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# VARIABLE INSTRUMENTAL PARA EL KAITZ: ¿SOBREVIVE EL EFECTO AL QUITAR EL RUIDO
# QUE COMPARTEN LA MEDIDA Y EL OUTCOME?
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/panel_analitico_firma_eam.rds (opcional, solo para
#              verificar que la réplica de Bite coincide con la original)
# Salidas:  4. RESULTADOS/Variable_instrumental/
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulo 6 (amenazas) o 4 (validación), según lo que resulte -- ver la REGLA
# DE DECISIÓN abajo, que se fija ANTES de correr el script.
#
# EL PROBLEMA QUE ATACA
# Bite2022 = SM_2023 / w_2022, donde w_2022 = nómina de obreros permanentes /
# número de obreros permanentes en 2022. Si en 2022 la firma reportó por azar
# un salario promedio bajo (nómina baja, o conteo de obreros alto), su Kaitz sale
# alto Y su outcome 2022 -> 2023 se mueve mecánicamente: el costo por trabajador
# "sube" y el empleo "cae" al normalizarse. Es el sesgo de división (Borjas 1980)
# y la reversión a la media que documentan 09-16.
#
# LA IDEA (Autor, Manning y Smith 2016, AEJ Applied; Card, Katz y Krueger 1994)
# Instrumentar la medida ruidosa con otra que comparta la SEÑAL pero no el RUIDO
# de 2022. Aquí: el Kaitz construido con el salario de 2021 o de 2019 (mismo
# SM_2023 en el numerador, mismo filtro, misma definición).
#
#   Kaitz_2022 = señal_i + ruido_i,2022
#   Kaitz_2021 = señal_i + ruido_i,2021
#
# Si ruido_2021 no está correlacionado con ruido_2022 ni con los choques del
# outcome alrededor de 2022-2023, el IV recupera el efecto de la señal y además
# corrige la atenuación que tiene la "celda limpia" de 04 (que es, en esencia, la
# forma reducida de este IV sin reescalar).
#
# SUPUESTOS QUE HAY QUE DEFENDER EN LA TESIS (y cómo este script los revisa)
#   1. Relevancia: Kaitz_2021 predice Kaitz_2022. -> Sección 2 (F de primera
#      etapa, con errores robustos). Con correlación ~0,57 entre 2019 y 2022 (tabla T03 de 04) no
#      debería haber problema de instrumento débil.
#   2. Ruido no persistente: si el ruido es AR(1) y no transitorio, ruido_2021
#      arrastra parte de ruido_2022 y el IV con 2021 queda sesgado en la misma
#      dirección que MCO (menos). Con 2019 el arrastre es menor. -> Se estiman
#      ambos por separado y juntos. Si 2021 y 2019 dan efectos distintos, el
#      ruido es persistente y hay que creerle más a 2019. (No se usa la prueba
#      de Sargan: supone errores independientes y en un panel con la misma
#      firma repetida rechaza casi siempre, aun con instrumentos válidos -- se
#      comprobó con datos simulados.)
#   3. Exclusión: Kaitz_2021 no afecta el outcome 2022->2023 salvo a través de
#      la exposición. Se viola si las firmas con salario bajo en 2021 tienen
#      tendencias propias. -> Pre-tendencias de la forma reducida (sección 3),
#      EXCLUYENDO el año con que se construyó el instrumento, cuyo coeficiente
#      está mecánicamente contaminado (igual que 2022 lo está para Bite).
#   4. Que el IV no resuelve la reversión GENÉRICA: si las firmas de salario
#      estructuralmente bajo crecen distinto todos los años (no por ruido, sino
#      por su naturaleza), el IV también lo recoge. -> Sección 4: el mismo IV
#      aplicado año por año (placebo rodante instrumentado, lógica de Dustmann
#      et al. 2022). El efecto del choque es el exceso de 2023 sobre los años
#      normales, no el coeficiente de 2023 solo.
#
# REGLA DE DECISIÓN (fijada antes de correr; comitear este archivo ANTES de
# ver resultados, como se hizo con NOTA_DECISIONES.md)
#   El efecto sobre el empleo se reporta como causal SOLO si se cumplen los tres:
#   (a) IV con 2019 (lectura A, 2023 vs 2022) significativo al 5%;
#   (b) pre-tendencias de la forma reducida con 2019 no rechazan al 10%;
#   (c) en el placebo rodante instrumentado, el coeficiente del año del choque
#       excede al promedio de los años normales, con p < 0,10.
#   Si (a) se cumple pero (c) no: "asociación que no se distingue de la dinámica
#   normal de las firmas de salario bajo" -> capítulo 6.
#   Si (a) no se cumple: el −1,75% de MCO se atribuye al ruido de 2022 ->
#   capítulo 6, citando a Autor, Manning y Smith (2016).
#   Misma regla para el costo laboral (primer eslabón).
#
# LIMITACIONES QUE NO RESUELVE
#   - La muestra se restringe a firmas con obreros permanentes en 2019, 2021 y
#     2022 (sobrevivientes con obreros los tres años). Por eso se reporta
#     también MCO en la MISMA muestra: la comparación relevante es MCO vs IV en
#     la muestra común, no contra el −1,75% de la muestra completa.
#   - 2021 es un año de recuperación pospandemia con PAEF vigente para firmas de
#     hasta 50 empleados; su salario puede tener ruido atípico.
# ------------------------------------------------------------------------------


# ==============================================================================
# 0. PREPARACIÓN
# ==============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(fixest)
  library(ggplot2)
  library(flextable)
})

CARPETA <- file.path("4. RESULTADOS", "Variable_instrumental")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 40) {
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

guardar_grafico <- function(grafico, nombre_archivo, carpeta = CARPETA, ancho = 9, alto = 5.5) {
  ggsave(file.path(carpeta, "figuras", paste0(nombre_archivo, ".png")),
         grafico, width = ancho, height = alto, dpi = 200, bg = "white")
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

log_seguro <- function(x) log(ifelse(!is.na(x) & x > 0, x, NA_real_))

winsorizar <- function(x) {
  lim <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, lim[1]), lim[2])
}

# Salario mínimo mensual por decreto (mismos valores que 01 y 11)
SM <- c(`2012` = 566700, `2013` = 589500, `2014` = 616000, `2015` = 644350,
        `2016` = 689455, `2017` = 737717, `2018` = 781242, `2019` = 828116,
        `2020` = 877803, `2021` = 908526, `2022` = 1000000, `2023` = 1160000,
        `2024` = 1300000)
# Anual en miles, igual que SMLV_2023_ANUAL_MILES en 04_decision_medida.R
sm_anual_miles <- function(anio) SM[[as.character(anio)]] * 12 / 1000

EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
ANIOS_VENTANA <- c(2015:2019, 2021:2024)
ANIOS_EVENTO <- setdiff(ANIOS_VENTANA, 2022)       # 2022 es la referencia
ANIOS_PREVIOS <- c(2015:2019, 2021)

OUTCOMES <- c(log_costo = "Costo laboral por trabajador (log)",
              log_empleo = "Empleo total (log)",
              log_obreros = "Obreros (log)",
              log_permanente = "Empleo permanente (log)")


# ==============================================================================
# 1. DATOS, OUTCOMES Y MEDIDAS DE EXPOSICIÓN
# ==============================================================================
titulo("1. DATOS, OUTCOMES Y MEDIDAS DE EXPOSICIÓN")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP),
         ANIO = as.integer(as.character(ANIO)))

duplicados <- panel %>% count(NORDEMP, ANIO) %>% filter(n > 1) %>% nrow()
if (duplicados > 0) stop("El panel tiene ", duplicados, " pares firma-año duplicados.")

# Outcomes con las MISMAS definiciones de 05_resultados_y_mecanismos.R
panel <- panel %>%
  mutate(
    empleo_total = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo_total > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo_total, NA_real_),
    empleo_permanente = obreros_permanentes + administrativos_permanentes +
      profesional_tecnico_permanentes,
    log_costo = log_seguro(costo_trabajador),
    log_empleo = log_seguro(empleo_total),
    log_obreros = log_seguro(obreros_total_ocupado),
    log_permanente = log_seguro(empleo_permanente),
    # Salario promedio del obrero permanente: definición exacta de Bite (C3R2C1
    # sobre obreros permanentes, sin prestaciones), ver 04_decision_medida.R
    salario_obrero = ifelse(obreros_permanentes > 0 & sueldos_permanentes_obreros_c3r2c1 > 0,
                            sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes,
                            NA_real_),
    ANIO_F = factor(ANIO),
    post = as.integer(ANIO >= 2023)
  )

# Kaitz de la firma con el salario de un año y el mínimo del año siguiente al
# choque que se estudia. Para el diseño principal el mínimo es siempre el de
# 2023: lo único que cambia entre endógena e instrumentos es el AÑO DEL SALARIO.
kaitz_con_salario <- function(anio_salario, anio_minimo, nombre) {
  panel %>%
    filter(ANIO == anio_salario, !is.na(salario_obrero)) %>%
    transmute(NORDEMP, !!nombre := sm_anual_miles(anio_minimo) / salario_obrero)
}

exposicion <- kaitz_con_salario(2022, 2023, "kaitz_2022") %>%
  full_join(kaitz_con_salario(2021, 2023, "kaitz_2021"), by = "NORDEMP") %>%
  full_join(kaitz_con_salario(2019, 2023, "kaitz_2019"), by = "NORDEMP")

# Verificación contra el Bite original (no bloqueante: si el archivo no está,
# se sigue; si está y no coincide, se detiene)
ruta_vieja <- file.path("1. DATOS", "panel_analitico_firma_eam.rds")
if (file.exists(ruta_vieja)) {
  original <- read_rds(ruta_vieja) %>%
    mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO))) %>%
    filter(ANIO == 2022) %>%
    select(NORDEMP, Bite2022_obreros)
  chequeo <- inner_join(exposicion, original, by = "NORDEMP")
  r <- cor(chequeo$kaitz_2022, chequeo$Bite2022_obreros, use = "complete.obs")
  cat("\nVERIFICACIÓN: correlación réplica vs Bite2022_obreros =", round(r, 5),
      "sobre", sum(complete.cases(chequeo)), "firmas\n")
  if (r < 0.999) stop("La réplica de Bite no coincide con la original (r < 0,999). ",
                      "Revisar la definición antes de seguir.")
} else {
  cat("\nAVISO: no se encontró", ruta_vieja, "-- se omite la verificación de la réplica.\n")
}

# Muestra común: firmas con las tres medidas. Winsorización 1/99 y
# estandarización A NIVEL DE FIRMA (una observación por firma). En 05 la DE se
# calcula sobre filas firma-año; la diferencia en la escala es menor, pero por
# eso MCO de este script puede no reproducir exactamente el −1,75%.
firmas_iv <- exposicion %>%
  filter(!is.na(kaitz_2022), !is.na(kaitz_2021), !is.na(kaitz_2019)) %>%
  mutate(across(c(kaitz_2022, kaitz_2021, kaitz_2019),
                ~ { w <- winsorizar(.x); w / sd(w) }, .names = "{.col}_de"))

clasificacion_2022 <- panel %>%
  filter(ANIO == 2022) %>%
  transmute(NORDEMP,
            sector_2022 = factor(CIIU4),
            depto_2022  = factor(DPTO),
            tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

firmas_iv <- firmas_iv %>% inner_join(clasificacion_2022, by = "NORDEMP")

cobertura <- tibble(
  concepto = c("Firmas con Kaitz 2022", "Firmas con Kaitz 2021", "Firmas con Kaitz 2019",
               "Muestra común (las tres + controles 2022)"),
  firmas = c(sum(!is.na(exposicion$kaitz_2022)), sum(!is.na(exposicion$kaitz_2021)),
             sum(!is.na(exposicion$kaitz_2019)), nrow(firmas_iv))
)
ver(cobertura)

datos <- panel %>%
  filter(ANIO %in% ANIOS_VENTANA) %>%
  inner_join(firmas_iv, by = "NORDEMP")

# Interacciones año x medida, construidas a mano (una columna por año) para que
# la versión MCO, la forma reducida y el IV usen exactamente los mismos regresores.
interactuar <- function(d, variable, prefijo) {
  for (k in ANIOS_EVENTO) d[[paste0(prefijo, k)]] <- d[[variable]] * as.integer(d$ANIO == k)
  d
}
datos <- datos %>%
  interactuar("kaitz_2022_de", "x_") %>%
  interactuar("kaitz_2021_de", "z21_") %>%
  interactuar("kaitz_2019_de", "z19_") %>%
  mutate(x_post = kaitz_2022_de * post,
         z21_post = kaitz_2021_de * post,
         z19_post = kaitz_2019_de * post)

cat("\nFirmas-año en la ventana:", nrow(datos), " | Firmas:", n_distinct(datos$NORDEMP), "\n")


# ==============================================================================
# 2. RELEVANCIA: ¿EL KAITZ DE 2021/2019 PREDICE EL DE 2022?
# ==============================================================================
titulo("2. PRIMERA ETAPA (CORTE TRANSVERSAL DE FIRMAS)")

# Como la exposición no varía en el tiempo, la primera etapa relevante es el
# corte transversal: Kaitz_2022 contra el instrumento, con los mismos controles
# fijos de 2022. Es la misma relación que usa cada interacción año x instrumento.
primera_etapa <- function(instrumentos, etiqueta) {
  f <- as.formula(paste("kaitz_2022_de ~", paste(instrumentos, collapse = " + "),
                        "| sector_2022 + tamano_2022 + depto_2022"))
  m <- feols(f, data = firmas_iv, vcov = "hetero")
  w <- wald(m, keep = paste0("^(", paste(instrumentos, collapse = "|"), ")$"), print = FALSE)
  tibble(instrumento = etiqueta,
         coeficientes = paste(round(coef(m), 3), collapse = " / "),
         F_primera_etapa = w$stat,
         p_valor = w$p,
         r2_within = as.numeric(fitstat(m, "wr2")),
         firmas = nobs(m))
}

relevancia <- bind_rows(
  primera_etapa("kaitz_2021_de", "Kaitz 2021"),
  primera_etapa("kaitz_2019_de", "Kaitz 2019"),
  primera_etapa(c("kaitz_2021_de", "kaitz_2019_de"), "Kaitz 2021 + 2019")
)
correlaciones <- firmas_iv %>%
  summarise(r_2022_2021 = cor(kaitz_2022_de, kaitz_2021_de),
            r_2022_2019 = cor(kaitz_2022_de, kaitz_2019_de),
            r_2021_2019 = cor(kaitz_2021_de, kaitz_2019_de))
ver(relevancia)
ver(correlaciones)
guardar_tabla(relevancia, "T01_primera_etapa",
              "Tabla 1. Primera etapa: Kaitz 2022 contra Kaitz de años previos (corte de firmas, controles 2022)")

# Lectura de las correlaciones (ruido transitorio vs persistente): si el ruido
# fuera puramente transitorio, r(2022,2021) ≈ r(2022,2019) ≈ r(2021,2019).
# Si r(2022,2021) es claramente mayor que r(2022,2019), hay componente
# persistente (o la señal cambió en el tiempo) y el IV con 2021 hereda más ruido.


# ==============================================================================
# 3. EVENT STUDY: MCO, FORMA REDUCIDA E IV EN LA MISMA MUESTRA
# ==============================================================================
titulo("3. EVENT STUDY: MCO vs FORMA REDUCIDA vs IV")

nombres_x <- paste0("x_", ANIOS_EVENTO)

# Años cuyo coeficiente está contaminado mecánicamente por la construcción del
# instrumento: no entran en la prueba de pre-tendencias de esa versión.
CONTAMINADOS <- list(MCO = integer(0), `IV 2021` = 2021L, `IV 2019` = 2019L,
                     `IV 2021+2019` = c(2019L, 2021L),
                     `FR 2021` = 2021L, `FR 2019` = 2019L)

estimar_evento <- function(outcome, version) {
  z <- switch(version,
              `IV 2021` = "z21_", `IV 2019` = "z19_", `IV 2021+2019` = c("z21_", "z19_"),
              `FR 2021` = "z21_", `FR 2019` = "z19_", NULL)
  regresores <- if (startsWith(version, "FR")) paste0(z, ANIOS_EVENTO) else nombres_x
  
  f <- if (startsWith(version, "IV")) {
    instr <- unlist(lapply(z, function(p) paste0(p, ANIOS_EVENTO)))
    as.formula(paste0(outcome, " ~ 1 | ", EFECTOS, " | ",
                      paste(nombres_x, collapse = " + "), " ~ ",
                      paste(instr, collapse = " + ")))
  } else {
    as.formula(paste0(outcome, " ~ ", paste(regresores, collapse = " + "), " | ", EFECTOS))
  }
  
  m <- tryCatch(feols(f, data = datos, cluster = ~NORDEMP),
                error = function(e) { cat("  ERROR en", outcome, version, ":", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)
  
  ct <- as.data.frame(coeftable(m))
  ct$nombre <- rownames(ct)
  ct$anio <- as.integer(sub(".*_", "", ct$nombre))
  
  previos <- setdiff(ANIOS_PREVIOS, CONTAMINADOS[[version]])
  nombres_previos <- ct$nombre[ct$anio %in% previos]
  p_previos <- tryCatch(
    wald(m, keep = paste0("^(", paste(nombres_previos, collapse = "|"), ")$"), print = FALSE)$p,
    error = function(e) NA_real_)
  
  list(
    tabla = tibble(outcome = outcome, version = version, anio = ct$anio,
                   coeficiente = ct$Estimate, error_estandar = ct$`Std. Error`,
                   p_valor = ct$`Pr(>|t|)`) %>%
      bind_rows(tibble(outcome = outcome, version = version, anio = 2022L,
                       coeficiente = 0, error_estandar = 0, p_valor = NA_real_)) %>%
      arrange(anio),
    resumen = tibble(outcome = outcome, version = version,
                     anios_prueba_previa = paste(previos, collapse = ","),
                     p_previos = p_previos,
                     observaciones = nobs(m)),
    modelo = m
  )
}

# Lectura C (DiD simple, promedio post - pre) en IV. Excluye del periodo previo
# los años usados para construir el instrumento (su outcome está contaminado).
estimar_did <- function(outcome, version) {
  excluir <- CONTAMINADOS[[version]]
  d <- datos %>% filter(!ANIO %in% excluir)
  f <- switch(version,
    MCO = paste0(outcome, " ~ x_post | ", EFECTOS),
    `IV 2021` = paste0(outcome, " ~ 1 | ", EFECTOS, " | x_post ~ z21_post"),
    `IV 2019` = paste0(outcome, " ~ 1 | ", EFECTOS, " | x_post ~ z19_post"),
    `IV 2021+2019` = paste0(outcome, " ~ 1 | ", EFECTOS, " | x_post ~ z21_post + z19_post"),
    NULL)
  if (is.null(f)) return(NULL)
  m <- tryCatch(feols(as.formula(f), data = d, cluster = ~NORDEMP), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  fila <- coeftable(m)[1, ]
  tibble(outcome = outcome, version = version, lectura = "C. DiD simple (post - pre)",
         coeficiente = fila[["Estimate"]], error_estandar = fila[["Std. Error"]],
         p_valor = fila[["Pr(>|t|)"]])
}

VERSIONES <- c("MCO", "FR 2021", "FR 2019", "IV 2021", "IV 2019", "IV 2021+2019")
eventos <- list()
for (y in names(OUTCOMES)) for (v in VERSIONES) {
  cat("Estimando", y, "-", v, "\n")
  eventos[[paste(y, v)]] <- estimar_evento(y, v)
}
eventos <- Filter(Negate(is.null), eventos)

coeficientes <- bind_rows(lapply(eventos, `[[`, "tabla"))
resumen_eventos <- bind_rows(lapply(eventos, `[[`, "resumen"))

lectura_a <- coeficientes %>%
  filter(anio == 2023) %>%
  transmute(outcome, version, lectura = "A. Cambio 2022 -> 2023",
            coeficiente, error_estandar, p_valor)

lecturas_c <- bind_rows(lapply(names(OUTCOMES), function(y)
  bind_rows(lapply(c("MCO", "IV 2021", "IV 2019", "IV 2021+2019"), function(v) estimar_did(y, v)))))

tabla_principal <- bind_rows(lectura_a, lecturas_c) %>%
  left_join(resumen_eventos, by = c("outcome", "version")) %>%
  mutate(efecto_pct = 100 * coeficiente,
         ic95_inf_pct = 100 * (coeficiente - 1.96 * error_estandar),
         ic95_sup_pct = 100 * (coeficiente + 1.96 * error_estandar),
         significancia = estrellas(p_valor),
         outcome = OUTCOMES[outcome]) %>%
  select(outcome, version, lectura, efecto_pct, ic95_inf_pct, ic95_sup_pct, p_valor,
         significancia, p_previos, anios_prueba_previa, observaciones) %>%
  arrange(outcome, lectura, factor(version, levels = VERSIONES))

ver(tabla_principal)
guardar_tabla(tabla_principal, "T02_mco_vs_iv",
              paste("Tabla 2. Efecto por DE de Kaitz 2022: MCO, forma reducida e IV en la muestra común.",
                    "FR = forma reducida (escala del instrumento, no comparable en magnitud)."))

guardar_tabla(coeficientes %>% mutate(outcome = OUTCOMES[outcome]),
              "T03_coeficientes_por_anio",
              "Tabla 3. Coeficientes del event study por año (referencia 2022)")

# Gráfico: MCO vs IV por outcome
for (y in names(OUTCOMES)) {
  g <- coeficientes %>%
    filter(outcome == y, version %in% c("MCO", "IV 2021", "IV 2019")) %>%
    mutate(desplazamiento = c(MCO = -0.2, `IV 2021` = 0, `IV 2019` = 0.2)[version]) %>%
    ggplot(aes(x = anio + desplazamiento, y = 100 * coeficiente, color = version)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                      ymax = 100 * (coeficiente + 1.96 * error_estandar)), width = 0.15) +
    geom_point(size = 2.2) +
    scale_x_continuous(breaks = ANIOS_VENTANA) +
    scale_color_manual(values = c(MCO = "#808080", `IV 2021` = "#1F4E79", `IV 2019` = "#C00000")) +
    labs(title = paste0(OUTCOMES[[y]], ": MCO vs variable instrumental"),
         subtitle = "Efecto de una DE de Kaitz 2022, frente a 2022. Muestra común de firmas.",
         x = NULL, y = "Efecto (%)", color = NULL,
         caption = paste("En cada IV, el coeficiente del año con que se construyó el instrumento",
                         "está contaminado por construcción y no es evidencia de pre-tendencia.")) +
    tema_tesis
  guardar_grafico(g, paste0("G01_evento_", y))
}


# ==============================================================================
# 4. PLACEBO RODANTE INSTRUMENTADO (lógica de Dustmann et al. 2022)
# ==============================================================================
titulo("4. PLACEBO RODANTE INSTRUMENTADO")

# Para cada año base t0: exposición = SM_(t0+1) / w_t0, instrumento =
# SM_(t0+1) / w_(t0-1), outcome = log y_(t0+1) - log y_t0, controles fijos en
# t0. Se apilan todos los años y se estima con errores agrupados por firma, para
# poder comparar el año del choque contra el promedio de los años normales con
# una covarianza bien calculada (las firmas se repiten entre años).
#
# Años base: 2014-2018 son "normales" (choques 2015-2019). 2021 usa el salario
# de 2020 como instrumento (pandemia) y se reporta aparte. 2022 es el choque
# real (mínimo de 2023). 2023 es el choque de 2024.

ANIOS_BASE <- c(2014:2018, 2021:2023)
NORMALES <- 2014:2018

controles_en <- function(anio) {
  panel %>% filter(ANIO == anio) %>%
    transmute(NORDEMP, sector_t0 = factor(CIIU4), depto_t0 = factor(DPTO),
              tamano_t0 = factor(tamano_empresa))
}

construir_corte <- function(t0) {
  base <- panel %>% filter(ANIO == t0) %>%
    select(NORDEMP, w_t0 = salario_obrero, any_of(names(OUTCOMES)))
  rezago <- panel %>% filter(ANIO == t0 - 1) %>% select(NORDEMP, w_rezago = salario_obrero)
  siguiente <- panel %>% filter(ANIO == t0 + 1) %>%
    select(NORDEMP, any_of(names(OUTCOMES))) %>%
    rename_with(~ paste0(.x, "_sig"), -NORDEMP)
  
  corte <- base %>%
    inner_join(rezago, by = "NORDEMP") %>%
    inner_join(siguiente, by = "NORDEMP") %>%
    inner_join(controles_en(t0), by = "NORDEMP") %>%
    filter(!is.na(w_t0), !is.na(w_rezago)) %>%
    mutate(x = sm_anual_miles(t0 + 1) / w_t0,
           z = sm_anual_miles(t0 + 1) / w_rezago,
           across(c(x, z), ~ { w <- winsorizar(.x); w / sd(w) }),
           anio_base = t0)
  for (y in names(OUTCOMES)) corte[[paste0("d_", y)]] <- corte[[paste0(y, "_sig")]] - corte[[y]]
  corte
}

apilado <- bind_rows(lapply(ANIOS_BASE, construir_corte))
for (t0 in ANIOS_BASE) {
  apilado[[paste0("xb_", t0)]] <- apilado$x * as.integer(apilado$anio_base == t0)
  apilado[[paste0("zb_", t0)]] <- apilado$z * as.integer(apilado$anio_base == t0)
}
apilado <- apilado %>% mutate(anio_base_f = factor(anio_base))

EFECTOS_APILADO <- "anio_base_f^sector_t0 + anio_base_f^depto_t0 + anio_base_f^tamano_t0"
xb <- paste0("xb_", ANIOS_BASE)
zb <- paste0("zb_", ANIOS_BASE)

placebo_rodante <- function(outcome, metodo) {
  dy <- paste0("d_", outcome)
  f <- if (metodo == "IV") {
    paste0(dy, " ~ 1 | ", EFECTOS_APILADO, " | ", paste(xb, collapse = " + "),
           " ~ ", paste(zb, collapse = " + "))
  } else {
    paste0(dy, " ~ ", paste(xb, collapse = " + "), " | ", EFECTOS_APILADO)
  }
  m <- tryCatch(feols(as.formula(f), data = apilado, cluster = ~NORDEMP),
                error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)
  
  b <- coef(m); V <- vcov(m)
  nb <- names(b)
  anio_de <- as.integer(sub(".*_", "", nb))
  
  por_anio <- tibble(outcome = outcome, metodo = metodo, anio_base = anio_de,
                     anio_choque = anio_de + 1, coeficiente = unname(b),
                     error_estandar = sqrt(diag(V)),
                     p_valor = 2 * pnorm(-abs(unname(b) / sqrt(diag(V)))),
                     firmas = as.integer(table(apilado$anio_base[obs(m)])[as.character(anio_de)]))
  
  # Contraste: año del choque menos promedio de años normales
  contraste <- function(t_choque) {
    w <- setNames(rep(0, length(b)), nb)
    w[anio_de == t_choque] <- 1
    w[anio_de %in% NORMALES] <- -1 / sum(anio_de %in% NORMALES)
    est <- sum(w * b); ee <- sqrt(as.numeric(t(w) %*% V %*% w))
    tibble(outcome = outcome, metodo = metodo,
           contraste = paste0("Base ", t_choque, " menos promedio de bases 2014-2018"),
           exceso = est, error_estandar = ee, p_valor = 2 * pnorm(-abs(est / ee)))
  }
  list(por_anio = por_anio, excesos = bind_rows(contraste(2022), contraste(2023)))
}

rodante <- list()
for (y in names(OUTCOMES)) for (mt in c("MCO", "IV")) {
  cat("Placebo rodante:", y, "-", mt, "\n")
  rodante[[paste(y, mt)]] <- placebo_rodante(y, mt)
}
rodante <- Filter(Negate(is.null), rodante)

rodante_anios <- bind_rows(lapply(rodante, `[[`, "por_anio")) %>%
  mutate(efecto_pct = 100 * coeficiente, significancia = estrellas(p_valor),
         tipo = case_when(anio_base == 2022 ~ "Choque 2023",
                          anio_base == 2023 ~ "Choque 2024",
                          anio_base == 2021 ~ "Base 2021 (instrumento 2020)",
                          TRUE ~ "Año normal"),
         outcome = OUTCOMES[outcome])
rodante_excesos <- bind_rows(lapply(rodante, `[[`, "excesos")) %>%
  mutate(exceso_pct = 100 * exceso, significancia = estrellas(p_valor),
         outcome = OUTCOMES[outcome])

ver(rodante_anios)
ver(rodante_excesos)
guardar_tabla(rodante_anios, "T04_placebo_rodante_iv",
              "Tabla 4. Placebo rodante: efecto de una DE de Kaitz en cada transición anual, MCO e IV")
guardar_tabla(rodante_excesos, "T05_exceso_sobre_anios_normales",
              "Tabla 5. Exceso del año del choque sobre el promedio de los años normales (errores agrupados por firma)")

g_rod <- rodante_anios %>%
  filter(metodo == "IV") %>%
  ggplot(aes(x = factor(anio_choque), y = efecto_pct, fill = tipo)) +
  geom_col() +
  geom_errorbar(aes(ymin = 100 * (coeficiente - 1.96 * error_estandar),
                    ymax = 100 * (coeficiente + 1.96 * error_estandar)), width = 0.25) +
  geom_hline(yintercept = 0, color = "grey40") +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_fill_manual(values = c(`Año normal` = "#9DB4CE", `Choque 2023` = "#C00000",
                               `Choque 2024` = "#E08080", `Base 2021 (instrumento 2020)` = "#CCCCCC")) +
  labs(title = "Placebo rodante instrumentado",
       subtitle = "Efecto de una DE de Kaitz (instrumentado con el Kaitz del año anterior) en cada año",
       x = "Año del aumento del mínimo", y = "Efecto (%)", fill = NULL,
       caption = "Si el Kaitz mide el choque, el año 2023 debe sobresalir de los años normales.") +
  tema_tesis
guardar_grafico(g_rod, "G02_placebo_rodante_iv", ancho = 11, alto = 7)


# ==============================================================================
# 5. APLICACIÓN DE LA REGLA DE DECISIÓN
# ==============================================================================
titulo("5. REGLA DE DECISIÓN")

decidir <- function(y) {
  a <- tabla_principal %>% filter(outcome == OUTCOMES[[y]], version == "IV 2019",
                                  lectura == "A. Cambio 2022 -> 2023")
  fr <- resumen_eventos %>% filter(outcome == y, version == "FR 2019")
  ex <- rodante_excesos %>% filter(outcome == OUTCOMES[[y]], metodo == "IV",
                                   grepl("^Base 2022", contraste))
  crit_a <- nrow(a) == 1 && !is.na(a$p_valor) && a$p_valor < 0.05
  crit_b <- nrow(fr) == 1 && !is.na(fr$p_previos) && fr$p_previos >= 0.10
  crit_c <- nrow(ex) == 1 && !is.na(ex$p_valor) && ex$p_valor < 0.10 &&
    sign(ex$exceso) == sign(a$efecto_pct)
  veredicto <- if (crit_a && crit_b && crit_c) "Efecto causal defendible (capítulo 4/5)"
  else if (crit_a && !crit_c) "Asociación que no se distingue de años normales (capítulo 6)"
  else if (!crit_a) "No sobrevive al IV: se atribuye al ruido de 2022 (capítulo 6)"
  else "Mixto: revisar pre-tendencias de la forma reducida (capítulo 6)"
  tibble(outcome = OUTCOMES[[y]],
         a_iv2019_signif = crit_a, b_sin_pretendencia = crit_b,
         c_excede_normales = crit_c, veredicto = veredicto)
}

decision <- bind_rows(lapply(names(OUTCOMES), decidir))
ver(decision)
guardar_tabla(decision, "T06_decision", "Tabla 6. Regla de decisión aplicada (fijada antes de correr)")

cat("\nListo. Resultados en", CARPETA, "\n")
