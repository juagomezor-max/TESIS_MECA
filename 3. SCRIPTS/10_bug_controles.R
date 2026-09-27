# ==============================================================================
# 10_bug_controles.R (antes 16_descomposicion_bug.R)
#
# ¿LA MEJORA EN 05_resultados_y_mecanismos.R VIENE DE LOS CONTROLES O DE LA
# MUESTRA?
#
# La corrección del bug de controles (sector_2022/depto_2022/tamano_2022
# recalculados por fila en vez de fijados en 2022 -- ver 09_validez_
# exposicion.R, transición a la sección 3, y su sección 4.1) cambió DOS cosas
# a la vez: (1) los controles pasaron de contemporáneos a fijos en 2022, y (2)
# la muestra se restringió a firmas con clasificación de 2022 (47% del panel
# no la tiene). Este script separa los dos canales estimando tres celdas de la
# misma especificación:
#
#   (A) controles viejos (contemporáneos) + muestra completa   = el original
#   (B) controles viejos (contemporáneos) + muestra restringida = aísla muestra
#   (D) controles nuevos (fijos 2022)     + muestra restringida = el corregido
#
# La celda "controles nuevos + muestra completa" no existe: sin clasificación
# de 2022 no hay control fijo que aplicar.
#
# RESULTADO: A y B salen BYTE-IDÉNTICOS en las dos pruebas que corre este
# script -- empleo (-0,2678268341325101%, n=44.612 en las dos celdas) y
# heterogeneidad medianas (1,4167952702271749%, n=13.947 en las dos celdas).
# Es decir, restringir la muestra a las firmas con clasificación de 2022 (el
# paso A->B) no cambia el resultado ni un decimal; todo el cambio observado
# entre la especificación original (A) y la corregida (D) ocurre en el paso
# B->D, cuando se fijan los controles en 2022. Atribución: 100% controles, 0%
# muestra. La sección 5 (quiénes son las firmas que se van) y la sección 6
# (conteo de NA) caracterizan esa muestra perdida y descartan la hipótesis de
# que un patrón de NA distinto por año explique el cambio en el número de
# firmas de 07_reconciliacion.R -- esa hipótesis no se sostuvo (cero NA en los
# años relevantes 2014-2024); la causa exacta de ese cambio en particular
# queda sin identificar.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds,
#           1. DATOS/exposicion_alternativa_2022.rds,
#           1. DATOS/panel_analitico_firma_eam.rds
# Salidas:  4. RESULTADOS/Descomposicion_bug/
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "Descomposicion_bug")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
ver <- function(tabla, filas = 40) {
  cat("\n>>> ", deparse(substitute(tabla)), ": ", nrow(tabla), " x ", ncol(tabla), "\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}
compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 4) {
  tw <- flextable(tabla); tw <- colformat_double(tw, digits = decimales)
  tw <- set_caption(tw, caption = titulo_tabla); tw <- autofit(tw)
  save_as_docx(tw, path = file.path(CARPETA, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(CARPETA, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tw
  cat("Tabla guardada:", nombre_archivo, "\n")
}
estrellas <- function(p) ifelse(is.na(p), "", ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*",""))))


# ==============================================================================
# 1. DATOS (idéntico a 05_resultados_y_mecanismos.R hasta antes de sector_2022)
# ==============================================================================
titulo("1. DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

alternativas <- read_rds(file.path("1. DATOS", "exposicion_alternativa_2022.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP)) %>%
  select(NORDEMP, any_of(c("golpe_c", "golpe_a", "golpe_costo")))

viejas <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO))) %>%
  filter(ANIO == 2022) %>%
  select(NORDEMP, any_of(c("Bite2022_obreros", "Exposure2022_obreros")))

base <- panel %>% left_join(alternativas, by = "NORDEMP") %>% left_join(viejas, by = "NORDEMP")

base <- base %>%
  mutate(
    empleo_total = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo_total > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo_total, NA_real_),
    w_obrero = ifelse(obreros_permanentes > 0,
                      sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA_real_),
    log_costo = log(ifelse(costo_trabajador > 0, costo_trabajador, NA_real_)),
    log_empleo = log(ifelse(empleo_total > 0, empleo_total, NA_real_)),
    ANIO_F = factor(ANIO),
    post = as.integer(ANIO >= 2023),
    # CONTROLES VIEJOS: contemporáneos, el bug tal como estaba en el pipeline real
    sector_2022_viejo = factor(CIIU4),
    depto_2022_viejo  = factor(DPTO),
    tamano_2022_viejo = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

# CONTROLES NUEVOS: fijos en 2022, con un join
clasificacion_2022 <- panel %>%
  filter(ANIO == 2022) %>%
  transmute(NORDEMP, sector_2022_fijo = factor(CIIU4), depto_2022_fijo = factor(DPTO),
            tamano_2022_fijo = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))

base <- base %>% left_join(clasificacion_2022, by = "NORDEMP")

# Flag de muestra restringida: la firma SÍ tiene clasificación de 2022
base <- base %>% mutate(tiene_2022 = NORDEMP %in% clasificacion_2022$NORDEMP)

n_total <- n_distinct(base$NORDEMP)
n_con_2022 <- n_distinct(base$NORDEMP[base$tiene_2022])
cat("Firmas totales en el panel:", n_total, "\n")
cat("Firmas CON clasificación de 2022:", n_con_2022, "\n")
cat("Firmas SIN clasificación de 2022:", n_total - n_con_2022, "\n")

for (m in c("Bite2022_obreros", "golpe_c", "golpe_a", "golpe_costo")) {
  if (m %in% names(base)) {
    base[[paste0(m, "_de")]] <- {
      lim <- quantile(base[[m]], probs = c(0.01, 0.99), na.rm = TRUE)
      w <- pmin(pmax(base[[m]], lim[1]), lim[2])
      w / sd(w, na.rm = TRUE)
    }
  }
}

datos_completos <- base %>% filter(ANIO %in% c(2015:2019, 2021:2024))
datos_restringidos <- datos_completos %>% filter(tiene_2022)

cat("\nFirmas-año, muestra completa:", nrow(datos_completos), "\n")
cat("Firmas-año, muestra restringida (con clasificación 2022):", nrow(datos_restringidos), "\n")


# ==============================================================================
# 2. FUNCIONES DE ESTIMACIÓN (idénticas a 05, parametrizadas por controles)
# ==============================================================================
titulo("2. ESPECIFICACIÓN")

estudio_evento <- function(outcome, tratamiento, base_datos, efectos) {
  v <- paste0(tratamiento, "_de")
  formula <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", v, ", ref = '2022') | ", efectos))
  modelo <- tryCatch(feols(formula, data = base_datos, cluster = ~NORDEMP), error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  p_previos <- tryCatch(wald(modelo, keep = "ANIO_F::(2015|2016|2017|2018|2019|2021):", print = FALSE)$p,
                        error = function(e) NA_real_)
  list(modelo = modelo, variable = v, p_previos = p_previos)
}

lectura_a <- function(evento) {
  if (is.null(evento)) return(tibble(efecto_pct = NA, p_valor = NA, p_previos = NA, n = NA))
  nombre <- paste0("ANIO_F::2023:", evento$variable)
  if (!nombre %in% rownames(coeftable(evento$modelo))) return(tibble(efecto_pct = NA, p_valor = NA, p_previos = NA, n = NA))
  f <- coeftable(evento$modelo)[nombre, ]
  tibble(efecto_pct = 100*f[["Estimate"]], p_valor = f[["Pr(>|t|)"]],
         significancia = estrellas(f[["Pr(>|t|)"]]), p_previos = evento$p_previos, n = nobs(evento$modelo))
}


# ==============================================================================
# 3. LAS TRES CELDAS -- EMPLEO, LECTURA A Y TENDENCIAS PREVIAS
# ==============================================================================
titulo("3. LAS TRES CELDAS: EMPLEO, LECTURA A")

EFECTOS_VIEJO  <- "NORDEMP + ANIO_F + sector_2022_viejo^ANIO_F + tamano_2022_viejo^ANIO_F + depto_2022_viejo^ANIO_F"
EFECTOS_NUEVO  <- "NORDEMP + ANIO_F + sector_2022_fijo^ANIO_F + tamano_2022_fijo^ANIO_F + depto_2022_fijo^ANIO_F"

celda_A <- lectura_a(estudio_evento("log_empleo", "Bite2022_obreros", datos_completos, EFECTOS_VIEJO)) %>% mutate(celda = "A. Controles viejos, muestra completa")
celda_B <- lectura_a(estudio_evento("log_empleo", "Bite2022_obreros", datos_restringidos, EFECTOS_VIEJO)) %>% mutate(celda = "B. Controles viejos, muestra restringida")
celda_D <- lectura_a(estudio_evento("log_empleo", "Bite2022_obreros", datos_restringidos, EFECTOS_NUEVO)) %>% mutate(celda = "D. Controles nuevos, muestra restringida")

empleo_ABD <- bind_rows(celda_A, celda_B, celda_D) %>% select(celda, everything())
ver(empleo_ABD)
guardar_tabla(empleo_ABD, "T01_empleo_lectura_A_ABD",
              "Tabla 1. Empleo, lectura A y tendencias previas -- las tres celdas", decimales = 4)

# --- Atribución ---------------------------------------------------------------
# Si B se parece a D: la mejora viene de la MUESTRA. Si B se parece a A: viene
# de los CONTROLES. Medido como fracción del cambio total (A->D) que ya ocurre
# en el paso A->B (muestra) vs. lo que queda en B->D (controles).
cambio_total <- celda_D$efecto_pct - celda_A$efecto_pct
cambio_muestra <- celda_B$efecto_pct - celda_A$efecto_pct
cambio_controles <- celda_D$efecto_pct - celda_B$efecto_pct

atribucion_empleo <- tibble(
  cifra = "Empleo, lectura A (efecto_pct)",
  cambio_total = cambio_total,
  cambio_por_muestra_A_a_B = cambio_muestra,
  cambio_por_controles_B_a_D = cambio_controles,
  pct_atribuible_a_muestra = round(100 * cambio_muestra / cambio_total, 1),
  pct_atribuible_a_controles = round(100 * cambio_controles / cambio_total, 1)
)
ver(atribucion_empleo)

# Lo mismo para p_previos (tendencias previas), en escala de p-valor
atribucion_pprevios <- tibble(
  cifra = "Tendencias previas del empleo (p_previos)",
  A = celda_A$p_previos, B = celda_B$p_previos, D = celda_D$p_previos
)
ver(atribucion_pprevios)

guardar_tabla(bind_rows(
  atribucion_empleo %>% transmute(cifra, A = celda_A$efecto_pct, B = celda_B$efecto_pct, D = celda_D$efecto_pct,
                                   pct_muestra = pct_atribuible_a_muestra, pct_controles = pct_atribuible_a_controles),
  atribucion_pprevios %>% transmute(cifra, A, B, D, pct_muestra = NA, pct_controles = NA)
), "T02_atribucion_empleo_y_tendencias",
  "Tabla 2. Atribución del cambio a muestra vs. controles -- empleo lectura A y tendencias previas", decimales = 4)


# ==============================================================================
# 4. LAS TRES CELDAS -- HETEROGENEIDAD MEDIANAS, EMPLEO
# ==============================================================================
titulo("4. LAS TRES CELDAS: HETEROGENEIDAD MEDIANAS, EMPLEO")

het_celda <- function(datos_het, tamano_col, efectos_sin_tamano) {
  sub <- filter(datos_het, .data[[tamano_col]] == "Mediana")
  if (nrow(sub) < 500) return(tibble(efecto_pct = NA, p_valor = NA, n = NA))
  formula <- as.formula(paste0("log_empleo ~ i(ANIO_F, Bite2022_obreros_de, ref = '2022') | ", efectos_sin_tamano))
  modelo <- tryCatch(feols(formula, data = sub, cluster = ~NORDEMP), error = function(e) NULL)
  if (is.null(modelo)) return(tibble(efecto_pct = NA, p_valor = NA, n = NA))
  nombre <- "ANIO_F::2023:Bite2022_obreros_de"
  if (!nombre %in% rownames(coeftable(modelo))) return(tibble(efecto_pct = NA, p_valor = NA, n = NA))
  f <- coeftable(modelo)[nombre, ]
  tibble(efecto_pct = 100*f[["Estimate"]], p_valor = f[["Pr(>|t|)"]],
         significancia = estrellas(f[["Pr(>|t|)"]]), n = nobs(modelo))
}

EFECTOS_VIEJO_SIN_TAMANO <- "NORDEMP + ANIO_F + sector_2022_viejo^ANIO_F + depto_2022_viejo^ANIO_F"
EFECTOS_NUEVO_SIN_TAMANO <- "NORDEMP + ANIO_F + sector_2022_fijo^ANIO_F + depto_2022_fijo^ANIO_F"

het_A <- het_celda(datos_completos, "tamano_2022_viejo", EFECTOS_VIEJO_SIN_TAMANO) %>% mutate(celda = "A. Controles viejos, muestra completa")
het_B <- het_celda(datos_restringidos, "tamano_2022_viejo", EFECTOS_VIEJO_SIN_TAMANO) %>% mutate(celda = "B. Controles viejos, muestra restringida")
het_D <- het_celda(datos_restringidos, "tamano_2022_fijo", EFECTOS_NUEVO_SIN_TAMANO) %>% mutate(celda = "D. Controles nuevos, muestra restringida")

heterog_ABD <- bind_rows(het_A, het_B, het_D) %>% select(celda, everything())
ver(heterog_ABD)
guardar_tabla(heterog_ABD, "T03_heterogeneidad_medianas_ABD",
              "Tabla 3. Heterogeneidad medianas, empleo -- las tres celdas", decimales = 4)

cambio_total_het <- het_D$efecto_pct - het_A$efecto_pct
cambio_muestra_het <- het_B$efecto_pct - het_A$efecto_pct
cambio_controles_het <- het_D$efecto_pct - het_B$efecto_pct
cat("\nAtribución heterogeneidad medianas: cambio total =", round(cambio_total_het,3),
    "pp | por muestra (A->B) =", round(cambio_muestra_het,3),
    "pp (", round(100*cambio_muestra_het/cambio_total_het,1), "%) | por controles (B->D) =",
    round(cambio_controles_het,3), "pp (", round(100*cambio_controles_het/cambio_total_het,1), "%)\n")


# ==============================================================================
# 5. QUIÉNES SON LAS FIRMAS QUE SE VAN
# ==============================================================================
titulo("5. CARACTERIZACIÓN: FIRMAS CON VS. SIN CLASIFICACIÓN DE 2022")

# Para cada firma, usamos su PRIMER año observado en el panel (no 2022, que por
# definición no existe para las que se van)
primera_obs <- panel %>%
  group_by(NORDEMP) %>%
  slice_min(ANIO, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(base %>% distinct(NORDEMP, tiene_2022), by = "NORDEMP") %>%
  mutate(
    empleo_total_obs = empleo_total_sin_propietarios,
    w_obrero_obs = ifelse(obreros_permanentes > 0,
                          sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA_real_)
  )

anios_por_firma <- panel %>% group_by(NORDEMP) %>% summarise(n_anios_en_panel = n_distinct(ANIO), .groups = "drop")
primera_obs <- primera_obs %>% left_join(anios_por_firma, by = "NORDEMP")

caracterizacion <- primera_obs %>%
  group_by(tiene_2022) %>%
  summarise(
    n_firmas = n(),
    empleo_promedio = round(mean(empleo_total_obs, na.rm=TRUE), 1),
    empleo_mediano = round(median(empleo_total_obs, na.rm=TRUE), 1),
    n_anios_promedio_en_panel = round(mean(n_anios_en_panel, na.rm=TRUE), 2),
    w_obrero_promedio = round(mean(w_obrero_obs, na.rm=TRUE), 0),
    pct_pequena = round(100*mean(tamano_empresa == "Pequena", na.rm=TRUE), 1),
    pct_mediana = round(100*mean(tamano_empresa == "Mediana", na.rm=TRUE), 1),
    pct_grande = round(100*mean(tamano_empresa == "Grande", na.rm=TRUE), 1),
    .groups = "drop"
  )
ver(caracterizacion)
guardar_tabla(caracterizacion, "T04_caracterizacion_firmas_perdidas",
              "Tabla 4. Firmas con vs. sin clasificación de 2022, en su primer año observado", decimales = 2)

# Sector más frecuente en cada grupo (top 5)
sector_comparado <- primera_obs %>%
  filter(!is.na(CIIU4)) %>%
  count(tiene_2022, CIIU4, sort = TRUE) %>%
  group_by(tiene_2022) %>%
  slice_max(n, n = 5) %>%
  ungroup()
ver(sector_comparado, filas = 20)
guardar_tabla(sector_comparado, "T05_sectores_mas_frecuentes",
              "Tabla 5. Los 5 sectores (CIIU4) más frecuentes, con y sin clasificación de 2022", decimales = 0)

# Distribución por año de PRIMERA aparición: para saber si son firmas que
# "salieron" (aparecen temprano y no llegan a 2022) o "entraron" (aparecen
# tarde, después de 2022)
primer_anio_dist <- primera_obs %>%
  group_by(tiene_2022, ANIO) %>%
  summarise(n_firmas = n(), .groups = "drop") %>%
  arrange(tiene_2022, ANIO)
ver(primer_anio_dist, filas = 40)
guardar_tabla(primer_anio_dist, "T06_distribucion_primer_anio",
              "Tabla 6. Año de primera aparición en el panel, con y sin clasificación de 2022", decimales = 0)


# ==============================================================================
# 6. EL SEGUNDO PROBLEMA: NA POR VARIABLE Y POR AÑO
# ==============================================================================
titulo("6. CONTEO DE NA EN CIIU4, DPTO Y TAMANO_EMPRESA, POR AÑO")

na_por_anio <- panel %>%
  group_by(ANIO) %>%
  summarise(
    filas = n(),
    na_ciiu4 = sum(is.na(CIIU4)),
    na_dpto = sum(is.na(DPTO)),
    na_tamano = sum(is.na(tamano_empresa)),
    pct_na_ciiu4 = round(100*mean(is.na(CIIU4)), 2),
    pct_na_dpto = round(100*mean(is.na(DPTO)), 2),
    pct_na_tamano = round(100*mean(is.na(tamano_empresa)), 2),
    .groups = "drop"
  )
ver(na_por_anio, filas = 20)
guardar_tabla(na_por_anio, "T07_NA_por_variable_y_anio",
              "Tabla 7. Conteo y porcentaje de NA en CIIU4, DPTO y tamano_empresa, por año", decimales = 2)

cat("\nCÓMO LEER: si pct_na_tamano (u otra columna) es mucho más alto en ciertos\n",
    "años que en 2022, eso explicaría por qué fijar los controles en 2022 puede\n",
    "RECUPERAR firmas en vez de solo perderlas -- confirmarlo o descartarlo aquí,\n",
    "sin ajustar nada más.\n")

if (length(compendio) > 0) save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
titulo("FIN DEL SCRIPT")
