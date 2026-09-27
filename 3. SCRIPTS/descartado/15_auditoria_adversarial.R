# ==============================================================================
# 15_auditoria_adversarial.R
#
# AUDITORÍA ADVERSARIAL DE LAS CINCO AFIRMACIONES DEL DIAGNÓSTICO
#
# Objetivo: buscar dónde las afirmaciones anteriores (de una conversación con
# IA, no de la asesoría) están equivocadas. Los datos de la EAM se consideran
# verídicos -- no se invoca error de reporte como explicación de nada.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds,
#           1. DATOS/panel_analitico_firma_eam.rds
# Salidas:  4. RESULTADOS/Auditoria_adversarial/
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "Auditoria_adversarial")
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
tema_tesis <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold"), legend.position = "bottom")


# ==============================================================================
# 0. DATOS Y CONSTRUCCIÓN (controles genuinamente fijos en 2022)
# ==============================================================================
titulo("0. DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

panel_original <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

SALARIO_MINIMO <- tibble(
  anio  = 2008:2024,
  valor = c(461500, 496900, 515000, 535600, 566700, 589500, 616000, 644350,
            689455, 737717, 781242, 828116, 877803, 908526, 1000000,
            1160000, 1300000),
  ipc_pct = c(rep(NA_real_, 4), 2.44, 1.94, 3.66, 6.77, 5.75, 4.09, 3.18,
              3.80, 1.61, 5.62, 13.12, 9.28, 5.20)
) %>%
  mutate(
    aumento_nominal_pct = 100 * (valor / lag(valor) - 1),
    aumento_real_pct = 100 * ((1 + aumento_nominal_pct/100) / (1 + ipc_pct/100) - 1),
    # Deflactor alternativo: IPC del año ANTERIOR (convención de negociación
    # del salario mínimo). Se calcula aquí para el punto A4.
    aumento_real_pct_ipc_previo = 100 * ((1 + aumento_nominal_pct/100) / (1 + lag(ipc_pct)/100) - 1)
  )
guardar_tabla(SALARIO_MINIMO, "T00_salario_minimo", "Tabla 0. Salario mínimo, las dos convenciones de aumento real", decimales = 2)

RAZON_COSTO_MINIMO <- 1.531

base <- panel %>%
  mutate(
    empleo = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo, NA_real_),
    w_obrero = ifelse(obreros_permanentes > 0 &
                        (sueldos_permanentes_obreros_c3r2c1 + prestaciones_permanentes_obreros_c3r3c1) > 0,
                      (sueldos_permanentes_obreros_c3r2c1 + prestaciones_permanentes_obreros_c3r3c1) /
                        obreros_permanentes / 12 * 1000, NA_real_),
    w_prof = ifelse(profesional_tecnico_permanentes > 0 &
                      (sueldos_permanentes_profesional_tecnico_c3r2pt + prestaciones_permanentes_profesional_tecnico_c3r3pt) > 0,
                    (sueldos_permanentes_profesional_tecnico_c3r2pt + prestaciones_permanentes_profesional_tecnico_c3r3pt) /
                      profesional_tecnico_permanentes / 12 * 1000, NA_real_),
    w_admin = ifelse(administrativos_permanentes > 0 &
                       (sueldos_permanentes_administrativos_c3r2c2 + prestaciones_permanentes_administrativos_c3r3c2) > 0,
                     (sueldos_permanentes_administrativos_c3r2c2 + prestaciones_permanentes_administrativos_c3r3c2) /
                       administrativos_permanentes / 12 * 1000, NA_real_),
    w_obrero_sueldos = ifelse(obreros_permanentes > 0 & sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes / 12 * 1000, NA_real_),
    n_categorias = (!is.na(w_obrero)) + (!is.na(w_prof)) + (!is.na(w_admin)),
    w_firma = ifelse(n_categorias > 0, rowSums(cbind(w_obrero, w_prof, w_admin), na.rm=TRUE) / n_categorias, NA_real_),
    c_firma = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                     costos_totales_personal_total_c3r10c3 / empleo / 12 * 1000, NA_real_),
    permanentes_total = rowSums(cbind(
      ifelse(is.na(w_obrero), 0, obreros_permanentes),
      ifelse(is.na(w_prof), 0, profesional_tecnico_permanentes),
      ifelse(is.na(w_admin), 0, administrativos_permanentes)), na.rm = TRUE),
    exposure_obreros = ifelse(empleo > 0, obreros_total_ocupado / empleo, NA_real_),
    log_costo = log(ifelse(costo_trabajador > 0, costo_trabajador, NA_real_)),
    # Masa salarial mensual del personal permanente, en pesos, para el punto A1
    masa_obrero_permanente = ifelse(obreros_permanentes > 0,
                                    sueldos_permanentes_obreros_c3r2c1 + prestaciones_permanentes_obreros_c3r3c1, NA_real_),
    masa_total_c3r10c3 = costos_totales_personal_total_c3r10c3
  )

controles_2022 <- base %>%
  filter(ANIO == 2022) %>%
  distinct(NORDEMP, .keep_all = TRUE) %>%
  transmute(NORDEMP, sector_2022 = factor(CIIU4), depto_2022 = factor(DPTO),
            tamano_2022 = factor(tamano_empresa, levels = c("Pequena","Mediana","Grande")))
base <- base %>% left_join(controles_2022, by = "NORDEMP")

construir_medidas <- function(anio_exposicion, anio_choque) {
  minimo <- SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == anio_choque]
  if (length(minimo) == 0) return(NULL)
  costo_minimo <- minimo * RAZON_COSTO_MINIMO
  medidas <- base %>%
    filter(ANIO == anio_exposicion) %>%
    transmute(
      NORDEMP,
      bite    = ifelse(!is.na(w_obrero_sueldos) & w_obrero_sueldos > 0, minimo / w_obrero_sueldos, NA_real_),
      golpe_c = ifelse(!is.na(w_firma) & w_firma > 0, minimo / w_firma, NA_real_),
      golpe_a = ifelse(permanentes_total > 0 & n_categorias > 0,
        (ifelse(is.na(w_obrero), 0, obreros_permanentes/permanentes_total * minimo/w_obrero) +
         ifelse(is.na(w_prof), 0, profesional_tecnico_permanentes/permanentes_total * minimo/w_prof) +
         ifelse(is.na(w_admin), 0, administrativos_permanentes/permanentes_total * minimo/w_admin)), NA_real_),
      golpe_costo = ifelse(!is.na(c_firma) & c_firma > 0, costo_minimo / c_firma, NA_real_),
      exposure = exposure_obreros
    )
  medidas %>% mutate(across(c(bite, golpe_c, golpe_a, golpe_costo, exposure), ~ {
    if (all(is.na(.x))) return(.x)
    lim <- quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE)
    w <- pmin(pmax(.x, lim[1]), lim[2]); w / sd(w, na.rm = TRUE)
  }))
}
MEDIDAS <- c(bite="Bite (Kaitz de obreros)", golpe_c="Golpe C (3 categorías)",
             golpe_a="Golpe A (ponderada)", golpe_costo="Golpe costo", exposure="Exposure (% obreros)")

salto <- function(medida, anio_exposicion, anio_base, anio_choque, outcome = "log_costo") {
  medidas <- construir_medidas(anio_exposicion, anio_choque)
  if (is.null(medidas) || !medida %in% names(medidas)) return(NULL)
  exposicion <- medidas %>% select(NORDEMP, valor = all_of(medida)) %>% filter(!is.na(valor), is.finite(valor))
  if (nrow(exposicion) < 500) return(NULL)
  valores <- base %>% filter(ANIO %in% c(anio_base, anio_choque)) %>%
    select(NORDEMP, ANIO, all_of(outcome), sector_2022, depto_2022, tamano_2022) %>%
    pivot_wider(names_from = ANIO, values_from = all_of(outcome), names_prefix = "y_")
  cb <- paste0("y_", anio_base); cc <- paste0("y_", anio_choque)
  if (!all(c(cb, cc) %in% names(valores))) return(NULL)
  datos <- valores %>% mutate(crecimiento = .data[[cc]] - .data[[cb]]) %>%
    inner_join(exposicion, by = "NORDEMP") %>% filter(is.finite(crecimiento))
  if (nrow(datos) < 400) return(NULL)
  modelo <- tryCatch(feols(crecimiento ~ valor | sector_2022 + depto_2022 + tamano_2022, data = datos, vcov = "hetero"),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  f <- coeftable(modelo)["valor", ]
  tibble(clave = medida, medida = MEDIDAS[[medida]], anio_exposicion, anio_base, anio_choque,
         ventana_anios = anio_choque - anio_base,
         efecto_pct = 100*f[["Estimate"]], ee_pct = 100*f[["Std. Error"]], p_valor = f[["Pr(>|t|)"]],
         significancia = estrellas(f[["Pr(>|t|)"]]), firmas = nobs(modelo))
}

cat("Firmas con controles fijos en 2022:", nrow(controles_2022), "\n")


# ==============================================================================
# BUG DE CONTROLES EN EL PIPELINE REAL (01-08)
# ==============================================================================
titulo("BUG-CONTROLES. ¿SECTOR/DEPTO/TAMANO_2022 ESTÁN REALMENTE FIJOS EN 05, 06, 07?")

ref_2022 <- base %>% filter(ANIO == 2022) %>% distinct(NORDEMP, .keep_all = TRUE) %>%
  transmute(NORDEMP, CIIU4_2022 = CIIU4, DPTO_2022 = DPTO, tamano_2022_fijo = tamano_empresa)

comparado <- panel %>% select(NORDEMP, ANIO, CIIU4, DPTO, tamano_empresa) %>%
  inner_join(ref_2022, by = "NORDEMP") %>% filter(ANIO != 2022, ANIO %in% 2015:2024, ANIO != 2020)

bug_controles <- comparado %>%
  mutate(distinta = (CIIU4 != CIIU4_2022) | (DPTO != DPTO_2022) | (tamano_empresa != tamano_2022_fijo)) %>%
  group_by(ANIO) %>%
  summarise(pct_al_menos_uno_distinto = round(100*mean(distinta, na.rm=TRUE), 2),
            pct_tamano_distinto = round(100*mean(tamano_empresa != tamano_2022_fijo, na.rm=TRUE), 2),
            pct_sector_distinto = round(100*mean(CIIU4 != CIIU4_2022, na.rm=TRUE), 2),
            n = n(), .groups = "drop")

ver(bug_controles)
guardar_tabla(bug_controles, "T_BUG_controles_por_anio",
              "Tabla BUG. % de firmas-año (2015-2024, sin 2020/2022) con sector/depto/tamaño distinto al de 2022 de esa misma firma -- confirmado en 05, 06, 07 del pipeline real", decimales = 2)

cat("\nCONCLUSIÓN: sector_2022/depto_2022/tamano_2022 en 05_resultados_y_mecanismos.R,\n",
    "06_mecanismos_por_grupo.R y 07_reconciliacion.R se construyen con\n",
    "`factor(CIIU4)` etc. sobre el panel COMPLETO (sin filtrar a 2022 primero),\n",
    "exactamente el mismo patrón que el bug encontrado en 10/12/13. En\n",
    "01_descriptivos_y_contexto.R y 08_tratamiento_continuo.R SÍ está bien\n",
    "hecho (filter(ANIO==2022) antes de construir los controles). En\n",
    "03_primer_eslabon_medidas.R y 04_decision_medida.R el dataframe ya es de\n",
    "corte transversal (una fila por firma), así que ahí no aplica el problema\n",
    "de la misma forma. CONFIRMADO: 3 de 8 scripts vivos tienen el bug.\n")


# ==============================================================================
# A1. EL TRASLAPE ALGEBRAICO
# ==============================================================================
titulo("A1. ¿HAY TRASLAPE ALGEBRAICO ENTRE W_OBRERO Y C?")

a1_2022 <- base %>% filter(ANIO == 2022, !is.na(masa_obrero_permanente), !is.na(masa_total_c3r10c3), masa_total_c3r10c3 > 0)

a1_resumen <- tibble(
  concepto = c("Firmas con ambos componentes, 2022",
               "Peso promedio de la masa salarial de obreros permanentes en C3R10C3",
               "Peso mediano",
               "Correlación w_obrero (por trabajador) vs c (por trabajador), 2022",
               "Correlación log(w_obrero) vs log(c), 2022"),
  valor = c(nrow(a1_2022),
            round(mean(a1_2022$masa_obrero_permanente / a1_2022$masa_total_c3r10c3, na.rm=TRUE), 4),
            round(median(a1_2022$masa_obrero_permanente / a1_2022$masa_total_c3r10c3, na.rm=TRUE), 4),
            round(cor(a1_2022$w_obrero, a1_2022$c_firma, use="complete.obs"), 4),
            round(cor(log(a1_2022$w_obrero), log(a1_2022$c_firma), use="complete.obs"), 4))
)
ver(a1_resumen)
guardar_tabla(a1_resumen, "T_A1_traslape", "Tabla A1. Peso de w_obrero en c y su correlación", decimales = 4)

cat("\nSUSTITUCIÓN METODOLÓGICA, declarada: en vez de simular un outcome con\n",
    "ruido puro (que requiere supuestos sobre cómo simular el ruido de\n",
    "medición), se usa la evidencia YA GENERADA del placebo rodante sobre años\n",
    "REALES sin choque (11_placebo_rodante_real.R, tabla T01): si el mecanismo\n",
    "algebraico opera, Bite debería predecir el crecimiento del costo laboral\n",
    "en CUALQUIER año, no solo en 2023. Esa tabla ya mostró coeficientes\n",
    "positivos y significativos en los 7 años sin choque probados (2015-2019,\n",
    "2022, 2024), con magnitudes de 3,16%-5,67%%, todas por encima del propio\n",
    "2023 (2,89%). Es una prueba más fuerte que una simulación porque usa\n",
    "variación real, no supuestos sobre el proceso de ruido.\n")


# ==============================================================================
# A2. ATENUACIÓN VS. TRASLAPE: CORRELACIÓN BITE 2019-2022 (Y LAS OTRAS CUATRO)
# ==============================================================================
titulo("A2. ¿LA CAÍDA DEL COEFICIENTE ES ATENUACIÓN CLÁSICA?")

m19 <- construir_medidas(2019, 2023) %>% rename_with(~paste0(.x, "_2019"), -NORDEMP)
m22 <- construir_medidas(2022, 2023) %>% rename_with(~paste0(.x, "_2022"), -NORDEMP)
comp_medidas <- m19 %>% inner_join(m22, by = "NORDEMP")

rho_tabla <- bind_rows(lapply(names(MEDIDAS), function(m) {
  v19 <- comp_medidas[[paste0(m, "_2019")]]; v22 <- comp_medidas[[paste0(m, "_2022")]]
  tibble(medida = MEDIDAS[[m]], clave = m,
         n_firmas_con_ambas = sum(!is.na(v19) & !is.na(v22)),
         rho_pearson = cor(v19, v22, use = "complete.obs"))
}))

# Caída observada del coeficiente estándar (exposición 2022) a la celda limpia
# (exposición 2019), choque 2023, ventana de 1 año -- ya calculadas en 13/14.
estandar_2023 <- bind_rows(lapply(names(MEDIDAS), function(m) salto(m, 2022, 2022, 2023, "log_costo")))
limpia_2023   <- bind_rows(lapply(names(MEDIDAS), function(m) salto(m, 2019, 2022, 2023, "log_costo")))

caida_tabla <- estandar_2023 %>% select(clave, medida, efecto_estandar = efecto_pct) %>%
  left_join(limpia_2023 %>% select(clave, efecto_limpio = efecto_pct), by = "clave") %>%
  mutate(razon_observada = round(efecto_limpio / efecto_estandar, 3)) %>%
  left_join(rho_tabla %>% select(clave, rho_pearson, n_firmas_con_ambas), by = "clave") %>%
  mutate(rho = round(rho_pearson, 3),
         diferencia_razon_menos_rho = round(razon_observada - rho, 3))

ver(caida_tabla)
guardar_tabla(caida_tabla, "T_A2_atenuacion_vs_traslape",
              "Tabla A2. Razón observada de caída (limpia/estándar) vs. rho (correlación 2019-2022), las cinco medidas", decimales = 3)

cat("\nCÓMO LEER: si razon_observada ~= rho, la atenuación clásica explica toda\n",
    "la caída y el argumento del traslape es innecesario. Si razon_observada\n",
    "es MENOR que rho (cae más de lo que la atenuación por sí sola predice),\n",
    "queda caída adicional atribuible a cortar el traslape.\n")


# ==============================================================================
# A3. EL CONTRASTE DE FALSACIÓN EN OTROS AÑOS DE AUMENTO REAL BAJO
# ==============================================================================
titulo("A3. ¿EL PATRÓN DE 2015 ES ESTRUCTURAL O ES UN AÑO RARO?")

# Ventanas equivalentes: usual (choque-1), salta (choque-2), larga (=año exposición, choque-4)
construir_ventanas_choque <- function(anio_choque) {
  anio_exp <- anio_choque - 4
  list(usual = anio_choque - 1, salta = anio_choque - 2, larga = anio_exp)
}

casos_a3 <- list(
  "2015 (real -2,03%)" = list(anio_choque = 2015, anio_exposicion = 2011),
  "2016 (real +1,18%)" = list(anio_choque = 2016, anio_exposicion = 2012),
  "2019 (real +2,12%)" = list(anio_choque = 2019, anio_exposicion = 2015),
  "2023 (real +6,15%, referencia)" = list(anio_choque = 2023, anio_exposicion = 2019)
)

resultado_a3 <- bind_rows(lapply(names(casos_a3), function(nombre) {
  caso <- casos_a3[[nombre]]
  vents <- construir_ventanas_choque(caso$anio_choque)
  bind_rows(lapply(names(vents), function(v) {
    anio_base_v <- vents[[v]]
    bind_rows(lapply(names(MEDIDAS), function(m) {
      r <- salto(m, caso$anio_exposicion, anio_base_v, caso$anio_choque, "log_costo")
      if (is.null(r)) return(NULL)
      # Aumento real ACUMULADO en la ventana: producto de los aumentos reales
      # anuales entre anio_base_v+1 y anio_choque
      anios_ventana <- (anio_base_v + 1):caso$anio_choque
      reales <- SALARIO_MINIMO$aumento_real_pct[SALARIO_MINIMO$anio %in% anios_ventana] / 100
      acumulado <- 100 * (prod(1 + reales, na.rm = TRUE) - 1)
      mutate(r, caso = nombre, ventana = v, aumento_real_acumulado_pct = round(acumulado, 2))
    }))
  }))
})) %>%
  select(caso, medida, ventana, anio_exposicion, anio_base, anio_choque, ventana_anios,
         aumento_real_acumulado_pct, efecto_pct, ee_pct, p_valor, significancia, firmas)

ver(resultado_a3, filas = 60)
guardar_tabla(resultado_a3, "T_A3_falsacion_multiples_anios",
              "Tabla A3. Falsación replicada en 2015, 2016, 2019, con 2023 de referencia", decimales = 3)

# Correlación entre el aumento real acumulado en la ventana y el efecto medido,
# por medida (usando SOLO los tres años de falsación + 2023, ventanas salta y larga)
escala_acumulado <- resultado_a3 %>%
  filter(ventana %in% c("salta", "larga")) %>%
  group_by(medida) %>%
  summarise(correlacion_con_acumulado = round(cor(aumento_real_acumulado_pct, efecto_pct, use="complete.obs"), 3),
            .groups = "drop")
ver(escala_acumulado)
guardar_tabla(escala_acumulado, "T_A3_correlacion_acumulado",
              "Tabla A3b. Correlación entre efecto y aumento real acumulado en la ventana, por medida", decimales = 3)

cat("\nCÓMO LEER: si el patrón de 'ventana larga = coeficiente grande' aparece\n",
    "también en 2016 y 2019 (no solo 2015), es estructural, no un año raro. Si\n",
    "correlacion_con_acumulado es alta y positiva, las ventanas largas sí\n",
    "podrían estar midiendo algo real (el aumento acumulado), no solo el largo\n",
    "de la ventana per se -- haría falta separar 'años de ventana' de 'aumento\n",
    "acumulado', que están correlacionados por construcción (ventanas más\n",
    "largas acumulan más aumento).\n")


# ==============================================================================
# A4. ¿EL DIFERENCIAL NO ESCALA CON EL AUMENTO REAL? -- ROBUSTEZ
# ==============================================================================
titulo("A4. ROBUSTEZ DEL 'NO ESCALA': SIN 2022, CON OTRO DEFLACTOR, CON IC")

anios_rodante <- c(2015, 2016, 2017, 2018, 2019, 2022, 2023, 2024)
rodante_estandar <- bind_rows(lapply(anios_rodante, function(a) {
  bind_rows(lapply(names(MEDIDAS), function(m) salto(m, a - 1, a - 1, a, "log_costo")))
})) %>%
  left_join(SALARIO_MINIMO %>% select(anio, aumento_real_pct, aumento_real_pct_ipc_previo),
            by = c("anio_choque" = "anio"))

# --- Sin 2022 ---------------------------------------------------------------------
sin_2022 <- rodante_estandar %>% filter(anio_choque != 2022)
cor_sin_2022 <- sin_2022 %>% group_by(medida) %>%
  summarise(correlacion_sin_2022 = round(cor(aumento_real_pct, efecto_pct, use="complete.obs"), 3), .groups="drop")

cor_con_2022 <- rodante_estandar %>% group_by(medida) %>%
  summarise(correlacion_con_2022 = round(cor(aumento_real_pct, efecto_pct, use="complete.obs"), 3), .groups="drop")

# --- Con el deflactor alternativo (IPC del año previo) -----------------------------
cor_ipc_previo <- rodante_estandar %>% filter(!is.na(aumento_real_pct_ipc_previo)) %>%
  group_by(medida) %>%
  summarise(correlacion_ipc_previo = round(cor(aumento_real_pct_ipc_previo, efecto_pct, use="complete.obs"), 3), .groups="drop")

a4_tabla <- cor_con_2022 %>%
  left_join(cor_sin_2022, by = "medida") %>%
  left_join(cor_ipc_previo, by = "medida")

ver(a4_tabla)
guardar_tabla(a4_tabla, "T_A4_robustez_correlacion",
              "Tabla A4. Correlación aumento real vs. diferencial: con/sin 2022, con las dos convenciones de deflactor", decimales = 3)

# --- Intervalo de confianza de la correlación (n=8), Bite -------------------------
ic_correlacion <- function(r, n, conf = 0.95) {
  z <- atanh(r); se <- 1/sqrt(n - 3)
  zc <- qnorm(1 - (1-conf)/2)
  tanh(c(z - zc*se, z + zc*se))
}
bite_r <- rodante_estandar %>% filter(clave == "bite")
r_bite <- cor(bite_r$aumento_real_pct, bite_r$efecto_pct, use = "complete.obs")
ic_bite <- ic_correlacion(r_bite, n = nrow(bite_r))
cat("\nCorrelación Bite (n=", nrow(bite_r), "): ", round(r_bite,3),
    " -- IC 95% (Fisher): [", round(ic_bite[1],3), ", ", round(ic_bite[2],3), "]\n", sep="")

ic_todas <- rodante_estandar %>% group_by(medida) %>%
  summarise(n = n(), r = cor(aumento_real_pct, efecto_pct, use="complete.obs"), .groups="drop") %>%
  rowwise() %>%
  mutate(ic_inf = ic_correlacion(r, n)[1], ic_sup = ic_correlacion(r, n)[2]) %>%
  ungroup() %>%
  mutate(across(c(r, ic_inf, ic_sup), ~round(.x, 3)))
ver(ic_todas)
guardar_tabla(ic_todas, "T_A4_intervalos_confianza",
              "Tabla A4b. Correlación con IC 95% (Fisher), n=8, las cinco medidas", decimales = 3)


# ==============================================================================
# A5. ¿BITE EN VENTANA DE 1 AÑO ES REALMENTE LA ÚNICA CELDA LIMPIA?
# ==============================================================================
titulo("A5. ROBUSTEZ DE LA CELDA 'BITE, VENTANA 1 AÑO, EXPOSICIÓN 4 AÑOS ANTES'")

anios_panel_todos <- sort(unique(base$ANIO))
anios_a5 <- anios_panel_todos[anios_panel_todos >= 2014 & !(anios_panel_todos %in% c(2020, 2021))]

rolling_bite_1yr <- bind_rows(lapply(anios_a5, function(a) {
  salto("bite", a - 4, a - 1, a, "log_costo")
})) %>%
  left_join(SALARIO_MINIMO %>% select(anio, aumento_real_pct), by = c("anio_choque" = "anio")) %>%
  mutate(es_2023_o_2024 = anio_choque %in% c(2023, 2024))

ver(rolling_bite_1yr)
guardar_tabla(rolling_bite_1yr, "T_A5_bite_rolling_1yr",
              "Tabla A5. Bite, ventana de 1 año, exposición 4 años antes, todos los años estimables", decimales = 3)

n_significativos_otros <- rolling_bite_1yr %>% filter(!es_2023_o_2024, p_valor < 0.05, efecto_pct > 0) %>% nrow()
n_anios_otros <- rolling_bite_1yr %>% filter(!es_2023_o_2024) %>% nrow()

cat("\nDe los", n_anios_otros, "años SIN choque de 2023/2024 probados, ",
    n_significativos_otros, "dan a Bite positivo y significativo al 5% con esta\n",
    "misma especificación (ventana 1 año, exposición 4 años antes).\n")

# --- Plausibilidad económica: elasticidad implícita --------------------------------
cat("\nPLAUSIBILIDAD ECONÓMICA:\n")
bite_2023_1yr <- rolling_bite_1yr %>% filter(anio_choque == 2023)
aumento_real_2023 <- SALARIO_MINIMO$aumento_real_pct[SALARIO_MINIMO$anio == 2023]
cat("Efecto de Bite en 2023 (celda limpia, 1 año):", round(bite_2023_1yr$efecto_pct, 3), "% por DE\n")
cat("Aumento real del mínimo en 2023:", round(aumento_real_2023, 2), "%\n")
cat("(Comparar la magnitud del efecto por DE de exposición contra el tamaño del\n",
    " choque agregado no da una 'elasticidad' en sentido estricto -- las unidades\n",
    " no son directamente comparables sin una escala de Bite en unidades de\n",
    " 'puntos de exposición al mínimo'. Se deja la comparación cualitativa: el\n",
    " efecto medido (~1 pp de costo laboral por DE de exposición) es una fracción\n",
    " pequeña del aumento real del mínimo (6,15%), lo cual es plausible si el\n",
    " paso-through del mínimo al costo laboral total no es completo -- hay\n",
    " sustitución hacia otros márgenes (documentados en 06_mecanismos_por_grupo.R:\n",
    " compresión salarial, tercerización).\n", sep = "")

if (length(compendio) > 0) save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
titulo("FIN DEL SCRIPT")
