# ==============================================================================
# graficar_tendencias_paralelas.R
#
# Genera G09_tendencias_paralelas (6 resultados, con y sin control por tamaño
# x año) sin correr 05_resultados_y_mecanismos.R completo -- HonestDiD ahí
# tarda varios minutos y no hace falta para este gráfico.
#
# REESTIMAR <- TRUE : reproduce lo mínimo necesario de 05 para estimar los 12
#   estudios de evento (6 variables x 2 especificaciones) y guarda sus
#   coeficientes en T11b_coeficientes_tendencias.csv.
# REESTIMAR <- FALSE (default): si ese CSV ya existe, no estima nada, solo
#   lee el CSV y grafica (corre en segundos).
#
# La sección de estimación (más abajo, dentro del if) es una copia literal de
# la sección 1 (construcción de variables) y de estudio_evento() (sección 2)
# de 05_resultados_y_mecanismos.R, sin cambios en la lógica. Si esas
# secciones cambian en 05, hay que actualizar esta copia a mano.
#
# Entradas (solo si REESTIMAR):
#   1. DATOS/panel_firma_eam_expalt_completo.rds
#   1. DATOS/exposicion_alternativa_2022.rds
#   1. DATOS/panel_analitico_firma_eam.rds
# Salidas:
#   4. RESULTADOS/05_resultados_y_mecanismos/T11b_coeficientes_tendencias.csv
#   4. RESULTADOS/05_resultados_y_mecanismos/figuras/G09_tendencias_paralelas.png
#
# Se corre desde la raíz del repositorio (abriendo TESIS_MECA.Rproj).
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(scales)

REESTIMAR <- FALSE

CARPETA <- file.path("4. RESULTADOS", "05_resultados_y_mecanismos")
RUTA_CSV <- file.path(CARPETA, "T11b_coeficientes_tendencias.csv")
RUTA_PNG <- file.path(CARPETA, "figuras", "G09_tendencias_paralelas.png")
dir.create(dirname(RUTA_PNG), recursive = TRUE, showWarnings = FALSE)

VARIABLES_TENDENCIAS <- tibble::tribble(
  ~variable,                   ~etiqueta,
  "log_costo",                 "Costo laboral",
  "log_empleo",                 "Empleo total",
  "log_permanente",             "Empleo permanente",
  "log_temporal_directo",       "Empleo temporal directo",
  "participacion_permanente",   "Participación de permanentes",
  "log_agencias",               "Personal de agencias"
)

if (REESTIMAR || !file.exists(RUTA_CSV)) {

  cat("REESTIMAR = TRUE (o no existe el CSV): estimando los 12 estudios de evento...\n")

  # ============================================================================
  # COPIA LITERAL de la sección 1 de 05_resultados_y_mecanismos.R
  # (construcción de variables). No modificar la lógica aquí sin modificarla
  # también allá, o las dos fuentes del mismo gráfico quedarán desalineadas.
  # ============================================================================

  panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
    mutate(NORDEMP = as.character(NORDEMP),
           ANIO = as.integer(as.character(ANIO)))

  alternativas <- read_rds(file.path("1. DATOS", "exposicion_alternativa_2022.rds")) %>%
    mutate(NORDEMP = as.character(NORDEMP)) %>%
    select(NORDEMP, any_of(c("golpe_c", "golpe_a", "golpe_costo")))

  viejas <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
    mutate(NORDEMP = as.character(NORDEMP),
           ANIO = as.integer(as.character(ANIO))) %>%
    filter(ANIO == 2022) %>%
    select(NORDEMP, any_of(c("Bite2022_obreros", "Exposure2022_obreros")))

  base <- panel %>%
    left_join(alternativas, by = "NORDEMP") %>%
    left_join(viejas, by = "NORDEMP")

  base <- base %>%
    mutate(
      empleo_total = empleo_total_sin_propietarios,

      costo_trabajador = ifelse(empleo_total > 0 & costos_totales_personal_total_c3r10c3 > 0,
                                costos_totales_personal_total_c3r10c3 / empleo_total, NA_real_),

      empleo_permanente = obreros_permanentes + administrativos_permanentes +
        profesional_tecnico_permanentes,
      empleo_temporal_directo = obreros_temporal_directo + administrativos_temporal_directo +
        profesional_tecnico_temporal_directo,
      empleo_temporal_agencias = obreros_temporal_agencias + administrativos_temporal_agencias +
        profesional_tecnico_temporal_agencias,
      empleo_aprendices = obreros_aprendices + administrativos_aprendices +
        profesional_tecnico_aprendices,
      empleo_temporal = empleo_temporal_directo + empleo_temporal_agencias,

      participacion_permanente = ifelse(empleo_total > 0, empleo_permanente / empleo_total, NA_real_),
      participacion_temporal = ifelse(empleo_total > 0, empleo_temporal / empleo_total, NA_real_),
      participacion_agencias = ifelse(empleo_total > 0, empleo_temporal_agencias / empleo_total, NA_real_),

      obreros_total = obreros_total_ocupado,
      participacion_obreros = ifelse(empleo_total > 0, obreros_total / empleo_total, NA_real_),

      w_obrero = ifelse(obreros_permanentes > 0,
                        sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA_real_),
      w_admin = ifelse(administrativos_permanentes > 0,
                       sueldos_permanentes_administrativos_c3r2c2 / administrativos_permanentes, NA_real_),
      brecha_obrero_admin = ifelse(!is.na(w_obrero) & !is.na(w_admin) & w_admin > 0,
                                   w_obrero / w_admin, NA_real_),

      outsourcing = outsourcing_total_c3r41c3,
      honorarios = honorarios_servicios_tecnicos_total_c3r15c3,
      servicios_terceros = productos_servicios_terceros_total_c3r14c3,
      pago_agencias = pago_agencias_temporales_total_c3r8c3,

      ventas = valor_ventas_valorven,
      produccion = produccion_bruta_prodbr2,
      valor_agregado = valor_agregado_valagri,
      inversion = inversion_bruta_invebrta,
      maquinaria_nueva = compra_maquinaria_nueva_c7c3r2,
      productividad = ifelse(empleo_total > 0 & valor_agregado_valagri > 0,
                             valor_agregado_valagri / empleo_total, NA_real_),

      participacion_aprendices = ifelse(empleo_total > 0, empleo_aprendices / empleo_total, NA_real_),
      costo_temporal = ifelse(empleo_temporal_directo > 0 &
                                sueldos_prest_temporal_directo_total_c3r4c3 > 0,
                              sueldos_prest_temporal_directo_total_c3r4c3 / empleo_temporal_directo,
                              NA_real_),
      energia_trabajador = ifelse(empleo_total > 0 & energia_electrica_kw_eelec > 0,
                                  energia_electrica_kw_eelec / empleo_total, NA_real_),
      empleo_por_ventas = ifelse(empleo_total > 0 & valor_ventas_valorven > 0,
                                 empleo_total / valor_ventas_valorven, NA_real_),
      margen = ifelse(produccion_bruta_prodbr2 > 0,
                      (valor_agregado_valagri - costos_totales_personal_total_c3r10c3) /
                        produccion_bruta_prodbr2, NA_real_),
      peso_costo = ifelse(produccion_bruta_prodbr2 > 0,
                          costos_totales_personal_total_c3r10c3 / produccion_bruta_prodbr2, NA_real_),
      participacion_mujeres_obreras = ifelse(obreros_total_ocupado > 0,
                                             mh_c4r5c1 / obreros_total_ocupado, NA_real_),

      ANIO_F = factor(ANIO),
      post = as.integer(ANIO >= 2023)
    )

  clasificacion_2022 <- panel %>%
    filter(ANIO == 2022) %>%
    transmute(
      NORDEMP,
      sector_2022 = factor(CIIU4),
      depto_2022  = factor(DPTO),
      tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
    )

  base <- base %>%
    select(-any_of(c("sector_2022", "depto_2022", "tamano_2022"))) %>%
    left_join(clasificacion_2022, by = "NORDEMP")

  log_seguro <- function(x) log(ifelse(!is.na(x) & x > 0, x, NA_real_))

  base <- base %>%
    mutate(
      log_costo = log_seguro(costo_trabajador),
      log_empleo = log_seguro(empleo_total),
      log_permanente = log_seguro(empleo_permanente),
      log_temporal = log_seguro(empleo_temporal),
      log_temporal_directo = log_seguro(empleo_temporal_directo),
      log_agencias = log_seguro(empleo_temporal_agencias),
      log_obreros = log_seguro(obreros_total),
      log_ventas = log_seguro(ventas),
      log_produccion = log_seguro(produccion),
      log_valor_agregado = log_seguro(valor_agregado),
      log_productividad = log_seguro(productividad),
      log_inversion = log_seguro(inversion),
      log_outsourcing = log_seguro(outsourcing),
      log_honorarios = log_seguro(honorarios),
      log_pago_agencias = log_seguro(pago_agencias),
      log_brecha_obrero_admin = log_seguro(brecha_obrero_admin),
      log_aprendices = log_seguro(empleo_aprendices),
      log_w_obrero = log_seguro(w_obrero),
      log_w_admin = log_seguro(w_admin),
      log_costo_temporal = log_seguro(costo_temporal),
      log_servicios_terceros = log_seguro(servicios_terceros),
      log_maquinaria_nueva = log_seguro(maquinaria_nueva),
      log_energia_trabajador = log_seguro(energia_trabajador),
      log_empleo_por_ventas = log_seguro(empleo_por_ventas),
      hace_outsourcing = as.integer(!is.na(outsourcing) & outsourcing > 0),
      usa_agencias = as.integer(!is.na(empleo_temporal_agencias) & empleo_temporal_agencias > 0),
      invierte = as.integer(!is.na(inversion) & inversion > 0),
      usa_aprendices = as.integer(!is.na(empleo_aprendices) & empleo_aprendices > 0),
      compra_maquinaria = as.integer(!is.na(maquinaria_nueva) & maquinaria_nueva > 0)
    )

  winsorizar <- function(x) {
    lim <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
    pmin(pmax(x, lim[1]), lim[2])
  }

  base <- base %>% mutate(margen = winsorizar(margen), peso_costo = winsorizar(peso_costo))

  MEDIDAS <- c("Bite2022_obreros", "Exposure2022_obreros", "golpe_c", "golpe_a", "golpe_costo")

  for (m in MEDIDAS) {
    if (m %in% names(base)) {
      base[[paste0(m, "_de")]] <- {
        w <- winsorizar(base[[m]])
        w / sd(w, na.rm = TRUE)
      }
    }
  }

  datos <- base %>% filter(ANIO %in% c(2015:2019, 2021:2024))

  cat("Firmas-año en la ventana de estimación:", nrow(datos), "\n")
  cat("Firmas distintas:", n_distinct(datos$NORDEMP), "\n")

  # ============================================================================
  # COPIA LITERAL de estudio_evento() (sección 2 de 05_resultados_y_mecanismos.R)
  # ============================================================================

  EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F"
  EFECTOS_TAMANO <- paste(EFECTOS, "+ tamano_2022^ANIO_F")

  estudio_evento <- function(outcome, tratamiento, base_datos = datos,
                             previos = "2015|2016|2017|2018|2019|2021",
                             efectos = EFECTOS) {
    v <- paste0(tratamiento, "_de")
    if (!v %in% names(base_datos) || !outcome %in% names(base_datos)) return(NULL)

    formula <- as.formula(paste0(outcome, " ~ i(ANIO_F, ", v, ", ref = '2022') | ", efectos))
    modelo <- tryCatch(feols(formula, data = base_datos, cluster = ~NORDEMP),
                       error = function(e) NULL)
    if (is.null(modelo)) return(NULL)

    coefs <- as.data.frame(coeftable(modelo))
    tabla <- tibble(
      anio = as.integer(gsub(paste0("ANIO_F::|:", v), "", rownames(coefs))),
      coeficiente = coefs[["Estimate"]],
      error_estandar = coefs[["Std. Error"]],
      p_valor = coefs[["Pr(>|t|)"]]
    ) %>%
      bind_rows(tibble(anio = 2022L, coeficiente = 0, error_estandar = 0, p_valor = NA_real_)) %>%
      arrange(anio)

    p_previos <- tryCatch(
      wald(modelo, keep = paste0("ANIO_F::(", previos, "):"), print = FALSE)$p,
      error = function(e) NA_real_)

    list(tabla = tabla, p_previos = p_previos, modelo = modelo, variable = v)
  }

  # ============================================================================
  # Estimación de los 12 estudios de evento (6 variables x 2 especificaciones)
  # ============================================================================

  CONTROLES_TENDENCIAS <- c("Sin control por tamaño" = EFECTOS,
                            "Con control por tamaño" = EFECTOS_TAMANO)

  estimar_variable <- function(variable, etiqueta, control) {
    evento <- estudio_evento(variable, "Bite2022_obreros", efectos = CONTROLES_TENDENCIAS[[control]])
    if (is.null(evento)) {
      cat("AVISO: no se pudo estimar", variable, "con", control, "- se omite\n")
      return(NULL)
    }
    evento$tabla %>%
      mutate(variable = variable, etiqueta = etiqueta, control = control,
             efecto_pct = 100 * coeficiente,
             ic95_inf_pct = 100 * (coeficiente - 1.96 * error_estandar),
             ic95_sup_pct = 100 * (coeficiente + 1.96 * error_estandar),
             p_previos = evento$p_previos,
             observaciones = nobs(evento$modelo))
  }

  resultados <- list()
  for (i in seq_len(nrow(VARIABLES_TENDENCIAS))) {
    for (control in names(CONTROLES_TENDENCIAS)) {
      resultados[[length(resultados) + 1]] <- estimar_variable(
        VARIABLES_TENDENCIAS$variable[i], VARIABLES_TENDENCIAS$etiqueta[i], control)
    }
  }

  coeficientes_tendencias <- bind_rows(Filter(Negate(is.null), resultados)) %>%
    select(variable, etiqueta, control, anio, efecto_pct, ic95_inf_pct, ic95_sup_pct,
           p_valor, p_previos, observaciones)

  write_csv(coeficientes_tendencias, RUTA_CSV)
  cat("Coeficientes guardados en", RUTA_CSV, "\n")

} else {
  cat("REESTIMAR = FALSE y", RUTA_CSV, "ya existe: no se estima nada, solo se grafica.\n")
  coeficientes_tendencias <- read_csv(RUTA_CSV, show_col_types = FALSE)
}

# ==============================================================================
# GRÁFICO
# ==============================================================================

formatear_p <- function(p) {
  ifelse(p < 0.001, "<0,001", sub("\\.", ",", formatC(p, format = "f", digits = 3)))
}

titulos_panel <- coeficientes_tendencias %>%
  distinct(variable, etiqueta, control, p_previos) %>%
  mutate(control_corto = ifelse(control == "Con control por tamaño", "con", "sin")) %>%
  select(variable, etiqueta, control_corto, p_previos) %>%
  pivot_wider(names_from = control_corto, values_from = p_previos) %>%
  mutate(titulo = paste0(etiqueta, "\np previos: ", formatear_p(sin), " (sin) | ",
                        formatear_p(con), " (con)"))

niveles_titulo <- titulos_panel$titulo[match(VARIABLES_TENDENCIAS$variable, titulos_panel$variable)]

datos_grafico <- coeficientes_tendencias %>%
  left_join(select(titulos_panel, variable, titulo), by = "variable") %>%
  mutate(titulo = factor(titulo, levels = niveles_titulo),
         control = factor(control, levels = c("Sin control por tamaño", "Con control por tamaño")))

PALETA_TENDENCIAS <- c("Sin control por tamaño" = "grey30",
                       "Con control por tamaño" = "#1F4E79")
FORMAS_TENDENCIAS <- c("Sin control por tamaño" = 16, "Con control por tamaño" = 17)

grafico_tendencias <- ggplot(datos_grafico,
                             aes(x = anio, y = efecto_pct, color = control, shape = control)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = ic95_inf_pct, ymax = ic95_sup_pct),
                position = position_dodge(width = 0.35), linewidth = 0.4, width = 0) +
  geom_point(position = position_dodge(width = 0.35), size = 1.6) +
  facet_wrap(~ titulo, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  scale_color_manual(values = PALETA_TENDENCIAS) +
  scale_shape_manual(values = FORMAS_TENDENCIAS) +
  labs(x = NULL, y = "Efecto frente a 2022 (%)", color = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 9),
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(grafico_tendencias)
ggsave(RUTA_PNG, grafico_tendencias, width = 16, height = 20, units = "cm", dpi = 300, bg = "white")
cat("Gráfico guardado en", RUTA_PNG, "\n")
