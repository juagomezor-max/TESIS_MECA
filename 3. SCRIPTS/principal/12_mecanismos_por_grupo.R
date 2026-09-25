# ==============================================================================
# 12_mecanismos_por_grupo.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# ¿POR DÓNDE AJUSTAN LAS FIRMAS, Y QUIÉNES AJUSTAN POR DÓNDE?
#
# principal/11_resultados_y_mecanismos.R mostró dos cosas que se combinan aquí:
#
#   - El efecto sobre el costo laboral y sobre el empleo varía fuertemente por
#     tamaño de firma: +4,92% de costo y -2,42% de empleo en las pequeñas,
#     frente a +1,97% y +0,19% (ninguno significativo) en las grandes.
#   - Los márgenes de ajuste se mueven, pero casi todos comparten la misma
#     trayectoria previa descendente que el costo laboral.
#
# Este script responde la pregunta de política: si el choque golpea distinto
# según el tamaño, ¿también se absorbe por canales distintos?
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
#           1. DATOS/panel_analitico_firma_eam.rds
# Salidas:  4. RESULTADOS/Mecanismos_por_grupo/
# ==============================================================================

# ------------------------------------------------------------------------------
# LUGAR EN LA TESIS
#
# Capítulo:     5. Resultados (mecanismos, heterogeneidad)
# Cifra clave:  compresión salarial, 0,22 DE -- cuatro veces cualquier otro
#               canal, y estable en los tres grupos de tamaño (0,218 pequeñas
#               / 0,212 medianas / 0,179 grandes).
# Depende de:   principal/11_resultados_y_mecanismos.R (mismo problema de
#               tendencias previas en los mecanismos, resuelto aquí con la
#               lectura B -- ver "DOS DECISIONES DE MEDICIÓN" abajo)
# ------------------------------------------------------------------------------


# ==============================================================================
# DOS DECISIONES DE MEDICIÓN QUE HACEN COMPARABLES LOS MECANISMOS
# ==============================================================================
#
# 1. LECTURA B COMO PRINCIPAL, NO LECTURA A.
#
# Casi todos los mecanismos comparten la trayectoria descendente del costo
# laboral, porque la exposición se mide con el salario de 2022 y ese año es el
# punto mínimo de la serie. Leer el coeficiente de 2023 contra 2022 (lectura A)
# mezcla el salto real con el rebote mecánico.
#
# La lectura B mide el salto de 2023 contra la PROPIA trayectoria previa del
# mecanismo: toma la pendiente anual observada entre 2016 y 2019 y la descuenta.
# Si una variable venía cayendo 2 puntos por año y en 2023 se queda plana, B lo
# registra como un salto de +2, que es lo correcto: la firma cambió de
# comportamiento respecto de lo que venía haciendo.
#
# Así la tendencia previa deja de ser un obstáculo y pasa a ser la línea base.
# Es la forma de contestar "qué canal usaron más" sin exigir que las trayectorias
# previas sean planas.
#
# 2. ESCALA COMÚN PARA PODER RANKEAR.
#
# Los mecanismos vienen en unidades distintas: "% temporales" sube 0,5 puntos
# porcentuales, "producción" sube 3,9%. Esos números no se pueden comparar.
#
# Estandarizamos cada outcome a desviación estándar 1 (calculada en el año base
# 2022, entre firmas). Entonces cada coeficiente se lee como "cuántas
# desviaciones estándar del propio mecanismo se mueve ante una DE más de
# exposición", y sí se pueden ordenar de mayor a menor uso.
#
# CÓMO SE INTERPRETA ESTO EN LA TESIS. Son asociaciones dentro de un diseño con
# tendencias previas documentadas, no efectos causales limpios. La afirmación
# defendible es comparativa: "entre los márgenes observables, las firmas más
# expuestas se apoyaron relativamente más en X que en Y". Eso es suficiente para
# orientar política, y es lo que la evidencia soporta.
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

CARPETA <- file.path("4. RESULTADOS", "Mecanismos_por_grupo")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 30) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3, carpeta = CARPETA) {
  tw <- flextable(tabla)
  tw <- colformat_double(tw, digits = decimales)
  tw <- set_caption(tw, caption = titulo_tabla)
  tw <- autofit(tw)
  save_as_docx(tw, path = file.path(carpeta, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(carpeta, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tw
  cat("Tabla guardada:", nombre_archivo, "\n")
}

graficos <- list()
guardar_grafico <- function(g, nombre_archivo, carpeta = CARPETA, ancho = 9, alto = 6) {
  print(g)
  ggsave(file.path(carpeta, "figuras", paste0(nombre_archivo, ".png")),
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

COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"
COLOR_MEDIO <- "#E69F00"


# ==============================================================================
# 1. DATOS Y VARIABLES
# ==============================================================================
titulo("1. DATOS Y VARIABLES")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

bite <- read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO))) %>%
  filter(ANIO == 2022) %>%
  select(NORDEMP, Bite2022_obreros)

log_seguro <- function(x) log(ifelse(!is.na(x) & x > 0, x, NA_real_))

base <- panel %>%
  left_join(bite, by = "NORDEMP") %>%
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
    empleo_temporal = empleo_temporal_directo + empleo_temporal_agencias,
    
    participacion_permanente = ifelse(empleo_total > 0, empleo_permanente / empleo_total, NA_real_),
    participacion_temporal = ifelse(empleo_total > 0, empleo_temporal / empleo_total, NA_real_),
    participacion_agencias = ifelse(empleo_total > 0, empleo_temporal_agencias / empleo_total, NA_real_),
    
    w_obrero = ifelse(obreros_permanentes > 0,
                      sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes, NA_real_),
    w_admin = ifelse(administrativos_permanentes > 0,
                     sueldos_permanentes_administrativos_c3r2c2 / administrativos_permanentes, NA_real_),
    brecha_salarial = ifelse(!is.na(w_obrero) & !is.na(w_admin) & w_admin > 0,
                             w_obrero / w_admin, NA_real_),
    
    productividad = ifelse(empleo_total > 0 & valor_agregado_valagri > 0,
                           valor_agregado_valagri / empleo_total, NA_real_),
    
    # Intensidad de tercerización, normalizada por producción para que sea
    # comparable entre firmas de distinto tamaño
    intensidad_outsourcing = ifelse(produccion_bruta_prodbr2 > 0,
                                    outsourcing_total_c3r41c3 / produccion_bruta_prodbr2, NA_real_),
    intensidad_honorarios = ifelse(produccion_bruta_prodbr2 > 0,
                                   honorarios_servicios_tecnicos_total_c3r15c3 /
                                     produccion_bruta_prodbr2, NA_real_),
    intensidad_agencias_pago = ifelse(produccion_bruta_prodbr2 > 0,
                                      pago_agencias_temporales_total_c3r8c3 /
                                        produccion_bruta_prodbr2, NA_real_),
    
    log_empleo = log_seguro(empleo_total),
    log_permanente = log_seguro(empleo_permanente),
    log_temporal = log_seguro(empleo_temporal),
    log_costo = log_seguro(costo_trabajador),
    log_brecha_salarial = log_seguro(brecha_salarial),
    log_produccion = log_seguro(produccion_bruta_prodbr2),
    log_ventas = log_seguro(valor_ventas_valorven),
    log_productividad = log_seguro(productividad),
    log_valor_agregado = log_seguro(valor_agregado_valagri),
    log_inversion = log_seguro(inversion_bruta_invebrta),
    
    usa_agencias = as.integer(!is.na(empleo_temporal_agencias) & empleo_temporal_agencias > 0),
    hace_outsourcing = as.integer(!is.na(outsourcing_total_c3r41c3) & outsourcing_total_c3r41c3 > 0),
    
    ANIO_F = factor(ANIO),
    sector_2022 = factor(CIIU4),
    depto_2022 = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

winsorizar <- function(x) {
  lim <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, lim[1]), lim[2])
}
base$bite_de <- {
  w <- winsorizar(base$Bite2022_obreros)
  w / sd(w, na.rm = TRUE)
}

datos <- base %>% filter(ANIO %in% c(2015:2019, 2021:2024))

cat("Firmas-año:", nrow(datos), " | Firmas:", n_distinct(datos$NORDEMP), "\n")
cat("Por tamaño:\n")
print(table(datos$tamano_2022[datos$ANIO == 2022]))


# --- Catálogo de mecanismos -----------------------------------------------------
MECANISMOS <- tribble(
  ~variable,                  ~etiqueta,                              ~canal,                  ~unidad,
  "log_empleo",               "Empleo total",                         "Empleo",                "log",
  "log_permanente",           "Empleo permanente",                    "Empleo",                "log",
  "log_temporal",             "Empleo temporal",                      "Empleo",                "log",
  "participacion_permanente", "Participación de permanentes",         "Temporalidad",          "proporcion",
  "participacion_temporal",   "Participación de temporales",          "Temporalidad",          "proporcion",
  "participacion_agencias",   "Participación de agencias",            "Temporalidad",          "proporcion",
  "usa_agencias",             "Usa agencias (sí/no)",                 "Temporalidad",          "indicador",
  "log_brecha_salarial",      "Brecha salarial obrero/admin",         "Compresión salarial",   "log",
  "intensidad_outsourcing",   "Outsourcing / producción",             "Tercerización",         "proporcion",
  "intensidad_honorarios",    "Honorarios / producción",              "Tercerización",         "proporcion",
  "intensidad_agencias_pago", "Pago a agencias / producción",         "Tercerización",         "proporcion",
  "hace_outsourcing",         "Hace outsourcing (sí/no)",             "Tercerización",         "indicador",
  "log_inversion",            "Inversión bruta",                      "Capital",               "log",
  "log_produccion",           "Producción bruta",                     "Escala",                "log",
  "log_ventas",               "Ventas",                               "Escala",                "log",
  "log_valor_agregado",       "Valor agregado",                       "Escala",                "log",
  "log_productividad",        "Productividad (VA/trabajador)",        "Productividad",         "log"
)

# Estandarización de cada outcome a DE = 1, usando la dispersión entre firmas en
# 2022. Esto es lo que permite comparar mecanismos medidos en unidades distintas.
for (v in MECANISMOS$variable) {
  if (v %in% names(datos)) {
    de <- sd(datos[[v]][datos$ANIO == 2022], na.rm = TRUE)
    if (!is.na(de) && de > 0) datos[[paste0(v, "_std")]] <- datos[[v]] / de
  }
}

cat("Mecanismos con versión estandarizada:",
    sum(paste0(MECANISMOS$variable, "_std") %in% names(datos)), "de", nrow(MECANISMOS), "\n")


# ==============================================================================
# 2. FUNCIÓN DE ESTIMACIÓN
# ==============================================================================
titulo("2. ESPECIFICACIÓN")

EFECTOS_COMPLETO <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
EFECTOS_SIN_TAMANO <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + depto_2022^ANIO_F"

cat("Muestra completa:", EFECTOS_COMPLETO, "\n")
cat("Dentro de grupo de tamaño:", EFECTOS_SIN_TAMANO, "\n")
cat("(dentro de un grupo, el control de tamaño x año ya no aporta variación)\n")

# estimar_mecanismo(): devuelve las lecturas A y B para un outcome.
#   A = coeficiente de 2023 frente a 2022
#   B = ese salto medido contra la propia pendiente previa del mecanismo
estimar_mecanismo <- function(variable, base_datos, efectos) {
  if (!variable %in% names(base_datos)) return(NULL)
  
  formula <- as.formula(paste0(variable, " ~ i(ANIO_F, bite_de, ref = '2022') | ", efectos))
  modelo <- tryCatch(feols(formula, data = base_datos, cluster = ~NORDEMP),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  
  nombres_necesarios <- paste0("ANIO_F::", c(2016, 2019, 2023), ":bite_de")
  if (!all(nombres_necesarios %in% names(coef(modelo)))) return(NULL)
  
  sacar <- function(pesos) {
    nom <- paste0("ANIO_F::", names(pesos), ":bite_de")
    b <- coef(modelo)[nom]
    V <- vcov(modelo)[nom, nom, drop = FALSE]
    est <- sum(pesos * b)
    err <- sqrt(as.numeric(t(pesos) %*% V %*% pesos))
    c(est = est, err = err, p = 2 * pnorm(-abs(est / err)))
  }
  
  a <- sacar(c(`2023` = 1))
  b <- sacar(c(`2023` = 1, `2016` = 1/3, `2019` = -1/3))
  
  p_previos <- tryCatch(
    wald(modelo, keep = "ANIO_F::(2015|2016|2017|2018|2019|2021):", print = FALSE)$p,
    error = function(e) NA_real_)
  
  tibble(
    coef_A = a[["est"]], se_A = a[["err"]], p_A = a[["p"]],
    coef_B = b[["est"]], se_B = b[["err"]], p_B = b[["p"]],
    p_previos = p_previos,
    observaciones = nobs(modelo)
  )
}


# ==============================================================================
# 3. QUÉ CANALES SE USAN MÁS: MUESTRA COMPLETA
# ==============================================================================
titulo("3. RANKING DE MECANISMOS EN LA MUESTRA COMPLETA")

ranking <- bind_rows(lapply(seq_len(nrow(MECANISMOS)), function(i) {
  fila <- MECANISMOS[i, ]
  v_std <- paste0(fila$variable, "_std")
  r <- estimar_mecanismo(v_std, datos, EFECTOS_COMPLETO)
  if (is.null(r)) {
    cat("  omitido:", fila$etiqueta, "\n")
    return(NULL)
  }
  bind_cols(tibble(mecanismo = fila$etiqueta, canal = fila$canal), r)
}))

ranking <- ranking %>%
  mutate(
    p_A_ajustado = p.adjust(p_A, method = "BH"),
    p_B_ajustado = p.adjust(p_B, method = "BH"),
    sig_A = estrellas(p_A_ajustado),
    sig_B = estrellas(p_B_ajustado),
    ic_B_inf = coef_B - 1.96 * se_B,
    ic_B_sup = coef_B + 1.96 * se_B,
    magnitud = abs(coef_B)
  ) %>%
  arrange(desc(magnitud))

tabla_ranking <- ranking %>%
  select(canal, mecanismo,
         efecto_B_en_DE = coef_B, ic_B_inf, ic_B_sup, p_B_ajustado, sig_B,
         efecto_A_en_DE = coef_A, p_A_ajustado, sig_A,
         p_previos, observaciones)

ver(tabla_ranking)
guardar_tabla(tabla_ranking, "T01_ranking_mecanismos",
              "Tabla 1. Mecanismos de ajuste ordenados por magnitud, en desviaciones estándar del propio mecanismo")

cat("\nCÓMO LEER: el efecto está en DESVIACIONES ESTÁNDAR del propio mecanismo,\n",
    "por cada DE de exposición. Eso permite comparar canales medidos en\n",
    "unidades distintas y ordenarlos por cuánto se usan.\n",
    "La columna B es la principal: mide el salto de 2023 contra la trayectoria\n",
    "previa del propio mecanismo, así que una tendencia descendente previa no\n",
    "invalida la lectura, sirve de línea base.\n")

grafico_ranking <- ranking %>%
  ggplot(aes(x = reorder(mecanismo, coef_B), y = coef_B, color = canal)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = ic_B_inf, ymax = ic_B_sup)) +
  coord_flip() +
  labs(title = "¿Por dónde absorben las firmas el aumento del costo laboral?",
       subtitle = "Salto de 2023 frente a la trayectoria previa del propio mecanismo, por DE de exposición",
       x = NULL, y = "Efecto (desviaciones estándar del mecanismo)", color = NULL,
       caption = paste("Escala común: cada mecanismo estandarizado a DE = 1 en 2022.",
                       "\np-valores ajustados por Benjamini-Hochberg. Controles: firma, año, sector x año, tamaño x año, departamento x año.")) +
  tema_tesis
guardar_grafico(grafico_ranking, "G01_ranking_mecanismos", alto = 7)


# ==============================================================================
# 4. MECANISMOS DENTRO DE CADA GRUPO DE TAMAÑO
# ==============================================================================
titulo("4. MECANISMOS POR GRUPO DE TAMAÑO")

# La pregunta de política: el choque golpea más fuerte a las pequeñas
# (+4,92% de costo laboral frente a +1,97% en las grandes) y solo ahí se observa
# caída de empleo. ¿Usan también canales distintos para absorberlo?
#
# Estimamos cada mecanismo por separado dentro de cada grupo. Los coeficientes
# son directamente comparables entre grupos porque todos están en DE del propio
# mecanismo, calculadas sobre la muestra completa.

por_tamano <- bind_rows(lapply(c("Pequena", "Mediana", "Grande"), function(t) {
  sub <- filter(datos, tamano_2022 == t)
  cat("\nEstimando para firmas", t, "-", n_distinct(sub$NORDEMP), "firmas\n")
  
  bind_rows(lapply(seq_len(nrow(MECANISMOS)), function(i) {
    fila <- MECANISMOS[i, ]
    v_std <- paste0(fila$variable, "_std")
    r <- estimar_mecanismo(v_std, sub, EFECTOS_SIN_TAMANO)
    if (is.null(r)) return(NULL)
    bind_cols(tibble(tamano = t, mecanismo = fila$etiqueta, canal = fila$canal), r)
  }))
}))

# El ajuste por pruebas múltiples se hace DENTRO de cada grupo: son familias de
# hipótesis distintas y no tiene sentido penalizar un grupo por el número de
# pruebas hechas en otro.
por_tamano <- por_tamano %>%
  group_by(tamano) %>%
  mutate(p_B_ajustado = p.adjust(p_B, method = "BH")) %>%
  ungroup() %>%
  mutate(
    sig_B = estrellas(p_B_ajustado),
    ic_B_inf = coef_B - 1.96 * se_B,
    ic_B_sup = coef_B + 1.96 * se_B,
    tamano = factor(tamano, levels = c("Pequena", "Mediana", "Grande"))
  )

tabla_por_tamano <- por_tamano %>%
  select(tamano, canal, mecanismo, efecto_B_en_DE = coef_B,
         ic_B_inf, ic_B_sup, p_B_ajustado, sig_B, p_previos, observaciones) %>%
  arrange(tamano, desc(abs(efecto_B_en_DE)))

ver(tabla_por_tamano, filas = 60)
guardar_tabla(tabla_por_tamano, "T02_mecanismos_por_tamano",
              "Tabla 2. Mecanismos de ajuste dentro de cada grupo de tamaño")

# Comparación lado a lado: solo los mecanismos significativos en algún grupo
significativos_en_algun_grupo <- por_tamano %>%
  filter(sig_B != "") %>%
  pull(mecanismo) %>%
  unique()

cat("\nMecanismos significativos en al menos un grupo de tamaño:",
    if (length(significativos_en_algun_grupo) > 0)
      paste(significativos_en_algun_grupo, collapse = ", ") else "ninguno", "\n")

if (length(significativos_en_algun_grupo) > 0) {
  grafico_comparacion <- por_tamano %>%
    filter(mecanismo %in% significativos_en_algun_grupo) %>%
    ggplot(aes(x = reorder(mecanismo, coef_B), y = coef_B, color = tamano)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_pointrange(aes(ymin = ic_B_inf, ymax = ic_B_sup),
                    position = position_dodge(width = 0.6)) +
    coord_flip() +
    scale_color_manual(values = c(Pequena = COLOR_ALTA, Mediana = COLOR_MEDIO,
                                  Grande = COLOR_BAJA)) +
    labs(title = "Canales de ajuste según el tamaño de la firma",
         subtitle = "Salto de 2023 frente a la trayectoria previa, en DE del propio mecanismo",
         x = NULL, y = "Efecto (desviaciones estándar del mecanismo)", color = "Tamaño",
         caption = paste("Solo mecanismos significativos en al menos un grupo (Benjamini-Hochberg dentro de cada grupo).",
                         "\nControles: firma, año, sector x año y departamento x año.")) +
    tema_tesis
  guardar_grafico(grafico_comparacion, "G02_mecanismos_por_tamano", alto = 7)
}

# Panel completo, para el anexo
grafico_panel <- por_tamano %>%
  ggplot(aes(x = reorder(mecanismo, coef_B), y = coef_B, color = sig_B != "")) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_pointrange(aes(ymin = ic_B_inf, ymax = ic_B_sup), size = 0.4) +
  coord_flip() +
  facet_wrap(~ tamano) +
  scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = "grey65"),
                     labels = c("No significativo", "Significativo")) +
  labs(title = "Todos los mecanismos, por grupo de tamaño",
       subtitle = "Salto de 2023 frente a la trayectoria previa, en DE del propio mecanismo",
       x = NULL, y = "Efecto (DE del mecanismo)", color = NULL,
       caption = "p-valores ajustados dentro de cada grupo.") +
  tema_tesis +
  theme(axis.text.y = element_text(size = 8))
guardar_grafico(grafico_panel, "G03_panel_completo_por_tamano", ancho = 12, alto = 8)


# ==============================================================================
# 5. PERFIL DE AJUSTE DE CADA GRUPO
# ==============================================================================
titulo("5. PERFIL DE AJUSTE POR GRUPO")

# Resumimos por canal: cuánto se mueve cada familia de mecanismos en cada grupo.
# Esto es lo que se traduce a recomendaciones de política, porque responde
# "las firmas pequeñas se apoyan en X, las medianas en Y".

perfil <- por_tamano %>%
  group_by(tamano, canal) %>%
  summarise(
    mecanismos = n(),
    efecto_promedio = mean(coef_B, na.rm = TRUE),
    efecto_max = coef_B[which.max(abs(coef_B))],
    mecanismo_dominante = mecanismo[which.max(abs(coef_B))],
    n_significativos = sum(sig_B != ""),
    .groups = "drop"
  ) %>%
  arrange(tamano, desc(abs(efecto_max)))

ver(perfil, filas = 30)
guardar_tabla(perfil, "T03_perfil_por_canal",
              "Tabla 3. Perfil de ajuste: qué canal domina en cada grupo de tamaño")

# Mapa de calor: canal por tamaño
grafico_calor <- por_tamano %>%
  group_by(tamano, canal) %>%
  summarise(efecto = mean(coef_B, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = tamano, y = canal, fill = efecto)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(efecto, 3)), color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(low = COLOR_BAJA, mid = "grey85", high = COLOR_ALTA, midpoint = 0) +
  labs(title = "Intensidad de uso de cada canal de ajuste",
       subtitle = "Efecto promedio de los mecanismos de cada canal, en DE, por grupo de tamaño",
       x = NULL, y = NULL, fill = "Efecto (DE)",
       caption = "Promedio simple de los mecanismos dentro de cada canal. Ver la tabla 2 para el detalle.") +
  tema_tesis
guardar_grafico(grafico_calor, "G04_mapa_calor_canales", alto = 5)


# ==============================================================================
# 6. CONTEXTO: EL CHOQUE Y EL EMPLEO POR GRUPO
# ==============================================================================
titulo("6. CONTEXTO: TAMAÑO DEL CHOQUE Y RESPUESTA DE EMPLEO")

# Para interpretar los mecanismos hace falta saber cuán fuerte fue el choque en
# cada grupo. Un canal que se mueve poco donde el choque fue pequeño no es lo
# mismo que uno que se mueve poco donde el choque fue grande.

contexto <- bind_rows(lapply(c("Pequena", "Mediana", "Grande"), function(t) {
  sub <- filter(datos, tamano_2022 == t)
  bind_rows(lapply(c("log_costo", "log_empleo"), function(oc) {
    r <- estimar_mecanismo(oc, sub, EFECTOS_SIN_TAMANO)
    if (is.null(r)) return(NULL)
    bind_cols(tibble(tamano = t,
                     variable = ifelse(oc == "log_costo", "Costo laboral por trabajador",
                                       "Empleo total")), r)
  }))
})) %>%
  mutate(
    efecto_A_pct = 100 * coef_A,
    efecto_B_pct = 100 * coef_B,
    sig_A = estrellas(p_A),
    sig_B = estrellas(p_B),
    tamano = factor(tamano, levels = c("Pequena", "Mediana", "Grande"))
  ) %>%
  select(tamano, variable, efecto_A_pct, sig_A, efecto_B_pct, sig_B,
         p_previos, observaciones)

ver(contexto)
guardar_tabla(contexto, "T04_contexto_por_tamano",
              "Tabla 4. Magnitud del choque y respuesta de empleo por grupo de tamaño")

grafico_contexto <- contexto %>%
  ggplot(aes(x = tamano, y = efecto_A_pct, fill = variable)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(efecto_A_pct, 1), "%", sig_A)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(`Costo laboral por trabajador` = COLOR_BAJA,
                               `Empleo total` = COLOR_ALTA)) +
  labs(title = "El choque golpea más fuerte a las firmas pequeñas",
       subtitle = "Efecto en 2023 por DE de exposición, dentro de cada grupo de tamaño",
       x = NULL, y = "Efecto (%)", fill = NULL,
       caption = "Controles: firma, año, sector x año y departamento x año. Errores agrupados por firma.") +
  tema_tesis
guardar_grafico(grafico_contexto, "G05_contexto_tamano")


# ==============================================================================
# 7. SÍNTESIS PARA POLÍTICA
# ==============================================================================
titulo("7. SÍNTESIS")

# Armamos la tabla que va directo al documento: para cada grupo, cuánto le pegó
# el choque, qué pasó con su empleo, y cuál fue su canal principal de ajuste.

sintesis <- contexto %>%
  select(tamano, variable, efecto_A_pct, sig_A) %>%
  pivot_wider(names_from = variable, values_from = c(efecto_A_pct, sig_A)) %>%
  left_join(
    por_tamano %>%
      filter(sig_B != "") %>%
      group_by(tamano) %>%
      slice_max(abs(coef_B), n = 1, with_ties = FALSE) %>%
      select(tamano, canal_principal = mecanismo, efecto_canal = coef_B),
    by = "tamano"
  ) %>%
  left_join(
    por_tamano %>%
      group_by(tamano) %>%
      summarise(canales_significativos = sum(sig_B != ""), .groups = "drop"),
    by = "tamano"
  )

ver(sintesis)
guardar_tabla(sintesis, "T05_sintesis_politica",
              "Tabla 5. Síntesis: choque, empleo y canal principal de ajuste por tamaño")

cat("
CÓMO USAR ESTOS RESULTADOS EN LA SECCIÓN DE POLÍTICA:

  1. EL CHOQUE NO ES UNIFORME. La tabla 4 muestra que el mismo aumento nacional
     del mínimo se traduce en presiones de costo muy distintas según el tamaño
     de la firma. Esa es la base de cualquier recomendación diferenciada.

  2. LA RESPUESTA TAMPOCO. Si el empleo cae solo en un grupo, el efecto agregado
     nulo esconde reasignación entre firmas, no ausencia de efecto. Eso tiene
     implicaciones distintas para política que un cero genuino.

  3. LOS CANALES DIFIEREN. La tabla 2 y el mapa de calor dicen por dónde absorbe
     cada grupo. Un canal que aparece en las pequeñas y no en las grandes sugiere
     que la capacidad de ajuste, no solo la magnitud del choque, varía con el
     tamaño.

  4. CÓMO FORMULAR LA RECOMENDACIÓN. Lo defendible es condicional y comparativo:
     'entre los márgenes observables en la EAM, las firmas pequeñas más expuestas
     se apoyaron relativamente más en X'. No: 'el salario mínimo causó X'.
     La primera formulación resiste preguntas; la segunda no, porque el diseño
     tiene tendencias previas documentadas.

  5. QUÉ DECLARAR. Que los mecanismos son exploratorios, que la lectura usada
     (B) descuenta la trayectoria previa de cada variable, que los p-valores
     están ajustados por pruebas múltiples dentro de cada grupo, y que la EAM
     solo observa manufactura formal.

  6. LO QUE NO SE PUEDE DECIR. Que estos son los únicos canales: la EAM no
     observa precios, rotación, horas ni estándares de desempeño, que Hirsch,
     Kaufman y Zelenska (2015) documentan como márgenes relevantes. La ausencia
     de evidencia sobre un canal no observable no es evidencia de su ausencia.
")

save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
cat("\nCompendio guardado con", length(compendio), "tablas.\n")

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

