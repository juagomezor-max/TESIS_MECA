# ==============================================================================
# 13_auditoria_seccion4_empleo.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# SEGUNDA AUDITORÍA DE LA SECCIÓN 4 DE 11_empleo_sin_traslape.R
#
# POR QUÉ ESTE SCRIPT
# 11_empleo_sin_traslape.R y 12_detalle_tendencias_previas.R concluyeron que
# el efecto de empleo sobrevive al traslape aritmético específico (secciones
# 1-3), pero que una tendencia previa grande y sistemática (sección 4) impide
# leerlo como efecto causal del choque de 2023. Antes de dar esa conclusión
# por buena, dos preguntas quedaban sin probar formalmente:
#
#   A. ¿La tendencia previa es un patrón genuinamente LINEAL (sistemático,
#      describible con una sola pendiente), o es solo la forma que toma
#      CUALQUIER conjunto de coeficientes de un event study al normalizarse a
#      cero en el año de referencia (2022)? Si fuera lo segundo, "converge
#      hacia 0 al acercarse a 2022" no sería evidencia de nada -- sería una
#      propiedad mecánica de la parametrización, no un patrón económico.
#
#   B. ¿El rechazo depende específicamente de 2019 -el mismo año que se usa
#      para construir la exposición-, lo que sugeriría un problema de
#      traslape distinto (compartir año de exposición con un año del
#      pre-período), o es un patrón parejo en todo 2015-2021 que no depende
#      de ese año en particular?
#
# QUÉ HACE
#   1. Prueba de tendencia LINEAL: en vez de una dummy por año, un solo
#      coeficiente de interacción entre la exposición y un año lineal
#      centrado, estimado SOLO en el pre-período (2015-2019, 2021, es decir,
#      antes o en el año de referencia). Un coeficiente lineal significativo
#      es evidencia de tendencia sistemática, independiente de qué año se
#      elija como referencia (no es un artefacto de normalizar a 2022).
#   2. La misma prueba lineal EXCLUYENDO 2019, para descartar que el patrón
#      dependa de compartir el año de exposición con un año del pre-período.
#   3. Verificación de que 2019 no es un valor atípico dentro de la serie
#      2015-2021 (ni el máximo ni el mínimo en valor absoluto).
#
# NO SE CAMBIA la construcción de las medidas ni de los controles -- se
# reutiliza exactamente la de 11_empleo_sin_traslape.R / 12_detalle_
# tendencias_previas.R.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  4. RESULTADOS/13_auditoria_seccion4_empleo/
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "13_auditoria_seccion4_empleo")
dir.create(CARPETA, recursive = TRUE, showWarnings = FALSE)

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
# 0. PREPARACIÓN (idéntica a 11_empleo_sin_traslape.R)
# ==============================================================================
titulo("0. DATOS Y PREPARACIÓN (igual que 11 y 12)")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

SALARIO_MINIMO <- tibble(
  anio  = 2008:2024,
  valor = c(461500, 496900, 515000, 535600, 566700, 589500, 616000, 644350,
            689455, 737717, 781242, 828116, 877803, 908526, 1000000,
            1160000, 1300000)
)

RAZON_COSTO_MINIMO <- 1.531
MEDIDAS <- c(bite = "Bite (Kaitz de obreros)", golpe_c = "Golpe C (3 categorías)",
             golpe_a = "Golpe A (ponderada)", golpe_costo = "Golpe costo",
             exposure = "Exposure (% obreros)")

base <- panel %>%
  mutate(
    empleo = empleo_total_sin_propietarios,
    log_empleo = log(ifelse(empleo > 0, empleo, NA_real_)),
    w_obrero_sueldos = ifelse(obreros_permanentes > 0 & sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 / obreros_permanentes / 12 * 1000, NA_real_),
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
    n_categorias = (!is.na(w_obrero)) + (!is.na(w_prof)) + (!is.na(w_admin)),
    w_firma = ifelse(n_categorias > 0, rowSums(cbind(w_obrero, w_prof, w_admin), na.rm = TRUE) / n_categorias, NA_real_),
    c_firma = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                     costos_totales_personal_total_c3r10c3 / empleo / 12 * 1000, NA_real_),
    permanentes_total = rowSums(cbind(
      ifelse(is.na(w_obrero), 0, obreros_permanentes),
      ifelse(is.na(w_prof), 0, profesional_tecnico_permanentes),
      ifelse(is.na(w_admin), 0, administrativos_permanentes)), na.rm = TRUE),
    exposure_obreros = ifelse(empleo > 0, obreros_total_ocupado / empleo, NA_real_),
    ANIO_F = factor(ANIO)
  )

clasificacion_2022 <- panel %>% filter(ANIO == 2022) %>%
  transmute(NORDEMP, sector_2022 = factor(CIIU4), depto_2022 = factor(DPTO),
            tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande")))
base <- base %>% left_join(clasificacion_2022, by = "NORDEMP")

construir_medidas <- function(anio_exposicion, anio_choque) {
  minimo <- SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == anio_choque]
  if (length(minimo) == 0) return(NULL)
  costo_minimo <- minimo * RAZON_COSTO_MINIMO
  medidas <- base %>% filter(ANIO == anio_exposicion) %>%
    transmute(NORDEMP,
      bite = ifelse(!is.na(w_obrero_sueldos) & w_obrero_sueldos > 0, minimo / w_obrero_sueldos, NA_real_),
      golpe_c = ifelse(!is.na(w_firma) & w_firma > 0, minimo / w_firma, NA_real_),
      golpe_a = ifelse(permanentes_total > 0 & n_categorias > 0,
        (ifelse(is.na(w_obrero), 0, obreros_permanentes/permanentes_total*minimo/w_obrero) +
         ifelse(is.na(w_prof), 0, profesional_tecnico_permanentes/permanentes_total*minimo/w_prof) +
         ifelse(is.na(w_admin), 0, administrativos_permanentes/permanentes_total*minimo/w_admin)), NA_real_),
      golpe_costo = ifelse(!is.na(c_firma) & c_firma > 0, costo_minimo / c_firma, NA_real_),
      exposure = exposure_obreros)
  medidas %>% mutate(across(c(bite, golpe_c, golpe_a, golpe_costo, exposure), ~ {
    if (all(is.na(.x))) return(.x)
    lim <- quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE)
    w <- pmin(pmax(.x, lim[1]), lim[2]); w / sd(w, na.rm = TRUE)
  }))
}

EFECTOS <- "NORDEMP + ANIO_F + sector_2022^ANIO_F + tamano_2022^ANIO_F + depto_2022^ANIO_F"
ANIOS_PREVIOS <- c(2015, 2016, 2017, 2018, 2019, 2021)

exposicion_2019 <- construir_medidas(2019, 2023) %>% rename_with(~paste0(.x, "_de"), -NORDEMP)
base_evento <- base %>% left_join(exposicion_2019, by = "NORDEMP") %>%
  filter(ANIO %in% c(2015:2019, 2021:2024))


# ==============================================================================
# 1. TENDENCIA PREVIA LINEAL (2015-2021), CON Y SIN 2019
# ==============================================================================
titulo("1. TENDENCIA PREVIA LINEAL, PRE-PERÍODO (2015-2019, 2021)")

# En vez de una dummy por año (que se normaliza a 0 en el año de referencia,
# 2022, por construcción), se interactúa la exposición con el AÑO como
# variable continua (centrada en 2021, el último año pre-período, para que el
# coeficiente sea interpretable como el cambio anual promedio). Esta prueba
# NO depende de qué año se elija como referencia: es una pendiente estimada
# solo con los años 2015-2019 y 2021, sin tocar 2022 ni el post-período.
pre_periodo <- base_evento %>% filter(ANIO %in% ANIOS_PREVIOS) %>%
  mutate(anio_centrado = ANIO - 2021)

tendencia_lineal <- function(m, datos = pre_periodo) {
  v <- paste0(m, "_de")
  formula <- as.formula(paste0("log_empleo ~ anio_centrado:", v, " | ", EFECTOS))
  modelo <- tryCatch(feols(formula, data = datos, cluster = ~NORDEMP), error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  nombre <- paste0("anio_centrado:", v)
  if (!nombre %in% rownames(coeftable(modelo))) return(NULL)
  f <- coeftable(modelo)[nombre, ]
  tibble(medida = MEDIDAS[[m]], clave = m,
         pendiente_anual_pct = 100 * f[["Estimate"]], ee_pct = 100 * f[["Std. Error"]],
         p_valor = f[["Pr(>|t|)"]], significancia = estrellas(f[["Pr(>|t|)"]]), n = nobs(modelo))
}

lineal_con_2019 <- bind_rows(lapply(names(MEDIDAS), tendencia_lineal))
ver(lineal_con_2019)
guardar_tabla(lineal_con_2019, "T1_tendencia_lineal_pre_periodo",
              "Tabla 1. Pendiente anual pre-período (2015-2019, 2021), interacción exposición x año centrado")

pre_periodo_sin2019 <- pre_periodo %>% filter(ANIO != 2019)
lineal_sin_2019 <- bind_rows(lapply(names(MEDIDAS), function(m) tendencia_lineal(m, pre_periodo_sin2019)))
ver(lineal_sin_2019)
guardar_tabla(lineal_sin_2019, "T2_tendencia_lineal_sin_2019",
              "Tabla 2. Misma pendiente anual, excluyendo 2019 (el año que también se usa para construir la exposición)")

comparacion_lineal <- lineal_con_2019 %>%
  select(medida, clave, pendiente_con_2019 = pendiente_anual_pct, p_con_2019 = p_valor) %>%
  left_join(lineal_sin_2019 %>% select(clave, pendiente_sin_2019 = pendiente_anual_pct, p_sin_2019 = p_valor),
            by = "clave")
ver(comparacion_lineal)
guardar_tabla(comparacion_lineal, "T3_comparacion_con_sin_2019",
              "Tabla 3. Pendiente anual del pre-período, con y sin 2019 -- si 2019 fuera el problema, la pendiente cambiaría mucho al quitarlo")

cat("\nCÓMO LEER:\n",
    " - Una pendiente anual negativa y significativa (columna pendiente_con_2019)\n",
    "   es evidencia de tendencia previa SISTEMÁTICA, no del año de referencia\n",
    "   elegido: esta prueba no usa 2022 en absoluto, solo 2015-2019 y 2021.\n",
    " - Si la pendiente CON 2019 y SIN 2019 son parecidas (mismo signo, magnitud\n",
    "   similar, ambas significativas o ambas no), el patrón NO depende de que\n",
    "   2019 sea también el año de la exposición -- es un patrón parejo en todo\n",
    "   el pre-período. Si la pendiente cambia mucho o pierde significancia al\n",
    "   quitar 2019, ese año específico sí es parte de la explicación.\n")


# ==============================================================================
# 2. ¿ES 2019 UN VALOR ATÍPICO DENTRO DE LA SERIE 2015-2021?
# ==============================================================================
titulo("2. ¿2019 ES UN VALOR ATÍPICO DENTRO DE 2015-2021?")

# Reconstruye los coeficientes año a año (idénticos a 11/12) para verificar,
# sin ambigüedad, si el coeficiente de 2019 se aparta del resto de la serie
# 2015-2021 o si es uno más dentro de un rango consistente.
estudio_evento <- function(m) {
  v <- paste0(m, "_de")
  formula <- as.formula(paste0("log_empleo ~ i(ANIO_F, ", v, ", ref = '2022') | ", EFECTOS))
  modelo <- tryCatch(feols(formula, data = base_evento, cluster = ~NORDEMP), error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  coefs <- as.data.frame(coeftable(modelo))
  tibble(anio = as.integer(gsub(paste0("ANIO_F::|:", v), "", rownames(coefs))),
         efecto_pct = 100 * coefs[["Estimate"]]) %>%
    mutate(medida = MEDIDAS[[m]], clave = m)
}

serie_completa <- bind_rows(lapply(names(MEDIDAS), estudio_evento))

atipicidad_2019 <- serie_completa %>%
  filter(anio %in% ANIOS_PREVIOS) %>%
  group_by(medida, clave) %>%
  summarise(
    coef_2019 = efecto_pct[anio == 2019],
    rango_pre_min = min(efecto_pct), rango_pre_max = max(efecto_pct),
    es_el_maximo_en_magnitud = abs(coef_2019) == max(abs(efecto_pct)),
    posicion_en_magnitud = rank(-abs(efecto_pct))[anio == 2019],
    .groups = "drop"
  )

ver(atipicidad_2019)
guardar_tabla(atipicidad_2019, "T4_atipicidad_2019",
              "Tabla 4. Coeficiente de 2019 frente al rango 2015-2021 -- ¿es el más extremo de la serie?", decimales = 3)

cat("\nCÓMO LEER: 'posicion_en_magnitud' = 1 significa que 2019 es el año MÁS\n",
    "extremo (en valor absoluto) de los 6 años del pre-período. Si 2019 no\n",
    "ocupa el primer lugar, no se distingue especialmente del resto de la\n",
    "serie -- es un punto más dentro de una trayectoria pareja, no un año\n",
    "atípico que arrastre el resultado por compartir el año de exposición.\n")

if (length(compendio) > 0) save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
titulo("FIN DEL SCRIPT")
