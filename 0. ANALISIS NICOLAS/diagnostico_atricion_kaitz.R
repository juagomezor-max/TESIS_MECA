# diagnostico_atricion_kaitz.R
# Ejecutar desde la RAIZ del repositorio.
#
# Ausencia en un año no equivale a cierre ni a salida definitiva.
# Características medidas en 2022, antes del seguimiento.
# No modifica las bases originales.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(fixest)
})

ruta_analitico <- file.path(
  "1. DATOS", "panel_analitico_firma_eam.rds"
)
ruta_completo <- file.path(
  "1. DATOS", "panel_firma_eam_expalt_completo.rds"
)
salidas <- file.path("4. RESULTADOS", "diagnostico_atricion_kaitz")
dir.create(salidas, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(ruta_analitico) || !file.exists(ruta_completo)) {
  stop("Faltan las bases. Ejecuta desde la raíz del repositorio.")
}

normalizar <- function(x) {
  x %>%
    mutate(
      NORDEMP = as.character(NORDEMP),
      ANIO = as.integer(as.character(ANIO))
    )
}

panel <- normalizar(read_rds(ruta_analitico))
completo <- normalizar(read_rds(ruta_completo))

requeridas <- c(
  "NORDEMP", "ANIO", "Bite2022_obreros",
  "CIIU4", "DPTO", "tamano_empresa", "empleo_total",
  "obreros_permanentes", "sueldos_permanentes_obreros_c3r2c1"
)
faltantes <- setdiff(requeridas, names(panel))
if (length(faltantes)) {
  stop("Faltan columnas: ", paste(faltantes, collapse = ", "))
}
if (!all(c("NORDEMP", "ANIO") %in% names(completo))) {
  stop("El panel completo no contiene NORDEMP y ANIO.")
}

base <- panel %>% filter(ANIO == 2022)

if (anyDuplicated(base$NORDEMP)) {
  stop("Hay más de una fila por firma en 2022. Revisar antes de continuar.")
}
if (any(is.na(base$NORDEMP) | base$NORDEMP == "")) {
  stop("Hay identificadores inválidos en la base de 2022.")
}
if (!all(c(2022, 2023, 2024) %in% completo$ANIO)) {
  stop("El panel completo debe contener 2022, 2023 y 2024.")
}
if (!all(base$NORDEMP %in%
         completo$NORDEMP[completo$ANIO == 2022])) {
  stop("Las bases no coinciden en las firmas de 2022.")
}

# El script principal conserva firmas con Kaitz no faltante.
# Se detiene si hay valores inválidos, en vez de filtrarlos silenciosamente.
valores <- base$Bite2022_obreros[!is.na(base$Bite2022_obreros)]
if (length(valores) < 5 ||
    any(!is.finite(valores) | valores <= 0)) {
  stop("Revisar cobertura o valores inválidos del Kaitz.")
}

limites <- quantile(valores, c(0.01, 0.99))
kaitz_recortado <- pmin(pmax(valores, limites[1]), limites[2])
de_kaitz <- sd(kaitz_recortado)

if (!is.finite(de_kaitz) || de_kaitz <= 0) {
  stop("El Kaitz no tiene variación suficiente.")
}

base <- base %>%
  mutate(
    con_kaitz = !is.na(Bite2022_obreros),
    kaitz = pmin(pmax(Bite2022_obreros, limites[1]), limites[2]),
    kaitz_de = kaitz / de_kaitz,
    sector_2022 = factor(CIIU4),
    depto_2022 = factor(DPTO),
    tamano_2022 = factor(tamano_empresa),
    # Sueldos anuales en miles de pesos; excluye prestaciones.
    salario_obrero_mensual_pesos = ifelse(
      obreros_permanentes > 0,
      sueldos_permanentes_obreros_c3r2c1 /
        obreros_permanentes * 1000 / 12,
      NA_real_
    )
  )

for (anio in c(2023, 2024)) {
  ids <- completo$NORDEMP[completo$ANIO == anio]
  base[[paste0("ausente_", anio)]] <-
    as.integer(!base$NORDEMP %in% ids)
}

# Quintiles: orden por Kaitz original, antes de recortar extremos.
# La winsorización conserva el orden, pero crea empates en las colas.
cohorte <- base %>%
  filter(con_kaitz) %>%
  arrange(Bite2022_obreros, NORDEMP) %>%
  mutate(quintil = paste0("Q", ntile(Bite2022_obreros, 5)))

# 1. Tasas de ausencia por quintil.
tasas <- bind_rows(lapply(c(2023, 2024), function(anio) {
  variable <- paste0("ausente_", anio)
  
  cohorte %>%
    group_by(quintil) %>%
    summarise(
      n_firmas = n(),
      n_ausentes = sum(.data[[variable]]),
      tasa = mean(.data[[variable]]),
      .groups = "drop"
    ) %>%
    mutate(
      anio = anio,
      tasa_ausencia_pct = 100 * tasa,
      se_pp = 100 * sqrt(tasa * (1 - tasa) / n_firmas)
    ) %>%
    select(-tasa)
}))

brechas <- bind_rows(lapply(c(2023, 2024), function(anio) {
  t <- filter(tasas, .data$anio == anio)
  q1 <- filter(t, quintil == "Q1")
  q5 <- filter(t, quintil == "Q5")
  diferencia <- q5$tasa_ausencia_pct - q1$tasa_ausencia_pct
  error <- sqrt(q1$se_pp^2 + q5$se_pp^2)
  
  tibble(
    anio = anio,
    brecha_q5_q1_pp = diferencia,
    se_pp = error,
    ic95_bajo_pp = diferencia - 1.96 * error,
    ic95_alto_pp = diferencia + 1.96 * error,
    p_valor = if (error > 0) {
      2 * pnorm(-abs(diferencia / error))
    } else {
      NA_real_
    }
  )
}))

# 2. Regresiones: cambio en probabilidad de ausencia por 1 DE de Kaitz.
# Misma muestra completa de controles en las tres especificaciones.
regresiones <- bind_rows(lapply(c(2023, 2024), function(anio) {
  variable <- paste0("ausente_", anio)
  d <- cohorte %>%
    filter(
      !is.na(sector_2022), !is.na(depto_2022),
      !is.na(tamano_2022)
    )
  
  formulas <- c(
    "Sin controles" = paste0(variable, " ~ kaitz_de"),
    "Sector + departamento" = paste0(
      variable, " ~ kaitz_de | sector_2022 + depto_2022"
    ),
    "Sector + departamento + tamaño" = paste0(
      variable,
      " ~ kaitz_de | sector_2022 + depto_2022 + tamano_2022"
    )
  )
  
  bind_rows(lapply(names(formulas), function(nombre) {
    m <- feols(
      as.formula(formulas[[nombre]]),
      data = d, vcov = "hetero"
    )
    if (!"kaitz_de" %in% names(coef(m))) {
      stop("Kaitz eliminado por colinealidad en: ", nombre)
    }
    intervalo <- confint(m)["kaitz_de", ]
    
    tibble(
      anio = anio,
      especificacion = nombre,
      n_firmas = nobs(m),
      coef_por_de_pp = 100 * unname(coef(m)["kaitz_de"]),
      ic95_bajo_pp = 100 * intervalo[1],
      ic95_alto_pp = 100 * intervalo[2],
      p_valor = unname(pvalue(m)["kaitz_de"])
    )
  }))
}))

# 3. Características iniciales: presentes frente a ausentes.
# Se usa toda la cohorte del panel analítico observada en 2022.
comparacion <- bind_rows(lapply(c(2023, 2024), function(anio) {
  base %>%
    mutate(
      anio_seguimiento = anio,
      grupo = ifelse(
        .data[[paste0("ausente_", anio)]] == 1,
        "Ausentes", "Presentes"
      )
    )
}))

numericas <- c(
  "empleo_total", "salario_obrero_mensual_pesos", "kaitz"
)

descriptivos <- comparacion %>%
  select(anio_seguimiento, grupo, all_of(numericas)) %>%
  pivot_longer(
    all_of(numericas), names_to = "variable", values_to = "valor"
  ) %>%
  group_by(anio_seguimiento, grupo, variable) %>%
  summarise(
    n_firmas = n(),
    n_validos = sum(is.finite(valor)),
    media = if (any(is.finite(valor))) {
      mean(valor[is.finite(valor)])
    } else NA_real_,
    mediana = if (any(is.finite(valor))) {
      median(valor[is.finite(valor)])
    } else NA_real_,
    .groups = "drop"
  )

composicion <- comparacion %>%
  transmute(
    anio_seguimiento, grupo,
    sector = as.character(CIIU4),
    departamento = as.character(DPTO),
    tamano = as.character(tamano_empresa)
  ) %>%
  pivot_longer(
    c(sector, departamento, tamano),
    names_to = "caracteristica", values_to = "categoria"
  ) %>%
  mutate(categoria = coalesce(categoria, "Sin dato")) %>%
  count(anio_seguimiento, grupo, caracteristica, categoria,
        name = "n_firmas") %>%
  group_by(anio_seguimiento, grupo, caracteristica) %>%
  mutate(pct = 100 * n_firmas / sum(n_firmas)) %>%
  ungroup()

# 4. Cobertura del Kaitz: selección adicional dentro de la base de 2022.
cobertura <- base %>%
  group_by(con_kaitz) %>%
  summarise(
    n_firmas = n(),
    empleo_promedio = mean(empleo_total, na.rm = TRUE),
    empleo_mediana = median(empleo_total, na.rm = TRUE),
    pct_ausentes_2023 = 100 * mean(ausente_2023),
    pct_ausentes_2024 = 100 * mean(ausente_2024),
    .groups = "drop"
  )

tablas <- list(
  tasas_por_quintil = tasas,
  brechas_q5_q1 = brechas,
  regresiones_continuas = regresiones,
  caracteristicas_presentes_ausentes = descriptivos,
  composicion_presentes_ausentes = composicion,
  cobertura_kaitz = cobertura
)

for (nombre in names(tablas)) {
  write_csv(tablas[[nombre]], file.path(salidas, paste0(nombre, ".csv")))
}

print(tasas)
print(brechas)
print(regresiones)
print(cobertura)

cat("\nResultados guardados en:", salidas, "\n")
cat("Firmas de 2022:", nrow(base), "\n")
cat("Firmas con Kaitz:", nrow(cohorte), "\n")
cat("Límites de winsorización:", limites, "\n")


getwd()

file.exists("1. DATOS/panel_analitico_firma_eam.rds")
file.exists("1. DATOS/panel_firma_eam_expalt_completo.rds")