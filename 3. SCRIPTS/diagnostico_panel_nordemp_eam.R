# Diagnostico de NORDEMP como identificador longitudinal en EAM
#
# Este script valida si NORDEMP puede usarse como identificador de empresa en
# un panel anual (2008-2024), y genera salidas tabulares y graficas en:
# - Tablas en 1. DATOS/6. BASES_DERIVADAS/panel_diagnostico/
# - Graficas en 4. RESULTADOS/panel_diagnostico/
# El foco no es econometrico sino de calidad de identificacion longitudinal.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# -----------------------------
# 1) Configuracion de rutas
# -----------------------------

plot_dir <- here::here("4. RESULTADOS", "panel_diagnostico")
data_output_dir <- here::here("1. DATOS", "6. BASES_DERIVADAS", "panel_diagnostico")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)

# Se prueban rutas candidatas para mantener compatibilidad con versiones previas
# del flujo.
macro_candidates <- c(
  here::here("1. DATOS", "5. MACROBASE", "macro_base_eam.rds"),
  here::here("4. RESULTADOS", "macro_base_eam.rds")
)

macro_path <- macro_candidates[file.exists(macro_candidates)][1]

if (is.na(macro_path) || !nzchar(macro_path)) {
  stop(
    "No se encontro macro_base_eam.rds en rutas candidatas:\n",
    paste0("- ", macro_candidates, collapse = "\n")
  )
}

message("Leyendo macrobase desde: ", macro_path)

# -----------------------------
# 2) Carga y chequeos basicos
# -----------------------------

macro_base <- readr::read_rds(macro_path)

required_cols <- c("NORDEMP", "ANIO")
missing_cols <- setdiff(required_cols, names(macro_base))

if (length(missing_cols) > 0) {
  stop("Faltan columnas requeridas en macro_base: ", paste(missing_cols, collapse = ", "))
}

panel <- macro_base %>%
  mutate(
    NORDEMP = as.character(NORDEMP),
    ANIO = suppressWarnings(as.integer(ANIO))
  ) %>%
  # Se excluyen identificadores vacios para no inflar artificialmente duplicados
  # o trayectorias incompletas.
  filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO))

if (nrow(panel) == 0) {
  stop("La base quedo vacia despues de filtrar NORDEMP/ANIO validos.")
}

expected_years <- 2008:2024
total_expected_years <- length(expected_years)

# -----------------------------
# 3) Duplicados NORDEMP-ANIO
# -----------------------------

duplicados_nordemp_anio <- panel %>%
  # Este es el chequeo minimo de llave panel: una firma no deberia repetirse
  # dentro del mismo anio si NORDEMP identifica empresa-anio de forma unica.
  count(NORDEMP, ANIO, name = "n_obs") %>%
  filter(n_obs > 1) %>%
  arrange(desc(n_obs), NORDEMP, ANIO)

# -----------------------------
# 4) Empresas unicas por anio
# -----------------------------

empresas_unicas_por_anio <- panel %>%
  group_by(ANIO) %>%
  summarise(empresas_unicas = n_distinct(NORDEMP), .groups = "drop") %>%
  arrange(ANIO)

# -----------------------------
# 5) Anios observados por empresa
# -----------------------------

anios_por_empresa <- panel %>%
  distinct(NORDEMP, ANIO) %>%
  group_by(NORDEMP) %>%
  summarise(
    n_anios = n_distinct(ANIO),
    anio_min = min(ANIO),
    anio_max = max(ANIO),
    .groups = "drop"
  )

# Distribucion de permanencia (1 anio, 2 anios, etc.)
distribucion_permanencia <- anios_por_empresa %>%
  count(n_anios, name = "n_empresas") %>%
  mutate(pct_empresas = 100 * n_empresas / sum(n_empresas)) %>%
  arrange(n_anios)

# -----------------------------
# 6) Discontinuidades en el tiempo
# -----------------------------

discontinuidades_por_empresa <- panel %>%
  distinct(NORDEMP, ANIO) %>%
  arrange(NORDEMP, ANIO) %>%
  group_by(NORDEMP) %>%
  summarise(
    n_anios = n_distinct(ANIO),
    anio_min = min(ANIO),
    anio_max = max(ANIO),
    anios_observados = list(sort(unique(ANIO))),
    .groups = "drop"
  ) %>%
  mutate(
    # Se comparan anios observados contra el intervalo continuo entre el minimo
    # y el maximo de cada firma, no contra todo el panel 2008-2024.
    anios_esperados_intervalo = map2(anio_min, anio_max, ~seq(.x, .y)),
    anios_faltantes_intervalo = map2(anios_esperados_intervalo, anios_observados, ~setdiff(.x, .y)),
    tiene_discontinuidad = lengths(anios_faltantes_intervalo) > 0,
    anios_faltantes_txt = map_chr(anios_faltantes_intervalo, ~if (length(.x) == 0) "" else paste(.x, collapse = ",")),
    anios_observados_txt = map_chr(anios_observados, ~paste(.x, collapse = ","))
  )

empresas_con_discontinuidad <- discontinuidades_por_empresa %>%
  filter(tiene_discontinuidad) %>%
  select(
    NORDEMP,
    n_anios,
    anio_min,
    anio_max,
    anios_observados_txt,
    anios_faltantes_txt
  )

# -----------------------------
# 7) Tabla resumen general
# -----------------------------

total_empresas <- n_distinct(anios_por_empresa$NORDEMP)
empresas_todos_los_anios <- sum(anios_por_empresa$n_anios == total_expected_years)
empresas_discontinuas <- nrow(empresas_con_discontinuidad)

resumen_panel <- tibble(
  total_empresas_unicas = total_empresas,
  promedio_anios_observados = mean(anios_por_empresa$n_anios),
  porcentaje_empresas_todos_los_anios = 100 * empresas_todos_los_anios / total_empresas,
  porcentaje_empresas_con_discontinuidades = 100 * empresas_discontinuas / total_empresas,
  anios_esperados_panel = paste(expected_years, collapse = ","),
  anios_presentes_panel = paste(sort(unique(panel$ANIO)), collapse = ",")
)

# -----------------------------
# 8) Exportacion de tablas
# -----------------------------

readr::write_csv(duplicados_nordemp_anio, file.path(data_output_dir, "duplicados_nordemp_anio.csv"))
readr::write_csv(empresas_con_discontinuidad, file.path(data_output_dir, "empresas_con_discontinuidades.csv"))
readr::write_csv(distribucion_permanencia, file.path(data_output_dir, "resumen_permanencia_empresas.csv"))

# Exportaciones adicionales utiles para auditoria
readr::write_csv(empresas_unicas_por_anio, file.path(data_output_dir, "empresas_unicas_por_anio.csv"))
readr::write_csv(anios_por_empresa, file.path(data_output_dir, "anios_observados_por_empresa.csv"))
readr::write_csv(resumen_panel, file.path(data_output_dir, "resumen_panel_general.csv"))

# -----------------------------
# 9) Grafico de permanencia
# -----------------------------

plot_permanencia <- distribucion_permanencia %>%
  # Este grafico resume cuantas firmas aparecen 1, 2, ..., 17 anios.
  ggplot(aes(x = n_anios, y = n_empresas)) +
  geom_col(fill = "#1f77b4", width = 0.8) +
  scale_x_continuous(breaks = seq(min(distribucion_permanencia$n_anios), max(distribucion_permanencia$n_anios), by = 1)) +
  labs(
    title = "Distribucion de permanencia de empresas (NORDEMP)",
    subtitle = "Macrobase EAM 2008-2024",
    x = "Anios observados por empresa",
    y = "Cantidad de empresas"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(plot_dir, "distribucion_permanencia_empresas.png"),
  plot = plot_permanencia,
  width = 10,
  height = 6,
  dpi = 150
)

# -----------------------------
# 10) Resumen en consola
# -----------------------------

message("--- Diagnostico NORDEMP completado ---")
message("Filas analizadas: ", nrow(panel))
message("Empresas unicas: ", total_empresas)
message("Duplicados NORDEMP-ANIO: ", nrow(duplicados_nordemp_anio))
message("Empresas con discontinuidad: ", empresas_discontinuas)
message("Promedio de anios observados: ", round(mean(anios_por_empresa$n_anios), 3))
message("Tablas en: ", data_output_dir)
message("Grafico en: ", plot_dir)
