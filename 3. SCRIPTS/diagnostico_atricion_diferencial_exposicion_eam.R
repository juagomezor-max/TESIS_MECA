# Diagnostico de atricion diferencial alrededor del choque de 2023: para
# las firmas presentes en el panel en 2022 (anio base de Exposure2022_obreros),
# verifica si siguen apareciendo en 2023 y en 2024, desagregado por
# quintil de exposicion.
#
# Objetivo puntual: detectar si la atricion del panel es DIFERENCIAL por
# nivel de exposicion (lo que amenazaria una comparacion pre/post 2023),
# no explicar la caida global de empresas en el panel a lo largo de todo
# 2008-2024 (eso queda fuera de alcance, ver nota en
# 0. PREPARACION/notas_exposicion_obreros_eam.md).
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/atricion_por_quintil_exposicion_eam.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

ANIO_BASE_EXPOSICION <- 2022

conteo_path <- file.path(data_dir, "conteo_personal_categoria_eam.rds")
exposicion_path <- file.path(data_dir, "exposicion_obreros_eam.rds")

if (!file.exists(conteo_path)) stop("Falta conteo_personal_categoria_eam.rds. Corre el Paso 3 primero.")
if (!file.exists(exposicion_path)) stop("Falta exposicion_obreros_eam.rds. Corre el Paso 5 primero.")

conteo <- readr::read_rds(conteo_path)
exposicion <- readr::read_rds(exposicion_path)

# ------------------------------------------------------------------
# Firmas presentes en el panel en el anio base, con su quintil de
# exposicion (constante para la firma, asignado en el Paso 5).
# ------------------------------------------------------------------

firmas_base <- exposicion %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, quintil_exposure2022_obreros)

n_firmas_base <- nrow(firmas_base)
message("Firmas presentes en ", ANIO_BASE_EXPOSICION, ": ", n_firmas_base)

# ------------------------------------------------------------------
# Presencia en 2023 y 2024, verificada directamente contra el panel
# (conteo_personal_categoria_eam), no asumida.
# ------------------------------------------------------------------

nordemp_2023 <- conteo %>% dplyr::filter(ANIO == 2023) %>% dplyr::pull(NORDEMP) %>% unique()
nordemp_2024 <- conteo %>% dplyr::filter(ANIO == 2024) %>% dplyr::pull(NORDEMP) %>% unique()

firmas_seguimiento <- firmas_base %>%
  dplyr::mutate(
    presente_2023 = NORDEMP %in% nordemp_2023,
    presente_2024 = NORDEMP %in% nordemp_2024
  )

# ------------------------------------------------------------------
# Tasa de salida por quintil
# ------------------------------------------------------------------

tabla_atricion <- firmas_seguimiento %>%
  dplyr::mutate(
    quintil = dplyr::if_else(is.na(quintil_exposure2022_obreros), "Sin quintil (NA)", as.character(quintil_exposure2022_obreros))
  ) %>%
  dplyr::group_by(quintil) %>%
  dplyr::summarise(
    n_firmas_2022 = dplyr::n(),
    pct_sale_2023 = round(100 * mean(!presente_2023), 2),
    pct_sale_2024 = round(100 * mean(!presente_2024), 2),
    .groups = "drop"
  )

# Reordena con el mismo orden de quintiles usado en el resto del proyecto.
orden_quintiles <- c("Q1 - Muy baja", "Q2 - Baja", "Q3 - Media", "Q4 - Alta", "Q5 - Muy alta", "Sin quintil (NA)")
tabla_atricion <- tabla_atricion %>%
  dplyr::mutate(quintil = factor(quintil, levels = orden_quintiles)) %>%
  dplyr::arrange(quintil)

readr::write_csv(tabla_atricion, file.path(data_dir, "atricion_por_quintil_exposicion_eam.csv"))

# ------------------------------------------------------------------
# Senal de atricion diferencial: diferencia Q5 - Q1 (en puntos
# porcentuales). Un umbral de "pocos puntos" se deja como referencia
# (5pp) pero el numero se reporta siempre, sin decidir por el usuario.
# ------------------------------------------------------------------

q1_2023 <- tabla_atricion$pct_sale_2023[tabla_atricion$quintil == "Q1 - Muy baja"]
q5_2023 <- tabla_atricion$pct_sale_2023[tabla_atricion$quintil == "Q5 - Muy alta"]
q1_2024 <- tabla_atricion$pct_sale_2024[tabla_atricion$quintil == "Q1 - Muy baja"]
q5_2024 <- tabla_atricion$pct_sale_2024[tabla_atricion$quintil == "Q5 - Muy alta"]

diff_2023 <- round(q5_2023 - q1_2023, 2)
diff_2024 <- round(q5_2024 - q1_2024, 2)

script_header("Atricion diferencial por quintil de Exposure2022_obreros")
print(tabla_atricion, n = Inf, width = Inf)
message("")
message("Diferencia Q5 (mas expuesta) - Q1 (menos expuesta):")
message("  2023: ", diff_2023, " puntos porcentuales")
message("  2024: ", diff_2024, " puntos porcentuales")
message("")
message("Tabla exportada en: ", file.path(data_dir, "atricion_por_quintil_exposicion_eam.csv"))
