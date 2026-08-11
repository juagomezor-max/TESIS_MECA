# Construye Bite2022_obreros: indice de Kaitz (SM_2023 / salario promedio de
# obreros en 2022), medida de exposicion pre-choque basada en cuanto tuvo que
# subir el salario de una firma para llegar al minimo de 2023, en vez de
# cuanto pesan los obreros en el empleo total (Exposure2022_obreros).
#
# Formula: Bite2022_obreros = (SM_2023 x 12) / salario_promedio_obrero_f_2022
#
# Ambos terminos en base ANUAL, miles de pesos: salario_promedio_obrero_f
# (fuente: C3R2C1, "Sueldos y salarios del personal permanente - obreros")
# es una cifra anual en miles de pesos (confirmado empiricamente en el Paso 1:
# comparar contra SM_2023 mensual daba una razon de 13.93x, implausible para
# el grupo peor pagado; contra SM_2023 anualizado daba 1.16x, plausible).
# SM_2023 = $1.160.000 COP mensual (Decreto 2613 de 2022, Ministerio del
# Trabajo), verificado via busqueda web, no asumido de memoria.
#
# Salida (no versionada):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/bite_obreros_eam.rds/.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

# Parametros explicitos: cambiar aqui, no en el cuerpo del script.
SM_2023_MENSUAL_COP <- 1160000  # Decreto 2613 de 2022, Ministerio del Trabajo
ANIO_BASE_EXPOSICION <- 2022

SM_2023_ANUAL_MILES <- SM_2023_MENSUAL_COP * 12 / 1000  # miles de pesos, misma unidad que salario_promedio_obrero_f

salarios_path <- file.path(data_dir, "salarios_promedio_categoria_eam.rds")
exposicion_path <- file.path(data_dir, "exposicion_obreros_eam.rds")

if (!file.exists(salarios_path)) {
  stop("Falta salarios_promedio_categoria_eam.rds. Corre construir_salarios_promedio_categoria_eam.R primero.")
}
if (!file.exists(exposicion_path)) {
  stop("Falta exposicion_obreros_eam.rds. Corre construir_exposicion_obreros_eam.R primero.")
}

safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

salarios <- readr::read_rds(salarios_path)
exposicion <- readr::read_rds(exposicion_path)

salario_base_2022 <- salarios %>%
  dplyr::filter(ANIO == ANIO_BASE_EXPOSICION) %>%
  dplyr::distinct(NORDEMP, salario_promedio_obrero)

bite_baseline <- salario_base_2022 %>%
  dplyr::mutate(Bite2022_obreros = safe_divide(SM_2023_ANUAL_MILES, salario_promedio_obrero)) %>%
  dplyr::select(NORDEMP, Bite2022_obreros)

# Se une con la base de Exposure2022_obreros (todos los NORDEMP-ANIO del
# panel), no solo el anio base, para que ambas medidas de exposicion queden
# juntas en un mismo archivo.
bite_obreros <- exposicion %>%
  dplyr::left_join(bite_baseline, by = "NORDEMP")

readr::write_rds(bite_obreros, file.path(data_dir, "bite_obreros_eam.rds"))
readr::write_csv(bite_obreros, file.path(data_dir, "bite_obreros_eam.csv"))

script_header("Bite2022_obreros construida")
message("SM_2023 mensual: $", format(SM_2023_MENSUAL_COP, big.mark = ",", scientific = FALSE), " COP")
message("SM_2023 anualizado: ", SM_2023_ANUAL_MILES, " (miles de pesos)")
message("Firmas con Bite2022_obreros no NA en ", ANIO_BASE_EXPOSICION, ": ", sum(!is.na(bite_baseline$Bite2022_obreros)))
message("")
message("Base exportada en: ", file.path(data_dir, "bite_obreros_eam.rds"))
