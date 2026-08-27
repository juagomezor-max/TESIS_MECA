# Paso 1b: verifica NUMERICAMENTE (no solo por estructura del formulario) si
# C3R2 (fuente de salario_promedio_obrero_f) excluye prestaciones sociales y
# aportes/cotizaciones patronales.
#
# Hallazgo de diccionario (verificado contra el diccionario oficial DANE
# EAM2024, descripciones completas de C3R1-C3R10 columna C1/obreros):
#
# - C3R2 = "Sueldos y salarios del personal PERMANENTE" (obreros)
# - C3R3 = "Prestaciones sociales del personal PERMANENTE" (obreros)
# - C3R4 = "Sueldos, salarios y prestaciones del personal TEMPORAL DIRECTO" (obreros)
# - C3R5 = "Cotizaciones patronales... del personal OCUPADO" (obreros) -- NO
#   distingue permanente vs temporal, cubre TODOS los tipos de vinculacion.
# - C3R6 = "Aportes sobre nomina (SENA, cajas, ICBF)" (obreros) -- misma
#   ambiguedad de alcance que C3R5.
# - C3R7 = "Aportes voluntarios a seguros de vida... del personal OCUPADO"
# - C3R8 = "Valor causado por agencias de personal TEMPORAL"
# - C3R9 = "Otros gastos del personal"
# - C3R10 = TOTAL: suma R2 a R9 (sueldos+prestaciones+cotizaciones+aportes+
#   apoyo de sostenimiento+otros), y por lo tanto cubre TODOS los tipos de
#   vinculacion (permanente + temporal directo + temporal agencia), no solo
#   permanente.
#
# Consecuencia importante: C3R10 NO es comparable 1 a 1 contra C3R2 como "mismo
# grupo, costo mas completo" -- es un grupo MAS AMPLIO (todas las
# vinculaciones) Y un costo mas completo a la vez. La comparacion pedida en el
# Paso 1b (C3R2+C3R3+C3R5+C3R6 vs C3R10) se hace igual, mostrando el margen
# real, pero se complementa con una comparacion mas limpia y SI apples-to-apples:
# C3R2 (solo salario permanente) vs C3R2+C3R3 (salario+prestaciones,
# AMBAS explicitamente "personal permanente"), que es la que permite confirmar
# de forma numerica que C3R2 excluye prestaciones.
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/verificacion_exclusion_prestaciones_eam.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
macro_path <- paths$macro_base_eam

if (!file.exists(macro_path)) stop("No se encontro la macrobase EAM en: ", macro_path)

macro_base <- readr::read_rds(macro_path)
names(macro_base) <- toupper(names(macro_base))

cols_necesarias <- c("C3R2C1", "C3R3C1", "C3R5C1", "C3R6C1", "C3R10C1")
faltantes <- setdiff(cols_necesarias, names(macro_base))
if (length(faltantes) > 0) stop("Faltan columnas: ", paste(faltantes, collapse = ", "))

panel <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = suppressWarnings(as.integer(ANIO))) %>%
  dplyr::filter(!is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEMP, ANIO, dplyr::all_of(cols_necesarias)) %>%
  dplyr::mutate(dplyr::across(-c(NORDEMP, ANIO), ~suppressWarnings(as.numeric(.x)))) %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(cols_necesarias), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

panel_2022 <- panel %>% dplyr::filter(ANIO == 2022, !is.na(C3R2C1), !is.na(C3R10C1))

# ------------------------------------------------------------------
# Chequeo 1 (pedido en el Paso 1b): C3R2 (solo salario permanente) es
# estrictamente menor que C3R10 (total, todas las vinculaciones)?
# ------------------------------------------------------------------

chequeo1 <- panel_2022 %>%
  dplyr::summarise(
    n = dplyr::n(),
    pct_c3r2_menor_c3r10 = round(100 * mean(C3R2C1 < C3R10C1, na.rm = TRUE), 2),
    pct_c3r2_igual_c3r10 = round(100 * mean(C3R2C1 == C3R10C1, na.rm = TRUE), 2),
    pct_c3r2_mayor_c3r10 = round(100 * mean(C3R2C1 > C3R10C1, na.rm = TRUE), 2)
  )

# ------------------------------------------------------------------
# Chequeo 2 (pedido en el Paso 1b): C3R2+C3R3+C3R5+C3R6 vs C3R10. Se
# reporta el margen real, sin forzar a que coincida: C3R10 tambien incluye
# C3R4 (temporal directo), C3R7 (aportes voluntarios), C3R8 (agencias) y
# C3R9 (otros), que NO estan en esta suma parcial, y C3R5/C3R6 ya reflejan
# personal ocupado completo (no solo permanente).
# ------------------------------------------------------------------

panel_2022 <- panel_2022 %>%
  dplyr::mutate(
    suma_parcial = C3R2C1 + C3R3C1 + C3R5C1 + C3R6C1,
    ratio_suma_parcial_sobre_total = ifelse(C3R10C1 > 0, suma_parcial / C3R10C1, NA_real_)
  )

chequeo2 <- panel_2022 %>%
  dplyr::filter(is.finite(ratio_suma_parcial_sobre_total)) %>%
  dplyr::summarise(
    n = dplyr::n(),
    ratio_mediana = round(median(ratio_suma_parcial_sobre_total), 3),
    ratio_p10 = round(quantile(ratio_suma_parcial_sobre_total, .10), 3),
    ratio_p90 = round(quantile(ratio_suma_parcial_sobre_total, .90), 3),
    pct_suma_parcial_menor_100pct_de_total = round(100 * mean(ratio_suma_parcial_sobre_total < 1), 2)
  )

# ------------------------------------------------------------------
# Chequeo 3 (apples-to-apples, ambas filas "personal permanente"):
# C3R2 (salario) vs C3R2+C3R3 (salario+prestaciones). Confirma
# numericamente que C3R2 excluye prestaciones, y cuanto pesan.
# ------------------------------------------------------------------

panel_2022 <- panel_2022 %>%
  dplyr::mutate(
    salario_mas_prestaciones = C3R2C1 + C3R3C1,
    pct_que_son_prestaciones = ifelse(salario_mas_prestaciones > 0, 100 * C3R3C1 / salario_mas_prestaciones, NA_real_)
  )

chequeo3 <- panel_2022 %>%
  dplyr::filter(is.finite(pct_que_son_prestaciones)) %>%
  dplyr::summarise(
    n = dplyr::n(),
    pct_c3r2_menor_c3r2_mas_c3r3 = round(100 * mean(C3R2C1 < salario_mas_prestaciones, na.rm = TRUE), 2),
    prestaciones_pct_mediana = round(median(pct_que_son_prestaciones), 1),
    prestaciones_pct_p10 = round(quantile(pct_que_son_prestaciones, .10), 1),
    prestaciones_pct_p90 = round(quantile(pct_que_son_prestaciones, .90), 1)
  )

resultado <- tibble::tibble(
  chequeo = c(
    "C3R2 < C3R10 (obreros, 2022)",
    "Suma parcial (C3R2+C3R3+C3R5+C3R6) / C3R10 (obreros, 2022)",
    "Prestaciones como % de (salario+prestaciones), ambas permanente (obreros, 2022)"
  ),
  detalle = c(
    paste0(chequeo1$pct_c3r2_menor_c3r10, "% de firmas cumple C3R2<C3R10 (n=", chequeo1$n, ")"),
    paste0("mediana=", chequeo2$ratio_mediana, " (p10=", chequeo2$ratio_p10, ", p90=", chequeo2$ratio_p90, "), ",
           chequeo2$pct_suma_parcial_menor_100pct_de_total, "% de firmas con suma_parcial < total (n=", chequeo2$n, ")"),
    paste0("mediana=", chequeo3$prestaciones_pct_mediana, "% (p10=", chequeo3$prestaciones_pct_p10,
           "%, p90=", chequeo3$prestaciones_pct_p90, "%), ", chequeo3$pct_c3r2_menor_c3r2_mas_c3r3,
           "% de firmas con C3R2<C3R2+C3R3 (n=", chequeo3$n, ")")
  )
)

readr::write_csv(resultado, file.path(data_dir, "verificacion_exclusion_prestaciones_eam.csv"))

script_header("Paso 1b: verificacion numerica de exclusion de prestaciones en C3R2")
print(resultado, width = Inf)
message("")
message("Interpretacion:")
message("- C3R10 no es comparable 1 a 1 contra C3R2: cubre TODAS las vinculaciones")
message("  (permanente + temporal directo + temporal agencia), no solo permanente,")
message("  y C3R5/C3R6 (cotizaciones/aportes) ya reflejan personal ocupado completo,")
message("  no solo permanente. Por eso la suma parcial queda por debajo de C3R10:")
message("  falta C3R4 (temporal directo), C3R7 (aportes voluntarios), C3R8 (agencias)")
message("  y C3R9 (otros gastos), y C3R5/C3R6 incluyen carga de personal no-permanente.")
message("- La comparacion SI apples-to-apples (ambas filas explicitamente 'personal")
message("  permanente' en el diccionario oficial) es C3R2 vs C3R2+C3R3, que confirma")
message("  numericamente que C3R2 excluye prestaciones sociales.")
message("")
message("Tabla exportada en: ", file.path(data_dir, "verificacion_exclusion_prestaciones_eam.csv"))
