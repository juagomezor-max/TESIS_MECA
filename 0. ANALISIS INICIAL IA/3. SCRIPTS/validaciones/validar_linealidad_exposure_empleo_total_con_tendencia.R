# validar_linealidad_exposure_empleo_total_con_tendencia.R
#
# ULTIMA verificacion de esta ronda de robustez (2026-09-14). Repite,
# SOLO para Exposure2022_obreros x empleo_total x quintiles, el mismo
# test F conjunto y el mismo test de linealidad (contrastes polinomiales
# ortogonales via contr.poly(), ver validar_robustez_conjunta_grupos_
# exposicion.R) que ya se corrieron SIN tendencia (F conjunto p=0.00194,
# no-linealidad p=0.0027), pero ahora CON el control de tendencia lineal
# pre-existente por grupo -- i(quintil_exposure2022_obreros, anio_lineal,
# ref='Q1 - Muy baja') -- ya usado en verificacion_grupos_exposure_
# empleo_con_tendencia.csv, agregado aqui como control adicional junto a
# los contrastes polinomiales del efecto "post".
#
# Objetivo: saber si la no-linealidad conjunta detectada SIN tendencia
# (p=0.0027) tambien desaparece al controlar por tendencia, para cerrar
# esta celda con el mismo nivel de evidencia que las demas.
#
# DERIVADO de (no reimplementado de memoria):
# - Contrastes polinomiales ortogonales (contr.poly, L/Q/C/X4) y el
#   patron de wald() para F conjunto y test de linealidad: IDENTICO a
#   validaciones/validar_robustez_conjunta_grupos_exposicion.R.
# - Control de tendencia por grupo (i(grupo, anio_lineal, ref)): IDENTICO
#   a validaciones/validar_grupos_bite_participacion_con_tendencia.R y
#   validar_robustez_conjunta_grupos_exposicion.R (Parte 1).
#
# NO se corre ninguna prueba adicional mas alla de esta -- instruccion
# explicita del usuario para cerrar esta ronda de robustez.
#
# Salidas (versionadas, 4. RESULTADOS/Validaciones/):
# - prueba_linealidad_exposure_empleo_total_con_tendencia.csv

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "fixest")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_formal <- readr::read_rds(file.path(data_dir, "panel_establecimiento_formal.rds"))
exposicion_firma <- readr::read_rds(file.path(data_dir, "exposicion_firma_eam.rds"))

datos_exposure <- panel_formal %>%
  dplyr::inner_join(
    exposicion_firma %>% dplyr::select(NORDEMP, quintil_exposure2022_obreros) %>% dplyr::filter(!is.na(quintil_exposure2022_obreros)),
    by = "NORDEMP"
  ) %>%
  dplyr::filter(!is.na(CIIU4), !is.na(DPTO_fijo), !is.na(tamano_empresa)) %>%
  dplyr::mutate(post_2023 = as.integer(ANIO >= 2023))

FE_CON <- "NORDEST + ANIO_F + CIIU4^ANIO_F + tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"
REF_LABEL <- "Q1 - Muy baja"
VAR_GRUPO <- "quintil_exposure2022_obreros"
VAR_Y <- "empleo_total"

# ------------------------------------------------------------------
# Contrastes polinomiales ortogonales para el efecto "post" (L,Q,C,X4).
# ------------------------------------------------------------------

niveles <- levels(datos_exposure[[VAR_GRUPO]])
cp <- stats::contr.poly(length(niveles))
nombres_grado <- c("L", "Q", "C", "X4")[1:ncol(cp)]
colnames(cp) <- nombres_grado
contrastes_df <- as.data.frame(cp)
contrastes_df$.grupo_chr <- niveles

datos_modelo <- datos_exposure %>%
  dplyr::mutate(.grupo_chr = as.character(.data[[VAR_GRUPO]])) %>%
  dplyr::left_join(contrastes_df, by = ".grupo_chr")

# ------------------------------------------------------------------
# Modelo: contrastes polinomiales de "post" + control de tendencia por
# grupo (dummy, no polinomial -- igual que en la verificacion anterior).
# ------------------------------------------------------------------

terminos_post <- paste0("post_2023:", nombres_grado)
formula_modelo <- stats::as.formula(paste0(
  VAR_Y, " ~ ", paste(terminos_post, collapse = " + "),
  " + i(", VAR_GRUPO, ", anio_lineal, ref = '", REF_LABEL, "') | ", FE_CON
))
modelo <- fixest::feols(formula_modelo, data = datos_modelo, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)

prueba_conjunta <- fixest::wald(modelo, keep = "post_2023:", print = FALSE)
prueba_no_lineal <- fixest::wald(modelo, keep = "post_2023:(Q|C|X4)$", print = FALSE)

resultado <- tibble::tibble(
  medida = "Exposure2022_obreros", particion = "Quintiles", variable = VAR_Y,
  control = "CON tendencia lineal pre-existente por grupo (i(grupo, anio_lineal, ref))",
  n_obs = stats::nobs(modelo),
  f_conjunto = round(prueba_conjunta$stat, 3), df1_conjunto = prueba_conjunta$df1, df2_conjunto = round(prueba_conjunta$df2, 1),
  p_conjunto = signif(prueba_conjunta$p, 4), rechaza_conjunto_al_5pct = prueba_conjunta$p < 0.05,
  f_no_lineal = round(prueba_no_lineal$stat, 3), df1_no_lineal = prueba_no_lineal$df1, df2_no_lineal = round(prueba_no_lineal$df2, 1),
  p_no_lineal = signif(prueba_no_lineal$p, 4), lineal_es_suficiente = prueba_no_lineal$p >= 0.05,
  # comparacion explicita contra el resultado SIN tendencia ya reportado
  # (prueba_conjunta_y_linealidad_por_grupo.csv, misma celda):
  p_conjunto_sin_tendencia = 0.00194, p_no_lineal_sin_tendencia = 0.0027
)

readr::write_csv(resultado, file.path(out_dir, "prueba_linealidad_exposure_empleo_total_con_tendencia.csv"))

script_header("validar_linealidad_exposure_empleo_total_con_tendencia.R -- Exposure x empleo_total x quintiles, CON tendencia")
message("")
print(resultado, n = Inf, width = Inf)
message("")
message("Tabla exportada en: ", file.path(out_dir, "prueba_linealidad_exposure_empleo_total_con_tendencia.csv"))
