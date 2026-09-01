# PASO 2 de feature/estimacion-preliminar: validar tendencias paralelas
# sobre el panel FORMAL ya ensamblado (Paso 1), no sobre las versiones
# preliminares previas (que solo cubrian 2015-2019). Este panel incluye
# TODO el pre-periodo aprobado: 2015-2019 + 2021-2022 (7 anios, 2020
# excluido). Es una validacion de un supuesto de identificacion, NO un
# resultado de empleo -- no se estima ningun efecto del choque de 2023
# aqui (el post-periodo 2023-2024 se excluye de este chequeo).
#
# Especificacion (misma metodologia ya validada en
# validar_tendencias_paralelas_establecimiento.R, ahora sobre el panel
# formal completo y con DPTO_fijo -- antes DPTO variaba por anio, ahora
# es fijo por establecimiento segun la regla aprobada del Paso 2.3):
#
#   Y ~ i(ANIO_F, exposicion_10pp_est, ref="2015") | NORDEST + CIIU4^ANIO_F + DPTO_fijo^ANIO_F
#   cluster = ~NORDEMP
#
# Prueba F conjunta de los coeficientes de TODOS los anios pre no-base
# (2016,2017,2018,2019,2021,2022) -- 6 coeficientes, no 4 como en la
# validacion preliminar (que solo llegaba a 2019).
#
# Dimensiones (Y): empleo_total, empleo_permanente, empleo_temporal,
# participacion_permanente (identicas a las validaciones previas).
#
# Salidas (versionadas, en 4. RESULTADOS/Validaciones/):
# - evento_pretendencias_panel_formal_empleo_total.png (y las otras 3 dimensiones)
# - tabla_pretendencias_panel_formal.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "fixest", "purrr")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion
out_dir <- paths$resultados_validaciones

panel_path <- file.path(data_dir, "panel_formal_establecimiento_eam.rds")
if (!file.exists(panel_path)) stop("Falta panel_formal_establecimiento_eam.rds. Corre construir_panel_formal_establecimiento_eam.R (Paso 1) primero.")

panel_completo <- readr::read_rds(panel_path)

ANIOS_PRE <- c(2015:2019, 2021:2022)

panel_pre <- panel_completo %>%
  dplyr::filter(ANIO %in% ANIOS_PRE, !is.na(exposicion_10pp_est), !is.na(CIIU4), !is.na(DPTO_fijo)) %>%
  dplyr::mutate(ANIO_F = factor(ANIO, levels = as.character(ANIOS_PRE)))

message("Panel pre-periodo (2015-2019+2021-2022) para validacion: ", nrow(panel_pre),
        " filas, ", dplyr::n_distinct(panel_pre$NORDEST), " establecimientos.")

metrics_info <- tibble::tribble(
  ~var, ~label, ~filename_stub,
  "empleo_total", "Empleo total", "empleo_total",
  "empleo_permanente", "Empleo permanente", "empleo_permanente",
  "empleo_temporal", "Empleo temporal (directo + agencia)", "empleo_temporal",
  "participacion_permanente", "Participacion de permanentes (%)", "participacion_permanente"
)

correr_evento <- function(data, var_y) {
  formula_modelo <- stats::as.formula(paste0(
    var_y, " ~ i(ANIO_F, exposicion_10pp_est, ref = '2015') | NORDEST + CIIU4^ANIO_F + DPTO_fijo^ANIO_F"
  ))
  fixest::feols(formula_modelo, data = data, cluster = ~NORDEMP, warn = FALSE, notes = FALSE)
}

resultados <- purrr::map_dfr(seq_len(nrow(metrics_info)), function(i) {
  metric <- metrics_info[i, ]
  modelo <- correr_evento(panel_pre, metric$var)

  png(
    file.path(out_dir, paste0("evento_pretendencias_panel_formal_", metric$filename_stub, ".png")),
    width = 1100, height = 650, res = 130
  )
  par(cex.main = 0.95)
  fixest::iplot(
    modelo,
    ref.line = 0,
    main = metric$label,
    xlab = "Anio",
    ylab = "Efecto de +10pp de Exposure2022_obreros_est (ref. 2015)",
    ci_level = 0.95
  )
  dev.off()

  prueba_f <- fixest::wald(modelo, keep = "ANIO_F::(2016|2017|2018|2019|2021|2022)", print = FALSE)
  tibble::tibble(
    variable = metric$var,
    n_obs = stats::nobs(modelo),
    n_establecimientos = dplyr::n_distinct(panel_pre$NORDEST[!is.na(panel_pre[[metric$var]])]),
    f_stat = round(prueba_f$stat, 3),
    df1 = prueba_f$df1,
    df2 = round(prueba_f$df2, 1),
    p_valor = signif(prueba_f$p, 4)
  )
})

readr::write_csv(resultados, file.path(out_dir, "tabla_pretendencias_panel_formal.csv"))

script_header("PASO 2: validar pre-tendencias sobre el panel FORMAL (2015-2019+2021-2022)")
message("")
message("Prueba F conjunta de los 6 coeficientes pre-periodo (2016,2017,2018,2019,2021,2022 vs. 2015):")
print(resultados, n = Inf, width = Inf)
message("")
message("NINGUN resultado de empleo (efecto del choque 2023) se estima aqui -- solo el pre-periodo.")
message("Graficos y tabla exportados en: ", out_dir)
