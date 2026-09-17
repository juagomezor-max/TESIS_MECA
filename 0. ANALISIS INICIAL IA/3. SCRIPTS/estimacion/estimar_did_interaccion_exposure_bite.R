if (!file.exists("3. SCRIPTS/pipeline/_utils_proyecto.R") &&
    file.exists("../../3. SCRIPTS/pipeline/_utils_proyecto.R")) {
  setwd("../..")
}

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))
load_project_packages(c("dplyr", "readr", "fixest"))

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

# 1. Cargar el panel y ambas exposiciones.
panel <- readr::read_rds(
  file.path(data_dir, "panel_establecimiento_formal.rds")
)

exposiciones <- readr::read_rds(
  file.path(data_dir, "exposicion_firma_eam.rds")
) |>
  dplyr::select(NORDEMP, Exposure2022_obreros, Bite2022_obreros)

stopifnot(!anyDuplicated(exposiciones$NORDEMP))

# 2. Usar observaciones con ambas exposiciones y controles disponibles.
panel <- panel |>
  dplyr::inner_join(exposiciones, by = "NORDEMP") |>
  dplyr::filter(
    is.finite(Exposure2022_obreros),
    is.finite(Bite2022_obreros),
    !is.na(CIIU4), !is.na(tamano_empresa), !is.na(DPTO_fijo)
  )

sd_bite <- panel |>
  dplyr::distinct(NORDEMP, Bite2022_obreros) |>
  dplyr::pull(Bite2022_obreros) |>
  stats::sd()

stopifnot(is.finite(sd_bite), sd_bite > 0)

panel <- panel |>
  dplyr::mutate(
    post_2023 = as.integer(ANIO >= 2023),
    ANIO_F = factor(ANIO),
    exposicion_10pp = Exposure2022_obreros / 0.1,
    bite_1sd = Bite2022_obreros / sd_bite
  )

# 3. Tres términos: Exposure × Post, Bite × Post e interacción × Post.
terminos <- paste(
  "post_2023:exposicion_10pp",
  "post_2023:bite_1sd",
  "post_2023:exposicion_10pp:bite_1sd",
  sep = " + "
)

efectos_fijos <- c(
  sin_controles = "NORDEST + ANIO_F",
  con_controles = paste(
    "NORDEST + ANIO_F + CIIU4^ANIO_F",
    "+ tamano_empresa^ANIO_F + DPTO_fijo^ANIO_F"
  )
)

outcomes <- c(
  "empleo_total", "empleo_permanente",
  "empleo_temporal", "participacion_permanente"
)

modelos <- list()

for (y in outcomes) {
  for (spec in names(efectos_fijos)) {
    formula <- stats::as.formula(
      paste(y, "~", terminos, "|", efectos_fijos[[spec]])
    )
    
    modelos[[paste(y, spec, sep = "_")]] <- fixest::feols(
      formula, data = panel, cluster = ~NORDEMP
    )
  }
}

# 4. Mostrar dos modelos por resultado.
for (y in outcomes) {
  cat("\n\nRESULTADO:", y, "\n")
  print(fixest::etable(
    modelos[paste(y, names(efectos_fijos), sep = "_")],
    fitstat = ~ n + r2
  ))
}

getwd()

list.files(
  path = "..",
  pattern = "^(panel_establecimiento_formal|exposicion_firma_eam)\\.rds$",
  recursive = TRUE,
  full.names = TRUE
)