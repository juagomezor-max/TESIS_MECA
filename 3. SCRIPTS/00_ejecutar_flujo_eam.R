source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

script_header("Flujo Reproducible EAM")

paths <- ensure_project_structure()

steps <- list(
  list(
    label = "1/4 Analisis inicial de archivos EAM",
    env = c(TARGET_SOURCE = "EAM"),
    script = file.path(paths$scripts, "analisis_eam_eac.R")
  ),
  list(
    label = "2/4 Construccion del diccionario maestro",
    env = character(),
    script = file.path(paths$scripts, "construir_diccionario_maestro.R")
  ),
  list(
    label = "3/4 Construccion de macrobase EAM",
    env = character(),
    script = file.path(paths$scripts, "construir_macro_base_eam.R")
  ),
  list(
    label = "4/4 Diagnosticos y descriptivos sobre macrobase EAM",
    env = character(),
    script = NULL
  )
)

for (step in steps[1:3]) {
  message("\n", step$label)
  old_env <- Sys.getenv(names(step$env), unset = NA_character_)
  if (length(step$env) > 0) {
    do.call(Sys.setenv, as.list(step$env))
  }
  source(step$script, echo = FALSE, local = new.env(parent = globalenv()))
  if (length(step$env) > 0) {
    for (nm in names(step$env)) {
      previous_value <- unname(old_env[nm])
      if (is.na(previous_value)) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, stats::setNames(list(previous_value), nm))
      }
    }
  }
}

message("\n", steps[[4]]$label)
source(file.path(paths$scripts, "diagnostico_panel_nordemp_eam.R"), echo = FALSE, local = new.env(parent = globalenv()))
source(file.path(paths$scripts, "descriptivo_exposicion_eam.R"), echo = FALSE, local = new.env(parent = globalenv()))

message("\nFlujo EAM completado.")
message("Macrobase: ", paths$macro_base_eam)
message("Diccionario maestro: ", paths$diccionario_maestro)
message("Resultados graficos: ", paths$resultados)
