source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

script_header("Limpieza De Temporales")

paths <- ensure_project_structure()

tmp_dirs <- c(paths$tmp_unzip, paths$tmp_diccionario, paths$tmp_macrobase)

for (tmp_dir in tmp_dirs) {
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    message("Temporal reiniciado: ", tmp_dir)
  }
}

message("Limpieza completada.")
