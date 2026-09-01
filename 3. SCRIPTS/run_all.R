# run_all.R -- Pipeline simplificado (rama `simplificacion`).
#
# Corre 01 a 05 en orden, de cero a resultados. Requiere que
# "1. DATOS/5. MACROBASE/macro_base_eam.rds" ya exista (construido por
# 3. SCRIPTS/construir_macro_base_eam.R, fuera del alcance de este
# pipeline -- ver nota de alcance en 01_construir_base.R).
#
# NO corre opcional_establecimiento.R (modulo fuera de la ruta principal,
# se corre aparte si se necesita el nivel de establecimiento).

scripts_en_orden <- c(
  "01_construir_base.R",
  "02_construir_exposicion.R",
  "03_construir_panel.R",
  "04_validaciones.R",
  "05_descriptivos.R"
)

for (script in scripts_en_orden) {
  message("")
  message(strrep("#", 70))
  message("# Corriendo: ", script)
  message(strrep("#", 70))
  source(file.path("3. SCRIPTS", script))
}

message("")
message(strrep("#", 70))
message("# Pipeline completo. Resultados en 4. RESULTADOS/Validaciones/simplificado_*.csv")
message(strrep("#", 70))
