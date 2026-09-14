# generar_diccionario_panel_csv.R
#
# Genera "1. DATOS/DICCIONARIO_PANEL.csv" -- version tabular (CSV) del
# codebook de los 2 paneles ya documentado en prosa en
# "1. DATOS/CODEBOOK.md". La cobertura se recalcula fresca desde los
# archivos .rds ya exportados (no se transcribe de memoria de
# CODEBOOK.md) -- incluye un chequeo cruzado que advierte si alguna
# variable del panel real queda sin metadata, o viceversa.
#
# Complementa (no reemplaza) el diccionario oficial completo de la EAM/EAC
# ("1. DATOS/diccionario_maestro_variables_EAM_EAC.csv", copia de
# "0. ANALISIS INICIAL IA/1. DATOS/3. DICCIONARIOS/diccionario_maestro_
# variables.csv") -- ese cubre TODOS los codigos crudos de la encuesta;
# este cubre solo las columnas que efectivamente estan en los 2 paneles.
#
# Requiere que "1. DATOS/panel_analitico_firma_eam.rds" y
# "1. DATOS/panel_establecimiento_formal.rds" ya existan -- generarlos
# primero con exportar_paneles_manuales_1datos.R si hace falta.
#
# Ejecutar con cwd = raiz del repositorio (no dentro de esta carpeta
# archivada -- este script solo lee/escribe en "1. DATOS/", no depende
# de _utils_proyecto.R ni de renv especifico de esta carpeta).

suppressMessages(library(dplyr))
suppressMessages(library(readr))
suppressMessages(library(tibble))

panel_firma <- readr::read_rds(file.path("1. DATOS", "panel_analitico_firma_eam.rds"))
panel_est <- readr::read_rds(file.path("1. DATOS", "panel_establecimiento_formal.rds"))

cobertura <- function(df, nombre_panel) {
  purrr_map <- lapply(names(df), function(v) {
    x <- df[[v]]
    no_na <- sum(!is.na(x))
    tibble::tibble(panel = nombre_panel, variable = v, cobertura_pct = round(100 * no_na / nrow(df), 2))
  })
  dplyr::bind_rows(purrr_map)
}

cob_firma <- cobertura(panel_firma, "panel_analitico_firma_eam")
cob_est <- cobertura(panel_est, "panel_establecimiento_formal")
cob_todas <- dplyr::bind_rows(cob_firma, cob_est)

metadata_firma <- tibble::tribble(
  ~variable, ~definicion, ~unidad, ~fuente,
  "NORDEMP", "Identificador de la firma (panel EAM)", "ID (texto)", "EAM directa",
  "ANIO", "Ano de la observacion", "ano calendario", "EAM directa",
  "CIIU4", "Sector economico, clasificacion CIIU rev. 4 digitos", "categorica (144 niveles)", "EAM directa",
  "DPTO", "Departamento de ubicacion (primer establecimiento reportado de la firma-anio)", "categorica (23 niveles)", "Construida (agregada NORDEMP-ANIO, dplyr::first(DPTO)) -- pipeline/01_construir_base.R",
  "tamano_empresa", "Tamano por empleo total: Pequena<50, Mediana<200, Grande>=200", "categorica (3 niveles)", "Construida -- pipeline/03_construir_panel.R",
  "empleo_total", "Suma de obreros + administrativos + profesionales/tecnicos", "personas", "Construida (suma columnas EAM C4R*) -- 03_construir_panel.R",
  "empleo_permanente", "Personal permanente contratado a termino indefinido", "personas", "Construida -- 03_construir_panel.R",
  "empleo_temporal", "Personal temporal (directo + por agencia)", "personas", "Construida -- 03_construir_panel.R",
  "participacion_permanente", "empleo_permanente / empleo_total x 100", "%", "Construida -- 03_construir_panel.R",
  "Exposure2022_obreros", "Participacion de obreros/operarios en el empleo por categorias ocupacionales, ano base 2022, winsorizada 1%/99%", "proporcion [0,1]", "Construida -- pipeline/02_construir_exposicion.R",
  "quintil_exposure2022_obreros", "Quintil de Exposure2022_obreros", "categorica ordinal (5 niveles)", "Construida -- 02_construir_exposicion.R",
  "Bite2022_obreros", "Medida alternativa de exposicion via costo laboral de obreros, ano base 2022", "continua", "Construida -- 02_construir_exposicion.R",
  "quintil_bite2022_obreros", "Quintil de Bite2022_obreros", "categorica ordinal (5 niveles)", "Construida -- 02_construir_exposicion.R",
  "post_2023", "Indicador de periodo posterior al choque (ANIO>=2023)", "binaria (0/1)", "Construida -- 03_construir_panel.R",
  "anio_lineal", "Ano recodificado como tendencia lineal (ANIO-2015)", "entero", "Construida -- 03_construir_panel.R",
  "ANIO_F", "ANIO como factor (efectos fijos por ano)", "categorica (9 niveles)", "Construida -- 03_construir_panel.R",
  "exposicion_10pp", "Exposure2022_obreros / 0.1", "continua", "Construida -- 03_construir_panel.R",
  "Multi_f", "1 si la firma tenia >1 establecimiento en 2022, 0 si no", "binaria (0/1)", "Construida -- pipeline/opcional_establecimiento.R, re-derivada en estimacion/estimar_especificacion_a_establecimiento.R",
  "C3R23C3", "Total mantenimiento, reparaciones, accesorios y repuestos consumidos", "pesos COP corrientes", "EAM directa (agregada NORDEMP-ANIO) -- estimacion/estimar_mecanismos_ajuste_firma.R",
  "C3R41C3", "Total costos y gastos por servicios contratados con terceros (outsourcing)", "pesos COP corrientes", "EAM directa (agregada NORDEMP-ANIO) -- estimar_mecanismos_ajuste_firma.R; 72.2% de la muestra en 0",
  "C7R10C2", "Total inversiones en activos fijos", "pesos COP corrientes", "EAM directa (agregada NORDEMP-ANIO) -- estimar_mecanismos_ajuste_firma.R",
  "VALORVEN", "Valor de las ventas", "pesos COP corrientes", "EAM directa (agregada NORDEMP-ANIO) -- estimar_mecanismos_ajuste_firma.R"
) %>% dplyr::mutate(panel = "panel_analitico_firma_eam", .before = 1)

metadata_est <- tibble::tribble(
  ~variable, ~definicion, ~unidad, ~fuente,
  "NORDEST", "Identificador del establecimiento (planta)", "ID (texto)", "EAM directa",
  "NORDEMP", "Identificador de la firma duena del establecimiento", "ID (texto)", "EAM directa",
  "ANIO", "Ano de la observacion", "ano calendario", "EAM directa",
  "ANIO_F", "ANIO como factor", "categorica (9 niveles)", "Construida -- construccion/construir_panel_establecimiento_formal.R",
  "anio_lineal", "Ano recodificado como tendencia lineal (ANIO-2015)", "entero", "Construida -- ídem",
  "CIIU4", "Sector economico CIIU 4 digitos", "categorica (144 niveles)", "EAM directa",
  "DPTO_fijo", "Departamento estabilizado por establecimiento (recodificado, resuelve inestabilidad ano a ano)", "categorica (23 niveles)", "Construida -- construir_panel_establecimiento_formal.R lineas 185-201",
  "tamano_empresa", "Tamano por empleo total del establecimiento", "categorica (3 niveles)", "Construida -- ídem",
  "empleo_total", "Empleo total del establecimiento", "personas", "Construida (suma C4R*) -- ídem",
  "empleo_permanente", "Personal permanente del establecimiento", "personas", "Construida -- ídem",
  "empleo_temporal", "Personal temporal del establecimiento", "personas", "Construida -- ídem",
  "participacion_permanente", "empleo_permanente / empleo_total x 100", "%", "Construida -- ídem",
  "costo_laboral_total", "Costo laboral total (cadena de respaldo, C3R10C3 primaria)", "pesos COP corrientes", "EAM directa (C3R10C3) -- construir_panel_establecimiento_formal.R lineas 118-121, 233-239",
  "salario_promedio", "costo_laboral_total / empleo_total", "pesos COP corrientes/trabajador", "Construida -- ídem linea 240",
  "Exposure2022_obreros", "Igual que en el panel de firma, heredada por NORDEMP", "proporcion [0,1]", "Construida (join por NORDEMP) -- exportar_paneles_manuales_1datos.R",
  "Bite2022_obreros", "Igual que en el panel de firma, heredada por NORDEMP", "continua", "Construida (join por NORDEMP) -- ídem",
  "Exposure2022_obreros_est", "Exposure recalculada A NIVEL DEL ESTABLECIMIENTO (no heredada de la firma)", "proporcion [0,1]", "Construida -- construccion/construir_exposicion_obreros_establecimiento_eam.R, re-derivada en estimar_especificacion_a_establecimiento.R",
  "Multi_f", "1 si la firma duena tenia >1 establecimiento en 2022, 0 si no (atributo de la firma)", "binaria (0/1)", "Construida -- misma logica que panel de firma",
  "C3R23C3", "Mantenimiento, reparaciones, accesorios y repuestos consumidos, del propio establecimiento", "pesos COP corrientes", "EAM directa (NORDEST-ANIO, sin agregar)",
  "C3R41C3", "Costos y gastos por servicios contratados con terceros (outsourcing), del propio establecimiento", "pesos COP corrientes", "EAM directa; mucha masa en 0",
  "C7R10C2", "Inversiones en activos fijos, del propio establecimiento", "pesos COP corrientes", "EAM directa",
  "VALORVEN", "Valor de las ventas, del propio establecimiento", "pesos COP corrientes", "EAM directa"
) %>% dplyr::mutate(panel = "panel_establecimiento_formal", .before = 1)

metadata_todas <- dplyr::bind_rows(metadata_firma, metadata_est)

diccionario_panel <- metadata_todas %>%
  dplyr::left_join(cob_todas, by = c("panel", "variable")) %>%
  dplyr::select(panel, variable, definicion, unidad, fuente, cobertura_pct)

# Chequeo cruzado: toda variable del panel real debe tener metadata, y viceversa.
faltan_metadata <- dplyr::anti_join(cob_todas, metadata_todas, by = c("panel", "variable"))
faltan_en_panel <- dplyr::anti_join(metadata_todas, cob_todas, by = c("panel", "variable"))
if (nrow(faltan_metadata) > 0) { cat("ADVERTENCIA -- variables en el panel SIN metadata:\n"); print(faltan_metadata) }
if (nrow(faltan_en_panel) > 0) { cat("ADVERTENCIA -- metadata de variables que NO estan en el panel:\n"); print(faltan_en_panel) }

readr::write_csv(diccionario_panel, file.path("1. DATOS", "DICCIONARIO_PANEL.csv"))
cat("Filas escritas:", nrow(diccionario_panel), "(", nrow(cob_firma), "+", nrow(cob_est), "esperadas)\n")
cat("Guardado en: 1. DATOS/DICCIONARIO_PANEL.csv\n")
