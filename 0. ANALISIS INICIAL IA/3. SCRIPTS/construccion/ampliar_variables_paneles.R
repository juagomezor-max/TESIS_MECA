# ampliar_variables_paneles.R
#
# Agregamos variables nuevas de la macrobase a los 2 paneles que usa
# "3. SCRIPTS/01_resultados_principales.R" (panel_analitico_firma_eam y
# panel_establecimiento_formal), y sobrescribimos esas 2 bases en
# "1. DATOS/" de la raiz. Las filas y las columnas que ya existian NO se
# tocan -- solo agregamos columnas nuevas con left_join.
#
# Reglas de agregacion que reusamos de scripts ya validados (no las
# reinventamos):
# - Establecimiento (NORDEST-ANIO): ya es unico en la macrobase, sin
#   necesidad de sumar plantas -- misma verificacion que
#   construccion/construir_panel_establecimiento_formal.R (si encontramos
#   duplicados, nos detenemos, igual que ese script).
# - Firma (NORDEMP-ANIO): sumamos las plantas de cada firma-anio, "si
#   todas las plantas tienen NA, el resultado es NA; si al menos una
#   tiene dato, sumamos con na.rm=TRUE" -- misma regla de
#   pipeline/01_construir_base.R. PORCVT y PORCON son porcentajes: los
#   promediamos ponderando por VALORVEN, no los sumamos.
#
# No modificamos "3. SCRIPTS/01_resultados_principales.R" ni
# "0. ANALISIS NICOLAS/". Este script solo agrega archivos nuevos dentro
# de "0. ANALISIS INICIAL IA/".

source(file.path("3. SCRIPTS", "pipeline", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble", "tidyr", "haven")
load_project_packages(required_packages)

paths <- ensure_project_structure()

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# Ruta de la raiz del repositorio (un nivel arriba de esta carpeta
# archivada) -- ahi vive "1. DATOS/" con los 2 paneles a actualizar.
raiz <- ".."
datos_raiz <- file.path(raiz, "1. DATOS")

# ------------------------------------------------------------------
# 1) Cargamos la macrobase y los 2 paneles actuales.
# ------------------------------------------------------------------

message("Cargando macrobase...")
macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))

message("Cargando los 2 paneles actuales desde 1. DATOS/...")
panel_firma_actual <- readr::read_rds(file.path(datos_raiz, "panel_analitico_firma_eam.rds"))
panel_establecimiento_actual <- readr::read_rds(file.path(datos_raiz, "panel_establecimiento_formal.rds"))

message("panel_analitico_firma_eam: ", nrow(panel_firma_actual), " filas, ", ncol(panel_firma_actual), " columnas")
message("panel_establecimiento_formal: ", nrow(panel_establecimiento_actual), " filas, ", ncol(panel_establecimiento_actual), " columnas")

# ------------------------------------------------------------------
# 2) Mapa de variables "simples": un codigo EAM -> una variable nueva.
#    Para estas, el tratamiento es siempre el mismo (suma en firma,
#    directo en establecimiento), salvo PORCVT/PORCON (promedio
#    ponderado). Un comentario de una linea por variable, con la
#    descripcion tal como aparece en el diccionario maestro.
# ------------------------------------------------------------------

mapa_simples <- tibble::tribble(
  ~variable_nueva, ~codigo_eam, ~tipo,
  # --- Personal, agregados directos de la EAM (no C4R sueltos) ---
  "personal_total_pertotal", "PERTOTAL", "suma", # Personal permanente + propietarios + temporal directo + temporal agencias
  "personal_permanente_persocu", "PERSOCU", "suma", # Personal Permanente
  "personal_permanente_propietarios_persoesc", "PERSOESC", "suma", # Personal Permanente + Propietarios
  "personal_temporal_directo_pertem3", "PERTEM3", "suma", # Personal Temporal Directo
  "personal_permanente_temporal_pperytem", "PPERYTEM", "suma", # Personal Permanente + Temporal Directo
  "personal_mujeres_c4r4c9t", "C4R4C9T", "suma", # Total Personal Ocupado Mujer
  "personal_hombres_c4r4c10t", "C4R4C10T", "suma", # Total Personal Ocupado Hombre

  # --- Salariales: C1 obreros, C2 administrativos, PT profesionales, C3 total ---
  "salario_integral_obreros_c3r1c1", "C3R1C1", "suma", # Salario Integral personal permanente - Produccion - Obreros
  "salario_integral_administrativos_c3r1c2", "C3R1C2", "suma", # Salario Integral personal permanente - Administrativos
  "salario_integral_total_c3r1c3", "C3R1C3", "suma", # Total Salario Integral personal permanente
  "salario_integral_profesionales_c3r1pt", "C3R1PT", "suma", # Salario Integral personal permanente - Produccion - Profesional Tecnologo
  "sueldos_permanentes_obreros_c3r2c1", "C3R2C1", "suma", # Sueldos y salarios personal permanente - Produccion - Obreros
  "sueldos_permanentes_administrativos_c3r2c2", "C3R2C2", "suma", # Sueldos y salarios personal permanente - Administrativos
  "sueldos_permanentes_total_c3r2c3", "C3R2C3", "suma", # Total sueldos y salarios personal permanente
  "sueldos_permanentes_profesionales_c3r2pt", "C3R2PT", "suma", # Sueldos y salarios personal permanente - Produccion - Profesional Tecnologo
  "prestaciones_permanentes_obreros_c3r3c1", "C3R3C1", "suma", # Prestaciones sociales personal permanente - Produccion - Obreros
  "prestaciones_permanentes_administrativos_c3r3c2", "C3R3C2", "suma", # Prestaciones sociales personal permanente - Administrativos
  "prestaciones_permanentes_total_c3r3c3", "C3R3C3", "suma", # Total prestaciones sociales personal permanente
  "prestaciones_permanentes_profesionales_c3r3pt", "C3R3PT", "suma", # Prestaciones sociales personal permanente - Produccion - Profesional Tecnologo
  "sueldos_temporal_directo_obreros_c3r4c1", "C3R4C1", "suma", # Sueldos, salarios y prestaciones personal temporal contratado directamente
  "sueldos_temporal_directo_administrativos_c3r4c2", "C3R4C2", "suma", # Idem, administrativos
  "sueldos_temporal_directo_total_c3r4c3", "C3R4C3", "suma", # Total sueldos, salarios y prestaciones personal temporal contratado directamente
  "sueldos_temporal_directo_profesionales_c3r4pt", "C3R4PT", "suma", # Idem, profesionales/tecnicos
  "cotizaciones_patronales_obreros_c3r5c1", "C3R5C1", "suma", # Cotizaciones patronales obligatorias (salud, ARP, pension) - Obreros
  "cotizaciones_patronales_administrativos_c3r5c2", "C3R5C2", "suma", # Idem, administrativos
  "cotizaciones_patronales_total_c3r5c3", "C3R5C3", "suma", # Total cotizaciones patronales obligatorias
  "cotizaciones_patronales_profesionales_c3r5pt", "C3R5PT", "suma", # Idem, profesionales/tecnicos
  "aportes_nomina_obreros_c3r6c1", "C3R6C1", "suma", # Aportes sobre nomina (SENA, cajas de compensacion, ICBF) - Obreros
  "aportes_nomina_administrativos_c3r6c2", "C3R6C2", "suma", # Idem, administrativos
  "aportes_nomina_total_c3r6c3", "C3R6C3", "suma", # Total aportes sobre nomina
  "aportes_nomina_profesionales_c3r6pt", "C3R6PT", "suma", # Idem, profesionales/tecnicos
  "seguros_vida_voluntarios_obreros_c3r7c1", "C3R7C1", "suma", # Aportes voluntarios a seguros de vida - Obreros
  "seguros_vida_voluntarios_administrativos_c3r7c2", "C3R7C2", "suma", # Idem, administrativos
  "seguros_vida_voluntarios_total_c3r7c3", "C3R7C3", "suma", # Total aportes voluntarios a seguros de vida
  "seguros_vida_voluntarios_profesionales_c3r7pt", "C3R7PT", "suma", # Idem, profesionales/tecnicos
  "agencias_personal_temporal_obreros_c3r8c1", "C3R8C1", "suma", # Valor causado por agencias de personal temporal - Obreros
  "agencias_personal_temporal_administrativos_c3r8c2", "C3R8C2", "suma", # Idem, administrativos
  "agencias_personal_temporal_total_c3r8c3", "C3R8C3", "suma", # Total valor causado por agencias de personal temporal
  "agencias_personal_temporal_profesionales_c3r8pt", "C3R8PT", "suma", # Idem, profesionales/tecnicos
  "otros_gastos_personal_obreros_c3r9c1", "C3R9C1", "suma", # Otros gastos del personal no incluidos antes - Obreros
  "otros_gastos_personal_administrativos_c3r9c2", "C3R9C2", "suma", # Idem, administrativos
  "otros_gastos_personal_total_c3r9c3", "C3R9C3", "suma", # Total otros gastos del personal
  "otros_gastos_personal_profesionales_c3r9pt", "C3R9PT", "suma", # Idem, profesionales/tecnicos
  "costos_totales_personal_obreros_c3r10c1", "C3R10C1", "suma", # Total costos y gastos causados por el personal ocupado - Obreros
  "costos_totales_personal_administrativos_c3r10c2", "C3R10C2", "suma", # Idem, administrativos
  "costos_totales_personal_total_c3r10c3", "C3R10C3", "suma", # Total costos y gastos causados por el personal ocupado
  "costos_totales_personal_profesionales_c3r10pt", "C3R10PT", "suma", # Idem, profesionales/tecnicos
  "salario_permanentes_salarper", "SALARPER", "suma", # Salario de Permanentes
  "salario_permanente_temporal_salpeyte", "SALPEYTE", "suma", # Salario Personal Permanente y Temporal
  "prestaciones_permanentes_pressper", "PRESSPER", "suma", # Prestaciones de Permanentes
  "prestaciones_permanente_temporal_prespyte", "PRESPYTE", "suma", # Prestaciones de Permanente y Temporal Directo
  "remuneracion_temporales_remutemp", "REMUTEMP", "suma", # Remuneracion empleados temporales
  "salarios_aprendices_salapren", "SALAPREN", "suma", # Salarios del personal aprendiz
  "prestaciones_aprendices_perapren", "PERAPREN", "suma", # Prestaciones del personal aprendiz
  "apoyo_sostenimiento_aprendices_r4csap", "R4CSAP", "suma", # Total valor del apoyo de sostenimiento causado por aprendices y pasantes (Ley 789)

  # --- Tercerizacion y mantenimiento ---
  "terceros_produccion_c3r41c1", "C3R41C1", "suma", # Costos y gastos por servicios contratados con terceros (outsourcing) - Produccion
  "terceros_administracion_c3r41c2", "C3R41C2", "suma", # Idem, administracion y ventas
  "terceros_total_c3r41c3", "C3R41C3", "suma", # Total costos y gastos por servicios contratados con terceros (outsourcing)
  "terceros_industriales_produccion_c3r14c1", "C3R14C1", "suma", # Costos y gastos de productos y servicios industriales elaborados por terceros
  "terceros_industriales_total_c3r14c3", "C3R14C3", "suma", # Total costos y gastos de productos y servicios industriales elaborados por terceros
  "honorarios_produccion_c3r15c1", "C3R15C1", "suma", # Honorarios y servicios tecnicos - Produccion
  "honorarios_administracion_c3r15c2", "C3R15C2", "suma", # Honorarios y servicios tecnicos - Administracion y ventas
  "honorarios_total_c3r15c3", "C3R15C3", "suma", # Total honorarios y servicios tecnicos
  "mantenimiento_produccion_c3r23c1", "C3R23C1", "suma", # Mantenimiento, reparaciones, accesorios y repuestos - Produccion
  "mantenimiento_administracion_c3r23c2", "C3R23C2", "suma", # Idem, administracion
  "mantenimiento_total_c3r23c3", "C3R23C3", "suma", # Total mantenimiento, reparaciones, accesorios y repuestos
  "arriendo_maquinaria_produccion_c3r17c1", "C3R17C1", "suma", # Arrendamiento sin opcion de compra de maquinaria y equipo - Produccion
  "arriendo_maquinaria_administracion_c3r17c2", "C3R17C2", "suma", # Idem, administracion y ventas
  "arriendo_maquinaria_total_c3r17c3", "C3R17C3", "suma", # Total arrendamiento sin opcion de compra de maquinaria y equipo

  # --- Inversion y activos ---
  "inversion_total_c7r10c2", "C7R10C2", "suma", # Total inversiones en activos fijos
  "inversion_terrenos_c7r1c2", "C7R1C2", "suma", # Total inversiones en activos fijos - Terrenos
  "inversion_edificios_c7r5c2", "C7R5C2", "suma", # Total inversiones en activos fijos - Edificios y estructuras
  "inversion_maquinaria_c7r6c2", "C7R6C2", "suma", # Total inversiones en activos fijos - Maquinaria y equipo industrial
  "inversion_transporte_c7r8c2", "C7R8C2", "suma", # Total inversiones en activos fijos - Equipo de transporte
  "inversion_informatica_c7c4r8", "C7C4R8", "suma", # Total inversiones en activos fijos - Equipo de informatica y comunicacion
  "inversion_oficina_c7c5r8", "C7C5R8", "suma", # Total inversiones en activos fijos - Equipos de oficina
  "retiros_edificios_c7c2r11", "C7C2R11", "suma", # Retiros causados - Edificios y estructuras
  "retiros_maquinaria_c7c3r11", "C7C3R11", "suma", # Retiros causados - Maquinaria y equipo industrial
  "retiros_informatica_c7c4r11", "C7C4R11", "suma", # Retiros causados - Equipo de informatica y comunicacion
  "retiros_oficina_c7c5r11", "C7C5R11", "suma", # Retiros causados - Equipos de oficina
  "retiros_transporte_c7c6r11", "C7C6R11", "suma", # Retiros causados - Equipo de transporte
  "retiros_total_c7c7r11", "C7C7R11", "suma", # Total retiros causados
  "leasing_terrenos_c7c1r19", "C7C1R19", "suma", # Leasing financiero - Terrenos
  "leasing_edificios_c7c2r19", "C7C2R19", "suma", # Leasing financiero - Edificios y estructuras
  "leasing_maquinaria_c7c3r19", "C7C3R19", "suma", # Leasing financiero - Maquinaria y equipo industrial
  "leasing_informatica_c7c4r19", "C7C4R19", "suma", # Leasing financiero - Equipo de informatica y comunicacion
  "leasing_oficina_c7c5r19", "C7C5R19", "suma", # Leasing financiero - Equipos de oficina
  "leasing_transporte_c7c6r19", "C7C6R19", "suma", # Leasing financiero - Equipo de transporte
  "leasing_total_c7c7r19", "C7C7R19", "suma", # Total leasing financiero
  "desvalorizaciones_total_c7r16c7", "C7R16C7", "suma", # Total desvalorizaciones causadas en el ano
  "inversion_bruta_invebrta", "INVEBRTA", "suma", # Inversion Bruta
  "activos_fijos_activfi", "ACTIVFI", "suma", # Activos Fijos
  "depreciacion_deprecia", "DEPRECIA", "suma", # Depreciacion

  # --- Produccion y ventas ---
  "produccion_bruta_prodbr2", "PRODBR2", "suma", # Produccion Bruta
  "produccion_industrial_prodbind", "PRODBIND", "suma", # Produccion Industrial
  "valor_agregado_valagri", "VALAGRI", "suma", # Valor Agregado
  "consumo_intermedio_consin2", "CONSIN2", "suma", # Consumo Intermedio
  "consumo_materias_consmate", "CONSMATE", "suma", # Consumo de Materias
  "otros_gastos_consin", "CONSIN", "suma", # Otros Gastos
  "valor_ventas_valorven", "VALORVEN", "suma", # Valor de las ventas

  # --- Cincuenta adicionales ---
  "costos_produccion_c3r35c1", "C3R35C1", "suma", # Total Costos y gastos de produccion
  "gastos_administracion_ventas_c3r35c2", "C3R35C2", "suma", # Total Gastos de administracion y ventas
  "costos_totales_c3r35c3", "C3R35C3", "suma", # Total costos de produccion, administracion y ventas
  "intereses_c3r31c3", "C3R31C3", "suma", # Total intereses causados sobre prestamos
  "gastos_financieros_c3r46c3", "C3R46C3", "suma", # Total gastos financieros
  "arriendo_inmuebles_c3r16c3", "C3R16C3", "suma", # Total arrendamiento de bienes inmuebles
  "seguros_c3r18c3", "C3R18C3", "suma", # Total seguros
  "valor_energia_comprada_c3r19c3", "C3R19C3", "suma", # Valor energia comprada
  "agua_c3r21c3", "C3R21C3", "suma", # Total servicio de agua
  "publicidad_c3r22c3", "C3R22C3", "suma", # Total propaganda y publicidad
  "franquicias_marcas_c3r24c3", "C3R24C3", "suma", # Total utilizacion de derechos de autor, franquicias, marcas, patentes
  "comunicaciones_c3r36c3", "C3R36C3", "suma", # Total servicios de comunicaciones
  "industria_comercio_c3r37c3", "C3R37C3", "suma", # Total impuestos de industria y comercio
  "predial_vehiculos_c3r38c3", "C3R38C3", "suma", # Total impuesto predial y sobre vehiculos
  "transporte_materias_primas_c3r42c3", "C3R42C3", "suma", # Total costos y gastos de transporte de materias primas
  "transporte_productos_c3r45c3", "C3R45C3", "suma", # Total costos y gastos de transporte de productos
  "provisiones_c3r26c3", "C3R26C3", "suma", # Total gastos para provision de cartera, inventarios y otros
  "otros_costos_c3r27c3", "C3R27C3", "suma", # Total otros costos y gastos no incluidos antes
  "costo_productos_no_fabricados_c3r44c3", "C3R44C3", "suma", # Total costo de venta de productos no fabricados por el establecimiento
  "costo_materias_primas_sin_transformar_c3r40c3", "C3R40C3", "suma", # Total costo de venta de materias primas, materiales y empaques sin transformar
  "muestras_gratis_c3r13c3", "C3R13C3", "suma", # Total muestras gratis
  "impuesto_4x1000_c3r25c3", "C3R25C3", "suma", # Total impuesto del 4 x 1000
  "materias_primas_inicio_c6r1c1", "C6R1C1", "suma", # Existencias materias primas, materiales y empaques - fin del ano anterior
  "materias_primas_fin_c6r1c3", "C6R1C3", "suma", # Existencias materias primas, materiales y empaques - fin del ano actual
  "productos_proceso_inicio_c6r2c1", "C6R2C1", "suma", # Existencias productos en proceso - fin del ano anterior
  "productos_proceso_fin_c6r2c3", "C6R2C3", "suma", # Existencias productos en proceso - fin del ano actual
  "productos_terminados_inicio_c6r3c1", "C6R3C1", "suma", # Existencias productos terminados - fin del ano anterior
  "productos_terminados_fin_c6r3c3", "C6R3C3", "suma", # Existencias productos terminados - fin del ano actual
  "inventario_total_inicio_c6r5c1", "C6R5C1", "suma", # Total inventarios al final del ano anterior
  "inventario_total_fin_c6r5c3", "C6R5C3", "suma", # Total inventarios al final del ano actual
  "valor_libros_inicio_c7r10c1", "C7R10C1", "suma", # Total valor en libros del ano anterior
  "valor_libros_fin_c7r10c6", "C7R10C6", "suma", # Total valor en libros del ano actual
  "depreciacion_causada_c7r10c7", "C7R10C7", "suma", # Total depreciacion causada en el ano
  "valorizaciones_c7r10c4", "C7R10C4", "suma", # Total valorizaciones causadas en el ano
  "valor_libros_maquinaria_c7r6c6", "C7R6C6", "suma", # Valor en libros del ano actual - Maquinaria y equipo industrial
  "depreciacion_maquinaria_c7r6c7", "C7R6C7", "suma", # Depreciacion causada en el ano - Maquinaria y equipo industrial
  "activos_nuevos_c7c7r2", "C7C7R2", "suma", # Total valor compra de activos nuevos
  "activos_usados_c7c7r3", "C7C7R3", "suma", # Total valor compra de activos usados
  "mejoras_activos_c7c7r7", "C7C7R7", "suma", # Total valor causado por mejoras y reformas a los activos
  "activos_trasladados_c7r17c7", "C7R17C7", "suma", # Total valor de activos trasladados a otros establecimientos de la empresa
  "activos_recibidos_c7r18c7", "C7R18C7", "suma", # Total valor de activos recibidos por traslado de otros establecimientos
  "materia_prima_comprada_valorcom", "VALORCOM", "suma", # Valor de la materia prima comprada
  "materia_prima_importada_valorcx", "VALORCX", "suma", # Valor compras de materia prima en el exterior
  "porcentaje_exportado_porcvt", "PORCVT", "wmean", # Porcentaje vendido al exterior (promedio ponderado por VALORVEN en firma)
  "porcentaje_insumos_importados_porcon", "PORCON", "wmean", # Porcentaje consumo de origen extranjero (promedio ponderado por VALORVEN en firma)
  "energia_comprada_kwh_c5r1c1", "C5R1C1", "suma", # Energia comprada (KWH)
  "energia_consumida_kwh_c5r1c4", "C5R1C4", "suma", # Total energia consumida (KWH)
  "energia_electrica_kw_eelec", "EELEC", "suma", # Energia Electrica en kw
  "valor_energeticos_totalv", "TOTALV", "suma", # Valor total energeticos
  "ingresos_cert_c2r5cert", "C2R5CERT", "suma" # Ingresos por CERT causados en el ano
)

message("Variables 'simples' en el mapa: ", nrow(mapa_simples))

# Avisamos y quitamos del mapa cualquier codigo que no exista en la
# macrobase (se documenta en variables_agregadas.csv como NA mas abajo,
# no se inventa el dato).
codigos_faltantes <- setdiff(mapa_simples$codigo_eam, names(macro_base))
if (length(codigos_faltantes) > 0) {
  message("AVISO -- estos codigos no existen en la macrobase, sus variables quedaran en NA: ",
          paste(codigos_faltantes, collapse = ", "))
}

# ------------------------------------------------------------------
# 3) Grupos de empleo construidos (formulas explicitas, identicas a
#    pipeline/01_construir_base.R y a las que ya usa el resto del
#    proyecto).
# ------------------------------------------------------------------

cols_obreros <- c("C4R2C1", "C4R2C2", "C4R3C1", "C4R3C2", "C4R4C1", "C4R4C2", "C4R6OM", "C4R6OH")
cols_administrativos <- c("C4R2C3", "C4R2C4", "C4R3C3", "C4R3C4", "C4R4C3", "C4R4C4", "C4R6DM", "C4R6DH")
cols_prof_tecnico <- c(
  "C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E",
  "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E",
  "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E",
  "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE"
)
cols_propietarios <- c("C4R1C1", "C4R1C2", "C4R1C3", "C4R1C4", "C4R1C1N", "C4R1C2N", "C4R2C1E", "C4R2C2E")
cols_obreros_permanentes <- c("C4R2C1", "C4R2C2")
cols_administrativos_permanentes <- c("C4R2C3", "C4R2C4")
cols_profesionales_permanentes <- c("C4R1C3N", "C4R1C4N", "C4R2C3E", "C4R2C4E")
cols_temporal_directo <- c("C4R3C1", "C4R3C2", "C4R3C3", "C4R3C4", "C4R1C5N", "C4R1C6N", "C4R2C5E", "C4R2C6E")
cols_temporal_agencias <- c("C4R4C1", "C4R4C2", "C4R4C3", "C4R4C4", "C4R1C7N", "C4R1C8N", "C4R2C7E", "C4R2C8E")
cols_aprendices <- c("C4R6OM", "C4R6OH", "C4R6DM", "C4R6DH", "C4R6MN", "C4R6HN", "C4R6ME", "C4R6HE")

cols_grupos_c4r <- unique(c(
  cols_obreros, cols_administrativos, cols_prof_tecnico, cols_propietarios,
  cols_obreros_permanentes, cols_administrativos_permanentes, cols_profesionales_permanentes,
  cols_temporal_directo, cols_temporal_agencias, cols_aprendices
))

codigos_c4r_faltantes <- setdiff(cols_grupos_c4r, names(macro_base))
if (length(codigos_c4r_faltantes) > 0) {
  message("AVISO -- estas columnas C4R para los grupos de empleo no existen en la macrobase: ",
          paste(codigos_c4r_faltantes, collapse = ", "))
  # Las creamos como NA para que las sumas de grupo no fallen.
  for (col in codigos_c4r_faltantes) macro_base[[col]] <- NA_real_
}

sumar_grupo <- function(datos, cols) {
  mat <- as.data.frame(lapply(datos[cols], safe_numeric))
  rowSums(mat, na.rm = TRUE)
}

# ------------------------------------------------------------------
# 4) Base cruda a nivel establecimiento (NORDEST-ANIO), con las
#    variables simples ya presentes en la macrobase, mas las columnas
#    C4R necesarias para los grupos de empleo.
# ------------------------------------------------------------------

codigos_simples_presentes <- intersect(mapa_simples$codigo_eam, names(macro_base))

base_cruda <- macro_base %>%
  dplyr::mutate(
    NORDEST = as.character(NORDEST),
    NORDEMP = as.character(NORDEMP),
    ANIO = as.integer(safe_numeric(ANIO))
  ) %>%
  dplyr::filter(!is.na(NORDEST), NORDEST != "", !is.na(NORDEMP), NORDEMP != "", !is.na(ANIO)) %>%
  dplyr::select(NORDEST, NORDEMP, ANIO, dplyr::all_of(unique(c(codigos_simples_presentes, cols_grupos_c4r)))) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(unique(c(codigos_simples_presentes, cols_grupos_c4r))), safe_numeric))

# Verificamos que NORDEST-ANIO sea unico, igual que
# construir_panel_establecimiento_formal.R. Si no lo es, nos detenemos
# en vez de inventar una regla de agregacion nueva.
n_dup_establecimiento <- base_cruda %>% dplyr::count(NORDEST, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup_establecimiento > 0) {
  stop("NORDEST-ANIO no es unico en la macrobase (", n_dup_establecimiento, " grupos duplicados). ",
       "No seguimos: revisar antes de continuar (ver construir_panel_establecimiento_formal.R).")
}
message("NORDEST-ANIO es unico en la macrobase (", nrow(base_cruda), " filas) -- sin necesidad de sumar plantas.")

# ------------------------------------------------------------------
# 5) Variables nuevas a nivel ESTABLECIMIENTO: las simples, tal cual
#    (renombradas), mas los grupos de empleo calculados con su formula.
# ------------------------------------------------------------------

nuevas_establecimiento <- base_cruda %>% dplyr::select(NORDEST, ANIO)

for (i in seq_len(nrow(mapa_simples))) {
  var_nueva <- mapa_simples$variable_nueva[i]
  codigo <- mapa_simples$codigo_eam[i]
  if (codigo %in% names(base_cruda)) {
    nuevas_establecimiento[[var_nueva]] <- base_cruda[[codigo]]
  } else {
    nuevas_establecimiento[[var_nueva]] <- NA_real_
  }
}

nuevas_establecimiento$obreros_total <- sumar_grupo(base_cruda, cols_obreros)
nuevas_establecimiento$administrativos_total <- sumar_grupo(base_cruda, cols_administrativos)
nuevas_establecimiento$profesionales_total <- sumar_grupo(base_cruda, cols_prof_tecnico)
nuevas_establecimiento$propietarios <- sumar_grupo(base_cruda, cols_propietarios)
nuevas_establecimiento$obreros_permanentes <- sumar_grupo(base_cruda, cols_obreros_permanentes)
nuevas_establecimiento$administrativos_permanentes <- sumar_grupo(base_cruda, cols_administrativos_permanentes)
nuevas_establecimiento$profesionales_permanentes <- sumar_grupo(base_cruda, cols_profesionales_permanentes)
nuevas_establecimiento$temporal_directo <- sumar_grupo(base_cruda, cols_temporal_directo)
nuevas_establecimiento$temporal_agencias <- sumar_grupo(base_cruda, cols_temporal_agencias)
nuevas_establecimiento$aprendices <- sumar_grupo(base_cruda, cols_aprendices)

variables_grupo <- c(
  "obreros_total", "administrativos_total", "profesionales_total", "propietarios",
  "obreros_permanentes", "administrativos_permanentes", "profesionales_permanentes",
  "temporal_directo", "temporal_agencias", "aprendices"
)

# ------------------------------------------------------------------
# 6) Variables nuevas a nivel FIRMA: sumamos las plantas de cada
#    NORDEMP-ANIO, salvo PORCVT/PORCON que promediamos ponderado por
#    VALORVEN. Agregamos n_establecimientos.
# ------------------------------------------------------------------

variables_suma <- c(mapa_simples$variable_nueva[mapa_simples$tipo == "suma"], variables_grupo)
variables_wmean <- mapa_simples$variable_nueva[mapa_simples$tipo == "wmean"]

base_firma_cruda <- base_cruda %>%
  dplyr::left_join(nuevas_establecimiento, by = c("NORDEST", "ANIO"))

n_establecimientos_tabla <- base_firma_cruda %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(n_establecimientos = dplyr::n_distinct(NORDEST), .groups = "drop")

suma_firma <- base_firma_cruda %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(variables_suma), ~if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Promedio ponderado por valor_ventas_valorven (ya calculada arriba
# entre las variables "suma"). Si no hay ventas positivas validas en
# la firma-anio, el resultado es NA.
wmean_firma <- base_firma_cruda %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(variables_wmean),
      ~{
        peso <- valor_ventas_valorven
        valido <- !is.na(.x) & !is.na(peso) & peso > 0
        if (!any(valido)) NA_real_ else sum(.x[valido] * peso[valido]) / sum(peso[valido])
      }
    ),
    .groups = "drop"
  )

nuevas_firma <- n_establecimientos_tabla %>%
  dplyr::left_join(suma_firma, by = c("NORDEMP", "ANIO")) %>%
  dplyr::left_join(wmean_firma, by = c("NORDEMP", "ANIO"))

n_dup_firma <- nuevas_firma %>% dplyr::count(NORDEMP, ANIO) %>% dplyr::filter(n > 1) %>% nrow()
if (n_dup_firma > 0) stop("NORDEMP-ANIO no quedo unico tras sumar plantas (", n_dup_firma, " grupos). Revisar.")
message("NORDEMP-ANIO unico tras sumar plantas: ", nrow(nuevas_firma), " filas.")

# ------------------------------------------------------------------
# 7) Evitamos duplicar variables que ya esten en cada base.
# ------------------------------------------------------------------

vars_candidatas_establecimiento <- setdiff(names(nuevas_establecimiento), c("NORDEST", "ANIO"))
vars_ya_en_establecimiento <- intersect(vars_candidatas_establecimiento, names(panel_establecimiento_actual))
if (length(vars_ya_en_establecimiento) > 0) {
  message("Ya estaban en panel_establecimiento_formal, no se duplican: ", paste(vars_ya_en_establecimiento, collapse = ", "))
}
vars_nuevas_establecimiento <- setdiff(vars_candidatas_establecimiento, vars_ya_en_establecimiento)

vars_candidatas_firma <- setdiff(names(nuevas_firma), c("NORDEMP", "ANIO"))
vars_ya_en_firma <- intersect(vars_candidatas_firma, names(panel_firma_actual))
if (length(vars_ya_en_firma) > 0) {
  message("Ya estaban en panel_analitico_firma_eam, no se duplican: ", paste(vars_ya_en_firma, collapse = ", "))
}
vars_nuevas_firma <- setdiff(vars_candidatas_firma, vars_ya_en_firma)

# ------------------------------------------------------------------
# 8) Unimos (left_join) a los paneles actuales, SOLO agregando columnas
#    nuevas -- las filas y columnas existentes no se tocan.
# ------------------------------------------------------------------

panel_establecimiento_nuevo <- panel_establecimiento_actual %>%
  dplyr::left_join(
    nuevas_establecimiento %>% dplyr::select(NORDEST, ANIO, dplyr::all_of(vars_nuevas_establecimiento)),
    by = c("NORDEST", "ANIO")
  )

panel_firma_nuevo <- panel_firma_actual %>%
  dplyr::left_join(
    nuevas_firma %>% dplyr::select(NORDEMP, ANIO, dplyr::all_of(vars_nuevas_firma)),
    by = c("NORDEMP", "ANIO")
  )

message("")
message("panel_establecimiento_formal -- columnas ANTES: ", ncol(panel_establecimiento_actual),
        " | DESPUES: ", ncol(panel_establecimiento_nuevo))
message("panel_analitico_firma_eam -- columnas ANTES: ", ncol(panel_firma_actual),
        " | DESPUES: ", ncol(panel_firma_nuevo))

# ------------------------------------------------------------------
# 9) Verificacion antes de sobrescribir: mismo numero de filas, y cada
#    columna original identica a la version actual.
# ------------------------------------------------------------------

verificar_intacto <- function(original, nuevo, nombre) {
  if (nrow(original) != nrow(nuevo)) {
    message("ERROR -- ", nombre, ": el numero de filas cambio (", nrow(original), " -> ", nrow(nuevo), "). No se sobrescribe.")
    return(FALSE)
  }
  problemas <- character(0)
  for (col in names(original)) {
    if (!identical(original[[col]], nuevo[[col]])) problemas <- c(problemas, col)
  }
  if (length(problemas) > 0) {
    message("ERROR -- ", nombre, ": estas columnas originales cambiaron: ", paste(problemas, collapse = ", "), ". No se sobrescribe.")
    return(FALSE)
  }
  message("Verificado -- ", nombre, ": filas y columnas originales identicas. OK para sobrescribir.")
  TRUE
}

ok_establecimiento <- verificar_intacto(panel_establecimiento_actual, panel_establecimiento_nuevo, "panel_establecimiento_formal")
ok_firma <- verificar_intacto(panel_firma_actual, panel_firma_nuevo, "panel_analitico_firma_eam")

# ------------------------------------------------------------------
# 10) Sobrescribimos en 1. DATOS/ solo si la verificacion paso.
# ------------------------------------------------------------------

quitar_factores <- function(df) df %>% dplyr::mutate(dplyr::across(where(is.factor), as.character))

# Stata (.dta) no acepta nombres de variable de mas de 32 caracteres --
# varios de nuestros nombres nuevos si los superan. Para el .dta
# truncamos a 32 caracteres (agregando un numero si hace falta para que
# sigan siendo unicos); el .rds y el .csv guardan el nombre completo, no
# se tocan.
preparar_para_dta <- function(df) {
  nombres_originales <- names(df)
  largos <- nchar(nombres_originales) > 32
  if (any(largos)) {
    message("AVISO -- ", sum(largos), " nombres de columna superan 32 caracteres, se truncan solo para el .dta.")
  }
  nombres_nuevos <- nombres_originales
  usados <- character(0)
  for (i in seq_along(nombres_nuevos)) {
    nombre <- nombres_nuevos[i]
    if (nchar(nombre) > 32) {
      base_corta <- substr(nombre, 1, 32)
      candidato <- base_corta
      sufijo <- 1
      while (candidato %in% usados) {
        sufijo_txt <- as.character(sufijo)
        candidato <- paste0(substr(base_corta, 1, 32 - nchar(sufijo_txt)), sufijo_txt)
        sufijo <- sufijo + 1
      }
      nombres_nuevos[i] <- candidato
    }
    usados <- c(usados, nombres_nuevos[i])
  }
  names(df) <- nombres_nuevos
  df
}

if (ok_establecimiento) {
  readr::write_rds(panel_establecimiento_nuevo, file.path(datos_raiz, "panel_establecimiento_formal.rds"))
  readr::write_csv(panel_establecimiento_nuevo, file.path(datos_raiz, "panel_establecimiento_formal.csv"))
  haven::write_dta(preparar_para_dta(quitar_factores(panel_establecimiento_nuevo)), file.path(datos_raiz, "panel_establecimiento_formal.dta"))
  message("panel_establecimiento_formal sobrescrito en 1. DATOS/ (.rds, .csv, .dta).")
} else {
  message("panel_establecimiento_formal NO se sobrescribio.")
}

if (ok_firma) {
  readr::write_rds(panel_firma_nuevo, file.path(datos_raiz, "panel_analitico_firma_eam.rds"))
  readr::write_csv(panel_firma_nuevo, file.path(datos_raiz, "panel_analitico_firma_eam.csv"))
  haven::write_dta(preparar_para_dta(quitar_factores(panel_firma_nuevo)), file.path(datos_raiz, "panel_analitico_firma_eam.dta"))
  message("panel_analitico_firma_eam sobrescrito en 1. DATOS/ (.rds, .csv, .dta).")
} else {
  message("panel_analitico_firma_eam NO se sobrescribio.")
}

# ------------------------------------------------------------------
# 11) variables_agregadas.csv: nombre nuevo, codigo EAM, descripcion
#     (del diccionario maestro), base, % NA y % ceros en 2022.
# ------------------------------------------------------------------

diccionario <- readr::read_csv(
  file.path(paths$diccionarios, "diccionario_maestro_variables.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(fuente == "EAM") %>%
  dplyr::distinct(variable, .keep_all = TRUE) %>%
  dplyr::transmute(codigo_eam = variable, descripcion_diccionario = descripcion_final)

calcular_cobertura <- function(df, vars, anio_col = "ANIO") {
  datos_2022 <- df %>% dplyr::filter(.data[[anio_col]] == 2022)
  purrr_map <- lapply(vars, function(v) {
    x <- datos_2022[[v]]
    tibble::tibble(
      variable_nueva = v,
      pct_na_2022 = round(100 * mean(is.na(x)), 2),
      pct_cero_2022 = round(100 * mean(x == 0, na.rm = TRUE), 2)
    )
  })
  dplyr::bind_rows(purrr_map)
}

# Usamos la lista COMPLETA de variables objetivo (no solo las que se
# unieron en esta corrida) para que el reporte sea el mismo sin importar
# si el script ya se habia corrido antes (variables que "ya estaban" no
# se pierden del reporte).
cobertura_establecimiento <- calcular_cobertura(panel_establecimiento_nuevo, vars_candidatas_establecimiento) %>%
  dplyr::mutate(base = "establecimiento")
cobertura_firma <- calcular_cobertura(panel_firma_nuevo, vars_candidatas_firma) %>%
  dplyr::mutate(base = "firma")

variables_agregadas <- dplyr::bind_rows(cobertura_establecimiento, cobertura_firma) %>%
  dplyr::left_join(
    dplyr::bind_rows(
      mapa_simples %>% dplyr::select(variable_nueva, codigo_eam),
      tibble::tibble(
        variable_nueva = variables_grupo,
        codigo_eam = c(
          "suma(C4R2C1,C4R2C2,C4R3C1,C4R3C2,C4R4C1,C4R4C2,C4R6OM,C4R6OH)",
          "suma(C4R2C3,C4R2C4,C4R3C3,C4R3C4,C4R4C3,C4R4C4,C4R6DM,C4R6DH)",
          "suma(16 columnas C4R1/C4R2 profesional-tecnico)",
          "suma(C4R1C1,C4R1C2,C4R1C3,C4R1C4,C4R1C1N,C4R1C2N,C4R2C1E,C4R2C2E)",
          "suma(C4R2C1,C4R2C2)",
          "suma(C4R2C3,C4R2C4)",
          "suma(C4R1C3N,C4R1C4N,C4R2C3E,C4R2C4E)",
          "suma(C4R3C1,C4R3C2,C4R3C3,C4R3C4,C4R1C5N,C4R1C6N,C4R2C5E,C4R2C6E)",
          "suma(C4R4C1,C4R4C2,C4R4C3,C4R4C4,C4R1C7N,C4R1C8N,C4R2C7E,C4R2C8E)",
          "suma(C4R6OM,C4R6OH,C4R6DM,C4R6DH,C4R6MN,C4R6HN,C4R6ME,C4R6HE)"
        )
      )
    ),
    by = "variable_nueva"
  ) %>%
  dplyr::left_join(diccionario, by = "codigo_eam") %>%
  dplyr::mutate(
    descripcion = dplyr::if_else(
      is.na(descripcion_diccionario),
      "Variable construida (suma de un grupo de columnas C4R) -- ver comentario en ampliar_variables_paneles.R",
      descripcion_diccionario
    )
  ) %>%
  dplyr::select(variable_nueva, codigo_eam, descripcion, base, pct_na_2022, pct_cero_2022) %>%
  dplyr::arrange(base, variable_nueva)

out_variables_agregadas <- file.path("0. PREPARACION", "variables_agregadas.csv")
readr::write_csv(variables_agregadas, out_variables_agregadas)
message("")
message("variables_agregadas.csv guardado en: ", out_variables_agregadas, " (", nrow(variables_agregadas), " filas)")

# ------------------------------------------------------------------
# 12) Resumen final en consola.
# ------------------------------------------------------------------

script_header("ampliar_variables_paneles.R -- resumen")
message("")
message("=== panel_establecimiento_formal ===")
message("Columnas ANTES (", ncol(panel_establecimiento_actual), "): ", paste(names(panel_establecimiento_actual), collapse = ", "))
message("Columnas DESPUES (", ncol(panel_establecimiento_nuevo), "): ", paste(names(panel_establecimiento_nuevo), collapse = ", "))
message("")
message("=== panel_analitico_firma_eam ===")
message("Columnas ANTES (", ncol(panel_firma_actual), "): ", paste(names(panel_firma_actual), collapse = ", "))
message("Columnas DESPUES (", ncol(panel_firma_nuevo), "): ", paste(names(panel_firma_nuevo), collapse = ", "))
message("")
message("=== Variables con mas de 80% de NA o de ceros en 2022 ===")
alerta_80 <- variables_agregadas %>% dplyr::filter(pct_na_2022 > 80 | pct_cero_2022 > 80)
print(alerta_80, n = Inf, width = Inf)
