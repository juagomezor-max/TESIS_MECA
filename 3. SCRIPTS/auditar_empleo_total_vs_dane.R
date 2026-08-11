# Sanity check (Paso 3 de la auditoria de reproducibilidad): compara el
# empleo manufacturero total implicito en la macrobase (PERTOTAL, sumado
# por NORDEMP-ANIO igual que en los paneles derivados) contra las cifras
# de "personal ocupado" publicadas por DANE en los boletines tecnicos
# oficiales de la EAM, para 2015, 2019 y 2023.
#
# Cifras oficiales DANE (extraidas directamente de los boletines PDF,
# texto literal "De las X personas ocupadas por la industria colombiana"):
# - 2015: 711.827 (https://www.dane.gov.co/files/investigaciones/boletines/eam/boletin_eam_2015.pdf)
# - 2019: 705.999 (https://www.dane.gov.co/files/investigaciones/boletines/eam/boletin_eam_2019.pdf)
# - 2023: 719.183 (https://www.dane.gov.co/files/operaciones/EAM/bol-EAM-2023.pdf)
#
# PERTOTAL (definicion oficial DANE, citada en los mismos boletines):
# Personal permanente + temporal directo + temporal por agencias +
# aprendices + propietarios/socios/familiares sin remuneracion fija. Es
# el "personal ocupado total" en la terminologia de DANE, por lo que es
# la variable correcta para esta comparacion (no empleo_total_categorias
# de la rama feature/exposicion-obreros-operarios, que excluye
# deliberadamente a los propietarios).
#
# Salidas (no versionadas):
# - 1. DATOS/6. BASES_DERIVADAS/descriptivos_exposicion/auditoria_empleo_vs_dane.csv

source(file.path("3. SCRIPTS", "_utils_proyecto.R"))

required_packages <- c("dplyr", "readr", "tibble")
load_project_packages(required_packages)

paths <- ensure_project_structure()
data_dir <- paths$bases_derivadas_exposicion

macro_base <- readr::read_rds(paths$macro_base_eam)
names(macro_base) <- toupper(names(macro_base))
check_required_vars(macro_base, c("NORDEMP", "ANIO", "PERTOTAL"))

anios_comparar <- c(2015, 2019, 2023)

empleo_implicito <- macro_base %>%
  dplyr::mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(ANIO)) %>%
  dplyr::filter(ANIO %in% anios_comparar) %>%
  dplyr::group_by(NORDEMP, ANIO) %>%
  dplyr::summarise(PERTOTAL = if (all(is.na(PERTOTAL))) NA_real_ else sum(PERTOTAL, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(ANIO) %>%
  dplyr::summarise(n_empresas = dplyr::n(), empleo_total_implicito = sum(PERTOTAL, na.rm = TRUE), .groups = "drop")

cifras_dane <- tibble::tribble(
  ~ANIO, ~empleo_dane_oficial, ~fuente,
  2015, 711827, "https://www.dane.gov.co/files/investigaciones/boletines/eam/boletin_eam_2015.pdf",
  2019, 705999, "https://www.dane.gov.co/files/investigaciones/boletines/eam/boletin_eam_2019.pdf",
  2023, 719183, "https://www.dane.gov.co/files/operaciones/EAM/bol-EAM-2023.pdf"
)

comparacion <- empleo_implicito %>%
  dplyr::left_join(cifras_dane, by = "ANIO") %>%
  dplyr::mutate(
    diferencia_absoluta = empleo_total_implicito - empleo_dane_oficial,
    diferencia_pct = round(100 * diferencia_absoluta / empleo_dane_oficial, 3)
  ) %>%
  dplyr::select(ANIO, n_empresas, empleo_total_implicito, empleo_dane_oficial, diferencia_absoluta, diferencia_pct, fuente)

readr::write_csv(comparacion, file.path(data_dir, "auditoria_empleo_vs_dane.csv"))

script_header("Empleo total implicito en la macrobase vs. cifras oficiales DANE (EAM)")
print(comparacion %>% dplyr::select(-fuente), n = Inf, width = Inf)
message("")
message("Tabla exportada en: ", file.path(data_dir, "auditoria_empleo_vs_dane.csv"))
