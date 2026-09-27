# ==============================================================================
# 11_placebo_rodante_real.R
#
# Tesis: Rigideces laborales y decisiones de la firma: evidencia desde choques
#        en costos laborales en Colombia
# Autores: Julio Gómez y Nicolás Jácome
#
# ¿EL PRIMER ESLABÓN ES EL CHOQUE O ES REVERSIÓN A LA MEDIA? — VERSIÓN CON
# AUMENTOS REALES DEL SALARIO MÍNIMO
#
# QUÉ CORRIGE ESTE SCRIPT RESPECTO A 09_diagnostico_reversion.R
# Esa versión ordenó y comparó los años por el aumento NOMINAL del salario
# mínimo. Eso está mal: el aumento nominal no dice cuánto subió el costo
# laboral REAL de la firma, porque no descuenta la inflación del año en que el
# salario está vigente. Con el IPC del DANE, el orden de los años cambia:
# 2022 (+10,07% nominal) parecía un año fuerte y 2015 (+4,60% nominal) débil;
# en términos reales es exactamente al revés (2022: -2,70% real, 2015: -2,03%
# real). 2023 y 2024 son, por un margen amplio, los dos años de mayor aumento
# real del panel. Esto además cierra un pendiente abierto desde agosto: la
# premisa de que 2023 fue un choque atípico SÍ se sostiene en términos reales
# (más del doble que cualquier año de 2015-2019).
#
# Todo lo demás de 09_diagnostico_reversion.R (Parte 1: placebo rodante;
# Parte 3: colinealidad de la medida C; Parte 4: compresión salarial) se deja
# igual -- solo cambia la Parte 2 (sección 3 de este script), que ahora escala
# el diferencial por el aumento REAL en vez del nominal, y agrega una
# comparación directa por grupos de años.
#
# QUÉ MOTIVA EL SCRIPT ORIGINAL (09_diagnostico_reversion.R)
# La tabla T25 de 08_tratamiento_continuo.R comparó, para el quintil más
# expuesto, el salto salarial del año del choque contra el de un año sin choque:
#
#   Medida                          Placebo 2019    Real 2023
#   A. Salario de la firma (Bite)      +9,41% ***   +10,09% ***
#   B. Salario promediado              +6,12% ***    +7,60% ***
#   C. Salario de firmas parecidas     -0,43%        -0,57%
#
# Leída de frente, esa tabla dice que Bite produce en un año cualquiera casi el
# mismo salto que en el año del choque, y que al quitar el salario propio del
# denominador (medida C) el efecto desaparece. Si eso es correcto, el primer
# eslabón de la tesis es en buena parte reversión a la media y no el salario
# mínimo.
#
# PERO LA TABLA NO ALCANZA PARA CONCLUIR, por dos razones concretas:
#
#   1. 2019 NO fue un año sin choque. El mínimo subió 6,0% ese año (y 5,9% en
#      2018). El "placebo" no compara choque contra nada: compara un choque
#      grande contra uno normal. Que el diferencial sea parecido puede
#      significar que 2023 no fue excepcional, que es una afirmación distinta
#      (y más débil) que "la medida inventa efectos".
#
#   2. El cero de la medida C puede ser mecánico. C es el salario promedio de
#      las firmas de la misma celda sector x departamento x tamaño, así que
#      varía poco DENTRO de cada celda. El modelo lleva sector_2022^ANIO_F
#      como efecto fijo, que absorbe justamente esa variación. Un cero por
#      colinealidad no es lo mismo que un cero económico.
#
# QUÉ HACE ESTE SCRIPT
#   Parte 1. Placebo rodante: construye la MISMA medida para cada año del panel
#            y estima el salto de cada año. Si 2023 no sobresale de la serie,
#            no hay choque que identificar. Si sobresale, el placebo de un
#            único año era engañoso y el diseño se sostiene.
#   Parte 2. Escala el salto de cada año por el tamaño del aumento del mínimo
#            de ese año. Si el mecanismo es real, un aumento mayor debe
#            producir un diferencial mayor.
#   Parte 3. Prueba de colinealidad de la medida C: la estima con y sin el
#            control de sector x año, para saber si su cero es real o absorbido.
#   Parte 4. Cuánta variación de cada medida sobrevive a los controles.
#
# ESTE SCRIPT NO DECIDE NADA POR SÍ SOLO. Produce la evidencia para decidir, y
# deja escrito qué lectura corresponde a cada resultado posible.
#
# Entradas: 1. DATOS/panel_firma_eam_expalt_completo.rds
# Salidas:  4. RESULTADOS/ARCHIVADO_commit_89b959e/Placebo_rodante_real/
# ==============================================================================


# ==============================================================================
# 0. PREPARACIÓN
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(flextable)

CARPETA <- file.path("4. RESULTADOS", "ARCHIVADO_commit_89b959e", "Placebo_rodante_real")
dir.create(file.path(CARPETA, "figuras"), recursive = TRUE, showWarnings = FALSE)

titulo <- function(texto) {
  cat("\n", strrep("=", 78), "\n", texto, "\n", strrep("=", 78), "\n", sep = "")
}

ver <- function(tabla, filas = 30) {
  nombre <- deparse(substitute(tabla))
  cat("\n>>> ", nombre, ": ", nrow(tabla), " filas y ", ncol(tabla), " columnas\n", sep = "")
  print(as.data.frame(head(tabla, filas)), row.names = FALSE)
}

compendio <- list()
guardar_tabla <- function(tabla, nombre_archivo, titulo_tabla, decimales = 3) {
  tw <- flextable(tabla)
  tw <- colformat_double(tw, digits = decimales)
  tw <- set_caption(tw, caption = titulo_tabla)
  tw <- autofit(tw)
  save_as_docx(tw, path = file.path(CARPETA, paste0(nombre_archivo, ".docx")))
  write_csv(tabla, file.path(CARPETA, paste0(nombre_archivo, ".csv")))
  compendio[[titulo_tabla]] <<- tw
  cat("Tabla guardada:", nombre_archivo, "\n")
}

graficos <- list()
guardar_grafico <- function(g, nombre_archivo, ancho = 9, alto = 5.5) {
  print(g)
  ggsave(file.path(CARPETA, "figuras", paste0(nombre_archivo, ".png")),
         g, width = ancho, height = alto, dpi = 200, bg = "white")
  graficos[[nombre_archivo]] <<- g
  cat("Gráfico guardado:", nombre_archivo, "\n")
}

estrellas <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                                                      ifelse(p < 0.10, "*", ""))))
}

tema_tesis <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey35"),
        plot.caption = element_text(color = "grey45", hjust = 0),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

COLOR_ALTA <- "#C00000"
COLOR_BAJA <- "#1F4E79"
COLOR_GRIS <- "#808080"


# ==============================================================================
# 1. DATOS Y SALARIO MÍNIMO
# ==============================================================================
titulo("1. DATOS")

panel <- read_rds(file.path("1. DATOS", "panel_firma_eam_expalt_completo.rds")) %>%
  mutate(NORDEMP = as.character(NORDEMP), ANIO = as.integer(as.character(ANIO)))

# Salario mínimo mensual por año, según los decretos. Lo necesitamos completo
# porque el placebo rodante construye una medida distinta para cada año.
#
# IPC_PCT: variación anual del IPC del DANE a diciembre de cada año (el mismo
# año calendario, no el año anterior).
#
# DEFLACTOR -- por qué el IPC del año EN QUE EL SALARIO ESTÁ VIGENTE, no el del
# año anterior: la pregunta de esta tesis es sobre el costo laboral REAL de la
# firma. Una firma que en 2023 paga 16% más de nómina vende a precios que
# subieron 9,28% ESE MISMO año, así que ese es el IPC relevante para saber
# cuánto le costó de verdad el aumento en términos de lo que produce y vende.
# La otra convención -deflactar por la inflación causada del año PREVIO- es la
# que se usa en la negociación del salario mínimo (mide si el trabajador
# recuperó el poder adquisitivo que perdió el año anterior) y responde una
# pregunta distinta: con esa convención, 2023 da un aumento real de solo ~3%,
# no el +6,15% que usamos aquí. Las dos cuentas son legítimas para sus propias
# preguntas; para el costo laboral de la firma, la correcta es la vigente.
SALARIO_MINIMO <- tibble(
  anio  = 2012:2024,
  valor = c(566700, 589500, 616000, 644350, 689455, 737717, 781242,
            828116, 877803, 908526, 1000000, 1160000, 1300000),
  ipc_pct = c(2.44, 1.94, 3.66, 6.77, 5.75, 4.09, 3.18,
              3.80, 1.61, 5.62, 13.12, 9.28, 5.20)
) %>%
  mutate(
    aumento_nominal_pct = 100 * (valor / lag(valor) - 1),
    aumento_real_pct = 100 * ((1 + aumento_nominal_pct / 100) / (1 + ipc_pct / 100) - 1)
  )

ver(SALARIO_MINIMO)
guardar_tabla(SALARIO_MINIMO, "T00_salario_minimo_nominal_y_real",
              "Tabla 0. Salario mínimo, IPC del año vigente, aumento nominal y real", decimales = 2)

cat("\nNOTA IMPORTANTE PARA LEER EL PLACEBO: en 2019 el mínimo subió 6,0%. No\n",
    "fue un año sin choque, fue un año con un aumento normal. Por eso este\n",
    "script compara 2023 contra TODOS los años, no contra uno solo.\n")

base <- panel %>%
  mutate(
    empleo = empleo_total_sin_propietarios,
    costo_trabajador = ifelse(empleo > 0 & costos_totales_personal_total_c3r10c3 > 0,
                              costos_totales_personal_total_c3r10c3 / empleo, NA_real_),
    # Salario mensual del obrero permanente, en pesos. Es el denominador de la
    # exposición y, por tanto, el sospechoso de la reversión a la media.
    w_obrero_mensual = ifelse(obreros_permanentes > 0 &
                                sueldos_permanentes_obreros_c3r2c1 > 0,
                              sueldos_permanentes_obreros_c3r2c1 /
                                obreros_permanentes / 12 * 1000, NA_real_),
    log_costo = log(ifelse(costo_trabajador > 0, costo_trabajador, NA_real_)),
    log_empleo = log(ifelse(empleo > 0, empleo, NA_real_)),
    ANIO_F = factor(ANIO),
    sector_2022 = factor(CIIU4),
    depto_2022 = factor(DPTO),
    tamano_2022 = factor(tamano_empresa, levels = c("Pequena", "Mediana", "Grande"))
  )

cat("\nPanel:", nrow(base), "filas |", n_distinct(base$NORDEMP), "firmas |",
    "años", min(base$ANIO), "a", max(base$ANIO), "\n")
cat("Firmas-año con salario de obrero:", sum(!is.na(base$w_obrero_mensual)), "\n")


# ==============================================================================
# 2. PARTE 1 — PLACEBO RODANTE
# ==============================================================================
titulo("2. PLACEBO RODANTE: LA MISMA MEDIDA, AÑO POR AÑO")

# LA IDEA
# Para cada año t construimos la exposición exactamente como se construye la de
# la tesis, pero con el salario del obrero de t-1 y el mínimo de t. Luego
# medimos el salto del costo laboral de t-1 a t entre quintiles de esa medida.
#
# Esto produce una serie de "efectos" comparables entre sí, uno por año. 2023 es
# uno más de esa serie.
#
# CÓMO SE LEE, y conviene fijarlo ANTES de ver el resultado:
#
#   - Si 2023 sobresale claramente de los demás años, el choque existe y es
#     separable de la reversión. El placebo de un solo año (2019) era engañoso
#     por haber elegido justo un año con un diferencial alto.
#
#   - Si 2023 queda dentro del rango de los años normales, el diferencial que
#     mide la tesis no es atribuible al aumento del mínimo: es lo que produce
#     esta medida todos los años, por construcción. En ese caso hay que
#     reconstruir la exposición, y este script lo dice sin rodeos.
#
# Ojo con no leer la serie como si el nivel fuera el efecto: el nivel refleja la
# reversión mecánica, que está presente todos los años. Lo informativo es la
# DESVIACIÓN de 2023 respecto de los demás años.

anios_disponibles <- sort(unique(base$ANIO))
ANIOS_PLACEBO <- anios_disponibles[anios_disponibles >= 2014 & anios_disponibles != 2020]
# 2021 queda fuera como año de choque porque su año base (2020) es pandemia
ANIOS_PLACEBO <- ANIOS_PLACEBO[ANIOS_PLACEBO != 2021]

cat("Años en los que se estima un 'salto':", paste(ANIOS_PLACEBO, collapse = ", "), "\n")

# construir_exposicion(): la medida de la tesis, para un año base cualquiera
construir_exposicion <- function(anio_base, anio_choque) {
  minimo_choque <- SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == anio_choque]
  if (length(minimo_choque) == 0) return(NULL)
  
  base %>%
    filter(ANIO == anio_base, !is.na(w_obrero_mensual)) %>%
    transmute(
      NORDEMP,
      kaitz_bruto = minimo_choque / w_obrero_mensual
    ) %>%
    filter(is.finite(kaitz_bruto), kaitz_bruto > 0) %>%
    mutate(
      # Winsorización 1/99, igual que en la especificación principal
      kaitz = {
        lim <- quantile(kaitz_bruto, probs = c(0.01, 0.99), na.rm = TRUE)
        pmin(pmax(kaitz_bruto, lim[1]), lim[2])
      }
    ) %>%
    mutate(kaitz_de = kaitz / sd(kaitz, na.rm = TRUE))
}

# salto_del_anio(): el diferencial de crecimiento del costo laboral entre
# anio_base y anio_choque, por DE de exposición. Es un corte transversal, que
# es la forma más limpia de comparar años entre sí: evita que la ventana del
# panel cambie de año a año.
salto_del_anio <- function(anio_base, anio_choque, outcome = "log_costo") {
  
  exposicion <- construir_exposicion(anio_base, anio_choque)
  if (is.null(exposicion) || nrow(exposicion) < 500) return(NULL)
  
  valores <- base %>%
    filter(ANIO %in% c(anio_base, anio_choque)) %>%
    select(NORDEMP, ANIO, all_of(outcome),
           sector_2022, depto_2022, tamano_2022) %>%
    pivot_wider(names_from = ANIO, values_from = all_of(outcome),
                names_prefix = "y_")
  
  col_base <- paste0("y_", anio_base)
  col_choque <- paste0("y_", anio_choque)
  if (!all(c(col_base, col_choque) %in% names(valores))) return(NULL)
  
  datos <- valores %>%
    mutate(crecimiento = .data[[col_choque]] - .data[[col_base]]) %>%
    inner_join(exposicion, by = "NORDEMP") %>%
    filter(is.finite(crecimiento))
  
  if (nrow(datos) < 500) return(NULL)
  
  modelo <- tryCatch(
    feols(crecimiento ~ kaitz_de | sector_2022 + depto_2022 + tamano_2022,
          data = datos, vcov = "hetero"),
    error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  
  f <- coeftable(modelo)["kaitz_de", ]
  aumento <- SALARIO_MINIMO$aumento_nominal_pct[SALARIO_MINIMO$anio == anio_choque]
  
  tibble(
    anio_choque = anio_choque,
    anio_base = anio_base,
    aumento_minimo_pct = aumento,
    efecto_pct = 100 * f[["Estimate"]],
    ee_pct = 100 * f[["Std. Error"]],
    p_valor = f[["Pr(>|t|)"]],
    significancia = estrellas(f[["Pr(>|t|)"]]),
    firmas = nobs(modelo)
  )
}

rodante_costo <- bind_rows(lapply(ANIOS_PLACEBO, function(a) {
  anterior <- anios_disponibles[anios_disponibles < a]
  anterior <- anterior[anterior != 2020]
  if (length(anterior) == 0) return(NULL)
  salto_del_anio(max(anterior), a, "log_costo")
})) %>%
  mutate(
    es_choque = anio_choque == 2023,
    ic_inf = efecto_pct - 1.96 * ee_pct,
    ic_sup = efecto_pct + 1.96 * ee_pct
  )

ver(rodante_costo)
guardar_tabla(rodante_costo, "T01_placebo_rodante_costo",
              "Tabla 1. Diferencial del costo laboral por DE de exposición, año por año")

# La comparación que decide: 2023 contra el resto
if (nrow(rodante_costo) > 2 && any(rodante_costo$es_choque)) {
  efecto_2023 <- rodante_costo$efecto_pct[rodante_costo$es_choque]
  otros <- rodante_costo$efecto_pct[!rodante_costo$es_choque]
  
  resumen_rodante <- tibble(
    concepto = c("Diferencial de 2023 (año del choque)",
                 "Promedio de los años normales",
                 "Mínimo de los años normales",
                 "Máximo de los años normales",
                 "Exceso de 2023 sobre el promedio normal",
                 "¿2023 está por encima de TODOS los años normales?"),
    valor = c(round(efecto_2023, 3),
              round(mean(otros), 3),
              round(min(otros), 3),
              round(max(otros), 3),
              round(efecto_2023 - mean(otros), 3),
              ifelse(efecto_2023 > max(otros), 1, 0))
  )
  
  ver(resumen_rodante)
  guardar_tabla(resumen_rodante, "T02_resumen_rodante",
                "Tabla 2. El diferencial de 2023 frente a los años normales")
  
  cat("\n", strrep("-", 78), "\n", sep = "")
  cat("LECTURA DEL RESULTADO (la regla se fijó antes de correr esto):\n\n")
  if (efecto_2023 > max(otros)) {
    cat("  2023 supera a TODOS los años normales. El choque es separable de la\n",
        "  reversión mecánica. El placebo de un solo año (2019) resultó\n",
        "  engañoso porque 2019 tenía un diferencial alto dentro del rango\n",
        "  normal. El diseño de la tesis se sostiene, y esta serie completa es\n",
        "  la defensa que hay que reportar en lugar del placebo de un año.\n")
  } else if (efecto_2023 > mean(otros)) {
    cat("  2023 está por encima del promedio normal pero NO por encima de todos\n",
        "  los años. Hay señal, pero no es limpia. Hay que reportar la serie\n",
        "  completa y declarar que el diferencial de 2023 no es único. La\n",
        "  magnitud defendible es el EXCESO sobre el promedio normal, no el\n",
        "  coeficiente crudo.\n")
  } else {
    cat("  2023 NO se distingue de un año normal. El diferencial que mide la\n",
        "  tesis es lo que esta medida produce todos los años por construcción,\n",
        "  y no es atribuible al aumento del mínimo. Hay que reconstruir la\n",
        "  exposición desde la distribución salarial de la firma (ver la\n",
        "  sección 6 de este script). Esto NO se arregla con más controles.\n")
  }
  cat(strrep("-", 78), "\n", sep = "")
}

grafico_rodante <- ggplot(rodante_costo, aes(x = factor(anio_choque), y = efecto_pct)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.7) +
  scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                     labels = c("Año normal", "2023 (choque)")) +
  labs(title = "¿Se distingue 2023 de un año cualquiera?",
       subtitle = "Diferencial del costo laboral por DE de exposición, con la medida reconstruida cada año",
       x = NULL, y = "Diferencial (%)", color = NULL,
       caption = paste("Cada punto usa el salario del obrero del año anterior y el mínimo del año en curso.",
                       "\nSi 2023 no sobresale, el diferencial no es atribuible al choque.")) +
  tema_tesis
guardar_grafico(grafico_rodante, "G01_placebo_rodante")


# ==============================================================================
# 3. PARTE 2 — ¿EL DIFERENCIAL ESCALA CON EL TAMAÑO REAL DEL AUMENTO?
# ==============================================================================
titulo("3. ¿UN AUMENTO MAYOR -EN TÉRMINOS REALES- PRODUCE UN DIFERENCIAL MAYOR?")

# Esta es la prueba económica, y es más exigente que la del placebo rodante.
#
# 09_diagnostico_reversion.R hizo esta prueba contra el aumento NOMINAL. Eso
# ordenaba mal los años: 2022 (+10,07% nominal) parecía el año más fuerte del
# panel cuando en términos reales fue uno de los más débiles (-2,70%). Aquí se
# repite la prueba contra el aumento REAL (sección 1), y se reportan las dos
# correlaciones para que se vea cuánto cambia.
#
# LA REGLA DE LECTURA SE FIJA AQUÍ, ANTES DE VER EL RESULTADO (tal como se pidió):
#
#   - SI EL DIFERENCIAL ESCALA CON EL AUMENTO REAL -los años de aumento real
#     alto (2023, 2024) por encima de los intermedios (2016-2019), y estos por
#     encima de los de aumento real bajo o negativo (2015, 2021, 2022)- el
#     diseño responde al choque y el problema de 09_diagnostico_reversion.R
#     era el deflactor, no la premisa. En ese caso la tesis sigue con 2023
#     como choque, y esta serie (en términos reales) va al capítulo 4 como
#     validación.
#
#   - SI EL DIFERENCIAL NO ESCALA -los tres grupos parecidos, o el orden
#     invertido- el diferencial de ~4% que aparece todos los años no responde
#     al tamaño del choque, y el traslape aritmético entre el denominador de
#     la exposición y la base del outcome sigue siendo la explicación más
#     simple. En ese caso hay que trabajar sobre la especificación, no sobre
#     la premisa.
#
#   - CASO INTERMEDIO: si 2023 y 2024 destacan pero el orden dentro de los
#     demás años es ruidoso, es señal débil. Se reporta la serie completa y se
#     declara la limitación.
#
# Con ~8 años esto sigue siendo poca potencia estadística: no se lee un
# p-valor como prueba, se mira el patrón y la comparación de grupos de abajo,
# que tiene más potencia que la correlación punto por punto.

if (nrow(rodante_costo) >= 4) {

  escala <- rodante_costo %>%
    filter(!is.na(aumento_minimo_pct)) %>%
    select(anio_choque, aumento_minimo_pct, efecto_pct, ee_pct, es_choque) %>%
    left_join(SALARIO_MINIMO %>% select(anio, aumento_real_pct),
              by = c("anio_choque" = "anio"))

  correlacion_nominal <- cor(escala$aumento_minimo_pct, escala$efecto_pct, use = "complete.obs")
  correlacion_real    <- cor(escala$aumento_real_pct,    escala$efecto_pct, use = "complete.obs")

  cat("Correlación entre el AUMENTO NOMINAL y el diferencial medido:", round(correlacion_nominal, 3), "\n")
  cat("Correlación entre el AUMENTO REAL y el diferencial medido:   ", round(correlacion_real, 3), "\n")
  cat("Años usados:", nrow(escala), "\n")

  ver(escala)
  guardar_tabla(escala, "T03_escala_con_aumento_real",
                "Tabla 3. Diferencial medido frente al aumento nominal y real del mínimo de cada año")

  cat("\nCÓMO LEER:\n",
      " - Correlación alta y positiva contra el aumento REAL: el diferencial\n",
      "   responde al tamaño del choque real. Es evidencia a favor del\n",
      "   mecanismo, y la más fuerte que puede dar este diseño.\n",
      " - Correlación cercana a cero o negativa contra el aumento real: el\n",
      "   diferencial no depende del tamaño del choque. Con ~8 puntos esto no\n",
      "   es concluyente por sí solo -- ver la comparación de grupos abajo.\n")

  # --- Comparación de grupos ------------------------------------------------------
  # Clasificación por aumento REAL, sobre los años que la sección 2 SÍ estimó.
  #
  # NOTA: 2021 no aparece aquí aunque su aumento real (-2,01%) lo pondría en
  # el grupo "bajo o negativo". 09_diagnostico_reversion.R excluye 2021 de
  # ANIOS_PLACEBO porque su año base (2020) está distorsionado por la
  # pandemia, y esa exclusión no se tocó aquí -- no se modifica la
  # especificación del diferencial, solo cómo se ordenan y clasifican los
  # años que ya estaban en la prueba. Se deja constancia en vez de forzar a
  # 2021 dentro del grupo.
  escala <- escala %>%
    mutate(
      grupo_aumento_real = case_when(
        anio_choque %in% c(2023, 2024)             ~ "1. Alto (2023-2024)",
        anio_choque %in% c(2016, 2017, 2018, 2019)  ~ "2. Intermedio (2016-2019)",
        anio_choque %in% c(2015, 2021, 2022)        ~ "3. Bajo o negativo (2015, 2021, 2022)",
        TRUE ~ "Sin clasificar"
      )
    )

  cat("\nAños de la sección 2 dentro de cada grupo (2021 no se estima -- ver nota arriba):\n")
  print(table(escala$grupo_aumento_real, escala$anio_choque))

  comparacion_grupos <- escala %>%
    group_by(grupo_aumento_real) %>%
    summarise(
      anios = paste(sort(anio_choque), collapse = ", "),
      n_anios = n(),
      efecto_promedio_pct = mean(efecto_pct),
      efecto_min_pct = min(efecto_pct),
      efecto_max_pct = max(efecto_pct),
      .groups = "drop"
    ) %>%
    arrange(grupo_aumento_real)

  ver(comparacion_grupos)
  guardar_tabla(comparacion_grupos, "T03b_comparacion_grupos_aumento_real",
                "Tabla 3b. Diferencial promedio por grupo de aumento real del salario mínimo",
                decimales = 3)

  cat("\nCÓMO LEER LA COMPARACIÓN DE GRUPOS:\n",
      " - Si el mecanismo es real: grupo 1 (alto) > grupo 2 (intermedio) > grupo 3 (bajo/negativo).\n",
      " - Si los tres promedios son parecidos, o el orden no es ese, el diferencial no responde\n",
      "   al tamaño real del choque.\n")

  # --- Cuál lectura se activa -------------------------------------------------------
  if (nrow(comparacion_grupos) == 3) {
    v_alto  <- comparacion_grupos$efecto_promedio_pct[comparacion_grupos$grupo_aumento_real == "1. Alto (2023-2024)"]
    v_medio <- comparacion_grupos$efecto_promedio_pct[comparacion_grupos$grupo_aumento_real == "2. Intermedio (2016-2019)"]
    v_bajo  <- comparacion_grupos$efecto_promedio_pct[comparacion_grupos$grupo_aumento_real == "3. Bajo o negativo (2015, 2021, 2022)"]

    cat("\n", strrep("-", 78), "\n", sep = "")
    cat("LECTURA ACTIVADA (regla fijada antes de ver el resultado, arriba en esta sección):\n\n")
    if (length(v_alto) == 1 && length(v_medio) == 1 && length(v_bajo) == 1) {
      if (v_alto > v_medio && v_medio > v_bajo) {
        cat("  ESCALA EN EL ORDEN ESPERADO: alto (", round(v_alto, 2), "%) > intermedio (",
            round(v_medio, 2), "%) > bajo/negativo (", round(v_bajo, 2), "%).\n",
            "  El diseño responde al tamaño del choque real. El problema de\n",
            "  09_diagnostico_reversion.R era el deflactor (ordenar por nominal),\n",
            "  no la premisa del choque. Esta serie en términos reales sustituye\n",
            "  al placebo nominal como validación de capítulo 4.\n", sep = "")
      } else if (v_alto > v_bajo) {
        cat("  SEÑAL PARCIAL: el grupo alto (", round(v_alto, 2), "%) supera al bajo/negativo (",
            round(v_bajo, 2), "%), pero el orden completo no es estrictamente\n",
            "  monótono (intermedio = ", round(v_medio, 2), "%). Reportar la serie\n",
            "  completa y declarar la limitación -- es señal débil, no confirmación limpia.\n", sep = "")
      } else {
        cat("  NO ESCALA: el grupo de mayor aumento real NO tiene el diferencial más\n",
            "  alto. El diferencial de ~4% que aparece en la mayoría de los años no\n",
            "  responde al tamaño del choque, ni en nominal ni en real. El traslape\n",
            "  aritmético entre el denominador de la exposición y la base del outcome\n",
            "  sigue siendo la explicación más simple -- esto apunta a la\n",
            "  especificación, no a la premisa del choque. La premisa (que 2023 fue\n",
            "  un choque real atípico) se sostiene por separado, ver la Tabla 0; lo\n",
            "  que no se sostendría es que ESTA MEDIDA la capture.\n", sep = "")
      }
    }
    cat(strrep("-", 78), "\n", sep = "")
  }

  grafico_escala_real <- ggplot(escala, aes(x = aumento_real_pct, y = efecto_pct)) +
    geom_smooth(method = "lm", se = TRUE, color = COLOR_BAJA, fill = "grey85") +
    geom_errorbar(aes(ymin = efecto_pct - 1.96 * ee_pct,
                      ymax = efecto_pct + 1.96 * ee_pct),
                  width = 0.15, color = "grey55") +
    geom_point(aes(color = es_choque), size = 3) +
    geom_text(aes(label = anio_choque), vjust = -1.2, size = 3.2) +
    scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                       guide = "none") +
    labs(title = "¿El diferencial responde al tamaño REAL del aumento del mínimo?",
         subtitle = paste0("Cada punto es un año. Correlación con aumento real: ", round(correlacion_real, 3),
                           "  (con nominal: ", round(correlacion_nominal, 3), ")"),
         x = "Aumento REAL del salario mínimo (%, deflactado por el IPC del mismo año)",
         y = "Diferencial del costo laboral (%)",
         caption = "Sustituye a G02_escala_con_aumento.png de 09_diagnostico_reversion.R, que usaba el aumento nominal (orden equivocado de los años).") +
    tema_tesis
  guardar_grafico(grafico_escala_real, "G02_escala_con_aumento_real")
}


# ==============================================================================
# 4. PARTE 3 — ¿EL CERO DE LA MEDIDA C ES REAL O COLINEALIDAD?
# ==============================================================================
titulo("4. LA MEDIDA C: ¿CERO ECONÓMICO O CERO ABSORBIDO?")

# La medida C usa el salario promedio de las firmas PARECIDAS (misma celda
# sector x departamento x tamaño), excluyendo a la propia firma. Su virtud es
# que el error de medición de la firma desaparece del denominador, así que no
# puede producir reversión a la media.
#
# Su problema es el espejo de esa virtud: si C es casi constante dentro de cada
# celda, y el modelo controla por esa misma celda, C queda absorbida y da cero
# por construcción. Un cero así no dice nada sobre economía.
#
# Lo verificamos de dos formas: estimando con y sin los controles de celda, y
# midiendo cuánta variación de C sobrevive a esos controles (el R2 de regresar
# C contra ellos).

ANIO_BASE_C <- 2022
ANIO_CHOQUE_C <- 2023

# Construcción de la medida C: promedio de la celda sin contarse a sí misma
medida_c <- base %>%
  filter(ANIO == ANIO_BASE_C, !is.na(w_obrero_mensual)) %>%
  group_by(sector_2022, depto_2022, tamano_2022) %>%
  mutate(
    firmas_celda = n(),
    suma_celda = sum(w_obrero_mensual, na.rm = TRUE),
    w_ajenas = (suma_celda - w_obrero_mensual) / (firmas_celda - 1)
  ) %>%
  ungroup() %>%
  filter(firmas_celda >= 5, is.finite(w_ajenas), w_ajenas > 0) %>%
  transmute(
    NORDEMP, firmas_celda,
    kaitz_c_bruto = SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == ANIO_CHOQUE_C] / w_ajenas,
    kaitz_a_bruto = SALARIO_MINIMO$valor[SALARIO_MINIMO$anio == ANIO_CHOQUE_C] / w_obrero_mensual,
    sector_2022, depto_2022, tamano_2022
  ) %>%
  mutate(
    across(c(kaitz_c_bruto, kaitz_a_bruto), ~ {
      lim <- quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE)
      pmin(pmax(.x, lim[1]), lim[2])
    }, .names = "{.col}_w"),
    kaitz_c_de = kaitz_c_bruto_w / sd(kaitz_c_bruto_w, na.rm = TRUE),
    kaitz_a_de = kaitz_a_bruto_w / sd(kaitz_a_bruto_w, na.rm = TRUE)
  )

cat("Firmas con medida C:", nrow(medida_c), "\n")

# --- Cuánta variación sobrevive a los controles ---------------------------------
# Regresamos cada medida contra los controles de celda. Un R2 alto significa que
# los controles se comen casi toda la variación y el coeficiente se estima sobre
# lo poco que queda.
variacion <- bind_rows(
  {
    m <- feols(kaitz_c_de ~ 1 | sector_2022 + depto_2022 + tamano_2022,
               data = medida_c)
    tibble(medida = "C. Salario de firmas parecidas",
           r2_de_los_controles = fitstat(m, "r2", simplify = TRUE),
           de_total = sd(medida_c$kaitz_c_de, na.rm = TRUE),
           de_residual = sd(resid(m), na.rm = TRUE))
  },
  {
    m <- feols(kaitz_a_de ~ 1 | sector_2022 + depto_2022 + tamano_2022,
               data = medida_c)
    tibble(medida = "A. Salario de la firma (Bite)",
           r2_de_los_controles = fitstat(m, "r2", simplify = TRUE),
           de_total = sd(medida_c$kaitz_a_de, na.rm = TRUE),
           de_residual = sd(resid(m), na.rm = TRUE))
  }
) %>%
  mutate(pct_variacion_que_sobrevive = round(100 * de_residual / de_total, 1))

ver(variacion)
guardar_tabla(variacion, "T04_variacion_tras_controles",
              "Tabla 4. Cuánta variación de cada medida sobrevive a los controles de celda",
              decimales = 4)

cat("\nCÓMO LEER: si a la medida C le sobrevive muy poca variación (digamos por\n",
    "debajo del 30%), su cero NO es evidencia de que no hay efecto: es que casi\n",
    "no queda variación con la que estimarlo. En ese caso C no sirve ni para\n",
    "confirmar ni para descartar, y hay que decirlo así.\n")

# --- El efecto de C con y sin controles ------------------------------------------
crecimiento_23 <- base %>%
  filter(ANIO %in% c(ANIO_BASE_C, ANIO_CHOQUE_C)) %>%
  select(NORDEMP, ANIO, log_costo) %>%
  pivot_wider(names_from = ANIO, values_from = log_costo, names_prefix = "y_") %>%
  mutate(crecimiento = .data[[paste0("y_", ANIO_CHOQUE_C)]] -
           .data[[paste0("y_", ANIO_BASE_C)]]) %>%
  select(NORDEMP, crecimiento)

datos_c <- medida_c %>%
  inner_join(crecimiento_23, by = "NORDEMP") %>%
  filter(is.finite(crecimiento))

estimar_variante <- function(tratamiento, controles, etiqueta_medida, etiqueta_controles) {
  formula <- if (is.null(controles)) {
    as.formula(paste0("crecimiento ~ ", tratamiento))
  } else {
    as.formula(paste0("crecimiento ~ ", tratamiento, " | ", controles))
  }
  modelo <- tryCatch(feols(formula, data = datos_c, vcov = "hetero"),
                     error = function(e) NULL)
  if (is.null(modelo)) return(NULL)
  f <- coeftable(modelo)[tratamiento, ]
  tibble(medida = etiqueta_medida, controles = etiqueta_controles,
         efecto_pct = 100 * f[["Estimate"]],
         ee_pct = 100 * f[["Std. Error"]],
         p_valor = f[["Pr(>|t|)"]],
         significancia = estrellas(f[["Pr(>|t|)"]]),
         firmas = nobs(modelo))
}

colinealidad <- bind_rows(
  estimar_variante("kaitz_c_de", NULL, "C. Firmas parecidas", "1. Sin controles"),
  estimar_variante("kaitz_c_de", "tamano_2022", "C. Firmas parecidas", "2. Solo tamaño"),
  estimar_variante("kaitz_c_de", "sector_2022 + tamano_2022", "C. Firmas parecidas", "3. Sector y tamaño"),
  estimar_variante("kaitz_c_de", "sector_2022 + depto_2022 + tamano_2022", "C. Firmas parecidas", "4. Celda completa"),
  estimar_variante("kaitz_a_de", NULL, "A. Bite", "1. Sin controles"),
  estimar_variante("kaitz_a_de", "tamano_2022", "A. Bite", "2. Solo tamaño"),
  estimar_variante("kaitz_a_de", "sector_2022 + tamano_2022", "A. Bite", "3. Sector y tamaño"),
  estimar_variante("kaitz_a_de", "sector_2022 + depto_2022 + tamano_2022", "A. Bite", "4. Celda completa")
)

ver(colinealidad)
guardar_tabla(colinealidad, "T05_medida_c_colinealidad",
              "Tabla 5. Efecto de cada medida según cuántos controles de celda se incluyan")

cat("\nCÓMO LEER:\n",
    " - Si C da un efecto positivo SIN controles y cae a cero al agregarlos, su\n",
    "   cero era colinealidad. C no descarta el primer eslabón.\n",
    " - Si C da cero YA SIN controles, el cero es real: sin el salario propio en\n",
    "   el denominador no hay primer eslabón, y la reversión es la explicación\n",
    "   más simple de lo que mide Bite.\n")

grafico_colinealidad <- ggplot(colinealidad,
                               aes(x = controles, y = efecto_pct, color = medida, group = medida)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.8) +
  geom_pointrange(aes(ymin = efecto_pct - 1.96 * ee_pct,
                      ymax = efecto_pct + 1.96 * ee_pct),
                  position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c(`A. Bite` = COLOR_ALTA, `C. Firmas parecidas` = COLOR_BAJA)) +
  labs(title = "¿El cero de la medida C es económico o absorbido por los controles?",
       subtitle = "Efecto sobre el crecimiento del costo laboral 2022-2023, agregando controles uno a uno",
       x = NULL, y = "Efecto (%)", color = NULL,
       caption = "Si C solo cae a cero cuando entran los controles de celda, su cero no descarta nada.") +
  tema_tesis +
  theme(axis.text.x = element_text(angle = 12, hjust = 1))
guardar_grafico(grafico_colinealidad, "G03_medida_c_colinealidad")


# ==============================================================================
# 5. PARTE 4 — EL MISMO DIAGNÓSTICO SOBRE LA COMPRESIÓN SALARIAL
# ==============================================================================
titulo("5. LA COMPRESIÓN SALARIAL BAJO LA MISMA LUPA")

# POR QUÉ ESTA SECCIÓN
# La compresión salarial (brecha w_obrero / w_admin) es el hallazgo de mayor
# magnitud de la tesis: 0,22 DE, cuatro veces cualquier otro canal.
#
# Pero es también el más expuesto al problema de esta sección, por una razón
# puramente aritmética: w_obrero está en el DENOMINADOR de Bite y en el
# NUMERADOR de la brecha. Kaitz alto significa w_obrero bajo en 2022, lo que
# significa brecha baja en 2022, lo que produce un rebote en 2023 aunque no
# pase nada económico. El p_previos de 1,4e-51 es consistente con eso.
#
# Le aplicamos el mismo placebo rodante. Si el salto de la brecha en 2023 se
# parece al de los años normales, la compresión salarial no puede ser el
# titular de la tesis.

base <- base %>%
  mutate(
    w_admin_mensual = ifelse(administrativos_permanentes > 0 &
                               sueldos_permanentes_administrativos_c3r2c2 > 0,
                             sueldos_permanentes_administrativos_c3r2c2 /
                               administrativos_permanentes / 12 * 1000, NA_real_),
    log_brecha = log(ifelse(!is.na(w_obrero_mensual) & !is.na(w_admin_mensual) &
                              w_admin_mensual > 0 & w_obrero_mensual > 0,
                            w_obrero_mensual / w_admin_mensual, NA_real_))
  )

rodante_brecha <- bind_rows(lapply(ANIOS_PLACEBO, function(a) {
  anterior <- anios_disponibles[anios_disponibles < a]
  anterior <- anterior[anterior != 2020]
  if (length(anterior) == 0) return(NULL)
  salto_del_anio(max(anterior), a, "log_brecha")
})) %>%
  mutate(es_choque = anio_choque == 2023,
         ic_inf = efecto_pct - 1.96 * ee_pct,
         ic_sup = efecto_pct + 1.96 * ee_pct)

if (nrow(rodante_brecha) > 0) {
  ver(rodante_brecha)
  guardar_tabla(rodante_brecha, "T06_placebo_rodante_brecha",
                "Tabla 6. Diferencial de la brecha obrero/administrativo, año por año")
  
  if (any(rodante_brecha$es_choque) && sum(!rodante_brecha$es_choque) > 1) {
    b_2023 <- rodante_brecha$efecto_pct[rodante_brecha$es_choque]
    b_otros <- rodante_brecha$efecto_pct[!rodante_brecha$es_choque]
    cat("\nCompresión salarial en 2023:", round(b_2023, 3), "%\n")
    cat("Promedio de los años normales:", round(mean(b_otros), 3), "%\n")
    cat("Máximo de los años normales:", round(max(b_otros), 3), "%\n")
    cat("Exceso de 2023:", round(b_2023 - mean(b_otros), 3), "puntos\n")
    
    if (b_2023 > max(b_otros)) {
      cat("\n-> La compresión de 2023 supera a todos los años normales. El\n",
          "   hallazgo se sostiene y puede ser el titular de la tesis.\n")
    } else {
      cat("\n-> La compresión de 2023 NO supera a los años normales. Es lo que\n",
          "   esta medida produce siempre, por el traslape aritmético entre\n",
          "   w_obrero en el denominador de Bite y en el numerador de la\n",
          "   brecha. NO puede presentarse como hallazgo causal del choque.\n")
    }
  }
  
  grafico_brecha <- ggplot(rodante_brecha, aes(x = factor(anio_choque), y = efecto_pct)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, color = es_choque), size = 0.7) +
    scale_color_manual(values = c(`TRUE` = COLOR_ALTA, `FALSE` = COLOR_GRIS),
                       labels = c("Año normal", "2023 (choque)")) +
    labs(title = "Compresión salarial: ¿2023 se distingue de un año cualquiera?",
         subtitle = "Diferencial de la brecha obrero/administrativo por DE de exposición",
         x = NULL, y = "Diferencial (%)", color = NULL,
         caption = "w_obrero está en el denominador de la exposición y en el numerador de la brecha: el traslape es aritmético.") +
    tema_tesis
  guardar_grafico(grafico_brecha, "G04_placebo_rodante_brecha")
}


# ==============================================================================
# 6. QUÉ HACER CON CADA RESULTADO POSIBLE
# ==============================================================================
titulo("6. CÓMO SE DECIDE A PARTIR DE AQUÍ")

cat("
Este script produce tres piezas de evidencia. La decisión depende de cómo
caigan, y las opciones están escritas ANTES de verlas para que la elección no
se acomode al resultado.

CASO 1 — 2023 sobresale en el placebo rodante Y el diferencial escala con el
tamaño del aumento del mínimo.
  El diseño se sostiene. El placebo de un solo año era engañoso por haber
  elegido un año con diferencial alto. En la tesis se reporta la SERIE COMPLETA
  en lugar del placebo de 2019, y se declara que el nivel del diferencial
  incluye reversión mecánica mientras que el exceso de 2023 es lo atribuible al
  choque. La magnitud que se reporta es el exceso, no el coeficiente crudo.

CASO 2 — 2023 está por encima del promedio pero no de todos los años.
  Hay señal débil. Se reporta el exceso sobre el promedio normal como
  estimación principal, con la serie completa a la vista, y se declara en el
  capítulo de amenazas que el diferencial de 2023 no es único. La tesis pierde
  fuerza en la magnitud pero conserva la estructura.

CASO 3 — 2023 no se distingue de un año normal.
  El primer eslabón no es atribuible al choque con esta medida. Esto NO se
  arregla con más controles ni con otra ventana. Dos salidas, y las dos son
  tesis defendibles:

    3a. RECONSTRUIR LA EXPOSICIÓN desde la distribución salarial de la firma
        (proporción de trabajadores cerca del mínimo) en lugar del salario
        promedio del obrero. Esa medida no tiene el salario propio como
        denominador, así que no sufre reversión. Es lo que ya estaba previsto
        como plan alternativo desde septiembre. Requiere verificar primero si
        la EAM permite construirla.

    3b. CAMBIAR LA PREGUNTA a una de medición: documentar que las medidas de
        exposición tipo Kaitz producen un primer eslabón espurio por reversión
        a la media, mostrar el diagnóstico que lo detecta, y proponer la
        alternativa. Es una contribución real y menos frecuente de lo que
        parece: la mayoría de los trabajos con bite salarial no corren este
        placebo. Con el tiempo disponible es la salida más segura, y una
        primera etapa que falla es un resultado, no un fracaso.

  En cualquiera de las dos, lo que NO se puede hacer es reportar el 4,03% como
  efecto del salario mínimo.

NOTA SOBRE LA COMPRESIÓN SALARIAL: su diagnóstico es independiente del resto.
Puede sobrevivir aunque el costo laboral no lo haga, o al revés. Se decide con
la tabla 6, no por analogía con el costo laboral.
")

if (length(compendio) > 0) {
  save_as_docx(values = compendio, path = file.path(CARPETA, "00_compendio_tablas.docx"))
  cat("\nCompendio guardado con", length(compendio), "tablas.\n")
}

cat("\nGráficos disponibles:\n")
cat(paste(" -", names(graficos)), sep = "\n")

titulo("FIN DEL SCRIPT")

