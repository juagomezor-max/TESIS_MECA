# Reglas de trabajo en TESIS_MECA

## Datos
- "1. DATOS/" no está en git; lo que se pierde ahí solo se recupera
  reconstruyendo desde la macrobase. NUNCA escribas, borres ni muevas nada
  en "1. DATOS/" sin permiso explícito en el chat. Antes de cualquier
  cambio autorizado, respalda los archivos afectados.
- Si un script escribe en "1. DATOS/" (revisa sus write_rds/saveRDS antes
  de correrlo), detente y pregunta.

## Ejecución
- Corre R siempre desde la raíz con Rscript. Excepción: los scripts de
  "0. ANALISIS INICIAL IA/3. SCRIPTS/exposicion_alternativa/" se corren con
  Rscript -e "setwd('0. ANALISIS INICIAL IA'); source('<ruta>', encoding = 'UTF-8')".
- Nunca corras: xx_script_base_historico.R, nada dentro de "descartado/",
  prueba_sintetica_17.R ni ningún script que genere datos sintéticos.
- Guarda la salida de cada corrida en "2. PROCESAMIENTO/_tmp_log_<script>.txt".
  Los logs no se comitean.
- No instales paquetes desde CRAN ni corras renv::snapshot()/restore() sin
  permiso; si falta algo, dilo.

## Verificación
- Después de correr un script, compara numéricamente sus CSV contra HEAD
  con "3. SCRIPTS/herramientas/comparar_con_head.R" (tolerancia 1e-8;
  ignora .docx y .png). Una diferencia es un hallazgo: detente y
  repórtala; no la "arregles".
- Nunca modifiques la lógica econométrica de un script sin mostrarme el
  diff y esperar aprobación.

## Git
- Un commit por script, que incluya solo las salidas de ese script.
- Nunca uses git add -A ni git add . ; agrega rutas explícitas.
- No hagas push, merge, reset, rebase ni checkout/restore de archivos sin
  permiso.
- Scripts con regla de decisión prefijada (como 17_variable_instrumental.R)
  se comitean ANTES de correrlos.

## Reportes
- Tablas cortas. No interpretes resultados más allá de lo que se pide.
