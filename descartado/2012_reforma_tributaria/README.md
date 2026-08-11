# Archivado: exposicion a la reforma tributaria de 2012 (Ley 1607)

## Por que se archivo

Se descarto definitivamente el choque de la reforma tributaria de 2012 del
alcance de esta tesis, que se enfoca exclusivamente en el choque de salario
minimo de 2023. Mantener ambos choques mezclados en el mismo script
(`3. SCRIPTS/descriptivo_exposicion_eam.R` combinaba las secciones de 2012 y
2023 en un mismo archivo) generaba riesgo de conflacion entre dos shocks
distintos, con mecanismos y periodos de exposicion diferentes, dentro de un
pipeline pensado para un solo choque.

Este contenido **se archiva, no se elimina**: el codigo y los resultados ya
generados quedan preservados aqui por si esta linea se retoma como analisis
independiente en el futuro.

## Fecha

2026-08-10.

## Que contiene esta carpeta

- `descriptivo_exposicion_2012_eam.R`: script autocontenido (no depende de
  `3. SCRIPTS/descriptivo_exposicion_eam.R`) que reconstruye `Exposure2012` y
  todos sus diagnosticos. Extraido integramente de las Secciones 5 y 8 (y las
  porciones de las Secciones 7, 10, 11 y 12 correspondientes a 2012) del
  script original. Se ejecuta desde la raiz del repositorio:

  ```powershell
  Rscript "descartado/2012_reforma_tributaria/descriptivo_exposicion_2012_eam.R"
  ```

- Resultados ya generados (movidos desde `4. RESULTADOS/descriptivos_exposicion/`
  y regenerables corriendo el script de arriba):
  - `histograma_exposure2012.png`
  - `boxplots_alta_baja_exposicion_2012.png`
  - `serie_2012_empleo_total.png`, `serie_2012_costo_laboral_total.png`,
    `serie_2012_productividad.png`, `serie_2012_participacion_permanentes.png`,
    `serie_2012_intensidad_laboral.png`
  - `tabla_resumen_quintiles_2012.csv`
  - `base_reducida_exposicion_2012_eam.rds`/`.csv` (base reducida con
    `Exposure2012`, `quintil_exposure2012` y `periodo_2012`)

## Que cambio en el pipeline activo

`3. SCRIPTS/descriptivo_exposicion_eam.R` ya **no** calcula `Exposure2012`,
`panel_2012`, `periodo_2012`, ni ninguna salida con sufijo `_2012`. Tambien se
retiro `tabla_resumen_quintiles_consolidada.csv` (combinaba filas de 2012 y
2023 con una columna `shock`): sin el choque de 2012, esa tabla dejo de tener
sentido; la tabla equivalente solo-2023 sigue siendo
`tabla_resumen_quintiles_2023.csv`. El resto del script (carga de datos,
`panel_built`, `Exposure2022` y sus diagnosticos) queda sin cambios.
