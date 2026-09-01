# Inventario del repositorio — `3. SCRIPTS/` y `4. RESULTADOS/`

Generado en la rama `feature/panel-establecimiento`, FASE 1 (solo inventario, nada borrado/movido/renombrado). Fecha de corte: 2026-08-31.

Metodologia: para cada archivo de `4. RESULTADOS/` se busco (via `grep` literal del nombre de archivo) que script de `3. SCRIPTS/` lo escribe, y si aparece citado en algun `README*.md` o en `0. PREPARACION/notas_*.md`. Para los pocos casos con nombre de archivo construido dinamicamente (`paste0(..., stub, ".png")`), se verifico manualmente contra el codigo fuente del script. Ningun archivo de `4. RESULTADOS/` quedo sin script productor identificado.

**Grupos:**
- **A** — Sustenta una cifra reportada en un README, en las notas del proyecto, o es un resultado documentado que podria ir a la tesis.
- **B** — Regenerable: lo produce un script versionado del repo en <2 min, y no esta claramente duplicado ni huerfano (infraestructura util, o detalle que respalda un resumen citado).
- **C** — Duplicado o version antigua de una cifra que ya existe (con mas detalle o mas actualizada) en otro archivo.
- **D** — Huerfano: ningun script ni README lo referencia, y no se sabe con certeza que cifra sustenta.

---

## 1. `3. SCRIPTS/` (48 archivos)

| Archivo | Tamaño | Última modificación | ¿Produce salida versionada? | Grupo | Nota |
|---|---|---|---|---|---|
| `00_ejecutar_flujo_eam.R` | 1.8 KB | 2026-08-09 14:07 | No (orquestador) | B | Corre el pipeline completo llamando a otros scripts; no genera archivos propios. |
| `00_limpiar_temporales.R` | 0.5 KB | 2026-08-09 14:07 | No (utilidad) | B | Limpieza de carpetas temporales de procesamiento. |
| `3. SCRIPTS/construir_base analitica.R` | 81.7 KB | 2026-08-27 16:46 | No detectada | **Ver nota especial** | Ruta anidada por error (aporte de `njacomev`, ver §3). No es candidato de borrado. |
| `Analisis_EAM_Markdown.Rmd` | 17.3 KB | 2026-08-27 16:46 | No (Rmd exploratorio) | B | Notebook de exploracion del compañero, fusionado tal cual. |
| `README.md` | 0.8 KB | 2026-08-09 14:07 | — (documentacion) | A | Documentacion del pipeline. |
| `_utils_proyecto.R` | 3.9 KB | 2026-08-31 20:42 | No (utilidad) | B | Funciones compartidas (`ensure_project_structure`, `load_project_packages`, etc.), fuente de casi todos los demas scripts. |
| `analisis_eam_eac.R` | 6.8 KB | 2026-08-09 14:07 | Sí, `plot_archivos_por_anio_eam.png` (nombre dinamico) | A | |
| `auditar_celdas_departamento_anio_ventana_final.R` | 6.5 KB | 2026-08-31 21:07 | Sí (3 CSV en Validaciones) | A | Paso 2.6. |
| `auditar_cobertura_dpto_establecimiento.R` | 4.5 KB | 2026-08-31 19:09 | Sí (2 CSV en Validaciones) | A | Paso 2.2. |
| `auditar_confiabilidad_nordest.R` | 7.9 KB | 2026-08-31 18:32 | Sí (4 CSV en Validaciones) | A | Paso 1.3. |
| `auditar_cruce_dpto_ciiu4.R` | 4.9 KB | 2026-08-31 20:57 | Sí (2 CSV en Validaciones) | A | Paso 2.5. |
| `auditar_deduplicacion_nordemp_anio.R` | 6.0 KB | 2026-08-11 23:36 | Solo no versionada (`1. DATOS/6. BASES_DERIVADAS/`) | A | Ya en `main`; resultados citados en historial de la auditoria de reproducibilidad. |
| `auditar_distribucion_dpto_establecimiento.R` | 5.5 KB | 2026-08-31 20:51 | Sí (2 CSV en Validaciones) | A | Paso 2.4. |
| `auditar_empleo_total_vs_dane.R` | 3.4 KB | 2026-08-11 23:36 | Solo no versionada | A | Ya en `main`; valida macrobase vs. cifras DANE. |
| `auditar_estabilidad_dpto_nordest.R` | 7.7 KB | 2026-08-31 19:21 | Sí (2 CSV en Validaciones) | A | Paso 2.3. |
| `auditar_recodificacion_multiplanta_nordest.R` | 7.2 KB | 2026-08-31 18:33 | Sí (2 CSV en Validaciones) | A | Paso 1.5. |
| `calcular_peso_empleo_variacion_interna_2022.R` | 4.1 KB | 2026-09-01 20:42 | Sí (1 CSV en Validaciones) | A | Paso 3.6. |
| `comparar_distribucion_exposure_establecimiento_vs_firma.R` | 3.7 KB | 2026-09-01 20:17 | Sí (1 CSV en Validaciones) | A | Paso 3.3. |
| `comparar_exposure_est_multiplanta_vs_monoplanta.R` | 4.2 KB | 2026-09-01 20:21 | Sí (1 CSV en Validaciones) | A | Paso 3.5. |
| `construir_bite_obreros_eam.R` | 3.4 KB | 2026-08-27 17:02 | Solo no versionada | A | Construye `Bite2022_obreros`; consumido por 4 scripts de validacion. |
| `construir_conteo_personal_categoria_eam.R` | 8.0 KB | 2026-08-11 21:20 | Solo no versionada | A | Ya en `main`; insumo de `Exposure2022_obreros`. |
| `construir_conteo_personal_categoria_establecimiento_eam.R` | 7.5 KB | 2026-08-31 22:02 | Solo no versionada | A | Paso 3.1, insumo de `Exposure2022_obreros_est`. |
| `construir_diccionario_maestro.R` | 6.7 KB | 2026-08-09 14:07 | Sí, `1. DATOS/3. DICCIONARIOS/*.csv` (fuera del alcance de este inventario) | A | |
| `construir_exposicion_obreros_eam.R` | 5.8 KB | 2026-08-11 21:20 | Solo no versionada | A | Ya en `main`; construye `Exposure2022_obreros`. |
| `construir_exposicion_obreros_establecimiento_eam.R` | 6.2 KB | 2026-08-31 22:03 | Solo no versionada | A | Paso 3.1, construye `Exposure2022_obreros_est`. |
| `construir_macro_base_eam.R` | 4.1 KB | 2026-08-09 14:07 | Sí, `1. DATOS/5. MACROBASE/*` (fuera del alcance) | A | ETL principal. |
| `construir_panel_efectivo_especificacion_b_por_anio.R` | 7.9 KB | 2026-09-01 20:47 | Sí (3 CSV en Validaciones) | A | Paso 3.7. |
| `construir_salarios_promedio_categoria_eam.R` | 6.0 KB | 2026-08-11 21:20 | Solo no versionada | A | Ya en `main`; insumo de `Bite2022_obreros`. |
| `descomponer_salidas_cohorte_2022.R` | 7.3 KB | 2026-09-01 20:51 | Sí (3 CSV en Validaciones) | A | Paso 3.8. |
| `descriptivo_exposicion_eam.R` | 15.4 KB | 2026-08-27 17:02 | Sí (varios PNG/CSV en `descriptivos_exposicion/`) | A | Script original de exposicion (`Exposure2022`), ya en `main`. |
| `descriptivos_estructura_multiplanta_2022.R` | 14.2 KB | 2026-08-31 22:14 | Sí (7 CSV en Validaciones) | A | Paso 3.2; ver §3 sobre 3 de sus 7 salidas superadas por `_parte2`. |
| `descriptivos_estructura_multiplanta_2022_parte2.R` | 6.2 KB | 2026-09-01 20:19 | Sí (3 CSV en Validaciones) | A | Paso 3.4. |
| `diagnostico_atricion_diferencial_exposicion_eam.R` | 4.7 KB | 2026-08-11 21:20 | Solo no versionada, **no citado en ningun README/nota** | **D (huérfano)** | Ver hallazgo especial en §3 — relevante para el trabajo de atricion pendiente. |
| `diagnostico_panel_nordemp_eam.R` | 7.0 KB | 2026-08-09 14:07 | Sí, `panel_diagnostico/distribucion_permanencia_empresas.png` | A | |
| `diagnostico_preliminar_tendencias_2015_2019.R` | 8.0 KB | 2026-08-27 17:02 | Sí, 4 PNG (nombre dinamico) en `descriptivos_exposicion/` | A | |
| `diagnostico_robustez_correlacion_bite_exposure_eam.R` | 8.1 KB | 2026-08-27 17:02 | Sí (3 archivos en `descriptivos_exposicion/`) | A | |
| `diagnosticos_validacion_bite_obreros_eam.R` | 7.7 KB | 2026-08-27 17:02 | Sí (5 archivos en `descriptivos_exposicion/`) | A | |
| `diagnosticos_validacion_exposicion_obreros_eam.R` | 11.6 KB | 2026-08-11 21:20 | Sí (8 archivos en `descriptivos_exposicion/`) | A | Ya en `main`. |
| `investigar_divergencia_pretendencias_2018_2019.R` | 14.1 KB | 2026-08-27 17:02 | Sí, 2 PNG en `descriptivos_exposicion/` | A | |
| `investigar_validez_test_pretendencias.R` | 11.8 KB | 2026-08-27 17:02 | Solo no versionada | A | Resultados citados en prosa en `notas_exposicion_obreros_eam.md`, aunque el CSV no esta versionado. |
| `validar_tendencias_paralelas_empleo_bite.R` | 10.1 KB | 2026-08-27 18:43 | Sí (1 CSV en Validaciones) | A | |
| `validar_tendencias_paralelas_empleo_exposure_grafico.R` | 8.8 KB | 2026-08-27 18:43 | Sí (4 PNG + 1 CSV en Validaciones) | A | |
| `validar_tendencias_paralelas_establecimiento.R` | 10.2 KB | 2026-08-27 18:43 | Sí (4 PNG + 1 CSV en Validaciones) | A | |
| `verificar_cobertura_exposicion_eam.R` | 1.9 KB | 2026-08-11 21:20 | Solo no versionada | B | |
| `verificar_consistencia_cruzada_multiplanta_2022.R` | 5.2 KB | 2026-09-01 20:29 | Sí (1 CSV en Validaciones) | A | Paso 3, verificacion de reglas de citacion. |
| `verificar_estabilidad_columnas_c3r_c4r.R` | 19.2 KB | 2026-08-11 21:20 | Solo no versionada | A | Confirma columnas C3R/C4R estables, citado extensamente en notas. |
| `verificar_exclusion_prestaciones_salario_obrero_eam.R` | 7.9 KB | 2026-08-27 17:02 | Solo no versionada | A | |
| `verificar_nombres_columnas_macrobase.R` | 6.4 KB | 2026-08-11 21:20 | Solo no versionada | B | |

---

## 2. `4. RESULTADOS/` (84 archivos, incluye subcarpetas)

### 2.1 `4. RESULTADOS/` (raiz)

| Archivo | Tamaño | Última modificación | Script productor | Citado en README/notas | Grupo |
|---|---|---|---|---|---|
| `README.md` | 0.4 KB | 2026-08-09 14:07 | — | — | A |
| `plot_archivos_por_anio_eam.png` | 33.6 KB | 2026-08-09 15:04 | `analisis_eam_eac.R` (dinamico) | No | B |

### 2.2 `4. RESULTADOS/panel_diagnostico/`

| Archivo | Tamaño | Última modificación | Script productor | Citado en README/notas | Grupo |
|---|---|---|---|---|---|
| `distribucion_permanencia_empresas.png` | 42.0 KB | 2026-08-09 15:04 | `diagnostico_panel_nordemp_eam.R` | No | B |

### 2.3 `4. RESULTADOS/descriptivos_exposicion/` (31 archivos, previos a esta rama)

| Archivo | Tamaño | Última modificación | Script productor | Citado en README/notas | Grupo |
|---|---|---|---|---|---|
| `boxplots_alta_baja_exposicion_2023.png` | 145.6 KB | 2026-08-27 17:02 | `descriptivo_exposicion_eam.R` | No | B |
| `correlacion_bite_exposure_por_sector.csv` | 0.2 KB | 2026-08-27 17:02 | `diagnostico_robustez_correlacion_bite_exposure_eam.R` | No | B |
| `exposure_obreros_por_dpto.csv` | 0.3 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |
| `exposure_obreros_por_sector_ciiu4.csv` | 1.3 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |
| `exposure_obreros_por_tamano.csv` | 0.1 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |
| `histograma_bite_obreros.png` | 45.4 KB | 2026-08-27 17:02 | `diagnosticos_validacion_bite_obreros_eam.R` | No | B |
| `histograma_exposure2022.png` | 40.2 KB | 2026-08-27 17:02 | `descriptivo_exposicion_eam.R` | No | B |
| `histograma_exposure_nueva_vs_original.png` | 68.5 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |
| `preliminar_tendencias_2015_2019_costo_laboral_total.png` | 111.3 KB | 2026-08-27 17:02 | `diagnostico_preliminar_tendencias_2015_2019.R` | No | B |
| `preliminar_tendencias_2015_2019_empleo_total.png` | 91.1 KB | 2026-08-27 17:02 | `diagnostico_preliminar_tendencias_2015_2019.R` | No | B |
| `preliminar_tendencias_2015_2019_intensidad_laboral.png` | 115.0 KB | 2026-08-27 17:02 | `diagnostico_preliminar_tendencias_2015_2019.R` | No | B |
| `preliminar_tendencias_2015_2019_produccion_ventas.png` | 106.9 KB | 2026-08-27 17:02 | `diagnostico_preliminar_tendencias_2015_2019.R` | No | B |
| `preliminar_tendencias_q4_con_sin_atipicas_empleo.png` | 60.8 KB | 2026-08-27 17:02 | `investigar_divergencia_pretendencias_2018_2019.R` | No | B |
| `preliminar_tendencias_q4_con_sin_atipicas_produccion.png` | 70.4 KB | 2026-08-27 17:02 | `investigar_divergencia_pretendencias_2018_2019.R` | No | B |
| `scatter_bite_vs_exposure.png` | 377.2 KB | 2026-08-27 17:02 | `diagnostico_robustez_correlacion_bite_exposure_eam.R` | **Sí**, `notas_exposicion_obreros_eam.md` | A |
| `serie_2023_costo_laboral_total.png` | 96.8 KB | 2026-08-27 17:02 | `descriptivo_exposicion_eam.R` (dinamico) | No | B |
| `serie_2023_empleo_total.png` | 84.3 KB | 2026-08-27 17:02 | `descriptivo_exposicion_eam.R` (dinamico) | No | B |
| `serie_2023_intensidad_laboral.png` | 89.5 KB | 2026-08-27 17:02 | `descriptivo_exposicion_eam.R` (dinamico) | No | B |
| `serie_2023_participacion_permanentes.png` | 95.2 KB | 2026-08-27 17:02 | `descriptivo_exposicion_eam.R` (dinamico) | No | B |
| `serie_2023_productividad.png` | 102.3 KB | 2026-08-27 17:02 | `descriptivo_exposicion_eam.R` (dinamico) | No | B |
| `tabla_bite_mayor_menor_1.csv` | 0.1 KB | 2026-08-27 17:02 | `diagnosticos_validacion_bite_obreros_eam.R` | No | B |
| `tabla_cobertura_categorias_ocupacionales.csv` | 0.3 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |
| `tabla_correlacion_bite_exposure_obreros.csv` | 0.06 KB | 2026-08-27 17:02 | `diagnosticos_validacion_bite_obreros_eam.R` | No | B |
| `tabla_correlacion_bite_exposure_winsorizado.csv` | 0.2 KB | 2026-08-27 17:02 | `diagnostico_robustez_correlacion_bite_exposure_eam.R` | No | B |
| `tabla_percentiles_bite_obreros.csv` | 0.2 KB | 2026-08-27 17:02 | `diagnosticos_validacion_bite_obreros_eam.R` | No | B |
| `tabla_percentiles_exposure_nueva_vs_original.csv` | 0.3 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |
| `tabla_relacion_bite_atributos.csv` | 0.2 KB | 2026-08-27 17:02 | `diagnosticos_validacion_bite_obreros_eam.R` | No | B |
| `tabla_relacion_exposure_atributos.csv` | 0.2 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |
| `tabla_salarios_promedio_por_categoria.csv` | 1.0 KB | 2026-08-11 21:20 | `diagnosticos_validacion_exposicion_obreros_eam.R` | No | B |

Nota sobre esta subcarpeta: son 31 archivos que sustentan cifras ya documentadas en prosa en `notas_exposicion_obreros_eam.md` (correlaciones, percentiles, brecha salarial por categoria, etc. — de la rama `feature/exposicion-obreros-operarios`, ya fusionada a `main`), pero **ninguno tiene un README propio en su carpeta** (a diferencia de `Validaciones/`, que si tiene README por tema). No son huerfanos (se sabe que cifra sustentan, esta en las notas), pero no siguen la convencion de documentacion mas reciente. Se marcaron B, no A, porque el archivo especifico no esta citado por nombre en ningun `.md`, solo su contenido en prosa.

### 2.4 `4. RESULTADOS/Validaciones/` (49 archivos)

| Archivo | Tamaño | Última modificación | Script productor | Citado en README/notas | Grupo |
|---|---|---|---|---|---|
| `README.md` | 5.8 KB | 2026-08-27 18:43 | — | — | A |
| `README_confiabilidad_dpto.md` | 8.3 KB | 2026-08-31 21:14 | — | — | A |
| `README_confiabilidad_nordest.md` | 5.1 KB | 2026-08-31 18:34 | — | — | A |
| `README_exposicion_establecimiento.md` | 16.2 KB | 2026-09-01 20:54 | — | — | A |
| `auditoria_celdas_casanare_vichada_todos_anios.csv` | 0.8 KB | 2026-08-31 21:08 | `auditar_celdas_departamento_anio_ventana_final.R` | Sí | A |
| `auditoria_celdas_dpto_anio_incluye_2020.csv` | 3.8 KB | 2026-08-31 21:08 | `auditar_celdas_departamento_anio_ventana_final.R` | Sí | A |
| `auditoria_celdas_dpto_anio_panel_final.csv` | 2.3 KB | 2026-08-31 21:08 | `auditar_celdas_departamento_anio_ventana_final.R` | Sí | A |
| `auditoria_dpto_cambio_sostenido_por_anio.csv` | 0.1 KB | 2026-08-31 19:27 | `auditar_estabilidad_dpto_nordest.R` | Sí | A |
| `auditoria_dpto_celdas_departamento_anio.csv` | 1.6 KB | 2026-08-31 20:52 | `auditar_distribucion_dpto_establecimiento.R` | Sí | A (ventana preliminar 2015-2019+2023, distinta de la final del Paso 2.6 — ver §3) |
| `auditoria_dpto_ciiu4_concentracion.csv` | 0.9 KB | 2026-08-31 20:57 | `auditar_cruce_dpto_ciiu4.R` | Sí | A |
| `auditoria_dpto_ciiu4_detalle.csv` | 12.3 KB | 2026-08-31 20:57 | `auditar_cruce_dpto_ciiu4.R` | No (el detalle completo no se cita fila por fila) | B |
| `auditoria_dpto_cobertura_por_anio.csv` | 0.4 KB | 2026-08-31 19:09 | `auditar_cobertura_dpto_establecimiento.R` | Sí | A |
| `auditoria_dpto_codigos_por_anio.csv` | 1.2 KB | 2026-08-31 19:09 | `auditar_cobertura_dpto_establecimiento.R` | Sí | A |
| `auditoria_dpto_distribucion_establecimientos.csv` | 0.4 KB | 2026-08-31 20:52 | `auditar_distribucion_dpto_establecimiento.R` | Sí | A |
| `auditoria_dpto_estabilidad_nordest_casos.csv` | 80.1 KB | 2026-08-31 19:27 | `auditar_estabilidad_dpto_nordest.R` | Sí (agregados citados; detalle no fila por fila) | B |
| `auditoria_dpto_estabilidad_nordest_resumen.csv` | 0.1 KB | 2026-08-31 19:27 | `auditar_estabilidad_dpto_nordest.R` | Sí | A |
| `auditoria_nordest_anios_por_establecimiento.csv` | 0.2 KB | 2026-08-31 18:33 | `auditar_confiabilidad_nordest.R` | Sí | A |
| `auditoria_nordest_cambio_nordemp.csv` | 43.6 KB | 2026-08-31 18:33 | `auditar_confiabilidad_nordest.R` | Sí (agregados; detalle no fila por fila) | B |
| `auditoria_nordest_establecimientos_por_anio.csv` | 0.5 KB | 2026-08-31 18:33 | `auditar_confiabilidad_nordest.R` | Sí | A |
| `auditoria_nordest_recodificacion_sospechosa.csv` | 0.2 KB | 2026-08-31 18:33 | `auditar_confiabilidad_nordest.R` | Sí | A |
| `auditoria_nordest_swaps_candidatos_recodificacion.csv` | 0.5 KB | 2026-08-31 18:33 | `auditar_recodificacion_multiplanta_nordest.R` | Sí | A |
| `auditoria_nordest_swaps_multiplanta.csv` | 1.4 KB | 2026-08-31 18:33 | `auditar_recodificacion_multiplanta_nordest.R` | Sí (agregados) | B |
| `descriptivos_comparacion_exposure_establecimiento_vs_firma_2022.csv` | 0.3 KB | 2026-09-01 20:17 | `comparar_distribucion_exposure_establecimiento_vs_firma.R` | Sí | A |
| `descriptivos_comparacion_exposure_multiplanta_vs_monoplanta_2022.csv` | 0.2 KB | 2026-09-01 20:21 | `comparar_exposure_est_multiplanta_vs_monoplanta.R` | Sí | A |
| `descriptivos_comparacion_exposure_salen_vs_mantienen_2023_2024.csv` | 0.4 KB | 2026-09-01 20:51 | `descomponer_salidas_cohorte_2022.R` | Sí | A |
| `descriptivos_descomposicion_salidas_cohorte_2023.csv` | 0.15 KB | 2026-09-01 20:51 | `descomponer_salidas_cohorte_2022.R` | Sí | A |
| `descriptivos_descomposicion_salidas_cohorte_2024.csv` | 0.16 KB | 2026-09-01 20:51 | `descomponer_salidas_cohorte_2022.R` | Sí | A |
| `descriptivos_multiplanta_2022_distribucion_n_departamentos.csv` | 0.08 KB | 2026-08-31 22:15 | `descriptivos_estructura_multiplanta_2022.R` | No | **C** — superado por `..._frecuencia_n_departamentos.csv` (ver §3) |
| `descriptivos_multiplanta_2022_distribucion_n_establecimientos.csv` | 0.06 KB | 2026-08-31 22:15 | `descriptivos_estructura_multiplanta_2022.R` | No | **C** — superado por `..._frecuencia_n_establecimientos_todas_firmas.csv` (ver §3) |
| `descriptivos_multiplanta_2022_frecuencia_n_departamentos.csv` | 0.16 KB | 2026-09-01 20:20 | `descriptivos_estructura_multiplanta_2022_parte2.R` | Sí | A |
| `descriptivos_multiplanta_2022_frecuencia_n_establecimientos_todas_firmas.csv` | 0.08 KB | 2026-09-01 20:20 | `descriptivos_estructura_multiplanta_2022_parte2.R` | Sí | A |
| `descriptivos_multiplanta_2022_peso_firmas_empleo.csv` | 0.3 KB | 2026-09-01 20:20 | `descriptivos_estructura_multiplanta_2022_parte2.R` | Sí | A |
| `descriptivos_multiplanta_2022_reconciliacion.csv` | 0.8 KB | 2026-08-31 22:15 | `descriptivos_estructura_multiplanta_2022.R` | Sí | A |
| `descriptivos_multiplanta_2022_resumen.csv` | 0.2 KB | 2026-08-31 22:15 | `descriptivos_estructura_multiplanta_2022.R` | No | **C** — todos sus campos superados por `reconciliacion.csv` + `frecuencia_n_departamentos.csv` + `umbral_variacion.csv` (ver §3) |
| `descriptivos_multiplanta_2022_umbral_variacion.csv` | 0.12 KB | 2026-08-31 22:15 | `descriptivos_estructura_multiplanta_2022.R` | Sí | A |
| `descriptivos_multiplanta_2022_variacion_interna.csv` | 14.7 KB | 2026-08-31 22:15 | `descriptivos_estructura_multiplanta_2022.R` | Sí (agregados; ademas es insumo directo de `calcular_peso_empleo_variacion_interna_2022.R`) | A |
| `descriptivos_panel_efectivo_especificacion_b_cohorte_2022_por_anio.csv` | 0.3 KB | 2026-09-01 20:47 | `construir_panel_efectivo_especificacion_b_por_anio.R` | Sí | A |
| `descriptivos_panel_efectivo_especificacion_b_persistencia_262.csv` | 0.1 KB | 2026-09-01 20:47 | `construir_panel_efectivo_especificacion_b_por_anio.R` | Sí | A |
| `descriptivos_panel_efectivo_especificacion_b_por_anio.csv` | 0.2 KB | 2026-09-01 20:47 | `construir_panel_efectivo_especificacion_b_por_anio.R` | Sí | A (referencia amplia, explicitamente marcada como "NO es la poblacion relevante", pero se mantiene como contraste documentado) |
| `descriptivos_peso_empleo_variacion_interna_2022.csv` | 0.5 KB | 2026-09-01 20:42 | `calcular_peso_empleo_variacion_interna_2022.R` | Sí | A |
| `evento_tendencias_2015_2019_empleo_permanente.png` | 12.5 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_empleo_exposure_grafico.R` | No (script si citado, PNG no por nombre) | B |
| `evento_tendencias_2015_2019_empleo_temporal.png` | 12.6 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_empleo_exposure_grafico.R` | No | B |
| `evento_tendencias_2015_2019_empleo_total.png` | 12.5 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_empleo_exposure_grafico.R` | No | B |
| `evento_tendencias_2015_2019_participacion_permanente.png` | 12.7 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_empleo_exposure_grafico.R` | No | B |
| `evento_tendencias_establecimiento_2015_2019_empleo_permanente.png` | 10.9 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_establecimiento.R` | No | B |
| `evento_tendencias_establecimiento_2015_2019_empleo_temporal.png` | 11.3 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_establecimiento.R` | No | B |
| `evento_tendencias_establecimiento_2015_2019_empleo_total.png` | 11.1 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_establecimiento.R` | No | B |
| `evento_tendencias_establecimiento_2015_2019_participacion_permanente.png` | 11.6 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_establecimiento.R` | No | B |
| `tabla_evento_tendencias_2015_2019_exposure.csv` | 0.2 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_empleo_exposure_grafico.R` | Sí | A |
| `tabla_evento_tendencias_establecimiento_2015_2019.csv` | 0.3 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_establecimiento.R` | Sí | A |
| `validacion_tendencias_paralelas_empleo_bite.csv` | 0.7 KB | 2026-08-27 18:43 | `validar_tendencias_paralelas_empleo_bite.R` | Sí | A |
| `verificacion_consistencia_cruzada_multiplanta_2022.csv` | 0.6 KB | 2026-09-01 20:30 | `verificar_consistencia_cruzada_multiplanta_2022.R` | Sí | A |

---

## 3. Hallazgos especiales

### 3.1 Scripts que no producen ninguna salida versionada

Todos escriben (por diseño del proyecto) a `1. DATOS/6. BASES_DERIVADAS/` (gitignored) — **esto es intencional, no un defecto**: son pasos intermedios del pipeline (construccion de bases derivadas) o auditorias cuyos resultados agregados se citan en prosa dentro de las notas, no como archivo:

- `auditar_deduplicacion_nordemp_anio.R`, `auditar_empleo_total_vs_dane.R` (ya en `main`)
- `construir_bite_obreros_eam.R`, `construir_conteo_personal_categoria_eam.R`, `construir_conteo_personal_categoria_establecimiento_eam.R`, `construir_exposicion_obreros_eam.R`, `construir_exposicion_obreros_establecimiento_eam.R`, `construir_salarios_promedio_categoria_eam.R`
- `investigar_validez_test_pretendencias.R`
- `verificar_cobertura_exposicion_eam.R`, `verificar_estabilidad_columnas_c3r_c4r.R`, `verificar_exclusion_prestaciones_salario_obrero_eam.R`, `verificar_nombres_columnas_macrobase.R`
- `00_ejecutar_flujo_eam.R`, `00_limpiar_temporales.R`, `_utils_proyecto.R` (infraestructura, no generan datos)

**Caso aparte, con bandera:** `diagnostico_atricion_diferencial_exposicion_eam.R` tambien cae en este grupo (solo escribe a `1. DATOS/6. BASES_DERIVADAS/.../atricion_por_quintil_exposicion_eam.csv`, no versionado), **pero a diferencia de los demas, no encontre ninguna cita de sus resultados en ningun README ni en las notas del proyecto.** Es del 2026-08-11 (anterior a toda la rama `feature/atricion-tendencias-paralelas` y `feature/panel-establecimiento`). Esto es relevante porque el trabajo de atricion diferencial se ha mencionado varias veces como pendiente — este script ya existe y aparentemente nunca se corrio hasta el final o nunca se documento su resultado. Vale la pena revisarlo antes de construir el analisis de atricion desde cero.

### 3.2 Salidas cuyo script generador ya no existe

**Ninguna.** Los 84 archivos de `4. RESULTADOS/` tienen un script productor identificado y presente en el repositorio (confirmado via `grep` del nombre de archivo, incluidos los 6 casos de nombre dinamico verificados manualmente).

### 3.3 Pares/grupos con la misma cifra bajo nombres distintos (candidatos a Grupo C)

Verificados leyendo el contenido real de cada archivo, no solo el nombre:

| Archivo mas nuevo/detallado (mantener) | Archivo mas viejo/agregado (candidato a C) | ¿Los valores coinciden? |
|---|---|---|
| `descriptivos_multiplanta_2022_frecuencia_n_departamentos.csv` (12 filas, detalle 1-16 departamentos) | `descriptivos_multiplanta_2022_distribucion_n_departamentos.csv` (2 filas: "1"/"2+") | **Sí, coinciden exactamente**: 81 en ambos para "1 departamento"; 181 = suma de las filas 2+ del archivo detallado (112+25+15+12+5+6+1+2+1+1+1=181). |
| `descriptivos_multiplanta_2022_frecuencia_n_establecimientos_todas_firmas.csv` (5 filas, poblacion=6,186 firmas totales) | `descriptivos_multiplanta_2022_distribucion_n_establecimientos.csv` (3 filas, poblacion=262 firmas multiplanta) | **Los conteos absolutos coinciden** (155, 45, y 62=21+41), pero los porcentajes usan denominadores DISTINTOS (262 vs 6,186) — no son intercambiables sin cuidado, aunque el archivo detallado contiene toda la informacion del viejo (mas la fila de monoplanta que el viejo no tenia). |
| `descriptivos_multiplanta_2022_reconciliacion.csv` + `descriptivos_multiplanta_2022_frecuencia_n_departamentos.csv` + `descriptivos_multiplanta_2022_umbral_variacion.csv` (juntos) | `descriptivos_multiplanta_2022_resumen.csv` (6 campos) | **Sí, cada uno de los 6 campos de `resumen.csv` coincide exactamente** con un valor ya presente en alguno de los 3 archivos mas nuevos: `n_multiplanta_2022`=262 y `n_multiplanta_paso1_5_2008_2024`=447 (en `reconciliacion.csv`), `n_multiplanta_con_2plus_departamentos`=181 (suma de `frecuencia_n_departamentos.csv`), `n_multiplanta_con_exposicion_valida_2plus_est`=260 (en `reconciliacion.csv`), `n_supera_umbral_15pp`=168 y `n_supera_umbral_20pp`=137 (en `umbral_variacion.csv`). |

No se encontraron otros pares con la misma cifra bajo nombres distintos entre los 84 archivos revisados. Casos que PARECEN similares pero NO son duplicados (se revisaron y son legitimamente distintos):

- `auditoria_dpto_celdas_departamento_anio.csv` (Paso 2.4, ventana preliminar 2015-2019+2023) vs. `auditoria_celdas_dpto_anio_panel_final.csv` (Paso 2.6, ventana final de 9 años) — **ventanas temporales distintas por diseño**, ambas citadas y comparadas explicitamente en `README_confiabilidad_dpto.md`. No es duplicado.
- `tabla_correlacion_bite_exposure_obreros.csv` (sin winsorizar) vs. `tabla_correlacion_bite_exposure_winsorizado.csv` — **version winsorizada vs. no winsorizada, ambas reportadas a proposito** como chequeo de robustez (`diagnostico_robustez_correlacion_bite_exposure_eam.R`). No es duplicado.
- `exposure_obreros_por_dpto.csv` / `_por_sector_ciiu4.csv` / `_por_tamano.csv` — tres desagregaciones distintas de la misma variable, no la misma cifra.

### 3.4 Anomalía estructural (no es C ni D, requiere decisión aparte)

`3. SCRIPTS/3. SCRIPTS/construir_base analitica.R` — quedo en una ruta anidada por error al fusionar el aporte del compañero (`njacomev`), y ademas tiene un `setwd()` absoluto a su maquina personal en la primera linea. Por decision explicita tuya en su momento, se fusiono tal cual sin corregir. No es un duplicado ni un huerfano — es trabajo sustantivo (exposicion obrera propia + estudios de evento) que nadie mas replico. No debe borrarse; si se reubica, debe ser una decision separada y explicita (posiblemente coordinada con el compañero), no parte de la limpieza C/D.

---

## 4. Resumen para la decisión de limpieza

- **Grupo C confirmado (mismo valor, ya disponible en otro archivo mas nuevo):** 3 archivos, todos en `4. RESULTADOS/Validaciones/`, todos de `descriptivos_estructura_multiplanta_2022.R`:
  - `descriptivos_multiplanta_2022_distribucion_n_departamentos.csv`
  - `descriptivos_multiplanta_2022_distribucion_n_establecimientos.csv`
  - `descriptivos_multiplanta_2022_resumen.csv`
- **Grupo D confirmado (huérfano, sin cita en ningún README/nota):** ningún archivo de `4. RESULTADOS/` cayó en D (todos los outputs versionados sustentan algo identificable). El único huérfano real encontrado es el **script** `diagnostico_atricion_diferencial_exposicion_eam.R` (no produce archivo versionado que evaluar, pero su existencia sin documentar amerita revisión antes de continuar con el trabajo de atrición).
- **Grupo B (regenerable, no citado por nombre pero no huérfano):** la mayoría de `descriptivos_exposicion/` (31 archivos, predatan la convención de README por carpeta) y varios PNG/detalles de `Validaciones/` cuyos agregados sí están citados.

Nada se borró, movió ni renombró en esta fase.
