# Índice de resultados citables — rama `feature/panel-establecimiento`

Índice de las cifras clave producidas en esta rama, cada una con su script, su archivo de salida versionado, y dónde está documentada en prosa. Complementa `INVENTARIO_REPO.md` (que inventaría archivos); este índice inventaría **hallazgos**. No incluye cada cifra de cada CSV — solo las que ya se citaron o son candidatas a citarse en la tesis.

| # | Tema | Hallazgo | Cifra clave | Script | Archivo (`4. RESULTADOS/Validaciones/`) | Documentado en |
|---|---|---|---|---|---|---|
| 1 | Identificador establecimiento | `NORDEST` confirmado como ID de planta, 17/17 años | — | `auditar_confiabilidad_nordest.R` | `auditoria_nordest_*.csv` | `notas_panel_establecimiento.md` §Paso 1.2 |
| 2 | Confiabilidad NORDEST | Recodificación en firmas de 1 planta | 0.004% | `auditar_confiabilidad_nordest.R` | `auditoria_nordest_recodificacion_sospechosa.csv` | `notas_panel_establecimiento.md` §Paso 1.3 |
| 3 | Confiabilidad NORDEST | Establecimientos con cambio de empresa dueña | 169/12,621 (1.34%) | `auditar_confiabilidad_nordest.R` | `auditoria_nordest_cambio_nordemp.csv` | `notas_panel_establecimiento.md` §Paso 1.3(4) |
| 4 | Recodificación multiplanta | Candidatos a recodificación (swap 1-a-1) | 4/12,621 (0.03%) | `auditar_recodificacion_multiplanta_nordest.R` | `auditoria_nordest_swaps_candidatos_recodificacion.csv` | `notas_panel_establecimiento.md` §Paso 1.5 |
| 5 | Variable de ubicación | `DPTO` confirmado, cobertura 100% en 17/17 años | — | `auditar_cobertura_dpto_establecimiento.R` | `auditoria_dpto_cobertura_por_anio.csv` | `README_confiabilidad_dpto.md` |
| 6 | Estabilidad DPTO | Establecimientos con DPTO inestable | 465/12,621 (3.68%) | `auditar_estabilidad_dpto_nordest.R` | `auditoria_dpto_estabilidad_nordest_resumen.csv` | `README_confiabilidad_dpto.md` §2 |
| 7 | Estabilidad DPTO | Inestables explicados por ambigüedad Bogotá(11)/Cundinamarca(25) | 334/465 (71.8%) | `auditar_estabilidad_dpto_nordest.R` | `auditoria_dpto_estabilidad_nordest_casos.csv` | `notas_panel_establecimiento.md` §Paso 2.3 |
| 8 | Celdas dpto×año | Ventana final (9 años): mínimo por celda | 13 establecimientos (Vichada 2024) | `auditar_celdas_departamento_anio_ventana_final.R` | `auditoria_celdas_dpto_anio_panel_final.csv` | `README_confiabilidad_dpto.md` §Paso 2.6 |
| 9 | Colinealidad sector×dpto | Concentración sectorial máxima (Casanare) | HHI=0.354 | `auditar_cruce_dpto_ciiu4.R` | `auditoria_dpto_ciiu4_concentracion.csv` | `README_confiabilidad_dpto.md` §4 |
| 10 | `Exposure2022_obreros_est` | Construida, correlación con versión firma | 0.964 | `construir_exposicion_obreros_establecimiento_eam.R` | (no versionado, ver `exposicion_obreros_establecimiento_eam.rds`) | `README_exposicion_establecimiento.md` §Construcción |
| 11 | Estructura multiplanta 2022 | `Multi_f` oficial (año base 2022) | 262 firmas (vs. 447 en 2008-2024 completo) | `descriptivos_estructura_multiplanta_2022.R` | `descriptivos_multiplanta_2022_reconciliacion.csv` | `README_exposicion_establecimiento.md` §2 |
| 12 | Peso económico multiplanta | % del empleo total 2022 que concentran | 21.44% | `descriptivos_estructura_multiplanta_2022_parte2.R` | `descriptivos_multiplanta_2022_peso_firmas_empleo.csv` | `README_exposicion_establecimiento.md` §2 |
| 13 | Variación interna exposición | Mediana del rango max-min entre plantas de una firma | 21.4pp | `descriptivos_estructura_multiplanta_2022.R` | `descriptivos_multiplanta_2022_variacion_interna.csv` | `README_exposicion_establecimiento.md` §3 |
| 14 | Peso empleo por variación | Firmas con brecha ≥15pp: % del empleo de las 262 | 75.9% (168 firmas, 64.1% en número) | `calcular_peso_empleo_variacion_interna_2022.R` | `descriptivos_peso_empleo_variacion_interna_2022.csv` | `README_exposicion_establecimiento.md` §5 |
| 15 | Panel efectivo cohorte 2022 | Firmas que mantienen ≥2 plantas los 9 años | 181/262 (69.1%) | `construir_panel_efectivo_especificacion_b_por_anio.R` | `descriptivos_panel_efectivo_especificacion_b_persistencia_262.csv` | `README_exposicion_establecimiento.md` §6 |
| 16 | Descomposición salidas cohorte | Atrición vs. pérdida de planta, 2024 | 9 atrición / 10 pérdida de planta (de 19 salidas) | `descomponer_salidas_cohorte_2022.R` | `descriptivos_descomposicion_salidas_cohorte_2024.csv` | `README_exposicion_establecimiento.md` §7 |
| 17 | **Atrición diferencial (antecedente, nivel firma)** | Brecha Q5-Q1, 2022→2023/2024 (commit `15105d6`, re-corrido y versionado) | 0.57pp (2023) / 1.70pp (2024) | `diagnostico_atricion_diferencial_exposicion_eam.R` | `atricion_por_quintil_exposicion_eam.csv` | `notas_exposicion_obreros_eam.md` §Antecedente |
| 18 | **Atrición (a) tasa por quintil con SE** | Brecha Q5-Q1 con IC 95% | 2023: [-0.78, 1.92]pp · 2024: [-0.46, 3.85]pp | `extender_diagnostico_atricion_diferencial.R` | `atricion_a_tasa_por_quintil_con_se.csv` | `notas_exposicion_obreros_eam.md` §Extensión(a) |
| 19 | **Atrición (b) especificación continua** | Coeficiente LPM con controles, signo NEGATIVO (no monotónico vs. gap Q5-Q1) | 2024: -0.00304/10pp, p=0.111 | `extender_diagnostico_atricion_diferencial.R` | `atricion_b_especificacion_continua.csv` | `notas_exposicion_obreros_eam.md` §Extensión(b) |
| 20 | **Atrición (c) placebo pre-choque** | Brecha Q5-Q1, 2017→2019 — MAYOR que el período real | 3.93pp, IC [1.81, 6.05], p=0.0003 | `extender_diagnostico_atricion_diferencial.R` | `atricion_c_placebo_2017_2018_2019.csv` | `notas_exposicion_obreros_eam.md` §Extensión(c) |
| 21 | **Atrición (c) placebo, especificación continua** | Correlación cruda desaparece con controles sector/tamaño | 2019: p=0.004 (sin) → p=0.513 (con) | `extender_diagnostico_atricion_diferencial.R` | `atricion_c_placebo_especificacion_continua.csv` | `notas_exposicion_obreros_eam.md` §Extensión(c) |
| 22 | **Atrición (d) descomposición por umbral EAM** | Salidas candidatas a umbral de cobertura (proxy PERTOTAL<10) | 35-47% según año, similar en placebo | `extender_diagnostico_atricion_diferencial.R` | `atricion_d_descomposicion_umbral.csv` | `notas_exposicion_obreros_eam.md` §Extensión(d) |

## Conclusiones metodológicas transversales (aparecen en más de un tema)

- **La identificación depende de `sector(CIIU4)×año` + `tamaño×año`, no de la exposición cruda.** Aparece de forma independiente en: las validaciones de tendencias paralelas (`README.md` de `Validaciones/`) y en la extensión del diagnóstico de atrición (fila 21 de esta tabla, placebo 2019).
- **La relación exposición-salida no es monotónica** (fila 19): el modelo principal del DiD debería probar también bins/cuantiles de exposición, no solo tratamiento continuo lineal — recomendación que aplica más allá de la atrición, a cualquier especificación continua de `Exposure2022_obreros`.
- **No hay evidencia de que el choque de 2023 induzca atrición diferencial ni pérdida de planta selectiva** (filas 16, 17, 18, 20): tanto a nivel de las 262 firmas multiplanta como a nivel de toda la muestra, con el matiz de que el IC 95% no descarta un efecto moderado (fila 18) y de que el patrón placebo pre-existe (fila 20).

## Limitaciones explícitas pendientes

- Umbral de cobertura EAM: solo se verificó la pata de empleo (PERTOTAL<10); la pata de valor de producción indexado por IPP industrial (base 2016) no se verificó (fila 22).
- Los 169 establecimientos con cambio de empresa dueña (fila 3) y los 465 con DPTO inestable (fila 6) tienen tratamientos ya aprobados pero aplicados solo parcialmente en scripts posteriores — confirmar que el panel formal (aún no construido) los incorpore.
