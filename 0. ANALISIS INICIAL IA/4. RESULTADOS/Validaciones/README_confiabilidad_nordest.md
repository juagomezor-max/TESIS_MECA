# Confiabilidad de NORDEST como panel longitudinal

Rama: `feature/panel-establecimiento`. Objetivo: evaluar si el diseño DiD de la tesis (choque de salario minimo 2023, EAM) puede correrse a nivel de establecimiento-anio (`NORDEST`-ANIO) en vez de empresa-anio (`NORDEMP`-ANIO), con el mismo rigor que se aplico a `NORDEMP` en la auditoria de reproducibilidad ya fusionada a `main` (`auditar_deduplicacion_nordemp_anio.R`, `auditar_empleo_total_vs_dane.R`).

Notas completas del proceso: `0. PREPARACION/notas_panel_establecimiento.md`.

## Identificador confirmado

`NORDEST` = "Numero de Establecimiento" (diccionario maestro de variables, EAM, 17/17 anios). Distinto de `NORDEMP` = "Numero de Empresa". Numerico, 0 valores NA/vacios en las 140,835 filas de la macrobase.

## 1. `auditoria_nordest_establecimientos_por_anio.csv`
Script: `3. SCRIPTS/auditar_confiabilidad_nordest.R`.

Establecimientos unicos por anio, 2008-2024. Pico en 2010 (9,945), declive sostenido hasta 6,583 en 2024 (**-34% desde el pico**, mismo patron de contraccion del marco muestral ya documentado para NORDEMP: -37% 2010-2024). En **los 17 anios, `filas == establecimientos`** en la macrobase (verificado con `n()` vs `n_distinct(NORDEST)` por ANIO) -- confirma que `NORDEST-ANIO` es unico en **todo el panel** 2008-2024, no solo en la ventana 2015-2019 verificada previamente.

| Anio | Establecimientos | Empresas | Var. % establecimientos |
|---|---|---|---|
| 2008 | 7,937 | 7,463 | - |
| 2010 (pico) | 9,945 | 9,468 | +8.9% |
| 2015 | 9,015 | 8,389 | -2.65% |
| 2019 | 7,631 | 6,984 | -3.54% |
| 2023 | 6,714 | 6,119 | -0.90% |
| 2024 | 6,583 | 5,973 | -1.95% |

## 2. `auditoria_nordest_recodificacion_sospechosa.csv`
Proxy de estabilidad en firmas de **un solo establecimiento** (donde el chequeo es inequivoco, sin ambiguedad de cual establecimiento es cual): de **114,073 pares anio-consecutivo**, solo **4 casos (0.004%)** muestran cambio de NORDEST sin razon aparente (mismo NORDEMP, un solo establecimiento en ambos anios, pero NORDEST distinto). Evidencia muy fuerte de que el identificador no se recodifica arbitrariamente.

## 3. `auditoria_nordest_anios_por_establecimiento.csv`
Distribucion de anios presentes por NORDEST, de 12,621 establecimientos unicos (2008-2024):

| Anios presentes | % de establecimientos |
|---|---|
| 1 (altas/bajas) | 5.48% |
| 2-16 (intermedio) | ~3-7% cada uno |
| 17 (panel completo) | 31.11% |

Distribucion pareja entre los extremos -- consistente con entradas/salidas reales del marco muestral, no con ruido de codificacion.

## 4. `auditoria_nordest_cambio_nordemp.csv`
Establecimientos asociados a **mas de un NORDEMP** distinto en el periodo (posible cambio de empresa duena): **169 de 12,621 (1.34%)**, 181 transiciones totales. Distribucion por anio del cambio pareja (5-21 casos/anio, sin picos), incluido 2024 -- consistente con M&A/reestructuracion corporativa genuina, no con un artefacto de recodificacion masiva concentrado en un anio.

**Tratamiento propuesto (pendiente de aprobacion):** mantener estos 169 establecimientos en el panel (NORDEST como efecto fijo de la unidad fisica), clusterizando errores estandar por NORDEMP vigente en cada anio; robustez opcional excluyendolos para confirmar que el resultado no cambia materialmente.

## 5. `auditoria_nordest_swaps_multiplanta.csv` / `auditoria_nordest_swaps_candidatos_recodificacion.csv`
Script: `3. SCRIPTS/auditar_recodificacion_multiplanta_nordest.R`.

Chequeo adicional para firmas **multiplanta** (donde el Paso 2 no puede detectar recodificacion sin ambiguedad). Señal indirecta: un NORDEST que desaparece de una firma en el anio *t* y OTRO NORDEST que aparece en la MISMA firma en *t+1* ("swap" 1-a-1), con mismo CIIU4 y tamaño (PERTOTAL) similar (razon min/max >= 0.5).

| Metrica | Valor |
|---|---|
| Firmas multiplanta (2008-2024) | 447 |
| Transiciones anio-consecutivo en firmas multiplanta | 6,697 |
| Swaps limpios 1-a-1 (1 desaparece + 1 aparece) | 15 |
| ... de esos, mismo CIIU4 | 6 |
| ... de esos, tamaño similar (razon >= 0.5) | 7 |
| **Candidatos a recodificacion** (mismo CIIU4 + tamaño similar) | **4 (0.06% de las transiciones, 0.03% de los establecimientos)** |

**LIMITACION CONOCIDA (documentada, no resuelta):** la macrobase EAM no tiene columnas de direccion/municipio (solo `DPTO`, geografia gruesa), asi que este chequeo es indirecto e imperfecto -- no distingue con certeza una recodificacion real de una firma que genuinamente cierra una planta y abre otra similar el mismo anio. Se documenta como cota superior de la posible recodificacion no detectada por el Paso 2, no como conteo definitivo. Dada la magnitud marginal (4 de 12,621 establecimientos), no compromete la confiabilidad general de NORDEST.

## Conclusion

`NORDEST` es un identificador tan confiable como `NORDEMP` para uso como panel longitudinal: unico en el 100% del panel (17/17 anios), con recodificacion practicamente inexistente en firmas de un establecimiento (0.004%) y marginal incluso bajo el chequeo mas permisivo en firmas multiplanta (0.03% de establecimientos). El unico punto que requiere una decision explicita es el tratamiento de los 169 establecimientos (1.34%) que cambian de empresa duena (ver punto 4), pendiente de aprobacion.
