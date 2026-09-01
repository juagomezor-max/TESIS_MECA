# Nota de preanálisis — Estimación preliminar del efecto del salario mínimo 2023

**Rama:** `feature/estimacion-preliminar`
**Commit y versión de base:** `0fbdcfd` (merge de `feature/panel-establecimiento` a `main`), etiquetado como **`panel-establecimiento-v1`**. Toda decisión metodológica citada en esta nota está documentada y comiteada en ese punto del historial — ver `INDICE_RESULTADOS.md` e `INVENTARIO_REPO.md` para el detalle completo con script y archivo fuente de cada cifra.
**Fecha:** 2026-09-01 (consolidación; ver fechas individuales de cada validación en las notas citadas).
**Estado:** comiteada ANTES de correr ninguna estimación de empleo. Ningún resultado de empleo aparece en este documento ni en la rama a la fecha de este commit.

---

## 1. Pregunta de investigación

Efecto del aumento real del salario mínimo de 2023 en Colombia sobre firmas/establecimientos manufactureros, en función de su exposición pre-choque (participación de obreros y operarios en el empleo total, año base 2022).

## 2. Decisiones de diseño ya validadas (no se re-abren aquí)

| Decisión | Elección | Evidencia / fuente |
|---|---|---|
| Medida de exposición principal | `Exposure2022_obreros` (composición ocupacional) | Pasa tendencias paralelas de forma robusta en 4 dimensiones de empleo, a nivel empresa Y establecimiento. `Bite2022_obreros` (Kaitz) rechazada como especificación principal — falla en 3/4 dimensiones incluso con controles. `4. RESULTADOS/Validaciones/README.md` |
| Controles obligatorios | `sector(CIIU4)×año` + `tamaño×año` | Sin estos controles, `Bite2022_obreros` y en menor medida `Exposure2022_obreros` muestran divergencias de tendencia. El mismo patrón aparece independientemente en el diagnóstico de atrición (la correlación cruda exposición-salida desaparece con estos controles). `INDICE_RESULTADOS.md` |
| Ventana del panel final | Pre: 2015-2019 + 2021-2022. Post: 2023-2024. **2020 excluido** (disrupción pandemia). 9 años totales. | `notas_panel_establecimiento.md` §Paso 2.6, consolidando `notas_exposicion_obreros_eam.md` |
| Identificador de establecimiento | `NORDEST`, tan confiable como `NORDEMP` (único en 17/17 años, recodificación 0.004-0.03%) | `README_confiabilidad_nordest.md` |
| Variable de ubicación | `DPTO`, fijada a un único valor por establecimiento: año 2022 para el patrón `cambio_sostenido`, modal para `salto_aislado`/`patron_irregular` (regla diferenciada aprobada) | `README_confiabilidad_dpto.md` |
| Especificación "dentro de firma" (B) | Muestra PRINCIPAL = cohorte BALANCEADA de 181 firmas con ≥2 plantas en los 9 años de la ventana. Cohorte completa (262) solo como robustez — evita confundir composición con tendencia. | `README_exposicion_establecimiento.md` §6-7 |
| Forma funcional de la exposición | Probar **ambas**: continua Y por bins/cuantiles. La relación exposición-salida no es monotónica (gap Q5-Q1 positivo, coeficiente continuo negativo) — no asumir que una especificación lineal continua es suficiente. | `notas_exposicion_obreros_eam.md` §Extensión(b) |
| Atrición diferencial | No es una amenaza de selección para el DiD (patrón placebo pre-choque igual o mayor que el post-choque), pero la exposición cruda SÍ correlaciona con dinámicas de salida preexistentes explicadas por sector/tamaño — refuerza la necesidad de los controles de la fila 2. | `notas_exposicion_obreros_eam.md` §Extensión(c) |

## 3. Decisiones metodológicas que siguen abiertas

- Tratamiento de `costo_laboral_total` (outcome descriptivo vs. control por crecimiento acumulado del salario mínimo nominal en el pre-período) — pendiente desde `notas_exposicion_obreros_eam.md`.
- Los 169 establecimientos con cambio de empresa dueña y los 465 con DPTO inestable tienen tratamiento aprobado pero debe confirmarse su aplicación efectiva al construir el panel formal (ver `INDICE_RESULTADOS.md`, sección de limitaciones).
- Umbral de cobertura EAM: solo verificada la pata de empleo (PERTOTAL<10) para el diagnóstico de atrición; la pata de producción (indexada por IPP industrial desde 2016) no se verificó.
- Robustez Casanare/Vichada (baja N + alta concentración sectorial en `departamento×año`): criterio preparado (exclusión vs. agrupación "Otros"), ejecución pendiente.

## 4. Alcance de esta rama a la fecha de este commit

Esta nota se comitea **antes** de construir el panel formal o correr cualquier regresión de empleo. Los pasos siguientes (construcción del panel formal con las decisiones de la sección 2, y validación de pre-tendencias sobre ese panel ya definitivo) se documentarán en commits posteriores de esta misma rama, cada uno verificado antes de avanzar al siguiente.
