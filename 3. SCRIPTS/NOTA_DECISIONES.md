# Nota de decisiones

Registro de decisiones de alcance y organización del pipeline, en orden
cronológico. No reemplaza a `CRONOLOGIA_DECISIONES.md` (decisiones
metodológicas específicas) ni a `NARRATIVA.md` (mapa del pipeline).

## 2026-09-27

Se archivan en `descartado/` los scripts `06`, `07` y `09` a `14` y sus
resultados. Se conservan en el flujo principal `01` a `05` y `08`.

`08_tratamiento_continuo.R` pasa a ser `06_tratamiento_continuo.R`, como
robustez adicional del flujo principal.

### Regla de elección de la medida de exposición

**Regla original** (commit `fe75b9a`, 2026-09-17, "Decisión: elección de la
medida de exposición"), cuatro criterios en orden:
1. Pasa el primer eslabón si el salto salarial de 2023 frente al cambio
   típico 2015-2018 es positivo y significativo al 5%.
2. Entre las que pasan, la principal es la de mayor M de quiebre del cambio
   salarial 2022 -> 2023.
3. El empleo no se usa para elegir; se reporta con las tres medidas.
4. Si ninguna pasa, se declara un problema de diseño y se evalúa construir
   la exposición desde la distribución salarial de la firma.

**Corrección de 2026-09** (`03_primer_eslabon_medidas.R`, líneas 927-966): se
elimina el criterio del placebo 2018-2019 (sección 7) porque da negativo y
significativo en las cuatro medidas basadas en salario -- refleja la
tendencia previa de esas firmas y no distingue entre medidas -- y pasa al
capítulo 6 como limitación reconocida. En su lugar, la regla vigente usa tres
criterios, en orden:
1. Coeficiente positivo y significativo en la especificación estándar,
   muestra común (sección 5).
2. Que se sostenga sin el traslape aritmético en la celda limpia, con
   exposición medida en 2019 (sección 6 de `03_primer_eslabon_medidas.R` y
   `04_decision_medida.R`) -- este criterio pesa más que la magnitud.
3. Cobertura de muestra y estabilidad de la medida entre años base
   (`04_decision_medida.R`).

Si ninguna medida pasa los criterios 1 y 2, sigue siendo un problema de
diseño, no de robustez. Si dos medidas pasan, se elige la más simple de
explicar en una defensa y se reporta la otra como robustez.

**Resultado** (`04_decision_medida.R`): gana el Kaitz de obreros (+0,82%,
p = 0,023 en la celda limpia).
