# Validaciones

Salidas de las validaciones de identificacion (tendencias paralelas 2015-2019; atricion diferencial, pendiente) desarrolladas en la rama `feature/atricion-tendencias-paralelas`.

Todas las validaciones de esta carpeta comparten el mismo panel base: 2015-2019 (pre-choque de salario minimo de 2023), sin excluir firmas/establecimientos atipicos (a diferencia de investigaciones previas de la rama `feature/postpandemia-descartar-2012`, que si excluian un grupo especifico diagnosticado para `Exposure2022_obreros` Q4 -- esa exclusion no se reusa aqui porque no esta validada para las medidas y niveles de agregacion usados en esta carpeta).

## Dimensiones de empleo evaluadas (Y)

Definidas a partir de las columnas C4R de personal ocupado de la EAM (confirmadas estables 2008-2024 en `verificar_estabilidad_columnas_c3r_c4r.R`), excluyendo siempre propietarios/socios sin remuneracion fija (no tienen salario fijo sujeto a la norma de salario minimo):

1. **empleo_total**: obreros + administrativos + profesional-tecnico (fila `empleo_total_categorias` de `construir_conteo_personal_categoria_eam.R`).
2. **empleo_permanente**: fila C4R2 del diccionario DANE ("Personal permanente"), las 3 categorias.
3. **empleo_temporal**: C4R3 (temporal directo) + C4R4 (temporal via agencia), las 3 categorias. No incluye aprendices (C4R6): es una figura contractual distinta (contrato de aprendizaje), ni permanente ni temporal en el sentido de estas dos filas del diccionario.
4. **participacion_permanente**: 100 x empleo_permanente / empleo_total.

## Validaciones incluidas

### 1. `validacion_tendencias_paralelas_empleo_bite.csv`
Script: `3. SCRIPTS/validar_tendencias_paralelas_empleo_bite.R`.
Exposicion: **Bite2022_obreros** (indice de Kaitz: SM_2023 anualizado / salario promedio de obreros en 2022), discretizada en quintiles (misma funcion `make_quintiles` que usa `Exposure2022_obreros`).
Nivel: empresa (NORDEMP). Efectos fijos: NORDEMP (+ CIIU4*anio + DPTO*anio en la especificacion con controles).
Prueba: F conjunto de `i(quintil_bite2022_obreros, anio_lineal, ref="Q1 - Muy baja")`.

| Variable | Sin controles (F / p) | Con sector*anio + dpto*anio (F / p) |
|---|---|---|
| empleo_total | 5.81 / 1.1e-4 | 5.07 / 4.4e-4 |
| empleo_permanente | 2.50 / 0.040 | 2.14 / 0.072 |
| empleo_temporal | 4.62 / 0.001 | 4.45 / 0.0014 |
| participacion_permanente | 15.0 / 2.9e-12 | 14.6 / 6.7e-12 |

**Rechaza tendencias paralelas en 3 de 4 dimensiones, incluso con controles.** `participacion_permanente` es el caso mas problematico: los controles casi no mueven el p-valor. `empleo_permanente` es el unico que deja de ser significativo al 5% con controles (p=0.072, sigue siendo marginal). Bite2022_obreros tiene un problema de identificacion en este diseño que no esta resuelto por controles de sector/region.

### 2. `evento_tendencias_2015_2019_*.png` + `tabla_evento_tendencias_2015_2019_exposure.csv`
Script: `3. SCRIPTS/validar_tendencias_paralelas_empleo_exposure_grafico.R`.
Exposicion: **Exposure2022_obreros** (participacion de obreros en el empleo total de la firma, continua, winsorizada 1%-99%), escalada a `exposicion_10pp` (por 10 puntos porcentuales) para interpretabilidad.
Nivel: empresa (NORDEMP). Estudio de evento: `i(ANIO_F, exposicion_10pp, ref="2015") | NORDEMP + CIIU4^ANIO_F + DPTO^ANIO_F`, cluster por NORDEMP.
Prueba: F conjunto de los coeficientes 2016-2019 (todo el panel es pre-choque, asi que estos son 100% chequeos de pre-tendencia).

| Variable | F | p-valor |
|---|---|---|
| empleo_total | 0.77 | 0.546 |
| empleo_permanente | 1.03 | 0.390 |
| empleo_temporal | 1.56 | 0.183 |
| participacion_permanente | 0.31 | 0.873 |

**No rechaza tendencias paralelas en ninguna de las 4 dimensiones.** Contraste directo con Bite2022_obreros (mismo panel, mismos controles): Exposure2022_obreros es la especificacion mas defendible para el diseño DiD de esta tesis.

### 3. `evento_tendencias_establecimiento_2015_2019_*.png` + `tabla_evento_tendencias_establecimiento_2015_2019.csv`
Script: `3. SCRIPTS/validar_tendencias_paralelas_establecimiento.R`.
Exposicion: **Exposure2022_obreros_est**, recalculada a nivel de establecimiento (no reusa el artefacto a nivel empresa) -- relevante porque 336 firmas del panel tienen mas de un establecimiento (NORDEST), y agregarlas a nivel NORDEMP puede mezclar tendencias propias de una planta especifica.
Nivel: establecimiento (NORDEST). Verificado antes de construir: NORDEST-ANIO ya es unico en la macrobase 2015-2019 y 2022 (0 duplicados), no requiere deduplicar.
Estudio de evento: `i(ANIO_F, exposicion_10pp, ref="2015") | NORDEST + CIIU4^ANIO_F + DPTO^ANIO_F`, cluster por NORDEMP (no por NORDEST: varios establecimientos de una misma empresa pueden compartir shocks correlacionados).

| Variable | F | p-valor |
|---|---|---|
| empleo_total | 0.39 | 0.815 |
| empleo_permanente | 0.67 | 0.611 |
| empleo_temporal | 0.58 | 0.676 |
| participacion_permanente | 0.27 | 0.898 |

**No rechaza tendencias paralelas en ninguna dimension, con p-valores aun mas altos que a nivel de empresa.** Panel: 32,516 filas establecimiento-anio, 6,670 establecimientos, 6,137 firmas.

## Conclusion consolidada

`Exposure2022_obreros` (composicion ocupacional) pasa la validacion de tendencias paralelas de forma robusta, tanto a nivel de empresa como de establecimiento, en las 4 dimensiones de empleo. `Bite2022_obreros` (indice de Kaitz) no la pasa en 3 de 4 dimensiones y los controles de sector/departamento no resuelven el problema -- no deberia usarse como especificacion principal del DiD sin investigar antes la causa de esa divergencia (queda pendiente).

## Pendiente

- Validacion de atricion diferencial por quintil de exposicion (nombre de la rama, aun no desarrollada en esta carpeta).
- Investigar la causa de la divergencia de Bite2022_obreros antes de decidir si se descarta como robustez principal o se corrige el diseño.
