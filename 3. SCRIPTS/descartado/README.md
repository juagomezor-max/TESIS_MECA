# Scripts descartados

Versiones superadas o consolidadas de los scripts de diagnóstico de validez
de la medida de exposición (antes `09` a `16`, hoy consolidados en
`09_validez_exposicion.R` y `10_bug_controles.R`). Se conservan aquí, con su
historial de git intacto (`git mv`, no se borró nada). **No correr**: sus
resultados ya no se citan en la tesis; la versión vigente de cada prueba está
en `09_validez_exposicion.R`.

Hay dos motivos distintos para estar en esta carpeta, y no son lo mismo:

- **Superados por error** (sección de abajo): tenían un bug de construcción
  (orden por aumento nominal en vez de real, o indexación incorrecta del
  rezago) que el siguiente script en la cadena corrigió. Sus cifras no deben
  citarse ni siquiera como referencia.
- **Consolidados, no por error** (sección siguiente): su lógica estaba
  correcta y sin bugs conocidos; se movieron aquí únicamente porque
  `09_validez_exposicion.R` reproduce sus cifras de forma verificada
  (7 cifras citadas, comprobadas una por una antes de archivar), y mantener
  dos scripts que producen el mismo resultado en carpetas de salida distintas
  es una fuente de confusión, no de robustez.

## Consolidados en 09_validez_exposicion.R (no por error)

### 11_placebo_rodante_real.R

**Qué probaba:** placebo rodante (misma medida "kaitz", año por año),
escalamiento del diferencial por el aumento REAL del mínimo, colinealidad de
la medida C, y compresión salarial bajo la misma lupa -- ya con el orden por
aumento real corregido respecto a `09_diagnostico_reversion.R`.

**Por qué se archiva:** ningún bug encontrado. Es exactamente la Sección 1 de
`09_validez_exposicion.R` (tablas `T1_00`-`T1_06`, gráficos `G1_01`-`G1_04`),
verificada cifra por cifra contra el original antes de archivar (2,89% del
placebo rodante 2023).

### 13_celda_limpia_corregida.R

**Qué probaba:** celda limpia con las cinco medidas, indexación del rezago ya
corregida, comparación por grupos de aumento real, sensibilidad al rezago
para 2023 y 2024.

**Por qué se archiva:** ningún bug encontrado -- es la versión que corrigió
`10_celda_limpia_rodante.R` y `12_celda_limpia_real.R`. Es la Sección 2 de
`09_validez_exposicion.R` (tablas `T2_00`-`T2_05c`, gráficos `G2_01`-`G2_04`),
verificada contra el 1,12%*** de Bite en celda limpia y contra el rho=0,565
(Bite 2019 vs. 2022).

### 14_outcome_alternativo.R

**Qué probaba:** si el aumento del mínimo de 2022 ensucia el outcome de 2023
(que arrastra 2022 en su año base), variando la ventana del outcome para
2023, 2024 y un contraste de falsación en 2015. Aquí se encontró el bug de
controles fijos en 2022 (documentado en `10_bug_controles.R` y en la sección
4.1 de `09_validez_exposicion.R`), pero ESTE script no tenía ningún error en
su propia lógica de ventanas.

**Por qué se archiva:** ningún bug propio. Es la Sección 3 de
`09_validez_exposicion.R` (tablas `T3_01`-`T3_04`, gráfico `G3_01`).

### 15_auditoria_adversarial.R

**Qué probaba:** auditoría adversarial de cinco afirmaciones (A1-A5),
buscando activamente dónde el diagnóstico anterior se equivoca: traslape
algebraico, atenuación vs. traslape, falsación en otros años, robustez del
"no escala", y robustez de la celda "Bite, ventana 1 año, exposición 4 años
antes".

**Por qué se archiva:** ningún bug encontrado. Es la Sección 4 de
`09_validez_exposicion.R` (tablas `T4_00`-`T4_05`), verificada contra las
cifras 8,92%*** (Bite, 2015 larga), 17,18%*** (golpe costo, 2015 larga),
1,53%*** (Bite, 2017 falso positivo) y -0,663 (correlación Bite con 2022).

## Superados por error

### 09_diagnostico_reversion.R

**Qué probaba:** si el "primer eslabón" de la tesis (el salto del costo
laboral en 2023 para las firmas más expuestas) es el choque del salario
mínimo o reversión a la media. Placebo rodante (misma medida, año por año),
escalamiento del diferencial por el tamaño del aumento del mínimo, prueba de
colinealidad de la medida C, y variación que sobrevive a los controles.

**Por qué quedó superado:** ordenaba y escalaba los años por el aumento
**nominal** del salario mínimo. Eso es un error de medición del propio
choque: 2022 (+10,07% nominal) parecía uno de los años más fuertes del panel
cuando en términos reales (con el IPC del DANE del año vigente) fue de los
más débiles (-2,70%); 2023 y 2024 son, por lejos, los de mayor aumento real.
Ordenar por nominal invertía la comparación que la prueba necesita hacer.

**Qué lo reemplaza:** `11_placebo_rodante_real.R` (misma lógica, aumento
real) y, ya consolidado, la Sección 1 de `09_validez_exposicion.R`.

### 10_celda_limpia_rodante.R

**Qué probaba:** la misma idea de "celda limpia" (exposición medida varios
años antes del año base del outcome, para cortar el traslape aritmético
entre el denominador de la exposición y la base del outcome) para las cinco
medidas de exposición, comparando 2023 contra el resto de la serie.

**Por qué quedó superado, por DOS razones:**
1. Igual que `09`, comparaba por aumento **nominal**, no real.
2. **Bug de indexación del rezago**: contaba el rezago desde el año BASE del
   outcome (`anio_base <- anio_choque - 1; anio_exp <- anio_base - REZAGO`) en
   vez de desde el año del CHOQUE. Con REZAGO=4 y el choque de 2023, eso da
   exposición de 2018, no 2019 -- un año antes de la celda limpia real de la
   tesis (`04_decision_medida.R`: exposición 2019 contra crecimiento
   2022-2023). Ver el detalle completo en el encabezado del propio script,
   sección "SUPERADO".

**Qué lo reemplaza:** `12_celda_limpia_real.R` corrigió el ordenamiento por
aumento real pero heredó el bug de indexación sin corregirlo (ver abajo);
`13_celda_limpia_corregida.R` corrigió las dos cosas. La versión vigente es
la Sección 2 de `09_validez_exposicion.R`.

### 12_celda_limpia_real.R

**Qué probaba:** lo mismo que `10_celda_limpia_rodante.R`, pero ordenando y
comparando los años por el aumento **real** del salario mínimo (corrigiendo
el problema #1 de `10`), con una comparación de tres grupos (alto /
intermedio / negativo) en vez de "2023 contra el resto", y con sensibilidad
al rezago para 2023 y 2024.

**Por qué quedó superado:** heredó de `10_celda_limpia_rodante.R` el bug de
indexación del rezago (problema #2 de arriba) SIN corregirlo -- la corrección
del ordenamiento por aumento real no tocó esa parte del código. El efecto de
Bite en la celda limpia de 2023 salía en ~0,14% (no significativo) en vez del
~0,81%-1,15% que corresponde a la celda limpia real de la tesis.

**Qué lo reemplaza:** `13_celda_limpia_corregida.R`, que corrigió la
indexación del rezago (`anio_exp <- anio_choque - REZAGO`, contado desde el
año del choque) y verificó el resultado contra el 1,15% de
`04_decision_medida.R`. La versión vigente es la Sección 2 de
`09_validez_exposicion.R`.
