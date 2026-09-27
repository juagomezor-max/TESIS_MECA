# Scripts descartados

Versiones superadas de los scripts de diagnóstico de validez de la medida de
exposición (antes `09` a `16`, hoy consolidados en `09_validez_exposicion.R` y
`10_bug_controles.R`). Se conservan aquí, con su historial de git intacto
(`git mv`, no se borró nada), porque cada uno documenta un paso real del
diagnóstico -- un error encontrado y corregido en el siguiente script -- y ese
rastro es parte de la auditoría de la cadena. **No correr**: sus resultados ya
no se citan en la tesis; la versión vigente de cada prueba está en
`09_validez_exposicion.R`.

## 09_diagnostico_reversion.R

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

## 10_celda_limpia_rodante.R

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

## 12_celda_limpia_real.R

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
