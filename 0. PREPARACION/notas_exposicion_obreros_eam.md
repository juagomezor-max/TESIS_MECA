# Notas: Exposure2022_obreros (medida de exposicion basada en obreros y operarios)

Rama: `feature/exposicion-obreros-operarios`. Scripts relacionados en `3. SCRIPTS/`:

1. `verificar_cobertura_exposicion_eam.R`
2. `verificar_estabilidad_columnas_c3r_c4r.R`
3. `verificar_nombres_columnas_macrobase.R`
4. `construir_conteo_personal_categoria_eam.R`
5. `construir_salarios_promedio_categoria_eam.R`
6. `construir_exposicion_obreros_eam.R`
7. `diagnosticos_validacion_exposicion_obreros_eam.R`

## Anios cubiertos

La macrobase EAM cubre **2008-2024** (no 2016-2025 como se asumia al inicio, y **no incluye 2025**). 17 anios, 140,835 filas crudas / 130,859 NORDEMP-ANIO tras consolidar duplicados.

## Codigos que cambiaron de significado o desaparecieron

- **C3R1** (Salario Integral personal permanente): discontinuado, solo 2008-2019 (12 de 17 anios). No afecta esta medida (usa C3R2, C4R\*, no C3R1).
- **C4R (personal ocupado), categoria Profesional-tecnico**: NO tiene fila propia por tipo de vinculacion como Obreros/Administrativos. Vive en las filas 1 (sufijo `N`=Nacional) y 2 (sufijo `E`=Extranjero), donde el **numero de columna** (no la fila) codifica el tipo de vinculacion. Confirmado contra el diccionario oficial DANE y validado empiricamente: los totales oficiales de C4R igualan la suma de sus componentes al 100% en **cada uno de los 17 anios individualmente**, incluidos los 6 sin labels de texto en el DTA (2019-2024).
- **CIIU3 vs CIIU4** (sector): CIIU3 solo tiene datos 2008-2011; desde 2012 el sector vive en CIIU4 (y en 2013 especificamente como `CIIU_4`, con guion bajo). Transicion limpia en 2012, sin anio de coexistencia. **No son directamente comparables** (revisiones distintas de la clasificacion CIIU, no un renombramiento simple). El escaneo de las 398 columnas de la macrobase no encontro ningun otro caso de renombramiento oculto.
- Ninguna de las 96 columnas C3R/C4R usadas para construir `total_obreros`, `total_prof_tecnico`, `total_administrativos` o `total_propietarios` tiene huecos: cobertura 100% en los 17 anios.

## Exposure2022_obreros (nueva) vs Exposure2022 (original)

- **Correlacion: 0.36** (n=6,178 empresas, linea base 2022). Correlacion moderada, no alta: las dos medidas capturan aspectos relacionados pero distintos de exposicion.
  - `Exposure2022` (original): inverso del salario promedio general de la firma. Rango ~0.0000089-0.0000733 (escala de miles de pesos).
  - `Exposure2022_obreros` (nueva): participacion de obreros en el empleo total (sin propietarios). Rango 0-1 por construccion.
- Ambas coinciden en el supuesto central: **obreros es el grupo peor pagado en los 17 anios sin excepcion** (Paso 4/6). La brecha salarial con administrativos se amplio (ratio mediana 1.74 -> 2.05 entre 2008 y 2024); con profesional-tecnico se **cerro** (2.12 -> 1.72).
- `Exposure2022_obreros` correlaciona mas con sector (CIIU4, R2=18.5%) que con tamano de empresa (R2=0.4%) o DPTO (R2=1.9%).

## Decisiones metodologicas que quedan abiertas (no resueltas unilateralmente)

1. **Propietarios/socios sin remuneracion fija**: excluidos por defecto de `empleo_total_categorias` (no perciben salario fijo, no estan sujetos a la normativa de salario minimo). Se dejo `empleo_total_categorias_con_propietarios` como alternativa para comparar. Su peso es marginal (0.1-0.4 personas promedio por empresa), asi que el efecto practico de esta decision deberia ser chico, pero la eleccion conceptual queda para revision.
2. **Caida sostenida de empresas en el panel**: de ~9,468 (2010) a ~5,973 (2024), -37%. No esta claro si es atricion real de firmas o cambios en el marco muestral de la EAM a lo largo del tiempo; puede afectar la interpretacion de tendencias temporales de exposicion.
3. **Que medida de exposicion usar en la estimacion econometrica**: `Exposure2022` (nivel salarial) y `Exposure2022_obreros` (composicion ocupacional) no son intercambiables (correlacion 0.36). La eleccion entre una, otra, o ambas como robustez, es una decision de la tesis, no tecnica.
4. **CIIU3 vs CIIU4**: si mas adelante se necesita sector de forma longitudinal en todo el panel 2008-2024, CIIU3 y CIIU4 deben tratarse como variables categoricas distintas (no concatenarse).

## Bite2022_obreros (indice de Kaitz)

Rama: `feature/postpandemia-descartar-2012`. Scripts:

1. `verificar_exclusion_prestaciones_salario_obrero_eam.R` (Paso 1b)
2. `construir_bite_obreros_eam.R` (Paso 2)
3. `diagnosticos_validacion_bite_obreros_eam.R` (Paso 3)

### Que representa

`Bite2022_obreros` es un indice de Kaitz: cuanto tuvo que subir el salario base
promedio de los obreros de una firma para llegar al salario minimo legal de
2023. Se calcula como:

```
Bite2022_obreros = (SM_2023 x 12) / salario_promedio_obrero_f_2022
```

- `SM_2023` = $1.160.000 COP mensual (Decreto 2613 de 2022, Ministerio del
  Trabajo), verificado via busqueda web, no asumido de memoria.
- `salario_promedio_obrero_f_2022` (fuente: `C3R2C1`) es una cifra **anual, en
  miles de pesos**, confirmada empiricamente comparando su magnitud contra
  SM_2023 mensual (razon 13.93x, implausible) vs. SM_2023 anualizado (razon
  1.16x, plausible).

### Verificacion de escala y contenido (Paso 1 y 1b)

- **Periodicidad y unidad (Paso 1)**: confirmada por magnitud empirica, no por
  texto explicito del diccionario (el diccionario oficial no declara
  periodicidad/unidad para `C3R2C1` especificamente, aunque si lo hace para
  otras variables de la EAM, ej. `C3R19C3` dice literalmente "en miles de
  pesos").
- **Exclusion de prestaciones sociales (C3R3)**: **confirmada numericamente**.
  `C3R2` (salario) vs. `C3R2+C3R3` (salario+prestaciones, ambas filas
  explicitamente "personal permanente" en el diccionario oficial): `C3R2 <
  C3R2+C3R3` en el 100% de las firmas (n=5,108). Las prestaciones representan
  una mediana de 19.1% de (salario+prestaciones), ~23-24% sobre el salario
  base, coherente con la carga prestacional legal colombiana.
- **Exclusion de cotizaciones patronales y aportes sobre nomina (C3R5/C3R6)**:
  **solo inferida por estructura del formulario** (son filas separadas de
  C3R2), **NO confirmada numericamente**. Se intento reconciliar contra
  `C3R10` (fila oficial "Total sueldos/salarios/prestaciones/cotizaciones/
  aportes"), pero `C3R10` no es comparable 1 a 1: cubre TODAS las
  vinculaciones (permanente + temporal directo + temporal agencia), no solo
  permanente, y `C3R5`/`C3R6` ya reflejan personal ocupado completo. La suma
  parcial `C3R2+C3R3+C3R5+C3R6` quedo en una mediana de 76.4% de `C3R10`, con
  rango muy amplio (p10=12.9%, p90=97.1%): no reconcilia bien, y no se forzo
  esa aproximacion.
- **Por esta razon se omitio el Paso 2b** (variante de robustez con costo
  laboral total `Bite2022_obreros_costo_total`): no hay forma limpia de aislar
  un costo laboral total solo-personal-permanente con las variables
  disponibles en la EAM. Queda como pendiente documentado, no como tarea
  completada de esta rama.

### Diagnosticos (Paso 3)

- **Percentiles**: mediana = 0.861 (la firma mediana pagaba a sus obreros
  ~86% de lo que seria el salario minimo de 2023).
- **Correlacion con Exposure2022_obreros**: Pearson = 0.124, Spearman = 0.159
  (n=5,099). Positiva, como se esperaba, pero **debil**, no solo "no
  perfecta" — se reporta tal cual, no se minimiza.
- **Relacion con atributos** (R2 de ANOVA): tamano de empresa R2=0.116
  (Spearman=-0.399, firmas mas grandes tienden a pagar mas por encima del
  minimo), sector (CIIU4) R2=0.092, DPTO R2=0.026.
- **Fraccion de firmas con Bite > 1** (el minimo de 2023 ya superaba lo que
  pagaban a sus obreros en 2022): **29.4%** (1,499 de 5,099 firmas). Con
  Bite < 1: 70.2%.

### Nota metodologica sobre exposicion pre-choque (parrafo acordado, sin reformular)

> Bite_f es una medida de exposición pre-choque (2022), no una medida de lo
> ocurrido después del choque. En particular, un valor bajo de Bite_f (firma ya
> pagaba por encima del mínimo) no implica ni asume que la firma haya comprimido
> su estructura salarial tras el aumento del salario mínimo. La compresión
> salarial —definida como el estrechamiento de la brecha entre categorías
> ocupacionales tras el choque— es una hipótesis de mecanismo que debe evaluarse
> empíricamente comparando outcomes (por ejemplo, la evolución de la razón
> salario_administrativo / salario_obrero por nivel de exposición o bite, antes y
> después de 2023), no una propiedad asumida en la construcción de la variable de
> tratamiento. Confundir ambas cosas equivaldría a dar por probado, en la
> construcción de una variable explicativa, el resultado que la tesis debe
> demostrar.

### Robustez de la correlacion debil Bite-Exposure (Paso 5)

Script: `diagnostico_robustez_correlacion_bite_exposure_eam.R`. La correlacion
debil reportada arriba (Pearson=0.124, Spearman=0.159) se sometio a tres
chequeos de robustez, para descartar que fuera un artefacto de outliers,
forma funcional o composicion sectorial no tratados:

1. **Winsorizacion**: `salario_promedio_obrero_f` no habia pasado por el mismo
   criterio de winsorizacion (1%-99%) que usa `descriptivo_exposicion_eam.R`
   para `Exposure2022`. Al aplicarlo y recalcular `Bite2022_obreros_winsorizado`,
   la correlacion **no cambia de forma material**: Pearson pasa de 0.124 a
   0.149, Spearman se mantiene exactamente en 0.159 (n=5,103 vs. 5,099; la
   winsorizacion "rescata" 4 firmas con salario=0 que antes daban NA por
   division entre cero, al reemplazar ese 0 por el limite p1).
2. **Scatter (Bite vs Exposure, n=5,099)**: nube muy dispersa en todo el rango
   de exposicion. La curva loess practicamente coincide con la linea de
   tendencia lineal — no hay ningun patron no lineal oculto que el
   coeficiente de correlacion lineal este pasando por alto. Ver
   `4. RESULTADOS/descriptivos_exposicion/scatter_bite_vs_exposure.png`.
3. **Correlacion por sector (CIIU4, los 6 sectores con mas observaciones)**:
   rango 0.039-0.272 (Pearson), todas del mismo orden que la agregada (0.124)
   o apenas superiores. La heterogeneidad sectorial en niveles salariales
   **no** esta enmascarando una relacion firma-a-firma mucho mas fuerte.

**Conclusion**: la correlacion debil entre `Bite2022_obreros` y
`Exposure2022_obreros` es un **hallazgo robusto, no un artefacto
metodologico**. Es consistente con que ambas variables capturan dimensiones
genuinamente distintas de exposicion: `Exposure2022_obreros` mide composicion
ocupacional (cuanto pesan los obreros en el empleo total), mientras que
`Bite2022_obreros` mide nivel salarial relativo al minimo (cuanto tuvo que
subir el salario para llegar al minimo de 2023). Una firma puede tener pocos
obreros pero pagarles muy cerca del minimo, o muchos obreros bien pagados por
encima del minimo — son ejes distintos de exposicion al choque.

**Implicacion para la estrategia de estimacion**: dada la correlacion debil
(r² ≈ 0.02), `Exposure_f` y `Bite_f` deben usarse como **especificaciones
separadas** del modelo DiD (o con interacciones independientes si los grados
de libertad lo permiten). **No deben combinarse en un indice compuesto**:
promediarlas o combinarlas en un solo indice destruiria la señal especifica
de cada una, dado que estan midiendo mecanismos distintos y casi
ortogonales de exposicion al choque de salario minimo.
