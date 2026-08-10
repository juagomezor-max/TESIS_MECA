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
