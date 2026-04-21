# 2. PROCESAMIENTO

Esta carpeta guarda temporales del pipeline.

Subcarpetas relevantes:

- `_tmp_unzip/`: archivos descomprimidos para el analisis inicial
- `_tmp_diccionario/`: extracciones temporales para construir metadatos
- `_tmp_macro_base_eam/`: DTA extraidos durante la construccion de la macrobase

Notas:

- Estos archivos no son insumos finales.
- Se pueden regenerar ejecutando los scripts del proyecto.
- Se pueden limpiar con `Rscript "3. SCRIPTS/00_limpiar_temporales.R"`.
