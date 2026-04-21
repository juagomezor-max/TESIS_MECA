# 3. SCRIPTS

Esta carpeta contiene los scripts ejecutables del proyecto.

Orden recomendado de ejecucion:

1. `00_ejecutar_flujo_eam.R`
2. `00_limpiar_temporales.R` cuando se quiera limpiar temporales

Scripts principales:

- `_utils_proyecto.R`: utilidades compartidas de rutas, paquetes y validaciones
- `analisis_eam_eac.R`: inventario exploratorio de archivos por fuente
- `construir_diccionario_maestro.R`: construccion del diccionario integrado EAM/EAC
- `construir_macro_base_eam.R`: consolidacion anual de la macrobase EAM
- `diagnostico_panel_nordemp_eam.R`: chequeos de consistencia longitudinal de NORDEMP
- `descriptivo_exposicion_eam.R`: descriptivos exploratorios de exposicion a shocks laborales

Uso minimo:

```powershell
Rscript "3. SCRIPTS/00_ejecutar_flujo_eam.R"
```
