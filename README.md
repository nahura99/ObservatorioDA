# Visualizador de Denuncias Ambientales - Uruguay 🇺🇾

Este es un tablero interactivo desarrollado con **R Shiny** para la visualización y análisis de denuncias ambientales en Uruguay. La herramienta permite explorar datos históricos mediante mapas interactivos y gráficos de series temporales.

## Características Principales

- **Mapas Interactivos**:
  - **Vista Departamental**: Visualización de denuncias agregadas por departamento.
  - **Vista Municipal**: Mapa detallado por municipios.
- **Serie, Composición y Estacionalidad**: Gráficos adicionales.
- **Filtros Dinámicos**: Filtrado por año, departamentos y motivos (Agua, Aire, Fauna, Residuos, etc.).
- **Panel de Información**: Información al pasar el mouse sobre cualquier territorio, incluyendo estadísticas de denuncias y expedientes.

## Estructura del Proyecto

- `app.R`: Aplicación principal del tablero de visualización.
- `app_clasificador.R`: Herramienta complementaria para la clasificación y limpieza de denuncias.
- `scripts/`: Colección de scripts para pre-procesamiento de datos y utilidades geométricas.
- `data/` & `planilla_limpia.RData`: Bases de datos locales procesadas.

##️ Requisitos e Instalación

Para ejecutar este proyecto localmente, asegúrate de tener instalado **R** y las siguientes librerías:

```r
install.packages(c("shiny", "bslib", "shinyWidgets", "ggplot2", "sf", 
                   "dplyr", "stringr", "lubridate", "ggiraph", 
                   "leaflet", "shinyjs", "htmlwidgets", "waiter"))
```

### Ejecutar la aplicación:
Simplemente abre el archivo `app.R` en RStudio y presiona el botón **"Run App"**, o ejecuta en la consola:
```r
shiny::runApp()
```

## Créditos
Desarrollado por Nahuel Roel.
