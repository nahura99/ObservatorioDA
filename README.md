# Visualizador de Denuncias Ambientales - Uruguay 🇺🇾

Este es un tablero interactivo desarrollado con **R Shiny** para la visualización y análisis de denuncias ambientales en Uruguay. La herramienta permite explorar datos históricos mediante mapas interactivos, gráficos de series temporales y paneles de detalles por territorio.

## Características Principales

- **Mapas Interactivos (Leaflet)**:
  - **Vista Departamental**: Visualización de denuncias agregadas por departamento.
  - **Vista Municipal**: Mapa detallado por municipios con zonas delimitadas ("Restos de departamento") para dar contexto completo.
- **Análisis Temporal**: Gráficos de series históricas que permiten ver la evolución de las denuncias según el motivo y el año.
- **Filtros Dinámicos**: Filtrado por año y categorías de motivos (Agua, Aire, Fauna, Residuos, etc.).
- **Panel de Información (Hover)**: Información instantánea al pasar el mouse sobre cualquier territorio, incluyendo estadísticas de denuncias y expedientes.
- **Diseño Responsivo**: Basado en `bslib` para una experiencia moderna y fluida.

## Estructura del Proyecto

- `app.R`: Aplicación principal del tablero de visualización.
- `app_clasificador.R`: Herramienta complementaria para la clasificación y limpieza de denuncias.
- `scripts/`: Colección de scripts para pre-procesamiento de datos y utilidades geométricas.
- `data/` & `planilla_limpia.RData`: Bases de datos locales procesadas.
- `municipios/`: Capas geográficas (Shapefiles) para la delimitación municipal.

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
