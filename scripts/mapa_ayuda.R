library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(geouy)

cat("Iniciando aplicación de ayuda cartográfica...\n")

sf_use_s2(FALSE)

# 1. Cargar Municipios Oficiales
cat("Cargando capa de Municipios...\n")
capa_muni <- st_read("municipios/snd_limmun.shp", quiet=TRUE, options="ENCODING=WINDOWS-1252") %>%
  st_transform(4326)

# 2. Cargar Localidades de IDE Uruguay (geouy)
cat("Descargando capa de Localidades desde IDE Uruguay...\n")
capa_loc <- NULL
rutas_nombres <- c("Centros Poblados", "Localidades", "Asentamientos")

for(nombre in rutas_nombres) {
  capa_temp <- tryCatch({
    suppressWarnings(suppressMessages(load_geouy(nombre)))
  }, error = function(e) { NULL })

  if(!is.null(capa_temp) && nrow(capa_temp) > 0) {
    capa_loc <- capa_temp %>% st_transform(4326)
    cat("Capa encontrada:", nombre, "\n")
    break
  }
}

if(is.null(capa_loc)) {
  stop("No se pudo descargar la capa de localidades desde IDE. Verifica tu conexión o el nombre de la capa.")
}

# Obtener nombre de la columna y centroides
capa_loc_pts <- suppressWarnings(st_centroid(capa_loc))
col_nombres <- grep("nombre|localidad|toponimo", names(capa_loc_pts), ignore.case = TRUE, value = TRUE)[1]
if(is.na(col_nombres)) col_nombres <- names(capa_loc_pts)[1]

capa_loc_pts <- capa_loc_pts %>%
  mutate(Nombre_Loc = as.character(!!sym(col_nombres))) %>%
  filter(!is.na(Nombre_Loc) & Nombre_Loc != "")

opciones_loc <- sort(unique(capa_loc_pts$Nombre_Loc))

# 3. Interfaz de Usuario
ui <- fluidPage(
  titlePanel("Visor de Localidades y Municipios - IDE Uruguay"),
  sidebarLayout(
    sidebarPanel(
      h4("Buscador"),
      selectizeInput("buscador", "Escriba para buscar una Localidad:",
                     choices = c("Seleccione..." = "", opciones_loc),
                     options = list(placeholder = 'Ej: San Carlos')),
      hr(),
      helpText("El marcador rojo indicará la localidad seleccionada. Los polígonos azules translúcidos son los límites municipales."),
      width = 3
    ),
    mainPanel(
      leafletOutput("mapa", height = "85vh"),
      width = 9
    )
  )
)

# 4. Lógica del Servidor
server <- function(input, output, session) {

  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = capa_muni,
                  color = "#2980b9", weight = 2, fillOpacity = 0.1,
                  label = ~paste("MUNICIPIO:", municipio, "-", depto),
                  highlightOptions = highlightOptions(color = "#e67e22", weight = 3, bringToFront = TRUE)) %>%
      addCircleMarkers(data = capa_loc_pts,
                       color = "#27ae60", radius = 3, stroke = FALSE, fillOpacity = 0.6,
                       label = ~Nombre_Loc)
  })

  observeEvent(input$buscador, {
    req(input$buscador)

    loc_sel <- capa_loc_pts %>% filter(Nombre_Loc == input$buscador)

    if(nrow(loc_sel) > 0) {
      bb <- st_bbox(loc_sel)
      leafletProxy("mapa") %>%
        setView(lng = as.numeric(bb["xmin"]), lat = as.numeric(bb["ymin"]), zoom = 12) %>%
        clearGroup("highlight") %>%
        addCircleMarkers(data = loc_sel, color = "#c0392b", radius = 8,
                         fillOpacity = 1, group = "highlight",
                         label = ~Nombre_Loc,
                         labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                                     style = list("color" = "red", "font-weight" = "bold")))
    }
  })
}

shinyApp(ui, server)
