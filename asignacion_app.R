library(shiny)
library(dplyr)
library(sf)
library(stringr)

# ========================================================
# 1. CARGA INICIAL DE DATOS
# ========================================================

FILE_DATA <- "planilla_limpia.RData"

limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "latin1", to = "UTF-8", sub = "byte")
  x <- toupper(str_trim(x))
  x <- setNames(c("A", "E", "I", "O", "U", "N"), c("Á", "É", "Í", "Ó", "Ú", "Ñ")) %>% stringr::str_replace_all(x, .)
  return(x)
}

# Cargar Shapefile real para extraer las opciones verdaderas de municipios por departamento
uruguay_municipios <- st_read("municipios/snd_limmun.shp", quiet = TRUE, options = "ENCODING=WINDOWS-1252") %>% 
  st_drop_geometry() %>%
  mutate(Muni_Limpio = limpiar_texto(municipio),
         Depto_Limpio = limpiar_texto(depto)) %>%
  # Corrección especial Cerro Chato
  mutate(Depto_Limpio = ifelse(Muni_Limpio == "CERRO CHATO", "TREINTA Y TRES", Depto_Limpio))

catalogo_municipios <- uruguay_municipios %>%
  select(Depto_Limpio, Muni_Limpio) %>%
  distinct() %>%
  arrange(Depto_Limpio, Muni_Limpio)

# Cargar dataset
load(FILE_DATA) # Crea 'planilla_limpia'
# Mapear al nombre interno usado por el asistente
sin_municipio <- planilla_limpia 
if(!"MUNICIPIO" %in% names(sin_municipio)) {
  sin_municipio$MUNICIPIO <- NA_character_
}

# Crear panel dinámico ordenando la frecuencia
pares_unicos <- sin_municipio %>%
  filter(is.na(MUNICIPIO) | MUNICIPIO == "" | MUNICIPIO == "Territorio no municipalizado") %>%
  mutate(
    Dep_Clean = limpiar_texto(DEPARTAMENTO),
    Loc_Clean = limpiar_texto(LOCALIDAD)
  ) %>%
  count(Dep_Clean, Loc_Clean, name = "Frecuencia") %>%
  arrange(desc(Frecuencia)) %>%
  mutate(ID = row_number())

# ========================================================
# 2. UI - LA INTERFAZ
# ========================================================
ui <- fluidPage(
  tags$head(tags$style("
    body { background-color: #f7f9fc; font-family: 'Segoe UI', Arial, sans-serif;}
    .main-card { background: white; border-radius: 12px; padding: 30px; box-shadow: 0 8px 16px rgba(0,0,0,0.1); max-width: 600px; margin: 40px auto; text-align: center; }
    .btn-save { font-weight: bold; width: 100%; border-radius: 8px; padding: 12px; font-size: 1.1em;}
    .counter { color: #e67e22; font-weight: 800; }
  ")),
  
  div(class = "main-card",
      h2("Asistente de Municipios", style="color: #002D62; font-weight:bold;"),
      p("Resolviendo localidades huérfanas ordenadas por volumen de impacto."),
      hr(),
      uiOutput("panel_principal")
  )
)

# ========================================================
# 3. SERVER - LÓGICA
# ========================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    indice_actual = 1,
    datos = sin_municipio,
    pares = pares_unicos,
    finalizado = nrow(pares_unicos) == 0
  )
  
  output$panel_principal <- renderUI({
    if(rv$finalizado || rv$indice_actual > nrow(rv$pares)) {
      return(div(
        h3("¡Todo resuelto! 🎉", style="color:#2ca02c;"),
        p("No quedan más localidades repetidas sin municipio asignado."),
        p("El archivo", strong("sin_municipio.Rdata"), "ha sido actualizado exitosamente.")
      ))
    }
    
    fila <- rv$pares[rv$indice_actual, ]
    depto <- fila$Dep_Clean
    loc <- fila$Loc_Clean
    frec <- fila$Frecuencia
    total_restantes <- nrow(rv$pares) - rv$indice_actual + 1
    
    # Buscar qué municipios pertenecen realmente a ese departamento
    munis_posibles <- catalogo_municipios %>% 
      filter(Depto_Limpio == depto) %>% 
      pull(Muni_Limpio)
    
    # Agregar opción de Resto
    opciones <- c(
      "Seleccionar uno...",
      "Asignar directamente a 'RESTO DE DEPARTAMENTO'" = "RESTO",
      munis_posibles
    )
    
    div(
      p("Faltan", span(class="counter", total_restantes), "casos únicos por resolver"),
      div(style="background:#f0f4f8; padding:20px; border-radius:8px; margin-bottom:20px;",
          h4(style="margin-top:0; color:#555;", "Impacto:", strong(paste(frec, "denuncias"))),
          h3(style="margin-top:5px; color:#c0392b;", depto, "-", loc)
      ),
      selectInput("muni_select", "Selecciona su municipio correcto:", choices = opciones, width = "100%"),
      br(),
      actionButton("btn_guardar", "Guardar e ir al siguiente", class="btn-primary btn-save"),
      br(), br(),
      actionButton("btn_ignorar", "No sé (Dejar en blanco y saltar)", class="btn-default", style="width:100%")
    )
  })
  
  observeEvent(input$btn_guardar, {
    req(input$muni_select)
    if(input$muni_select == "Seleccionar uno...") return()
    
    fila <- rv$pares[rv$indice_actual, ]
    depto <- fila$Dep_Clean
    loc <- fila$Loc_Clean
    muni_asignado <- input$muni_select
    
    if(muni_asignado == "RESTO") { muni_asignado <- paste("RESTO DE", depto) }
    
    # Actualizar tabla original
    rv$datos <- rv$datos %>%
      mutate(
        DepTemp = limpiar_texto(DEPARTAMENTO),
        LocTemp = limpiar_texto(LOCALIDAD),
        MUNICIPIO = ifelse(DepTemp == depto & LocTemp == loc, muni_asignado, MUNICIPIO)
      ) %>%
      select(-DepTemp, -LocTemp)
    
    # Guardar a disco destructivamente
    planilla_limpia <- rv$datos
    save(planilla_limpia, file = FILE_DATA)
    
    # Avanzar
    rv$indice_actual <- rv$indice_actual + 1
  })
  
  observeEvent(input$btn_ignorar, {
    rv$indice_actual <- rv$indice_actual + 1
  })
}

shinyApp(ui, server)
