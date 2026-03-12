library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(shinyWidgets)

# 1. Cargar Datos
cat("Cargando planilla_limpia.RData...\n")
load("planilla_limpia.RData")

# Validar y cargar Municipios Oficiales preexistentes (para autocompletado)
municipios_conocidos <- unique(planilla_limpia$MUNICIPIO[planilla_limpia$MUNICIPIO != ""])
municipios_conocidos <- sort(municipios_conocidos)
# Asegurar la opción "Territorio no municipalizado" en la cima de la lista
municipios_conocidos <- c("Territorio no municipalizado", municipios_conocidos[!municipios_conocidos == "Territorio no municipalizado"])

# 2. UI
ui <- page_fluid(
  theme = bs_theme(version = 5, preset = "lumen"),
  tags$head(tags$style(HTML("
    body { background-color: #f8f9fa; }
    .card-custom { background: white; border-radius: 10px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); margin-top: 20px; }
    .title-h3 { color: #002D62; font-weight: bold; border-bottom: 2px solid #002D62; padding-bottom: 10px; margin-bottom: 20px;}
    .loc-text { font-size: 24px; color: #B91D47; font-weight: bold; text-transform: uppercase;}
    .dep-text { font-size: 18px; color: #666; font-weight: bold;}
    .counter-text { font-size: 16px; color: #888; font-style: italic; text-align: right; }
  "))),

  fluidRow(
    column(10, offset = 1,
      div(class = "card-custom",
        fluidRow(
          column(8, h3("Clasificador de Municipios Manual", class = "title-h3")),
          column(4, uiOutput("contador_ui"))
        ),

        # Panel de Clasificación Actual
        fluidRow(
          column(12, align = "center",
            uiOutput("info_actual"),
            br(),
            div(style = "max-width: 500px; margin: 0 auto;",
              selectizeInput(
                inputId = "input_municipio",
                label = "Asignar Municipio (Ingrese texto libre o seleccione):",
                choices = NULL, # Se carga en server para permitir nueva entrada
                width = "100%",
                options = list(create = TRUE, placeholder = "Ej: Municipio C, San Carlos, Ninguno...")
              )
            ),
            br(),
            actionButton("btn_guardar", "Guardar y Avanzar", class = "btn-success btn-lg", icon = icon("save")),
            actionButton("btn_saltar", "Saltar / Dejar Vacío", class = "btn-outline-secondary btn-lg", icon = icon("forward"))
          )
        ),
        hr(),

        # Tabla de Contexto
        h4("Contexto: Denuncias asociadas a esta localidad", style = "color:#555;"),
        DTOutput("tabla_contexto")
      )
    )
  )
)

# 3. INTERFAZ
server <- function(input, output, session) {

  # Estado (Reactive Values)
  rv <- reactiveValues(
    datos = planilla_limpia,
    pares_unicos = NULL,
    index_actual = 1,
    municipios_lista = municipios_conocidos,
    progreso_guardado = FALSE
  )

  # Al iniciar, recalcular los pares que faltan
  observe({
    faltantes <- rv$datos %>%
      filter(MUNICIPIO == "" | is.na(MUNICIPIO),
             tolower(DEPARTAMENTO) != "montevideo",
             tolower(LOCALIDAD) != "montevideo") %>%
      group_by(DEPARTAMENTO, LOCALIDAD) %>%
      summarise(Cant_Asociada = n(), .groups = "drop") %>%
      arrange(desc(Cant_Asociada)) # Ordenar por los que más impactan primero

    rv$pares_unicos <- faltantes

    updateSelectizeInput(session, "input_municipio", choices = rv$municipios_lista, server = TRUE)
  })

  # UI de contador
  output$contador_ui <- renderUI({
    req(rv$pares_unicos)
    total <- nrow(rv$pares_unicos)
    if(total == 0) {
      div(class = "counter-text", "¡Clasificación Completa!")
    } else {
      div(class = "counter-text", paste("Clasificando:", rv$index_actual, "de", total, "localidades únicas"))
    }
  })

  # UI de Localidad/Depto Actual
  output$info_actual <- renderUI({
    req(rv$pares_unicos)
    if(nrow(rv$pares_unicos) == 0 || rv$index_actual > nrow(rv$pares_unicos)) {
      return(h3("No quedan más registros por clasificar (excluyendo Montevideo).", style="color: green;"))
    }

    actual <- rv$pares_unicos[rv$index_actual, ]
    tagList(
      div(class = "dep-text", actual$DEPARTAMENTO),
      div(class = "loc-text", actual$LOCALIDAD),
      p(style = "color: #777; margin-top: 5px;", paste("Esta localidad actualizará", actual$Cant_Asociada, "denuncias en la base."))
    )
  })

  # Tabla de Contexto
  output$tabla_contexto <- renderDT({
    req(rv$pares_unicos)
    if(nrow(rv$pares_unicos) == 0 || rv$index_actual > nrow(rv$pares_unicos)) return(NULL)

    actual <- rv$pares_unicos[rv$index_actual, ]

    filas <- rv$datos %>%
      filter(DEPARTAMENTO == actual$DEPARTAMENTO, LOCALIDAD == actual$LOCALIDAD) %>%
      select(`FECHA DENUNCIA`, MOTIVO_AGRUPADO, MOTIVO, `N° EXPEDIENTE`) %>%
      arrange(desc(`FECHA DENUNCIA`))

    datatable(filas,
              options = list(pageLength = 5, dom = 'tip', scrollX = TRUE),
              rownames = FALSE)
  })

  # Acción: Saltar (y no clasificar)
  observeEvent(input$btn_saltar, {
    if (rv$index_actual < nrow(rv$pares_unicos)) {
      rv$index_actual <- rv$index_actual + 1
      updateSelectizeInput(session, "input_municipio", selected = "")
    } else {
      showNotification("¡Llegaste al final de la lista!", type = "message")
    }
  })

  # Acción: Guardar Clasificación
  observeEvent(input$btn_guardar, {
    req(rv$pares_unicos)
    if(nrow(rv$pares_unicos) == 0 || rv$index_actual > nrow(rv$pares_unicos)) return()

    muni_ingresado <- trimws(input$input_municipio)

    # Si ingresó algo, guardamos en la base global reactiva
    if(muni_ingresado != "") {
      actual <- rv$pares_unicos[rv$index_actual, ]

      rv$datos <- rv$datos %>%
        mutate(MUNICIPIO = ifelse(
          DEPARTAMENTO == actual$DEPARTAMENTO & LOCALIDAD == actual$LOCALIDAD,
          muni_ingresado,
          MUNICIPIO
        ))

      # Agregar a la lista de sugerencias si es nuevo
      if(!(muni_ingresado %in% rv$municipios_lista)) {
        rv$municipios_lista <- sort(c(rv$municipios_lista, muni_ingresado))
      }

      # Sobreescribir archivo RData silenciosamente
      planilla_limpia <- rv$datos
      save(planilla_limpia, file = "planilla_limpia.RData")

      showNotification(paste("Guardado:", actual$LOCALIDAD, "->", muni_ingresado), type = "message", duration = 3)
    }

    # Avanzar
    if (rv$index_actual < nrow(rv$pares_unicos)) {
      rv$index_actual <- rv$index_actual + 1
      updateSelectizeInput(session, "input_municipio", selected = "")
    } else {
      showNotification("¡Llegaste al final de la lista!", type = "message")
    }
  })
}

shinyApp(ui, server)
