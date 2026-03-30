library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(querychat)
library(bsicons)

# --- INYECTAR LA LLAVE PRINCIPAL DEL SISTEMA ---
# Agregamos esta línea para prevenir que R intente loguearse a través del navegador web, 
# ya que lo bloqueaba Google por permisos.
Sys.setenv(GEMINI_API_KEY = "AIzaSyB3rQtSXBpz15y3u5WnWOAezcUHw9Y5Fag")
# -----------------------------------------------

# 1. Carga de los datos dinámicamente
env_data <- new.env()
load("planilla_limpia.RData", envir = env_data)
obj_name <- ls(env_data)[1]
datos_iniciales <- env_data[[obj_name]]

# 2. Inicializar el agente globalmente con los parámetros aceptados y correctos.
# Usamos data_source en lugar de data y extra_instructions en lugar de system_prompt.
chat_agent <- QueryChat$new(
  data_source = datos_iniciales,
  
  # ---------- CONFIGURACIÓN DEL LLM ----------
  # Opción 1 (Nube - Inteligente, Rápido y Totalmente Gratis [Recomendado]):
  client = ellmer::chat_google_gemini(),
  
  # Opción Local: Ollama (Deshabilitado: Llama 3.2 falla con las Tools SQL)
  # client = ellmer::chat_ollama(model = "llama3.2"),
  # -------------------------------------------
  
  greeting = "¡Hola! Soy el analista experto del Observatorio de Denuncias Ambientales de Uruguay. Trabajos con más de 10,000 registros de incidentes registrados. ¿De qué departamento te gustaría revisar los datos o qué tipo de incidentes quieres filtrar hoy?",
  extra_instructions = "Actúa como un analista experto en el Observatorio de Denuncias Ambientales de Uruguay. Trabajas con un conjunto de datos (planillas_limpias) de más de 10,000 registros. Usa EXACTAMENTE estas columnas: 'FECHA DENUNCIA', 'DEPARTAMENTO', 'LOCALIDAD', 'MOTIVO', 'DERIVACIÓN EXTERNA', 'N° EXPEDIENTE', 'MOTIVO_AGRUPADO', 'MUNICIPIO'. Cuidado con las mayúsculas. \n\nREGLA ESTRICTA DE COMPORTAMIENTO: NUNCA le muestres al usuario tu razonamiento interno. Si te equivocas usando el código SQL, arréglalo en silencio sin decir 'Hubo un error'. \n\nIMPORTANTE SOBRE GRÁFICOS: La interfaz donde habitas ya tiene un gráfico dinámico inteligente en la parte superior. Éste se adapta matemáticamente a la tabla SQL que le pases por debajo. Si devuelves toda la tabla filtrada (SELECT *), graficará una radiografía general agrupada de DEPARTAMENTO vs MOTIVOS. Pero si tú mismo haces un SELECT custom agrupando variables (ej: SELECT MUNICIPIO, COUNT(*) FROM ... GROUP BY MUNICIPIO), el gráfico inteligente detectará que devolviste solo dos columnas e inmediatamente generará un gráfico de barras exacto sobre lo que agrupaste. Si el usuario te pide un gráfico de líneas, asegúrate de devolver dos columnas (ej: mes/año y cantidad). ¡Nunca digas 'No puedo graficar', di 'Ya en la parte superior tu gráfico se adaptó a la nueva agrupación'!"
)

# 3. Definición de la UI
ui <- page_fillable(
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly", 
    primary = "#2C3E50", 
    secondary = "#18BC9C"
  ),
  title = "Observatorio de Denuncias Ambientales de Uruguay",
  
  # --- Estructura de Tres Columnas (GRAFICO | PANEL | CHAT) ---
  layout_columns(
    col_widths = c(5, 3, 4),
    
    # [COLUMNA 1] Gráfico Dinámico
    card(
      full_screen = TRUE,
      card_header("Distribución Analítica", class = "bg-primary text-white"),
      card_body(
        plotlyOutput("grafico_principal", height = "100%")
      )
    ),
    
    # [COLUMNA 2] Panel de Métricas / Contexto
    div(
      class = "d-flex flex-column h-100 justify-content-center",
      style = "gap: 20px;",
      value_box(
        title = "Registros Resultantes",
        value = textOutput("casos_totales"),
        theme = "bg-primary",
        showcase = bsicons::bs_icon("bar-chart-fill"),
        width = "100%"
      ),
      value_box(
        title = "Estado del Analista IA",
        value = "En línea",
        theme = "bg-secondary",
        showcase = bsicons::bs_icon("cpu"),
        width = "100%"
      )
    ),
    
    # [COLUMNA 3] App de Chat nativa de querychat
    card(
      full_screen = TRUE,
      class = "d-flex",
      tags$style("
        .chat-card {
          flex: 1 1 auto;
          overflow: hidden;
        }
        /* --- Ocultar razonamientos abstractos (Tool Calls y Logs) de la interfaz de chat --- */
        #chat_history_container details, 
        .shinychat-message > details,
        .chat-message-tool {
          display: none !important;
        }
      "),
      chat_agent$ui(id = "chat")
    )
  )
)

# 4. Definición del Servidor (Lógica interactiva)
server <- function(input, output, session) {
  
  # Conectar el servidor nativo de querychat
  chat_state <- chat_agent$server(id = "chat")
  
  # Renderizando Total (Value Box)
  output$casos_totales <- renderText({
    df <- chat_state$df()
    format(nrow(df), big.mark = ".", decimal.mark = ",")
  })
  
  # Renderizando el Gráfico con Ggplotly (SUPER DINÁMICO)
  output$grafico_principal <- renderPlotly({
    df <- chat_state$df()
    
    req(is.data.frame(df))
    if(nrow(df) == 0) {
      return(plot_ly(type = "scatter", mode = "markers") %>% layout(title = "No hay datos resultantes del análisis."))
    }
    
    # Extraer titulo si querychat lo construyó, sino genérico
    un_titulo <- "Análisis Dinámico"
    if(!is.null(chat_state$title) && is.function(chat_state$title)) { 
        val <- chat_state$title()
        if(!is.null(val) && val != "") un_titulo <- val
    }

    cols <- names(df)
    n_cols <- length(cols)
    
    # ESCENARIO 1: El bot devolvió una agrupación simple de 2 columnas (ej. Categoría vs Valor Numérico)
    if(n_cols == 2) {
      col_x <- cols[1]
      col_y <- cols[2]
      
      # Intentar deducir cuál es la numérica (Y)
      if(is.numeric(df[[col_y]])) {
        df_plot <- df %>% arrange(desc(.data[[col_y]])) %>% head(15)
        p <- ggplot(df_plot, aes(x = reorder(.data[[col_x]], .data[[col_y]]), y = .data[[col_y]])) +
          geom_col(fill = "#18BC9C") + coord_flip() + theme_minimal(base_size = 14) +
          labs(x = col_x, y = col_y, title = un_titulo)
        return(ggplotly(p))
      } else if(is.numeric(df[[col_x]])) {
        df_plot <- df %>% arrange(desc(.data[[col_x]])) %>% head(15)
        p <- ggplot(df_plot, aes(x = reorder(.data[[col_y]], .data[[col_x]]), y = .data[[col_x]])) +
          geom_col(fill = "#18BC9C") + coord_flip() + theme_minimal(base_size = 14) +
          labs(x = col_y, y = col_x, title = un_titulo)
        return(ggplotly(p))
      }
    }
    
    # ESCENARIO 2: El bot filtró filas pero mantuvo la tabla completa original (>= 4 columnas usuales)
    if("DEPARTAMENTO" %in% cols && "MOTIVO_AGRUPADO" %in% cols) {
      top_depto <- df %>% count(DEPARTAMENTO, sort = TRUE) %>% head(8) %>% pull(DEPARTAMENTO)
      df_top <- df %>% filter(DEPARTAMENTO %in% top_depto)
      df_top$MOTIVO_AGRUPADO <- ifelse(is.na(df_top$MOTIVO_AGRUPADO), "Desconocido", df_top$MOTIVO_AGRUPADO)
      
      p_base <- ggplot(df_top, aes(x = DEPARTAMENTO, fill = MOTIVO_AGRUPADO)) +
        geom_bar(position = "stack") +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(x = "Departamento", y = "Cantidad de Denuncias", title = un_titulo) +
        theme(legend.position = "right", plot.title = element_text(face = "bold", size = 16)) +
        scale_fill_viridis_d(option = "cividis")
      
      return(ggplotly(p_base, tooltip = c("y", "fill")) %>%
        layout(legend = list(orientation = "v", x = 1, y = 0.5), margin = list(l = 20, r = 20, b = 20, t = 40)))
    } 
    
    # ESCENARIO 3: Agrupación genérica por 1 columna sola o más variables
    if(n_cols == 1 && is.numeric(df[[1]])) {
      return(plot_ly(type="indicator", mode="number", value=df[[1]][1], title=list(text=cols[1])))
    }
    
    # Salvavidas visual genérico
    plot_ly(type = "scatter", mode = "markers") %>% layout(title = paste("Datos agrupados (", n_cols, "columnas). Filtro aplicado correctamente."))
  })
}

# Ejecución final
shinyApp(ui, server)
