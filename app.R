################################################################################
############################## OBSERVATORIO-DA #################################
################################################################################

# Cargar librerías necesarias
packages <- c("shiny", "bslib", "shinyWidgets", "ggplot2", "sf", "dplyr", "stringr", "lubridate", "ggiraph", "shinycssloaders", "leaflet", "shinyjs", "htmlwidgets", "waiter", "ggrepel", "tidyr", "DT")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
}
options(shiny.maxRequestSize = 30 * 1024^2)
options(shiny.proxy.address.forwarding = TRUE)
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(ggplot2)
  library(sf)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(ggiraph)
  library(shinycssloaders)
  # (geouy removida para mayor estabilidad)
  library(leaflet)
  library(shinyjs)
  library(htmlwidgets)
  library(waiter)
  library(ggrepel)
  library(tidyr)
  library(DT)
})

# Helper para el cargador original (UX revertida a solicitud del usuario)
make_waiter_html <- function(pct = 0, text = NULL) {
  # Volvemos al círculo simple original (spin_3) sin números ni texto
  tagList(
    div(style = "transform: scale(1.6);", waiter::spin_3())
  )
}

# ==========================================
# 1. PREPARACIÓN DE DATOS GLOBALES
# ==========================================
if (file.exists("data/df_app.rds")) {
  df <- readRDS("data/df_app.rds")
} else {
  stop("El archivo data/df_app.rds no se encuentra. Ejecute scripts/preparar_app_data.R primero.")
}
anios_str <- sort(unique(df$Anio), decreasing = FALSE)
# Primer año con al menos un expediente (para restringir el slider en modo Expedientes)
primer_anio_expediente <- df %>%
  filter(Tiene_Expediente == 1) %>%
  pull(Anio) %>%
  as.numeric() %>%
  min(na.rm = TRUE)
motivos_str <- sort(unique(df$MOTIVO_AGRUPADO))

# Ordenamiento de chips en grilla 3x4
opciones_motivo <- c(
  "Todos los motivos",
  "Agroquímicos y Sustancias Peligrosas",
  "Agua y Vertidos",
  "Aire y Emisiones",
  "Costa y Áreas Protegidas",
  "Fauna y Biodiversidad",
  "Gestión de Residuos",
  "Gestión Institucional y Obras",
  "Minería",
  "Ruidos",
  "Olores",
  "Otros"
)
colores_motivos <- c(
  "Todos los motivos" = "#002D62",
  "Fauna y Biodiversidad" = "#2ca02c",
  "Otros" = "#7f7f7f",
  "Agua y Vertidos" = "#1f77b4",
  "Ruidos" = "#9467bd",
  "Minería" = "#d62728",
  "Aire y Emisiones" = "#17becf",
  "Costa y Áreas Protegidas" = "#20c997",
  "Agroquímicos y Sustancias Peligrosas" = "#FF7700",
  "Gestión de Residuos" = "#8B4513",
  "Gestión Institucional y Obras" = "#bcbd22",
  "Olores" = "#e377c2"
)

# Paleta categorial para Submotivos (Tableau ampliada)
colores_distintos_12 <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
  "#bcbd22", "#17becf", "#393b79", "#637939"
)
# ==========================================
# Cargar geodatos pre-procesados (evita descarga de internet en cada inicio)
# Ejecutar scripts/preparar_geodatos.R una sola vez para generar este archivo.
# ==========================================
if (!file.exists("geodatos.RData")) {
  stop("El archivo geodatos.RData no se encuentra. Ejecute scripts/preparar_geodatos.R primero.")
}
load("geodatos.RData")
# geodatos.RData contiene: uruguay_mapa, uruguay_municipios, uruguay_mapa_bboxes,
# uruguay_mapa_centroids, uruguay_municipios_centroids, deps_bb

# Este archivo ya contiene uruguay_mapa y uruguay_municipios optimizados en tamaño.

lista_deptos_vals <- sort(unique(uruguay_mapa$Depto_Limpio))
lista_deptos_nombres <- stringr::str_to_title(tolower(lista_deptos_vals))
lista_deptos_nombres <- gsub(" Y ", " y ", lista_deptos_nombres)
lista_deptos <- setNames(lista_deptos_vals, lista_deptos_nombres)

# Definir paleta de colores fija para Serie Histórica
colores_departamentos_vivos <- c(
  "MONTEVIDEO" = "#E31A1C", # Rojo Fuerte
  "MALDONADO"  = "#1F78B4", # Azul Fuerte
  "ROCHA"      = "#33A02C", # Verde Fuerte
  "COLONIA"    = "#FF7F00", # Naranja Fuerte
  "CANELONES"  = "#6A3D9A", # Violeta Fuerte
  "SALTO"      = "#e6194B" # Carmesí Fuerte
)
paleta_suave <- c("#A6CEE3", "#B2DF8A", "#FB9A99", "#FDBF6F", "#CAB2D6", "#FFFF99", "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5")
depto_resto <- setdiff(lista_deptos_vals, names(colores_departamentos_vivos))
colores_departamentos_suaves <- setNames(paleta_suave[1:length(depto_resto)], depto_resto)
paleta_final_series <- c(colores_departamentos_vivos, colores_departamentos_suaves, "Todos" = "#7f7f7f")
# Configuración de dimensiones relativas por defecto (Vista Mapa)
COL_LEFT_PCT <- "60%"
COL_RIGHT_PCT <- "38%" # 60 + 38 + 2 (gap) = 100
GAP_PCT <- "2%"

# Configuración de ancho para la vista de SERIE Histórica
# Aumenta este valor si quieres que el gráfico de serie sea más ancho (Ej: "75%")
SERIES_WIDTH_PCT <- "60%"
# El ancho de la columna derecha (Motivos) se ajustará automáticamente
SERIES_RIGHT_PCT <- paste0(100 - as.numeric(gsub("%", "", SERIES_WIDTH_PCT)) - as.numeric(gsub("%", "", GAP_PCT)), "%")

# ==========================================
# 2. INTERFAZ DE USUARIO (UI)
# ==========================================

# >>> AJUSTES MANUALES DE DISEÑO <<<
# Altura máxima del mapa y del panel de serie de tiempo (en píxeles)
# Disminuye este valor si quieres que la interfaz termine más arriba
MAP_HEIGHT <- "550px"

# Altura específica para el gráfico de SERIES
SERIES_HEIGHT <- "450px"

# Ancho interno de la caja blanca del gráfico de SERIE (puede ser en px o %)
# Disminuye este valor para que la tarjeta no sobre horizonalmente sino que abrace al gráfico de serie
SERIES_BOX_WIDTH <- "800px"

# Desplazamiento vertical interno del mapa
# Aumenta este valor (ej. 0.8, 1.2) para "bajar" el mapa visualmente dentro de su caja sin perder el nivel de zoom
MAP_VERTICAL_OFFSET <- 0

# Tamaño del logo superior izquierdo (ancho y alto). Por defecto 80px.
TAMANIO_LOGO_ENCABEZADO <- "60px"

ui <- page_fluid(
  theme = bs_theme(version = 5, preset = "lumen"),
  useShinyjs(),
  useWaiter(),
  waiterShowOnLoad(
    html = tagList(
      HTML('<svg width="120" height="120" viewBox="0 0 100 100" style="margin-bottom: 20px;">
              <rect x="5" y="5" width="90" height="90" rx="22" fill="#1B4332" />
              <path d="M25 50 H75 V80 H25 Z" fill="none" stroke="white" stroke-width="4" stroke-linejoin="round"/>
              <path d="M25 50 L50 70 L75 50" fill="none" stroke="white" stroke-width="4" stroke-linejoin="round"/>
              <path d="M50 50 L50 20" stroke="white" stroke-width="3" stroke-linecap="round" />
              <path d="M50 20 L58 12 C 54 10, 46 10, 42 12 L50 20" fill="white" />
              <path d="M50 28 L60 22 C 55 18, 50 18, 50 28" fill="white" />
              <path d="M50 38 L62 34 C 58 30, 52 30, 50 38" fill="white" />
              <path d="M50 28 L40 22 C 45 18, 50 18, 50 28" fill="white" />
              <path d="M50 38 L38 34 C 42 30, 48 30, 50 38" fill="white" />
            </svg>'),
      spin_3(),
      h4("Cargando Observatorio...", style = "color: #1B4332; margin-top: 15px; font-weight: bold;")
    ),
    color = "#F4FAF6"
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML(paste0("
    body, .content-wrapper, .right-side { background-color: #F4FAF6 !important; overflow-x: auto; }
    .obs-title { font-weight: 900; letter-spacing: -0.5px; margin: 0 !important; font-size: 2.2rem; color: #1B4332; white-space: nowrap; line-height: 60px; display: flex; align-items: center; height: 60px; pointer-events: none; }

    /* Estilos Datatable Paginacion Minimalista */
    .dataTables_wrapper .pagination .page-link {
      padding: 10px 18px !important;
      text-decoration: none !important;
      border: 1px solid transparent !important;
      margin: 0 4px !important;
      border-radius: 8px !important;
      color: #1B4332 !important;
      background-color: transparent !important;
      font-weight: 600 !important;
      transition: all 0.2s ease-in-out !important;
    }
    .dataTables_wrapper .pagination .page-link:hover {
      background-color: #e9f2ec !important;
      border-color: #e9f2ec !important;
    }
    .dataTables_wrapper .pagination .page-item.active .page-link {
      background-color: #1B4332 !important;
      color: white !important;
      border-color: #1B4332 !important;
    }
    .dataTables_wrapper .pagination .page-item.disabled .page-link {
      color: #adb5bd !important;
      background-color: transparent !important;
    }

    /* Estilos para el chip de contacto y su popup */
    .contacto-wrapper {
      position: relative;
      display: flex;
      align-items: center;
      height: 60px;
      z-index: 5000; /* Suficiente para estar sobre el mapa (1000) pero debajo del Waiter inicial */
    }
    .contacto-chip {
      background-color: #fce4ec;
      border: 1px solid #e91e63;
      border-radius: 12px;
      padding: 0 12px;
      font-size: 0.75rem;
      font-weight: bold;
      color: #e91e63;
      cursor: pointer;
      display: flex;
      align-items: center;
      gap: 5px;
      transition: all 0.2s;
      height: 34px; /* Altura exacta para alinear con selectores */
      box-sizing: border-box;
      position: relative;
      z-index: 100001;
    }
    .contacto-chip:hover {
      background-color: #e91e63;
      color: white;
    }
    .contacto-popup {
      display: none;
      position: absolute;
      top: 100%;
      right: 0;
      margin-top: 10px;
      background: white;
      border: 2px solid #1B4332;
      border-radius: 10px;
      padding: 15px;
      width: 320px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.15);
      z-index: 100005; /* Valor muy alto para estar sobre Waiters y mapas */
    }
    .contacto-popup.active {
      display: block;
    }
    .contacto-close {
      position: absolute;
      top: 5px;
      right: 10px;
      cursor: pointer;
      font-weight: bold;
      color: #999;
    }
    .contacto-close:hover { color: #333; }

    .btn-rounded { border-radius: 20px !important; font-weight: 700; font-size: 1rem; padding: 6px 20px; }

    .shiny-input-container { margin-bottom: 0 !important; }
    .header-container { display: flex; align-items: center; justify-content: space-between; flex-wrap: nowrap; width: 100%; margin: 5px 0 15px 0; padding: 0; height: 60px; }
    .header-left-block { flex: 0 0 ", COL_LEFT_PCT, "; width: ", COL_LEFT_PCT, "; display: flex; align-items: center; justify-content: space-between; gap: 15px; height: 60px; }
    .header-right-block { flex: 0 0 ", COL_RIGHT_PCT, "; width: ", COL_RIGHT_PCT, "; display: flex; align-items: center; justify-content: flex-end; height: 60px; box-sizing: border-box; }

    .main-selector-container { width: 100%; margin: 0 !important; display: flex; align-items: center; height: 60px; }
    .main-selector-container label.btn {
      flex: 1;
      border-radius: 10px !important;
      padding: 4px 6px !important;
      margin: 2px 2px !important;
      font-weight: 600;
      font-size: 0.75rem !important;
      border: 1px solid #1B4332 !important;
      background-color: #ffffff !important;
      color: #1B4332 !important;
      text-transform: none !important;
      transition: all 0.2s ease-in-out;
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 34px;
      width: 100%;
      flex: 1 1 0;
      box-shadow: 0 2px 4px rgba(0,0,0,0.02);
    }
    .main-selector-container .btn-group { width: 100% !important; display: flex !important; }

    /* Responsividad */
    @media (max-width: 992px) {
      .header-container { flex-direction: column; align-items: stretch; gap: 10px; }
      .header-left-block, .header-right-block { flex: 0 0 100%; width: 100%; padding-right: 0; }
      .obs-title { font-size: 1.5rem; text-align: center; }
      .contacto-wrapper { margin-left: 0; }
    }
    .main-selector-container label.btn:hover {
      background-color: #1B4332 !important;
      color: white !important;
    }
    .main-selector-container input:checked + label.btn {
      background-color: #1B4332 !important;
      border-color: #1B4332 !important;
      color: white !important;
    }

    .slider-row { padding: 0 10px; }
    .irs--round .irs-bar { background-color: #002D62 !important; }
    .irs--round .irs-handle { border-color: #002D62 !important; }
    .irs--round .irs-single, .irs--round .irs-from, .irs--round .irs-to { background-color: #002D62 !important; font-weight: bold; }

    .motivo-buttons { margin-top: -10px; margin-left: 15px; }
    .motivo-buttons label.btn {
      border-radius: 10px !important;
      padding: 3px 5px !important;
      margin: 2px !important;
      font-weight: 600;
      font-size: 0.6rem !important;
      border: 1px solid #1B4332 !important;
      background-color: #ffffff !important;
      color:#1B4332 !important;
      text-transform: none !important;
      transition: all 0.2s ease-in-out;
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 28px;
      text-align: center;
      height: 100%;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      width: 145px;
      flex: 0 0 145px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.02);
    }
    .motivo-buttons .btn-group { display: flex; flex-wrap: wrap; justify-content: center; gap: 0; width: 100%; }

    .depto-buttons { margin-top: 10px; margin-left: 15px; }
    .depto-buttons label.btn, .btn-depto-action {
      border-radius: 10px !important;
      padding: 2px 4px !important;
      margin: 2px !important;
      font-weight: 600;
      font-size: 0.55rem !important;
      border: 1px solid #002D62 !important;
      background-color: #ffffff !important;
      color: #002D62 !important;
      text-transform: none !important;
      transition: all 0.2s ease-in-out;
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 24px;
      text-align: center;
      height: 100%;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      width: 105px;
      flex: 0 0 105px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.02);
    }
    .btn-depto-action {
      border: 1px solid #777777 !important;
      color: #777777 !important;
      width: 105px !important;
      flex: 0 0 105px !important;
    }
    .btn-depto-action:hover, .btn-depto-action:active {
      background-color: #777777 !important;
      color: white !important;
    }
    .depto-buttons .btn-group { display: flex; flex-wrap: wrap; justify-content: center; gap: 0; width: 100%; }
    .depto-buttons input:checked + label.btn,
    .depto-buttons input + label.btn:hover {
      background-color: #002D62 !important;
      color: white !important;
    }

    .capa-buttons .btn { border-radius: 8px !important; padding: 8px 15px; margin: 5px; font-size: 0.95rem; font-weight: 600; border: 2px solid #002D62; color: #002D62; background: transparent;}
    .capa-buttons .btn.active { background-color: #002D62 !important; color: white !important; }

    .leaflet-container { background: transparent !important; height: ", MAP_HEIGHT, " !important; width: 100% !important; }
    .container-fluid { padding-left: 0px !important; padding-right: 0px !important; max-width: 100% !important; }
    #mapa_interactivo { max-width: 100% !important; margin: 0 !important; height: ", MAP_HEIGHT, " !important; width: 100% !important; }

    /* Ajuste de Colores por Motivo (Hover y Active) */
    ", paste0(sapply(names(colores_motivos), function(m) {
      col <- colores_motivos[m]
      m_esc <- gsub("'", "\\\\'", m)
      paste0("
        .motivo-buttons input[value='", m_esc, "'] + label:hover,
        .motivo-buttons input[value='", m_esc, "']:checked + label {
          background-color: ", col, " !important;
          border-color: ", col, " !important;
          color: white !important;
        }
      ")
    }), collapse = ""), "
  ")))
  ),
  div(
    id = "wrapper_principal", style = "width: 98%; margin: 0 auto;",
    div(
      class = "header-container",
      style = "padding: 0;",
      # Bloque Izquierdo: Logo + Título + Contacto (alineado con la columna del mapa)
      div(
        class = "header-left-block",
        div(
          style = "display: flex; align-items: center;",
          HTML(paste0('<svg width="', TAMANIO_LOGO_ENCABEZADO, '" height="', TAMANIO_LOGO_ENCABEZADO, '" viewBox="0 0 100 100" style="margin-right: 15px;">
                  <rect x="5" y="5" width="90" height="90" rx="22" fill="#1B4332" />
                  <path d="M25 50 H75 V80 H25 Z" fill="none" stroke="white" stroke-width="4" stroke-linejoin="round"/>
                  <path d="M25 50 L50 70 L75 50" fill="none" stroke="white" stroke-width="4" stroke-linejoin="round"/>
                  <path d="M50 50 L50 20" stroke="white" stroke-width="3" stroke-linecap="round" />
                  <path d="M50 20 L58 12 C 54 10, 46 10, 42 12 L50 20" fill="white" />
                  <path d="M50 28 L60 22 C 55 18, 50 18, 50 28" fill="white" />
                  <path d="M50 38 L62 34 C 58 30, 52 30, 50 38" fill="white" />
                  <path d="M50 28 L40 22 C 45 18, 50 18, 50 28" fill="white" />
                  <path d="M50 38 L38 34 C 42 30, 48 30, 50 38" fill="white" />
                </svg>')),
          h1("Observatorio de Denuncias Ambientales", class = "obs-title")
        ),
        div(
          class = "contacto-wrapper",
          div(
            class = "contacto-chip", id = "btn_contacto_chip",
            HTML("<i class='fa-solid fa-address-card'></i> Contacto")
          ),
          div(
            class = "contacto-popup", id = "popup_contacto",
            div(class = "contacto-close", id = "btn_cerrar_contacto", "✖"),
            p(style = "margin: 0 0 10px 0; font-size: 0.9rem; color: #444;", "Por dudas, consultas o más información comunicarse con:"),
            a(href = "mailto:nahuel.roel@cienciasociales.edu.uy", style = "font-weight: bold; color: #1B4332; word-wrap: break-word;", "nahuel.roel@cienciasociales.edu.uy"),
            hr(style = "margin: 10px 0;"),
            p(style = "margin: 0; font-size: 0.8rem; color: #777; font-style: italic;", "Desarrollado con R y Shiny")
          ),
          # Script mínimo para alternar visibilidad sin viajes al servidor
          tags$script(HTML("
            document.getElementById('btn_contacto_chip').addEventListener('click', function() {
              document.getElementById('popup_contacto').classList.toggle('active');
            });
            document.getElementById('btn_cerrar_contacto').addEventListener('click', function() {
              document.getElementById('popup_contacto').classList.remove('active');
            });
            // Cerrar al hacer click afuera
            document.addEventListener('click', function(event) {
              var wrapper = document.querySelector('.contacto-wrapper');
              if (wrapper && !wrapper.contains(event.target)) {
                document.getElementById('popup_contacto').classList.remove('active');
              }
            });
          "))
        )
      ),
      # Bloque Derecho: Selector de vista (alineado con la columna de motivos)
      div(
        class = "header-right-block",
        div(
          class = "main-selector-container",
          radioGroupButtons(inputId = "modo_vista", label = NULL, choices = c("Mapa Departamental" = "MAPA", "Mapa Municipal" = "MAPA MUNICIPAL", "Expedientes" = "EXPEDIENTES", "Serie" = "SERIE", "Estacionalidad" = "ESTACIONALIDAD", "Composición" = "COMPOSICION", "Tabla" = "TABLA"), selected = "MAPA", status = "primary", justified = TRUE)
        )
      )
    ),
    div(
      style = paste0("display: flex; gap: ", GAP_PCT, "; align-items: flex-start; width: 100%;"),
      # Columna Izquierda (Controles de tiempo + Visualización principal)
      div(
        id = "columna_izquierda", style = paste0("flex: 0 0 ", COL_LEFT_PCT, "; width: ", COL_LEFT_PCT, "; transition: width 0.3s ease;"),
        div(
          id = "controles_mapa", style = "margin-bottom: 5px;",
          div(
            class = "slider-row",
            tags$div(style = "font-size: 11px; font-weight: bold; color: #555; text-align: center;", "Ajustar período"),
            sliderInput("filtro_anio", NULL, min = as.numeric(min(anios_str)), max = as.numeric(max(anios_str)), value = c(as.numeric(min(anios_str)), as.numeric(max(anios_str))), step = 1, sep = "", width = "100%")
          )
        ),
        shinyjs::hidden(div(
          id = "controles_serie", style = "text-align: center; margin-bottom: 8px;",
          radioGroupButtons("filtro_formato_serie", choices = c("CANTIDAD", "PORCENTAJE"), selected = "CANTIDAD", status = "primary")
        )),
        div(
          id = "contenedor_mapa", style = paste0("height: ", MAP_HEIGHT, "; width: 100%; position: relative; border-radius: 12px; overflow: hidden; box-shadow: 0 4px 12px rgba(0,0,0,0.05);"),
          actionButton("reset_map_zoom", HTML("<i class='fa-solid fa-rotate-right'></i>"),
            style = "position: absolute; top: 10px; left: 10px; z-index: 1000; border-radius: 5px; padding: 6px 10px; color: #1B4332; background: white; border: 2px solid rgba(0,0,0,0.2); box-shadow: 0 1px 5px rgba(0,0,0,0.65);",
            title = "Restaurar vista de mapa"
          ),
          div(
            style = "position: absolute; bottom: 20px; left: 10px; z-index: 1000; display: flex; flex-direction: column; gap: 5px;",
            actionButton("btn_zoom_in", HTML("<i class='fa-solid fa-plus'></i>"),
              style = "border-radius: 5px; padding: 6px 10px; color: #1B4332; background: white; border: 2px solid rgba(0,0,0,0.2); box-shadow: 0 1px 5px rgba(0,0,0,0.65);",
              title = "Aumentar zoom"
            ),
            actionButton("btn_zoom_out", HTML("<i class='fa-solid fa-minus'></i>"),
              style = "border-radius: 5px; padding: 6px 10px; color: #1B4332; background: white; border: 2px solid rgba(0,0,0,0.2); box-shadow: 0 1px 5px rgba(0,0,0,0.65);",
              title = "Disminuir zoom"
            )
          ),
          leafletOutput("mapa_interactivo", height = MAP_HEIGHT, width = "100%")
        ),
        shinyjs::hidden(div(
          id = "contenedor_serie",
          style = paste0("width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 5px auto 0 auto; position: relative; border-radius: 12px; overflow: hidden; background: white; box-shadow: 0 4px 12px rgba(0,0,0,0.05);"),
          uiOutput("titulo_serie"),
          girafeOutput("grafico_serie", height = SERIES_HEIGHT)
        )),
        shinyjs::hidden(div(
          id = "contenedor_estacional",
          style = paste0("width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 15px auto 0 auto; position: relative; border-radius: 12px; overflow: hidden; background: white; box-shadow: 0 4px 12px rgba(0,0,0,0.05);"),
          tags$div(style = "text-align: center; font-weight: 700; font-size: 13px; color: #333; padding: 12px 0 4px 0; background: white;", "Distribuci\u00f3n mensual de denuncias"),
          girafeOutput("grafico_estacional", height = SERIES_HEIGHT)
        )),
        shinyjs::hidden(div(
          id = "contenedor_composicion",
          style = "width: 100%; max-width: 100%; margin: 15px auto 0 auto; position: relative; border-radius: 12px; overflow: hidden; background: white; box-shadow: 0 4px 12px rgba(0,0,0,0.05);",
          tags$div(style = "text-align: center; font-weight: 700; font-size: 13px; color: #333; padding: 12px 0 4px 0; background: white;", "Composici\u00f3n de sub-motivos"),
          girafeOutput("grafico_composicion", height = SERIES_HEIGHT)
        )),
        shinyjs::hidden(div(
          id = "contenedor_tabla",
          style = paste0("width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 15px auto 0 auto; position: relative; border-radius: 12px; overflow: hidden; background: white; box-shadow: 0 4px 12px rgba(0,0,0,0.05); padding: 15px;"),
          tags$div(style = "text-align: center; font-weight: 700; font-size: 13px; color: #333; margin-bottom: 15px;", "Tabla de Denuncias"),
          DT::dataTableOutput("tabla_denuncias")
        )),
        # --- FILTRO DEPARTAMENTOS: SERIE ---
        shinyjs::hidden(div(
          id = "panel_deptos_serie", class = "depto-buttons", style = paste0("width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 15px auto 0 auto;"),
          tags$div(style = "font-size: 11px; font-weight: bold; color: #555; text-align: center; margin-bottom: 5px;", "Departamentos (Serie)"),
          actionButton("btn_sel_serie", "Todos", class = "btn-depto-action"),
          actionButton("btn_desel_serie", "Ninguno", class = "btn-depto-action"),
          checkboxGroupButtons("filtro_deptos_serie", NULL, choices = lista_deptos, selected = c("CANELONES", "MALDONADO", "ROCHA", "COLONIA"))
        )),
        # --- FILTRO DEPARTAMENTOS: ESTACIONALIDAD ---
        shinyjs::hidden(div(
          id = "panel_deptos_estacional", class = "depto-buttons", style = paste0("width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 40px auto 0 auto;"),
          tags$div(style = "font-size: 11px; font-weight: bold; color: #555; text-align: center; margin-bottom: 5px;", "Departamentos (Estacionalidad)"),
          actionButton("btn_sel_estacional", "Todos", class = "btn-depto-action"),
          actionButton("btn_desel_estacional", "Ninguno", class = "btn-depto-action"),
          checkboxGroupButtons("filtro_deptos_estacional", NULL, choices = lista_deptos, selected = lista_deptos)
        )),
        # --- FILTRO DEPARTAMENTOS: COMPOSICION ---
        shinyjs::hidden(div(
          id = "panel_deptos_composicion", class = "depto-buttons", style = paste0("width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 15px auto 0 auto;"),
          tags$div(style = "font-size: 11px; font-weight: bold; color: #555; text-align: center; margin-bottom: 5px;", "Departamentos (Composición)"),
          actionButton("btn_sel_composicion", "Todos", class = "btn-depto-action"),
          actionButton("btn_desel_composicion", "Ninguno", class = "btn-depto-action"),
          checkboxGroupButtons("filtro_deptos_composicion", NULL, choices = lista_deptos, selected = lista_deptos)
        ))
      ),

      # Columna Derecha (Filtro de motivos + Detalle + Filtro Departamentos)
      div(
        id = "columna_derecha", style = paste0("flex: 0 0 ", COL_RIGHT_PCT, "; width: ", COL_RIGHT_PCT, "; display: flex; flex-direction: column; transition: width 0.3s ease;"),
        div(
          class = "motivo-buttons", style = "margin-bottom: 10px;",
          tags$div(style = "font-size: 11px; font-weight: bold; color: #555; text-align: center;", "Motivos"),
          radioGroupButtons("filtro_motivo", NULL, choiceNames = opciones_motivo, choiceValues = opciones_motivo, selected = opciones_motivo[1], justified = TRUE)
        ),
        uiOutput("panel_descripcion_modo"),
        div(
          id = "panel_detalle_container", style = paste0("height: ", MAP_HEIGHT, "; width: 100%; margin-top: 0;"),
          uiOutput("panel_hover_info")
        )
      )
    )
  )
)
# ==========================================
# 3. LÓGICA DEL SERVIDOR
# ==========================================
server <- function(input, output, session) {
  # Ocultar la pantalla de carga inicial una vez que el servidor se conecta
  # Se añade un pequeño retraso para asegurar que los elementos pesados (el mapa) empiecen a dibujar
  shinyjs::delay(1500, waiter_hide())

  # Mover los botones Todos y Ninguno dentro del grupo flexible de botones de forma nativa (para los 3 grupos)
  shinyjs::delay(100, shinyjs::runjs("
    $('#filtro_deptos_serie .btn-group').prepend($('#btn_desel_serie')).prepend($('#btn_sel_serie'));
    $('#filtro_deptos_estacional .btn-group').prepend($('#btn_desel_estacional')).prepend($('#btn_sel_estacional'));
    $('#filtro_deptos_composicion .btn-group').prepend($('#btn_desel_composicion')).prepend($('#btn_sel_composicion'));
  "))

  # Configurar Waiters para los componentes individuales (Círculo simple)
  w_mapa <- Waiter$new(id = "mapa_interactivo", html = make_waiter_html(), color = "rgba(255,255,255,0.85)")
  w_serie <- Waiter$new(id = "grafico_serie", html = make_waiter_html(), color = "rgba(255,255,255,0.85)")
  w_estacional <- Waiter$new(id = "grafico_estacional", html = make_waiter_html(), color = "rgba(255,255,255,0.85)")
  w_composicion <- Waiter$new(id = "grafico_composicion", html = make_waiter_html(), color = "rgba(255,255,255,0.85)")
  w_tabla <- Waiter$new(id = "tabla_denuncias", html = make_waiter_html(), color = "rgba(255,255,255,0.85)")

  # Valores reactivos para estado de hover
  rv <- reactiveValues(depto_hover = NULL)

  observeEvent(input$mapa_interactivo_shape_mouseover, {
    new_id <- input$mapa_interactivo_shape_mouseover$id

    if (!is.null(new_id)) {
      if (new_id == "EXTERIOR") {
        rv$depto_hover <- NULL
      } else {
        # Ahora permitimos todos los IDs (Departamentos o Municipios)
        # Si el ID es el especial de Montevideo en modo municipal, mapearlo a MONTEVIDEO para el panel
        rv$depto_hover <- if (new_id == "MONTEVIDEO_MUNI") "MONTEVIDEO" else new_id
      }
    }
  })

  observeEvent(input$mapa_interactivo_shape_mouseout, {
    # Vacío intencionalmente. Delegamos el reseteo al polígono EXTERIOR para evitar parpadeos al pasar por los ríos.
  })

  # Botones de seleccionar todos / ninguno para departamentos (SERIE)
  observeEvent(input$btn_sel_serie, {
    updateCheckboxGroupButtons(session, "filtro_deptos_serie", selected = lista_deptos)
  })
  observeEvent(input$btn_desel_serie, {
    updateCheckboxGroupButtons(session, "filtro_deptos_serie", selected = character(0))
  })

  # Botones de seleccionar todos / ninguno (ESTACIONALIDAD)
  observeEvent(input$btn_sel_estacional, {
    updateCheckboxGroupButtons(session, "filtro_deptos_estacional", selected = lista_deptos)
  })
  observeEvent(input$btn_desel_estacional, {
    updateCheckboxGroupButtons(session, "filtro_deptos_estacional", selected = character(0))
  })

  # Botones de seleccionar todos / ninguno (COMPOSICION)
  observeEvent(input$btn_sel_composicion, {
    updateCheckboxGroupButtons(session, "filtro_deptos_composicion", selected = lista_deptos)
  })
  observeEvent(input$btn_desel_composicion, {
    updateCheckboxGroupButtons(session, "filtro_deptos_composicion", selected = character(0))
  })

  # Ocultar indicador de carga cuando el mapa termina de procesarse en el cliente
  observeEvent(input$mapa_listo, {
    w_mapa$hide()
  })

  observeEvent(input$modo_vista, {
    modo <- input$modo_vista
    es_mapa <- modo %in% c("MAPA", "MAPA MUNICIPAL", "EXPEDIENTES")
    es_serie <- modo == "SERIE"
    # -- Visibilidad de contenedores --
    if (es_mapa) {
      shinyjs::show("controles_mapa")
      shinyjs::show("contenedor_mapa")
    } else {
      shinyjs::hide("controles_mapa")
      shinyjs::hide("contenedor_mapa")
    }

    if (es_serie) {
      shinyjs::show("controles_serie")
      shinyjs::show("contenedor_serie")
      shinyjs::show("panel_deptos_serie")
    } else {
      shinyjs::hide("controles_serie")
      shinyjs::hide("contenedor_serie")
      shinyjs::hide("panel_deptos_serie")
    }

    if (modo == "ESTACIONALIDAD") {
      shinyjs::show("contenedor_estacional")
      shinyjs::show("panel_deptos_estacional")
    } else {
      shinyjs::hide("contenedor_estacional")
      shinyjs::hide("panel_deptos_estacional")
    }

    if (modo == "COMPOSICION") {
      shinyjs::show("contenedor_composicion")
      shinyjs::show("panel_deptos_composicion")
    } else {
      shinyjs::hide("contenedor_composicion")
      shinyjs::hide("panel_deptos_composicion")
    }

    if (modo == "TABLA") {
      shinyjs::show("contenedor_tabla")
    } else {
      shinyjs::hide("contenedor_tabla")
    }

    # -- Anchos de columna --
    if (es_mapa) {
      shinyjs::runjs(sprintf("document.getElementById('columna_izquierda').style.width = '%s'; document.getElementById('columna_izquierda').style.flex = '0 0 %s';", COL_LEFT_PCT, COL_LEFT_PCT))
      shinyjs::runjs(sprintf("document.getElementById('columna_derecha').style.width = '%s'; document.getElementById('columna_derecha').style.flex = '0 0 %s';", COL_RIGHT_PCT, COL_RIGHT_PCT))
    } else {
      shinyjs::runjs(sprintf("document.getElementById('columna_izquierda').style.width = '%s'; document.getElementById('columna_izquierda').style.flex = '0 0 %s';", SERIES_WIDTH_PCT, SERIES_WIDTH_PCT))
      shinyjs::runjs(sprintf("document.getElementById('columna_derecha').style.width = '%s'; document.getElementById('columna_derecha').style.flex = '0 0 %s';", SERIES_RIGHT_PCT, SERIES_RIGHT_PCT))
    }
    shinyjs::delay(300, shinyjs::runjs('window.dispatchEvent(new Event("resize"));'))

    # -- Slider: restringir años en modo Expedientes --
    if (modo == "EXPEDIENTES") {
      new_val1 <- max(isolate(input$filtro_anio[1]), primer_anio_expediente)
      updateSliderInput(session, "filtro_anio",
        min   = primer_anio_expediente,
        value = c(new_val1, isolate(input$filtro_anio[2]))
      )
    } else {
      updateSliderInput(session, "filtro_anio", min = as.numeric(min(anios_str)))
    }
    # Mostrar el waiter del modo activo de forma inmediata.
    # Con req(input$modo_vista == X) en cada renderGirafe, el render SIEMPRE
    # se re-ejecuta al volver a la pestaña, por lo que on.exit(hide()) siempre corre.
    # Esto elimina la condición de carrera anterior.
    if (es_mapa) {
      w_mapa$show()
    } else if (es_serie) {
      w_serie$show()
    } else if (modo == "ESTACIONALIDAD") {
      w_estacional$show()
    } else if (modo == "COMPOSICION") {
      w_composicion$show()
    } else if (modo == "TABLA") w_tabla$show()
  })

  observeEvent(input$reset_map_zoom, {
    padding_top <- MAP_VERTICAL_OFFSET
    leafletProxy("mapa_interactivo") %>%
      fitBounds(
        lng1 = unname(deps_bb["xmin"]), lat1 = unname(deps_bb["ymin"]) - padding_top,
        lng2 = unname(deps_bb["xmax"]), lat2 = unname(deps_bb["ymax"]) - padding_top,
        options = list(padding = c(0, 0), animate = TRUE)
      )
  })

  # 1. Datos base filtrados por motivo
  datos_filtrados_motivo <- reactive({
    req(input$filtro_motivo)
    d <- df
    if (input$filtro_motivo != "Todos los motivos") d <- d %>% filter(MOTIVO_AGRUPADO == input$filtro_motivo)
    return(d)
  })

  # 2. Datos para el MAPA (Agregados por Depto)
  datos_mapa <- reactive({
    req(input$filtro_anio)
    d_base <- datos_filtrados_motivo()
    var_tooltip <- ifelse(input$filtro_motivo == "Todos los motivos", "MOTIVO_AGRUPADO", "MOTIVO")

    # Filtrar por año para el mapa
    d <- d_base %>% filter(Anio >= as.character(input$filtro_anio[1]) & Anio <= as.character(input$filtro_anio[2]))

    # Agregación por departamento
    base <- d %>%
      group_by(Depto_Limpio) %>%
      mutate(Total_Depto = n()) %>%
      group_by(Depto_Limpio, !!sym(var_tooltip)) %>%
      summarise(Cant_Sub = n(), Total_Depto = first(Total_Depto), Exp_Sub = sum(Tiene_Expediente, na.rm = T), .groups = "drop_last") %>%
      mutate(Pct_Sub = round((Cant_Sub / Total_Depto) * 100, 1)) %>%
      arrange(Depto_Limpio, desc(Pct_Sub)) %>%
      group_by(Depto_Limpio) %>%
      summarise(
        D_Tot = first(Total_Depto), E_Tot = sum(Exp_Sub),
        TooltipHTML = paste0("<div style='font-size:13px; margin-bottom:4px; line-height:1.3;'><b style='color:#002D62;'>", Pct_Sub, "%</b> - ", str_trunc(!!sym(var_tooltip), 100), "</div>", collapse = ""),
        .groups = "drop"
      )

    # Unir con mapa y asegurar que sea objeto sf
    uruguay_mapa %>%
      left_join(base, by = "Depto_Limpio") %>%
      mutate(
        D_Tot = ifelse(is.na(D_Tot), 0, D_Tot),
        E_Tot = ifelse(is.na(E_Tot), 0, E_Tot),
        TooltipHTML = ifelse(is.na(TooltipHTML), "<div style='color:#999;font-size:12px;'>0 Registros</div>", TooltipHTML)
      ) %>%
      st_as_sf()
  })

  # 2.5 Datos para el MAPA MUNICIPAL
  datos_municipios <- reactive({
    req(input$filtro_anio)
    d_base <- datos_filtrados_motivo()
    var_tooltip <- ifelse(input$filtro_motivo == "Todos los motivos", "MOTIVO_AGRUPADO", "MOTIVO")

    # Filtrar por año
    d <- d_base %>% filter(Anio >= as.character(input$filtro_anio[1]) & Anio <= as.character(input$filtro_anio[2]))

    # Agregación por municipio
    base <- d %>%
      group_by(Muni_Limpio) %>%
      mutate(Total_Muni = n()) %>%
      group_by(Muni_Limpio, !!sym(var_tooltip)) %>%
      summarise(Cant_Sub = n(), Total_Muni = first(Total_Muni), Exp_Sub = sum(Tiene_Expediente, na.rm = T), .groups = "drop_last") %>%
      mutate(Pct_Sub = round((Cant_Sub / Total_Muni) * 100, 1)) %>%
      arrange(Muni_Limpio, desc(Pct_Sub)) %>%
      group_by(Muni_Limpio) %>%
      summarise(
        D_Tot = first(Total_Muni), E_Tot = sum(Exp_Sub),
        TooltipHTML = paste0("<div style='font-size:13px; margin-bottom:4px; line-height:1.3;'><b style='color:#002D62;'>", Pct_Sub, "%</b> - ", str_trunc(!!sym(var_tooltip), 100), "</div>", collapse = ""),
        .groups = "drop"
      )

    # Unir con todos los municipios
    uruguay_municipios %>%
      left_join(base, by = "Muni_Limpio") %>%
      mutate(
        D_Tot = ifelse(is.na(D_Tot), 0, D_Tot),
        E_Tot = ifelse(is.na(E_Tot), 0, E_Tot),
        TooltipHTML = ifelse(is.na(TooltipHTML), "<div style='color:#999;font-size:12px;'>0 Registros</div>", TooltipHTML)
      ) %>%
      st_as_sf()
  })

  # 2.6 Datos para el "RESTO" de cada departamento (No municipalizados + Indeterminados)
  datos_restos <- reactive({
    req(input$filtro_anio)
    d_base <- datos_filtrados_motivo()
    var_tooltip <- ifelse(input$filtro_motivo == "Todos los motivos", "MOTIVO_AGRUPADO", "MOTIVO")

    # Filtrar por año
    d <- d_base %>% filter(Anio >= as.character(input$filtro_anio[1]) & Anio <= as.character(input$filtro_anio[2]))

    # Identificar nombres de municipios reales para filtrar
    munis_reales <- unique(uruguay_municipios$Muni_Limpio)

    # Agregación por Departamento para aquellos registros que NO son un municipio real
    base <- d %>%
      filter(!(Muni_Limpio %in% munis_reales)) %>%
      group_by(Depto_Limpio) %>%
      mutate(Total_Resto = n()) %>%
      group_by(Depto_Limpio, !!sym(var_tooltip)) %>%
      summarise(Cant_Sub = n(), Total_Resto = first(Total_Resto), Exp_Sub = sum(Tiene_Expediente, na.rm = T), .groups = "drop_last") %>%
      mutate(Pct_Sub = round((Cant_Sub / Total_Resto) * 100, 1)) %>%
      arrange(Depto_Limpio, desc(Pct_Sub)) %>%
      group_by(Depto_Limpio) %>%
      summarise(
        D_Tot = first(Total_Resto), E_Tot = sum(Exp_Sub),
        TooltipHTML = paste0("<div style='font-size:13px; margin-bottom:4px; line-height:1.3;'><b style='color:#002D62;'>", Pct_Sub, "%</b> - ", str_trunc(!!sym(var_tooltip), 100), "</div>", collapse = ""),
        .groups = "drop"
      )

    return(base)
  })

  # 2.7 Datos para el mapa de EXPEDIENTES (% con expediente por departamento)
  datos_expedientes <- reactive({
    req(input$filtro_anio)
    d_base <- datos_filtrados_motivo()
    d <- d_base %>%
      filter(Anio >= as.character(input$filtro_anio[1]) & Anio <= as.character(input$filtro_anio[2]))

    base <- d %>%
      group_by(Depto_Limpio) %>%
      summarise(
        D_Tot   = n(),
        E_Tot   = sum(Tiene_Expediente, na.rm = TRUE),
        Pct_Exp = round((sum(Tiene_Expediente, na.rm = TRUE) / n()) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        TooltipHTML = paste0(
          "<div style='font-size:13px; margin-bottom:4px; line-height:1.3;'>",
          "<b style='color:#bf360c;'>", Pct_Exp, "%</b> con expediente (",
          E_Tot, " de ", D_Tot, " denuncias)</div>"
        )
      )

    uruguay_mapa %>%
      left_join(base, by = "Depto_Limpio") %>%
      mutate(
        D_Tot = ifelse(is.na(D_Tot), 0, D_Tot),
        E_Tot = ifelse(is.na(E_Tot), 0, E_Tot),
        Pct_Exp = ifelse(is.na(Pct_Exp), 0, Pct_Exp),
        TooltipHTML = ifelse(is.na(TooltipHTML),
          "<div style='color:#999;font-size:12px;'>0 Registros</div>", TooltipHTML
        )
      ) %>%
      st_as_sf()
  })

  # 3. Datos para la SERIE (Agregados por Depto y Año)
  datos_serie <- reactive({
    req(input$filtro_formato_serie)
    d_base <- datos_filtrados_motivo()
    var_tooltip <- ifelse(input$filtro_motivo == "Todos los motivos", "MOTIVO_AGRUPADO", "MOTIVO")

    # Filtrar por departamentos seleccionados
    if (is.null(input$filtro_deptos_serie) || length(input$filtro_deptos_serie) == 0) {
      return(data.frame())
    } else {
      d_base <- d_base %>% filter(Depto_Limpio %in% input$filtro_deptos_serie)
    }

    base_tiempo <- d_base %>%
      group_by(Depto_Limpio, Anio) %>%
      mutate(T_Anio = n()) %>%
      group_by(Depto_Limpio, Anio, !!sym(var_tooltip)) %>%
      summarise(Cant_Sub = n(), T_Anio = first(T_Anio), Exp_Sub = sum(Tiene_Expediente, na.rm = T), .groups = "drop_last") %>%
      mutate(Pct_Sub = round((Cant_Sub / T_Anio) * 100, 1)) %>%
      arrange(Anio, Depto_Limpio, desc(Pct_Sub)) %>%
      group_by(Depto_Limpio, Anio) %>%
      summarise(D_Tot = first(T_Anio), E_Tot = sum(Exp_Sub), .groups = "drop") %>%
      tidyr::complete(Depto_Limpio = input$filtro_deptos_serie, Anio = anios_str, fill = list(D_Tot = 0, E_Tot = 0))

    if (length(input$filtro_deptos_serie) == length(lista_deptos)) {
      base_tiempo <- base_tiempo %>%
        group_by(Anio) %>%
        summarise(
          D_Tot = sum(D_Tot, na.rm = TRUE),
          E_Tot = sum(E_Tot, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Depto_Limpio = "Todos")
    }

    # Calcular valor de eje Y según selección (Capa fija en Denuncias)
    v_formato <- input$filtro_formato_serie

    if (v_formato == "CANTIDAD") {
      base_tiempo <- base_tiempo %>%
        mutate(Y_Val = D_Tot) %>%
        mutate(Tooltip = paste0("<div style='padding:5px;'><h4>", Depto_Limpio, " (", Anio, ")</h4>Denuncias: <b>", D_Tot, "</b><br>Expedientes: <b>", E_Tot, "</b></div>"))
    } else {
      # Calcular porcentaje en base al total de denuncias DE LOS DEPARTAMENTOS SELECCIONADOS en ese año
      base_tiempo <- base_tiempo %>%
        group_by(Anio) %>%
        mutate(Total_Anio_Sel = sum(D_Tot, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Y_Val = round((D_Tot / Total_Anio_Sel) * 100, 1)) %>%
        mutate(Tooltip = paste0("<div style='padding:5px;'><h4>", Depto_Limpio, " (", Anio, ")</h4>Denuncias: <b>", D_Tot, "</b><br>Porcentaje: <b>", Y_Val, "%</b></div>"))
    }
    return(base_tiempo)
  })

  crear_mapa_base <- function() {
    # Inicialización estática básica del mapa para mejorar carga inicial y estabilidad
    leaflet(options = leafletOptions(zoomControl = FALSE, dragging = TRUE, scrollWheelZoom = TRUE)) %>%
      fitBounds(
        lng1 = unname(deps_bb["xmin"]), lat1 = unname(deps_bb["ymin"]) - MAP_VERTICAL_OFFSET,
        lng2 = unname(deps_bb["xmax"]), lat2 = unname(deps_bb["ymax"]) - MAP_VERTICAL_OFFSET,
        options = list(padding = c(0, 0), animate = FALSE)
      ) %>%
      # Capa EXTERIOR invisible enorme para interceptar cuando el mouse sale de Uruguay
      addRectangles(
        layerId = "EXTERIOR", lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90,
        fillColor = "transparent", stroke = FALSE
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          var svg = el.querySelector('svg');
          if (!svg) return;

          function addPatterns() {
            if (!document.getElementById('pattern-hatch')) {
              var defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
              var pattern = document.createElementNS('http://www.w3.org/2000/svg', 'pattern');
              pattern.setAttribute('id', 'pattern-hatch');
              pattern.setAttribute('patternUnits', 'userSpaceOnUse');
              pattern.setAttribute('width', '8');
              pattern.setAttribute('height', '8');
              pattern.setAttribute('patternTransform', 'rotate(45)');

              var rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
              rect.setAttribute('width', '8');
              rect.setAttribute('height', '8');
              rect.setAttribute('fill', 'white');

              var line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
              line.setAttribute('x1', '0'); line.setAttribute('y1', '0');
              line.setAttribute('x2', '0'); line.setAttribute('y2', '8');
              line.setAttribute('stroke', '#d9d9d9');
              line.setAttribute('stroke-width', '2');

              pattern.appendChild(rect);
              pattern.appendChild(line);
              defs.appendChild(pattern);
              svg.appendChild(defs);
            }
          }

          function applyHatch(layer) {
            if (layer.options && layer.options.layerId === 'MONTEVIDEO_MUNI') {
              if (layer._path) {
                layer._path.setAttribute('fill', 'url(#pattern-hatch)');
                layer._path.setAttribute('fill-opacity', '1');
              }
              // Manejo de eventos manual para mayor eficiencia
              layer.on('mouseover', function(e) {
                if(layer._path) {
                   layer._path.setAttribute('stroke', '#e67e22');
                   layer._path.setAttribute('stroke-width', '4');
                   layer.bringToFront();
                }
              });
              layer.on('mouseout', function(e) {
                if(layer._path) {
                  layer._path.setAttribute('stroke', '#000000');
                  layer._path.setAttribute('stroke-width', '0.8');
                }
              });
              layer.on('mouseover mousemove mouseout viewreset zoomend', function() {
                if(layer._path) {
                  layer._path.setAttribute('fill', 'url(#pattern-hatch)');
                  layer._path.setAttribute('fill-opacity', '1');
                }
              });
            }
          }

          addPatterns();
          // Aplicar a capas existentes e interceptar nuevas
          map.eachLayer(applyHatch);
          map.on('layeradd', function(e) { applyHatch(e.layer); });

          Shiny.setInputValue('mapa_listo', Math.random());
        }
      ")
  }

  output$mapa_interactivo <- renderLeaflet({
    crear_mapa_base()
  })

  # Actualización reactiva eficiente mediante leafletProxy
  observe({
    req(input$modo_vista %in% c("MAPA", "MAPA MUNICIPAL", "EXPEDIENTES"))
    w_mapa$show()

    # Fallback de seguridad para ocultar el waiter
    shinyjs::delay(10000, w_mapa$hide())

    is_muni <- input$modo_vista == "MAPA MUNICIPAL"
    is_exp <- input$modo_vista == "EXPEDIENTES"

    if (is_muni) {
      d <- datos_municipios()
    } else if (is_exp) {
      d <- datos_expedientes()
    } else {
      d <- datos_mapa()
    }

    centroids <- if (is_muni) uruguay_municipios_centroids else uruguay_mapa_centroids
    id_col <- if (is_muni) "Muni_Limpio" else "Depto_Limpio"

    col_motivo <- if (is_exp) "#bf360c" else colores_motivos[[input$filtro_motivo %||% "Todos los motivos"]]
    fill_var <- if (is_exp) d$Pct_Exp else d$D_Tot
    d <- d %>% mutate(FillVar = fill_var)

    d_centr <- d %>%
      st_drop_geometry() %>%
      group_by(across(all_of(id_col))) %>%
      slice(1) %>%
      ungroup() %>%
      left_join(centroids, by = id_col) %>%
      filter(!is.na(lon)) %>%
      mutate(Label = if (is_exp) paste0(FillVar, "%") else as.character(FillVar))

    d_restos_counts <- if (is_muni) datos_restos() else NULL

    if (is_muni) {
      d_depts_base <- uruguay_mapa %>%
        left_join(d_restos_counts, by = "Depto_Limpio") %>%
        mutate(
          D_Tot = ifelse(is.na(D_Tot), 0, D_Tot),
          E_Tot = ifelse(is.na(E_Tot), 0, E_Tot),
          TooltipHTML = ifelse(is.na(TooltipHTML), "<div style='color:#999;font-size:12px;'>0 Registros</div>", TooltipHTML)
        ) %>%
        filter(!Depto_Limpio %in% c("MONTEVIDEO", "CANELONES", "MALDONADO"))
    } else if (is_exp) {
      d_depts_base <- d %>% mutate(D_Tot = Pct_Exp)
    } else {
      d_depts_base <- d # Ya filtrado y simplificado en datos_mapa()
    }

    # Paleta
    if (is_exp) {
      paleta_global <- colorNumeric(palette = c("#fff3e0", "#bf360c"), domain = c(0, 100), na.color = "#d9d9d9")
    } else {
      max_global <- max(c(d$D_Tot, d_depts_base$D_Tot), na.rm = TRUE)
      paleta_global <- colorNumeric(palette = c("#f2f4f7", col_motivo), domain = c(0, max_global + 0.1), na.color = "#d9d9d9")
    }

    proxy <- leafletProxy("mapa_interactivo") %>%
      clearShapes() %>%
      clearMarkers() %>%
      # Re-añadir el EXTERIOR que borramos con clearShapes para el hover-reset
      addRectangles(
        layerId = "EXTERIOR", lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90,
        fillColor = "transparent", stroke = FALSE
      ) %>%
      addPolygons(
        data = d_depts_base, layerId = ~Depto_Limpio,
        fillColor = ~ paleta_global(D_Tot),
        fillOpacity = if (is_muni) 0.5 else 0.95,
        color = "#000000", weight = 1,
        smoothFactor = 0.5,
        highlightOptions = if (!is_muni) highlightOptions(weight = 4, color = "#e67e22", bringToFront = TRUE) else NULL
      )

    if (is_muni) {
      d_reales <- d %>% filter(Muni_Limpio != "MONTEVIDEO")
      d_mvd <- d %>% filter(Muni_Limpio == "MONTEVIDEO")

      proxy <- proxy %>%
        addPolygons(
          data = d_mvd, layerId = "MONTEVIDEO_MUNI", fillColor = ~ paleta_global(FillVar), fillOpacity = 1, color = "#000000", weight = 0.8
        ) %>%
        addPolygons(
          data = d_reales, layerId = ~Muni_Limpio, fillColor = ~ paleta_global(FillVar), fillOpacity = 1, color = "#000000", weight = 0.8,
          highlightOptions = highlightOptions(weight = 4, color = "#e67e22", bringToFront = TRUE)
        )
    }

    proxy %>%
      addLabelOnlyMarkers(
        data = d_centr, lng = ~lon, lat = ~lat, label = ~Label,
        labelOptions = labelOptions(
          noHide = TRUE, textOnly = TRUE, direction = "center",
          style = list(
            "color"       = if (is_exp) "#222222" else "#e67e22",
            "font-weight" = "bold",
            "font-size"   = "15px",
            "text-shadow" = "1px 1px 1px white"
          )
        )
      )

    # Retraso para evitar flash blanco (0.5s extra de carga solicitado por el usuario)
    shinyjs::delay(700, w_mapa$hide())
  })

  # Titulo dinamico para el grafico de Serie
  output$titulo_serie <- renderUI({
    formato <- input$filtro_formato_serie %||% "CANTIDAD"
    sufijo <- if (formato == "CANTIDAD") "Cantidad" else "Porcentaje"
    tags$div(
      style = "text-align: center; font-weight: 700; font-size: 13px; color: #333; padding: 8px 0 4px 0; background: white;",
      paste0("Serie 2010\u20132025 a nivel departamental \u2014 ", sufijo)
    )
  })

  # ----- Panel de descripción del modo activo -----
  descripciones_modo <- list(
    MAPA = list(
      titulo = "Mapa Departamental",
      color  = "#002D62",
      texto  = "Muestra la cantidad total de denuncias recibidas por departamento en el per\u00edodo seleccionado. El color de cada departamento refleja su volumen relativo. La informaci\u00f3n proviene de pedidos de informaci\u00f3n p\u00fablica efectuados al Ministerio de Ambiente. *El registro del a\u00f1o 2019 se encuentra incompleto, y falta la informaci\u00f3n de diciembre de 2025.*"
    ),
    `MAPA MUNICIPAL` = list(
      titulo = "Mapa Municipal",
      color  = "#002D62",
      texto  = "ATENCI\u00d3N: resta asignar municipio a un n\u00famero significativo de denuncias cuya informaci\u00f3n est\u00e1 incompleta. Desagrega las denuncias a nivel municipal. Las zonas rayadas (Montevideo) y los fondos semitransparentes representan \u00e1reas sin municipios formalmente constituidos. *El registro del a\u00f1o 2019 se encuentra incompleto, y falta la informaci\u00f3n de diciembre de 2025.*"
    ),
    EXPEDIENTES = list(
      titulo = "Expedientes",
      color  = "#bf360c",
      texto  = "Refleja el porcentaje de denuncias de cada departamento que tienen un n\u00famero de expediente asignado. Una cobertura alta indica mayor seguimiento administrativo. Solo se muestran los a\u00f1os con datos de expediente disponibles. *El registro del a\u00f1o 2019 se encuentra incompleto, y falta la informaci\u00f3n de diciembre de 2025.*"
    ),
    SERIE = list(
      titulo = "Serie Hist\u00f3rica",
      color  = "#002D62",
      texto  = "Compara la evoluci\u00f3n anual de denuncias entre los departamentos seleccionados. Se puede visualizar en cantidad absoluta o como porcentaje sobre el total del grupo. Ideal para identificar tendencias y departamentos con comportamiento at\u00edpico. *El registro del a\u00f1o 2019 se encuentra incompleto, y falta la informaci\u00f3n de diciembre de 2025.*"
    ),
    ESTACIONALIDAD = list(
      titulo = "Estacionalidad",
      color  = "#002D62",
      texto  = "Revela si hay meses del a\u00f1o con mayor concentraci\u00f3n de denuncias. El mapa de calor muestra cada celda mes\u00d7a\u00f1o: a mayor intensidad de color, mayor actividad. El filtro de motivo permite explorar patrones estacionales por categor\u00eda. *El registro del a\u00f1o 2019 se encuentra incompleto, y falta la informaci\u00f3n de diciembre de 2025.*"
    ),
    COMPOSICION = list(
      titulo = "Composici\u00f3n",
      color  = "#002D62",
      texto  = "Muestra c\u00f3mo se distribuyen los motivos de denuncia a lo largo del tiempo. Con \u2018Todos los motivos\u2019 se visualizan las grandes categor\u00edas; al seleccionar un motivo espec\u00edfico, se despliegan sus sub-motivos internos. *El registro del a\u00f1o 2019 se encuentra incompleto, y falta la informaci\u00f3n de diciembre de 2025.*"
    )
  )

  output$panel_descripcion_modo <- renderUI({
    modo <- input$modo_vista %||% "MAPA"
    desc <- descripciones_modo[[modo]]
    if (is.null(desc)) {
      return(NULL)
    }
    div(
      style = paste0(
        "background: white; border: 1.5px dashed ", desc$color, "; ",
        "border-radius: 10px; padding: 10px 14px; margin: 0 0 10px 15px; ",
        "box-shadow: 0 2px 6px rgba(0,0,0,0.05);"
      ),
      tags$p(
        style = paste0("font-size: 0.78rem; font-weight: 800; color: ", desc$color, "; margin: 0 0 4px 0;"),
        desc$titulo
      ),
      tags$p(
        style = "font-size: 0.75rem; color: #555; margin: 0; line-height: 1.45;",
        desc$texto
      )
    )
  })

  output$panel_hover_info <- renderUI({
    hover_id <- rv$depto_hover
    is_muni <- input$modo_vista == "MAPA MUNICIPAL"
    is_exp <- input$modo_vista == "EXPEDIENTES"

    # Siempre usamos datos_mapa para el resumen nacional (es el más completo)
    d_nacional <- datos_mapa()

    # ---- Bloque EXPEDIENTES ----
    if (is_exp) {
      d_exp <- datos_expedientes()
      if (is.null(hover_id) || hover_id == "") {
        # Resumen nacional de expedientes
        d_tot_nac <- sum(d_exp$D_Tot, na.rm = TRUE)
        e_tot_nac <- sum(d_exp$E_Tot, na.rm = TRUE)
        pct_nac <- if (d_tot_nac > 0) round((e_tot_nac / d_tot_nac) * 100, 1) else 0
        ranking_exp <- d_exp %>%
          st_drop_geometry() %>%
          filter(D_Tot > 0) %>%
          arrange(desc(Pct_Exp)) %>%
          head(10) %>%
          mutate(HTML = paste0(
            "<div style='font-size:13px; margin-bottom:4px; line-height:1.3;'>",
            "<b style='color:#bf360c;'>", Pct_Exp, "%</b> — ",
            stringr::str_to_title(tolower(Depto_Limpio)), "</div>"
          )) %>%
          pull(HTML) %>%
          paste(collapse = "")
        return(HTML(paste0(
          "<div style='background:white; border:2px solid #bf360c; padding:15px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);'>",
          "<h3 style='color:#bf360c; margin-top:0;'>Resumen Nacional — Expedientes</h3>",
          "<div style='display:flex; justify-content:space-around; margin-bottom:15px; text-align:center;'>",
          "<div><span style='font-size:0.8rem; color:#666;'>DENUNCIAS</span><br><b style='font-size:1.4rem;'>", d_tot_nac, "</b></div>",
          "<div><span style='font-size:0.8rem; color:#666;'>EXPEDIENTES</span><br><b style='font-size:1.4rem;'>", e_tot_nac, "</b></div>",
          "<div><span style='font-size:0.8rem; color:#bf360c; font-weight:bold;'>% CON EXPTE.</span><br><b style='font-size:1.4rem; color:#bf360c;'>", pct_nac, "%</b></div>",
          "</div>",
          "<hr style='margin:10px 0;'>",
          "<div style='font-weight:bold; color:#555; margin-bottom:8px; font-size:0.9rem;'>Deptos. con mayor cobertura:</div>",
          ranking_exp, "</div>"
        )))
      } else {
        d_hover <- d_exp %>% filter(Depto_Limpio == hover_id)
        if (nrow(d_hover) == 0) {
          return(NULL)
        }
        return(HTML(paste0(
          "<div style='background:white; border:2px solid #bf360c; padding:15px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);'>",
          "<h3 style='color:#bf360c; margin-top:0;'>", d_hover$Depto_Limpio, "</h3>",
          "<div style='display:flex; justify-content:space-around; margin-bottom:15px; text-align:center;'>",
          "<div><span style='font-size:0.8rem; color:#666;'>DENUNCIAS</span><br><b style='font-size:1.4rem;'>", d_hover$D_Tot, "</b></div>",
          "<div><span style='font-size:0.8rem; color:#666;'>EXPEDIENTES</span><br><b style='font-size:1.4rem;'>", d_hover$E_Tot, "</b></div>",
          "<div><span style='font-size:0.8rem; color:#bf360c; font-weight:bold;'>% CON EXPTE.</span><br><b style='font-size:1.4rem; color:#bf360c;'>", d_hover$Pct_Exp, "%</b></div>",
          "</div>",
          "<hr style='margin:10px 0;'>",
          d_hover$TooltipHTML,
          "</div>"
        )))
      }
    }
    # ---- Fin bloque EXPEDIENTES ----

    if (is.null(hover_id) || hover_id == "") {
      if (is_muni) {
        return(HTML(paste0(
          "<div style='background:white; border:2px dashed #ccc; padding:20px; text-align:center; border-radius:10px; color:#999; margin-top:20px;'>",
          "<h4 style='color:#777;'>Pasá el ratón por arriba de un territorio...</h4>",
          "</div>"
        )))
      }

      # Resumen Nacional
      d_tot <- sum(d_nacional$D_Tot, na.rm = TRUE)
      e_tot <- sum(d_nacional$E_Tot, na.rm = TRUE)

      d_raw <- df %>% filter(Anio >= as.character(input$filtro_anio[1]) & Anio <= as.character(input$filtro_anio[2]))
      if (input$filtro_motivo != "Todos los motivos") d_raw <- d_raw %>% filter(MOTIVO_AGRUPADO == input$filtro_motivo)

      var_rank <- ifelse(input$filtro_motivo == "Todos los motivos", "MOTIVO_AGRUPADO", "MOTIVO")

      ranking_global <- d_raw %>%
        group_by(!!sym(var_rank)) %>%
        summarise(Cant = n(), .groups = "drop") %>%
        mutate(Pct = round((Cant / sum(Cant)) * 100, 1)) %>%
        arrange(desc(Cant)) %>%
        head(10) %>%
        mutate(HTML = paste0("<div style='font-size:13px; margin-bottom:4px; line-height:1.3;'><b style='color:#002D62;'>", Pct, "%</b> - ", str_trunc(!!sym(var_rank), 100), "</div>")) %>%
        pull(HTML) %>%
        paste(collapse = "")

      return(HTML(paste0(
        "<div style='background:white; border:2px solid #002D62; padding:15px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);'>",
        "<h3 style='color:#002D62; margin-top:0;'>Resumen Nacional</h3>",
        "<div style='display:flex; justify-content:space-around; margin-bottom:15px; text-align:center;'>",
        "<div><span style='font-size:0.8rem; color:#666;'>DENUNCIAS</span><br><b style='font-size:1.4rem;'>", d_tot, "</b></div>",
        "<div><span style='font-size:0.8rem; color:#666;'>EXPEDIENTES</span><br><b style='font-size:1.4rem;'>", e_tot, "</b></div>",
        "</div>",
        "<hr style='margin:10px 0;'>",
        "<div style='font-weight:bold; color:#555; margin-bottom:8px; font-size:0.9rem;'>Principales Motivos:</div>",
        ranking_global,
        "</div>"
      )))
    }

    # Buscar datos específicos del territorio bajo el mouse
    if (is_muni) {
      # 1. Intentar Municipio
      d_muni <- datos_municipios()
      d_hover <- d_muni %>% filter(Muni_Limpio == hover_id)

      if (nrow(d_hover) > 0) {
        titulo <- d_hover$Muni_Limpio
        v_d_tot <- d_hover$D_Tot
        v_e_tot <- d_hover$E_Tot
        v_tooltip <- d_hover$TooltipHTML
      } else {
        # 2. Intentar Departamento (Fondo) - Usamos el valor del "Resto"
        d_restos <- datos_restos()
        d_hover_restos <- d_restos %>% filter(Depto_Limpio == hover_id)

        if (nrow(d_hover_restos) > 0) {
          titulo <- paste0("<span style='font-size:0.85em;'>", toupper(hover_id), ": zonas no municipalizadas y capital departamental</span>")
          v_d_tot <- d_hover_restos$D_Tot
          v_e_tot <- d_hover_restos$E_Tot
          v_tooltip <- d_hover_restos$TooltipHTML
        } else {
          # Caso borde: si no hay denuncias en el "Resto"
          titulo <- paste0("<span style='font-size:0.85em;'>", toupper(hover_id), ": zonas no municipalizadas y capital departamental</span>")
          v_d_tot <- 0
          v_e_tot <- 0
          v_tooltip <- "<div style='color:#999;font-size:12px;'>0 Registros</div>"
        }
      }
    } else {
      # Modo Departamental
      d_hover <- d_nacional %>% filter(Depto_Limpio == hover_id)
      if (nrow(d_hover) > 0) {
        titulo <- d_hover$Depto_Limpio
        v_d_tot <- d_hover$D_Tot
        v_e_tot <- d_hover$E_Tot
        v_tooltip <- d_hover$TooltipHTML
      } else {
        return(NULL)
      }
    }

    HTML(paste0(
      "<div style='background:white; border:2px solid #002D62; padding:15px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);'>",
      "<h3 style='color:#002D62; margin-top:0;'>", titulo, "</h3>",
      "<div style='display:flex; justify-content:space-around; margin-bottom:15px; text-align:center;'>",
      "<div><span style='font-size:0.8rem; color:#666;'>DENUNCIAS</span><br><b style='font-size:1.4rem;'>", v_d_tot, "</b></div>",
      "<div><span style='font-size:0.8rem; color:#666;'>EXPEDIENTES</span><br><b style='font-size:1.4rem;'>", v_e_tot, "</b></div>",
      "</div>",
      "<hr style='margin:10px 0;'>",
      "<div style='font-weight:bold; color:#555; margin-bottom:8px; font-size:0.9rem;'>Principales Motivos:</div>",
      v_tooltip,
      "</div>"
    ))
  })

  output$grafico_serie <- renderGirafe({
    # req() garantiza que este render solo corre en modo SERIE.
    # Tambien hace que se invalide (y re-corra) cada vez que modo_vista cambia a SERIE.
    req(input$modo_vista == "SERIE")
    if (is.null(input$filtro_deptos_serie) || length(input$filtro_deptos_serie) == 0) {
      return(NULL)
    }
    w_serie$show()
    on.exit(w_serie$hide(), add = TRUE)

    d <- datos_serie()
    if (nrow(d) == 0) {
      return(NULL)
    }
    Y_max <- max(d$Y_Val, na.rm = TRUE)

    # Textos y escalas variables según formato
    v_ylab <- ifelse(input$filtro_formato_serie == "CANTIDAD", "Cantidad de Denuncias", "Porcentaje sobre Selección (%)")
    v_margin <- ifelse(input$filtro_formato_serie == "CANTIDAD", 60, 10)

    gg <- ggplot(d, aes(x = as.numeric(Anio), y = Y_Val, group = Depto_Limpio, color = Depto_Limpio)) +
      geom_line(size = 1.5, alpha = 0.7) +
      geom_point_interactive(aes(tooltip = Tooltip, data_id = Depto_Limpio), size = 3) +
      geom_text_repel(
        data = d %>% filter(as.numeric(Anio) == max(as.numeric(Anio))),
        aes(label = Depto_Limpio), hjust = 0, nudge_x = 0.5, direction = "y", segment.color = NA, size = 3.5, color = "#888888"
      ) +
      scale_x_continuous(breaks = as.numeric(anios_str), labels = anios_str, expand = expansion(mult = c(0.05, 0.15))) +
      scale_y_continuous(limits = c(0, Y_max + v_margin)) +
      scale_color_manual(values = paleta_final_series) +
      labs(x = "Año", y = v_ylab, color = "Departamento") +
      theme_minimal() +
      theme(legend.position = "none")
    girafe(ggobj = gg, width_svg = 10, height_svg = 6.5, options = list(opts_sizing(rescale = TRUE, width = 1)))
  })

  # =========================================================
  # ESTACIONALIDAD — Heatmap mes x año
  # =========================================================
  datos_estacional <- reactive({
    meses_lbl <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    d <- datos_filtrados_motivo()
    if (!is.null(input$filtro_deptos_estacional) && length(input$filtro_deptos_estacional) > 0) {
      d <- d %>% filter(Depto_Limpio %in% input$filtro_deptos_estacional)
    }
    d <- d %>%
      filter(!is.na(`FECHA DENUNCIA`)) %>%
      mutate(
        Mes_num = lubridate::month(`FECHA DENUNCIA`),
        Mes     = factor(Mes_num, levels = 1:12, labels = meses_lbl)
      ) %>%
      group_by(Anio, Mes) %>%
      summarise(N = n(), .groups = "drop") %>%
      mutate(Tooltip = paste0("<b>", Mes, " ", Anio, "</b><br>Denuncias: <b>", N, "</b>"))

    # Completar cuadricula para que no queden huecos
    anios_sel <- anios_str
    meses_fac <- factor(meses_lbl, levels = meses_lbl)
    grid <- expand.grid(Anio = anios_sel, Mes = meses_fac, stringsAsFactors = FALSE) %>%
      mutate(Mes = factor(Mes, levels = meses_lbl))
    grid %>%
      left_join(d, by = c("Anio", "Mes")) %>%
      mutate(
        N = ifelse(is.na(N), 0L, N),
        Tooltip = ifelse(is.na(Tooltip),
          paste0("<b>", Mes, " ", Anio, "</b><br>Sin registros"), Tooltip
        )
      )
  })

  output$grafico_estacional <- renderGirafe({
    req(input$modo_vista == "ESTACIONALIDAD")
    if (is.null(input$filtro_deptos_estacional) || length(input$filtro_deptos_estacional) == 0) {
      return(NULL)
    }
    w_estacional$show()
    on.exit(w_estacional$hide(), add = TRUE)
    d <- datos_estacional()
    if (nrow(d) == 0) {
      return(NULL)
    }
    col_mot <- colores_motivos[[input$filtro_motivo %||% "Todos los motivos"]]
    gg <- ggplot(d, aes(x = Mes, y = Anio, fill = N)) +
      geom_tile_interactive(
        aes(tooltip = Tooltip, data_id = paste0(Anio, "_", Mes)),
        color = "white", linewidth = 0.5, width = 0.95, height = 0.95
      ) +
      scale_fill_gradient(low = "#f2f4f7", high = col_mot, name = "Denuncias") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid      = element_blank(),
        legend.position = "none",
        plot.margin     = margin(t = 10, r = 10, b = 5, l = 5)
      )
    girafe(
      ggobj = gg, width_svg = 10, height_svg = 8,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover(css = "stroke:black;stroke-width:1.5px;")
      )
    )
  })

  # =========================================================
  # COMPOSICION — Barras apiladas 100% por motivo y año
  # =========================================================
  datos_composicion <- reactive({
    var_comp <- ifelse(input$filtro_motivo == "Todos los motivos", "MOTIVO_AGRUPADO", "MOTIVO")
    d <- datos_filtrados_motivo()
    if (!is.null(input$filtro_deptos_composicion) && length(input$filtro_deptos_composicion) > 0) {
      d <- d %>% filter(Depto_Limpio %in% input$filtro_deptos_composicion)
    }
    d %>%
      group_by(Anio, Categoria = !!sym(var_comp)) %>%
      summarise(N = n(), .groups = "drop") %>%
      group_by(Anio) %>%
      mutate(Pct = round(N / sum(N) * 100, 1)) %>%
      ungroup() %>%
      mutate(Tooltip = paste0(
        "<b>", Categoria, "</b> (", Anio, ")<br>",
        "Denuncias: <b>", N, "</b><br>",
        "Porcentaje: <b>", Pct, "%</b>"
      ))
  })

  output$grafico_composicion <- renderGirafe({
    req(input$modo_vista == "COMPOSICION")
    if (is.null(input$filtro_deptos_composicion) || length(input$filtro_deptos_composicion) == 0) {
      return(NULL)
    }
    w_composicion$show()
    on.exit(w_composicion$hide(), add = TRUE)
    d <- datos_composicion()
    if (nrow(d) == 0) {
      return(NULL)
    }
    es_agrupado <- input$filtro_motivo == "Todos los motivos"
    gg <- ggplot(d, aes(x = Anio, y = N, fill = Categoria)) +
      geom_col_interactive(
        aes(tooltip = Tooltip, data_id = paste0(Anio, "_", Categoria)),
        position = "fill", width = 0.75
      ) +
      scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
      {
        if (es_agrupado) {
          cats <- unique(d$Categoria)
          scale_fill_manual(values = colores_motivos[cats], name = "Motivo")
        } else {
          cats <- unique(d$Categoria)
          n_cats <- length(cats)
          paleta_sub <- rep(colores_distintos_12, length.out = n_cats)
          scale_fill_manual(values = setNames(paleta_sub, cats), name = "Sub-motivo")
        }
      } +
      labs(x = "A\u00f1o", y = "Porcentaje") +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 11),
        legend.position    = "bottom",
        legend.text        = element_text(size = 8),
        legend.title       = element_text(size = 9, face = "bold"),
        panel.grid.major.x = element_blank()
      ) +
      guides(fill = guide_legend(ncol = 3, byrow = TRUE))
    girafe(
      ggobj = gg, width_svg = 11, height_svg = 6.5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover(css = "opacity:0.85;stroke:white;stroke-width:1px;")
      )
    )
  })

  # =========================================================
  # TABLA DE DENUNCIAS
  # =========================================================
  output$tabla_denuncias <- DT::renderDataTable({
    req(input$modo_vista == "TABLA")
    w_tabla$show()
    on.exit(w_tabla$hide(), add = TRUE)
    d <- datos_filtrados_motivo() %>% 
      filter(Anio >= as.character(input$filtro_anio[1]) & Anio <= as.character(input$filtro_anio[2]))
    
    # Seleccionar y renombrar columnas
    d <- d %>% select(
      Fecha = `FECHA DENUNCIA`,
      Departamento = Depto_Limpio,
      Municipio = Muni_Limpio,
      `Motivo Agrupado` = MOTIVO_AGRUPADO,
      Expediente,
      `Derivación` = Derivacion
    ) %>%
      mutate(
        Expediente = ifelse(
          Expediente != "No registra",
          paste0("<a href='https://expediente.ute.com.uy/ConsultasWebMMA/' target='_blank' style='color: blue; text-decoration: underline;'>", Expediente, "</a>"),
          Expediente
        )
      )
    
    DT::datatable(
      d,
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        pagingType = "full_numbers",
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ entradas",
          info = "Estás en página _PAGE_",
          infoEmpty = "",
          infoFiltered = "",
          paginate = list(first = "<<", previous = "<", `next` = ">", last = ">>")
        )
      ),
      rownames = FALSE,
      style = "bootstrap"
    )
  })

  # Botones de Zoom Manual
  observeEvent(input$btn_zoom_in, {
    req(input$mapa_interactivo_zoom, input$mapa_interactivo_center)
    leafletProxy("mapa_interactivo") %>%
      setView(
        lng = input$mapa_interactivo_center$lng,
        lat = input$mapa_interactivo_center$lat,
        zoom = input$mapa_interactivo_zoom + 1
      )
  })

  observeEvent(input$btn_zoom_out, {
    req(input$mapa_interactivo_zoom, input$mapa_interactivo_center)
    leafletProxy("mapa_interactivo") %>%
      setView(
        lng = input$mapa_interactivo_center$lng,
        lat = input$mapa_interactivo_center$lat,
        zoom = input$mapa_interactivo_zoom - 1
      )
  })
}
shinyApp(ui = ui, server = server)
