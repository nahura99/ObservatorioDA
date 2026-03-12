# Cargar librerías necesarias
packages <- c("shiny", "bslib", "shinyWidgets", "ggplot2", "sf", "dplyr", "stringr", "lubridate", "ggiraph", "shinycssloaders", "geouy", "leaflet", "shinyjs", "htmlwidgets", "waiter", "ggrepel")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
}
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
  library(geouy)
  library(leaflet)
  library(shinyjs)
  library(htmlwidgets)
  library(waiter)
  library(ggrepel)
})
# ==========================================
# 1. PREPARACIÓN DE DATOS GLOBALES
# ==========================================
if (file.exists("planilla_limpia.RData")) {
  load("planilla_limpia.RData")
  df <- planilla_limpia
} else {
  stop("El archivo planilla_limpia.RData no se encuentra.")
}
limpiar_texto <- function(x) {
  x <- toupper(str_trim(as.character(x)))
  x <- setNames(c("A", "E", "I", "O", "U", "N"), c("Á", "É", "Í", "Ó", "Ú", "Ñ")) %>% stringr::str_replace_all(x, .)
  return(x)
}
df <- df %>%
  mutate(
    Anio = as.character(year(`FECHA DENUNCIA`)),
    MOTIVO = ifelse(is.na(MOTIVO) | trimws(MOTIVO) == "", "Sin especificar", str_trim(MOTIVO)),
    MOTIVO_AGRUPADO = case_when(
      # Prioridad: Si el submotiivo es "Otros", el grupo es "Otros"
      MOTIVO == "Otros" ~ "Otros",
      trimws(MOTIVO_AGRUPADO) == "Ruido" ~ "Ruidos",
      trimws(MOTIVO_AGRUPADO) == "Fauna" ~ "Fauna y Biodiversidad",
      trimws(MOTIVO_AGRUPADO) == "Agroquímicos" ~ "Agroquímicos y Sustancias Peligrosas",
      trimws(MOTIVO_AGRUPADO) == "Faja de Costas" ~ "Costa y Áreas Protegidas",
      trimws(MOTIVO_AGRUPADO) == "Minería y Suelos" ~ "Minería",
      # Reglas por submotivo específico
      MOTIVO %in% c(
        "Incumplimiento de la faja de amortiguación en la Laguna del Sauce",
        "Incumplimiento de la faja de amortiguación en el Río Santa Lucía"
      ) ~ "Costa y Áreas Protegidas",
      trimws(MOTIVO_AGRUPADO) == "Residuos Sólidos y Suelos" ~ "Gestión de Residuos",
      # Reclasificación desde el grupo "Otros" (captura lo que no es submotiivo 'Otros' pero estaba huerfano)
      (trimws(MOTIVO_AGRUPADO) == "Otros" | is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "") &
        MOTIVO == "Incumplimiento de autorizaciones" ~ "Gestión Institucional y Obras",
      (trimws(MOTIVO_AGRUPADO) == "Otros" | is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "") &
        MOTIVO == "Sustancias peligrosas" ~ "Agroquímicos y Sustancias Peligrosas",
      (trimws(MOTIVO_AGRUPADO) == "Otros" | is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "") &
        MOTIVO == "Actividades en áreas protegidas" ~ "Costa y Áreas Protegidas",
      # Mover Olores de Aire a su propia categoría
      trimws(MOTIVO_AGRUPADO) == "Aire y Emisiones" & str_detect(MOTIVO, "Olores") ~ "Olores",
      # Limpiar nombres de grupos sobrantes
      trimws(MOTIVO_AGRUPADO) == "Gestión Institucional, Obras y Otros" ~ "Gestión Institucional y Obras",
      is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "" ~ "Otros",
      TRUE ~ str_trim(MOTIVO_AGRUPADO)
    ),
    Depto_Limpio = limpiar_texto(DEPARTAMENTO),
    Muni_Limpio_Nom = limpiar_texto(MUNICIPIO),
    Depto_Limpio = ifelse(Muni_Limpio_Nom == "CERRO CHATO", "TREINTA Y TRES", Depto_Limpio),
    Muni_Limpio_Nom = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO", Muni_Limpio_Nom),
    Tiene_Expediente = as.integer(!is.na(`N° EXPEDIENTE`) & str_trim(`N° EXPEDIENTE`) != "")
  ) %>%
  filter(!is.na(Anio))

# Corregir vacios municipales por RESTO
df <- df %>%
  mutate(Muni_Limpio = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO",
    ifelse(is.na(Muni_Limpio_Nom) | Muni_Limpio_Nom == "" | Muni_Limpio_Nom == "TERRITORIO NO MUNICIPALIZADO",
      paste("RESTO DE", Depto_Limpio),
      paste0(Muni_Limpio_Nom, " (", Depto_Limpio, ")")
    )
  ))
anios_str <- sort(unique(df$Anio), decreasing = FALSE)
motivos_str <- sort(unique(df$MOTIVO_AGRUPADO))
opciones_motivo <- c("Todos los motivos", motivos_str)
colores_motivos <- c(
  "Todos los motivos" = "#002D62",
  "Fauna y Biodiversidad" = "#2ca02c",
  "Otros" = "#7f7f7f",
  "Agua y Vertidos" = "#1f77b4",
  "Ruidos" = "#9467bd",
  "Minería" = "#d62728",
  "Aire y Emisiones" = "#17becf",
  "Costa y Áreas Protegidas" = "#20c997",
  "Agroquímicos y Sustancias Peligrosas" = "#e31a1c",
  "Gestión de Residuos" = "#8b8b83",
  "Gestión Institucional y Obras" = "#bcbd22",
  "Olores" = "#e377c2"
)
# Obteniendo topología oficial
sf_use_s2(FALSE)
uruguay_mapa <- load_geouy("Departamentos") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 300, preserveTopology = TRUE) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  mutate(Depto_Limpio = limpiar_texto(nombre))

uruguay_municipios <- st_read("municipios/snd_limmun.shp", quiet = TRUE, options = "ENCODING=WINDOWS-1252") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.002, preserveTopology = TRUE) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  mutate(
    Muni_Limpio_Nom = limpiar_texto(municipio),
    Depto_Limpio_Orig = limpiar_texto(depto),
    Depto_Limpio = ifelse(Muni_Limpio_Nom == "CERRO CHATO", "TREINTA Y TRES", Depto_Limpio_Orig),
    Muni_Limpio_Nom = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO", Muni_Limpio_Nom),
    Muni_Limpio = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO", paste0(Muni_Limpio_Nom, " (", Depto_Limpio, ")"))
  )

# Geometría Restante (Restos de Departamento)
# Excluimos Montevideo de este cálculo ya que lo manejaremos como una sola pieza limpia al final
munis_unidos <- uruguay_municipios %>%
  filter(Depto_Limpio != "MONTEVIDEO") %>%
  group_by(Depto_Limpio_Orig) %>%
  summarise(geom = suppressWarnings(suppressMessages(st_union(st_geometry(.)))), .groups = "drop") %>%
  st_make_valid()

restos_mapa <- uruguay_mapa %>%
  filter(Depto_Limpio != "MONTEVIDEO") %>%
  mutate(Muni_Limpio = paste("RESTO DE", Depto_Limpio))

# Iterar de forma manual para evitar joins y list-columns complejos
for (i in seq_len(nrow(restos_mapa))) {
  depto <- restos_mapa$Depto_Limpio[i]
  muni_union <- munis_unidos %>% filter(Depto_Limpio_Orig == depto)
  if (nrow(muni_union) > 0) {
    g_unida <- st_make_valid(st_geometry(muni_union)[[1]])
    g_base <- st_make_valid(st_geometry(restos_mapa)[[i]])
    if (!is.null(g_unida) && !is.null(g_base) && !st_is_empty(g_unida)) {
      st_geometry(restos_mapa)[[i]] <- st_make_valid(suppressWarnings(st_difference(g_base, g_unida)))
    }
  }
}

restos_mapa <- restos_mapa %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  select(Depto_Limpio, Muni_Limpio)


# Asegurar el mismo nombre en la columna de geometría para evitar que rbind omita la forma
names(restos_mapa)[names(restos_mapa) == attr(restos_mapa, "sf_column")] <- "geometry"
st_geometry(restos_mapa) <- "geometry"

# Preparar Municipios Reales (Excluyendo Montevideo para unirlo limpio después)
uruguay_municipios_reales <- uruguay_municipios %>%
  filter(Depto_Limpio != "MONTEVIDEO")

names(uruguay_municipios_reales)[names(uruguay_municipios_reales) == attr(uruguay_municipios_reales, "sf_column")] <- "geometry"
st_geometry(uruguay_municipios_reales) <- "geometry"

uruguay_municipios_reales <- uruguay_municipios_reales %>%
  group_by(Depto_Limpio, Muni_Limpio) %>%
  summarise(geometry = suppressWarnings(suppressMessages(st_union(geometry))), .groups = "drop") %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

# Montevideo Limpio (usando la geometría del departamento directamente)
montevideo_limpio <- uruguay_mapa %>%
  filter(Depto_Limpio == "MONTEVIDEO") %>%
  mutate(Muni_Limpio = "MONTEVIDEO") %>%
  select(Depto_Limpio, Muni_Limpio)

names(montevideo_limpio)[names(montevideo_limpio) == attr(montevideo_limpio, "sf_column")] <- "geometry"
st_geometry(montevideo_limpio) <- "geometry"

# Unir todo: Municipios Reales + Zonas Resto + Montevideo Limpio
uruguay_municipios <- bind_rows(
  uruguay_municipios_reales %>% select(Depto_Limpio, Muni_Limpio),
  restos_mapa %>% select(Depto_Limpio, Muni_Limpio),
  montevideo_limpio %>% select(Depto_Limpio, Muni_Limpio)
) %>% st_as_sf()

# BBOX por departamento para zoom dinámico
uruguay_mapa_bboxes <- uruguay_mapa %>%
  rowwise() %>%
  mutate(bbox = list(st_bbox(st_geometry(.)))) %>%
  ungroup()

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
paleta_final_series <- c(colores_departamentos_vivos, colores_departamentos_suaves)

deps_bb <- sf::st_bbox(uruguay_mapa)

uruguay_mapa_centroids <- suppressWarnings(st_centroid(uruguay_mapa)) %>%
  mutate(lon = sf::st_coordinates(.)[, 1], lat = sf::st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

uruguay_municipios_centroids <- suppressWarnings(st_centroid(uruguay_municipios)) %>%
  mutate(lon = sf::st_coordinates(.)[, 1], lat = sf::st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  group_by(Muni_Limpio) %>%
  slice(1) %>%
  ungroup()
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
SERIES_HEIGHT <- "400px"

# Ancho interno de la caja blanca del gráfico de SERIE (puede ser en px o %)
# Disminuye este valor para que la tarjeta no sobre horizonalmente sino que abrace al gráfico de serie
SERIES_BOX_WIDTH <- "800px"

# Desplazamiento vertical interno del mapa
# Aumenta este valor (ej. 0.8, 1.2) para "bajar" el mapa visualmente dentro de su caja sin perder el nivel de zoom
MAP_VERTICAL_OFFSET <- 0

# Margen hacia la derecha de los botones "MAPA DEPARTAMENTAL", etc. (espacio entre el botón de Contacto y estos selectores)
POSICION_CHIPS_VISTA_IZQUIERDA <- "40px"
# Margen hacia abajo de los botones de vista (0px para subirlos, 50px para bajarlos más)
POSICION_CHIPS_VISTA_ARRIBA <- "15px"

# Margen horizontal específico para alejar/acercar el botón de "Contacto" del título principal de la izquierda
POSICION_CONTACTO_IZQUIERDA <- "110px"

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
    body, .content-wrapper, .right-side { background-color: #F4FAF6 !important; overflow-x: hidden; }
    .header-container { display: flex; align-items: center; margin-top: 5px; margin-bottom: 10px; padding: 0 10px; }
    .obs-title { font-weight: 900; letter-spacing: -0.5px; margin: 0; font-size: 2.2rem; color: #1B4332; white-space: nowrap; }

    /* Estilos para el chip de contacto y su popup */
    .contacto-wrapper {
      position: relative;
      margin-left: ", POSICION_CONTACTO_IZQUIERDA, ";
      display: inline-block;
      align-self: center;
    }
    .contacto-chip {
      background-color: #fce4ec;
      border: 1px solid #e91e63;
      border-radius: 12px;
      padding: 2px 10px;
      font-size: 0.75rem;
      font-weight: bold;
      color: #e91e63;
      cursor: pointer;
      display: flex;
      align-items: center;
      gap: 5px;
      transition: all 0.2s;
    }
    .contacto-chip:hover {
      background-color: #e91e63;
      color: white;
    }
    .contacto-popup {
      display: none;
      position: absolute;
      top: 100%;
      left: 0;
      margin-top: 10px;
      background: white;
      border: 2px solid #1B4332;
      border-radius: 10px;
      padding: 15px;
      width: 320px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.15);
      z-index: 1050;
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
    .main-selector-container { margin-left: ", POSICION_CHIPS_VISTA_IZQUIERDA, "; margin-top: ", POSICION_CHIPS_VISTA_ARRIBA, "; align-self: center; }
    .main-selector-container label.btn {
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
      text-align: center;
      height: 100%;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      width: 175px;
      flex: 0 0 175px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.02);
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
    .motivo-buttons input[value='Todos los motivos'] + label { width: 100% !important; flex: 0 0 calc(100% - 12px) !important; }
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
            if (!wrapper.contains(event.target)) {
              document.getElementById('popup_contacto').classList.remove('active');
            }
          });
        "))
      ),
      div(
        class = "main-selector-container",
        radioGroupButtons(inputId = "modo_vista", label = NULL, choices = c("Mapa Departamental" = "MAPA", "Mapa Municipal" = "MAPA MUNICIPAL", "Serie" = "SERIE"), selected = "MAPA", status = "primary")
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
          id = "controles_serie", style = "text-align: center; margin-bottom: 30px;",
          radioGroupButtons("filtro_formato_serie", choices = c("CANTIDAD", "PORCENTAJE"), selected = "CANTIDAD", status = "primary")
        )),
        div(
          id = "contenedor_mapa", style = paste0("height: ", MAP_HEIGHT, "; width: 100%; position: relative; border-radius: 12px; overflow: hidden; box-shadow: 0 4px 12px rgba(0,0,0,0.05);"),
          actionButton("reset_map_zoom", HTML("<i class='fa-solid fa-rotate-right'></i>"),
            style = "position: absolute; top: 10px; left: 10px; z-index: 1000; border-radius: 5px; padding: 6px 10px; color: #1B4332; background: white; border: 2px solid rgba(0,0,0,0.2); box-shadow: 0 1px 5px rgba(0,0,0,0.65);",
            title = "Restaurar vista de mapa"
          ),
          leafletOutput("mapa_interactivo", height = MAP_HEIGHT, width = "100%")
        ),
        shinyjs::hidden(div(
          id = "contenedor_serie", style = paste0("height: ", SERIES_HEIGHT, "; width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 0 auto; position: relative; border-radius: 12px; overflow: hidden; box-shadow: 0 4px 12px rgba(0,0,0,0.05);"),
          girafeOutput("grafico_serie", height = SERIES_HEIGHT)
        )),
        shinyjs::hidden(div(
          id = "panel_deptos_serie", class = "depto-buttons", style = paste0("width: ", SERIES_BOX_WIDTH, "; max-width: 100%; margin: 15px auto 0 auto;"),
          tags$div(style = "font-size: 11px; font-weight: bold; color: #555; text-align: center; margin-bottom: 5px;", "Departamentos"),
          actionButton("btn_sel_all_deptos", "Todos", class = "btn-depto-action"),
          actionButton("btn_desel_all_deptos", "Ninguno", class = "btn-depto-action"),
          checkboxGroupButtons("filtro_deptos", NULL, choices = lista_deptos, selected = c("CANELONES", "MALDONADO", "ROCHA", "COLONIA"))
        ))
      ),

      # Columna Derecha (Filtro de motivos + Detalle + Filtro Departamentos)
      div(
        id = "columna_derecha", style = paste0("flex: 0 0 ", COL_RIGHT_PCT, "; width: ", COL_RIGHT_PCT, "; display: flex; flex-direction: column; transition: width 0.3s ease;"),
        div(
          class = "motivo-buttons", style = "margin-bottom: 20px;",
          tags$div(style = "font-size: 11px; font-weight: bold; color: #555; text-align: center;", "Motivos"),
          radioGroupButtons("filtro_motivo", NULL, choiceNames = opciones_motivo, choiceValues = opciones_motivo, selected = opciones_motivo[1], justified = TRUE)
        ),
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

  # Mover los botones Todos y Ninguno dentro del grupo flexible de botones de forma nativa
  shinyjs::delay(100, shinyjs::runjs("$('#filtro_deptos .btn-group').prepend($('#btn_desel_all_deptos')).prepend($('#btn_sel_all_deptos'));"))

  # Configurar Waiters para los componentes individuales
  w_mapa <- Waiter$new(id = "mapa_interactivo", html = spin_3(), color = transparent(0.7))
  w_serie <- Waiter$new(id = "grafico_serie", html = spin_3(), color = transparent(0.7))

  # Valores reactivos para estado de hover
  rv <- reactiveValues(depto_hover = NULL)

  observeEvent(input$mapa_interactivo_shape_mouseover, {
    new_id <- input$mapa_interactivo_shape_mouseover$id

    if (!is.null(new_id)) {
      if (new_id == "EXTERIOR") {
        rv$depto_hover <- NULL
      } else if (new_id != "INTERIOR") {
        # Si el ID es el especial de Montevideo en modo municipal, mapearlo a MONTEVIDEO para el panel
        rv$depto_hover <- if (new_id == "MONTEVIDEO_MUNI") "MONTEVIDEO" else new_id
      }
    }
  })

  observeEvent(input$mapa_interactivo_shape_mouseout, {
    # Vacío intencionalmente. Delegamos el reseteo al polígono EXTERIOR para evitar parpadeos al pasar por los ríos.
  })

  # Botones de seleccionar todos / ninguno para departamentos
  observeEvent(input$btn_sel_all_deptos, {
    updateCheckboxGroupButtons(session, "filtro_deptos", selected = lista_deptos)
  })

  observeEvent(input$btn_desel_all_deptos, {
    updateCheckboxGroupButtons(session, "filtro_deptos", selected = character(0))
  })

  observeEvent(input$modo_vista, {
    if (input$modo_vista %in% c("MAPA", "MAPA MUNICIPAL")) {
      shinyjs::hide("controles_serie")
      shinyjs::hide("contenedor_serie")
      shinyjs::hide("panel_deptos_serie")
      shinyjs::show("controles_mapa")
      shinyjs::show("contenedor_mapa")
      shinyjs::runjs(sprintf("document.getElementById('columna_izquierda').style.width = '%s'; document.getElementById('columna_izquierda').style.flex = '0 0 %s';", COL_LEFT_PCT, COL_LEFT_PCT))
      shinyjs::runjs(sprintf("document.getElementById('columna_derecha').style.width = '%s'; document.getElementById('columna_derecha').style.flex = '0 0 %s';", COL_RIGHT_PCT, COL_RIGHT_PCT))
      shinyjs::delay(300, shinyjs::runjs('window.dispatchEvent(new Event("resize"));'))
    } else {
      shinyjs::hide("controles_mapa")
      shinyjs::hide("contenedor_mapa")
      shinyjs::show("controles_serie")
      shinyjs::show("contenedor_serie")
      shinyjs::show("panel_deptos_serie")
      shinyjs::runjs(sprintf("document.getElementById('columna_izquierda').style.width = '%s'; document.getElementById('columna_izquierda').style.flex = '0 0 %s';", SERIES_WIDTH_PCT, SERIES_WIDTH_PCT))
      shinyjs::runjs(sprintf("document.getElementById('columna_derecha').style.width = '%s'; document.getElementById('columna_derecha').style.flex = '0 0 %s';", SERIES_RIGHT_PCT, SERIES_RIGHT_PCT))
      shinyjs::delay(300, shinyjs::runjs('window.dispatchEvent(new Event("resize"));'))
    }
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

  # 3. Datos para la SERIE (Agregados por Depto y Año)
  datos_serie <- reactive({
    req(input$filtro_formato_serie)
    d_base <- datos_filtrados_motivo()
    var_tooltip <- ifelse(input$filtro_motivo == "Todos los motivos", "MOTIVO_AGRUPADO", "MOTIVO")

    # Filtrar por departamentos seleccionados
    if (is.null(input$filtro_deptos) || length(input$filtro_deptos) == 0) {
      return(data.frame())
    } else {
      d_base <- d_base %>% filter(Depto_Limpio %in% input$filtro_deptos)
    }

    base_tiempo <- d_base %>%
      group_by(Depto_Limpio, Anio) %>%
      mutate(T_Anio = n()) %>%
      group_by(Depto_Limpio, Anio, !!sym(var_tooltip)) %>%
      summarise(Cant_Sub = n(), T_Anio = first(T_Anio), Exp_Sub = sum(Tiene_Expediente, na.rm = T), .groups = "drop_last") %>%
      mutate(Pct_Sub = round((Cant_Sub / T_Anio) * 100, 1)) %>%
      arrange(Anio, Depto_Limpio, desc(Pct_Sub)) %>%
      group_by(Depto_Limpio, Anio) %>%
      summarise(D_Tot = first(T_Anio), E_Tot = sum(Exp_Sub), .groups = "drop")

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

  output$mapa_interactivo <- renderLeaflet({
    w_mapa$show()
    on.exit(
      {
        w_mapa$hide()
      },
      add = TRUE
    )

    is_muni <- input$modo_vista == "MAPA MUNICIPAL"
    d <- if (is_muni) datos_municipios() else datos_mapa()
    centroids <- if (is_muni) uruguay_municipios_centroids else uruguay_mapa_centroids
    id_col <- if (is_muni) "Muni_Limpio" else "Depto_Limpio"

    col_motivo <- colores_motivos[[input$filtro_motivo %||% "Todos los motivos"]]
    fill_var <- d$D_Tot
    d <- d %>% mutate(FillVar = fill_var)

    d_centr <- d %>%
      st_drop_geometry() %>%
      group_by(across(all_of(id_col))) %>%
      slice(1) %>%
      ungroup() %>%
      left_join(centroids, by = id_col) %>%
      filter(!is.na(lon))

    # Excluir etiquetas de Montevideo si estamos en mapa municipal (Eliminado para mostrar Montevideo)
    # if (is_muni) d_centr <- d_centr %>% filter(Muni_Limpio != "MONTEVIDEO")

    paleta <- colorNumeric(
      palette = c("#f2f4f7", col_motivo),
      domain = c(0, max(fill_var, na.rm = T) + 0.1),
      na.color = "#d9d9d9"
    )

    # Offset global configurado por el usuario
    padding_top <- MAP_VERTICAL_OFFSET

    map_out <- leaflet(options = leafletOptions(zoomControl = FALSE, dragging = TRUE, scrollWheelZoom = TRUE)) %>%
      fitBounds(
        lng1 = unname(deps_bb["xmin"]), lat1 = unname(deps_bb["ymin"]) - padding_top,
        lng2 = unname(deps_bb["xmax"]), lat2 = unname(deps_bb["ymax"]) - padding_top,
        options = list(padding = c(0, 0), animate = FALSE)
      ) %>%
      # Capa EXTERIOR invisible enorme para interceptar cuando el mouse sale de Uruguay
      addRectangles(
        layerId = "EXTERIOR", lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90,
        fillColor = "transparent", stroke = FALSE
      ) %>%
      # Capa de fondo nacional (INTERIOR)
      addPolygons(
        data = uruguay_mapa, layerId = "INTERIOR", fillColor = "#FFFDE7", fillOpacity = 0.5, color = "#000000", weight = 1,
        highlightOptions = NULL
      )

    # Separar polígonos de Resto y Polígonos Reales para desactivar el hover en los Restos
    if (is_muni) {
      # Al quitar & Muni_Limpio != "MONTEVIDEO", permitimos que aparezca en una de las dos capas
      d_reales <- d %>% filter(!grepl("RESTO DE", Muni_Limpio) & Muni_Limpio != "MONTEVIDEO")
      d_restos <- d %>% filter(grepl("RESTO DE", Muni_Limpio) & Muni_Limpio != "MONTEVIDEO")
      d_mvd <- d %>% filter(Muni_Limpio == "MONTEVIDEO")

      # 1. Restos (Fondo, sin hover)
      map_out <- map_out %>%
        addPolygons(data = d_restos, layerId = ~Muni_Limpio, fillColor = ~ paleta(FillVar), fillOpacity = 0.95, color = "#000000", weight = 0.8) %>%
        # 2. Montevideo (ID especial para hatching, con hover)
        addPolygons(
          data = d_mvd, layerId = "MONTEVIDEO_MUNI", fillColor = "white", fillOpacity = 1, color = "#000000", weight = 0.8,
          highlightOptions = highlightOptions(weight = 4, color = "#e67e22", bringToFront = TRUE)
        ) %>%
        # 3. Municipios reales (Frente, con hover)
        addPolygons(
          data = d_reales, layerId = ~Muni_Limpio, fillColor = ~ paleta(FillVar), fillOpacity = 0.95, color = "#000000", weight = 0.8,
          highlightOptions = highlightOptions(weight = 4, color = "#e67e22", bringToFront = TRUE)
        )
    } else {
      map_out <- map_out %>%
        addPolygons(
          data = d, layerId = ~Depto_Limpio, fillColor = ~ paleta(FillVar), fillOpacity = 0.95, color = "#000000", weight = 0.8,
          highlightOptions = highlightOptions(weight = 4, color = "#e67e22", bringToFront = TRUE)
        )
    }

    map_out %>%
      addLabelOnlyMarkers(
        data = d_centr, lng = ~lon, lat = ~lat, label = ~ as.character(FillVar),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center", style = list("color" = "#e67e22", "font-weight" = "bold", "font-size" = "15px", "text-shadow" = "1px 1px 1px white"))
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

          function applyHatch() {
            map.eachLayer(function(layer) {
              if (layer.options && layer.options.layerId === 'MONTEVIDEO_MUNI') {
                if (layer._path) {
                  layer._path.setAttribute('fill', 'url(#pattern-hatch)');
                  layer._path.setAttribute('fill-opacity', '1');
                }

                // Asegurar que el patrón se mantenga incluso después de eventos de Leaflet
                layer.on('mouseover mousemove mouseout viewreset zoomend', function() {
                   setTimeout(function() {
                     if(layer._path) {
                       layer._path.setAttribute('fill', 'url(#pattern-hatch)');
                       layer._path.setAttribute('fill-opacity', '1');
                     }
                   }, 10);
                });
              }
            });
          }

          addPatterns();
          applyHatch();

          // Re-aplicar cuando Leaflet redibuja los SVG (ej. al mover el mapa)
          map.on('layeradd', function() {
            applyHatch();
          });
        }
      ")
  })

  output$panel_hover_info <- renderUI({
    hover_id <- rv$depto_hover
    is_muni <- input$modo_vista == "MAPA MUNICIPAL"
    d <- if (is_muni) datos_municipios() else datos_mapa()
    id_col <- if (is_muni) "Muni_Limpio" else "Depto_Limpio"

    if (is.null(hover_id) || hover_id == "") {
      if (is_muni) {
        return(HTML(paste0(
          "<div style='background:white; border:2px dashed #ccc; padding:20px; text-align:center; border-radius:10px; color:#999; margin-top:20px;'>",
          "<h4 style='color:#777;'>Pasá el ratón por arriba de un territorio...</h4>",
          "</div>"
        )))
      }

      # Resumen (Nacional o Departamental)
      d_tot <- sum(d$D_Tot, na.rm = TRUE)
      e_tot <- sum(d$E_Tot, na.rm = TRUE)

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

    d_hover <- d %>%
      filter(.data[[id_col]] == hover_id) %>%
      slice(1)
    if (nrow(d_hover) > 0) {
      HTML(paste0(
        "<div style='background:white; border:1px solid #002D62; padding:15px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);'>",
        "<h3 style='color:#002D62; margin-top:0;'>", d_hover[[id_col]], "</h3>",
        "<div style='display:flex; justify-content:space-around; margin-bottom:15px; text-align:center;'>",
        "<div><span style='font-size:0.8rem; color:#666;'>DENUNCIAS</span><br><b style='font-size:1.4rem;'>", d_hover$D_Tot, "</b></div>",
        "<div><span style='font-size:0.8rem; color:#666;'>EXPEDIENTES</span><br><b style='font-size:1.4rem;'>", d_hover$E_Tot, "</b></div>",
        "</div>",
        "<hr style='margin:10px 0;'>",
        "<div style='font-weight:bold; color:#555; margin-bottom:8px; font-size:0.9rem;'>Principales Motivos en el dpto:</div>",
        d_hover$TooltipHTML,
        "</div>"
      ))
    }
  })

  output$grafico_serie <- renderGirafe({
    w_serie$show()
    on.exit(
      {
        w_serie$hide()
      },
      add = TRUE
    )

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
    girafe(ggobj = gg, width_svg = 10, height_svg = 5, options = list(opts_sizing(rescale = TRUE, width = 1)))
  })
}
shinyApp(ui = ui, server = server)
