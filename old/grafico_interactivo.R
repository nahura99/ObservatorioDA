# Instalar librerías necesarias si no están
options(repos = c(CRAN = "https://cloud.r-project.org"))
required_packages <- c("openxlsx", "dplyr", "sf", "leaflet", "htmlwidgets", "stringr", "RColorBrewer")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste("Instalando", pkg, "...\n"))
    install.packages(pkg, quiet = TRUE)
  }
}

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(sf)
  library(leaflet)
  library(htmlwidgets)
  library(stringr)
  library(RColorBrewer)
})

cat("Leyendo planilla Excel...\n")
file_path <- "planilla.xlsx"

df <- read.xlsx(file_path)
cols <- toupper(names(df))

motivo_col <- names(df)[grepl("MOTIVO", cols)][1]
dpto_col <- names(df)[grepl("DEPARTAMENTO|DPTO", cols)][1]

if (is.na(motivo_col) || is.na(dpto_col)) {
  cat("\nERROR: No se pudo identificar correctamente la columna de Motivo o Departamento.\n")
  quit(status = 1)
}

# Limpiar los nombres de departamentos para hacer join con GeoJSON
# (Normalizar: a mayúsculas, sacar tildes)
limpiar_texto <- function(x) {
  x <- toupper(str_trim(as.character(x)))
  x <- setNames(
    c("A", "E", "I", "O", "U", "N"),
    c("Á", "É", "Í", "Ó", "Ú", "Ñ")
  ) %>%  stringr::str_replace_all(x, .)
  return(x)
}

# Agrupar datos por departamento y motivo, y calcular porcentajes
cat("Calculando porcentajes por motivo y departamento...\n")
df_agrupado <- df %>%
  filter(!is.na(!!sym(motivo_col)) & trimws(!!sym(motivo_col)) != "" & 
         !is.na(!!sym(dpto_col)) & trimws(!!sym(dpto_col)) != "") %>%
  mutate(Depto_Limpio = limpiar_texto(!!sym(dpto_col)),
         Motivo = str_trim(!!sym(motivo_col))) %>%
  group_by(Depto_Limpio, Motivo) %>%
  summarise(Cantidad = n(), .groups = "drop_last") %>%
  mutate(Total_Depto = sum(Cantidad),
         Porcentaje = round((Cantidad / Total_Depto) * 100, 1)) %>%
  ungroup()

# Agrupar motivos en un texto único para el popup del mapa, listando primero los de mayor porcentaje
df_resumen <- df_agrupado %>%
  arrange(Depto_Limpio, desc(Porcentaje)) %>%
  group_by(Depto_Limpio) %>%
  summarise(
    Total_Denuncias = first(Total_Depto),
    Detalles_Motivos = paste0("<b>", Motivo, "</b>: ", Porcentaje, "% (", Cantidad, ")", collapse = "<br>")
  )

cat("Descargando mapa GeoJSON de Uruguay...\n")
# URL de GeoJSON con los departamentos de Uruguay
geojson_url <- "https://raw.githubusercontent.com/alotropico/uruguay.geo/master/uruguay.geojson"
uruguay_mapa <- st_read(geojson_url, quiet = TRUE)

# Limpiar la columna NAME_1 del geojson de Uruguay para que coincida con nuestros datos
uruguay_mapa <- uruguay_mapa %>%
  mutate(Depto_Limpio = limpiar_texto(NAME_1))

# Unir datos espaciales con nuestro resumen
mapa_datos <- uruguay_mapa %>%
  left_join(df_resumen, by = "Depto_Limpio") %>%
  mutate(Total_Denuncias = ifelse(is.na(Total_Denuncias), 0, Total_Denuncias),
         Detalles_Motivos = ifelse(is.na(Detalles_Motivos), "Sin denuncias", Detalles_Motivos))

cat("Generando el mapa interactivo...\n")
# Crear paleta de colores basada en el total de denuncias relativas
pal <- colorNumeric(palette = "YlOrRd", domain = mapa_datos$Total_Denuncias)

labs <- lapply(seq(nrow(mapa_datos)), function(i) {
  htmltools::HTML(paste0(
    "<h4 style='margin-bottom:3px'>", mapa_datos$NAME_1[i], "</h4>",
    "<strong>Total Denuncias: </strong>", mapa_datos$Total_Denuncias[i], "<br><hr style='margin: 4px 0;'>",
    mapa_datos$Detalles_Motivos[i]
  ))
})

mapa <- leaflet(mapa_datos) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(Total_Denuncias),
    weight = 1.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.9,
      bringToFront = TRUE),
    label = labs,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3x 8px"),
      textsize = "13px",
      direction = "auto")
  ) %>%
  addLegend(pal = pal, values = ~Total_Denuncias, opacity = 0.7, title = "Cant. Total Denuncias",
    position = "bottomright")

out_html <- "mapa_denuncias.html"
saveWidget(mapa, out_html, selfcontained = FALSE)

cat("==================================================\n")
cat(" MAPA GENERADO EXITOSAMENTE:\n", out_html, "\n")
cat("==================================================\n")
