# ====================================================================
# Ejecutar UNA SOLA VEZ localmente para pre-cachear los geodatos.
# El archivo geodatos.RData resultante debe subirse junto con la app.
# ====================================================================

library(sf)
library(dplyr)
library(stringr)
library(geouy)

sf_use_s2(FALSE)

limpiar_texto <- function(x) {
    x <- toupper(str_trim(as.character(x)))
    x <- setNames(c("A", "E", "I", "O", "U", "N"), c("Á", "É", "Í", "Ó", "Ú", "Ñ")) %>% stringr::str_replace_all(x, .)
    return(x)
}

message("Descargando mapa departamental desde geouy...")
uruguay_mapa_raw <- load_geouy("Departamentos") %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON") %>%
    st_make_valid() %>%
    mutate(Depto_Limpio = limpiar_texto(nombre))

names(uruguay_mapa_raw)[names(uruguay_mapa_raw) == attr(uruguay_mapa_raw, "sf_column")] <- "geometry"
st_geometry(uruguay_mapa_raw) <- "geometry"

message("Procesando topología departamental...")
uruguay_mapa <- uruguay_mapa_raw %>%
    group_by(Depto_Limpio) %>%
    summarise(.groups = "drop") %>%
    st_make_valid() %>%
    st_buffer(10) %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    select(Depto_Limpio, geometry) %>%
    st_simplify(dTolerance = 0.0005, preserveTopology = TRUE)

message("Leyendo shapefile de municipios...")
uruguay_municipios_raw <- st_read("municipios/snd_limmun.shp", quiet = TRUE, options = "ENCODING=WINDOWS-1252") %>%
    st_make_valid() %>%
    st_transform(4326) %>%
    st_simplify(dTolerance = 0.0005, preserveTopology = TRUE) %>%
    st_cast("MULTIPOLYGON") %>%
    st_make_valid() %>%
    mutate(
        Muni_Limpio_Nom = limpiar_texto(municipio),
        Depto_Limpio    = limpiar_texto(depto),
        Muni_Limpio_Nom = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO", Muni_Limpio_Nom),
        Muni_Limpio     = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO", paste0(Muni_Limpio_Nom, " (", Depto_Limpio, ")"))
    )

message("Procesando municipios reales...")
uruguay_municipios_reales <- uruguay_municipios_raw %>%
    filter(Depto_Limpio != "MONTEVIDEO")

names(uruguay_municipios_reales)[names(uruguay_municipios_reales) == attr(uruguay_municipios_reales, "sf_column")] <- "geometry"
st_geometry(uruguay_municipios_reales) <- "geometry"

uruguay_municipios_reales <- uruguay_municipios_reales %>%
    group_by(Depto_Limpio, Muni_Limpio) %>%
    summarise(geometry = suppressWarnings(suppressMessages(st_union(geometry))), .groups = "drop") %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON")

montevideo_limpio <- uruguay_mapa %>%
    filter(Depto_Limpio == "MONTEVIDEO") %>%
    mutate(Muni_Limpio = "MONTEVIDEO") %>%
    select(Depto_Limpio, Muni_Limpio)

names(montevideo_limpio)[names(montevideo_limpio) == attr(montevideo_limpio, "sf_column")] <- "geometry"
st_geometry(montevideo_limpio) <- "geometry"

uruguay_municipios <- bind_rows(
    uruguay_municipios_reales %>% select(Depto_Limpio, Muni_Limpio),
    montevideo_limpio %>% select(Depto_Limpio, Muni_Limpio)
) %>% 
  st_as_sf() %>%
  select(Muni_Limpio, Depto_Limpio, geometry) %>%
  st_simplify(dTolerance = 0.0005, preserveTopology = TRUE)

message("Calculando bboxes y centroides...")
uruguay_mapa_bboxes <- uruguay_mapa %>%
    rowwise() %>%
    mutate(bbox = list(st_bbox(st_geometry(.)))) %>%
    ungroup()

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

message("Guardando geodatos.RData...")
save(
    uruguay_mapa,
    uruguay_municipios,
    uruguay_mapa_bboxes,
    uruguay_mapa_centroids,
    uruguay_municipios_centroids,
    deps_bb,
    file = "geodatos.RData"
)

message("✅ geodatos.RData guardado correctamente.")
