library(sf)
library(dplyr)
library(stringr)

limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "latin1", to = "UTF-8", sub = "byte")
  x <- toupper(str_trim(x))
  x <- setNames(c("A", "E", "I", "O", "U", "N"), c("Á", "É", "Í", "Ó", "Ú", "Ñ")) %>% stringr::str_replace_all(x, .)
  return(x)
}

sf_use_s2(FALSE)
uruguay_mapa <- geouy::load_geouy("Departamentos") %>% 
  st_make_valid() %>% 
  st_simplify(dTolerance = 300, preserveTopology = TRUE) %>% 
  st_transform(4326) %>% 
  st_cast("MULTIPOLYGON") %>% 
  mutate(Depto_Limpio = limpiar_texto(nombre))

uruguay_municipios <- st_read("municipios/snd_limmun.shp", quiet = TRUE, options = "ENCODING=WINDOWS-1252") %>% 
  st_make_valid() %>% 
  st_simplify(dTolerance = 0.002, preserveTopology = TRUE) %>% 
  st_transform(4326) %>% 
  st_cast("MULTIPOLYGON") %>% 
  mutate(Muni_Limpio = limpiar_texto(municipio),
         Depto_Limpio = limpiar_texto(depto))

munis_unidos <- uruguay_municipios %>% 
  group_by(Depto_Limpio) %>% 
  summarise(geom = st_union(st_geometry(.)), .groups = "drop") %>% 
  st_make_valid()

restos_mapa <- uruguay_mapa %>%
  mutate(Muni_Limpio = paste("RESTO DE", Depto_Limpio))

for(i in seq_len(nrow(restos_mapa))) {
  depto <- restos_mapa$Depto_Limpio[i]
  muni_union <- munis_unidos %>% filter(Depto_Limpio == depto)
  if(nrow(muni_union) > 0) {
    g_unida <- st_geometry(muni_union)[[1]]
    g_base <- st_geometry(restos_mapa)[[i]]
    if(!is.null(g_unida) && !is.null(g_base) && !st_is_empty(g_unida)) {
      st_geometry(restos_mapa)[[i]] <- suppressWarnings(st_difference(g_base, g_unida))
    }
  }
}

restos_mapa <- restos_mapa %>%
  st_make_valid() %>%
  select(Depto_Limpio, Muni_Limpio)

cat("Restos count: ", nrow(restos_mapa), "\n")
cat("Empty Restos count: ", sum(st_is_empty(restos_mapa)), "\n")
cat("Restos with area > 0: ", sum(st_area(restos_mapa) > units::set_units(0, "m^2")), "\n")
