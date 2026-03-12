library(geouy)
meta <- metadata_geouy()
posibles <- meta[grep("localidad|poblado|asentamiento|censal", meta$capa, ignore.case=TRUE), c("capa", "descripcion")]
print(posibles)
