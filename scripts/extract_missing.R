load("c:/Proyectos/Denuncias/planilla_completa.RData")
library(dplyr)
mis <- planilla_completa %>% 
  filter((MUNICIPIO == "" | is.na(MUNICIPIO)) & tolower(DEPARTAMENTO) != "montevideo") %>% 
  count(DEPARTAMENTO, LOCALIDAD, name="Frecuencia", sort=TRUE)
write.csv(mis, "c:/Proyectos/Denuncias/data/top_missing.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("Found", nrow(mis), "missing pairs. Exported to data/top_missing.csv\n")
print(head(mis, 30))
