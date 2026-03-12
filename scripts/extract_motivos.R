load("planilla_limpia.RData")
library(dplyr)

# Obtener combinaciones únicas y ordenarlas
estructura <- planilla_limpia %>%
  select(MOTIVO_AGRUPADO, MOTIVO) %>%
  distinct() %>%
  arrange(MOTIVO_AGRUPADO, MOTIVO)

# Imprimir por grupos
grupos <- split(estructura$MOTIVO, estructura$MOTIVO_AGRUPADO)

cat("\nESTRUCTURA DE MOTIVOS Y SUBMOTIVOS\n")
cat("==================================\n\n")

for (agrupado in names(grupos)) {
  cat("[", agrupado, "]\n", sep="")
  submotivos <- sort(unique(grupos[[agrupado]]))
  for (sub in submotivos) {
    cat("  - ", sub, "\n", sep="")
  }
  cat("\n")
}
