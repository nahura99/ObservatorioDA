load("planilla_limpia.RData")
obj <- ls()[ls() != "obj"] # Encontrar el nombre del objeto cargado
cat("Objeto cargado:", obj, "\n")
df <- get(obj[1])
cat("Row count:", nrow(df), "\n")
cat("Columns:", paste(colnames(df), collapse=", "), "\n")
