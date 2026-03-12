load("planilla_limpia.RData")
# Encontrar el nombre del objeto principal
objs <- ls()
main_obj <- objs[objs != "objs"]
cat("Object name:", main_obj, "\n")
df_test <- get(main_obj)
cat("Column names:\n")
print(colnames(df_test))
cat("\nFirst few rows:\n")
print(head(df_test))
