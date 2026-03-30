load("planilla_limpia.RData")
obj <- ls()[ls() != "obj"]
print(names(get(obj)))
