setwd('C:/Proyectos/Denuncias')
tryCatch({
    source('app.R')
    cat("Sourced successfully.\n")
    if (exists("uruguay_municipios_centroids")) {
        cat("Centroids OK. Num rows:", nrow(uruguay_municipios_centroids), "\n")
    }
}, error = function(e) {
    cat("Error loading app.R:\n")
    print(e)
})
