source("app.R")

cat("==== CHEQUEO DE GEOMETRÍAS REPARADAS ====\n")
cat("uruguay_municipios rows: ", nrow(uruguay_municipios), "\n")
cat("Hay Restos?: ", sum(grepl("RESTO", uruguay_municipios$Muni_Limpio)), "\n")
cat("Geometrias vacias?: ", sum(st_is_empty(uruguay_municipios)), "\n")
cat("Geometrias clase sf?: ", inherits(uruguay_municipios, "sd"), "\n\n")

cat("==== CHEQUEO DE DATOS DE DENUNCIAS EN RESTOS ====\n")
cat("Denuncias totales df: ", nrow(df), "\n")
cat("Denuncias en RESTOS: ", nrow(df %>% filter(grepl("RESTO", Muni_Limpio))), "\n")
cat("Ejemplo de Muni_Limpio en RESTOS: ", head(df$Muni_Limpio[grepl("RESTO", df$Muni_Limpio)], 2), "\n\n")

cat("==== TEST MOCK FUNCION DATOS MUNICIPALES ====\n")
d <- df %>% filter(MOTIVO_AGRUPADO != "Todos los motivos" | TRUE) # Fake filter
base <- d %>% group_by(Muni_Limpio) %>% mutate(Total_Muni = n()) %>%
      group_by(Muni_Limpio, MOTIVO_AGRUPADO) %>%
      summarise(Cant_Sub=n(), Total_Muni=first(Total_Muni), Exp_Sub=sum(Tiene_Expediente, na.rm=T), .groups="drop_last") %>%
      mutate(Pct_Sub=round((Cant_Sub/Total_Muni)*100, 1)) %>% arrange(Muni_Limpio, desc(Pct_Sub)) %>%
      group_by(Muni_Limpio) %>%
      summarise(D_Tot = first(Total_Muni), E_Tot = sum(Exp_Sub),
                TooltipHTML = "TEST HTML",
                .groups = "drop")

cat("Base resumida rows: ", nrow(base), "\n")
cat("Hay Restos en base?: ", sum(grepl("RESTO", base$Muni_Limpio)), "\n")

# Unir con shapefile (simulando reactivo de app.R)
d_final <- uruguay_municipios %>% 
      left_join(base, by="Muni_Limpio") %>%
      mutate(D_Tot=ifelse(is.na(D_Tot), 0, D_Tot), 
             E_Tot=ifelse(is.na(E_Tot), 0, E_Tot),
             TooltipHTML=ifelse(is.na(TooltipHTML), "<div style='color:#999;font-size:12px;'>0 Registros</div>", TooltipHTML))

cat("Row count tras JOIN: ", nrow(d_final), "\n")
cat("Restos mapeados: ", sum(grepl("RESTO", d_final$Muni_Limpio)), "\n")
cat("Están vacíos en geometría?: ", sum(st_is_empty(d_final %>% filter(grepl("RESTO", Muni_Limpio)))) , "\n")
cat("Clases de geometría final:\n")
print(table(st_geometry_type(d_final)))
