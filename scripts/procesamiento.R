library(dplyr)

planilla <- planilla %>%
  mutate(MOTIVO_AGRUPADO = case_when(
    # 1. Fauna y Biodiversidad
    MOTIVO %in% c("Caza, tenencia y venta ilegal de fauna", "fauna", "Fauna", 
                  "Tala de Monte Nativo y/o otras especies autóctonas") ~ "Fauna y Biodiversidad",
    
    # 2. Agua y Vertidos
    MOTIVO %in% c("Contaminación cuerpo de agua", "Mortandad de peces", "Presencia de cianobacterias", 
                  "Vertido de barométricas", "Vertido de efluentes", "Vertido de efluentes de saneamiento de MEVIR", 
                  "Vertidos de efluentes de locales comerciales, residencias", 
                  "Vertidos de efluentes de saneamiento de Cooperativas de Vivienda", 
                  "Vertidos de efluentes industriales") ~ "Agua y Vertidos",
    
    # 3. Aire y Emisiones
    MOTIVO %in% c("Emisiones al aire", "Emisiones de chimenea hoteles, residencias, comercios", 
                  "Emisiones de chimenea y polvo (material particulado) de industrias") ~ "Aire y Emisiones",
    
    # 4. Olores
    MOTIVO %in% c("olores", "Olores", "Olores molestos provenientes de comercios, residencias", 
                  "Olores molestos provenientes de industrias, saneamiento, establecimientos de engorde a corral, vertederos,gestores de residuos, otros") ~ "Olores",
    
    # 5. Ruidos
    MOTIVO %in% c("Ruido", "Ruidos de comercios y residencias. Otros ruidos", "Ruidos de industrias") ~ "Ruidos",
    
    # 6. Costa y Áreas Protegidas
    MOTIVO %in% c("Actividades en áreas protegidas", "Actividades en Áreas protegidas no autorizadas", 
                  "Afectación de humedales y/o áreas de interés ecosistémico", 
                  "Construcciones irregulares en faja de defensa de costas", "Faja de defensas de costas", 
                  "Otras actividades irregulares en faja de defensa de costas", "Vehículos en faja defensa de costa") ~ "Costa y Áreas Protegidas",
    
    # 7. Minería y Suelos
    MOTIVO %in% c("Extracción de horizontes de suelo", "Extracción de minerales", 
                  "Extracción de minerales en alvéo de curso de agua", 
                  "Extracción de minerales en canteras, faja defensa de costas, área fiscal") ~ "Minería y Suelos",
    
    # 8. Gestión de Residuos
    MOTIVO %in% c("Bolsas", "Residuos", "residuos asimilables a urbanos", "Residuos asimilables a urbanos", 
                  "Residuos industriales, especiales y otros") ~ "Gestión de Residuos",
    
    # 9. Agroquímicos y Sustancias Peligrosas
    MOTIVO %in% c("Contaminación por agroquímicos", "Envases de plaguicidas", "Sustancias peligrosas",
                  "Incumplimiento de la faja de amortiguación en el Río Santa Lucía", 
                  "Incumplimiento de la faja de amortiguación en la Laguna del Sauce", 
                  "Riesgos ambientales por almacenamiento y fraccionamiento de sustancias químicas y/o derrames en la vía pública") ~ "Agroquímicos y Sustancias Peligrosas",
    
    # 10. Gestión Institucional, Obras y Otros
    MOTIVO %in% c("Denuncias vinculadas a la construcción de la obra del Ferrocarril Central", 
                  "Incumplimiento de autorizaciones", "Otros") ~ "Gestión Institucional, Obras y Otros",
    
    TRUE ~ "Sin Categorizar" # Por si aparece algún valor nuevo en el futuro
  ))