# ====================================================================
# Ejecutar UNA SOLA VEZ localmente para limpiar la planilla original.
# El archivo data/df_app.rds resultante será el usado por app.R.
# ====================================================================

library(dplyr)
library(stringr)
library(lubridate)

message("Cargando planilla_limpia.RData...")
if (file.exists("planilla_limpia.RData")) {
  load("planilla_limpia.RData")
  df <- planilla_limpia
} else {
  stop("El archivo planilla_limpia.RData no se encuentra.")
}

limpiar_texto <- function(x) {
  x <- toupper(str_trim(as.character(x)))
  x <- setNames(c("A", "E", "I", "O", "U", "N"), c("Á", "É", "Í", "Ó", "Ú", "Ñ")) %>% stringr::str_replace_all(x, .)
  return(x)
}

message("Limpiando textos y asignando categorías...")
df <- df %>%
  mutate(
    Anio = as.character(year(`FECHA DENUNCIA`)),
    MOTIVO = ifelse(is.na(MOTIVO) | trimws(MOTIVO) == "", "Sin especificar", str_trim(MOTIVO)),
    MOTIVO_AGRUPADO = case_when(
      # Prioridad: Si el submotiivo es "Otros", el grupo es "Otros"
      MOTIVO == "Otros" ~ "Otros",
      trimws(MOTIVO_AGRUPADO) == "Ruido" ~ "Ruidos",
      trimws(MOTIVO_AGRUPADO) == "Fauna" ~ "Fauna y Biodiversidad",
      trimws(MOTIVO_AGRUPADO) == "Agroquímicos" ~ "Agroquímicos y Sustancias Peligrosas",
      trimws(MOTIVO_AGRUPADO) == "Faja de Costas" ~ "Costa y Áreas Protegidas",
      trimws(MOTIVO_AGRUPADO) == "Minería y Suelos" ~ "Minería",
      # Reglas por submotivo específico
      MOTIVO %in% c(
        "Incumplimiento de la faja de amortiguación en la Laguna del Sauce",
        "Incumplimiento de la faja de amortiguación en el Río Santa Lucía"
      ) ~ "Costa y Áreas Protegidas",
      trimws(MOTIVO_AGRUPADO) == "Residuos Sólidos y Suelos" ~ "Gestión de Residuos",
      # Reclasificación desde el grupo "Otros"
      (trimws(MOTIVO_AGRUPADO) == "Otros" | is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "") &
        MOTIVO == "Incumplimiento de autorizaciones" ~ "Gestión Institucional y Obras",
      (trimws(MOTIVO_AGRUPADO) == "Otros" | is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "") &
        MOTIVO == "Sustancias peligrosas" ~ "Agroquímicos y Sustancias Peligrosas",
      (trimws(MOTIVO_AGRUPADO) == "Otros" | is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "") &
        MOTIVO == "Actividades en áreas protegidas" ~ "Costa y Áreas Protegidas",
      # Mover Olores de Aire a su propia categoría
      trimws(MOTIVO_AGRUPADO) == "Aire y Emisiones" & str_detect(MOTIVO, "Olores") ~ "Olores",
      # Limpiar nombres de grupos sobrantes
      trimws(MOTIVO_AGRUPADO) == "Gestión Institucional, Obras y Otros" ~ "Gestión Institucional y Obras",
      is.na(MOTIVO_AGRUPADO) | trimws(MOTIVO_AGRUPADO) == "" ~ "Otros",
      TRUE ~ str_trim(MOTIVO_AGRUPADO)
    ),
    Depto_Limpio = limpiar_texto(DEPARTAMENTO),
    Muni_Limpio_Nom = limpiar_texto(MUNICIPIO),
    Muni_Limpio_Nom = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO", Muni_Limpio_Nom),
    Tiene_Expediente = as.integer(!is.na(`N° EXPEDIENTE`) & str_trim(`N° EXPEDIENTE`) != "")
  ) %>%
  filter(!is.na(Anio)) %>%
  mutate(
    MOTIVO = case_when(
      MOTIVO == "Riesgos ambientales por almacenamiento y fraccionamiento de sustancias químicas y/o derrames en la vía pública" ~ "Almacenar y fraccionar sustancias químicas y/o derrames en la vía pública",
      MOTIVO == "Vertidos de efluentes de locales comerciales, residencias" ~ "Vertidos de efluentes de locales comerciales y residencias",
      MOTIVO == "Contaminación cuerpo de agua" ~ "Contaminación de cuerpos de agua",
      MOTIVO == "Emisiones de chimenea hoteles, residencias, comercios" ~ "Emisiones de chimenea de hoteles, residencias y comercios",
      MOTIVO == "Faja de defensas de costas" ~ "Faja de defensa de costas",
      MOTIVO %in% c("Actividades en áreas protegidas no autorizadas", "Actividades en áreas protegidas") ~ "Actividades no autorizadas en áreas protegidas",
      MOTIVO %in% c("Incumplimiento de la faja de amortiguación en la Laguna del Sauce", "Incumplimiento de la faja de amortiguación en el Río Santa Lucía") ~ "Incumplimiento de la faja de amortiguación de ríos y lagunas",
      MOTIVO == "Fauna" ~ "Fauna (general)",
      MOTIVO == "Residuos" ~ "Residuos (general)",
      MOTIVO == "Extracción de minerales" ~ "Extracción de minerales (general)",
      MOTIVO == "Olores" ~ "Olores (general)",
      MOTIVO == "Olores molestos provenientes de comercios, residencias" ~ "Olores molestos provenientes de comercios y residencias",
      MOTIVO == "Ruido" ~ "Ruido (general)",
      MOTIVO == "Otros" ~ "Sin especificar",
      TRUE ~ MOTIVO
    )
  )

message("Limpiando municipios...")
df <- df %>%
  mutate(Muni_Limpio = ifelse(Depto_Limpio == "MONTEVIDEO", "MONTEVIDEO",
    ifelse(is.na(Muni_Limpio_Nom) | Muni_Limpio_Nom == "" | Muni_Limpio_Nom == "TERRITORIO NO MUNICIPALIZADO",
      paste("RESTO DE", Depto_Limpio),
      paste0(Muni_Limpio_Nom, " (", Depto_Limpio, ")")
    )
  )) %>%
  # Mantener solo columnas estrictamente necesarias para reducir memoria
  select(`FECHA DENUNCIA`, MOTIVO, MOTIVO_AGRUPADO, Depto_Limpio, Muni_Limpio, Anio, Tiene_Expediente)

if (!dir.exists("data")) {
  dir.create("data")
}

message("Guardando df_app.rds en la carpeta data...")
saveRDS(df, "data/df_app.rds")

message("✅ df_app.rds guardado exitosamente.")
