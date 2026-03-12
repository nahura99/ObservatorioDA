# auto_classify.R
library(dplyr)
library(stringr)
library(readr)

cat("Cargando locs_pendientes.csv...\n")
df <- read.csv("c:/Proyectos/Denuncias/locs_pendientes.csv", stringsAsFactors = FALSE)

# Función de limpieza
limpiar_texto <- function(x) {
  x <- tolower(str_trim(as.character(x)))
  x <- setNames(c("a", "e", "i", "o", "u", "n"), c("á", "é", "í", "ó", "ú", "ñ")) %>% stringr::str_replace_all(x, .)
  return(x)
}

cat("Iniciando clasificación basada en diccionario AI...\n")

df_clasif <- df %>%
  mutate(
    LOC_CLEAN = limpiar_texto(LOCALIDAD),
    DEP_CLEAN = limpiar_texto(DEPARTAMENTO),
    
    # Exclusión rápida de calles con números (direcciones exactas) y rutas
    # excepto si mencionan una fecha patria como 18 de julio o 25 de mayo
    ES_DIRECCION = str_detect(LOC_CLEAN, "[0-9]+") & !str_detect(LOC_CLEAN, "18 de|19 de|25 de") | str_detect(LOC_CLEAN, "ruta|km |camino|calle|esquina"),
    
    MUNICIPIO_IA = case_when(
      ES_DIRECCION ~ "",
      str_detect(LOC_CLEAN, "n/a|no aplica|sin informacion|falta informacion|varias|zona rural|no identifica|n/s|chacra|campo") ~ "",
      
      # MONTEVIDEO TYPOS (que tenían mal el departamento o estaban mal escritos)
      str_detect(LOC_CLEAN, "montevideo|motevideo|ontevideo|miontevideo|carasco|shopping") ~ "Montevideo (Capital)",
      str_detect(LOC_CLEAN, "carrasco|punta gorda|malvin|buceo|pocitos|punta carretas|centro|ciudad vieja|aguada|cordon|parque rodo|tres cruces|la blanqueada|union|maroñas|flor de maroñas|ituzaingo|piedras blancas|manga|casavalle|borro|cerrito|perez castellanos|peñarol|sayago|colon|lezica|melilla|paso de la arena|cerro|la teja|prado|belvedere|nuevo paris|santiago vazquez|atahualpa|brazo oriental|reducto|goes|jacinto vera|villa española|la comercial|villa muñoz|capurro|bella vista|arroyo seco|aguada|kramer|tres ombues") ~ "Montevideo (Barrios)",

      # CANELONES
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "salinas|marindia|mirindia|neptunia|pinamar|remanso|fortin") ~ "Salinas",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "floresta|jaureguiberry|jaugueriberry|cuchilla alta|santa lucia del este|araminda|aramindia|costa azul|bello horizonte|bello orizonte|guazuvira|guazubira|titanes|san luis|biarritz|sierras del mar|santa ana") ~ "La Floresta",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "ciudad de la costa|cuidad de la costa|solymar|sol y mar|pinar|shangrila|lagomar|san jose de carrasco|medanos|lomas|tahona|horneros|parque carrasco") ~ "Ciudad de la Costa",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "atlantida|villa argentina|estacion atlantida|city golf|las toscas|parque del plata") ~ "Atlántida / Parque del Plata",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "joanico|juanico|villa juanico|canelon chico|las brujas|margat|canelon|canelnes|aguas corrientes") ~ "Canelones",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "pando|totoral|empalme olmos|epalme|olmos") ~ "Pando",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "sauce|cuchilla de rocha") ~ "Sauce",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "toledo") ~ "Toledo",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "san ramon") ~ "San Ramón",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "san jacinto") ~ "San Jacinto",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "tala") ~ "Tala",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "miguez|migues") ~ "Migues",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "paso carrasco") ~ "Paso Carrasco",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "san antonio") ~ "San Antonio",

      # MALDONADO
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "san carlos|buenos aires|manantiales|manatiales|el chorro|balneario santa monica|jose ignacio|la barra|edén|fasano|punta piedras|bikini|la juanita") ~ "San Carlos",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "piriapolis|pirapolis|playa verde|playa hermosa|playa grande|punta colorada|san francisco|punta negra|chihuahua|chiuhahua|chiguhahua|chihuaua|ocean park|sauce de portezuelo|sierras de las animas|capuera") ~ "Piriápolis",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "maldonado|punta ballena|sauce|portezuelo|solanas|laguna del diario|las delicias|pinares") ~ "Maldonado",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "punta del este|mansa|brava|san rafael|gorriti|lobos|montoya") ~ "Punta del Este",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "pan de azucar|gerona|nueva carrara") ~ "Pan de Azúcar",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "solis|cerros azules|las flores|bella vista|gregorio aznarez|estacion solis|arrayanes|matadero de solis") ~ "Solís Grande",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "garzon|jose ignacio") ~ "Garzón",

      # ROCHA
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "chuy|barra chuy|la coronilla|san miguel") ~ "Chuy",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "castillos|aguas dulces|valizas|balizas|cabo polonio|oceania|esmeralda|santa teresa") ~ "Castillos",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "la paloma|la pedrera|antoniades|arachania|archania|antoniopolis|costa azul|aguada|punta rubia|roca y mar") ~ "La Paloma",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "lascano|lescano") ~ "Lascano",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "velazquez") ~ "Velázquez",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "punta del diablo") ~ "Punta del Diablo",
      
      # COLONIA
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "carmelo|cerro carmelo|estrella|zagarzazu") ~ "Carmelo",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "conchillas|puerto conchillas|campana|radial|ombues|cuatro piedras") ~ "Ombúes de Lavalle",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "rosario|britopolis|fomento|santa regina|blanca arena|brisa|piamonteses|la paz|blancarena") ~ "Rosario / La Paz",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "colonia del sacramento|colonia|riachuelo|ferrando|el cano|artilleros|san pedro") ~ "Colonia del Sacramento",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "nueva palmira|nueva plamina") ~ "Nueva Palmira",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "tarariras") ~ "Tarariras",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "florencio sanchez") ~ "Florencio Sánchez",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "juan lacaze") ~ "Juan Lacaze",

      # SAN JOSE
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "ciudad del plata|cuidad del plata|delta del tigre|autodromo|playa pascual|penino") ~ "Ciudad del Plata",
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "libertad|kiyu|colonia wilson|arazati|bocas del cufre|cufre") ~ "Libertad",
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "san jose|san jjose|juan soler|boyada|rafael peraza|raafel perazza|mal abrigo") ~ "San José de Mayo",
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "ecilda poullier") ~ "Ecilda Paullier",

      # CERRO LARGO
      DEP_CLEAN == "cerro largo" & str_detect(LOC_CLEAN, "melo|cerro largo|esperanza|fraile muerto") ~ "Melo",
      DEP_CLEAN == "cerro largo" & str_detect(LOC_CLEAN, "rio branco|lago merin|laguna merin|placido rosas|getulio|centurion") ~ "Río Branco",

      # SORIANO
      DEP_CLEAN == "soriano" & str_detect(LOC_CLEAN, "mercedes|soriano|marcedes") ~ "Mercedes",
      DEP_CLEAN == "soriano" & str_detect(LOC_CLEAN, "dolores|dolares|villa soriano|concordia|agraciada|castellanos") ~ "Dolores",

      # PAYSANDU
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "paysandu|payandu|paysansu") ~ "Paysandú",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "guichon|almiron|termas|esperanza|casablanca|porvenir|san felix") ~ "Guichón / Porvenir",
      
      # SALTO
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "salto|dayman") ~ "Salto",
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "arapey|itapebi") ~ "Colonia Lavalleja / Arapey",
      
      # FLORIDA
      DEP_CLEAN == "florida" & str_detect(LOC_CLEAN, "florida|mendoza|chamizo|berrondo|cruz|macana") ~ "Florida",
      
      # LAVALLEJA
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "minas|villa serrana|villa sera|villa serana|penitente|arequita|minuano|lavalleja|barriga negra|campanero|piraraja|zapican") ~ "Minas",
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "solis de mataojo") ~ "Solís de Mataojo",
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "jose pedro varela") ~ "José Pedro Varela",

      # FLORES
      DEP_CLEAN == "flores" & str_detect(LOC_CLEAN, "flores|trinidad") ~ "Trinidad",

      # DURAZNO
      DEP_CLEAN == "durazno" & str_detect(LOC_CLEAN, "durazno|druazno|santa bernardina|santa bernandina|centenario|baygorria|aguas buenas") ~ "Durazno",

      # RIO NEGRO
      DEP_CLEAN == "rio negro" & str_detect(LOC_CLEAN, "rio negro|fray bentos|las cañas|esteros|berlin|san javier") ~ "Fray Bentos / Río Negro",

      # TREINTA Y TRES
      DEP_CLEAN == "treinta y tres" & str_detect(LOC_CLEAN, "treinta|trienta|cuervos|vergara|cerro chato") ~ "Treinta y Tres",

      # RIVERA
      DEP_CLEAN == "rivera" & str_detect(LOC_CLEAN, "rivera|lunarejo|tranqueras|curticeiras|amarillo") ~ "Rivera",
      
      # TACUAREMBO
      DEP_CLEAN == "tacuarembo" & str_detect(LOC_CLEAN, "tacuarembo|tacarembo|paso de los toros|bonete|ansina|tambores|san gregorio") ~ "Tacuarembó",

      # ARTIGAS
      DEP_CLEAN == "artigas" & str_detect(LOC_CLEAN, "artigas|pintadito|franquia|bella union") ~ "Artigas",
      
      TRUE ~ ""
    )
  )

df_logrados <- df_clasif %>% filter(MUNICIPIO_IA != "")

cat(sprintf("=> LOGRADOS: %d de %d localidades pendientes (%.1f%%).\n", nrow(df_logrados), nrow(df), (nrow(df_logrados)/nrow(df))*100))

# Ahora cruzamos esto con la base original
cat("\nAplicando cruce con planilla_completa.RData...\n")
load("c:/Proyectos/Denuncias/planilla_completa.RData")

planilla_completa <- planilla_completa %>%
  left_join(
    df_clasif %>% select(DEPARTAMENTO, LOCALIDAD, MUNICIPIO_IA), 
    by = c("DEPARTAMENTO", "LOCALIDAD")
  ) %>%
  mutate(
    MUNICIPIO = case_when(
      MUNICIPIO != "" ~ MUNICIPIO,
      !is.na(MUNICIPIO_IA) & MUNICIPIO_IA != "" ~ MUNICIPIO_IA,
      TRUE ~ MUNICIPIO
    )
  ) %>%
  select(-MUNICIPIO_IA)

totales <- table(planilla_completa$MUNICIPIO == "")
cat(sprintf("Registros finales con MUNICIPIO asignado: %d\n", totales["FALSE"]))
cat(sprintf("Registros finales con MUNICIPIO VACÍO: %d\n", totales["TRUE"]))

save(planilla_completa, file = "c:/Proyectos/Denuncias/planilla_completa.RData")
cat("Guardado completado exitosamente.\n")
