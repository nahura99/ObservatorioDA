# Script para agregar la variable MUNICIPIO a planilla_completa.RData
# Solo mapea casos obvios y deja vacíos (NA o "") los ambiguos.

library(dplyr)
library(stringr)

cat("Cargando planilla_completa.RData...\n")
load("c:/Proyectos/Denuncias/planilla_completa.RData")

# Función auxiliar para limpieza de strings
limpiar_texto <- function(x) {
  x <- tolower(str_trim(as.character(x)))
  x <- setNames(c("a", "e", "i", "o", "u", "n"), c("á", "é", "í", "ó", "ú", "ñ")) %>% stringr::str_replace_all(x, .)
  return(x)
}

planilla_completa <- planilla_completa %>%
  mutate(
    LOC_CLEAN = limpiar_texto(LOCALIDAD),
    DEP_CLEAN = limpiar_texto(DEPARTAMENTO),
    MUNICIPIO = case_when(
      
      # ==========================================
      # MONTEVIDEO
      # ==========================================
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "paso de la arena|cerro|casabo|pajas blancas|la teja|prado|belvedere|nuevo paris|santiago vazquez") ~ "Municipio A",
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "ciudad vieja|centro|barrio sur|palermo|parque rodo|cordon|tres cruces|aguada") ~ "Municipio B",
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "capurro|atahualpa|brazo oriental|reducto|goes|jacinto vera") ~ "Municipio C",
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "punta carretas|pocitos|buceo|parque batlle|villa dolores") ~ "Municipio CH",
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "toledo chico|manga|piedras blancas|casavalle|borro|marconi|cerrito|perez castellanos") ~ "Municipio D",
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "union|malvin|punta gorda|carrasco|la blanqueada|euskal erria") ~ "Municipio E",
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "punta de rieles|bañados de carrasco|flor de maroñas|maroñas|ituzaingo|jardines del hipodromo|piedras blancas") ~ "Municipio F",
      DEP_CLEAN == "montevideo" & str_detect(LOC_CLEAN, "lezica|melilla|colon|peñarol|sayago|conciliacion|paso de las duranas|abayuba") ~ "Municipio G",
      
      # ==========================================
      # CANELONES
      # ==========================================
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "18 de mayo") ~ "18 de Mayo",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "aguas corrientes") ~ "Aguas Corrientes",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "atlantida|pinamar") ~ "Atlántida",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "barros blancos") ~ "Barros Blancos",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "ciudad de la costa|solymar|pinar|shangrila|lagomar") ~ "Ciudad de la Costa",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "colonia nicolich|aeroparque") ~ "Colonia Nicolich",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "empalme olmos") ~ "Empalme Olmos",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "joaquin suarez|suarez") ~ "Joaquín Suárez",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "la floresta|costa azul|bello horizonte|guazubira|san luis|los titanes|la tuna") ~ "La Floresta",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "la paz") ~ "La Paz",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "las piedras") ~ "Las Piedras",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "los cerrillos|cerrillos") ~ "Los Cerrillos",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "migues") ~ "Migues",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "montes") ~ "Montes",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "pando") ~ "Pando",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "parque del plata|las toscas") ~ "Parque del Plata",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "paso carrasco") ~ "Paso Carrasco",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "progreso") ~ "Progreso",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "san antonio") ~ "San Antonio",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "san bautista") ~ "San Bautista",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "san jacinto") ~ "San Jacinto",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "san ramon") ~ "San Ramón",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "santa lucia") ~ "Santa Lucía",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "santa rosa") ~ "Santa Rosa",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "sauce") ~ "Sauce",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "soca") ~ "Soca",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "tala") ~ "Tala",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "toledo") ~ "Toledo",
      DEP_CLEAN == "canelones" & str_detect(LOC_CLEAN, "canelones|juanico|canelon chico") ~ "Canelones",

      # ==========================================
      # MALDONADO
      # ==========================================
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "aigua") ~ "Aiguá",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "garzon|jose ignacio") ~ "Garzón",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "pan de azucar") ~ "Pan de Azúcar",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "piriapolis|playa verde|playa hermosa|playa grande|punta colorada|san francisco|punta negra") ~ "Piriápolis",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "punta del este|mansa|brava|san rafael") ~ "Punta del Este",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "san carlos|la barra|manantiales|el tesoro|balneario buenos aires|eden") ~ "San Carlos",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "solis|cerros azules|las flores|bella vista") ~ "Solís Grande",
      DEP_CLEAN == "maldonado" & str_detect(LOC_CLEAN, "maldonado|punta ballena|sauce|portezuelo") ~ "Maldonado",

      # ==========================================
      # ROCHA
      # ==========================================
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "castillo|aguas dulces|valizas") ~ "Castillos",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "chuy|barra chuy") ~ "Chuy",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "la paloma|la pedrera|antoniades|arachania") ~ "La Paloma",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "lascano") ~ "Lascano",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "punta del diablo") ~ "Punta del Diablo",
      DEP_CLEAN == "rocha" & str_detect(LOC_CLEAN, "rocha|cabo polonio") ~ "Rocha", # Asumiendo capital u otras áreas

      # ==========================================
      # COLONIA
      # ==========================================
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "carmelo") ~ "Carmelo",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "colonia valdense|valdense") ~ "Colonia Valdense",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "florencio sanchez") ~ "Florencio Sánchez",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "juan lacaze|lacaze") ~ "Juan Lacaze",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "miguelete") ~ "Miguelete",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "nueva helvecia") ~ "Nueva Helvecia",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "nueva palmira") ~ "Nueva Palmira",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "ombues") ~ "Ombúes de Lavalle",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "rosario") ~ "Rosario",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "tarariras") ~ "Tarariras",
      DEP_CLEAN == "colonia" & str_detect(LOC_CLEAN, "colonia del sacramento|colonia|riachuelo") ~ "Colonia del Sacramento",

      # ==========================================
      # SAN JOSE
      # ==========================================
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "ciudad del plata|delta del tigre|autodromo|playa pascual") ~ "Ciudad del Plata",
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "ecilda paullier") ~ "Ecilda Paullier",
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "libertad|kiyu|colonia wilson") ~ "Libertad",
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "rodriguez") ~ "Rodríguez",
      DEP_CLEAN == "san jose" & str_detect(LOC_CLEAN, "san jose|juan soler|boyada|rafael peraza") ~ "San José de Mayo",

      # ==========================================
      # SORIANO
      # ==========================================
      DEP_CLEAN == "soriano" & str_detect(LOC_CLEAN, "cardona") ~ "Cardona",
      DEP_CLEAN == "soriano" & str_detect(LOC_CLEAN, "dolores") ~ "Dolores",
      DEP_CLEAN == "soriano" & str_detect(LOC_CLEAN, "palmitas") ~ "Palmitas",
      DEP_CLEAN == "soriano" & str_detect(LOC_CLEAN, "rodo|jose enrique rodo|risso|santa catalina") ~ "José Enrique Rodó",
      DEP_CLEAN == "soriano" & str_detect(LOC_CLEAN, "mercedes|palmar") ~ "Mercedes",

      # ==========================================
      # FLORIDA
      # ==========================================
      DEP_CLEAN == "florida" & str_detect(LOC_CLEAN, "casupa") ~ "Casupá",
      DEP_CLEAN == "florida" & str_detect(LOC_CLEAN, "fray marcos") ~ "Fray Marcos",
      DEP_CLEAN == "florida" & str_detect(LOC_CLEAN, "sarandi grande") ~ "Sarandí Grande",
      DEP_CLEAN == "florida" & str_detect(LOC_CLEAN, "florida|mendoza|25 de mayo|25 de agosto|cerro colorado|la cruz|capilla del sauce") ~ "Florida",

      # ==========================================
      # LAVALLEJA
      # ==========================================
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "batlle|ordonez") ~ "José Batlle y Ordóñez",
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "pedro varela|varela") ~ "José Pedro Varela",
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "mariscala") ~ "Mariscala",
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "solis de mataojo") ~ "Solís de Mataojo",
      DEP_CLEAN == "lavalleja" & str_detect(LOC_CLEAN, "minas") ~ "Minas",

      # ==========================================
      # CERRO LARGO
      # ==========================================
      DEP_CLEAN == "cerro largo" & str_detect(LOC_CLEAN, "fraile muerto") ~ "Fraile Muerto",
      DEP_CLEAN == "cerro largo" & str_detect(LOC_CLEAN, "noblia") ~ "Isidoro Noblia",
      DEP_CLEAN == "cerro largo" & str_detect(LOC_CLEAN, "rio branco") ~ "Río Branco",
      DEP_CLEAN == "cerro largo" & str_detect(LOC_CLEAN, "acegua") ~ "Aceguá",
      DEP_CLEAN == "cerro largo" & str_detect(LOC_CLEAN, "melo") ~ "Melo",

      # ==========================================
      # ARTIGAS
      # ==========================================
      DEP_CLEAN == "artigas" & str_detect(LOC_CLEAN, "brum") ~ "Baltasar Brum",
      DEP_CLEAN == "artigas" & str_detect(LOC_CLEAN, "bella union|cuareim") ~ "Bella Unión",
      DEP_CLEAN == "artigas" & str_detect(LOC_CLEAN, "gomensoro") ~ "Tomás Gomensoro",
      DEP_CLEAN == "artigas" & str_detect(LOC_CLEAN, "artigas|san eugenio") ~ "Artigas",

      # ==========================================
      # SALTO
      # ==========================================
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "belen") ~ "Belén",
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "colonia lavalleja") ~ "Colonia Lavalleja",
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "constitucion") ~ "Constitución",
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "mataojo") ~ "Mataojo",
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "valentin|biassini") ~ "Rincón de Valentín",
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "san antonio") ~ "San Antonio",
      DEP_CLEAN == "salto" & str_detect(LOC_CLEAN, "salto") ~ "Salto",

      # ==========================================
      # PAYSANDU
      # ==========================================
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "chapicuy") ~ "Chapicuy",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "guichon") ~ "Guichón",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "lorenzo geyres") ~ "Lorenzo Geyres",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "piedras coloradas") ~ "Piedras Coloradas",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "porvenir") ~ "Porvenir",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "quebracho") ~ "Quebracho",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "tambores") ~ "Tambores",
      DEP_CLEAN == "paysandu" & str_detect(LOC_CLEAN, "paysandu") ~ "Paysandú",

      # ==========================================
      # RIO NEGRO
      # ==========================================
      DEP_CLEAN == "rio negro" & str_detect(LOC_CLEAN, "nuevo berlin") ~ "Nuevo Berlín",
      DEP_CLEAN == "rio negro" & str_detect(LOC_CLEAN, "san javier") ~ "San Javier",
      DEP_CLEAN == "rio negro" & str_detect(LOC_CLEAN, "young") ~ "Young",
      DEP_CLEAN == "rio negro" & str_detect(LOC_CLEAN, "fray bentos|paso de la cruz|zorrila") ~ "Fray Bentos",

      # ==========================================
      # TACUAREMBO
      # ==========================================
      DEP_CLEAN == "tacuarembo" & str_detect(LOC_CLEAN, "ansina") ~ "Ansina",
      DEP_CLEAN == "tacuarembo" & str_detect(LOC_CLEAN, "paso de los toros") ~ "Paso de los Toros",
      DEP_CLEAN == "tacuarembo" & str_detect(LOC_CLEAN, "san gregorio") ~ "San Gregorio de Polanco",
      DEP_CLEAN == "tacuarembo" & str_detect(LOC_CLEAN, "tacuarembo|caraguata") ~ "Tacuarembó",

      # ==========================================
      # RIVERA
      # ==========================================
      DEP_CLEAN == "rivera" & str_detect(LOC_CLEAN, "corrales") ~ "Minas de Corrales",
      DEP_CLEAN == "rivera" & str_detect(LOC_CLEAN, "tranqueras") ~ "Tranqueras",
      DEP_CLEAN == "rivera" & str_detect(LOC_CLEAN, "vichadero|lapuente") ~ "Vichadero",
      DEP_CLEAN == "rivera" & str_detect(LOC_CLEAN, "rivera") ~ "Rivera",

      # ==========================================
      # TREINTA Y TRES
      # ==========================================
      DEP_CLEAN == "treinta y tres" & str_detect(LOC_CLEAN, "cerro chato") ~ "Cerro Chato",
      DEP_CLEAN == "treinta y tres" & str_detect(LOC_CLEAN, "enrique martinez|charqueada") ~ "General Enrique Martínez",
      DEP_CLEAN == "treinta y tres" & str_detect(LOC_CLEAN, "rincon") ~ "Rincón",
      DEP_CLEAN == "treinta y tres" & str_detect(LOC_CLEAN, "santa clara") ~ "Santa Clara de Olimar",
      DEP_CLEAN == "treinta y tres" & str_detect(LOC_CLEAN, "vergara") ~ "Vergara",
      DEP_CLEAN == "treinta y tres" & str_detect(LOC_CLEAN, "treinta y tres|villa sara|isla patrulla") ~ "Treinta y Tres",

      # ==========================================
      # DURAZNO
      # ==========================================
      DEP_CLEAN == "durazno" & str_detect(LOC_CLEAN, "carmen") ~ "Ciudad del Carmen",
      DEP_CLEAN == "durazno" & str_detect(LOC_CLEAN, "sarandi del yi") ~ "Sarandí del Yí",
      DEP_CLEAN == "durazno" & str_detect(LOC_CLEAN, "durazno|feliciano|blanquillo") ~ "Durazno",

      # ==========================================
      # FLORES
      # ==========================================
      DEP_CLEAN == "flores" & str_detect(LOC_CLEAN, "ismael cortinas") ~ "Ismael Cortinas",
      DEP_CLEAN == "flores" & str_detect(LOC_CLEAN, "trinidad|san gregorio") ~ "Trinidad",

      TRUE ~ "" # Casos que no matcheen, quedan vacíos para clasificación manual
    )
  ) %>%
  # Limpiar columnas auxiliares
  select(-LOC_CLEAN, -DEP_CLEAN)

totales <- table(planilla_completa$MUNICIPIO == "")
cat(sprintf("Registros con MUNICIPIO asignado: %d\n", totales["FALSE"]))
cat(sprintf("Registros con MUNICIPIO VACÍO: %d\n", totales["TRUE"]))

save(planilla_completa, file = "c:/Proyectos/Denuncias/planilla_completa.RData")
cat("Guardado completado exitosamente con la nueva variable MUNICIPIO.\n")

