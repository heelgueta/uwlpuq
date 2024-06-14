################################################################################
# Proyecto Humedales Urbanos PUQ
# Marco Muestral Censo
################################################################################

library(tidyverse)
library(questionr)
library(censo2017)

# Listado Variables
variables_censo <- censo_tabla("variables_codificacion")

# Cargar base de datos del censo
# censo_conectar()
# Desconectar base de datos del censo
#censo_desconectar()

# Base de datos Personas 18 años y más - PUQ Zonas Urbanas, hogares particulares
puq <- tbl(censo_conectar(), "zonas") %>%
  mutate(
    region = substr(as.character(geocodigo), 1, 2),
    comuna = substr(as.character(geocodigo), 1, 5),
    distrito = substr(as.character(geocodigo), 6, 7),
    area = substr(as.character(geocodigo), 8, 8),
    localidad = substr(as.character(geocodigo), 9, 11)
  ) %>%
  filter(comuna == "12101" & area == "1") %>% # Punta Arenas Zonas Urbanas
  inner_join(select(tbl(censo_conectar(), "viviendas"),
                    zonaloc_ref_id,
                    vivienda_ref_id,
                    tipo_vivienda = p01
  ),
  by = "zonaloc_ref_id"
  ) %>%
  inner_join(select(
    tbl(censo_conectar(), "hogares"),
    vivienda_ref_id,
    hogar_ref_id,
    tipo_hogar
  ),
  by = "vivienda_ref_id"
  ) %>%
  inner_join(select(tbl(censo_conectar(), "personas"),
                    hogar_ref_id,
                    pco = p07, # Relación de parentesco
                    sexo = p08,
                    edad = p09,
                    aseduc = p13, # Asistencia Educación 
                    niveduc = p15, # Nivel del Curso Más Alto Aprobado
                    compeduc = p15a, # Completó el Nivel Especificado
                    porig = p16, # Pertenencia a pueblo originario
                    porig_grupo = p16a_grupo, # Pueblo originario (grupo)
                    condact = p17, # Condición de actividad
                    rama = p18 # Rama de Actividad Económica
  ),
  by = "hogar_ref_id"
  )

# Crea base de datos (tbl_df)
puq <- as_tibble(puq)

# Selecciona sólo mayores de 18 años que residen hogares particulares
puq <- puq |> filter(edad >= 18 & tipo_hogar < 98)


# RECODIFICACIÓN ---------------------------------------------------------------

## SEXO
puq$sexo <- puq$sexo %>%
  as.character() %>%
  fct_recode(
    "Hombre" = "1",
    "Mujer" = "2"
  )

## EDAD
puq$edad2 <- cut(puq$edad,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(18, 25, 35, 45, 55, 65, 100)
                 )
puq$edad2 <- puq$edad2 %>%
  fct_recode(
    "18-24" = "[18,25)",
    "25-34" = "[25,35)",    
    "35-44" = "[35,45)",
    "45-54" = "[45,55)",
    "55-64" = "[55,65)",
    "65+" = "[65,100]"
  )

## NIVEL EDUCACIONAL

# Nivel más alto cursado
puq$niveduc_rec <- puq$niveduc %>%
  as.character() %>%
  fct_recode(
    "Menos que Básica" = "1",
    "Menos que Básica" = "2",
    "Menos que Básica" = "3",
    "Menos que Básica" = "4",
    "Ed. Básica / Primaria" = "5",
    "Ed. Básica / Primaria" = "6",
    "Ed. Media / Humanidades" = "7",
    "Ed. Media / Humanidades" = "8",
    "Ed. Media / Humanidades" = "9",
    "Técnico Superior" = "10",
    "Técnico Superior" = "11",
    "Universitaria" = "12",
    "Postgrado" = "13",
    "Postgrado" = "14",
    "No aplica" = "98",
    "Sin información" = "99"
  )
puq$niveduc_rec <- puq$niveduc_rec %>%
  fct_relevel(
    "Menos que Básica",
    "Ed. Básica / Primaria",
    "Ed. Media / Humanidades",
    "Técnico Superior",
    "Universitaria",
    "Postgrado",
    "No aplica",
    "Sin información"
  )
# Recodificación Nivel educacional según término de nivel
puq <- puq %>% mutate(educacion = case_when(  
  aseduc == 3 ~ "Menos que Básica", # Nunca asistó a un establecimiento
  niveduc_rec == "Menos que Básica" 
  ~ "Menos que Básica",
  niveduc_rec == "Ed. Básica / Primaria" & compeduc == 1
  ~ "Ed. Básica / Primaria Completa",
  niveduc_rec == "Ed. Básica / Primaria" & compeduc == 2
  ~ "Ed. Básica / Primaria Incompleta",
  niveduc_rec == "Ed. Básica / Primaria" & compeduc == 99
  ~ "Ed. Básica / Primaria Completa",
  niveduc_rec == "Ed. Media / Humanidades" & compeduc == 1
  ~ "Ed. Media / Humanidades Completa",
  niveduc_rec == "Ed. Media / Humanidades" & compeduc == 2
  ~ "Ed. Media / Humanidades Incompleta",
  niveduc_rec == "Ed. Media / Humanidades" & compeduc == 99
  ~ "Ed. Media / Humanidades Completa",
  niveduc_rec == "Técnico Superior" & compeduc == 1
  ~ "Técnico Superior Completa",
  niveduc_rec == "Técnico Superior" & compeduc == 2
  ~ "Técnico Superior Incompleta",
  niveduc_rec == "Técnico Superior" & compeduc == 99
  ~ "Técnico Superior Completa",
  niveduc_rec == "Universitaria" & compeduc == 1
  ~ "Universitaria Completa",
  niveduc_rec == "Universitaria" & compeduc == 2
  ~ "Universitaria Incompleta",
  niveduc_rec == "Universitaria" & compeduc == 99
  ~ "Universitaria Completa",
  niveduc_rec == "Postgrado" ~ "Postgrado",
  niveduc_rec == "No aplica" ~ "No aplica",
  niveduc_rec == "Sin información" ~ "Sin información"
))
puq$educacion <- puq$educacion %>%
  fct_relevel(
    "Menos que Básica", 
    "Ed. Básica / Primaria Incompleta",
    "Ed. Básica / Primaria Completa",
    "Ed. Media / Humanidades Incompleta",
    "Ed. Media / Humanidades Completa",
    "Técnico Superior Incompleta",
    "Técnico Superior Completa",
    "Universitaria Incompleta",
    "Universitaria Completa",
    "Postgrado",
    "Sin información"
  )

# Nivel Educacional resumido (Último Nivel completado)
puq$educacion_rec <- puq$educacion %>%
  fct_recode(
    "Menos que Básica" = "Ed. Básica / Primaria Incompleta",
    "Ed. Básica" = "Ed. Básica / Primaria Completa",
    "Ed. Básica" = "Ed. Media / Humanidades Incompleta",
    "Ed. Media" = "Ed. Media / Humanidades Completa",
    "Ed. Media" = "Técnico Superior Incompleta",
    "Técnica Superior" = "Técnico Superior Completa",
    "Ed. Media" = "Universitaria Incompleta",
    "Universitaria o más" = "Universitaria Completa",
    "Universitaria o más" = "Postgrado"
  )


names(puq)
# RESULTADOS CENSO 2017 --------------------------------------------------------
# Se consiera sólo a la población mayor de 18 años que reside en hogares particulares

options(OutDec= ",")

xtabs(~ edad2 + sexo, data = puq) |> prop()

xtabs(~ educacion_rec + sexo, data = puq) |> prop()







