################################################################################
# HUMEDALES URBANOS PUQ
# Supervisión Final
# Versión : 03.05.24
################################################################################

# Librerías
library(readxl)
library(questionr)
library(openxlsx)
library(tidyverse)


# Importar Base de datos con chequeo de sexo y edad de Supervisora (P.Espinoza)
humecheq <- read_excel(
  "supervision final/Humedales - Supervision Final - Rev. P.Espinoza - 240402.xlsx",
  sheet = "datos_chequeo")

names(humecheq)

describe(humecheq)

humecheq$fecha1 <-  as.numeric(humecheq$fecha_inicio)
humecheq$fecha2 <-  as.numeric(humecheq$fecha_final)

# Identificación de casos según fechas de registro 
humecheq <- humecheq %>%
  unite(col = ident, 
        c("fecha1", "fecha2"),
        sep = "")

# Verificación de registros duplicados
sum(duplicated(humecheq$ident)) # No hay registros duplicados

# Eliminar registros sin sexo y edad
humecheq <- humecheq |> 
  filter(!is.na(genero) & !is.na(edad))

# Verificación de casos en dirección
sum(duplicated(humecheq$direccion)) # Se observan 4 direciones duplicadas

# Filtrar direcciones duplicadas
set.seed(2524)
humecheq <- select(humecheq, -comentario)
humecheq <- humecheq |>
  group_by(direccion) |> 
  slice_sample(n = 1) |> 
  ungroup()



# Consolidar Nombres de Encuestadores
## Recoding humecheq$encuestador
humecheq$encuestador <- humecheq$encuestador %>%
  fct_recode(
    "Alejandra Carroza" = "Alejandra   carroza",
    "Alejandra Carroza" = "Alejandra  Carroza",
    "Alejandra Carroza" = "Alejandra carroza",
    "Alejandra Carroza" = "Alejandra Carroza ll",
    "Benjamín Ramírez" = "Benjamín Ramírez E",
    "Catalina Buvinic" = "catalina buvinic",
    "Catalina Gerding" = "Catalina Gerding Guequen",
    "Diego Pérez" = "DIEGO PÉREZ NAIMAN",
    "Fernando Fritz" = "Fernando Fritz Sánchez",
    "Francesca Caipillan" = "Francesca caipillan",
    "Fransheska Arjel" = "Fransheska Arjel Guerrero",
    "Gonzalo Manzo" = "Gonzalo manzo",
    "Graciela Mazzella" = "Graciela",
    "Ignacio Soto" = "Ignacio Soto Quezada",
    "Isabel Muñoz" = "Isabe Muñoz",
    "Karla Barria" = "Karla barria",
    "Katherine Sobarzo" = "Katherine Sobarzo Bustamante",
    "Maria Jose Gallardo" = "Maria gallardo",
    "Maria Jose Gallardo" = "Maria jose gallardo",
    "María Soledad Molina" = "Maria Soledad Molina Osorio",
    "María Soledad Molina" = "María Soledad Molina Osorio",
    "Marianka Flores" = "marianka flores",
    "Marianka Flores" = "marianka Flores",
    "Patricio Piña" = "Patricio Piña Fernández",
    "Paula Fuentes" = "Paula fuentes",
    "Valeska Oyarzo" = "Valeska Oyarzo Igor",
    "Vicky Márquez" = "vicky márquez navarro"
  )
freq(humecheq$encuestador)
        
## Guardar base de datos Humedales Revisión final
# Formato RDS
saveRDS(humecheq,
        file = "supervision final/humedales_revision_final_OK.rds"
)

        
