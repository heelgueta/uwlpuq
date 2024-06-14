################################################################################
# HUMEDALES URBANOS PUQ
# Base de Datos Final
# Versión : 03.05.24
################################################################################

# Librerías

library(tidyverse)
library(questionr) # easier processing and analysis of surveys 
library(gtsummary)
library(readxl) # importar archivos en formato excel
library(openxlsx) # exportar archivos en formato excel
library(srvyr)
library(survey)
library(weights) # functions for producing simple weighted statistics
library(anesrake) # para calculo de ponderadores (raking - "iterative proportional fitting")


# Importar Base de datos de Supervisión Final
humecheq <-  readRDS("supervision final/humedales_revision_final_OK.rds")

# Importar Base de datos original Qualtrics
bdorig <- read_excel(
  "base_orig_qualtrics/Humedales - 2 de abril de 2024_21.12.xlsx", 
                                                 sheet = "datos_orig")


# Nombres de variables en minúsclas
names(bdorig) <- tolower(names(bdorig))

glimpse(bdorig)

# Identificación de casos según fechas de registro  
bdorig$fecha1 <-  as.numeric(bdorig$startdate)
bdorig$fecha2 <-  as.numeric(bdorig$enddate)

bdorig <- bdorig %>%
  unite(col = ident, 
        c("fecha1", "fecha2"),
        sep = "")

bdorig$ident

# Agregar BBDD original y BBDD chequeo final -----------------------------------
# Fusión según variable ident
humedales <- bdorig |> inner_join(humecheq, by = "ident")

## CREAR ID
# Crea variable de identificador de caso
# Se usa codigo "ident" (concatenación -como numero- de fecha inicio y término)
# Los casos están ordenados temporalmente en función de la fecha de registro
humedales <- humedales |> 
  arrange(ident) |> 
  rowid_to_column("id_caso")

glimpse(humedales)


# Recodificación variables ------------------------------------------

## Género
humedales$genero <- humedales$genero %>%
  fct_recode(
    "Hombre" = "hombre",
    "Mujer" = "mujer"
  )
freq(humedales$genero)
# Crear variable sexo para ponderación (Se codifica Otro = Mujer [n = 1])
humedales$sexo <- humedales$genero %>%
  fct_recode("Mujer" = "Otro")


## EDAD
humedales$edad <- as.numeric(humedales$edad)

# Edad en tramos
humedales$edad2 <- cut(humedales$edad,
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(18, 25, 35, 45, 55, 65, 100)
                       )
humedales$edad2 <- humedales$edad2 |> 
  fct_recode(
    "18-24" = "[18,25)",
    "25-34" = "[25,35)",    
    "35-44" = "[35,45)",
    "45-54" = "[45,55)",
    "55-64" = "[55,65)",
    "65+" = "[65,100]"
  )


## NIVEL EDUCACIONAL
# Recodificación educación, Último Nivel aprobado
## Recoding humedales$d1 into humedales$educacion_rec
humedales$d1 <- as_factor(humedales$d1)

## Recoding humedales$d1 into humedales$d1_rec
humedales$educacion_rec <- humedales$d1 %>%
  fct_recode(
    "Técnica Superior" = "7. Técnico Superior completa",
    "Universitaria o más" = "9. Universitaria completa",
    "Ed. Básica" = "3. Educación Básica o Preparatoria completa",
    "Menos que Básica" = "2. Educación Básica o Preparatoria incompleta",
    "Ed. Media" = "5. Educación Media o Humanidades completa",
    "Ed. Media" = "8. Universitaria incompleta",
    "Ed. Básica" = "4. Educación Media o Humanidades incompleta",
    "Universitaria o más" = "10. Estudios de posgrado (magíster o doctorado)",
    "Ed. Media" = "6. Técnico Superior incompleta",
    "Sin información" = "99. No responde (no leer)",
    "Sin información" = "-888. No sabe (no leer)",
  )
## Reordering humedales$educacion_rec
humedales$educacion_rec <- humedales$educacion_rec %>%
  fct_relevel(
    "Menos que Básica", "Ed. Básica", "Ed. Media", "Técnica Superior",
    "Universitaria o más", "Sin información"
  )

freq(humedales$educacion_rec)

# CALCULO PONDERADORES (RAKING) ------------------------------------------------

glimpse(humedales)

## Proporciones variables de ponderación (Muestra)
wpct(humedales$sexo)
wpct(humedales$edad2)
wpct(humedales$educacion_rec)

## Proporciones variables de ponderación (Marco Muestral: CENSO)
wpct(puq$sexo)
wpct(puq$edad2)
wpct(puq$educacion_rec)


# bases de datos en formato "data.frame" (mejor funcionamiento de "anesrake")
puq <- as.data.frame(puq)
humedales <- as.data.frame(humedales)

# Definir valores "target" para ponderación, a partir de datos del Censo
target <- with(puq, list(
  sexo = wpct(sexo),
  edad2  = wpct(edad2),
  educacion_rec  = wpct(educacion_rec)
))
target
str(target)

## Definir levels y nombres para variables "target"
# Sexo
levels(target$sexo) <-  levels(humedales$sexo)
names(target$sexo) <- levels(humedales$sexo)
# Edad
levels(target$edad2) <-  levels(humedales$edad2)
names(target$edad2) <- levels(humedales$edad2)
# Educación
levels(target$educacion_rec) <-  levels(humedales$educacion_rec)
names(target$educacion_rec) <- levels(humedales$educacion_rec)
# Chequear levels y names en "target"
str(target)

## Análisis Raking

# Diferencia entre los valores de la muestra y del marco (target)
# Método: "total", suma agregada de las diferencias en todos los nivels de las variables   
anesrakefinder(target, humedales, choosemethod = "total")

outsave <- anesrake(
  target, 
  humedales, 
  caseid = humedales$id_caso, # identificador caso
  verbose= FALSE, 
  cap = 5, # Máximo valor admitido para los ponderadores, x>5 son truncados
  choosemethod = "total",
  type = "pctlim", 
  pctlim = .05, 
  nlim = 5,
  iterate = TRUE,
  force1 = TRUE
)

summary(outsave)

# Agrega variable de ponderación a la base de datos Humedales
humedales$pond  <- unlist(outsave[1])
summary(humedales$pond)

# Calcular efecto diseño de la ponderación
# The weighting loss is the inflation in the variance of sample estimates that 
n <- length(humedales$sexo)
((sum(humedales$pond ^ 2) / (sum(humedales$pond)) ^ 2) * n) - 1

# ## Chequeo ponderación
# wpct(humedales$sexo, humedales$pond)
# wpct(puq$sexo)
# wpct(humedales$edad2, humedales$pond)
# wpct(puq$edad2)
# wpct(humedales$educacion_rec, humedales$pond)
# wpct(puq$educacion_rec)

# ## Chequeo Variable Preocupación por el medioambiente
# # Definir base ponderada
# hp <- humedales |>
#   labelled::user_na_to_na() |>
#   labelled::unlabelled() |>
#   as_survey_design(weights = pond)
# hp
# 
# # Resultado ponderado
# svytable(~a1 + sexo, hp) |>
#   cprop()
# 
# # Resultado no ponderado
# freq(humedales$a1)




