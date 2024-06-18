##########################################################################
# 11setup.R                                                              #
# 1.1 Overall setup for obtaining dfs suitable for analyses (odf and idf)#
##########################################################################

options(digits = 3);options(scipen = 999);options(max.print = 5000)

odf <- readRDS("base_humedales_final.rds")#load rds datafile
#file not provided as it contains sensitive info, however final dataset is loaded at the end of this file

#install and load dplyr package
#install.packages("dplyr")  # Install the package if not already installed
library(dplyr)             # Load the package

#rename all columns
odf <- odf %>%
  rename(
    # Context variables (1-13)
    caseid = id_caso,
    startd = startdate,
    finisd = enddate,
    status = status,
    ipaddr = ipaddress,
    progrs = progress,
    duratn = `duration (in seconds)`,
    fnishd = finished,
    record = recordeddate,
    respid = responseid,
    loclat = locationlatitude,
    loclon = locationlongitude,
    intnam = q1.1,
    consnt = q2.1,
    
    # General and environmental views (14-45)
    genenv = a1,
    prchil = a2.1,
    prchot = a2.1_otesp,
    prpers = a2.2,
    prpeot = a2.2_otesp,
    envec1 = a2.3_1,
    envec2 = a2.3_2,
    envec3 = a2.3_3,
    envec4 = a2.3_4,
    envec5 = a2.3_5,
    envec6 = a2.3_6,
    natcn1 = a3.1_1,
    natcn2 = a3.1_2,
    natcn3 = a3.1_3,
    natcn4 = a3.1_4,
    envpa1 = a3.2_1,
    envpa2 = a3.2_2,
    envpa3 = a3.2_3,
    envpa4 = a3.2_4,
    envpa5 = a3.2_5,
    envpa6 = a3.2_6,
    envpa7 = a3.2_7,
    govrol = a.4,
    penvb1 = a.5.1_1,
    penvb2 = a.5.1_2,
    penvb3 = a.5.1_3,
    penvb4 = a.5.1_4,
    penvb5 = a.5.1_5,
    penvb6 = a.5.1_6,
    pecsa1 = a5.2_1,
    pecsa2 = a5.2_2,
    pecsa3 = a5.2_3,
    
    # Wetlands knowledge and opinions (46-109)
    wlknw1 = b1.1_1,
    wlknw2 = b1.1_2,
    wlknw3 = b1.1_3,
    wlknw4 = b1.1_4,
    wlknw5 = b1.1_5,
    wlknw6 = b1.1_6,
    wlknw7 = b1.1_7,
    wlknot = b1.1_otesp,
    wlim01 = b1.2_1,
    wlim02 = b1.2_2,
    wlim03 = b1.2_3,
    wlim04 = b1.2_4,
    wlim05 = b1.2_5,
    wlim06 = b1.2_6,
    wlim07 = b1.2_7,
    wlim08 = b1.2_8,
    wlim09 = b1.2_9,
    wlim10 = b1.2_10,
    wlim11 = b1.2_11,
    wlim12 = b1.2_12,
    wlim13 = b1.2_14,
    wlim14 = b1.2_15,
    wlim15 = b1.2_13,
    wlan01 = b1.3_1,
    wlan02 = b1.3_2,
    wlan03 = b1.3_3,
    wlan04 = b1.3_4,
    wlan05 = b1.3_8,
    wlan06 = b1.3_5,
    wlan07 = b1.3_9,
    wlan08 = b1.3_10,
    wlan09 = b1.3_11,
    wlan10 = b1.3_12,
    wlan11 = b1.3_6,
    wlan12 = b1.3_7,
    wlanot = b1.3_otesp,
    wlpl01 = b1.4_1,
    wlpl02 = b1.4_2,
    wlpl03 = b1.4_3,
    wlpl04 = b1.4_4,
    wlpl05 = b1.4_5,
    wlpl06 = b1.4_6,
    wlpl07 = b1.4_7,
    wlplot = b1.4_otesp,
    wlth01 = b1.5_1,
    wlth02 = b1.5_2,
    wlth03 = b1.5_3,
    wlth04 = b1.5_4,
    wlth05 = b1.5_5,
    wlth06 = b1.5_7,
    wlth07 = b1.5_8,
    wlth08 = b1.5_10,
    wlth09 = b1.5_11,
    wlth10 = b1.5_12,
    wlth11 = b1.5_13,
    wlth12 = b1.5_6,
    wlth13 = b1.5_9,
    wlth14 = b1.5_14,
    wlthot = b1.5_otesp,
    wlpri1 = b1.6_1,
    wlpri2 = b1.6_2,
    wlpri3 = b1.6_3,
    wlpri4 = b1.6_4,
    wlrspn = b1.7,
    
    # Parque Maria Behety (110-152)
    mbfreq = b2.1,
    mbleng = b2.2,
    mbac01 = b2.3_1,
    mbac02 = b2.3_2,
    mbac03 = b2.3_3,
    mbac04 = b2.3_4,
    mbac05 = b2.3_5,
    mbac06 = b2.3_6,
    mbac07 = b2.3_7,
    mbac08 = b2.3_8,
    mbac09 = b2.3_9,
    mbacot = b2.3_otesp,
    mbtran = b2.4,
    mbtrot = b2.4_otesp,
    mbaccs = b2.5,
    mbstat = b2.6,
    mbbe01 = b2.7_1,
    mbbe02 = b2.7_2,
    mbbe03 = b2.7_3,
    mbbe04 = b2.7_4,
    mbbe05 = b2.7_5,
    mbbe06 = b2.7_6,
    mbbe07 = b2.7_7,
    mbbe08 = b2.7_8,
    mbbe09 = b2.7_9,
    mbbe10 = b2.7_10,
    mbbe11 = b2.7_11,
    mbbe12 = b2.7_12,
    mbbe13 = b2.7_13,
    mbbe14 = b2.7_14,
    mbbemi = b2.8,
    mbpr01 = b2.9_1,
    mbpr02 = b2.9_2,
    mbpr03 = b2.9_3,
    mbpr04 = b2.9_4,
    mbpr05 = b2.9_5,
    mbpr06 = b2.9_6,
    mbpr07 = b2.9_8,
    mbpr08 = b2.9_7,
    mbprot = b2.9_otesp,
    mblvis = b2.10,
    mbnvis = b2.11,
    mbnvot = b2.11_otesp,
    
    # Humedal Tres Puentes (153-197)
    tpfreq = b3.1,
    tpleng = b3.2,
    tpac01 = b3.3_1,
    tpac02 = b3.3_2,
    tpac03 = b3.3_3,
    tpac04 = b3.3_4,
    tpac05 = b3.3_5,
    tpac06 = b3.3_6,
    tpac07 = b3.3_7,
    tpac08 = b3.3_8,
    tpac09 = b3.3_9,
    tpacot = b3.3_otesp,
    tptran = b3.4,
    tptrot = b3.4_otesp,
    tpaccs = b3.5,
    tpstat = b3.6,
    tpbe01 = b3.7_1,
    tpbe02 = b3.7_2,
    tpbe03 = b3.7_3,
    tpbe04 = b3.7_4,
    tpbe05 = b3.7_5,
    tpbe06 = b3.7_6,
    tpbe07 = b3.7_7,
    tpbe08 = b3.7_8,
    tpbe09 = b3.7_9,
    tpbe10 = b3.7_10,
    tpbe11 = b3.7_11,
    tpbe12 = b3.7_12,
    tpbe13 = b3.7_13,
    tpbe14 = b3.7_14,
    tpbemi = b3.8,
    tppr01 = b3.9_1,
    tppr02 = b3.9_2,
    tppr03 = b3.9_3,
    tppr04 = b3.9_4,
    tppr05 = b3.9_5,
    tppr06 = b3.9_6,
    tppr07 = b3.9_8,
    tppr08 = b3.9_9,
    tppr09 = b3.9_10,
    tppr10 = b3.9_11,
    tpprot = b3.9_otesp,
    tplvis = b3.10,
    tpnvis = b3.11,
    tpnvot = b3.11_otesp,
    
    # Behavior intentions (198-207)  
    bint01 = b4.1_1,
    bint02 = b4.1_2,
    bint03 = b4.1_3,
    bint04 = b4.1_4,
    bint05 = b4.1_5,
    bint06 = b4.1_6,
    bint07 = b4.1_7,
    bint08 = b4.1_8,
    bint09 = b4.1_9,
    bint10 = b4.1_10,
    
    # Regional social identification (208-211)
    regid1 = c1.1_1,
    regid2 = c1.1_2,
    regid3 = c1.1_3,
    regid4 = c1.1_4,
    
    # Place attachment (212-215)
    place1 = c1.2_1,
    place2 = c1.2_2,
    place3 = c1.2_3,
    place4 = c1.2_4,
    
    # Background/demographic variables (216-234)
    nation = c2.1,
    natiot = c2.1_ot,
    bplace = c2.2,
    bplaot = c2.2_ot,
    ethnic = c2.3,
    ethnot = c2.3_ot,
    educat = d1,
    emplst = d2,
    occupa = d3,
    srvstd = fecha_inicio,
    srvend = fecha_final,
    intna2 = encuestador,
    gender = genero,
    agepar = edad,
    phonen = telefono,
    addrss = direccion,
    sexpar = sexo,
    agegrp = edad2,
    educar = educacion_rec,
    
    # Weighting (235)  
    weight = pond #weighting factor
  )

#explore data to fix anything before analyses
data.frame(colnames(odf))


#check these
summary(odf[,7])#response time, has some outliers that dont make sense
length(unique(odf[,11])) #196 different x y coordinates
length(unique(odf[,12])) #196 different x y coordinates

#fixing intnam
# Read the name mappings from the CSV file
name_mapping <- read.csv("name_mappings.csv", stringsAsFactors = FALSE)

# Perform the name standardization and code-name mapping using the lookup table
#the lookup table is not provided, as it contains sensitive info, however, the final dataset is loaded at the end
odf$intnam <- name_mapping$standardized_name[match(odf$intnam, name_mapping$original_name)]
odf$intnam <- name_mapping$code_name[match(odf$intnam, name_mapping$standardized_name)]

# Fix odf[,15]
odf[,15] <- as.numeric(ifelse(odf[,15] %in% c("-888 No sabe (no leer)", "99 No responde (no leer)", NA), NA,
                              ifelse(odf[,15] == "5 Está muy preocupado", 5,
                                     ifelse(odf[,15] == "4", 4,
                                            ifelse(odf[,15] == "3", 3,
                                                   ifelse(odf[,15] == "2", 2,
                                                          ifelse(odf[,15] == "1 No está preocupado", 1, odf[,15])))))))

# Fix LIKERT items (odf[,20:36] and odf[,44:46])
odf[, c(20:36, 44:46, 106:109, 199:216)] <- lapply(odf[, c(20:36, 44:46, 106:109, 199:216)], function(x) {
  as.numeric(ifelse(x %in% c("No sabe (no leer)", "No responde (no leer)", NA), NA,
                    ifelse(x == "Muy de acuerdo", 5,
                           ifelse(x == "De acuerdo", 4,
                                  ifelse(x == "Ni de acuerdo ni en desacuerdo", 3,
                                         ifelse(x == "En desacuerdo", 2,
                                                ifelse(x == "Muy en desacuerdo", 1, x)))))))
})

# Fix odf[,37]
odf[,37] <- as.numeric(ifelse(odf[,37] %in% c("99. No responde (no leer)", "-888. No sabe (no leer)", NA), NA,
                              ifelse(odf[,37] == "2. El gobierno debería aprobar leyes para hacer que la gente proteja el medio ambiente, incluso si esto interfiere con los derechos que tienen las personas de tomar sus propias decisiones", 2,
                                     ifelse(odf[,37] == "1. El gobierno debería dejar a la gente decidir cómo proteger el medio ambiente, incluso si esto significa que no siempre hagan lo correcto", 1, odf[,37]))))
odf$govrol < - odf$govrol-1 #make dichotomous 1 and 0

# Fix FREQUENCY items (odf[,38:43])
odf[, 38:43] <- lapply(odf[, 38:43], function(x) {
  as.numeric(ifelse(x %in% c("No sabe (no leer)", "No responde (no leer)", NA), NA,
                    ifelse(x == "Muy frecuentemente", 4,
                           ifelse(x == "Frecuentemente", 3,
                                  ifelse(x == "A veces", 2,
                                         ifelse(x == "Casi nunca", 1,
                                                ifelse(x == "Nunca", 0, x)))))))
})

#dichotomous mention vars
odf[, c(47:53, 55:69, 70:81, 83:89, 91:104, 113:121, 127:140, 142:149, 156:164, 170:183, 185:194)] <- lapply(odf[, c(47:53, 55:69, 70:81, 83:89, 91:104, 113:121, 127:140, 142:149, 156:164, 170:183, 185:194)], function(x) ifelse(is.na(x), 0, 1))



# Fix both odf[,111] and odf[,154]
odf[, 111] <- as.numeric(ifelse(odf[, 111] == "Al menos 1 vez a la semana", 4,
                                ifelse(odf[, 111] == "Entre 2 y 3 veces al mes", 3,
                                       ifelse(odf[, 111] == "Una vez al mes", 2,
                                              ifelse(odf[, 111] %in% c("Entre 5 y 11 veces al año", "Cuatro o menos veces al año"), 1,
                                                     ifelse(odf[, 111] == "Nunca", 0, NA))))))

odf[, 154] <- as.numeric(ifelse(odf[, 154] == "Al menos 1 vez a la semana", 4,
                                ifelse(odf[, 154] == "Entre 2 y 3 veces al mes", 3,
                                       ifelse(odf[, 154] == "Una vez al mes", 2,
                                              ifelse(odf[, 154] %in% c("Entre 5 y 11 veces al año", "Cuatro o menos veces al año"), 1,
                                                     ifelse(odf[, 154] == "Nunca", 0, NA))))))

# Fix both odf[,112] and odf[,155]
odf[, c(112, 155)] <- lapply(odf[, c(112, 155)], function(x) {
  as.numeric(ifelse(x == "Más de 4 horas", 4,
                    ifelse(x == "Entre 3 y 4 horas", 3,
                           ifelse(x == "Entre 1 y 2 horas", 2,
                                  ifelse(x == "Menos de 1 hora", 1, NA)))))
})

# Fix both odf[,125] and odf[,168]
odf[, 125] <- as.numeric(ifelse(odf[, 125] == "Muy fácil", 5,
                                ifelse(odf[, 125] == "Fácil", 4,
                                       ifelse(odf[, 125] == "Más o menos difícil", 3,
                                              ifelse(odf[, 125] == "Difícil", 2,
                                                     ifelse(odf[, 125] == "Muy difícil", 1, NA))))))

odf[, 168] <- as.numeric(ifelse(odf[, 168] == "Muy fácil", 5,
                                ifelse(odf[, 168] == "Fácil", 4,
                                       ifelse(odf[, 168] == "Más o menos difícil", 3,
                                              ifelse(odf[, 168] == "Difícil", 2,
                                                     ifelse(odf[, 168] == "Muy difícil", 1, NA))))))

# Fix both odf[,126] and odf[,169]
odf[, c(126, 169)] <- lapply(odf[, c(126, 169)], function(x) {
  as.numeric(ifelse(x == "Ha mejorado", 3,
                    ifelse(x == "Está igual", 2,
                           ifelse(x == "Ha empeorado", 1,
                                  ifelse(x == "No me he fijado", 0, NA)))))
})

# Fix both odf[,151] and odf[,196]
odf[, 151] <- as.numeric(ifelse(odf[, 151] == "Esta semana", 4,
                                ifelse(odf[, 151] == "La semana pasada", 3,
                                       ifelse(odf[, 151] == "Hace 2 o 3 semanas", 2,
                                              ifelse(odf[, 151] == "Hace un mes o más", 1,
                                                     ifelse(odf[, 151] %in% c("No lo recuerdo", NA), NA, odf[, 151]))))))

odf[, 196] <- as.numeric(ifelse(odf[, 196] == "Esta semana", 4,
                                ifelse(odf[, 196] == "La semana pasada", 3,
                                       ifelse(odf[, 196] == "Hace 2 o 3 semanas", 2,
                                              ifelse(odf[, 196] == "Hace un mes o más", 1,
                                                     ifelse(odf[, 196] %in% c("No lo recuerdo", NA), NA, odf[, 196]))))))

# Fix odf[,217]
odf[,217] <- as.numeric(ifelse(odf[,217] == "1. Chilena", 0,
                               ifelse(odf[,217] == "2. Otra. Especificar...", 1,
                                      ifelse(odf[,217] %in% c("-888. No sabe (no leer)", NA), NA, odf[,217]))))

# Fix odf[,219]
odf[,219] <- as.numeric(ifelse(odf[,219] == "1. En esta comuna", 0,
                               ifelse(odf[,219] %in% c("2. En otra comuna de Chile. Especifique ¿cuál comuna?", "3. En otro país. Especifique ¿cuál país?"), 1,
                                      ifelse(odf[,219] %in% c("-999. No responde (no leer)", NA), NA, odf[,219]))))

# Fix odf[,221]
# Copy relevant information from var 221 to var 222 where var 222 is NA
odf[is.na(odf[,222]) & odf[,221] %in% c("1. Mapuche", "2. Kawashkar o Alacalufes", "3. Yámana o Yagán"), 222] <- odf[is.na(odf[,222]) & odf[,221] %in% c("1. Mapuche", "2. Kawashkar o Alacalufes", "3. Yámana o Yagán"), 221]
# Remove the prefixes (e.g., "1.", "2.") from var 222
odf[,222] <- sub("^\\d+\\. ", "", odf[,222])

# Convert var 221 to numeric
odf[,221] <- as.numeric(ifelse(odf[,221] == "12. No pertenece a ningún pueblo indígena", 0,
                               ifelse(odf[,221] %in% c("1. Mapuche", "2. Kawashkar o Alacalufes", "11. Otro. Especifique", "3. Yámana o Yagán"), 1,
                                      ifelse(odf[,221] %in% c("-888. No sabe (no leer)", NA), NA, odf[,221]))))

# Fix odf[,223]
odf[,223] <- as.numeric(gsub("\\D", "", odf[,223]))
odf[, 223][odf[, 223] == 888 | odf[, 223] == 99] <- NA

#install.packages("lubridate")  # Install the package if not already installed
library(lubridate)

odf$srtmon <- format(as.Date(odf$finisd), "%m")
odf$srtday <- format(as.Date(odf$finisd), "%d")
odf$srtdow <- format(as.POSIXlt(odf$finisd), "%w")
odf$srthou <- format(as.POSIXlt(odf$finisd), "%H")
odf$srtdos <- as.numeric(as.Date(odf$finisd) - as.Date(min(odf$finisd))) + 1

#last min specify some other NAs
odf$prchil[odf$prchil == "-888. No sabe (no leer)"] <- NA
odf$prpers[odf$prpers == "-888. No sabe (no leer)"] <- NA
odf$emplst[odf$emplst == "99. No responde (no leer)"] <- NA

#quick fix mbleng tpleng so they don't have as many NAs (logic fix)
odf$mbleng[odf$mbfreq == 0] <- 0 #length of stay becomes 0 if they say they don't go
odf$tpleng[odf$tpfreq == 0] <- 0 #length of stay becomes 0 if they say they don't go
#this might induce problematic collinearity for some analyses, but allows not losing n of people say they don't visit these places

# Recode odf$educar as a numerical ordinal variable
odf$educar <- as.numeric(factor(odf$educar,
                                levels = c("Menos que Básica", "Ed. Básica", "Ed. Media", "Técnica Superior", "Universitaria o más", "Sin información"),
                                labels = c(1, 2, 3, 4, 5, NA)))

#remove some vars, save file and load it
odf <- odf[, -c(2,3,4, 5, 6, 8, 9, 10, 11,12, 14, 226, 227, 228, 231, 232)]
odf <- odf[, c(1, 4:8,26,9:25,27:219,221:225, 2, 3,220)]
saveRDS(odf, "uwlpuq.rds")
odf <- readRDS("uwlpuq.rds")

#if needed, here's a version of the df with imputed data, idf
#install.packages("mice")
library(mice)

# Impute missing data using MICE
#imputed_data <- mice::mice(odf, method = 'pmm', m = 2)

# Set random seed from random.org (powered by RANDOM.ORG)
# Seed: 225; Min: 1, Max: 10000; 2024-06-18 13:49:13 UTC
#imputed_data <- mice(odf, m = 3, maxit = 20, method = 'pmm', seed = 225)
#imputed_data <- mice(odf, m = 10, maxit = 100, method = 'pmm', seed = 225)

# Save the imputed dataframe as 'idf'
idf <- complete(imputed_data)

# Save 'idf' as an RDS file named 'uwlpui.rds'
saveRDS(idf, file = "uwlpui.rds")

#cleanup
rm(list = setdiff(ls(), "odf"))  # Remove all objects except 'odf'
lapply(setdiff(loadedNamespaces(), base::loadedNamespaces()), function(pkg) try(detach(paste("package", pkg, sep = ":"), character.only = TRUE, unload = TRUE), silent = TRUE))  # Unload all non-base packages
idf <- readRDS("uwlpui.rds")

#now the analyses can be performed...
#odf is the original data, just re-arranged, re-ordered, with vars filtered out for sensitive and redundant data
#idf is the imputed dataset, if needed