
# Se omiten las tildes intencionalmente.

# Semana 3 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr)

# CPV 2020
# (Cuestionario ampliado) 

# Datos ----
load("viviendas_cpv2020.RData")

# Explorando un data.frame
class(viviendas)
dim(viviendas)
names(viviendas)
View(as.data.frame(names(viviendas)))
head(viviendas)
tail(viviendas)
str(viviendas)

# Se puede manejar el data.frame por columnas.
head(viviendas$AGUA_ENTUBADA)
tail(viviendas[, 22])
head(viviendas[, "AGUA_ENTUBADA"])

# Varias columnas
head(viviendas[,c(1,2,22)])
names(viviendas[,c(1,2,22)])
head(viviendas[, c("ENT", "MUN", "AGUA_ENTUBADA")])

# Para crear nuevas columnas o variables.
viviendas$ceros<-0
head(viviendas, 3)

# O modificar variables existentes.
# Atencion: la informacion se reescribe
viviendas$ceros <- 2
head(viviendas, 3)

# Eliminar columnas
viviendas$ceros <- NULL
head(viviendas, 3)

# 35,156,897 Viviendas particulares habitadas
sum(viviendas$FACTOR)

# Identificador unico de viviendas
head(viviendas$ID_VIV)
options(scipen=999)
length(unique(viviendas$ID_VIV))

# Seleccion de variables ----
names(viviendas)

variables <- c("ENT", "MUN", "LOC50K", "ID_VIV", 
               "COBERTURA", "ESTRATO", "UPM", "FACTOR", 
               "AGUA_ENTUBADA", "ABA_AGUA_ENTU", "ABA_AGUA_NO_ENTU", 
               "JEFE_SEXO", "JEFE_EDAD", "TAMLOC")

# Opcion 1 (base)
viviendas_seleccion_opcion1 <- viviendas[, variables]

# Opcion 2 (dplyr)
viviendas_seleccion_opcion2 <- viviendas %>% 
  select(all_of(variables))

# Opcion 3 (dplyr)
viviendas_seleccion_opcion3 <- viviendas %>% 
  select("ENT", "MUN", "LOC50K", "ID_VIV", 
         "COBERTURA", "ESTRATO", "UPM", "FACTOR", 
         "AGUA_ENTUBADA", "ABA_AGUA_ENTU", "ABA_AGUA_NO_ENTU", 
         "JEFE_SEXO", "JEFE_EDAD", "TAMLOC")

agua <- viviendas_seleccion_opcion3

save(agua, file="agua_cpv2020.RData")
rm( list= ls()[ ls() != "agua" ] )
gc()

# load("agua_cpv2020.RData")
# Variables ----

# Clave de entidad
# https://www.inegi.org.mx/app/ageeml/
table(agua$ENT, useNA = "ifany")

agua$cve_ent <- as.character(agua$ENT)

?ifelse
agua$cve_ent <- ifelse(nchar(agua$cve_ent) == 1,
                       paste0("0", agua$cve_ent),
                       agua$cve_ent)
head(agua[, c("ENT","cve_ent")], 10)
tail(agua[, c("ENT","cve_ent")], 10)
table(agua$cve_ent, useNA = "ifany")

# Clave de municipio
head(agua$MUN); tail(agua$MUN)
agua$cve_mun <- as.character(agua$MUN)

agua$cve_mun <- ifelse(nchar(agua$cve_mun) == 1, paste0("00", agua$cve_mun),
                       ifelse(nchar(agua$cve_mun) == 2, paste0("0", agua$cve_mun),
                              agua$cve_mun))

head(agua[, c("MUN","cve_mun")], 10)
tail(agua[, c("MUN","cve_mun")], 10)

# cvegeo ----
agua$cvegeo <- paste0(agua$cve_ent, agua$cve_mun)
head(agua$cvegeo)
tail(agua$cvegeo)

length(unique(agua$cvegeo))

save(agua, file="agua_cpv2020.RData")

