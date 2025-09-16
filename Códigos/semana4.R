
# Se omiten las tildes intencionalmente.

# Semana 4 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr)


# Datos ----
load("agua_cpv2020.RData")

# Seleccionando estados de la frontera sur

agua <- agua[agua$cve_ent=="04" | agua$cve_ent=="07" | 
             agua$cve_ent=="23" | agua$cve_ent=="27", ]


# Variables ----

## Nombre de entidad federativa ----
cvegeo <- read.delim("cvegeo.txt", colClasses = "character")
str(cvegeo)
head(cvegeo)
cvegeo <- cvegeo[, c(2,3,4)]
cvegeo <- unique(cvegeo)

agua <- agua %>%
  left_join(cvegeo, by = c("cve_ent"="CVE_ENT"))

head(agua)


## Area de residencia ----
# Vamos a definir urbano como una localidad mayor a 2500 habitantes y mas.
# 0: Urbano
# 1: Rural
table(agua$TAMLOC, useNA = "ifany")

# Opcion 1 (base)
agua$area <- ifelse(agua$TAMLOC >= 2, 0, 1)

# Opcion 2 (dplyr)
# agua <- agua %>%
#   mutate(area = ifelse(agua$TAMLOC >= 2, 0, 1))

# Convirtiendo la variable en variable tipo factor (ie, categorica).
agua$area <- factor(agua$area, levels = c(0,1), labels = c("Urbano", "Rural"))
table(agua$area, useNA = "ifany")

# Para mayor informacion sobre variables tipo factor:
# https://www.youtube.com/watch?v=xkRBfy8_2MU


## Abastecimiento de agua -----
table(agua$AGUA_ENTUBADA, useNA = "ifany")
table(agua$ABA_AGUA_ENTU, useNA = "ifany")

# 0: Entubada dentro de la vivienda abastecida por el servicio público de agua (dentroviv_serpublico)
# 1: Entubada dentro de la vivienda de otra fuente de abastecimiento (dentroviv_otrafuente)
# 2: Entubada fuera de la vivienda abastecida por el servicio público de agua (fueraviv_serpublico)
# 3: Entubada fuera de la vivienda de otra fuente de abastecimiento (fueraviv_otrafuente)
# 4: No entubada (noentubada)
# NA: Sin información

# Opcion 1 (dplyr)
agua <- agua %>%
  mutate(fuenteagua = case_when(
    AGUA_ENTUBADA==1 & ABA_AGUA_ENTU==1 ~ 0,
    AGUA_ENTUBADA==1 & (ABA_AGUA_ENTU>=2 & ABA_AGUA_ENTU<=9) ~ 1,
    AGUA_ENTUBADA==2 & ABA_AGUA_ENTU==1 ~ 2,
    AGUA_ENTUBADA==2 & (ABA_AGUA_ENTU>=2 & ABA_AGUA_ENTU<=9) ~ 3,
    AGUA_ENTUBADA==3 ~ 4,
    AGUA_ENTUBADA==9 ~ NA))

# Opcion 2 (base)
# agua$fuenteagua[agua$AGUA_ENTUBADA==1 & agua$ABA_AGUA_ENTU==1] <- 0
# agua$fuenteagua[agua$AGUA_ENTUBADA==1 & 
#                   (agua$ABA_AGUA_ENTU>=2 & agua$ABA_AGUA_ENTU<=9)] <- 1
# agua$fuenteagua[agua$AGUA_ENTUBADA==2 & agua$ABA_AGUA_ENTU==1] <- 2
# agua$fuenteagua[agua$AGUA_ENTUBADA==2 & 
#                   (agua$ABA_AGUA_ENTU>=2 & agua$ABA_AGUA_ENTU<=9)] <- 3
# agua$fuenteagua[agua$AGUA_ENTUBADA==3] <- 4
# agua$fuenteagua[agua$AGUA_ENTUBADA==9] <- NA

table(agua$AGUA_ENTUBADA, agua$ABA_AGUA_ENTU, useNA = "ifany")
table(agua$fuenteagua, useNA = "ifany")
class(agua$fuenteagua)

agua$fuenteagua <- factor(agua$fuenteagua, 
                          levels = c(0:4), 
                          labels = c("dentroviv_serpublico", "dentroviv_otrafuente",
                                     "fueraviv_serpublico", "fueraviv_otrafuente",
                                     "noentubada"))

table(agua$fuenteagua, useNA = "ifany")
prop.table(table(agua$fuenteagua, useNA = "ifany"))

## Sexo del jefe de hogar ----
table(agua$JEFE_SEXO, useNA = "ifany")
agua$sexojh[agua$JEFE_SEXO==1] <- 0
agua$sexojh[agua$JEFE_SEXO==3] <- 1
agua$sexojh <- factor(agua$sexojh, levels = c(0,1), labels = c("Hombre", "Mujer"))
table(agua$JEFE_SEXO, agua$sexojh, useNA = "ifany")

## Edad del jefe de hogar ----
summary(agua$JEFE_EDAD)
hist(agua$JEFE_EDAD)

agua$edadjh <- agua$JEFE_EDAD
agua$edadjh[agua$edadjh==999] <- NA
summary(agua$edadjh)
hist(agua$edadjh)

# Cierre ----
# Vamos a organizar nuestro data.frame. 

# Normalizar los nombres
names(agua) <- tolower(names(agua))
names(agua)

# Seleccionar variables
agua <- agua %>% 
  select("cvegeo", "cve_ent", "nom_ent", "nom_abr", "cve_mun", "id_viv", 
         "cobertura", "estrato", "upm", "factor", 
         "area", "tamloc", "fuenteagua", "sexojh", "edadjh")

# Guardar archivo
save(agua, file="agua_fronterasur.RData")
