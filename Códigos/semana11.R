
# Se omiten las tildes intencionalmente.

# Semana 11 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr, survey, ggplot2)
options(scipen=999)
options(survey.lonely.psu = "certainty")

# Datos -----
load("viviendas_envi2020.RData")


# Estadistica descriptiva -----

## 1. Variables de intervalo -----

# Gasto en arrendamiento

# Total de viviendas particulares habitadas
viviendas$unos <- 1

disenio <- svydesign(strata = ~est_dis,
                     id = ~upm_dis,  
                     weights = ~factor, 
                     data = viviendas)

svytotal(~unos, disenio)

# Renta a nivel nacional
media_nacional <- svymean(~renta, disenio, na.rm = T)
svyvar(~renta, disenio, na.rm = T)
sqrt(svyvar(~renta, disenio, na.rm = T))

# Error estandar de la media
SE(media_nacional)

# Coeficiente de variacion
cv(media_nacional)

svyhist(~renta, disenio)
svyhist(~renta, disenio, probability = FALSE)

plot(svysmooth(~renta, disenio))


# Renta en CDMX
svymean(~renta, subset(disenio, cve_ent=="09"), na.rm = T)


# Renta por area de residencia
svyby(~renta, ~area, disenio, na.rm = T, svymean)
barplot(svyby(~renta, ~area, disenio, na.rm = T, svymean))

svyby(~renta, ~area, disenio, na.rm = T, svyquantile, quantiles=c(0.25, 0.50, 0.75))

# Cual es el rango intercuartil en el area urbana?
3000 - 1250

svyboxplot(renta ~ area, disenio)

# Intervalos de confianza
confint(svymean(~renta, disenio, na.rm = T), level = 0.95)
confint(svyby(~renta, ~area, disenio, na.rm = T, svymean))


## 2. Proporciones -----

# Porcentaje de viviendas rentadas

viviendas$vivrentada <- 0
viviendas$vivrentada[viviendas$p5_1 == 1] = 1

prop.table(table(viviendas$vivrentada))

disenio <- svydesign(strata = ~est_dis,
                     id = ~upm_dis,  
                     weights = ~factor, 
                     data = viviendas)

# Total de viviendas en renta
svytotal(~vivrentada, disenio)

# Descriptivos de la proporcion de viviendas rentadas
svymean(~vivrentada, disenio, na.rm = T)
svyvar(~vivrentada, disenio, na.rm = T)
sqrt(svyvar(~vivrentada, disenio, na.rm = T))

barplot(svyby(~vivrentada, ~area, disenio, na.rm = T, svymean))

# Intervalos de confianza de la proporcion media
svyciprop(~vivrentada, disenio)

# Renta por area de residencia
svyby(~vivrentada, ~cve_ent, disenio, na.rm = T, svymean)


## 3. Variables categoricas -----

# Tipo de tenencia de la vivienda

viviendas$tenencia <- NA
viviendas$tenencia[viviendas$p5_1 == 1] = 0
viviendas$tenencia[viviendas$p5_1 == 2 | viviendas$p5_1 == 3 | 
                   viviendas$p5_1 == 6 | viviendas$p5_1 == 7] = 1
viviendas$tenencia[viviendas$p5_1 == 4 | viviendas$p5_1 == 5] = 2

prop.table(table(viviendas$tenencia, useNA = "ifany"))

viviendas$tenencia <- factor(viviendas$tenencia, levels = c(0:2), 
                             labels = c("Rentada","Prestada y otras","Propia"))

prop.table(table(viviendas$tenencia))

disenio <- svydesign(strata = ~est_dis,
                     id = ~upm_dis,  
                     weights = ~factor, 
                     data = viviendas)

svytable(~tenencia, disenio)

svytable(~tenencia + area, disenio)

