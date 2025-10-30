
# Se omiten las tildes intencionalmente.

# Semana 12 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr, survey, ggplot2)
options(scipen=999)
options(survey.lonely.psu = "certainty")

# Datos -----
load("viviendas_envi2020.RData")


# Pruebas de hipotesis -----

## Ejercicio 1 ----
qt(0.025, df = 15)
pt(4.276, df = 15,lower.tail = F)

ic <- function(media, varianza,n, alpha) {
  # Valor critico de t
  t_crit <- qt(1 - alpha/2, df = n-1)
  
  # Error estandar
  ee <- sqrt(varianza) / sqrt(n)
  
  # Intervalo de confianza
  IC_inf <- media - (t_crit * ee)
  IC_sup <- media + (t_crit * ee)
  
  print(c(IC_inf, IC_sup))
  
}

ic(media=25, varianza = 3.5, n=16, alpha = 0.05)

## Ejercicio 2 ---- 
qt(0.05, df = 16)
pt(1.65, df = 16,lower.tail = F)


## PH para proporciones -----

viviendas$vivrentada <- 0
viviendas$vivrentada[viviendas$p5_1 == 1] = 1

prop.table(table(viviendas$vivrentada[viviendas$cve_ent=="08"]))

# Z critico
qnorm(0.025, lower.tail = F)

# p-valor
pvalor = 2*pnorm(6.54, lower.tail = FALSE)


# Pruebas de hipotesis considerando el disenio muestral -----

# Ho: La renta promedio es igual a 0)
svyttest(renta~0, disenio, na = TRUE)

# Ho: la media de la renta nacional = 5,000 pesos mensuales
svyttest(I(renta - 5000) ~ 0, disenio, na.rm = TRUE)

# Ho: la media de la renta nacional = 2,800 pesos mensuales
svyttest(I(renta - 2800) ~ 0, disenio, na.rm = TRUE)

# Ho: la diferencia de las medias = 0 (i.e., la renta promedio urbano es igual a la rural)
# ttest
svyttest(renta ~ area, disenio,  na.rm = T)
# Se rechaza la Ho.

# Ho: la media de la renta de Chihuahua es igual a la nacional
svymean(~renta, disenio, na.rm = T)

svyttest(I(renta - 2785.2) ~ 0, 
         subset(disenio, cve_ent=="08"), na.rm = TRUE)

# Ho: la proporcion de viviendas rentadas en Chihuahua es igual a la nacional
svymean(~vivrentada, disenio, na.rm = T)

svyttest(I(vivrentada - 0.16399) ~ 0, 
         subset(disenio, cve_ent=="08"), na.rm = TRUE)

# Estimacion puntual
svymean(~vivrentada, subset(disenio, cve_ent=="08"), na.rm = T)

# Intervalo de confianza
confint(svymean(~vivrentada, subset(disenio, cve_ent=="08"), na.rm = T), level = 0.95)
