
# Se omiten las tildes intencionalmente.

# Semana 5 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr, questionr)

# Datos ----
load("agua_fronterasur.RData")

# Exploracion ----

#### Numero de viviendas por area de residencia en la muestra
table(agua$area)

#### Porcentaje de viviendas por area de residencia en la muestra
prop.table(table(agua$area))

# Opcion 1 (questionr)
t1 <- wtd.table(agua$area, weights = agua$factor)
class(t1)
prop.table(t1)

# Opcion 2 (dplyr)
t3 <- count(x = agua, area, wt = factor)
class(t3)
t3$porcentaje <- t3$n / sum(agua$factor)
t3

#### Total de viviendas habitadas particulares por abastecimiento de agua
table(agua$fuenteagua, useNA = "ifany")
prop.table(table(agua$fuenteagua, useNA = "ifany"))

aux1 <- wtd.table(agua$fuenteagua, weights = agua$factor)
abastecimiento <- data.frame(aux1)
abastecimiento$porcentaje <- round((abastecimiento$Freq / sum(abastecimiento$Freq))*100, 1)
abastecimiento

#### Total de viviendas habitadas particulares por abastecimiento de agua y area de residencia

# Cuadro de contingencia o de doble entrada
aux2 <- wtd.table(agua$fuenteagua, agua$area, weights = agua$factor)
aux2

# Distribucion conjunta
# Distribucion de las proporciones de todas las intersecciones de X y Y 
# en la tabla de contingencia.
prop.table(aux2)

# Distribucion marginal
# Distribucion de cada una de las variables de la tabla de contingencia.

# Distribucion marginal por fila
prop.table(wtd.table(agua$fuenteagua, weights = agua$factor))

# Distribucion marginal por columna
prop.table(wtd.table(agua$area, weights = agua$factor))

# Distribucion condicional
# Distribucion de una de las variables de la tabla de contingencia dada 
# una categoria de la variable restante.

# Distribucion condicional por fila
prop.table(aux2, margin = 1)

# Distribucion condicional por columna
prop.table(aux2, margin = 2)

#### Total de viviendas habitadas particulares por abastecimiento de agua y entidad
aux2 <- wtd.table(x=agua$fuenteagua, y=agua$area,  weights = agua$factor)

abastecimiento_area_fila <- round(prop.table(aux2, margin=1)*100,1)
abastecimiento_area_fila

abastecimiento_area_columna <- round(prop.table(aux2, margin=2)*100,1)
abastecimiento_area_columna

#### Total de viviendas habitadas particulares por abastecimiento de agua y entidad
aux3 <- wtd.table(x=agua$nom_ent, y=agua$fuenteagua,  weights = agua$factor)
abastecimiento_estatal <- round(prop.table(aux3, margin=1)*100,1)
View(abastecimiento_estatal)

write.csv(abastecimiento_estatal, file="abastecimiento_estatal.csv")



# Introduccion a visualizacion ----

# Datos ----
aguadentrovivienda <- agua %>%
  group_by(nom_abr) %>%
  summarise(
    viviendas = sum(factor, na.rm = TRUE),
    viviendas_aguadentroviv  = sum(factor[fuenteagua == "dentroviv_serpublico"], na.rm = TRUE), 
    edadpromediojf = weighted.mean(edadjh, w=factor, na.rm = TRUE))

# Base (plot y variaciones) ----

## Grafico de barras 1 ---- 
ord1 <- order(aguadentrovivienda$viviendas, decreasing = TRUE)
summary(aguadentrovivienda$viviendas)
aguadentrovivienda$viviendasmillon <- aguadentrovivienda$viviendas /1000000

barplot(aguadentrovivienda$viviendasmillon[ord1],
        names.arg = aguadentrovivienda$nom_abr[ord1],
        ylim = c(0, 2),
        col = "firebrick3",
        border = NA, 
        las = 2,
        ylab = "Número de viviendas (millones)",
        main = "Número de viviendas particulares habitadas")


## Grafico de barras 2 ---- 

aguadentrovivienda$porcentaje <- round(
  (aguadentrovivienda$viviendas_aguadentroviv/aguadentrovivienda$viviendas*100),1)

ord2 <- order(aguadentrovivienda$porcentaje, decreasing = TRUE)

barplot(aguadentrovivienda$porcentaje[ord2],
        names.arg = aguadentrovivienda$nom_abr[ord2],
        col = "steelblue",
        border = NA, 
        horiz = TRUE,
        las = 1,
        xlim = c(0, 100),
        family = "serif", 
        xlab = "Porcentaje",
        main = "Porcentaje de viviendas con agua dentro de la vivienda abastecida por el servicio público")

# las = 0: orientación paralela al eje (la predeterminada).
# las = 1: etiquetas horizontales.
# las = 2: etiquetas perpendiculares al eje.
# las = 3: etiquetas verticales.
# family: tipo de letra (sans, mono, serif).

## Grafico de dispersion ---- 

plot(aguadentrovivienda$edadpromediojf, aguadentrovivienda$porcentaje,
     xlab = "Edad promedio de jefatura",
     ylab = "Porcentaje",
     pch = 19, 
     cex = 1.5,
     ylim = c(0, 100),
     col = "darkslategray")


## Grafico de caja ----
boxplot(edadjh ~ fuenteagua, data = agua, 
        xlab = "Abastecimiento de agua", 
        ylab = "Edad de la jefatura del hogar", 
        col = c("darkorange2", "darkolivegreen3", "gold", 
                "mediumpurple", "lightseagreen"),
        border = "gray20", 
        cex.axis = 0.5)


