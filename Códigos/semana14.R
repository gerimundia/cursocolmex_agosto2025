
# Se omiten las tildes intencionalmente.

# Semana 14 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(ggplot2, corrplot)
options(scipen=999)


# Cuarteto de Anscombe -----
datos <- datasets::anscombe
head(datos)

mean(datos$x1); var(datos$x1)
mean(datos$y1); var(datos$y1)

cov(datos$x2, datos$y2)
cor(datos$x2, datos$y2)

ggplot(datos, aes(x = x3, y = y3)) +
  geom_point(size = 3, color = "deeppink2") +
  theme_minimal()


# Matriz de correlacion -----
load("estatales_cpv2020.RData")
head(datos_cpv2020)
cor(datos_cpv2020[, c(2:6)])

matrizcorr <- cor(datos_cpv2020[, c(2:6)])
corrplot(matrizcorr, diag = F, type = "upper")


# Prueba de hipotesis -----
correlacion <- cor(datos_cpv2020$drenaje, datos_cpv2020$afiliacion)

# Manualmente
n <- nrow(datos_cpv2020)
t_prueba <- correlacion * (sqrt((n-2)/(1-(correlacion*correlacion))))
qt(0.025, df = n-2)

pvalor <- 2*pt(t_prueba, df = n-2,lower.tail = F)

cor.test(datos_cpv2020$drenaje, datos_cpv2020$afiliacion)
# No se puede rechazar que la correlacion es igual a cero.
# No hay evidencia de una relacion lineal entre drenaje y afiliacion.


ggplot(datos_cpv2020) +
  geom_point(aes(x = drenaje, y = afiliacion),
             size = 3, colour = "deeppink2") +
  theme_minimal()

