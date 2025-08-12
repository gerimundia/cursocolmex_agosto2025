
# Se omiten las tildes intencionalmente.

# Semana 2 ----

rm(list=ls())
gc()


setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr)


# Tipos de datos en R (continuacion) -----

## Matrices ----
# Arreglo de filas y columnas de valores de una misma clase.
A <- matrix(1:10, ncol=2)
dim(A)
B <- matrix(11:20, nrow=2)
ncol(B)
B

C <- A + A
class(C)
A==C      # Para ver si los elementos de dos matrices son iguales.
colnames(A)
colnames(A) <- c("Columna 1", "Columna 2")
rownames(A) <- c("Fila 1", "Fila 2", "Fila 3", "Fila 4", "Fila 5")
A

# Matrices a partir de vectores
a <- c(1, 2, 3)
b <- c(4, 5, 6)
c <- c(7, 8, 9)
Matriz1 <- cbind(a, b, c)
Matriz2 <- rbind(a, b, c)

# Para enlistar los objetos en el area de trabajo.
ls()

# Para eliminar un objeto en particular.
rm(B)

# Para eliminar todos los objetos excepto uno en particular.
rm( list= ls()[ ls() != "A" ] )

# Para eliminar todos los objetos del área de trabajo.
rm( list = ls())

# __________________________________________________________________________________________________

### Ejercicio propuesto ----

# A partir del vector x <- c(2, 3, 4, 5, 6, 7, 8, 9), generar un vector y que 
# incluya, en orden, dígitos del 0 al 10. 

# Ahora, a partir del vector y, genera un nuevo vector z que incluya 
# los cubos (por ejemplo 2^3) de los valores de y.

# Utilizando los vectores y y z, generar una matriz D, tal que cada vector
# sea una columna de la matriz. 

x <- c(2, 3, 4, 5, 6, 7, 8, 9)
y <- c(0, 1, x, 10)
z <- y^3
D <- cbind(y, z)
  
# __________________________________________________________________________________________________
  
  
## Data frames ----

# Un data frame es un conjunto de vectores de misma longitud 
# y diferentes clases.

# ***** Estructura básica: df[observaciones, variables] *****

a <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
  "Chiapas", "Chihuahua", "Ciudad de México", "Coahuila", "Colima",
  "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
  "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
  "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
  "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

b <- 1:32
estados<-data.frame(id=b, estado=a)
is.data.frame(estados)
class(estados)

# Algunas funciones para explorar un data.frame.
dim(estados)
names(estados)
nrow(estados)
View(estados)
head(estados, 5)
tail(estados)
str(estados)


# Indexacion
estados$estado
estados[, 2]
estados[, "estado"]
estados[1:3, "estado"]

## Listas ----

# Una lista (list) es una colección de objetos (incluyendo data.frames) 
# de cualquier clase.
a <- 1:5
b <- 10:20
c <- 15:30
list(1, 2, a,b,c)

L <- list("Hola mundo", a, estados)
class(L)

names(L) <- c("Valor", "Vector", "Data.frame")
length(L)
str(L)
View(L)



# __________________________________________________________________________________________________

rm(list=ls())

# CPV 2020 -----
# (Cuestionario ampliado) 

# Vamos a descargar los datos a analizar.
viviendas <- read.csv("Viviendas00.csv")
dim(viviendas)
save(viviendas, file="viviendas_cpv2020.RData")

