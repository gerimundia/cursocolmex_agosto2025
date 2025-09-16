
# Se omiten las tildes intencionalmente.

# Semana 1 ----

# Hola mundo. Esto es un comentario. 
# CTRL + Shift + C
# Tip: En algunas ocasiones, los scripts no reconocen, en nuevas sesiones, el uso de 
# acentos, enies o dieresis.

# ---- Permite generar secciones para navegar a traves del codigo.
# CTRL + Shift + R

# Nuevo script: 
# Ctrl + Shift + N

# Guardar script: 
# Ctrl + S

## Configuracion ----

# Vamos a limpiar el ambiente (es decir retirar los objetos del ambiente de trabajo) y 
# liberar espacio en memoria.

rm(list=ls())
gc()


# Vamos a definir nuestro lugar de trabajo, es decir la carpeta en la que vamos 
# a trabajar y en donde se alojan todos los archivos.

# Notar que la ruta está en formato texto.

setwd("C:/curso_estadistica_basica/taller/")

# Verifiquemos
getwd()
dir()


## Carga de librerias ----
# Informacion de la sesion y paquetes cargados en memoria
sessionInfo()

# Paquetes en el disco
library()

# Cargar paquetes en memoria
library("foreign")

# Documentacion del paquete.
help(package="foreign")
library(help="foreign")

# Para citar un paquete.
citation("foreign")

# Para instalar paquetes en el disco por primera vez.
install.packages("ggplot2")

# Para cargar paquetes.
library(ggplot2)

# Otra forma:
# Por unica vez, instalaremos el paquete pacman. 

# install.packages("pacman")

pacman::p_load(dplyr, data.table, survey, viridis)


# Y en caso que sus gatitos (o perritos) tambien quieran aprender R:
# www.rforcats.net (desarrollado por Scott Chamberlain)


# Tipos de datos en R -----

# Generando objetos: valores, vectores, matrices

## Valores ----
# Numérico, caracter (texto) o lógico (Verdadero o Falso)
valor <- 2
a <- pi
b <- "hola"
c <- T

is.numeric(a)
is.integer(a)
is.logical(c)
is.character(c)

class(a)

# Recordatorio: Para conocer mas sobre una funcion, o para 
# entender su estructura y argumentos:
# help(class) o ?class

# Es posible generar valores a partir de otros previamente generados.
aa <- a * 2
aa <- a + 5  # Notar que el valor se reescribe.
pi2 <- round(a,2)
c2 <- as.numeric(c)    # Se forzo al valor logico en transformarse en numerico.

## Vectores ----
# Arreglo de una fila o columna de valores de una misma clase.
# Función c para combinar items y generar vectores.

y <- letters
length(y)
class(y)

z <- c(T, F, T, F)
length(z)
class(z)

x <- c(2, 3, 4, 5, 6, 7, 8, 9)
length(x)
class(x)

# Sobre vectores numericos podemos calcular medidas de 
# tendencia central y de dispersion.
mean(x)
sd(x)

# Es posible generar vectores a partir de otros objetos previamente generados.
xx <- c(x, x)
class(xx)  # Vector numérico

# Sobre vectores numericos es posible usar operaciones y funciones numericas.
mean(xx)
max(xx)
sd(xx)
summary(xx)

# Creando vectores a partir de secuencias
1:5
seq(1,5, by=1)

5:-5
seq(5,-5, by=-1)

vector1 <- seq(1,5, by=0.5)  
# En R se utiliza la nomenclatura inglesa de punto para decimal.
length(vector1)
