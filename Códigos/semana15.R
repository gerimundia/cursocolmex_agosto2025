
# Se omiten las tildes intencionalmente.

# Semana 15 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")

pacman::p_load(dplyr, ggplot2, moments)
options(scipen=999)


# Internet en funcion de la asistencia escolar -----
load("internet.RData")
head(datos)

# Previo al modelo, realizaremos un analisis exploratorio de los datos. 

# Posteriormente, identificar si tenemos datos validos en las variables que 
# intervienen en emodelo1

datos$validos <- complete.cases(datos[, c("internet","porc_asist_20_24")])
table(datos$validos)
prop.table(table(datos$validos))

# Es importante hacer un analisis exploratorio de ambos grupos para 
# verificar si hay imformacion importante que dejamos de ver en el modelo. 

# Este paso lo podemos hacer porque es censo. Recordar en los casos de 
# encuestas con muestras complejas. 
datosok <- datos[datos$validos==T, ]

# El acceso a internet es modelado como funcion de la asistencia escolar.
modelo1 <- lm(internet ~ porc_asist_20_24, data=datosok)

class(modelo1)
modelo1
names(modelo1)

# Coeficientes del modelo
coef(modelo1)
confint(modelo1)

# Mas informaciones sobre los coeficientes del modelo
summary(modelo1)

# La pendiente del modelo de RLS es igual a la razón entre cov(x,y) y var(x).
coef(modelo1)[2]
cov(datosok$internet, datosok$porc_asist_20_24) / var(datosok$porc_asist_20_24)

# Cuadrado del coeficiente de correlación de x y y.
cor(datosok$internet, datosok$porc_asist_20_24)^2
summary(modelo1)

# Predicciones
datosok$y_estimado <- fitted.values(modelo1)
# Otra forma: predict(modelo1) 

# Errores (residuales) 
datosok$erroresmanual <- datosok$internet - datosok$y_estimado
datosok$errores <- residuals(modelo1)

sum(datosok$errores)
mean(datosok$errores)

# Normalidad
hist(datosok$errores)

# Plot Q-Q (grafico cuantil-cuantil)
qqnorm(datosok$errores)
qqline(datosok$errores, col = "red")

# Prueba de Shapiro-Wilk 
# H0: Los residuos siguen una distribucion normal.
shapiro.test(datosok$errores)
# Si p < 0.05 → Rechazamos H0 (residuos NO normales)

# Outliers o valores extremos
boxplot(datosok$errores, main = "Boxplot de Residuos")

skewness(datosok$errores)  
kurtosis(datosok$errores) 

# La no normalidad afecta principalmente a las pruebas de hipotesis y 
# los intervalos de confianza, no tanto a las predicciones.


# Precios  -----
load("precios.RData")
head(precios)

precios$validos <- complete.cases(precios[, c("price","sqrft")])
table(precios$validos)

# Regresion lineal simple
# El precio de la vivienda explicado linealmente por el area.
modelo2 <- lm(price ~ sqrft, data=precios)

# Coeficientes del modelo
coef(modelo2)
confint(modelo2)
summary(modelo2)

# H0: Bk = 0
# Estadistico de prueba: tobs = B/ee
# Region de rechazo: tobs > tcrit
summary(modelo2)

11.20414 / 24.74261
0.14021  /  0.01182
qt(0.025, df = 86)

# Usando valor p
2*pnorm((11.20414 / 24.74261), lower.tail = FALSE)
2*pnorm((0.14021  /  0.0118), lower.tail = FALSE)

# Predicciones
summary(precios$sqrft)
datos <- data.frame(sqrft=seq(1000, 5000, by=5))
y_estimado <- predict(modelo2, newdata=datos, interval="confidence")

datos <- cbind(datos, as.data.frame(y_estimado))
names(datos) <- c("sqrft","precios","ic_inf","ic_sup")
head(datos)

ggplot() +
  geom_point(data=precios, aes(sqrft, price), 
             shape=20, size=3, colour="steelblue") +
  geom_line(data=datos, aes(sqrft, precios), colour="firebrick", linewidth=1) +
  geom_ribbon(data=datos, aes(x=sqrft, ymin=ic_inf, ymax=ic_sup), 
              alpha=0.2, fill="firebrick1") +
  labs(x="Área de la vivienda (pies al cuadrado)", 
       y="Precios (miles de dólares)") +
  theme_minimal()

# Verificar linealidad y homoscedasticidad
plot(modelo2, which=1)
