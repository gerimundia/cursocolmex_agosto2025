
# Se omiten las tildes intencionalmente.

# Semana 10 ----


rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr, survey, ggplot2)
options(scipen=999)




# Ley de los Grandes Numeros -----

# Semilla de aleatorizacion 
set.seed(123)

# Muestra grande de numeros aleatorios (distribucion uniforme)
n_total <- 10000
muestra <- runif(n_total, min = 0, max = 1)  

# La media teorica es igual a 0.5.
media_teorica <- rep(0.5, n_total)

# ¿Que ocurre con la media acumulada a medida que n crece?
medias_acumuladas <- cumsum(muestra) / (1:n_total)
head(medias_acumuladas, 10)
tail(medias_acumuladas, 10)

# Visualmente
muestra <- data.frame(
  n = 1:n_total,
  media = medias_acumuladas
)

ggplot(muestra, aes(x = n, y = media)) +
  geom_line(color = "dodgerblue4", size = 1) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(title = "Ley de los Grandes Números",
       x = "Tamaño de la muestra (n)",
       y = "Media muestral") +
  theme_minimal(base_size = 14)




# Datos -----

# ENVI 2020

viviendas <- read.csv("envi2020/TVIVIENDA.csv")

class(viviendas)
dim(viviendas)
names(viviendas)

# Identificador unico de la vivienda
viviendas$id <- paste0(viviendas$FOLIO,"_",viviendas$VIV_SEL)
length(unique(viviendas$id))

# Clave de entidad
viviendas$cve_ent <- as.character(viviendas$ENT)

viviendas$cve_ent <- ifelse(nchar(viviendas$cve_ent) == 1,
                            paste0("0", viviendas$cve_ent),
                            viviendas$cve_ent)

head(viviendas[, c("ENT","cve_ent")], 10)
tail(viviendas[, c("ENT","cve_ent")], 10)
table(viviendas$cve_ent, useNA = "ifany")

# Area de residencia
# Urbano: localidad de 2500 habitantes y mas.
# 0: Urbano
# 1: Rural
viviendas$area <- ifelse(viviendas$TLOC == 4, 1, 0)
viviendas$area <- factor(viviendas$area, levels = c(0,1), labels = c("Urbano", "Rural"))
table(viviendas$area, useNA = "ifany")


# Seleccion de variables
viviendas <- viviendas %>% 
  select("cve_ent","id","EST_DIS","UPM_DIS","FACTOR","TLOC","area",
         "P1_1","P4_3","P4_4","P4_5","P4_6","P4_9","P4_11","P4_13",
         "P4_15","P4_16","P4_17","P4_25_1","P4_25_2","P4_25_3","P4_25_4",
         "P4_25_5","P4_25_6","P4_25_7","P5_1","P5_2")


names(viviendas) <- tolower(names(viviendas))


# Gasto en arrendamiento -----

## Inferencia estadistica -----

summary(viviendas$p5_2)
str(viviendas$p5_2)
hist(viviendas$p5_2)

viviendas$renta <- viviendas$p5_2
viviendas$renta[viviendas$renta >= 999999888] = NA
viviendas$renta[viviendas$p5_1 != 1] = NA

# Sin considerar el DM de la encuesta
summary(viviendas$renta)
var(viviendas$renta, na.rm = T)
sd(viviendas$renta, na.rm = T)

viviendas %>% 
  summarise(
    Media = mean(renta, na.rm = T), 
    Mediana = median(renta, na.rm = T),
    Varianza = var(renta, na.rm = T),
    DesEst = sd(renta, na.rm = T)
  )

## Considerando diseño muestral ----
disenio <- svydesign(strata = ~est_dis,
                     id = ~upm_dis,  
                     weights = ~factor, 
                     data = viviendas)

options(survey.lonely.psu = "certainty")
# fail (por default), remove, adjust


# Guardar ----
save(viviendas, disenio, file="viviendas_envi2020.RData")

svymean(~renta, disenio, na.rm = T)
svyvar(~renta, disenio, na.rm = T)
sqrt(svyvar(~renta, disenio, na.rm = T))

# Promedio de renta en CDMX
svymean(~renta, subset(disenio, cve_ent=="09"), na.rm = T)


# Intervalos de confianza

# Construyendo manualmente el intervalo de confianza con un nivel del 95% de confianza:
# ls: 2785.2+(1.96*55.8)
svymean(~renta, disenio, na.rm = T)

confint(svymean(~renta, disenio, na.rm = T))

# Renta por area de residencia
svyby(~renta, ~area, disenio, na.rm = T, svymean)
confint(svyby(~renta, ~area, disenio, na.rm = T, svymean))
