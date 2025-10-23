
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


# Porcentaje de hogares en viviendas rentadas segun el sexo de la jefatura del hogar

# 1. Viviendas que son rentadas
viviendas$vivrentada <- 0
viviendas$vivrentada[viviendas$p5_1 == 1] = 1

prop.table(table(viviendas$vivrentada))
mean(viviendas$vivrentada)


# Sexo de la jefatura
residentes <- read.csv("TSDEM.csv")

residentes$id <- paste0(residentes$FOLIO,"_",residentes$VIV_SEL)
length(unique(residentes$id))

residentes$id_hog <- paste0(residentes$FOLIO,"_",residentes$VIV_SEL,"_",residentes$HOGAR)
length(unique(residentes$id_hog))

# ¿Cuantos hogares hay por vivienda?
residentes <- residentes %>%
  group_by(id) %>%
  mutate(hogares_por_vivienda = n_distinct(id_hog)) %>%
  ungroup()

View(residentes[, c(1:10, 24:26)])

# Un JH por hogar
table(residentes$PAREN)

names(residentes) <- tolower(names(residentes))
names(residentes)

table(residentes$sexo, useNA = "ifany")

# Variables que necesitamos
viviendas_seleccion <- viviendas %>% 
  select("id", "vivrentada")  

# Uniendo las bases
datos <- left_join(x = residentes, y = viviendas_seleccion, by="id")
View(head(datos, 20))


# Porcentaje de hogares en viviendas rentadas

mean(datos$vivrentada[datos$paren==1])

# Disenio a nivel de hogares
disenio2 <- svydesign(strata = ~est_dis,
                      id = ~upm_dis,  
                      weights = ~factor, 
                      data = datos)

svymean(~vivrentada, subset(disenio2, paren==1), na.rm = T)
svyciprop(~vivrentada, subset(disenio2, paren==1))

# Porcentaje de hogares en viviendas rentadas segun el sexo de la jefatura

svyby(~vivrentada, ~sexo,  subset(disenio2, paren==1), na.rm = T, svymean)
confint(svyby(~vivrentada, ~p2_6,  subset(disenio2, paren==1), na.rm = T, svymean))

barplot(svyby(~vivrentada, ~sexo,  subset(disenio2, paren==1), na.rm = T, svymean))

# En ggplot (previamente, generar una variable de sexo tipo factor)
resultado <- svyby(~vivrentada, ~sexo,  subset(disenio2, paren==1), na.rm = TRUE, svymean)

resultado$ic_inf = resultado$vivrentada - (1.96 * resultado$se)
resultado$ic_sup = resultado$vivrentada + (1.96 * resultado$se)
    
ggplot(resultado) + 
  geom_col(aes(x = sexo, y = vivrentada), fill="orange") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup, x=sexo), width = 0.2) +
  labs(y = "Proporción de hogares en viviendas rentadas", x=NULL) +
  ylim(0,0.8) + theme_minimal()
