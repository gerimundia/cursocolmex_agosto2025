
# Se omiten las tildes intencionalmente.

# Semana 9 ----


rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr, ggplot2)
options(scipen=999)


# Datos ----

load("informales.RData")

paises <- c("Argentina","Belize","Bolivia (Plurinational State of)",
            "Brazil","Chile",
            "Colombia","Costa Rica","Ecuador","El Salvador",
            "Guatemala","Guyana","Honduras","Mexico","Nicaragua",
            "Panama","Paraguay","Peru","Suriname","Uruguay",
            "Venezuela (Bolivarian Republic of)")

informales_la <- informales %>% 
  filter(pais_nombre %in% paises) %>% 
  select(pais_nombre, anio, poblacion)


# Funciones ----
pesosadolares <- function(pesos) {
  print(pesos/18.71)
}

pesosadolares(20)

pesosadolares2 <- function(pesos) {
  print(paste(pesos, "pesos mexicanos es igual a", pesos/18.71, 
              "dólares estadounidenses, al cambio del día", Sys.Date()))
}

pesosadolares2(20)

lanzamientomoneda <- function(numerolanzamientos) {
  ladosmoneda <- c("cara", "cruz")
  sample(ladosmoneda, replace=T, size=numerolanzamientos)
}

lanzamientomoneda(15)

exploraciondf <- function(x) {
  print(class(x))
  print(dim(x))
  print(names(x))
  print(head(x, 2))
  print(tail(x, 2))
}

exploraciondf(informales_la)

promedio <- function(x) 
{
  # print(paste("El promedio anual de personas que viven en espacios informales es:", as.character(mean(x, na.rm=T))))
  # print(paste("El promedio anual de personas que viven en espacios informales es:", 
  #             as.character(round(mean(x, na.rm=T),0))))
  print(paste("El promedio anual de personas que viven en espacios informales en América Latina es:", 
              as.character(round(mean(x, na.rm=T),0)),
              "miles."))
}

informales_la %>% 
  filter(anio==2010) %>% 
  summarise(promedio(poblacion))


# Loop -----
x = c(1:10)

for  (i in x) {
  print(i)
}

for  (i in x) {
  a = i*i
  print(a)
}

pesos = seq(100,1000, by=100)

for  (p in pesos) {
  pesosadolares2(p)
  Sys.sleep(2)
  message("----------- Calculando ----------- ")
}


# Grafico para Argentina
plot <- informales_la %>% 
  filter(pais_nombre=="Argentina") %>% 
  ggplot() +
  geom_line(aes(x=anio, y=poblacion), color = "darkgoldenrod", linewidth=1) +
  geom_point(aes(x=anio, y=poblacion), color = "darkgoldenrod4", size=2) +
  labs(title = paste("Población en asentamientos informales:", "Argentina"), 
       y="Población (miles)", 
       x=NULL) +
  theme_minimal()

ggsave(plot = plot, filename = paste0("grafico_grupo_", "Argentina", ".jpg"))


for (p in paises) {
  plot <- informales_la %>% 
    filter(pais_nombre==p) %>% 
    ggplot() +
    geom_line(aes(x=anio, y=poblacion), color = "darkgoldenrod", linewidth=1) +
    geom_point(aes(x=anio, y=poblacion), color = "darkgoldenrod4", size=2) +
    labs(title = paste("Población en asentamientos informales:", p), 
         y="Población (miles)", x=NULL) +
    theme_minimal()
  
  ggsave(plot = plot, filename = paste0("grafico_grupo_", p, ".jpg"))
  
  message(paste("Se ha guardado el gráfico de:", p))
}

