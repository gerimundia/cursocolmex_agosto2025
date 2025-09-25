
# Se omiten las tildes intencionalmente.

# Semana 8 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(readxl, dplyr, ggplot2, scales)
options(scipen=999)


# Datos ----

# https://data.unhabitat.org/ 
# datasets
# Housing, slums and informal settlements
# Total urban population living in slums, informal settlements or inadequate housing by Country

informales <- read_xlsx("SDG_11-1-1_total_population_in_slums_and_informal_settlements.xlsx",
                        range = "B1:I4331", col_names = T)

names(informales)
head(informales)

informales <- informales %>%
  select(-c(1,2)) %>% 
  rename(pais_codigo = `Country or Territory code`, 
         pais_nombre = `Country or Territory Name`,
         ods_region = `SDG Region`,
         ods_subregion = `SDG Sub-Region`,
         anio = `Data Reference Year`,
         poblacion = `Urban population living in slums or informal settlements (thousands)`)

save(informales, file="informales.RData")

# Varias geometrias -----

ggplot() +
  geom_col(data = informales[is.na(informales$pais_nombre)==T & 
                  informales$ods_region=="Latin America and the Caribbean", ], 
           aes(x=anio, y=poblacion, 
               fill = "América Latina y el Caribe"), alpha=0.8) +
  geom_line(data = informales[informales$pais_nombre=="Mexico", ], 
            aes(x=anio, y=poblacion, colour="México"), linewidth=1.4) +
  scale_fill_manual(values = c("América Latina y el Caribe" = "indianred")) +
  scale_colour_manual(values = c("México" = "mediumpurple4")) +
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) +
  labs(y = "Población (miles)", x = NULL, fill=NULL, colour=NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=10))

# Mapa 1 -----
pacman::p_load(leaflet, rnaturalearth, rnaturalearthdata, sf, RColorBrewer)

# Geometrias de Mexico (nivel 1 = estados)
mexico <- ne_states(country = "Mexico", returnclass = "sf")

leaflet(mexico) %>%
  addProviderTiles("CartoDB.Positron") %>%   
  addPolygons(fillColor = "lightblue", color = "black",              
              weight = 1, popup = ~name)    

# Supongamos que pegaron algun indicador por estado (por ejemplo, usando left_join).
mexico <- mexico %>%
  mutate(indicadorx = runif(n(), min = 1, max = 20))

display.brewer.all()
pal <- colorNumeric(palette = "YlOrRd", domain = mexico$indicadorx)
pal <- colorNumeric(palette = "OrRd", domain = mexico$indicadorx)

leaflet(mexico) %>%
  addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(
    fillColor = ~pal(indicadorx),   
    color = "gray80",                        
    weight = 1, opacity = 1, fillOpacity = 0.7,
    popup = ~paste0("<b>", name, "</b><br>",
                    "Indicadorx: ", round(indicadorx, 1)))  %>%
  addLegend(
    pal = pal,
    values = ~indicadorx,
    position = "bottomleft",
    title = "Indicadorx"
  )

# Mapa 2 -----

pacman::p_load(viridis, maps)


## Mapa base ----
mapa <- map_data('world')

la <- c("Argentina","Belize","Bolivia","Brazil","Chile",
        "Colombia","Costa Rica","Ecuador","El Salvador",
        "Guatemala","Guyana","Honduras","Mexico","Nicaragua",
        "Panama","Paraguay","Peru","Suriname","Uruguay","Venezuela")

t1 <- data.frame(region=la, seleccionado=1)
head(t1)
head(mapa)

mapa1 <- left_join(mapa, t1, by="region")
head(mapa1)

mapa1 %>% 
  filter(is.na(mapa1$seleccionado)==F) %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group, fill=seleccionado), 
               color="gray70", fill="cornsilk") 

## Datos -----
seleccion_la <- informales %>% 
  filter(ods_subregion == "South America" | ods_subregion == "Central America") %>% 
  filter(anio==2022) %>% 
  select(c("pais_nombre","poblacion"))

head(seleccion_la)
table(seleccion_la$pais_nombre)

mapa1 <- left_join(mapa1, seleccion_la, by=c("region"="pais_nombre"))
head(mapa1)

summary(mapa1$poblacion)

## Mapa ----
mapa1 %>% 
  filter(is.na(mapa1$seleccionado)==F) %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group, fill=poblacion), 
               color="gray60") +
  # scale_fill_viridis_c(na.value = "gray80")
  scale_fill_continuous(name = "Población (miles)",
                        low = "palegreen",
                        high = "olivedrab4",
                        breaks = seq(0, 20000, by = 5000),
                        labels = comma_format(big.mark = ",", decimal.mark = ".")) + 
theme(legend.position = "left") +
labs(y = NULL, x = NULL) 
