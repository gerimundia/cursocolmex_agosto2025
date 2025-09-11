
# Se omiten las tildes intencionalmente.

# Semana 6 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")

pacman::p_load(dplyr, questionr, ggplot2)


# Datos ----
load("agua_fronterasur.RData")


# S6-----

# Datos

aguadentrovivienda <- agua %>%
  group_by(nom_abr) %>%
  summarise(
    viviendas = sum(factor, na.rm = TRUE),
    viviendas_aguadentroviv  = sum(factor[fuenteagua == "dentroviv_serpublico"], na.rm = TRUE), 
    edadpromediojf = weighted.mean(edadjh, w=factor, na.rm = TRUE))

aguadentrovivienda$viviendasmillon <- aguadentrovivienda$viviendas /1000000

aguadentrovivienda$porcentaje <- round(
  (aguadentrovivienda$viviendas_aguadentroviv/aguadentrovivienda$viviendas*100),1)

# Gramatica de ggplot2 ----

## Grafico de barras 1 ---- 
ggplot(aguadentrovivienda) +
  geom_col(aes(x = reorder(nom_abr, viviendasmillon), y = viviendasmillon),
           fill = "firebrick3") +
  labs(x = NULL, 
       y = "Número de viviendas (en millones)",
       title = "Número de viviendas particulares habitadas por entidad federativa") 


## Grafico de barras 2 ---- 
ggplot(aguadentrovivienda) +
  geom_col(aes(x = reorder(nom_abr, porcentaje), y = porcentaje),
           fill = "steelblue") +
  coord_flip() + 
  labs(x = NULL, 
       y = "Porcentaje",
       title = "Porcentaje de viviendas con agua dentro de la vivienda abastecida por el servicio público") 


## Grafico de dispersion ---- 
ggplot(aguadentrovivienda) +
  geom_point(aes(x = edadpromediojf, y = porcentaje),
             color = "darkslategray", size = 3) + 
  labs(x = "Edad promedio de jefatura",
       y = "Porcentaje") +
  ylim(0, 100) 


## Grafico de caja ----
agua %>% 
  filter(is.na(fuenteagua)==F) %>% 
  ggplot() +
  geom_boxplot(aes(x = fuenteagua, y = edadjh, fill = fuenteagua),
               color = "gray20") +
  scale_fill_manual(values = c("darkorange2", "darkolivegreen3", "gold", 
                               "mediumpurple", "lightseagreen")) +
  labs(x = "Abastecimiento de agua",
       y = "Edad de la jefatura del hogar")




# Personalizacion de componentes -----

## Grafico de barras ----

# Datos
aguaxarea <- prop.table(
  wtd.table(agua$fuenteagua, agua$area, weights = agua$factor),
  margin=2)

aguaxarea <- round((aguaxarea*100), 1)
aguaxarea <- data.frame(aguaxarea)
names(aguaxarea) <- c("fuentedeagua","area","porcentaje")

ggplot(aguaxarea) +
  geom_col(aes(x=area, y=porcentaje, fill=fuentedeagua)) +
  labs(x = "Área de residencia", 
       y = "Porcentaje de viviendas", 
       fill = "Tipo de abastecimiento de agua",
       caption = "Fuente: INEGI, CPV 2020, Cuestionario ampliado.") +
  # title = "Título xxxxx", 
  # subtitle = "Subtítulo xxxxx") + 
  scale_fill_manual(
    values = c("darkorange2", "darkolivegreen3", "gold", "mediumpurple", "lightseagreen"),
    labels = c("Dentro, servicio público",
               "Dentro, otra fuente",
               "Fuera, servicio público",
               "Fuera, otra fuente",
               "No entubada")) +
  theme(
    axis.title = element_text(size=10, color="gray20"),
    axis.text = element_text(size=8, color="gray30"),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
    legend.position = "right",
    legend.text = element_text(size=8, color="gray30"),
    legend.title = element_text(size=9, color="gray10"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0, 
                                face = "italic",
                                # otras opciones: "plain", "bold", "bold.italic"
                                family = "serif"
                                # otras opciones: "serif", "mono", "sans"
    ))

# Guardando los data.frames.
save(agua, aguaxarea, file="agua_fronterasur.RData")
