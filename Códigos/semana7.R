
# Se omiten las tildes intencionalmente.

# Semana 7 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(dplyr, ggplot2, viridis)


# Datos ----
load("agua_fronterasur.RData")


# Grafico ----
ggplot(aguaxarea) +
  geom_col(aes(x=area, y=porcentaje, fill=fuentedeagua)) +
  labs(x = "Área de residencia", 
       y = "Porcentaje de viviendas", 
       fill = "Tipo de abastecimiento de agua",
       caption = "Fuente: INEGI, CPV 2020, Cuestionario ampliado.") +
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


# Puedo guardar mi tema para futuros usos
temapersonal1 <- theme(
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
                              family = "serif"))

## Paneles ----

# Datos
mujeres <- agua %>%
  filter(sexojh=="Mujer" & is.na(fuenteagua)==F) %>% 
  group_by(area, fuenteagua) %>%
  summarise(total = sum(factor)) %>%
  mutate(porcentaje = 100 * total / sum(total),
         sexo = "Mujeres")

hombres <- agua %>%
  filter(sexojh=="Hombre" & is.na(fuenteagua)==F) %>% 
  group_by(area, fuenteagua) %>%
  summarise(total = sum(factor)) %>%
  mutate(porcentaje = 100 * total / sum(total),
         sexo = "Hombres")

resumen <- rbind(mujeres, hombres)

ggplot(resumen) +
  geom_col(aes(x=area, y=porcentaje, fill=fuenteagua)) +
  labs(x = "Área de residencia", 
       y = "Porcentaje de viviendas", 
       fill = "Tipo de abastecimiento de agua") +
  facet_grid(cols = vars(sexo)) +
  scale_fill_viridis_d(
    labels = c("Dentro, servicio público", 
               "Dentro, otra fuente", 
               "Fuera, servicio público", 
               "Fuera, otra fuente", 
               "No entubada"),
    option = "D") +
  temapersonal1 + 
  theme(strip.text = element_text(face = "bold"),
        panel.spacing.x = unit(1, "cm"))

## Temas completos ----
plot1 <- ggplot(aguaxarea) +
  geom_col(aes(x=area, y=porcentaje, fill=fuentedeagua)) +
  labs(x = "Área de residencia", 
       y = "Porcentaje de viviendas", 
       fill = "Tipo de abastecimiento de agua") +
  scale_fill_manual(
    values = c("darkorange2", "darkolivegreen3", "gold", "mediumpurple", "lightseagreen"),
    labels = c("Dentro, servicio público",
               "Dentro, otra fuente",
               "Fuera, servicio público",
               "Fuera, otra fuente",
               "No entubada")) 

class(plot1)

plot1 + theme_minimal()
plot1 + theme_bw()
plot1 + theme_dark()
plot1 + theme_gray()


# En el caso que quieran probar otros temas completos, pueden usar el paquete ggthemes:
pacman::p_load(ggthemes)

plot1 + theme_excel()
plot1 + theme_few()
plot1 + theme_economist()
plot1 + theme_wsj()
plot1 + theme_fivethirtyeight() 
plot1 + theme_solarized()


## Colores -----
# Colores establecidos
colorpersonal1 <-  "hotpink3"
# HEX
colorpersonal2 <-  "#059898"
# RGB
colorpersonal3 <-  rgb(153, 0, 153, maxColorValue = 255)

fuenteaguatotal <- as.data.frame(prop.table(
  wtd.table(agua$fuenteagua, weights = agua$factor)))

names(fuenteaguatotal) <- c("fuente", "porcentaje")

ggplot(fuenteaguatotal) +
  geom_col(aes(x=reorder(fuente, porcentaje), y=porcentaje), 
           fill=colorpersonal2) +
  ylim(0,1) +
  coord_flip() +
  labs(x = "Fuente de abastecimiento de agua", 
       y = "Porcentaje de viviendas")
  
  # Aqui un conversor de colores muy util: https://r-charts.com/es/colores/
  
  ## Exportaciones de graficos ----
# Exportemos los graficos

ggsave(filename="plot1.pdf", 
       plot=plot1, units="cm", width=19, height=16, dpi=400)

ggsave(filename="plot1.jpg", 
       plot=last_plot(), units="cm", width=19, height=16, dpi=400)

