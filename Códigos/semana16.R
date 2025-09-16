
# Se omiten las tildes intencionalmente.

# Semana 16 ----

rm(list=ls())
gc()

setwd("C:/curso_estadistica_basica/taller/")
getwd()

pacman::p_load(foreign, dplyr, circlize, data.table)
options(scipen=999)


# Datos -----
viajes <- read.dbf("bd_eod_2017_dbf/TVIAJE.DBF")


## Origen ----
prop.table(table(viajes$P5_6, useNA = "ifany"))

class(viajes$P5_6)

# 0. Hogar P5_6==1
# 1. Escuela P5_6==2
# 2. Trabajo P5_6==3 | P5_6==4 | P5_6==12
# 3. Comercio P5_6==5
# 4. Recreacion P5_6==6 | P5_6==10 | P5_6==9
# 5. Otra vivienda P5_6==7
# 6. Otro P5_6==8 | P5_6==11 | P5_6==13 | P5_6==14 | P5_6==15 | P5_6==16

viajes <- viajes %>%
  mutate(origen = case_when(
    P5_6=="01" ~ 0,
    P5_6=="02" ~ 1,
    (P5_6=="03" | P5_6=="04" | P5_6=="12") ~ 2,
    P5_6=="05" ~ 3,
    (P5_6=="09" | P5_6=="06" | P5_6=="10") ~ 4,
    P5_6=="07" ~ 5,
    (P5_6=="08" | P5_6=="11" | P5_6=="13" | 
       P5_6=="14" | P5_6=="15" | P5_6=="16") ~ 6,
    P5_6==99 ~ NA))

prop.table(table(viajes$origen, useNA = "ifany"))

viajes$origen <- factor(viajes$origen, 
                        levels = c(0:6), 
                        labels = c("Hog", "Esc",
                                   "Tra", "Com",
                                   "Rec", "O_vi",
                                   "Otr"))


## Destino ----
prop.table(table(viajes$P5_13, viajes$SEXO, useNA = "ifany"), margin=2)

prop.table(table(viajes$P5_11A, useNA = "ifany"))
prop.table(table(viajes$P5_13, useNA = "ifany"))

# 0. Hogar
# 1. Escuela a estudiar
# 2. Trabajo a trabajar
# 3. Comercio a compras 
# 4. Recreacion propia
# 5. Otra vivienda 
# 6. Otro

viajes <- viajes %>%
  mutate(destino = case_when(
    P5_11A=="01" ~ 0,
    P5_11A=="02" | P5_13=="03" ~ 1,
    (P5_11A=="03" | P5_11A=="04" | P5_11A=="12") | (P5_13=="02") ~ 2,
    P5_11A=="05" | P5_13=="04" ~ 3,
    (P5_11A=="09" | P5_11A=="06" | P5_11A=="10") | P5_13=="05" ~ 4,
    P5_11A=="07" ~ 5,
    (P5_11A=="08" | P5_11A=="11" | P5_11A=="13" | 
     P5_11A=="14" | P5_11A=="15" | P5_11A=="16") ~ 6,
    P5_11A==99 ~ NA))

table(viajes$destino, useNA = "ifany")
table(viajes$destino, viajes$P5_13, useNA = "ifany")

viajes$destino[viajes$P5_13=="06"] <- NA

viajes$destino <- factor(viajes$destino, 
                        levels = c(0:6), 
                        labels = c("Hog", "Esc",
                                   "Tra", "Com",
                                   "Rec", "O_vi",
                                   "Otr"))


# Datos por sexo -----
# Viajes entre semana: P5_3==1
table(viajes$P5_3, useNA = "ifany")

hombres <- viajes %>% 
  filter(SEXO=="1" & P5_3=="1") %>% 
  group_by(origen, destino) %>% 
  summarise(total = sum(FACTOR, na.rm = T))

hombres <- hombres %>% 
  filter(is.na(origen)==F) %>% 
  filter(is.na(destino)==F)

mujeres <- viajes %>% 
  filter(SEXO=="2" & P5_3=="1") %>% 
  group_by(origen, destino) %>% 
  summarise(total = sum(FACTOR, na.rm = T))

mujeres <- mujeres %>% 
  filter(is.na(origen)==F) %>% 
  filter(is.na(destino)==F)

# Preparar datos -----
datos <- mujeres

datos$origen <- as.character(datos$origen)
datos$destino <- as.character(datos$destino)

df0 <- as.data.table(datos)
df0 <- df0[df0$origen == df0$destino, total:=0]
df0 <- subset(df0,total>0)

# df0$lntotal <- log(df0$total)
# df0$lntotal  <- round(df0$lntotal, 1) 
data <- df0 %>% select(1,2,3)

# Archivos auxiliares ----
orden <- c("Hog", "Esc", "Tra", "Com", "Rec", "O_vi", "Otr")
df1 <- data.frame(lugar=orden, orden1=c(1:7))
paleta <- viridis::plasma(7)

# Plot ----

png("mujeres.png", width = 600, height = 600)

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, 
             points.overflow.warning = FALSE)
chordDiagram(x = data, transparency = 0.4,
               grid.col = paleta,
               order = orden,
               annotationTrack = "grid")
circos.trackPlotRegion(
    track.index = 1, 
    bg.border = NA, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      sector.index = get.cell.meta.data("sector.index")
      etiqueta = df1$lugar[df1$lugar == sector.index]
      circos.text(x = mean(xlim), y = 2.5, 
                  labels = etiqueta, cex = 1.2, 
                  facing = "reverse.clockwise", 
                  adj=c(0.7, 0))
    })

dev.off()
