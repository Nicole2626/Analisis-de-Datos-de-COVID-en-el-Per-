 install.packages("rgdal")
 install.packages("rgeos")
 install.packages("dplyr")
 install.packages("tmap")
 install.packages("leaflet")
 install.packages("sp")
 install.packages("readOGR")
 
 
#CARGAMOS LAS SIGUIENTES LIBRERIAS
library(rgdal)
library(rgeos)
library(dplyr)
library(tmap)
library(leaflet)
library(sp)


#CARGAMOS LA LIBRERIA DONDE TENEMOS GUARDADO NUESTRO SHP Y CSV PARA ESTE CASO DE PROVINCIAS
setwd("C:/Users/NICOLE/Desktop/PROGRA")
mapa <- readOGR("C:/Users/NICOLE/Desktop/PROGRA/PROVINCIAS.shp")
datacovid <- read.csv2("C:/Users/NICOLE/Desktop/PROGRA/positivos_covid.csv")
#CAMBIAMOS EL NOMBRE DE LA COLUMNA
  head(mapa)
names(mapa)[2]="DEPARTAMENTO"

#SUMAMOS EL NUMERO DE CASOS POSITIVOS CON RESPECTO A LAS PROVINCIAS 

covid <- datacovid %>% group_by(PROVINCIA) %>% summarise(Frequency=n())
head(mapa)
head(datacovid)
covid_mapa1 <- merge(mapa,covid,by="PROVINCIA")

#PLOTEAMOS EL MAPA CON RESPECTO A LAS PROVINCIAS
plot(covid_mapa1)
qtm(covid_mapa1,fill = "Frequency",col="col_blind")
#LO SEPARAMOS CON RESPECTO
qtm(covid_mapa1,fill=c("Frequency"),
    col = "Median_income",
    title="Casos covid por provincia",
    palette = " BuGn" ,
    scale = 0.7 , 
    fill.title="Casos positivos",
    title.font=1,
    sill.style ="fixed",
    title.fontface=3,
    fill.breaks=round(c(seq(0,30000,length.out = 7),Inf),),0)

#CARGAMOS LA LIBRERIA DONDE TENEMOS GUARDADO NUESTRO SHP Y CSV PARA ESTE CASO DE DEPARTAMENTOS

mapa <- st_read("C:/Users/NICOLE/Desktop/PROGRA/DEPARTAMENTOS.shp") ########
head(mapa)
names(mapa)[2]="DEPARTAMENTO"

covid2 <- datacovid %>% group_by(DEPARTAMENTO) %>% summarise(Frequency=n())  ########

covid_mapa2 <- merge(mapa,covid2,by="DEPARTAMENTO")

#PLOTEAMOS EL MAPA CON RESPECTOA LOS DEPARTAMENTOS

qtm(covid_mapa2,fill = "Frequency",col="col_blind")
qtm(covid_mapa2,fill=c("Frequency"),
    col = "Median_income",
    title="Casos covid por Departamento",
    palette = " BuGn" , 
    scale = 0.7 ,
    fill.title="Casos positvos",
    title.font=1,
    sill.style ="fixed",
    title.fontface=3,
    fill.breaks=round(c(seq(0,100000,length.out = 6),Inf)),0)+ 
  tm_text("DEPARTAMENTO", size = 0.7)+
  tm_layout(legend.format = list(text.separator = "-"),frame = F, asp=NA)+
  tm_legend(legend.position = c("left", "bottom"))+
  tm_scale_bar(position = c("center","bottom"))+
  tm_graticules()+
  tm_compass(position = c("left","top"))

#CON RESPECTO A LOS DEPARTAMENTOS SIN CONTAR LIMA   
View(covid2)
covid3 <- covid2[c(-15,-16),]
View(covid3)

mapa2 <- mapa[-15,]

covid_mapa3 <- merge(mapa2,covid3,by="DEPARTAMENTO")

qtm(covid_mapa3,fill = "Frequency",col="col_blind")

qtm(covid_mapa3,
    fill=c("Frequency"),
    col = "Median_income",
    title="Casos covid por Departamento sin Lima",
    palette = " BuGn" ,
    scale = 0.7 ,
    fill.title="Casos positvos",
    title.font=1,
    sill.style ="fixed",
    title.fontface=2,
    fill.breaks=round(c(seq(0,50000,length.out = 7),Inf)),0)+
tm_text("DEPARTAMENTO", size = 0.7)+
  tm_layout(legend.format = list(text.separator = "-"),frame = F, asp=NA)+
  tm_legend(legend.position = c("left", "bottom"))+
  tm_scale_bar(position = c("center","bottom"))+
  tm_graticules()+
  tm_compass(position = c("left","top"))


#MAPAS 3D
library(viridisLite)
library(viridis)
library(tidyverse)
library(sf)
install.packages("rayshader")
library(rayshader)
install.packages("magick")
library(magick)
install.packages("av")
library(av)
library(dplyr)
install.packages("rayrender")
library(rayrender)
covid_mapa2 <- mutate(covid_mapa2,Datos =Frequency)

#PLOT CON LIMA

ggcovid <- ggplot(data = covid_mapa2) +
  geom_sf(aes(fill = Datos)) +
  scale_fill_viridis_c(option = "A")+
  ggtitle("Casos Covid") +
  theme_bw()

plot_gg(ggcovid,
        multicore = T,
        width = 5 , 
        height = 5,
        scale = 200, 
        windowsize = c(1280,720),
        zoom = 0.60 ,
        phi = 50 ,
        sunangle = 120,
        theta=45) 

render_camera(fov = 0, theta = 60, zoom = 0.75, phi = 45)

render_scalebar(limits=c(0, 1000, 2000),
                label_unit = "km",
                position = "W",
                y=50,
                scale_length = c(0.33,1))

render_compass(position = "E")

render_snapshot(clear=TRUE)

#PLOT SIN LIMA

covid3 <- covid2[c(-15,-16),]

mapa2 <- mapa[-15,]

covid_mapa3 <- merge(mapa2,covid3,by="DEPARTAMENTO")

covid_mapa3 <- mutate(covid_mapa3,Datos =Frequency)

ggcovid <- ggplot(data = covid_mapa3) +
  geom_sf(aes(fill = Datos)) + 
  scale_fill_viridis_c(option = "A")+ 
  ggtitle("Casos Covid sin Lima") +
  theme_bw()


plot_gg(ggcovid,
        multicore = T,
        width = 5 , 
        height = 5, 
        scale = 200, 
        windowsize = c(1280,720)
        ,zoom = 0.60 ,
        phi = 50 ,
        sunangle = 120,
        theta=45) 

render_camera(fov = 0, theta = 60, zoom = 0.75, phi = 45)  ###cambia la posicion de la camara

render_scalebar(limits=c(0, 1000, 2000),label_unit = "km",position = "W", y=50,
                scale_length = c(0.33,1))

render_compass(position = "E") ####colocar brujula

render_snapshot(clear=TRUE)  #### guarda la vista actual

#####link: https://www.tylermw.com/3d-ggplots-with-rayshader/ #####



