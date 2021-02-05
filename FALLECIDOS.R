install.packages("tmap")
library(tmap)
#PLOT FALLECIDOS DISTRITO

fallecidos <-read.csv2("C:/Users/NICOLE/Desktop/PROGRA/fallecidos_covid.csv")
fallecidos_covid <- fallecidos %>% group_by(DEPARTAMENTO) %>% summarise(Frequency=n())
fallecidos_mapa <- merge(mapa,fallecidos_covid,by="DEPARTAMENTO")
qtm(fallecidos_mapa,fill = "Frequency",col="col_blind")
qtm(fallecidos_mapa,fill=c("Frequency"),col = "Median_income",title="Fallecidos por covid por distrito",palette = " BuGn" , scale = 0.7 , fill.title="Fallecidos",
    title.font=1,sill.style ="fixed",title.fontface=2,fill.breaks=round(c(seq(0,3000,length.out = 7),Inf)),0)+tm_text("DEPARTAMENTO", size = 0.7)+
  tm_layout(legend.format = list(text.separator = "-"),frame = F, asp=NA)+
  tm_legend(legend.position = c("left", "bottom"))

#PLOT FALLECIDOS PROVINCIA
fallecidos_covid2 <- fallecidos %>% 
  group_by(PROVINCIA) %>% 
  summarise(Frequency=n())
fallecidos_mapa2 <- merge(mapa,
                          fallecidos_covid2,
                          by="PROVINCIA")
names(mapa)

qtm(fallecidos_mapa2,fill = "Frequency",col="col_blind")
qtm(fallecidos_mapa2,fill=c("Frequency"),col = "Median_income",title="Fallecidos por covid por provincia",palette = " BuGn" , scale = 0.7 , fill.title="Fallecidos",
    title.font=1,sill.style ="fixed",title.fontface=2,fill.breaks=round(c(seq(0,1500,length.out = 7),Inf)),0)

#PLOT FALLECIDOS DISTRITO SIN LIMA
fallecidos_covid3 <- fallecidos_covid[c(-15),]
fallecidos_mapa3 <- merge(mapa2,fallecidos_covid3,by="DEPARTAMENTO")
qtm(fallecidos_mapa3,fill = "Frequency",col="col_blind")
qtm(fallecidos_mapa3,fill=c("Frequency"),col = "Median_income",title="Fallecidos por covid por distrito sin Lima",palette = " BuGn" , scale = 0.7 , fill.title="Fallecidos",
    title.font=1,sill.style ="fixed",title.fontface=2,fill.breaks=round(c(seq(0,2000,length.out = 7),Inf)),0)+tm_text("DEPARTAMENTO", size = 0.7)+
  tm_layout(legend.format = list(text.separator = "-"),frame = F, asp=NA)+
  tm_legend(legend.position = c("left", "bottom"))
#EN 3D 
library(viridisLite)
library(viridis)
library(tidyverse)
library(sf)
library(rayshader)
library(magick)
library(av)
#PLOT 
fallecidos_mapa <- mutate(fallecidos_mapa,Datos =Frequency)
ggfallecidos <- ggplot(data = fallecidos_mapa) +
  geom_sf(aes(fill = Datos)) + scale_fill_viridis_c(option = "A")+ ggtitle("Fallecidos") +
  theme_bw()
plot_gg(ggcovid,multicore = T,width = 5 , height = 5, scale = 200, windowsize = c(1280,720)
        ,zoom = 0.60 ,phi = 50 ,sunangle = 120,theta=45) 
render_camera(fov = 0, theta = 60, zoom = 0.75, phi = 45)
render_scalebar(limits=c(0, 1000, 2000),label_unit = "km",position = "W", y=50,
                scale_length = c(0.33,1))
render_compass(position = "E")
render_snapshot(clear=TRUE)
#PLOT SIN LIMA
fallecidos_mapa3 <- mutate(fallecidos_mapa3,Datos =Frequency)
ggfallecidos2  <- ggplot(data = fallecidos_mapa3) +
  geom_sf(aes(fill = Datos)) + scale_fill_viridis_c(option = "A")+ ggtitle("Fallecidos sin contar Lima") +
  theme_bw()
plot_gg(ggcovid,multicore = T,width = 5 , height = 5, scale = 200, windowsize = c(1280,720)
        ,zoom = 0.60 ,phi = 50 ,sunangle = 120,theta=45) 
render_camera(fov = 0, theta = 60, zoom = 0.75, phi = 45)
render_scalebar(limits=c(0, 1000, 2000),label_unit = "km",position = "W", y=50,
                scale_length = c(0.33,1))
render_compass(position = "E")
render_snapshot(clear=TRUE)
