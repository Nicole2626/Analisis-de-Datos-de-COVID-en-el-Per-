install.packages("ggplot2")
install.packages("sp")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")

library(ggplot2)
library(dplyr)
library(ggplot)

#TOP10 CASOS DE COVID DEPARTAMENTO

top_10 <- covid2 %>%  top_n(wt = Frequency,n=10)%>% arrange(desc(Frequency))
top_10[top_10$Frequency]
g_top10 <- ggplot(top_10, aes(x= DEPARTAMENTO,y= Frequency))+ geom_col(fill="Red",col = "Red")+
  ggtitle("TOP 10 CANTIDAD DE CONTAGIADOS POR COVID POR DEPARTAMENTO")+ 
  xlab("")+ ylab("CASOS") +theme_classic()

#TOP10 CASOS DE COVID PROVINCIA
covid3<- covid[-72,]
top_10pro<- covid3 %>%  top_n(wt = Frequency,n=10)%>% arrange(desc(Frequency))
g_top10pro <- ggplot(top_10pro, aes(x= PROVINCIA,y= Frequency))+ geom_col(fill="Red",col = "Red")+
  ggtitle("TOP 10 CANTIDAD DE CONTAGIADOS POR COVID POR PROVINCIA ")+ 
  xlab("")+ ylab("CASOS") +theme_classic()

#TOP10 FALLECIDOS POR COVID DEPARTAMENTO

top_10falle <- fallecidos_covid%>%  top_n(wt = Frequency,n=10)%>% arrange(desc(Frequency))
g_top10falle <- ggplot(top_10falle, aes(x= DEPARTAMENTO,y= Frequency))+ geom_col(fill="Black",col = "Black")+
  ggtitle("TOP 10 CANTIDAD DE FALLECIDOS POR COVID POR DEPARTAMENTO")+ 
  xlab("")+ ylab("CASOS") +theme_classic()
#TOP10 FALLECIDOS POR COVID PROVINCIA
top_10falle2 <- fallecidos_covid2%>%  top_n(wt = Frequency,n=10)%>% arrange(desc(Frequency))
g_top10falle2 <- ggplot(top_10falle2, aes(x= PROVINCIA,y= Frequency))+ geom_col(fill="Black",col = "Black")+
  ggtitle("TOP 10 CANTIDAD DE FALLECIDOS POR COVID POR PROVINCIA")+ 
  xlab("")+ ylab("CASOS") +theme_classic()
#GRAFICO POR SEXO CONTAGIADOS
covidsex <- datacovid %>% group_by(SEXO) %>% summarise(Frequency=n())
g_sex <- ggplot(covidsex,aes(x=SEXO,y=Frequency))+ geom_col(fill="ORANGE3",col = "ORANGE3")+
  ggtitle("CONTAGIADOS CON RESPECTO AL SEXO")+ 
  xlab("")+ ylab("CASOS") +theme_classic()
#GRAFICO POR SEXO FALLECIDOS
covidfallesex <- fallecidos%>% group_by(SEXO) %>% summarise(Frequency=n())
covidfallesex2<- covidfallesex[c(-1,-2),]
g_sexfalle <- ggplot(covidfallesex2,aes(x=SEXO,y=Frequency))+ geom_col(fill="black",col = "black")+
  ggtitle("FALLECIDOS POR COVID CON RESPECTO AL SEXO")+ 
  xlab("")+ ylab("CASOS") +theme_classic()
#GRAFICO POR EDAD CONTAGIADOS
covidage<- datacovid %>% group_by(EDAD) %>% summarise(Frequency=n())
g_age <- ggplot(covidage,aes(x=EDAD,y=Frequency))+ geom_col(fill="ORANGE3",col = "ORANGE3")+
  ggtitle("CONTAGIADOS CON RESPECTO A LA EDAD")+ 
  xlab("")+ ylab("CASOS") +theme_classic()
#GRAFICO POR EDAD FALLECIDOS
covidagedead<- fallecidos %>% group_by(EDAD_DECLARADA) %>% summarise(Frequency=n())
g_agedead <- ggplot(covidagedead,aes(x=EDAD_DECLARADA,y=Frequency))+ geom_col(fill="ORANGE3",col = "ORANGE3")+
  ggtitle("CONTAGIADOS CON RESPECTO A LA EDAD")+ 
  xlab("")+ ylab("CASOS") +theme_classic()
