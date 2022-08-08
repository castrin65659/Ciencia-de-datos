## Carga de los datos

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)



datos=read_excel("Datos_Rotacion.xlsx")

#Separando unicamente variables cualitativas y la variable rotación 

# Variables cualitativas seleccionadas Satisfacción_Ambiental,cargo,horas extra

#revisión inicial del set de datos

glimpse(datos)

#Conviritiendo la variable Satisfacción_Ambiental a factor [variable categorica]

datos$Satisfacción_Ambiental<- as.factor(datos$Satisfacción_Ambiental) #Conversion a factor 

#revision del orden de los levels de la variable
levels(datos$Satisfacción_Ambiental)

#Separando el dataset con las variables escogidas usando dplyr, para mayor facilidad 
# tambien se aislan las variables cuantitativas seleccionadas

datos_grupo<- datos %>% 
    select(Rotación,Edad,Distancia_Casa,Años_Experiencia,Satisfacción_Ambiental,Cargo,Horas_Extra)

#Revision del dataset con las variables de interes
glimpse(datos_grupo)

# Revisión de distribución univariante de variables cualitativas


# ----------------Revisión de la distribución de la variable de control Rotación---------------------

# Tabla de frecuencia variable rotación
rotacion_freq<-datos_grupo %>%
    group_by(Rotación) %>%
    summarise(FrecAbs=n())
rotacion_freq<-rotacion_freq %>%
    mutate(FrecRelativa=FrecAbs/sum(FrecAbs))

# Tabla de frecuencias formateada

rotacion_freq.t<- ggtexttable(rotacion_freq,rows=NULL,
                              theme = ttheme("mOrange"))


# Gráfica de barras de la variable rotación
bart_rot<- ggplot(data=datos_grupo,aes(as.factor(datos_grupo$Rotación)))+
    geom_bar(color="black",fill="orange",alpha=0.7)+
    ggtitle("Distribución de rotación")+
    theme(
        plot.title=element_text(family =" ",face="bold",colour="black",size=13,
                                hjust=0.5,vjust=0.5)
    )+xlab("Rotación")+ylab("Conteo")

# Montaje de gráfica y tabla en un mismo espacio
ggarrange(bart_rot,rotacion_freq.t,ncol=1,nrow=2,
          heights = c(0.9,0.3))
     
    
#--------------------------Revisión de la distribución de la variable Horas_Extra----------------


# Tabla de frecuencia variable Horas_Extra
horasext_freq<-datos_grupo %>%
    group_by(Horas_Extra) %>%
    summarise(FrecAbs=n())
horasext_freq<-horasext_freq %>%
    mutate(FrecRelativa=FrecAbs/sum(FrecAbs))

# Tabla de frecuencias formateada

horasext_freq.t<- ggtexttable(horasext_freq,rows=NULL,
                              theme = ttheme("mOrange"))


# Gráfica de barras de la variable Horas_Extra
bart_horasext<- ggplot(data=datos_grupo,aes(as.factor(datos_grupo$Horas_Extra)))+
    geom_bar(color="black",fill="orange",alpha=0.7)+
    ggtitle("Distribución de funcionarios con horas extra")+
    theme(
        plot.title=element_text(family =" ",face="bold",colour="black",size=13,
                                hjust=0.5,vjust=0.5)
    )+xlab("¿Labora horas extra?")+ylab("Conteo")

# Montaje de gráfica y tabla en un mismo espacio
ggarrange(bart_horasext,horasext_freq.t,ncol=1,nrow=2,
          heights = c(0.9,0.3))


#--------------------------Revisión de la distribución de la variable Satisfacción_Ambiental----------------


# Tabla de frecuencia variable Satisfacción_Ambiental
satisfamb_freq<-datos_grupo %>%
    group_by(Satisfacción_Ambiental) %>%
    summarise(FrecAbs=n())
satisfamb_freq<-satisfamb_freq %>%
    mutate(FrecRelativa=FrecAbs/sum(FrecAbs))

# Tabla de frecuencias formateada

satisfamb_freq.t<- ggtexttable(satisfamb_freq,rows=NULL,
                              theme = ttheme("mOrange"))


# Gráfica de barras de la variable Satisfacción_Ambiental
bart_satisfamb<- ggplot(data=datos_grupo,aes(as.factor(datos_grupo$Satisfacción_Ambiental)))+
    geom_bar(color="black",fill="orange",alpha=0.7)+
    ggtitle("Distribución nivel de satisfacción ambiente laboral")+
    theme(
        plot.title=element_text(family =" ",face="bold",colour="black",size=13,
                                hjust=0.5,vjust=0.5)
    )+xlab("¿Qué tan satisfecho esta con el ambiente laboral?")+ylab("Conteo")

# Montaje de gráfica y tabla en un mismo espacio
ggarrange(bart_satisfamb,satisfamb_freq.t,ncol=1,nrow=2,
          heights = c(0.9,0.3))


#--------------------------Revisión de la distribución de la variable Cargo----------------

# Tabla de frecuencia variable Cargo
Cargo_freq<-datos_grupo %>%
    group_by(Cargo) %>%
    summarise(FrecAbs=n())
Cargo_freq<-Cargo_freq %>%
    mutate(FrecRelativa=FrecAbs/sum(FrecAbs))
Cargo_freq<-Cargo_freq %>%
    arrange(desc(FrecAbs))


# Tabla de frecuencias formateada

Cargo_freq.t<- ggtexttable(Cargo_freq,rows=NULL,
                               theme = ttheme("mOrange"))


# Gráfica de barras de la variable Satisfacción_Ambiental
bart_cargo<- ggplot(data=Cargo_freq,aes(x=Cargo,y=FrecAbs))+
    geom_bar(stat="identity",color="black",fill="orange",alpha=0.7)+
    ggtitle("Distribución de cargos")+
    theme(
        plot.title=element_text(family =" ",face="bold",colour="black",size=13,
                                hjust=0.5,vjust=0.5)
    )+xlab("Cargos")+ylab("Conteo")+coord_flip()

# Montaje de gráfica y tabla en un mismo espacio
ggarrange(bart_cargo,Cargo_freq.t,ncol=2,nrow=1,
          heights = c(0.9,0.3))


## Grid of barcharts 

ggarrange(bart_rot,bart_horasext,bart_satisfamb,bart_cargo,ncol=2,nrow=2)



# ----------------------------------Análisis bivariado---------------------------------------
#--------------------------Rotación vs Variables cualitativas--------------------------------

## Agrupaciones 


agrup1_herot<- datos_grupo %>% 
    group_by(Horas_Extra,Rotación) %>%
    summarise(FrecAbs=n()) %>% 
    mutate(FrecRelativa=FrecAbs/sum(FrecAbs)) %>%
    arrange(desc(FrecAbs))
    
    
agrup2_satisfamb<- datos_grupo %>% 
    group_by(Satisfacción_Ambiental,Rotación) %>%
    summarise(FrecAbs=n()) %>% 
    mutate(FrecRelativa=FrecAbs/sum(FrecAbs)) %>%
    arrange(desc(FrecAbs))

agrup3_cargo<- datos_grupo %>% 
    group_by(Cargo,Rotación) %>% 
    summarise(FrecAbs=n()) %>% 
    mutate(FrecRelativa=FrecAbs/sum(FrecAbs)) %>%
    arrange(desc(FrecAbs))



## Rotación vs horas extras

barplot_rothe<-ggplot(agrup1_herot,aes(fill=Rotación,y=FrecAbs,x=Horas_Extra))+
    geom_bar(position="stack",stat="identity")

barplot_rothe_rel<-ggplot(agrup1_herot,aes(fill=Rotación,y=FrecAbs,x=Horas_Extra))+
    geom_bar(position="fill",stat="identity")

## Rotación vs Satisfacción Ambiental

barplot_rotsatisf<- ggplot(agrup2_satisfamb,aes(fill=Rotación,y=FrecAbs,x=Satisfacción_Ambiental))+
    geom_bar(position="stack",stat="identity")

barplot_rotsatisf_rel<- ggplot(agrup2_satisfamb,aes(fill=Rotación,y=FrecAbs,x=Satisfacción_Ambiental))+
    geom_bar(position="fill",stat="identity")


## Rotación vs Cargo

barplot_rotcargo<- ggplot(agrup3_cargo,aes(fill=Rotación,y=FrecAbs,x=Cargo))+
    geom_bar(position="stack",stat="identity") +coord_flip()

barplot_rotcargo_rel<- ggplot(agrup3_cargo,aes(fill=Rotación,y=FrecAbs,x=Cargo))+
    geom_bar(position="fill",stat="identity") +coord_flip()

