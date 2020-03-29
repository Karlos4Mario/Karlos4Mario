
##########################################################################
#nombre del proyecto:Actividad de visitantes florales en P.Voluvilis
#autores:Duvan Felipe Riaño-Karlos Mario Peñaloza 
#estadistica A grupo-G


#           carga de los paquetes de datos usados 

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(car)){install.packages("car")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(sp)){install.packages("sp")}
if(!require(ape)){install.packages("ape")}
if(!require(ggridges)){install.packages("ggridges")}
if(!require(plyr)){install.packages("plyr")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(biotools)){install.packages("biotools")}
if(!require(moments)){install.packages("moments")}
if(!require(nortest)){install.packages("nortest")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(car)){install.packages("car")}
if(!require(HardyWeinberg)){install.packages("HardyWeinberg")}
if(!require(stats)){install.packages("stats")}
if(!require(plotly)){install.packages("plotly")}

library(plotly)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(multcomp)
library(sp)
library(stats)
library(ggplot2)
library(ggridges)
library(car)

# Carga de la tabla con los datos registrados 

getwd()
setwd("c:/Users/DuvanF/Downloads/poster estadistica")
NO_visitantes<-read.csv("Datos_visitantes_P.volubilis 1.csv", sep = ";",header = T)
View(NO_visitantes)

#Analisis de datos visitantes florales de P.volubilis
#exploracion de datos
##### comportamiento del numero de visitantes
####### exploracion visual se usan graficos para mirar a grandes rasgos el comportamiento de los datos

##### puntos
summary(NO_visitantes)
ggplot(NO_visitantes)+
geom_point(aes(x=Intervalo, y=visitas,color=Intervalo,shape=Intervalo))
#### cajas ## OK
ggplot(NO_visitantes)+
        geom_boxplot(aes(x=Intervalo, y=visitas, fill=Intervalo),outlier.colour = "red", outlier.shape = 1)
##### violin ## 
## VIOLINPLOT Y BOXPLOT MUESTRAN PRACTICAMENTE LO MISMO!
ggplot(NO_visitantes) +
        geom_violin(aes(x=Intervalo, y=visitas, fill=Intervalo, color=Intervalo),alpha=0.4,width=1.4) +
        geom_boxplot(aes(x=Intervalo, y=visitas),outlier.colour = "red", outlier.shape = 16,width=0.1)+
        geom_jitter(aes(x=Intervalo, y=visitas),alpha=0.4)
##### barras ### 
ggplot(NO_visitantes)+
        geom_bar(aes(x=visitas, fill=Intervalo))
ggplot(NO_visitantes)+
        geom_bar(aes(x=visitas, fill=Intervalo))+
        facet_wrap(~Intervalo)

##### densidad ###### 
ggplot(NO_visitantes, aes(x = visitas, y = Intervalo, fill = 0.5 - abs(0.5 - stat(ecdf))))+
 stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
        scale_fill_viridis_c(name = "Tail probability", direction = -1)

######  variables dependientes o independientes

#visitas=variable dependiente 
#Intervalo=variable independiente 

#########        pruebas de los supuestos
####               NORMALIDAD

####grafico q-q: ###
ggplot(NO_visitantes, aes(sample = visitas, colour = Intervalo)) +
        stat_qq() +
        stat_qq_line()


######       coeficiente de asimetria y curtosis      

#Si el coeficiente de asimetría es menor que -1 o mayor que 1, 
#la distribución es extremadamente sesgada. Si el coeficiente de asimetría se encuentra 
#entre -1 y -0,5 o entre 0,5 y 1, la distribución es moderadamente sesgada. 
#Si el coeficiente de asimetría se encuentra entre -0,5 y 0,5, la distribución es aproximadamente sesgada. 
#Si el coeficiente de asimetría es 0, los datos son perfectamente simétricos
skewness(NO_visitantes$visitas,na.rm = T)

#curtosis 
#mesocúrtica = 0
# platicúrtica < 0
# leptocúrtica > 0
kurtosis(NO_visitantes$visitas)

############# TEST DE NORMALIDAD PARA LOS TRES INTERVALOS
#muestras menores a 50, se usa shapiro, tenemos 60 datos pero estos se dividen
#en 20 para mañana, 20 para tarde y 20 para mediodía

by(NO_visitantes$visitas,NO_visitantes$Intervalo,shapiro.test)

### QQPLOT + IC

qqPlot(NO_visitantes$visitas,col=NO_visitantes$Intervalo,pch=16, ylab=paste("Cantidad de visitas") )
                                                                            
                                                                           
#########     homogeneidad
#se evalua las diferencias entre los grupos de muestreo
#se hace un diagrama de cajas
attach(NO_visitantes)

ggplot(data = NO_visitantes, aes(x = Intervalo, y = visitas, colour = Intervalo)) + 
        geom_boxplot() + geom_point() + theme_bw() + theme(legend.position = "none")


##### varianza de cada intervalo 
aggregate(visitas~Intervalo, data = NO_visitantes, FUN = var)

#####   homogeneidad de todos los datos
bartlett.test(NO_visitantes$visitas~NO_visitantes$Intervalo) ### no hay diferencias significativas entre los grpos

####### prueva de ANNOVA
#permite evaluar las medias de dos o más grupos. El cálculo de esta técnica se basa en:  
#si el promedio entre los grupos es lo suficientemente más grande que el promedio de 
#la variación dentro de los grupos, entonces la media de al menos un grupo no es igual a las otras.

###      supuestos del annova

#Muestras independientes:si
#No outlier: hay outliers, pero no afectan de forma significativa 
#Variables con distribución cercana a la normal:si
#Homogeneidad de Varianzas:si

######  para tener en cuenta ######
media<-aggregate(visitas~Intervalo, data = NO_visitantes, FUN = mean)
print(media)
mediana<-aggregate(visitas~Intervalo, data = NO_visitantes, FUN = median)
print(mediana)
desviacion<-aggregate(visitas~Intervalo, data = NO_visitantes, FUN = sd)
print(desviacion)
varianza<-aggregate(visitas~Intervalo, data = NO_visitantes, FUN = var)
print(varianza)

##### hay o no diferencias significativas entre los grupos
anovva_visitantes <- aov(NO_visitantes$visitas ~ NO_visitantes$Intervalo)
summary(anovva_visitantes)

#si hay diferencias significativas

##### entre cuales de los grupos hay mayor diferencia significativa
##ASI:
TukeyHSD(anovva_visitantes)
##O LO PUEDEN GRAFICAR
plot(TukeyHSD(anovva_visitantes),las=3,cex.axis=0.6)



#######  cual es el orden que mas visita 
###### numeros totales de individuos de cada orden registrado durante el muestreo ####

Intervalos_de_visita <- c("Mañana","Mediodía","Tarde")
Lepidoptera <- c(84,130,90)
himenoptera <- c(277,431,265)
Diptera <-c(44,60,34)

ordenes_visita <- data.frame(Intervalos_de_visita,Lepidoptera,himenoptera,Diptera)
View(ordenes_visita)

plot_ly(ordenes_visita,x=Intervalos_de_visita, y=Diptera, name="Diptera",type='bar')%>%
        add_trace(y=himenoptera,name = "Himenoptera", type='bar')%>% 
        add_trace(y=Lepidoptera,name="Lepidoptera",type='bar')
summary(ordenes_visita)

#### los himenopteros son el orden que mas visita esta especie en los tres intervalos

