#Script 2: Manejando dataframes
#Autor: Pepekike

rm(list = ls()) #Borrar las variables del ambiente

#Carga de datos
data("USArrests")
str(USArrests)

USArrests
USArrests$Murder
USArrests[1:9,]
          #Ren,Col
USArrests[c(1,6,9),c("Assault","UrbanPop")]

dfMuchoMurder = USArrests[USArrests$Murder > 10, ] #Filtrado de información
dfMuchoMuchoMurder = dfMuchoMurder[dfMuchoMurder$Assault > 20, ]
dfMuchoMuchoMurder = USArrests[USArrests$Murder > 10 & USArrests$Assault > 20,]

USArrests$MurderperPop = USArrests$Murder/USArrests$UrbanPop

summary(USArrests)

cor(USArrests)
pairs(USArrests)

#Gráfica de correlacipon
library(corrplot)
corrplot(cor(USArrests))

#Ejemplo de creación de data.frames
Anio = c(2016,2017,2018,2019)
Poblacion = c(10,11,12,13)

dfCrecimiento = data.frame(Anio, Poblacion)
