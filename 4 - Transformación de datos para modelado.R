#4 Transformación de datos para modelado
#Autor: Pepekike

#Carga de librerías
library(tidyverse)   #Administración de datos
library(mice)        #Imputación de datos
library(GGally)      #Funciones extra para ggplot2

#Escalado y análisis de componentes principales
#Carga de información de arrestos
data("USArrests")

var(USArrests)

#Manera larga
murder_esc = (USArrests$Murder - mean(USArrests$Murder))/sd(USArrests$Murder)
assault_esc = (USArrests$Assault - mean(USArrests$Assault))/sd(USArrests$Assault)
up_esc = (USArrests$UrbanPop - mean(USArrests$UrbanPop))/sd(USArrests$UrbanPop)
rape_esc = (USArrests$Rape - mean(USArrests$Rape))/sd(USArrests$Rape)

USArrests_esc = data.frame(murder_esc,
                           assault_esc,
                           up_esc,
                           rape_esc)
#Manera corta
USArrests_esc = as.data.frame(scale(USArrests))
var(USArrests_esc) #

#Comparación de gráficas
USArrests_esc %>%
  # filter(Assault > 0.5) %>%
  # mutate(UrbanPopperMurder = Murder/UrbanPop) %>%
  ggplot(aes(x = Assault,
             y = Rape)) +
  geom_point()

#Análisis de componentes principales
PCUSArrests = prcomp(USArrests)
PCUSArrests_esc = prcomp(USArrests_esc)

summary(PCUSArrests_esc)
biplot(PCUSArrests)

ggpairs(USArrests) #Gráfica de matriz

#Imputacion de datos
rm(list = ls())

data("airquality") #Carga de datos de calidad del aire
ggpairs(airquality) #Gráfica de matriz

summary(airquality)
md.pattern(airquality)

airquality %>%
  filter(!(is.na(Ozone) & is.na(Solar.R))) %>%
  nrow()

airquality %>%
  filter(is.na(Solar.R)) %>%
  nrow()

tempData = mice(airquality, m = 5, method = "pmm", seed = 70)

airqualitysinNA = complete(tempData, 3)

#Explicación de una lista
vector1 = runif(100)
matrix1 = matrix(rnorm(25), nrow = 5, ncol = 5)

lista_objeto = list(airquality,matrix1,vector1)
