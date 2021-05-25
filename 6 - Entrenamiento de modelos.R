#6 Entrenamiento de modelos lineales
#Autor: Ing. Pepekike

#00 Carga de librerías####
library(tidyverse)        #Administración de los datos
library(caTools)          #Partición de los datos
library(GGally)           #Funciones extra para ggplot2
library(caret)            #Framework para entrenamiento ML
library(corrplot)         #Gráficas para correlaciones lineales
library(mice)             #Imputación/Comprensión de valores NA

#01 Carga de la información####
setwd("C:/Users/jose.acuna/Desktop/Semana i/CompetenciaPrecios")
dfPreciosCasas = read_csv("train.csv")

dfPreciosNumericos = dfPreciosCasas[, sapply(dfPreciosCasas, is.numeric)]
corrplot(cor(dfPreciosNumericos))

#Revisar comportamiento de valores nulos
md.pattern(dfPreciosNumericos)
sapply(dfPreciosNumericos, function(X) {sum(is.na(X))})/nrow(dfPreciosNumericos)

#Nueva base de datos numérica
dfPreciosNum_Limpio = dfPreciosNumericos %>% select(-LotFrontage)
# dfPreciosNum_Limpio = dfPreciosNumericos[,-3]

#Correlación con SalePrice
cor(dfPreciosNum_Limpio)["SalePrice",]  #Vector de rho para SalePrice
sort(abs(cor(dfPreciosNum_Limpio)["SalePrice",]))

dfPreciosNum_Limpio %>%
  select(SalePrice,OverallQual,GrLivArea) %>%
  ggpairs()

#Modelo lineal (Base R) con estas dos variables
lmodel1 = lm(SalePrice ~ OverallQual + GrLivArea, data = dfPreciosNum_Limpio)
lmodel2 = lm(log(SalePrice) ~ OverallQual + log(GrLivArea), data = dfPreciosNum_Limpio)

summary(lmodel1)
plot(lmodel1)

summary(lmodel2)
plot(lmodel2)

#Entrenamiento con caret
caretlmodel = train(SalePrice ~ .,
                    data = dfPreciosNum_Limpio %>% select(-Id),
                    preProcess = c("BoxCox","knnImpute"),
                    method = "lm",
                    na.action = na.omit)

summary(caretlmodel$finalModel)

predict(caretlmodel, newdata = dfPreciosNum_Limpio)

#Entrenamiento con RL Ridge
caretlmodel_ridge = train(SalePrice ~ .,
                          data = dfPreciosNum_Limpio %>% select(-Id),
                          preProcess = c("center","scale","BoxCox","knnImpute"),
                          method = "glmnet",
                          tuneGrid = expand.grid(alpha = seq(0,1,0.1),
                                                 lambda = 2^seq(-2,3,1)),
                          na.action = na.omit)

caretlmodel_ridge
plot(caretlmodel_ridge)

#Entrenamiento con PCR o PLS
set.seed(mean(c(17,50)))

caretlmodel_pls = train(SalePrice ~ .,
                          data = dfPreciosNum_Limpio %>% select(-Id),
                          preProcess = c("center","scale","BoxCox","knnImpute"),
                          method = "pls",
                          tuneGrid = expand.grid(ncomp = 2:20),
                          na.action = na.omit)

plot(caretlmodel_pls)

#Entrenamiento con MARS
set.seed(mean(c(17,50)))

caretlmodel_mars = train(SalePrice ~ .,
                        data = dfPreciosNum_Limpio %>% select(-Id),
                        preProcess = c("center","scale","knnImpute"),
                        method = "earth",
                        tuneGrid = expand.grid(nprune = seq(2,20,2),
                                               degree = 1:2),
                        na.action = na.omit)

plotmo(caretlmodel_mars$finalModel)

caretlmodel_mars$finalModel$coefficients

#Entrenamiento con RF
set.seed(mean(c(17,50)))

caretlmodel_rf = train(SalePrice ~ .,
                         data = dfPreciosNum_Limpio %>% select(-Id),
                         preProcess = c("knnImpute"),
                         method = "rf",
                         tuneGrid = expand.grid(mtry = 1:30),
                         na.action = na.omit)