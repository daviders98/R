#7 División en conjunto de entrenamiento, conjunto de prueba, y CV
#Autor: Ing. Pepekike

#00 Carga de librerías####
library(tidyverse)        #Administración de los datos
library(caTools)          #Partición de los datos
library(caret)            #Framework para entrenamiento ML

#01 Carga de información####
setwd("C:/Users/jose.acuna/Desktop/Semana i/PobrezaCostaRica(1)/PobrezaCostaRica")
dfDatos = read_csv("train.csv")

#02 División en conjunto de entrenamiento y conjunto de prueba
set.seed(999)
logicTrain = sample.split(dfDatos$Target, SplitRatio = 0.7)

dfEntrenar = dfDatos[logicTrain,]
dfPrueba = dfDatos[!logicTrain,]

logicValidacion = sample.split(dfPrueba$Target, SplitRatio = 0.5)

dfPruebaPrueba = dfPrueba[!logicValidacion,]
dfValidacion = dfPrueba[logicValidacion,]

#03 Entrenar dos modelos sencillos
# caretRF = train(Target ~ .,
#                 data = dfEntrenar,
#                 preProc = c("knnImpute","nzv"),
#                 method = "rf",
#                 tuneGrid = expand.grid(mtry = 1:10),
#                 trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE,
#                                          allowParallel = TRUE),
#                 na.action = na.pass)

caretRF = train(Target ~ .,
                data = dfEntrenar %>% select(-Id, -idhogar),
                preProc = c("knnImpute","nzv"),
                method = "rf",
                tuneGrid = expand.grid(mtry = 5),
                trControl = trainControl(method = "none", verboseIter = TRUE),
                na.action = na.pass)

# caretSVM = train(Target ~ .,
#                  data = dfEntrenar %>% select(-Id),
#                  preProc = c("center","scale","knnImpute","nzv"),
#                  method = "svmRadial",
#                  tuneLength = 5,
#                  trControl = trainControl(method = "cv", number = 2, verboseIter = TRUE),
#                  na.action = na.pass)

caretSVM = train(Target ~ .,
                 data = dfEntrenar %>% select(-Id, -idhogar),
                 preProc = c("center","scale","knnImpute","nzv"),
                 method = "svmRadial",
                 tuneGrid = expand.grid(sigma = 0.00545, C = 4),
                 trControl = trainControl(method = "none", verboseIter = TRUE),
                 na.action = na.pass)

caretRF
caretSVM

y_prueba = na.omit(dfPruebaPrueba)$Target
pred_prueba_rf = predict(caretRF, newdata = dfPruebaPrueba)
pred_prueba_svm = predict(caretSVM, newdata = dfPruebaPrueba)

dfComparacionPrueba = data.frame(y = y_prueba,
                                 pred_rf = pred_prueba_rf,
                                 pred_svm = pred_prueba_svm)

dfComparacionPrueba %>%
  mutate(error_rf = y - pred_rf,
         error_svm = y - pred_svm) %>%
  summarise(rmse_rf = sqrt(mean(error_rf^2)),
            mae_rf = mean(abs(error_rf)),
            rmse_svm = sqrt(mean(error_svm^2)),
            mae_svm = mean(abs(error_svm)))

y_valid = na.omit(dfValidacion)$Target
pred_valid_svm = predict(caretSVM, newdata = dfValidacion)

dfComparacionValidacion = data.frame(y = y_valid,
                                 pred = pred_valid_svm)

dfComparacionValidacion %>%
  mutate(error = y - pred) %>%
  summarise(rmse = sqrt(mean(error^2)),
            mae = mean(abs(error)))