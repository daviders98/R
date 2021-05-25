#5 Clusterización de tiendas
#Autor: Pepekike

# install.packages("NbClust")

#Carga de librerías
library(tidyverse)      #Administración de datos
library(scales)         #Escalas en eje x y eje y
library(NbClust)        #Clusterización automática
library(GGally)         #Funciones extra de ggplot2

#Cargar información de plantas
data("iris")
iris$Species = NULL

#Entender un poco la info que tenemos
str(iris)

ggpairs(iris, mapping = aes(col = Species))

#Escalar iris
iris_sc = scale(iris) %>% as.data.frame()

#Obtener la matriz de distancia
matriz_dist = dist(iris_sc)
matriz_matriz_dist = as.matrix(matriz_dist)

#Clusterización jerárquica
clust_jer = hclust(d = matriz_dist)
 
plot(clust_jer) #Graficar el dendograma

clusters = cutree(clust_jer, k = 3)

iris_concluster = cbind(iris, cluster = clusters)

ggpairs(iris_concluster, mapping = aes(col = as.factor(cluster)))

#Clusterización k-medias
set.seed(19)
clusterkmeans = kmeans(x = iris_sc, centers = 5)
clusterkmeans


set.seed(19)
m = 10

resultado = c()

for(i in 1:m) {
  
  cluster_cache = kmeans(x = iris_sc, centers = i)
  
  resultado = c(resultado, mean(cluster_cache$withinss))
  
}

plot(1:m, resultado, type = "b")

ggpairs(iris[,1:4])

clusterautomatico = NbClust(data = iris_sc, min.nc = 2, max.nc = 10,
                            method = "ward.D")
