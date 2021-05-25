#Script 1: Conociendo R
#Autor: Pepekike

numero = 5    #Asignación =
numero <- 5   #Asignacion '<-'

x = 8
y = 9
z = x + y
z = x - y
x+y
z
print(z)

z = x/y
z = x*y

print(paste("La multiplicación entre",x,"y",y,"es",z))

a = "hola"
b = "mundo"

c = paste(a,b)
c = paste0(a,b)

m1 = x == 8
m2 = y == 10

!m1 #Negativo
!m2
m1 & m2 #AND
m1 | m2 #OR
(m1 | m2)&(m1)

#Factores
x = factor(c("Masculino","Femenino","Masculino"))

vector2 = c(10,29,30,40)
vector2*10
vector2+30
vector2 >= 30

vector3 = c(1,2,3,4)
vector2 + vector3

#Matrices
set.seed(7)
rnorm(5, mean = 10, sd = 3)
set.seed(8)
valores = rnorm(20, mean = 10, sd = 3)

valores_matrix = matrix(data = valores, nrow = 5, ncol = 4, byrow = TRUE)

set.seed(10)
A = matrix(data = rnorm(25), nrow = 5, ncol = 5)
B = matrix(data = rnorm(25, mean = 5), nrow = 5, ncol = 5)

A
B

A + B
A - B
A * B
A/B
A %*% B

#Histogramas
set.seed(mean(c(9,4)))
x = rnorm(2000, mean = 15, sd = 4)

hist(x)
boxplot(x)

n = 100
m = 5000

media_muestral = c()

set.seed(mean(c(17, 19)))

for(i in 1:m) {
  
  print(i)
  
  muestra = sample(x,n)
  
  media_muestral = c(media_muestral, mean(muestra))
  
}

mean(media_muestral)
sd(media_muestral)
summary(media_muestral)

library(e1071)

skewness(media_muestral)
kurtosis(media_muestral)


####### EXPONENCIAL
set.seed(mean(c(9,4)))
x = rexp(2000, rate = 3)

hist(x, breaks = 10, xlim = c(0,3))

boxplot(x)

n = 100
m = 5000

media_muestral = c()

set.seed(mean(c(17, 19)))

for(i in 1:m) {
  
  print(i)
  
  muestra = sample(x,n)
  
  media_muestral = c(media_muestral, mean(muestra))
  
}

hist(media_muestral)
