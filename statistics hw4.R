setwd("C:/Users/David/Desktop/STATISTICS2")

#We will create the first dataset
data=read.table("MALE.txt",header=T)

head(data)
names(data)
attach(data)

#setting for plotting
graphics.off()
par("mar")
par(mar=c(3,3,3,3))

#standard normal distribution data
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

#plot the standard normal distribution
plot(x, hx, type="l", lty=1, xlab="x value")
#plot a vertical line at both alpha/2
abline(v= qnorm(.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), col='red')
text(x=qnorm(.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), y=0.1,labels='-1.959964', col='black')
abline(v= qnorm(1-.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), col='red')
text(x=qnorm(1-.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE), y=0.1,labels='1.959964', col='black')

p1 <- Male[2]/Male[1]
p2 <- Female[2]/Female[1]

root <-(Male[2]+Female[2])/(Male[1]+Female[1])
z<-(p1-p2)/sqrt(root*(1-root)*((1/Male[1])+(1/Female[1])))
z
abline(v=z, col='black')
text(x=z, y=0.1,labels='z value', col='black')

if(z <= qnorm(.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) || z >= qnorm(1-.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) ){
  print("H0 is rejected")
} else{
  print("HO is accepted") 
}