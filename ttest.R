setwd("C:/Users/David/Desktop/STATISTICS2/project 2nd term/First Deliverable")
data.A=read.csv("ShiftA.csv",header=T)
data.B=read.csv("ShiftB.csv",header=T)
attach(data.A)
names(data.A)
data.A.var=var(data.A$Footage)
A.Footage=data.A$Footage
attach(data.B)
names(data.B)
data.B.var=var(data.B$Footage)
B.Footage=data.B$Footage


-(qt(.05, 561,834))

t.test(A.Footage,B.Footage,alternative = c("less"),mu=0,paired = FALSE,var.equal = FALSE, conf.level = 0.95)
