setwd("C:/Users/David/Desktop/STATISTICS2/project 2nd term/First Deliverable")
data.A=read.csv("ShiftA.csv",header=T)
data.B=read.csv("ShiftB.csv",header=T)

attach(data.A)
names(data.A)
data.A.var=var(data.A$Footage)

attach(data.B)
names(data.B)
data.B.var=var(data.B$Footage)

qf(0.975,561,834)
qf(0.025,561,834)


F.Ratio=(data.A.var)/(data.B.var)
F.Ratio

if(F.Ratio> qf(0.975,561,834) || F.Ratio<qf(0.025,561,834)){
  print("H0 is rejected, therefore the standard deviations are different")
} else{
  print("HO is accepted, therefore the standard deviations are the same") 
}