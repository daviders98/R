setwd("C:/Users/André González/Desktop/Stats 2")

data=read.csv("means p2.csv")

names(data)
attach(data)
library(fbasics)

#QUESTION 1
t.test(Men, Women, alternative=c("two.sided"), mu=0, paired=FALSE, conf.level=0.95)
Dif=Men-Women
boxplot(Dif)

#QUESTION 2
t.test(A, B, alternative=c("two.sided"), mu=0, paired=TRUE, conf.level=0.99)


#QUESTION 3
DryMouth=matrix(c(50, 31, 1655-50, 1652-31),
                        nrow = 2,
                        dimnames = list("Clarinex" = c("Clarinex", "Placebo"),
                                        "Placebo" = c("Dry mouth", "No dry mouth")))
prop.test(DryMouth, alternative = c("greater"), conf.level=0.995)

#QUESTION 4
nF=length(Female)
df.F=nF-1
nM=length(Male)
df.M=nM-1
varF=var(Female)
varM=var(Male)
F=varM/varF
qf(0.025, df.M, df.F)
qf(0.975, df.M, df.F)

