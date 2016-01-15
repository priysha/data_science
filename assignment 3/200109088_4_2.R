advertising<-read.csv("D://fods/hw3/Advertising.csv", sep=",", header=T)
head(advertising)
TV<-advertising[,2]
Radio<-advertising[,3]
Newspaper<-advertising[,4]
Sales<-advertising[,5]

multipleReg <- lm(Sales ~ TV + Radio + Newspaper) 
coefficients(multipleReg) 
multipleReg
# We get values of coefficients for all 3 x
sum(resid(multipleReg)^2)
