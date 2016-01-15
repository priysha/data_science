my_data<-read.table("D://fods/hw3/hw3-q4c.txt", sep="\t")
head(my_data)

Y<- my_data[,1]
X1<- my_data[,2]
X2<- my_data[,3]
X3<- my_data[,4]

better_model <-bic.glm(Y~X1+X2+X3, glm.family=binomial(), data=my_data)
summary (better_model)
