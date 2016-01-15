my_data<-read.table("D://fods/hw3/hw3-q4c.txt", sep="\t")
head(my_data)

Y<- my_data[,1]
X1<- my_data[,2]
X2<- my_data[,3]
X3<- my_data[,4]

Y1 <-cbind(Y)

lfit <- glm(Y1~X1+X2+X3, family=binomial, data=my_data)
summary (lfit)
