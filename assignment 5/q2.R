#part_(4)

mt1<-c(74,70,66,55,52,47,33)
mt2<-c(66,58,74,47,61,38,41)

Reg_Imputation <- lm(mt2~mt1)

summary(Reg_Imputation)  #14.452 + 0.715*mt1

#part_(5) after regression values are imputed

mt1_reg<-c(74,70,66,55,52,47,45,38,33,28)
mt2_reg<-c(66,58,74,47,61,38,46.63,41.6,41,34.47)

#sd(mt1_reg)= 15.65461  se(mt1)=4.950422
#sd(mt1_reg)= 13.2   se(mt2)=4.17

Regression <- lm(mt2_reg~mt1_reg)

mt2_new_pred <-14.4446 + mt1_reg*0.7151

mean(mt1_reg) #50.8
mean(mt2_reg) #50.77


obs_pred_reg <- mt2_reg - mt2_new_pred

sse_reg <- sum(obs_pred_reg*obs_pred_reg)

std_err_reg <- sqrt(sse_reg/8)

#part (2)_a

mt1<-c(74,70,66,55,52,47,45,38,33,28)
mt2<-c(66,58,74,47,61,38,32,46,41,44)

#sd(mt1)= 15.65461  se(mt1)=4.950422
#sd(mt2)= 13.4251   se(mt2)=4.245389

normal <- lm(mt2~mt1)

mt2_pred1 <- 18.722 + mt1*0.6295

mean(mt1) #50.8
mean(mt2) #50.7

obs_pred1 <- mt2 - mt2_pred1

sse1 <- sum(obs_pred1*obs_pred1)

std_err1 <- sqrt(sse1/8)

#part (2)_b after listwise deletion

mt1_list<-c(74,70,66,55,52,47,33)
mt2_list<-c(66,58,74,47,61,38,41)


#sd(mt1_list) 14.4  se 5.44
#sd(mt2_list) 13.4  se 5.06
normal <- lm(mt2_list~mt1_list)


mt2_pred2 <- 14.452 + mt1_list*0.715

mean(mt1_list) #56.7
mean(mt2_list) #55

obs_pred2 <- mt2_list - mt2_pred2

sse2 <- sum(obs_pred2*obs_pred2)

std_err2 <- sqrt(sse2/5)