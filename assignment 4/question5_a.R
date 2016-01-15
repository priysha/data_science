library(ISLR)
summary(Default)
attach(Default)

#part (a)
set.seed(1)
logic_reg_fit = glm(default ~ income + balance, data = Default, family = binomial)

#part (b)
Test_Error = function() {
  train_set = sample(dim(Default)[1], dim(Default)[1]/2)
  
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train_set)
  
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, Default[-train_set, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  
  return(mean(glm.pred != Default[-train_set, ]$default))
}
Test_Error()

#part (c)
print( "Three more estimates of the validation set error:" )
print( Test_Error() )
print( Test_Error() )
print( Test_Error() )



#part (d)
Test_Error = function() {
train_set = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train_set)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train_set, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train_set, ]$default)
}
print( Test_Error() )
