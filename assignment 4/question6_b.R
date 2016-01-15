#part (a)
set.seed(1)
X = rnorm(100)
epsilon = rnorm(100)

#part (b)
beta0 = 1
beta1 = 0.5
beta2 = 3
beta3 = -2
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + epsilon

#part (c)
library(leaps)
data_set = data.frame(y = Y, x = X)
model = regsubsets(y ~ poly(x, 10, raw = T), data = data_set, nvmax = 10)
model_summary = summary(model)

which.min(model_summary$cp)

which.min(model_summary$bic)