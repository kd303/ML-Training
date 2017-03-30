hist(repair$Minutes)

hist(repair$Units)

View(repair)

cor(repair)

model = lm(repair$Minutes~repair$Units)

summary(model)
## residual plot doesnot have some pattern  - hence we cant decide from
## this data, there must be other variable explaining the behaviour.
plot(repair$Units,repair$Minutes)
plot(repair$Minutes,model$residuals)
plot(predict(model), model$residuals)
## Normality test
qqnorm(model$residuals)

qqline(model$residuals)

## p-value is 0.84 > 0.05, distribution is normal

shapiro.test(model$residuals)

plot(repair$Minutes, model$residuals)

plot(predict(model), model$residuals)

lines(repair$Units, model$residuals, col="green")

library(car)
## hypothesis H0 - error is zero, H1 greater then or less
## p-value > 0.05 H0 accepted.
outlierTest(model)

## p-value > 0.05 means no potential error

plot(model$residuals)