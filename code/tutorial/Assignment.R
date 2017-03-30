hist(repair$Minutes)

hist(repair$Units)

View(repair)

cor(repair)

model = lm(repair$Minutes~repair$Units)

summary(model)
## residual plot looks to have some pattern  - hence we cant decide from
## this data, there must be other variable explaining the behaviour.
qqnorm(model$residuals)

qqline(model$residuals)

shapiro.test(model$residuals)

plot(model$residuals)