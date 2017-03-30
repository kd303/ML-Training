cor(mpg)
## width  and length are corelated
## wheelbase and length are corelated
## u-turn and length
## u-turn and width
## wheelbase and width ar
## wheelbase and weight
## weight and length
## weight and width
## weight and uturn
## weight and fuel tank
## weight and horsepower
## weigth and wheebase, widgth,fuel tank


## outlier - residuals Xs to be handled before modelling
## residuals - outlier is validity of the model
## 
mpgmodel = lm(mpg$`MPG Highway`~ mpg$Passengers + mpg$Wheelbase + mpg$Length  + mpg$Width + mpg$`U Turn Space`+mpg$Horsepower  + mpg$Fueltank + mpg$`Rear seat` + mpg$Luggage )

summary(mpgmodel)


install.packages("nloptr")

library(nloptr)
library(car)
vif(mpgmodel)

## values of wheelbase, width, weight, fueltank are corelated
## as VIF > 5
sqrt(vif(mpgmodel))


library(MASS)

stepAIC(mpgmodel,direction = "both")

mpgfinalmodel = lm( mpg$`MPG Highway` ~ mpg$Passengers + mpg$Wheelbase + 
                      mpg$Width + mpg$Weight + mpg$Fueltank)

vif(mpgfinalmodel)
summary(mpgmodel)
summary(mpgfinalmodel)

qqnorm(mpgfinalmodel$residuals)

shapiro.test(mpgfinalmodel$residuals)

outlierTest(mpgfinalmodel)