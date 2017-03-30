cor(bodyfat)

## tricep * thighcircumframe are corelated

colnames(bodyfat)=c("tricep","thighcir","midarmcir","fat")

bodyfatmodel=lm(bodyfat$fat~bodyfat$tricep+bodyfat$thighcir+bodyfat$midarmcir)

vif(bodyfatmodel)

stepAIC(bodyfatmodel,direction = "both")

# lm(formula = bodyfat$fat ~ bodyfat$tricep + bodyfat$midarmcir)

bodyfatmodelthigh  = lm(bodyfat$fat~bodyfat$thighcir+bodyfat$midarmcir)

summary(bodyfatmodel)

summary(bodyfatmodelthigh)

plot(predict(bodyfatmodelthigh), bodyfatmodelthigh$residuals)

plot(predict(bodyfatmodelthigh), bodyfat$fat)
# p-val 0.587 > 0.05 hence normal
shapiro.test(bodyfatmodelthigh$residuals)
# does not look like outlier present
outlierTest(bodyfatmodelthigh)

library(DAAG)

cv.lm(bodyfatmodelthigh, data= bodyfat,m=20)

crossmodel = CVlm(data=bodyfat,bodyfat$fat~bodyfat$thighcir+bodyfat$midarmcir, m=1)
## Sum of squares = 111    Mean square = 5.56    n = 20 
summary(crossmodel)
## MSE 4.92 with original all variables
mean(bodyfatmodel$residuals^2)
## RMSE 5.56 with removing tricep info
mean(bodyfatmodelthigh$residuals^2)

