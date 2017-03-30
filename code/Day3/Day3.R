# install.packages("devtools")

# install_github("Infosys/IIPR")
# 
# Correlation 
## no xs should be related - multico linearity
multRegYield$`%Yield`

plot(multRegYield$Time, multRegYield$`%Yield`)

plot(multRegYield$`%Yield`, multRegYield$Temperature)

cor(multRegYield)

ymodel = lm(multRegYield$`%Yield` ~ multRegYield$Time+multRegYield$Temperature)

summary(ymodel)
## model significance - looking at p value p-value: 2.319e-05
## model accuracy - Rsquare, Adjusted (adjusted R) > 0.6 and above its alright
## model adequecy -Residual Analysis residual plot (pattern - no relation ship) - 
##                shapiro test and normalality test
##                residual plots - residuals vs X -
## for many Xs - predicted -> residuals plot.
## model generalised  - Cross Validation (60% - model , 20% test, 10% ) / Cross validated MSE
## 

ymodel1 = lm(multRegYield$`%Yield` ~ multRegYield$Time)

cbind(predict(ymodel1),multRegYield)

summary(ymodel1)
## test the pvalue of residuals
shapiro.test(ymodel1$residuals)
qqnorm(ymodel1$residuals)
qqline(ymodel1$residuals)
plot(ymodel1$residuals)

## p-value is greater than 0.05 then H0, else H1 // All H0 should have equal
## Normality test 
## H0 Deviation form bell shape = 0
## H1 deviation from bell shape != 0

plot(multRegYield$Time, multRegYield$`%Yield`)
## model adequecy - predicted vs residuals should show random pattern.
plot(predict(ymodel1), ymodel1$residuals)
## outliers will check where model will not work in particular scenario or region
lines(multRegYield$Time, predict(ymodel1), col="green")

## outlier test
install.packages("minqa")
install.packages("nloptr")
install.packages("SparseM")
install.packages("MatrixModels")
install.packages("psych")
library(car)

## if p value < 0.05 are potentiall error
outlierTest(ymodel1)

mse=mean(ymodel1$residuals^2)
rmse=sqrt(mse)

## predicted valu + or - 1.96*RMSE

## Higer value of residual SSE is increase errotr

res_ss = sum(ymodel1$residuals^2)

## variance of y = total sum of square / (n-1)

## total sum of square - variance of y devided by (n-1)
total_ss  =var(multRegYield$`%Yield`)* (16-1)

## residual SS can vary from 0 (no error) to total sum of square
## maxium of 
## how not good this model is
=res_ss / total_ss
## how good it is - rsquare
rsquare = 1-(res_ss/total_ss)
## r-square - variance of Y
## adj. rsaure <= r-square - if difference is significant unneccesary variable is there.
### adjusted R-square - is always increasing function
## correction given to rsquare 
## adjusted Rsquare will be highest for optimal number of X (input variable)
## adjusted rsqaure will increase more accurate model (more useful X) 
## and descrese if x is not useful. 
## adj. rsqure will give info wwhether added X is useful or not.
## adj rquare = 1- ( (( 1-R)*(n-1) )/ (n-p-1))

## regresionn SS = Total SS - residual SS


## MS of regression = Regrssion SS / (number of independent - 1)
## MS of Residual = Residual SS / (number of observation - number of input in model - 1)
# MS of total = Total SS / (n -1)

## F-STAT = MS Regression / MS Residuals = 1

## larger the F, smaller the p, hence model is significant.

library(DAAG)
install.packages("DAAG")
install.packages("VIF")
install.packages("lattice")

testYield = 
  
  attach(multRegYield)

cv.lm(form.lm = formula(`%Yield`~Time), data = multRegYield, m = 4)

CVlm(ymodel1, data = multRegYield, m = 1)

cv.lm(data=multRegYield, ymodel1, m=4)

## predictedValue +or - (RMSE* 1.96) - wider the interval useless is the model
## Higher Cross validated MSE the range will be wider - importance is low
## Cross Validation MSE > model MSE (generally the scenarip)
## if above difference is significant - then its overfitting
## R-square - predicted 
## Rsq = 1- MSE(cv)/SST - R-squire predicted (how good the power of prediction)
###################  VIF - variance influence factor ####################################

## minimum - no multi-co linearity - 1
## maximum - maximum co-linearity - infinity
## VIF > 5 there is multi-colinearity
## remove any one of the X1 or X5 - decision depends on business importance.
## X1 / X5 are corelated and they may have higher corelation with Y

## 1. develop overall model
## 2. check overall Rsquare
## 3. check multi-colinearity
## 4. selecting the right model
## 
#forward selection - AIC should be minmimum
# 1. select highest contributor - corelation coefficient
# 2. Rsquare adj is increasing - addition worth, if decreasing its not worth.
# 3. once its start decreasing dont add.
# 4. Once added - cant be remove 
#backward elimination
# 1. remove one by one
# 2. if removed - Radj increasing
# 3. once removed cant be added
#
#stepwise
# 
#
#
library(MASS)
multRegModel = lm(multRegConv$`% Conversion` ~ multRegConv$Time+multRegConv$Temperature+multRegConv$`Kappa number`)

summary(multRegModel)

step = stepAIC(multRegModel, direction="both")

## stepwise selection
vif(multRegModel)

## value of Time / temprature VIF is 12.0+ hence they are highly correlated

install.packages("nloptr")


