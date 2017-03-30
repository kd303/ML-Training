
cor(newcap$Sales,newcap$Newcap)

cor(newcap$Sales,newcap$Value)

cor(newcap$Value,newcap$Newcap)

## all three variables are highly correlated 

## although sales in market value relation is higher than the newcap and sales
## corelation cannot be ignored between new cap and val

## also the distribution of individual variable is not the same.
## so cannot make a decision.
## pearson corelation - linear data
## sparman corelation -  non linear data
## candel tao - ordinal data
## what is partial correlation***
## Problem is about partial correlation
## y = f(x2) + E , X1 = f(x2) + E - cor(e1, e2)
hist(newcap$Sales)

hist(newcap$Newcap)

hist(sqrt(newcap$Newcap))
hist(log(newcap$Newcap))
hist(newcap$Value)

hist(log(newcap$Value))

cor(newcap)

hist(newcap$Sales)

hist(log(newcap$Sales))

## linear model and VIF

newcapmodel = lm(newcap$Sales~newcap$Newcap+newcap$Value)

summary(newcapmodel)

library(car)

vif(newcapmodel)

## VIF values are < 5 so dont think there is a multicolinearity observed.

plot(predict(newcapmodel),newcapmodel$residuals)
lines(newcap$Sales,predict(newcapmodel), col="red")
plot(predict(newcapmodel), newcap$Sales)

## sale has an outlier 
plot(newcap$Sales, newcap$Value)
##
plot(newcap$Sales,newcap$Newcap)
qqnorm(newcapmodel$residuals)

outlierTest(newcapmodel)
## Bonferonni p-value for for 44,90,171 is <0.05 hence there are outliers in the dat

## do we need to transform the data?
## model with transformed data


### log transformation is effective but 0s is infity or NaN - how do we treat them??

newcaplogdata = newcap

newcaplogdata$Sales = log(newcap$Sales)

newcaplogdata$Newcap = log(newcap$Newcap)

newcaplogdata$Value = log(newcap$Value)

hist(newcaplogdata$Sales)

newcaplogmodel = lm(newcaplogdata$Sales~newcaplogdata$Newcap+newcaplogdata$Value)

