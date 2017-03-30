
hist(stay$`Length of service`)

hist(stay$`Cost per day`)

staym = lm(stay$`Cost per day`~stay$`Length of service`)

summary(staym)

## distributio looks normal??
qqnorm(staym$residuals)
qqline(staym$residuals)
plot(staym$residuals)




plot(predict(staym), staym$residuals)

plot(stay$`Length of service`,stay$`Cost per day`)

plot( stay$`Length of service`,staym$residuals)
## population is normally distributed or not
## p-value is 0.03, hence null hypothesis rejected 
## residuals are not normally distributed
shapiro.test(staym$residuals)
## p-value 0.06 > 0.05 - no outlier
## H0 - potential outliers
## H1 - no potential outliers, p > 0.05 hence H1
outlierTest(staym)
## potential outlier
boxplot(staym$residuals)
## observation 24, looks like an outlier if we remove it then?
staym$residuals

newstay = data.frame(wt = c(55,75))

predict(staym, newdata = newstay)
## prediction looks very weird - for 250 days avg cost is 1.1 and 55 days 90 USD

## Conclusions:
## all parameter of the model looks okay, but the residuals are not normally
## distributed - hence - shapiro test fails.
## does this mean for longer the healtcare duration - program is losing money??

## 116.1158 - 0.46(55)
## observation 24 looks like an outlier even after removing the
## qqplots are not looking normally distributed.
## 

newstay <- stay[-c(24),]


newstaym = lm(newstay$`Cost per day`~newstay$`Length of service`)

boxplot(newstaym$residuals)

qqnorm(newstaym$residuals)
# population is normally distributed or not
## p-value is 0.08, hence null hypothesis not rejected 
## residuals are normally distributed
## still the 
shapiro.test(newstaym$residuals)

summary(newstaym)


