
## Anova analysis

advenq$Day <- as.factor(advenq$Day)

colnames(advenq) <- c("Day","secion","enq")

advenq$Day <- as.factor(advenq$Day)

advenq$secion <- as.factor(advenq$secion)

advfit = aov(advenq$enq~advenq$Day + advenq$secion)

advfit1 = aov(advenq$enq ~ advenq$Day * advenq$secion)

summary(advfit)

summary(advfit1)
## hisogram does not look normal esp +2 side
hist(advfit$residuals)
# p-value > 0.3605 > 0.05 normal
shapiro.test(advfit$residuals)

shapiro.test(advfit1$residuals)

plot(advfit$residuals,advfit$fitted.values)

plot(advfit1$residuals,advfit1$fitted.values)

interaction.plot(advenq$Day,advenq$secion, advenq$enq, fun = mean , main="average", col = 1:nlevels(advenq$secion), pch = 19)

## business section the mean enq remains high throughout the week
## monday advertise in business, and on Fridays News, possibily leave thruseday out.
## overall repsonse on Friday remains high - 8 and above so section should not matter ??
## Data for weekend is not available - so is this only for work days?
## R square calculation

# summary(advfit1)

# sum of squares = all variable, + residuals
## rsquare = 1 = residual sq / total - R - 0.88 
hist(advenq$enq)

hist(as.numeric(advenq$Day))

interaction.plot(advenq$Day,advenq$secion, advenq$enq, fun = var, main="Variance")

library(gplots)

plotmeans(advenq$enq~advenq$Day)

plotmeans(advenq$enq~advenq$secion)