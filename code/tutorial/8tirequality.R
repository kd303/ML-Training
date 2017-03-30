
colnames(tires) <- c("shift","quality","numbers")

tires$shift <- as.factor(tires$shift)

tires$quality <- as.factor(tires$quality)

tiresmode = aov(tires$numbers~tires$shift+tires$quality)

summary(tiresmode)

interaction.plot(tires$shift, tires$quality, tires$numbers , col=1:nlevels(tires$quality))


library(gplots)

plotmeans(tires$numbers~tires$quality)

plotmeans(tires$numbers~tires$shift)

mytable=table(tires$shift,tires$quality)

chisq.test(mytable)

# have to do it excel - cause observation are not individual and hence cant create a table in R
chidist(6.55,5)
# DF - 6 (3 shifts and 3 levels of quality) - 1
 ## p-value - 0.25, since p > 0.05 H0 is accepted
## hence both the variables are independent and cannot conclude that there are differences in
## quality.