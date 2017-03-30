## experimental data - the subject knows in advance delivery time or not.
### type 1 anova 
pizza$Crust<-as.factor(pizza$Crust)
pizza$Coke<-as.factor(pizza$Coke)

pizza$`GBread`<-as.factor(pizza$`GBread`)

pizza$Driver<-as.factor(pizza$Driver)

pizzamodel=lm(pizza$time~pizza$Crust+pizza$Coke+pizza$GBread+pizza$GBread+pizza$Driver)

summary(pizzamodel)

## only Garlic bread some difference reduces time and crust adds about same time,
## rest of the variables are not singficant. Ideally it is
## sd is around 2.02 so dont think any of these variables makes
## any difference.
mean(pizza$time)

describe(pizza$time)

plot(pizza$time)
lines(mean(pizza$time))

pzmdl = lm(pizza$time~pizza$Crust+pizza$Coke+pizza$GBread+pizza$GBread)

summary(pzmdl)

## do Anova