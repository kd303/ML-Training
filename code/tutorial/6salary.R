describe(sal)
## TODO : add anova analysis - ANCOVA
colnames(sal) <- c("salary","exp", "eduction", "management")

sal$eduction <- as.factor(sal$eduction)
sal$management <- as.factor(sal$management)

sal$eduction <- factor(sal$eduction, levels = c(1,2,3), labels = c("diploma", "graduate", "advdegree"))

sal$management <- factor(sal$management, levels = c(0,1), labels = c("No", "Yes"))

View(sal)

boxplot(sal$salary~sal$exp)

plot(sal$exp, sal$salary,col=ifelse( sal$management=="Yes", "red", "blue"), pch = 19 )

plot(sal$exp, sal$salary,col=ifelse( sal$eduction=="diploma", "red", ifelse(sal$eduction=="graduate", "blue","green")), pch = 19+as.integer(sal$management) )

#ggplot(sal, aes=(x=sal$exp y=sal$salary col=sal$eduction shape=sal$management)) + geom_point(size=5)

library(ggplot2)

ggplot(sal) + geom_point(aes(x=sal$exp, y=sal$salary, col=sal$eduction, shape=sal$management),size=5)

#ggplot2::aes(x=sal$exp, y=sal$salary, col=sal$eduction, shape=sal$management) 

table(sal)
## 1. Mean salaries are much higher for the people with management experience
boxplot(sal$salary~sal$management)
## 2. avg sal of those who holds diploma is much lower then grads and adv degree 
## whist ppl with adv digree and graduate mean sals are not differnt
boxplot(sal$salary~sal$eduction)
## ppl with management experience, amongst graduate and ad digree have higher then those
## with adv, digree
## graduate with management experience have higher salaries then ppl  with advance degree and management
boxplot(sal$salary~sal$eduction+sal$management)


boxplot(sal$exp~sal$eduction+sal$management)

salmodel=lm(sal$salary~sal$exp+sal$management+sal$eduction)

summary(salmodel)
## no pattern so looks fine
plot(predict(salmodel), salmodel$residuals)
## distribution is normal
qqnorm(salmodel$residuals)
qqline(salmodel$residuals)
plot(sal$salary,predict(salmodel))
### Normality test
shapiro.test(salmodel$residuals)
## p-value is 0.27 > 0.05 hence normal distributed

## apply chi-square test - convert sal and exp in categorical and then do the test.


## ancova

## type 2 is because the combination of education and management - is not equal
## if the combination is equal then type 1 and type 2 will both remains.
## here effect of experience is known hence not taken in interaction

ancovamdl = lm(sal$salary~sal$exp+sal$eduction*sal$management)

summary(ancovamdl)
Anova(ancovamdl, type = 2)

anMdl = Anova(ancovamdl, type = 2)

summary(anMdl)

