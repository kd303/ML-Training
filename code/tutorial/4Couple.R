# corelation 0.76 wives and husbands not related with height
cor(couple[,c(2,3)])

plot(couple$Wife,couple$Husband)

## 

couplm = lm(couple$Husband~couple$Wife)

summary(couplm)

summary(couple$Wife)

summary(couple$Husband)
## distribution is normal for height of wives dadta
hist(couple$Wife)
hist(couple$Husband)

install.packages("Hmisc")
library(psych)
## range is the same.
describe(couple$Wife)
## paired t-test ?!

describe(couple$Husband)