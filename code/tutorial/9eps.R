cor(eps$Anil,eps$Sourav)

# corelation factor is 0.96 so both the methods that analyst are following
# and their outcome is highly corelated.
# hence can be concluded that the new method has high level of consistancy.

hist(eps$Anil)

hist(eps$Sourav)

t.test(eps$Anil, eps$Sourav, paired = TRUE)

#p-value 0.49

### we have to calculate the uncertainity - whether method is consistant
## or not.
## pair t-test dont do without looking at variance
## average of difference is 0 and Sd < 1 % 

summary(lm(eps$Anil~eps$Sourav))

mean(eps$Anil-eps$Sourav)

describe(eps$Anil-eps$Sourav)

## H0 - difference of mean is 0
## H1 - diference of mean not 0