
x<- matrix(c(106,67,37,124,85,72,15,10,12), nrow = 3)

dimnames(x)=list(c("A","B","C"), c("Perfect", "Satisfactory", "Defective"))

chisq.test(x)

# p-value is > 0.05 hence null hypothesis cannot be rejected. Null hypothis is 
## variables are independent
## DF = r-1 * c - 1 = 3-1 * 3-1

library(car)

## 2 TYPE OF ANOVA
## TYPE 1 - 
## tYPE 2