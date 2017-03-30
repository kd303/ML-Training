getwd()
setwd("D:/z_kaushal/ISIDMBA/R Softwares/R Softwares/libraries")
pkgs <- list.files()
install.packages(c(print(as.character(pkgs), collapse="\",\"")), repos = NULL, type="binary")

library(c(print(as.character(pkgs), collapse="\",\"")))

setwd("D:/z_kaushal/ISIDMBA/code")

cc <- Credit_Card_Expenses

View(cc)

summary(cc$CC_Expenses)

exp <- cc$CC_Expenses

summary(exp)

range(exp)

## 0 / 25 / 50 / 100 

quantile(exp)


## 90th percentile
quantile(exp, 0.1)
quantile(exp, 0.1)
library(psych)
describe(exp)
?describe

hist(cc)
hist(Credit_Card_Expenses)
hist(cc$CC_Expenses)

?hist


boxplot(cc)


##-------------------------Excersize 2------------------------------
## ROW NUMBER RANGE, COLUM RANGE D:\z_kaushal\ISIDMBA\DataSets
cce20 = cce[1:20,1:5]

write.csv(cce20, "d:/z_kaushal/ISIDMBA/DataSets/sometemp.csv")

describe(cce$`Credit Card usage`)

attach(cce)

`Credit Card usage`

gender = factor(Sex, labels = c("Male","Female"))

gender

shopping = factor(cce$Shopping, labels = c("Yes","No"))

banking = factor(Banking,labels = c("Yes","No"))

aggregate(`Credit Card usage`,by=list(gender), FUN=mean)

boxplot(`Credit Card usage` ~ gender)  

aggregate(`Credit Card usage`,by=list(shopping), FUN=mean)

boxplot(`Credit Card usage` ~ shopping)  

histBy(`Credit Card usage`,gender)

histBy(`Credit Card usage`)

boxplot(`Credit Card usage` ~ shopping + gender)  

aggregate(`Credit Card usage`~shopping + banking + gender, FUN=mean)

aggregate(`Credit Card usage`~shopping + banking + gender, FUN=summary)

sats.survey = CSat_Freq_Table

attach(sats.survey)

#for frequency use tbale function
survey = table(q1)

# columnar format 
cbind(survey)
# proportanate table - prop.table what is it?
q1_perc = prop.table(survey)*100

pie(survey)

pie(table(q3) , radius = 10)

barplot(table(q3))

box(table(q4))

pie(q1_perc)

