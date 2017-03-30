## do multiple linear regression # CR plot and check the x

## if power functions are present then its a polynomiyal 

## orthononal / polynomimal to use - to remove the multi co-linearity

library(readr)
nonLinearCost <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Nonlinear_Cost.csv")
View(nonLinearCost)

nonCostMdl = lm(nonLinearCost$y~nonLinearCost$x)

summary(nonCostMdl)

plot(nonCostMdl$residuals)
qqnorm(nonCostMdl$residuals)
crPlots(nonCostMdl)

nonCstMdl1 = lm(nonLinearCost$y~poly(nonLinearCost$x, 2, raw=TRUE))

cr.plots(nonCstMdl1)

summary(nonCstMdl1)

shapiro.test(nonCstMdl1$residuals)


plot(nonLinearCost$y, nonLinearCost$x)

plot(nonCstMdl1$residuals)
### regresion splines
library(readxl)
regSplinData <- read_excel("D:/z_kaushal/ISIDMBA/DataSets/Reg_Spline_DFR.xls")
View(regSplinData)

colnames(regSplinData) = c("design", "coding")

plot(regSplinData$design ~regSplinData$coding)

splDfrMdl = lm(regSplinData$coding~regSplinData$design)

summary(splDfrMdl)

predLnrMdl = predict(splDfrMdl)

plot(regSplinData$design, regSplinData$coding)

lines(regSplinData$design, predLnrMdl, col="red")
## 0.44 is where the splin is changing - by looking at scatter plots.
## 0.44 is where the knot is 
design44 = regSplinData$design - 0.44

design44[design44 < 0] = 0

design44

## after removing all the data
## this is putting a knot - the value of the variable should be 0 till the knot
## and keep rest is the same.
## 
model44 = lm(regSplinData$coding ~ regSplinData$design + design44 )

summary(model44)

pred44 = predict(model44)
## its not a continuous function
plot(regSplinData$design, regSplinData$coding)
## The function is not a continuous function
lines(regSplinData$design, pred44, col = "red")
## for continous function first and second derivative should not be constant/zero, hence
## take square or cube

design44Cube = design44^3

model44cube = lm(regSplinData$coding ~ regSplinData$design + design44Cube + poly(regSplinData$design, 3, raw = TRUE))

summary(model44cube)

predict44cube = predict(model44cube)

plot(regSplinData$design, regSplinData$coding)

lines(regSplinData$design, predict44cube, col="red")


################### Excersize ###################################

library(readxl)
Reg_Spline_Adv <- read_excel("D:/z_kaushal/ISIDMBA/DataSets/Reg_Spline_Adv.xls", 
                             col_types = c("blank", "numeric", "numeric"))
View(Reg_Spline_Adv)


plot(Reg_Spline_Adv$Sales~Reg_Spline_Adv$Adv_Cost)

advrtMdl1 = lm(Reg_Spline_Adv$Sales~Reg_Spline_Adv$Adv_Cost)

summary(advrtMdl1)

predSalAdvMdl1 = predict(advrtMdl1)

plot( Reg_Spline_Adv$Adv_Cost, Reg_Spline_Adv$Sales )

lines(Reg_Spline_Adv$Sales, predSalAdvMdl1, col="red")
## first knot
cost55 = Reg_Spline_Adv$Adv_Cost - 55.5

cost55

cost55[ cost55 < 0 ] = 0

## second knot

cost57 = Reg_Spline_Adv$Adv_Cost - 57

cost57[cost57 < 0] = 0

fnlMdl = lm(Reg_Spline_Adv$Sales~Reg_Spline_Adv$Adv_Cost+cost55+cost57)

fnlPred = predict(fnlMdl)

plot(Reg_Spline_Adv$Adv_Cost, Reg_Spline_Adv$Sales)

lines(Reg_Spline_Adv$Adv_Cost, fnlPred, col="red")

cost55Cube = cost55^3

cost57Cube = cost57^3

fnlMdlC = lm(Reg_Spline_Adv$Sales ~ Reg_Spline_Adv$Adv_Cost + cost55Cube +  cost57Cube +  poly(cost55 , 3, raw=TRUE)+ poly(cost57, 3, raw=TRUE))

fnlMdlC1 = lm(Reg_Spline_Adv$Sales ~ Reg_Spline_Adv$Adv_Cost +   poly(cost55 , 3, raw=TRUE)+ poly(cost57, 3, raw=TRUE))

fnlMdlC2 = lm(Reg_Spline_Adv$Sales ~ Reg_Spline_Adv$Adv_Cost +   poly(cost55 , 3, raw=TRUE))

## cost55Cube +  cost57Cube +

prd = predict(fnlMdlC)

prd1 = predict(fnlMdlC1)

prd2 = predict(fnlMdlC2)

plot(Reg_Spline_Adv$Adv_Cost, Reg_Spline_Adv$Sales)

lines(Reg_Spline_Adv$Adv_Cost, prd, col="red" )


plot(Reg_Spline_Adv$Adv_Cost, Reg_Spline_Adv$Sales)


lines(Reg_Spline_Adv$Adv_Cost, prd1, col="green" )

lines(Reg_Spline_Adv$Adv_Cost, prd2, col="blue" )
