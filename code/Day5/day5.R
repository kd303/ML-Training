
# logistic regression - day 5 - Example

library(readr)
resortVisitData <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Resort_Visit.csv")
View(resortVisitData)


colnames(resortVisitData) <- c("visit", "income", "attitude", "importance", "size","age")
## check the correlation
cor(resortVisitData)
## step 1 : covert the Y to a factor
resortVisitData$visit = factor(resortVisitData$visit)
## higher the difference in means - stronger the variable. impact
aggregate(resortVisitData$income~resortVisitData$visit, FUN = mean)

aggregate(resortVisitData$attitude~resortVisitData$visit, FUN = mean)
## step 2 : create a basic logistic regression model
resortVisitModel= glm(resortVisitData$visit~resortVisitData$income+resortVisitData$attitude+resortVisitData$importance+resortVisitData$size+resortVisitData$age, family=binomial(logit))
## step 3 : see the p-values ofintercept
summary(resortVisitModel)
## step 4: test of deviance 
## value of residual deviacnce is decreaseing with adding a singificant variables in 
## the model.
anova(resortVisitModel, test="Chisq")
## step 5: create a model with new variables 
resortVisitNewModel= glm(resortVisitData$visit~resortVisitData$income+resortVisitData$importance+resortVisitData$size, family=binomial(logit))

summary(resortVisitNewModel)

## in the absence of Rsquire how do you know model is singnificant? cd plots - conditional density plots

cdplot(resortVisitData$visit~resortVisitData$income)

cdplot(resortVisitData$visit~resortVisitData$size)

cdplot(resortVisitData$visit~resortVisitData$importance)
## 
response = predict(resortVisitNewModel, type = "response")

residuals(resortVisitNewModel, type="deviance")
## predicting the threshold of 0.5 
predVisits = ifelse(predict(resortVisitNewModel, type="response")>0.5,"1","0")
## confusion matrix - equivalen of R - donot look at residual analysis
accuracyTable = table(resortVisitData$visit,predVisits)

## correct predictions are 0.86 and error s 0.133
prop.table(accuracyTable)


#####################################################################################

mailResponseData <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Mail_Respond.csv", 
                             col_types = cols(`SL No` = col_skip()))
View(mailResponseData)


colnames(mailResponseData)=c("district","housetype","income","prevcust","outcome")

library(rpart)

install.packages("rpart.plot")

library(rpart.plot)
# minimum number of records required at that node - minsplit = 10
mailResponseModel=rpart(mailResponseData$outcome~mailResponseData$district+mailResponseData$housetype+mailResponseData$prevcust+mailResponseData$income,method="class", control=rpart.control(minsplit = 10))

# cost of complexcity factor

plotcp(mailResponseModel)

## reading the plot

## size of the tree =  6 , use cp value is 0.03 take that as a size of the tree

optimizedModel = prune(mailResponseModel, cp=0.03)

rpart.plot(optimizedModel)
## * is where the node is ending
print(optimizedModel)
## 2nd branch - High = true, number of responses,  number of "no" (accuracy of prediction at the branch)
summary(optimizedModel)

predOutcome = predict(optimizedModel, type="class")

confusionMailOutcom = table(mailResponseData$outcome,predOutcome)

prop.table(confusionMailOutcom)*100


## mailResponseData$income=High 47 19 No (0.5957447 0.4042553) 
 # at this node, total points 47, outcome positive number is 19, 
# and predicting No (error, accuracy)
## probability of getting yes is (0.4) out of 47 records. 19 yes and rest is no
## probability if getting yes 

### excersize

library(readr)
bankData <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/bank-data.csv")
View(bankData)
## sampling with replacement and withouth replacement.
## repacement = TRUE - when 100% data is to be split then 
bankdataSample = sample(2,600,replace = TRUE, prob = c(0.8,0.2))

bankDataTrain = bankData[bankdataSample ==1,]
bankDataTest = bankData[bankdataSample ==2,]
## . & comma ## 10% of remaining
bankDataModel=rpart(bankDataTrain$pep~., data=bankDataTrain, method="class", control = rpart.control(minsplit = 45))

plotcp(bankDataModel)

bankDataModel = prune(bankDataModel, cp=0.019)

rpart.plot(bankDataModel)

predBankData = predict(bankDataModel, type = "class")

predBankTable = table(bankDataTrain$pep, predBankData)
## 87% accuracty 11% error
prop.table(predBankTable)*100

preBankTest = predict(bankDataModel, type="class", newdata = bankDataTest)

bankTestTable = table(bankDataTest$pep, preBankTest)
## 85% accuracy 14% error
prop.table(bankTestTable, 1)*100

prop.table(bankTestTable, 2)*100


#############################

library(readr)
multKappaRegTree <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Mult_Reg_Conversion.csv", 
                             col_types = cols(`SL No.` = col_skip()))
View(multKappaRegTree)

multKappaRegTreeModel=rpart(multKappaRegTree$`% Conversion`~., method ="anova", data= multKappaRegTree, control = rpart.control(minsplit = 2))

plotcp(multKappaRegTreeModel)


multKappaREgTreeMdl = prune(multKappaRegTreeModel, cp=0.017)

rpart.plot(multKappaREgTreeMdl)

conversion = predict(multKappaREgTreeMdl)

cbind(conversion, multKappaRegTree$`% Conversion`)

residuals = residuals(multKappaREgTreeMdl)

MSE = mean(residuals^2)

RMSE = sqrt(MSE)

### Total sum of squares & R square
total_ss = var(multKappaRegTree$`% Conversion`)*15
rsq = 1 - (sum(residuals^2))/total_ss
## 0.97
rsq

# Bagging - only rows will be sampled
### bootstrap sampling - Jack Kinfe sampling - it will take randomly takes 80% data for 500 times
## all models will predict the outcome - and final output will be mean(continous)/ vote(categorical) of all outcome values.

# Random Forrest
## 500 models will not take all columns and do bagging
## will take care of outliers - both row and column will be sampled
## no of variable will it will take sqrt of number of predictors (xs) - classification
## regression tree will be take Xs P/3 -

iris <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Iris.csv")
View(iris)

require(randomForest)

iris$Class= as.factor(iris$Class)

install.packages("randomForest")



library(randomForest)

irisMdl = randomForest(iris$Class~iris$`sepal length`+iris$`sepal width`+iris$`petal length`+iris$`petal width`, data=iris)

summary(irisMdl)

View(Iris_test)

predict(irisMdl, newdata = Iris_test)

########################## Bagging ###################################################

library(readr)
bostonHsngBagging <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Boston_Housing Data.csv")
View(bostonHsngBagging)


sampleids= sample(2, 506, replace= TRUE, prob = c(0.8,0.2))

trainigBstnHsg = bostonHsngBagging[sampleids==1,]

testBstnHsg = bostonHsngBagging[sampleids==2,]

bstnHsgMdlBagg = randomForest(trainigBstnHsg$MEDV~., data=trainigBstnHsg, mtry = 13, importance = TRUE)

bstnHsgMdlBagg

## MSE 10.844
## explained variace 87.56
## RMSE 3.29

prdtest = predict(bstnHsgMdlBagg, newdata = testBstnHsg)

res = testBstnHsg$MEDV - prdtest
## 7.2319
T_MSE = mean(res^2)

## T_RMSE - 2.68

importance(bstnHsgMdlBagg)
## node impurity is more important 
## 
varImpPlot(bstnHsgMdlBagg)

T_RMSE = sqrt(T_MSE)

rsqr = 1 - (sum(res^2)/(var(testBstnHsg$MEDV)*105))
## 0.90 
rsqr

#################################################################################
# polynomial regresssion

library(readr)
nonlinrData <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Nonlinear_Thrust.csv")
View(nonlinrData)

plot(nonlinrData$x1, nonlinrData$y)

plot(nonlinrData$x2, nonlinrData$y)

plot(nonlinrData$x3, nonlinrData$y)

cor(nonlinrData)

nonlnrMdl = lm(nonlinrData$y~nonlinrData$x1 + nonlinrData$x2 + nonlinrData$x3)

summary(nonlnrMdl)

qqnorm(nonlnrMdl$residuals)

plot(nonlnrMdl$residuals)

library(car)
## gives plot of X vs partial residuals (taking other affect out)
crPlots(nonlnrMdl)

## red and green are same then linear, else non-linear relationship

## trail and error with power function cube / 
## use orthonoial, ploynomi

nonlnrMdlNw = lm(nonlinrData$y ~ poly(nonlinrData$x1, 2, raw=TRUE)+nonlinrData$x2+nonlinrData$x3)

summary(nonlnrMdlNw)

## metrics diagnoal are zero, if large 

crPlots(nonlnrMdlNw)


nonlnrMdlNw3 = lm(nonlinrData$y ~ poly(nonlinrData$x1, 3, raw=TRUE)+nonlinrData$x2+nonlinrData$x3)

crPlots(nonlnrMdlNw3)

plot(nonlnrMdlNw3$residuals)

nonlnrMdlNw32 = lm(nonlinrData$y ~ poly(nonlinrData$x1, 3, raw=TRUE)+poly(nonlinrData$x2, 3, raw=TRUE)+nonlinrData$x3)
crPlots(nonlnrMdlNw32)
summary(nonlnrMdlNw32)

plot(nonlnrMdlNw32$residuals)

nonlnrMdlNw33 = lm(nonlinrData$y ~ poly(nonlinrData$x1, 3, raw=TRUE)+poly(nonlinrData$x2, 3, raw=TRUE)+poly(nonlinrData$x3, 3, raw=TRUE))
crPlots(nonlnrMdlNw33)
plot(nonlnrMdlNw33$residuals)
summary(nonlnrMdlNw33)


nonlnrMdlNw34 = lm(nonlinrData$y ~ poly(nonlinrData$x1, 3, raw=TRUE)+poly(nonlinrData$x2, 3, raw=TRUE)+sqrt(nonlinrData$x3))
crPlots(nonlnrMdlNw34)
plot(nonlnrMdlNw34$residuals)
summary(nonlnrMdlNw34)

