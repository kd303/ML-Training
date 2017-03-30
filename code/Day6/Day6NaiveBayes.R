library(readr)
irisData <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Iris.csv")
View(irisData)


as.factor(irisData$Class)

x = irisData[,1:4]
y = irisData[,5]

x = as.matrix(x)

y = as.matrix(y)

y = as.factor(y)

library(e1071)

irismdl = naiveBayes(x, y)

summary(irismdl)

pred = predict(irismdl, x)

table(y, pred)

prop.table(table(y, pred)) * 100


library(readr)
Iris_test <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Iris_test.csv")
View(Iris_test)


xtest = Iris_test[,1:4]
ytest = Iris_test[,5]

xtest = as.matrix(xtest)

ytest = as.matrix(ytest)

ytest = as.factor(ytest)

prdTst = predict(irismdl,xtest)

table(ytest, prdTst)

prop.table(table(ytest, prdTst))*100


