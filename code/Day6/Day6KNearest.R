### k nearest

## optimum k value using validation
## distance between two point using eculidean distance

library(class)

library(readr)
iris <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Iris.csv")
View(iris)


x = iris[,1:4]

y = iris[,5]

x = as.matrix(x)
y = as.matrix(y)

predict = knn(x, x , y, k =5)

table(y, predict)

prop.table(table(y, predict))*100

library(readr)
irisTst <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Iris_test.csv")
View(irisTst)


xTst = irisTst[,1:4]

yTst = irisTst[,5]

xTst = as.matrix(xTst)
yTst = as.matrix(yTst)

prTst = knn(x, xTst , y, k =5)

prTst

prop.table(table(yTst, prTst))*100
