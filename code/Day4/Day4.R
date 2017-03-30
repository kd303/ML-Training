## Unsupervised learning

## Factor analysis

factortoothpaste <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Factor_Analysis_Example.csv", col_types = cols(Respondent = col_skip()))


cor(factortoothpaste)
scaledToothpaste=scale(factortoothpaste) # scaled data - z - scaling

apply(scaledToothpaste,2,sd)
apply(scaledToothpaste,)
apply(scaledToothpaste,2,mean)

cor(scaledToothpaste)

## x1,x3,x5

## x2,x4,X6
## KMO statsitcs
KMO(scaledToothpaste)

## overall MSA = 0.66, if the overall MSA > 0.5 than we can do Factor analysis

## if it is 
## corelation analysis - for high number of variables
cortest.bartlett(scaledToothpaste)
## H0 is not corelated / H1 corelated, if p-val >= 0.05 H0 cannot be rejected
## p value is 9.013E-17 < 0.05 we can go with factor test

pincipaltoothpaste = princomp(scaledToothpaste)

summary(pincipaltoothpaste)

plot(pincipaltoothpaste)
## loading is w1, w2, w3
loadings(pincipaltoothpaste)

toothscore = pincipaltoothpaste$scores
## score = loading*value of x1, x2, x3, x4 - only interested in comp1 and comp2
score=toothscore[,1:2]
## scoring 
## compare the values and bucket the customer in a bucket with highest value
## 
score

toothscore

## maximum likely hood algorithm
## factanal - by default does maximum likelyhood - where you select the components/factor


toothpasteMax= factanal(scaledToothpaste,2)

## see the model
toothpasteMax


summary(toothpasteMax)

## Excersize
factor_excer <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Factor_Analysis_Exercise.csv", 
                         col_types = cols(Id = col_skip()))
View(factor_excer)

factorExcerScaled=scale(factor_excer)

KMO(factorExcerScaled)
## MSA value is 0.67, hence can proceed

cortest.bartlett(factorExcerScaled)
## p-value 1.00E-22 - H1 accepted - hence move on.

excerModel = princomp(factorExcerScaled)
## variance = square of standard deviation is greater then 1 take those as component
## cumulative % > 65
summary(excerModel)

## 5 components eigen is < 1 and cumulative variance > 0.65

excerscore=excerModel$score[,1:5]
## check the value of each variable against top components, and highest value means
## it resides with that factor / bucket
loadings(excerModel)

excerModel

excerscore

newmodelExcer = factanal(factorExcerScaled,5,rotation = "varimax", scores = "regression")

newmodelExcer

### cluster analysis

library(readr)
clusterAnalExample <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Cluster_Analysis_Example.csv", 
                               col_types = cols(`Case No.` = col_skip()))
View(clusterAnalExample)

clusterAnalDistance = dist(clusterAnalExample, method='euclidean')

clusterAnalDistance

## clustering based on avg. distnace - use of ward method for linkage based on avg. distance
clusterAnalHClust = hclust(clusterAnalDistance, method = "ward")
## check the distance and see whether the distance is optimal or not.
plot(clusterAnalHClust)

rect.hclust(clusterAnalHClust, 3, border="red")

rect.hclust(clusterAnalHClust, 4, border="blue")
## 2 groups - it includes larger distance between last two groups the distance is large hence not accepted.
rect.hclust(clusterAnalHClust, 2, border="green")


clusterid = cutree(clusterAnalHClust,3)

clusterid
## tells you which recor is which group
outputCluster = cbind(clusterAnalExample,clusterid)
## indicates average rating across how three groups - whcih indicates their pattern
## look avg, median, sd to pattern across the group
aggregate(clusterAnalExample,by=list(clusterid),FUN=mean)
# 75.8
kmodelClusterExp = kmeans(clusterAnalExample, 3)
# 47.5
kmodelClusterExp2 = kmeans(clusterAnalExample, 2)

# 79.6
kmodelClusterExp4 = kmeans(clusterAnalExample, 4)
# 83.6
kmodelClusterExp5 = kmeans(clusterAnalExample, 5)
# 86.7
kmodelClusterExp6 = kmeans(clusterAnalExample, 6)

kmodelClusterExp7 = kmeans(clusterAnalExample, 7)

kmodelClusterExp8 = kmeans(clusterAnalExample, 8)

k = c(1,2,4,5,6,7,8)

BGSS = c(kmodelClusterExp2$betweenss, kmodelClusterExp$betweenss, kmodelClusterExp4$betweenss, kmodelClusterExp5$betweenss, kmodelClusterExp6$betweenss, kmodelClusterExp7$betweenss, kmodelClusterExp8$betweenss)

plot(k, BGSS, type="b")


kmodelClusterExp
## optimal test
## with-in the group variance should be minimum
## between the group variance should be maximum

## with-in group variation / total variations
## between group variation / total variation



