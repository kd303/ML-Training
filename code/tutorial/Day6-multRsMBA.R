
library(readr)
marBasketData <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Mar_Basket.csv")
View(marBasketData)

target = factor(marBasketData$items)
ident = marBasketData$Id

library(arules)

transactions=as(split(target,ident),)

transactions = as(split(target,ident), "transactions")

rules = apriori(transactions, parameter = list(support = 0.25, confidence = 0.05, minlen = 2))

rules

rules = sort(rules, decreasing = TRUE, by="lift")

inspect(rules)


install.packages("arulesViz")

install.packages("kernlab")

install.packages("grid")

library("arulesViz")
plot(rules)