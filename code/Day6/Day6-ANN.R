
library(readr)
multRegConv <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Mult_Reg_Conversion.csv")
View(multRegConv)


colnames(multRegConv) = c("temp", "time","kappnum", "conv")

library(neuralnet)

maxs = apply(multRegConv,2, max)

mins = apply(multRegConv,2, min)

multRegSclData = scale(multRegConv, center = mins, scale= maxs - mins)

apply(multRegSclData, 2, max)

apply(multRegSclData, 2, min)

multRegSclDf = as.data.frame(multRegSclData)

## layers - C(3,2,2) - 3 layer, each layer num of neurons
## number of neurons = 2/3 * num X - so in this case it will be 2/3*3 = 2
## linear.output = TRUE - for real number, false for Y is descreet

mymodel= neuralnet(multRegSclDf$conv ~ multRegSclDf$temp + multRegSclDf$time + multRegSclDf$kappnum, hidden = c(2), data=multRegSclDf)

mymodel2 = neuralnet(multRegSclDf$conv ~ multRegSclDf$temp + multRegSclDf$time + multRegSclDf$kappnum, hidden = c(2,1), data=multRegSclDf)

mymodel3 = neuralnet(multRegSclDf$conv ~ multRegSclDf$temp + multRegSclDf$time + multRegSclDf$kappnum, hidden = c(2,2,1), data=multRegSclDf)

plot(mymodel)

plot(mymodel2)

plot(mymodel3)

mymodel$result.matrix
## predict
pred = mymodel$net.result[[1]]

res = multRegSclDf$conv - pred

cbind(multRegSclDf, pred, res)

plot(res)

MSE= mean(res^2)

TSS = var(multRegSclDf$conv)*15

(1 - (MSE/TSS))*100


library(readxl)
testData <- read_excel("D:/z_kaushal/ISIDMBA/DataSets/Converion_Std_Output.xls")
View(testData)

output = compute(mymodel, testData)

output$net.result

#####################################Excersize#####################################

library(readxl)
annExcer <- read_excel("D:/z_kaushal/ISIDMBA/DataSets/ANN_Exercise.xls")
View(annExcer)


maxsEx = apply(annExcer,2, max)

minsEx = apply(annExcer,2, min)

annExcerSclData = scale(annExcer, center = minsEx, scale= maxsEx - minsEx)

apply(annExcerSclData, 2, max)

apply(annExcerSclData, 2, min)

annExcerSclDF = as.data.frame(annExcerSclData)

sampleId = sample(2, 111,replace = TRUE, prob = c(0.8,0.2))

annExcerSclTraining = annExcerSclDF[sampleId == 1,]
annExcerSclTest = annExcerSclDF[sampleId == 2,]
View(annExcerSclTraining)

annexcrmdl = neuralnet(annExcerSclTraining$`Sprint_Productivity `~annExcerSclTraining$Team_Size+annExcerSclTraining$Module_Size+annExcerSclTraining$Est_effort+annExcerSclTraining$Domain_Skill+annExcerSclTraining$`Agile_Skill `,hidden = c(3), data=annExcerSclTraining)

plot(annexcrmdl)

prdTrn = annexcrmdl$result.matrix[[1]]

resTrn = annExcerSclTraining$`Sprint_Productivity ` - prdTrn

RSS = mean(resTrn^2)
TSS = var(annExcerSclTraining$`Sprint_Productivity `)*90

rsqr = (1- RSS/TSS)*100
View(annExcerSclTest)

prdTst = compute(annexcrmdl, annExcerSclTest[,1:5])



annexcrmdl$result.matrix


