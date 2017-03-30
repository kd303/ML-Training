## Dimensionality Reduction technique

toothPasteData <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/Factor_Analysis_Exercise.csv",col_types = cols(Id = col_skip()))

## factor analysis - removing non influencing variables in such a way that
# ## no loss of information is observed.
# Generally loss of information is measured by variability 
# factors are the way to associate corelated variables

## step 1: scale the data
# Scaling reduces the bias towards larger values.
# 1. Min/Max transformation -> (data - min)/(max-min)
# 2. z transformation - z = (data - mean)/standard deviation
scaledToothPasteData = scale(toothPasteData)

# ## step 2 : Sampling Adequecy Test - Measure of proportion of variance among variables 
#     that might be common variance
# KMO values close to zero means there are large correlations compared 
# to sum of correlation
# MSA (model sample adequecy) value of > 0.6 is acceptable - higer the better
KMO(scaledToothPasteData)


# step 3 : Bartlett Test for correlation analysis
# 
# H0 is correlation matrix = identity matrix ( no corrleation) - p >= 0.05
# H1 is correlation matrix != identity matrix (corrleation) - p < 0.05

cortest.bartlett(scaledToothPasteData)

# p-value is 1.006974e-22 < 0.05 hence H0 is rejected, there are corrlations

# step - 4 There are two methods that can do factor analysis
# 1. Principal component Analysis
# 2. Maximum likely hood test
# Method 1
toothPasteModel = princomp(scaledToothPasteData)
## see the summary of the model
summary(toothPasteModel)
## scree plot
plot(toothPasteModel)
# 
# ## There are total 14 components (factor) are present  the most significant one is 
# variance value (in R it is deviation so take square of deviation) of that factor  > 1 
# or cumulative proportion <= 0.6
## so top 6 Factors are selected

finalScore = toothPasteModel$scores[,1:5]
## gives a correlation matrix, highest absolute value determines which variable
## aligns to which factor
loadings(toothPasteModel)

# Method 2: Maximum likelyhood analysis 
## difference is - provide the number of factors upfront
## what does the score option significe in below ??
finalModel = factanal(scaledToothPasteData,5 , rotation = "varimax", scores = "regression")
## earlier Q3 was not sure where the analysis sits
## whislt it is clear now after applying varimax rotation 
## Finally based on where the variable and factor corelates, typicall all multi-corleation variables 
## under one grouping and can use for some analysis
loadings(finalModel)

## this is whole piece of crap that I dont understand and copy pasted from r-bloggers!
install.packages("nFactors")

library(nFactors)

ev=eigen(cor(toothPasteData))

ap=parallel(subject=nrow(toothPasteData), var=ncol(toothPasteData), rep = 100, cent = 0.5)

nS = nScree(x=ev$values, aparallel = ap$eigen$qevpea)

plotnScree(nS)

## Structural equation modelling - usless piece of crap again.
install.packages("sem")
library(sem)
cv = cov(toothPasteData)
model.cv = specify.model()
