## company claims that it taske 40 hours or less

## H0  avg processing <= 40
## H1 avg processing > 40



describe(po)

## mean - 49
## se  - 2.43

## 
tval = (49-40)/2.43

t.test(po$Processing_Time, alternative = "greater", mu = 40)

## p value = 0.0001753, H1 is accepted, takes more than 40 hrs

## exercise # 2

## H0  avg response <=24                                                           
## H1 avg response > 24

t.test(CRTime$`Response Time`,alternative = "greater", mu=24)

## p value is 6.675E-08 , (p < 0.05) takes more than 24 hrs, H1 accepted


## test of 2 hypothesis, 2 groups

## H1 - avg sales in outlet 1 => avg sales in outlet 2
# #H0 avg sales in outlet 1 <= avg sales outlet 2

outlet = factor(Sales_Promotion$Outlet)

t.test(Sales_Promotion$Sales ~ outlet, alternative = 'greater')
## p > 0.07 hence they are similar
var.test(Sales_Promotion$Sales ~ outlet)

boxplot(Sales_Promotion$Sales ~ outlet)

## Page 69
## mean utilization for both method is same or not
## H0 - meanutilzation of method old = mean utilization of new method
## H1 - mean utilzation of old method != mean util of old method

method = factor(bpo_util$Method)

t.test(bpo_util$Utilization ~ method)
## p value is 0.84 which is greater then > 0.05 since H0 is accepted.

## F-stat

var.test(bpo_util$Utilization ~ method)

## p-value -  0.7359, which is greater > 0.05 hence null hypothis
##for variace is accepted


## Paired T-test ## homogenious data samples (same outloet giving the data
## )


## page 73

## H0 both are avgs are same, H1 they are not equal.

b1 = Tires$`Brand 1`
b2 = Tires$`Brand 2`

t.test(b1,b2, paired = TRUE)
## p-value = 0.09863, if the p-value 0.09 is greater than 0.05 hence
## difference in mean is equal to zero, is H0 is accepted.
boxplot(Tires)

## H0 - wieght before diet > wieght after diet (program fail)
## H1 - wieght before diet <= wieght before diet (program success)

t.test(Diet$Before, Diet$After, alternative = 'greater', paired=TRUE)

## p-value = 1.208e-05, p < 0.05 or p is not > 0.05 hence h0 is rejected, H1 is acceptd
## hence program is success 

boxplot(Diet$Before, Diet$After)

## descrete data test.


## At least >= 99%  accurate or no rework H0
##  <1% must rework  H1
## H0 and H1 - should be specified based on alphabatical order
## so in loan dataset, Good and Rework - Good should be used for null
## hypothisis
loan = factor(Loan_processing$Loans)

table(loan)

prop.test(table(loan), alternative = "less", p=0.99)

##  p-value = 3.574e-05 which is < 0.05 hence the H0 rejected.
## H0 should have equal sign in it. In the test function
## alternate should be used of H1 - cause we are providing alternate
## hypothesis.
## excer 2 / 152
## proportion of damaga <= 0.02 ----- H0
## proportion of damage > 0.02  ------H1

prop.test(table(Shipment), alternative = "greater", p=0.02)

prop.test(table(loan), alternative = "less", p=0.98)

##  p-value = 0.9084 which is greater than 0.05 H0 is accepted

## Two proportion test !
## 

## excersize 1 # Page 78

## order defective processing India < order defective processing in manila H1 
## order defective processing India >= good order processing in manila H0

 table(Order_Processing)
 
 prop.test(table(Order_Processing), alternative = "less")
 
 # p-value = 0.01767 less then 0.05 hence H0 is rejected. H1 is accepted.
 
 
 
 
 ###### Normal distribution - 
 
 hist(PO_Processing$Processing_Time)
 
 hist(PO_Processing$Processing_Time, breaks=7)
 
 qqnorm(PO_Processing$Processing_Time)
 qqline(PO_Processing$Processing_Time)
 
 shapiro.test(PO_Processing$Processing_Time)
 
 
 ############3 ANNOVA #############333
 
 location = factor(Sales_Revenue_Anova$Location)
 
 boxplot(Sales_Revenue_Anova$`Sales Revenue`~location)
 
 z = aov(Sales_Revenue_Anova$`Sales Revenue`~location)
 
 ## anova - response is continuous, explanator is categorical variable
 ## look at which factor is impacting more looking at pValue
 ## which should less than or equal to 0.05 to be accepted******
 ## you can do percentaage of sum of squares - 
 ## 1. it is controlled via given  variable ()
 ## 2. If residual sum sq. is more then some other variables are impacting
 ## 3. F stat gives variance which is affected or not.
 ## All assumptions - model normality of residuals
 
 summary(z)
 
 
 ## study of residuals - whether the error is following normal
 ## distribution
 z$residuals
 
 qqnorm(z$residuals)
 qqline(z$residuals)
 
 ## checks the normal distribution if P >= 0.05 then normal distribution
 shapiro.test(z$residuals)
 
 ## here p-value = 0.5284 which is p >= 0.05 it follows normal distribution
 
 aggregate(Sales_Revenue_Anova$`Sales Revenue`~Sales_Revenue_Anova$Location, FUN=mean)
 
#### Capsule data
 ## DF(AB) = DF(A) * DF(B)
 ## DF(A) = LEVEL(A) - 1
 
 Capsule
 
 model = aov(Capsule$Time~Capsule$Juice+Capsule$Capsule+Capsule$Juice*Capsule$Capsule)
 ## even if individual variables are not identified then 
 ## also model considers the those variable mentioned in interaction
 model1 = aov(Capsule$Time~Capsule$Capsule+Capsule$Juice*Capsule$Capsule)
 
 summary(model)
 
 summary(model1)
 ## model residuals are high almost 50% of explanatory variables, means
 ## something a miss in the variables.
 model$residuals
 shapiro.test(model$residuals)
 qqnorm(model$residuals)
 qqline(model$residuals)
 
 interaction.plot(Capsule$Juice, Capsule$Capsule, Capsule$Time, fun = mean)
 
 interaction.plot(Capsule$Capsule, Capsule$Juice, Capsule$Time, fun = mean)
 
