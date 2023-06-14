setwd("C:\\Users\\mraer\\Desktop\\UW\\Semester-2\\Advanced Econometrics\\Econometrics")
Sys.setenv(LANG = "en")


library(xts)
library(lmtest)
library(fBasics) 
library(urca) 
library("lmtest")
library(ggplot2)

source("function_testdf2.R")


dataset1 <- read.csv('Aurum.csv')
dataset2 <- read.csv('petrol.csv')


#dataset1 <- dataset1[, c(1, 5)]

colnames(dataset1) <- c('DATE', 'GOLD')
colnames(dataset2) <- c('DATE', 'OIL')

dataset1$DATE <- as.Date(dataset1$DATE, format = "%Y-%m-%d")
dataset2$DATE <- as.Date(dataset2$DATE,  format = "%Y-%m-%d")

merged_ts <- merge(dataset1, dataset2, by = "DATE", all = TRUE)


merged_ts$OIL <- as.numeric(merged_ts$OIL)
merged_ts$GOLD <- as.numeric(merged_ts$GOLD)

merged_ts <- na.omit(merged_ts)

#merged_ts <- merged_ts[seq(1, nrow(merged_ts), 2), ]

#merged_ts <- merged_ts[1:500,]



merged_ts <- xts(merged_ts[, -1], merged_ts$DATE)


merged_ts$dOIL <- diff.xts(merged_ts$OIL)
merged_ts$dGOLD <- diff.xts(merged_ts$GOLD)

plot(merged_ts$OIL, 
     main = 'Oil vs Gold indicies', lwd = 0.7, col = "red")

lines(merged_ts$GOLD, col = "blue", lwd = 0.5)

legend("topleft", c("Oil", "Gold"), 
        col = c("red", "blue"), lty = 1)

testdf2(variable = merged_ts$OIL, test.type = "c", 
        max.augmentations = 12, max.order = 12)

testdf2(variable = merged_ts$dOIL, test.type = "nc", 
        max.augmentations = 12, max.order = 12)

# 3rd Row = Aug 2 Order 1


testdf2(variable = merged_ts$GOLD, test.type = "c", 
        max.augmentations = 12, max.order = 12)

testdf2(variable = merged_ts$dGOLD, test.type = "nc", 
        max.augmentations = 12, max.order = 12)

# 9th Row = Aug 8 Order 1



model.coint <- lm(OIL~GOLD, data = merged_ts)
summary(model.coint)

testdf2(variable = residuals(model.coint), test.type="nc",
        max.augmentations = 9, max.order = 9)

# Row 2 thus 1 augmentation (m=1) test statistics = -3.727834

length(residuals(model.coint)) #=234

#Checking the chart provided by Prof. Rafal, at 5% (N=2) the critical value is -3.37. Our test statistic = -3.727834 is in the rejection zone/ intervl
#Thus we reject the NUll Hypothesis that the TS is non-stationary and conclude Stationary TS with
#Co-integration. So price of Gold and Petrol are cointegrated

### Cointegratiing Vector: We check residual summary
#[1, 3.70444 (intercept), -0.27005(Gold)] <- Remember opposite sign of there
#petrol = -3.7044+0.27005*Gold + Epsilon

#This vector defines the linear combination of the Gold and Petrol price that is stationary.
#This also defines the long term relationship between the petrol and gold price
#In the long term, if Gold increases by 1 unit then Petrol increases by 0.27005 unit


####################################Cointegration Model: ERROR Correction

#1. Creating Lagged Residuals

merged_ts$lresid <- lag.xts(residuals(model.coint))

#2. Estimating ECM

model.ecm <- lm(dOIL~dGOLD + lresid -1, data = merged_ts)
summary(model.ecm)

# Error-Correction Mechanism model
# delta(PETROL)=beta_1*delta(GOLD)+beta_2*(lagged residuals) + epsilon
# delta(PETROL) = 0.19313*delta(GOLD) -0.07114*(lagged residuals) + epsilon

#0.19313 is known as the short term dynamics. If delta(GOLD) is increased by one unit, delta(PETROL) will be increased 0.19313 unit

# -0.07114*(lagged residuals) descrbes if there is a unitary shock to the relation between Petrol and Gold in steady state solution, the system goes out of the steady
#state solution.After one period, the system will correct itself by 7.011% of the shock
# THIS IS THE ERROR CORRECTION MECHANISM as the system Teaches on the previous errors and corrects on the next period.So the system needs approximately 14 periods to
# return to the steady state solution

#The Lagged Residual Parameter (coefficient alpha_2) should be within the interval [-2,0]. It means that the shock is corrected and error correction mechanism works

# lresid = petrol +3.70444 -0.27005*Gold <- From Cointegrating Vector Model


## LONG TERM RELATION: described here by Cointegrating vector
## Short TERM RELATION: described here by ECM Vector






########################### General to Specific (GETS Approach)

# Granger Causalty is used to determine the Causal relationship between the variables based on their predictive power.
# There there are only two variables, GETS using backward or forward elimination is not possible.
# Rather, Granger Causalty can determine the possible outcome where one variable can be used to predict another variable, indicating a potential causal relationship.
# It helps in the process of model selection by identifying the lagged variables that have predictive power for the dependent variable. By examining the significance 
# of the lagged variables in the model, you can determine if they contribute to the explanatory power of the model and provide evidence of causal relationships.

names(merged_ts)

grangertest(x=merged_ts$dOIL, y=merged_ts$dGOLD, order = 3)
# H0: the first, the second, and the third lags of OIL are insignificant
# OIL lags are inignificant
# OIL does not cause GOLD:  where OIL variable cannot be used to predict another variable GOLD

grangertest(x=merged_ts$dOIL, y=merged_ts$dGOLD, order = 4)
# Now at Lag3, Price of Oil is helpful to explain the model Price of Gold. So Price of Oil is a cause to Price of Gold
#H0 is rejected > OIL lags at order 4 are significant

# Checking the other way:
grangertest(x=merged_ts$dGOLD, y=merged_ts$dOIL, order = 3)
# H0: the first, the second, and the third lags of GOLD are insignificant
# GOLD lags are inignificant
# GOLD does not cause OIL:  where GOLD variable cannot be used to predict another variable OIL

grangertest(x=merged_ts$dGOLD, y=merged_ts$dOIL, order = 4)
# Now at Lag3, Price of GOLD  is helpful to explain the model Price of OIL. So Price of GOLD is a cause to Price of OIL
#H0 is rejected > GOLD lags at order 3 are significant


##^^^^^^^^^^^^^^^^^^^^^  CAUSAL RELATIONSHIP BETWEEN BOTH VARIABLE AT ORDER 4 IS ESTABLISHED


################### Ramsey Rest TEST: CHECKING Linearity of the Model 

resettest(model.coint, power = 2, type="fitted")

## p-value = 0.1613/0.9 if power is 3 > above 5% significance level > Failing to Reject the Null Hypothesis and establishing that the model is okay.
## ALSO The Ramsey Reset test is typically used to detect nonlinearities in the relationship between the dependent variable and the independent variables in a linear regression framework.
## SINCE Cointegration is already established, it is not actually fruitful. As long term and short term relationship was established, this test can be ommited.




###################### Breusch-Pagan’s and White’s tests–homoscedasticity

bptest(model.coint, studentize=TRUE)

# p-value = 0.001499 > null hypothesis of homoscedasticity is rejected in favour of heteroscedasticity
# it suggests that the assumption of constant variance is violated. This can affect the reliability of the estimated cointegration relationship.



#################### Breusch-Godfrey test–no autocorrelation

bgtest(model.coint, order = 1)

## p-value < 0.00000000000000022 > Null hypothesis is rejected: There is strong autocorrelation

