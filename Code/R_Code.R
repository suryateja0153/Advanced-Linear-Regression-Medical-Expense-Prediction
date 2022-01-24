#Author: Suryateja Chalapati

#Import Libraries
library(readxl)
library("car")
library(lmtest)
library("ggpubr")
library(stargazer)
library(data.table)

setwd("C:/Users/surya/Downloads")

hi <- read_excel("HealthInsurance.xlsx", sheet = 'Data')

#NA values column wise
sapply(hi, function(x) sum(is.na(x)))
str(hi)

#Checking if DV is suitable for OLS
hist(hi$medexpense, col = 'lightcoral', main = "Histogram of Medical Expense", xlab = 'MedExpense', ylab = 'Frequency')
hist(hi$logmedexpense, col = 'lightgreen', main = "Histogram of log(Medical Expense)", xlab = 'log(MedExpense)', ylab = 'Frequency')
#Histogram shows the DV follows an exponential distribution. Log of DV makes it a ND
#hist(hi$income)
#hist(hi$logincome)
#Histogram for income follows an exponential distribution. Log of income makes it a ND

#Feature Engineering/Pre-processing
#Converting categoricals into binary factor levels
names(hi) <- tolower(colnames(hi))
hi$health <- ifelse(hi$poor == 1, 1, ifelse((hi$verygood + hi$good + hi$fair) == 1, 2, 0))
str(hi)

#Checking correlations
#1,2:8,20,22:23   1,3,6,8,9,17:20,21,22,25,26,27    1,9:19,21,24:29
hi_corr <- hi[, c(1,25,2:7,8,23:24,27,30)]
hi_corr_out <- hi[, c(1,25,8:9,12:13,16:22,26:29)]

library(PerformanceAnalytics)
chart.Correlation(hi_corr)
#chart.Correlation(hi_corr_out)

library(corrplot)
hi_corplot <- cor(hi_corr)
corrplot(hi_corplot, method = "number", number.cex= 0.7)

hi_corplot_out <- cor(hi_corr_out)
corrplot(hi_corplot_out, method = "number", number.cex= 0.7)

#Regression models
names(hi)
m1 <- lm(log(medexpense) ~ blackhisp + illnesses + health + prioritylist, data = hi)
summary(m1)
m2 <- lm(log(medexpense) ~ healthins + age + female + blackhisp + income + illnesses + ssiratio + health + msa + prioritylist, data = hi)
summary(m2)
m3 <- lm(log(medexpense) ~ healthins + age + female + blackhisp + illnesses*age + ssiratio*age + health + msa + prioritylist, data = hi)
summary(m3)

#Stargazer
stargazer(m1, m2, m3, type='text', single.row = TRUE)
stargazer(m1, m2, m3, type='text', ci=TRUE, ci.level=0.95, single.row = TRUE)

#Assumptions tests
#Linearity
par(mfrow = c(2, 2))
plot(m2)
par(mfrow=c(1,1))

hist(m2$fit, col = 'turquoise', main = "Fitted Plot of M2", xlab = 'Fit', ylab = 'Frequency')

#Normality
#Kolmogorov-Smirnov Test
qqnorm(m2$res)
qqline(m2$res, col = 'red')

qqnorm(m3$res)
qqline(m3$res, col = 'red')

ggqqplot(m2$res, ylab = "Medical Expense - Model 2")
ggqqplot(m3$res, ylab = "Medical Expense - Model 3")

n <- rnorm(10000)
hist(n)
ks.test(n, m2$res)
ks.test(n, m3$res)

#Homoscedasticity
#Bartlett's Test
bartlett.test(list(m2$res, m2$fit))
bartlett.test(list(m3$res, m3$fit))   

#Levene's Test (System Froze)
#leveneTest(m2$res, m2$fit, center=mean)
#leveneTest(m3$res, m3$fit, center=mean)

#Breusch-Pagan Test
bptest(m2)
bptest(m3)

#Multicollinearity
vif(m2)
vif(m3)

#Autocorrelation (Independence)
#Durbin-Watson Test
dwtest(m2)
dwtest(m3)
