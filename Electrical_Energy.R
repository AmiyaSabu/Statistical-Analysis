install.packages("datarium")
install.packages("qqplotr")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("moments")
install.packages("caret")
install.packages("TTR")
install.packages("forecast")

library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(corrplot)
library(tidyverse)
library(moments)
library(caret)
library(TTR)
library(forecast)

#comprehensive descriptive statistical analysis

Electricity_Access <- read.csv("Electricity_Access.csv", header = TRUE)
Electricity_Access_Full <- read.csv("Sustainable_Energy_for_All.csv", header = TRUE)

#explore the dataset:
names(Electricity_Access)
head(Electricity_Access)
tail(Electricity_Access)
str(Electricity_Access)
summary(Electricity_Access)

mean(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit)
median(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit)
sd(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit)

newElectricity_Access_Full<-Electricity_Access_Full %>% select(-Total.Access.to.Clean.Fuels.and.Technologies.for.cooking..,-Country.Name,-Time)

head(newElectricity_Access_Full,5)

#mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(newElectricity_Access_Full, uniqv)))]
}

result<-getmode(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit)
result


Electricity_2009<-Electricity_Access[Electricity_Access$Time==2009,]
Electricity_2009

Electricity_Maldives<-Electricity_Access_Full[Electricity_Access_Full$Country.Name=="Maldives",]
Electricity_Maldives

barplot(Electricity_2009$Total.electricity.Produced.unit)
barplot(Electricity_2009$Total.electricity.Produced.unit,names.arg = Electricity_2009$Country.Name)

hist(Electricity_Maldives$Total.electricity.Produced.unit)
hist(Electricity_Maldives$Total.electricity.Produced.unit,breaks = 16)

pie(Electricity_2009$Total.electricity.Produced.unit)
pie(Electricity_2009$Total.electricity.Produced.unit, labels = Electricity_2009$Country.Name,)

percent <- round(100*Electricity_2009$Total.electricity.Produced.unit/sum(Electricity_2009$Total.electricity.Produced.unit), 1)
percent <- paste(Electricity_2009$Country.Name, "-",percent,"%") # add percents to labels
pie(Electricity_2009$Total.electricity.Produced.unit, labels = percent,)

###########Skewness and Kurtosis

skewness(Electricity_Access)

kurtosis(Electricity_Access)

##############~~~~~~~~~~~~~~~~~~~correlation analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~##################

head(Electricity_Access_Full,5) #displays first 5 observations
#removing Total.Access.to.Clean.Fuels.and.Technologies.for.cooking variable
newElectricity_Access_Full<-Electricity_Access_Full %>% select(-Total.Access.to.Clean.Fuels.and.Technologies.for.cooking..,-Country.Name,-Time)

head(newElectricity_Access_Full,5)

#correlation coefficient between 2 variables

#pearson method
cor(newElectricity_Access_Full$Total.Renewable.electricity.Produced.unit, newElectricity_Access_Full$Total.electricity.consumption.unit)
#correlation between x and y is equal to y and x

#spearman method
cor(newElectricity_Access_Full$Total.Renewable.electricity.Produced.unit, newElectricity_Access_Full$Total.electricity.consumption.unit, method = "spearman")

#correlation matrix
# correlation for all variables and rounded to 2 decimals
round(cor(newElectricity_Access_Full), digits = 2)

corrplot(cor(newElectricity_Access_Full), method = "number", type = "upper")
 
mean(newElectricity_Access_Full$Total.Renewable.electricity.Produced.unit)
median(newElectricity_Access_Full$Total.Renewable.electricity.Produced.unit)
sd(newElectricity_Access_Full$Total.Renewable.electricity.Produced.unit)

######################~~~~~~~~~~Regression~~~~~~~~~~~~#############################

##objective of the regression analysis

head(as.data.frame(Electricity_Access_Full))
str(Electricity_Access_Full) #check the variables and data types for each variable

#extracted Numerical variables from the data set
head(newElectricity_Access_Full,5)
#correlation matrix
cor(newElectricity_Access_Full)
##correlation matrix
corrplot(cor(newElectricity_Access_Full))

#linear regression analysis
model_1<-lm(Total.electricity.consumption.unit~Total.Renewable.electricity.Produced.unit, newElectricity_Access_Full)
summary.lm(model_1)

#fitted regression
plot(Total.electricity.consumption.unit ~ Total.Renewable.electricity.Produced.unit, newElectricity_Access_Full,
     col = "blue",
     main = "Regression: Total Electricity Consumption and Total Renewable Electricity Produced",
     xlab = "Totoal Electricity Consumption",
     ylab = "Total Renewable Elecricity Production")

#adding the regression line
abline(model_1, col="red")

#residuals versus fits
plot(model_1, 1)

#Normality of residuals
plot(model_1, 2)

#Equal variances of the residuals (Homoscedasticity)
plot(model_1, 3)

########################--------------Time Series---------------------###################

TSRenewableProduction<-ts(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit,start = min(Electricity_Access_Full$Time),end = max(Electricity_Access_Full$Time),frequency = 1)
plot(TSRenewableProduction)

#####################~~~~~~~~~~Hypothesis Testing~~~~~~~~~~~~~######################

#######One Sample T-Test

summary(Electricity_Access_Full)
dim(Electricity_Access_Full)

hist(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit)

mean(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit)
sd(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit)

##one-tailed hypothesis test
t.test(Electricity_Access_Full$Total.Renewable.electricity.Produced.unit, mu=106259, alternative = "less")