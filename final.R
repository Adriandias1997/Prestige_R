#importing
Prestige=read.csv("E:\\Assignments\\BA\\Prestige.csv", head= TRUE) 
Prestige
install.packages("nortest")
library("nortest")

install.packages("gplots")
library("gplots")

#--------------------Task B------------------------------

#-----------------Prestige----------------------
#getting the prestige column
Prestige$prestige

#sorting the prestige column
sortedprestige=sort(Prestige$prestige)
sortedprestige

#Finding the mean Prestige
prestigemean=mean(sortedprestige)
prestigemean

#Finding Prestige Median
prestigeMedian=median(sortedprestige)
prestigeMedian

#find the sd of Prestige
prestigesd=sd(sortedprestige)
prestigesd

#Visually Normality tests
#boxplot for prestige
boxplot(sortedprestige, main="Boxplot for prestige",ylab="prestige")

#bell curve for Prestige
dnorm(sortedprestige,prestigemean,prestigesd)
plot(sortedprestige,dnorm(sortedprestige,prestigemean,prestigesd),xlab="Prestige",main="Bell Curve for prestige",type = "l")

#Histogram for Prestige
hist(sortedprestige,main="Histogram  prestige",xlab="prestige")

#Kernel Density for Prestige
plot(density(sortedprestige), main="Kernel Density of prestige", xlab="prestige")

#qq plot for Prestige
qqnorm(sortedprestige)
qqline(sortedprestige)

#Monte Carlo Graphical 
plot(sortedprestige,dnorm(sortedprestige,prestigemean,prestigesd), xlab="prestige")
plot(sortedprestige,dnorm(sortedprestige,prestigemean,prestigesd),type = "l" ,xlab="prestige")

#Anderson Darling Test for prestige
ad.test(sortedprestige)

install.packages("nortest")
library("nortest")

#Anderson Darling Test for prestige
ad.test(sortedprestige)

#Shapiro Test for prestige
shapiro.test(sortedprestige)

#Lilliefors Test for prestige
lillie.test(sortedprestige)

#----------------education---------------------
#getting the education
Prestige$education

#sorting the education
sortededucation=sort(Prestige$education)
sortededucation

#Finding the mean education
educationMean=mean(sortededucation)
educationMean

#Finding education Median
educationMedian=median(sortededucation)
educationMedian

#find the sd of education
educationsd=sd(sortededucation)
educationsd

#boxplot for education
boxplot(sortededucation, main="Boxplot for education", ylab="education")

#bell curve for education
dnorm(sortededucation,educationMean,educationsd)
plot(sortededucation,dnorm(sortededucation,educationMean,educationsd),type = "l",
     main="education",xlab="education")

#Histogram for education
hist(sortededucation,main="Histogram for education",xlab="education")

#Kernel Density for education
plot(density(sortededucation), main="Kernel Density of education")

#Normality tests
#Anderson Darling Test for education
ad.test(sortededucation)

#Shapiro Test for education
shapiro.test(sortededucation)

#Lilliefors Test for education
lillie.test(sortededucation)

#qq plot for education
qqnorm(sortededucation)
qqline(sortededucation)

#Monte Carlo Graphical 
plot(sortededucation,dnorm(sortededucation,educationMean,educationsd),xlab="education")
plot(sortededucation,dnorm(sortededucation,educationMean,educationsd)
     ,type = "l",xlab="education")

#-------------income---------
#Getting the income
Prestige$income

#sorting the income
sortedincome=sort(Prestige$income)
sortedincome

#Finding the mean income
incomeMean=mean(sortedincome)
incomeMean

#Finding income Median
incomeMedian=median(sortedincome)
incomeMedian

#find the sd of income
incomesd=sd(sortedincome)
incomesd

#boxplot for income
boxplot(sortedincome,main="Boxplot for income", ylab="income")

#bell curve for income
dnorm(sortedincome,incomeMean,incomesd)
plot(sortedincome,dnorm(sortedincome,incomeMean,incomesd)
     ,type = "l",xlab="income")

#Histogram for income
hist(sortedincome,main="Histogram for income",xlab="income")

#Kernel Density for income
plot(density(sortedincome), main="Kernel Density of income")

#Normality tests
#Anderson Darling Test for income
ad.test(sortedincome)

#Shapiro Test for income
shapiro.test(sortedincome)

#Lilliefors Test for income
lillie.test(sortedincome)

#qq plot for income
qqnorm(sortedincome)
qqline(sortedincome)

#Monte Carlo Graphical
plot(sortedincome,dnorm(sortedincome,incomeMean,incomesd),xlab="income")
plot(sortedincome,dnorm(sortedincome,incomeMean,incomesd),
     type = "l",xlab="income")

#---------------------------------TASK C------------------------------------
#Box Plot for Prestige and Occupation type
boxplot(Prestige$prestige~Prestige$type,xlab = "Occupation Type", ylab = "Prestige",
        main="Box Plot for Prestige and Occupation type")

# Mean Plot for Prestige and Occupation type
plotmeans(Prestige$prestige~Prestige$type,xlab ="occupation type", ylab="prestige", 
          main=" Mean Plot for Prestige and Occupation type")


# Compute the analysis of variance Prestige and Occupation TYPE
res.aov <- aov(Prestige$prestige~Prestige$type)
# Summary of the analysis
summary(res.aov)


install.packages("gplots")
library("gplots")

#Homogeneity of variances
plot(res.aov, 1)

install.packages("titanic")
library(titanic)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library("tidyr")

install.packages("gridExtra")
library("gridExtra")

#Bartlett Test of Prestige and Occupation
bartlett.test(Prestige$prestige~Prestige$type)

#oneway test Prestige and Occupation type
oneway.test(Prestige$prestige~Prestige$type)

#Normality
plot(res.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


#-----------------TASK D------------------------------------------
#Scatter Plot
#education in comparission to Prestige
plot(Prestige$education,Prestige$prestige,
     xlab="education",ylab="prestige",main="Scatter Plot of prestige*education")

#Correlation Test for education and prestige
cor.test(Prestige$education,Prestige$prestige)

#income in comparission to Prestige
plot(Prestige$income,Prestige$prestige,     
     xlab="income",ylab="prestige",main="Scatter Plot of prestige*income")

#Correlation Test for income and prestige
cor.test(Prestige$income,Prestige$prestige)

#-------------------TASK E---------------------------------------------

#prestige and income
regressionModel<-lm(Prestige$income~Prestige$prestige)
regressionModel

summary(regressionModel)

#prestige and education
regressionModel<-lm(Prestige$education~Prestige$prestige)
regressionModel

summary(regressionModel)

