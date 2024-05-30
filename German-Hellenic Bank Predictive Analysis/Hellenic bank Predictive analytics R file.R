df <- read.csv("D:/masters- digital innovation/business data analytics/final assignment (project)/Section 2 - data/Data2.csv", header =T)
#data cleaning
#removing the unwanted variable - duration
df$duration <- NULL
# removing any null value in dataset 
df <- na.omit(df)
str(df)
#converting columns into factors/numeric
df$job <- as.factor(df$job)
df$marital <- as.factor(df$marital)
df$education <- as.factor(df$education)
df$contact <- as.factor(df$contact)
df$month <- as.factor(df$month)
df$poutcome <- as.factor(df$poutcome)
df$y <- as.factor(df$y)
df$housing <- as.factor(df$housing)
df$loan <- as.factor(df$loan)
df$default <- as.factor(df$default)
df$y <- ifelse(df$y == "yes", 1, 0)
library(sjPlot)
library(ggpubr)
library(verification)
library(pROC)
# spliting the data into training and test set 
library(caret)
library(car)
set.seed(999)
inTrain <-  createDataPartition(df$y, p = 0.8, list = F)
dfTrain <- df[inTrain,]
dfTest <- df[-inTrain,]

#...............logisitic model.....................................

#making model  
# creating a full model  
fit <- glm(y ~ ., data = dfTrain, family = "binomial")
vif(fit)
#glm is generalized linear model
summary(fit)
#reduce the model to variables which are most important for the model and not over inflate the variance explained
#reduce the model- step wise ( doing on thetrain set)
fit_reduce <- step(fit, k = log(nrow(dfTrain)))
summary(fit_reduce)
#check multicolinearity
vif(fit_reduce)
#to check how much better our model is as compare to previous one 
#how much likely this model is going to fit into the model
#to check this use BIC
#final BIC
BF <- exp((22064.69 - 21972.68)/2)
BF
#this model is 9.543719e+19 times more likely to fit the data then full model
#day variable included
BF <- exp((21979.78 - 21972.68)/2)
BF
#include age - this model is 34.8 times more likely to fit the data then full model
#education & age
BF <- exp((21986.45 - 21972.68)/2)
BF
#education & age included - this model is 977.5 times more likely to fit the data then full model
#day,education & age
BF <- exp((21994.19 - 21972.68)/2)
BF#this model is 46863 times more likely to fit the data then full model
#previous, day,education & age
BF <- exp((22001.88 - 21972.68)/2)
BF#this model is 2191288 times more likely to fit the data then full model
#default, previous, day,education & age
BF <- exp((22010.74 - 21972.68)/2)
BF#this model is 1.8e+9 times more likely to fit the data then full model 
#Pseudo R
pseudoR <- 1-fit_reduce$deviance/fit_reduce$null.deviance
pseudoR #=0.165
#taking exp of predictors which captured most of the variance in final model- step wise model 
#exponent of log ODDS
(exp(fit_reduce$coefficients[2]))
1-0.7875026
#martial married- married client has approx 21.2% less chance to subscribe to term deposit then the divorced 
(exp(fit_reduce$coefficients[3]))
#martial single- single client are 8.8% more likely to subscribe to a term deposit then divorced
(exp(fit_reduce$coefficients[4]))
#balance- with every unit increase in balance, the person is 0.002% more likely to open an account
(exp(fit_reduce$coefficients[5]))
1-0.5484235
# housing yes- if the customer has a housing loan, he is approx 45.15% less likely to subscribe for term account
(exp(fit_reduce$coefficients[6]))
1-0.6693557 
# persloan yes-if the person has taken personal loan, he is 33% less likely to subscribe
(exp(fit_reduce$coefficients[7]))
1- 0.8049097 
#contact telephone- if the person contacted on telephone, 19.5% less likely to subscribe as compared to cellular
(exp(fit_reduce$coefficients[8]))
1-0.2485409  
#contact unknown- if he client is communicated through unknown type, he is 75% less likely to subscribe as compared to one with cellular
(exp(fit_reduce$coefficients[9]))
1-0.4223951 
#monthaug- client contacted in august is 57.7% less likely to subscribe then the one contacted in april 
(exp(fit_reduce$coefficients[10]))
#monthdec- person contacted in december is 63% more likely to subscribe as compared tot he one in april
(exp(fit_reduce$coefficients[11]))
1-0.613402
#monthfeb- client contacted in feb is 38.6% less likely to open a term acc then the one in april 
(exp(fit_reduce$coefficients[12]))
1-0.2939791 
#monthjan - a person contacted in jan is 70% less likely to say yes then april 
(exp(fit_reduce$coefficients[13]))
1-0.4714065  
#monthjul- a person contacted in july is 52.8% less likely to say yes then the one in april
(exp(fit_reduce$coefficients[14]))
#monthjune- 13% more likely to subscribe then in april 
(exp(fit_reduce$coefficients[15]))
#monthmarch- approx 216% more likely to open then the one in april
(exp(fit_reduce$coefficients[16]))
#monthmay- approx 40% less likely to subscribe then april one
(exp(fit_reduce$coefficients[17]))
1-0.4070363  
#monthnov- 59% less likely to subscribe as compared to april  
(exp(fit_reduce$coefficients[18]))
#monthoct - 97% more likely to open then april
(exp(fit_reduce$coefficients[19]))
#monthsep- 111% more likely to subscribe then april
(exp(fit_reduce$coefficients[20]))
1-0.9155219  
# with increase in each unit of number of contact performed in campagin, there is 8.4% less likely chance that the client subscribe
(exp(fit_reduce$coefficients[21]))
#poutcomeother- client with outcome other, there is approx 33.6% chance that client will open term acc as compared to the client with previous outcome was a failure 
(exp(fit_reduce$coefficients[22]))
#poutcomesuccess- client with which previous marketing campaign was a success, 911% more likely to open a term acc as compared to ones with which it was a failure.
(exp(fit_reduce$coefficients[23]))
#poutcomeunknown- client with which previous marketing campaign was a unknown, 6% more likely to open a term acc as compared to ones with which it was a failure

# predict value on test data 

prd <- predict(fit_reduce, dfTest, type = "response")

head(prd)
library(pROC)
library(verification)
roc.plot(dfTest$y == 1, prd)
#looking at the roc curve,adding the threshold to multiple models and choosing the desired cutoff
prd1 <- ifelse(prd > 0.1, 1, 0)
print(confusionMatrix(as.factor(dfTest$y), as.factor(prd1))) 
prd2 <- ifelse(prd > 0.15, 1, 0)
print(confusionMatrix(as.factor(dfTest$y), as.factor(prd2)))
prd3 <- ifelse(prd > 0.2, 1, 0)
print(confusionMatrix(as.factor(dfTest$y), as.factor(prd3)))
prd4 <- ifelse(prd > 0.25, 1, 0)
print(confusionMatrix(as.factor(dfTest$y), as.factor(prd4)))
prd5 <- ifelse(prd > 0.3, 1, 0)
print(confusionMatrix(as.factor(dfTest$y), as.factor(prd5)))


#---------------------linear regression----------------------------

#full model
fit1 <- lm(balance ~ ., data = dfTrain)
summary(fit1)#Adjusted R-squared:  0.04923- very less variance is explained, the , p- value < 0.01
plot_model(fit1, type = "pred")
#outcome variable- age
fit2 <- lm(age ~ ., data = dfTrain)
summary(fit2)# adjusted R - squared: 0.42, thats a bit acceptable
plot_model(fit2, type = "pred")
#multicolinearity tells the colineraity between variables
vif(fit2)
#qualitative method- diagnostic plots 
plot(fit2)# looking at the plots, there are lot of parametric violations so, presenting the result with caution
#reduce the model to variables which are most important for the model and not over inflate the variance explained
fit_reduce2 <- step(fit2, k = log(nrow(dfTrain)))
summary(fit_reduce2)# Adjusted R-squared:  0.4201- model explains 42% variance, p<0.05
vif(fit_reduce2) # multicolinearity <5 , for all variables now
#ncv test- quantitative method
ncvTest(fit2)# p < 0.05, Chisquare = 429.4993

#to check how much better our model is as compare to previous one 
#how much likely this model is going to fit into the model
#to check this use BIC
BF <- exp((151614.1 - 151565.5)/2)
BF #this model is 3.5756e+11 more likely to fit the data then full model
#include loan& default
BF <- exp((151567.4 - 151565.5)/2)
BF #this model is 2.58 times more likely to fit the data then full model
# day, loan & default
BF <- exp((151574.1- 151565.5)/2)
BF #this model is 73.6 times more likely to fit the data then full model
# pdays,day, loan & default
BF <- exp((151583- 151565.5)/2)
BF# this model is 6310 times more likely to fit the data then full model
# campaign, pdays,day, loan & default
BF <- exp((151593.2- 151565.5)/2)
BF #this model is 1035091 times more likely to fit the data then full model
# previous, campaign, pdays,day, loan & default
BF <- exp((151603.6- 151565.5)/2)
BF #this model is 1.8 e^9 times more likely to fit the data then full model


predtest <- predict(fit_reduce2, dfTest)
cor(predtest,dfTest$age)^2
#adjusted r: 0.41 - 41.2% of the variance in age variable was explained by 9 variable.
dfTest$predtest <- predtest
fitplot1 <- lm(age ~ predtest,data = dfTest)
plot_model(fitplot1, type = "pred")

write.csv(df,file = "predictive.csv")
predictive_csv <- read.csv(file = "predictive.csv")
View(predictive_csv)
