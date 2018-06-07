#Preliminary report 
#Abhishek Kumar.

#few assumptions for Logistic Regression:
#  no multicollinearity among the independent variables
#  observations to be independent of each other
#  dependent variable to be binary and ordinal 
#  logistic regression requires the dependent variable to be ordinal.
#  linearity of independent variables and log odds
#  requires a large sample size.  A general guideline is that you need 
#at minimum of 10 cases with the least frequent outcome for each 
#independent variable in your model. For example, if you have 5 
#independent variables and the expected probability of your least 
#frequent outcome is .10, then you would need a minimum sample size of 
#500 (10*5 / .10).

#setting the directory location
setwd("G://analytics//ISB CBA//Residency//Residency3&4//B9MT2SA2//Mini Project  Data sets-20180206//HR Analytics")

#loading required library
library(car)
library(tidyverse)
library(ggplot2)



#Reading the Hr file.
hrdata = read.csv("HR.csv",header = TRUE,stringsAsFactors = FALSE,na.strings = c("NA","N/A"," "))
#View(hrdata)
unique(hrdata$sales)
str(hrdata)
class(hrdata)
summary(hrdata)
#time_spend_company has value 10 hours which we will check if it is outlier or not.
#WE can also check if we can build another model considering who will get the 
#promotion in five years now
#we will do further investigation on sales and salary by converting 
# them into dummy variables

#checking for missing value if any present in the data
colSums(is.na(hrdata))
sum(is.na(hrdata)) # no missing value in the data

nrow(hrdata)

#seperating the quantitative and qualitative data

hrquant = c("satisfaction_level","last_evaluation","number_project"
            ,"average_montly_hours","time_spend_company")
hrquality = c("Work_accident"
              ,"left","promotion_last_5years","sales","salary")

#only quantitative data
hrdataquant =hrdata[,-c(6:10)]
#View(hrdataquant)

#Qualitative analysis


##Univariate analysis on continous data
hrdata %>% gather(satisfaction_level:time_spend_company, key = "variable", value = "value") %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~ variable, scales = 'free_x')

hrdata %>% gather(satisfaction_level:time_spend_company, key = "variable", value = "value") %>%
  ggplot(aes(x=variable,y = value)) + 
  geom_boxplot() + facet_wrap(~ variable, scales = 'free_y')
#time_spend_company is right skewed with outliers.

boxplot(hrdata$time_spend_company, main="time_spend_company", col ="green")
boxplot(sqrt(hrdata$time_spend_company), main="sqrt time_spend_company", col ="blue")
boxplot(log(hrdata$time_spend_company), main="log time_spend_company", col ="red")
#we will see later on if we require the transformed data to build better
# logistic models, as of now going with the original data.

#individual histogram
for(i in 1:ncol(hrdataquant))
{ if(!(is.factor(hrdataquant[,i])))
{
  print(ggplot(hrdataquant,aes(hrdataquant[,i]))+geom_histogram()+labs(x=colnames(hrdataquant)[i]))
  
}
}


#in one sheet to give better presentation of data
boxplot(hrdataquant)
## Qualitative variables 
# table shows the frequency of categories in the dependent variable
table(hrdata$left)
table(hrdata$promotion_last_5years)
table(hrdata$salary)
table(hrdata$sales)
table(hrdata$Work_accident)

hrdata %>% gather(Work_accident:salary, key = "variable", value = "value") %>% ggplot(aes(x = value)) +
  geom_bar()+ facet_wrap(~variable, scales = 'free_x')

for(i in 1:ncol(hrdataquant))
{ if(!(is.factor(hrdataquant[,i])))
{
print(qplot(x=hrdataquant[,i],data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~left)+labs(x="Left",y=colnames(hrdataquant)[i]))
}
}

for(i in 1:ncol(hrdataquant))
{ if(!(is.factor(hrdataquant[,i])))
{
  print(qplot(x=hrdataquant[,i],data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$Work_accident)+labs(x="Work Accident",y=colnames(hrdataquant)[i]))
}
}

for(i in 1:ncol(hrdataquant))
{ if(!(is.factor(hrdataquant[,i])))
{
  print(qplot(x=hrdataquant[,i],data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$promotion_last_5years)+labs(x="promotion_last_5years",y=colnames(hrdataquant)[i]))
}
}

for(i in 1:ncol(hrdataquant))
{ if(!(is.factor(hrdataquant[,i])))
{
  print(qplot(x=hrdataquant[,i],data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$sales)+labs(x="sales",y=colnames(hrdataquant)[i]))
}
}
#below graph shows that there are people leaving the orgranisation have majority of 
#people having satisfaction level above 50%...which is strange.
qplot(x=hrdata$sales,data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$left)+labs(x="sales",y="Left Count")
qplot(x=hrdata$sales,data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$Work_accident)+labs(x="sales",y="Work_accident Count")
qplot(x=hrdata$sales,data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$promotion_last_5years)+labs(x="sales",y="promotion_last_5years Count")
qplot(x=hrdata$sales,data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$salary)+labs(x="sales",y="salary Count")
qplot(x=hrdata$last_evaluation,data=hrdata,col = I("blue"),fill = I("red")) + facet_wrap(~hrdata$left)

for(i in 1:ncol(hrdataquant))
{ if(!(is.factor(hrdataquant[,i])))
{
  print(ggplot(hrdata,aes(x = sales , y =hrdataquant[,i]))+geom_bar(stat="identity")+labs(x="Sales",y=colnames(hrdataquant)[i]))
  
}
}
#more number of people are from sales, support and technical department
#employees from sales, support and technical department have recieved the last evaluations
#major contribution of project is from sales, support and technical departement
#avg monthy hours and time spend in the company are also more for these three departments
#accident in work is also more in these department.
#and they are one who leaves the company more, sales being the highest.
#sales, management and marketing are the ones who have contributes in getting
#promotion in last 5 years

for(i in 1:ncol(hrdataquant))
{ if(!(is.factor(hrdataquant[,i])))
{
  print(ggplot(hrdata,aes(x = salary , y =hrdataquant[,i]))+geom_bar(stat="identity")+labs(x="Salary",y=colnames(hrdataquant)[i]))
  
}
}

#clear
#boxplot(formula=retail_raw[,i]~retail_raw$Sale.Made,data=retail_raw, ylab=colnames(retail_raw)[i])+labs(caption="ye")

plot(hrdata[,hrquant])
#cor(hrdata[,hrquant])
#there is no need to check the collinearlity between x and y
library(car)

#scatterplot.matrix(hrdataquant) #taking too much time
#library(corrgram)
#library(corrplot)
#corrgram(hrdataquant)
#corrplot(cor(hrdataquant),method="number")
#correlation does show some negative relation between left and word_accident,also
#some positive relation between left and time_spend, though the relations are not
#strong enough to comment as of now. we will dive deep going forward.

ggplot(data = hrdata, aes(x = hrdata$left, fill = hrdata$sales)) + 
  geom_bar()

# ggplot(data = TTM, aes(x = Type.of.Behavior, y = Sample.Size, fill = Stage.of.Change)) + 
#   geom_bar() + coord_flip()
# 
# df %>% 
#   gather(variable, value, F1:F3) %>% 
#   ggplot(aes(x = Rank, y = value, fill = variable)) + 
#   geom_bar(stat = "identity")


# Base on above analysis we will build logistic model to see which employee will
# leave the organization.

#check if response variable is of factor type or not
class(hrdata$left)
#it is integer, lets convert it into factor.

#########################################################################
##########################Model Building#################################

#Start building the basic naive model for the benchmark.
set.seed(777)
# we will subset based on categories - after basic model
rownumbers <- sample(1:nrow(hrdata),0.75*nrow(hrdata))
hrtrain <- hrdata[rownumbers,]
hrtest <- hrdata[-rownumbers,]

nrow(hrtrain)
table(hrtrain$left)
nrow(hrtest)
table(hrtest$left)
#will calculate percentage ratio of 0 and 1

glmmodel <- glm(formula = left~.,family = "binomial",data = hrtrain)
summary(glmmodel)
#Some of the department are not significant as per the GLM model
#Deviance is a measure of goodness of fit of a generalized linear model
#Or rather, it's a measure of badness of fit-higher numbers indicate worse fit.
#he null deviance shows how well the response variable is 
#predicted by a model that includes only the intercept (grand mean)
#There is not a good gap between Null Deviance and Residual Deviance,
# thus this is not a good fit, though we will do validation further.


#Checking the multicollinearity in the data
library(usdm)
vifstep(hrtrain[,-c(9,10)],th=2)
#there is no issue of multicollinearty in the data.


#lets see if we get something better from AIC
library(MASS)
glmaicmodel <- stepAIC(glmmodel,direction = "both")
glmaicmodel$anova
#It gives the same model as earlier model- no change
summary(glmmodel)
# The logistic regression coefficients give the change in the log odds of the 
# outcome for a unit increase in the predictor variable.	
# The indicator variables for Sales Departments have a slightly different interpretation. 
# For example, having Employee from HR Department versus the Employee from 
# Accounting(baseline) Department , changes the log odds of leaving the company 
# by 0.343, i.e changes the odds of leaving the company by 1.41 , so the 
# probability of HR employee leaving the company is 58.5%  as compared to Accounting employee										
##Note that while R produces it, the odds ratio for the intercept is not generally interpreted.


#calculate the odds ratios for each preditor.
exp((glmmodel$coefficients))
perunitpercentchange<-100/(1+exp(-glmmodel$coefficients))
perunitpercentchange
#satisfaction_level - there will be decrease of 1.41% of the employee
# leaving the company with every unit increase of satisfaction level.
#last_evaluation - Employee leaving the company by 68.74% with every 
# unit of increase in last_evaluation.

## odds ratios and 95% CI
exp(cbind(OR=glmmodel$coefficients,confint(glmmodel)))
# We can use the confint function to obtain confidence intervals for the 
# coefficient estimates. Note that for logistic models, confidence intervals 
# are based on the profiled log-likelihood function. We can also get CIs based 
# on just the standard errors by using the default method.

## CIs using profiled log-likelihood
confint(glmmodel)

## CIs using standard errors
confint.default(glmmodel)


#model2 <- train(left~.,data = hrtrain,method = "glm",family="binomial")
#model2$finalModel
#model1$coefficients
# Prediction/Evaluation of the glmmodel

##1. Accuracy of the glmmodel
glmpredict <- predict(object = glmmodel,newdata = hrtest, type = "response")
glmclass <- ifelse(glmpredict>0.5,1,0)
table(glmclass,hrtest$left)
cat("Accuracy of glm model with 0.5 as cutoff")
sum(diag(table(hrtest$left,glmclass)))/nrow(hrtest)
# cat("Miss classification Error")
# misClassError(hrtest$left, glmpredict)
# #quite high misclassificaiton

library(InformationValue)
optCutOff<- optimalCutoff(hrtest$left, glmpredict)[1]
optCutOff


glmclass <- ifelse(glmpredict>optCutOff,1,0)
table(glmclass,hrtest$left)
cat("Accuracy of glm model with optCutOff as cutoff")
sum(diag(table(hrtest$left,glmclass)))/nrow(hrtest)
cat("Miss classification Error with optimal cutoff")
misClassError(hrtest$left, glmpredict, threshold = optCutOff)
#quite high misclassificaiton

library(caret)
confusionMatrix(table(glmclass,hrtest$left,dnn=list("Predicted","Actual")))
#So the accuracy of our glm model is 80.4%, as we have calculated from
# different method above.

##2. Goodness of Fit - Hosmer Lemeshow Test


library(ResourceSelection)
hoslem.test(hrtrain$left,fitted(glmmodel),g=10)
#since p-value is less than 0.05, so we reject the null hypothesis, i.e 
# the current model is not a good fit as shown by Hosmer & Lemeshow test.

##3. evaluate the statistical significance of each coffecient in the model
# - Wald test
# h0: - coffecients are zeros/insignificant
# h1 :- coffecients of the independent variabe in the model is significantly
# different from 0.
#if the test fails to reject the null  hypothesis ,this suggests that
# removing the variable from the model  will not  substantially harm the 
# fit of the model.


library(survey)
#regTermTest(glmmodel,"satisfaction_level")

for (i in c(1:ncol(hrtrain[,-7]))){
  if (i==7){
    #cat("Significance of Response Variable is not required")
  }else{
  print (regTermTest(glmmodel,colnames(hrtrain)[i]))
  }
}
#All the predictors in the model are significant as per the wald test.
#As P-value is less than 0.05 thus we reject null hypothesis, i.e removing
# any predictor from the model will harm the fit of the model.


## we will use aod package to perform wald test to check overall effect of sales
## and salary predictors..
#install.packages("aod")
library(aod)
wald.test(b=coef(glmmodel),Sigma = vcov(glmmodel), Terms = 9:17)
# The chi-squared test statistic of 33.6, with 9 degrees of freedom is 
# associated with a p-value of 0.00011 indicating that the overall effect of 
# Sales is statistically significant.

wald.test(b=coef(glmmodel),Sigma = vcov(glmmodel), Terms = 18:19)
# The chi-squared test statistic of 226.6, with 2 degrees of freedom is 
# associated with a p-value of 0.0 indicating that the overall effect of 
# Salary is statistically significant.

## we can also test the difference in the coefficient for the different level
## of Sales, like HR and IT

hrit <- cbind(0,0,0,0,0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0) # deifnes the vector for which we want to test.
# To contrast these two terms, we multiply one of them by 1, and the other by 
# -1. The other terms in the model are not involved in the test, so they are 
# multiplied by 0. The second line of code below uses L=hrit to tell R that we 
# wish to base the test on the vector hrit (rather than using the Terms option as 
# we did above).
wald.test(b=coef(glmmodel),Sigma = vcov(glmmodel),L=hrit)
# The chi-squared test statistic of 8.7 with 1 degree of freedom is associated 
# with a p-value of 0.0031, indicating that the difference between the coefficient 
# for Sales=HR and the coefficient for Sales=IT is statistically significant.
#This way we can test the difference between anyother coefficients.


##4. MC Fadden's R2 Test.
#unlike linear regression, there is no R2 statistic which
# explains the proportion of variation in the dependent
# variable that is explained by the predictors.
# for this we use Mc Fadden's R2

#install.packages("pscl")
library(pscl)
pR2(glmmodel)
#McFadden = .2196 
#The predictors in our model explain just the 21.96% 
# variation in the data.
# This suggests that one or more variables are missing 
# in our model

#5. ROC
#install.packages("InformationValue")
library(InformationValue)
plotROC(actuals = hrtrain$left,predictedScores = as.numeric(fitted(glmmodel)))
#since the area under curve is 82.06, we can say  the correctly identification
# ability of our model is good.

#6. Kolmogrov Smirnov Chart
#measure the performance of classification models.
# It is a measure of degree of seperation between left(1) and not-left(0)
# Distance between left and not-left should be as large as possible
#The random line in the chart corresponds to the case  of capturing the responders(1)
# by random selection,i.e  when you dont have any model at disposal.
#The model line represent the case of capturing the responders if you go by the model
# generated probability scores where you begin by targeting datapoint with 
# highest probability scores.

ks_plot(actuals = hrtrain$left,predictedScores = as.numeric(fitted(glmmodel)))
# as the measure between left and not-left is not so large, we can say that
# performance of the model is not good.
# Kolmogorv Smirnov statistic is the maximum  difference between the cumulative 
# true positive and cumulative false positive rate
# the hihger the value of kolmogrov statistic,  the more efficient is the 
# model at capturing the responders(1).
ks_stat(actuals = hrtrain$left,predictedScores = as.numeric(fitted(glmmodel)))
#As the kolmogrov Smirnov statistic is .5033 which is small, we can say 
# that the model is not that efficient in capturing  the responders(1).

#7. F1 score - accuracy test.
hrtr <- hrtrain
hrtr$fitted_label <- glmmodel$fitted.values
#hrtrain$fitted_label <- glmmodel$fitted.values
hrtr$fitted_label <- ifelse(hrtr$fitted_label>optCutOff,1,0)
optCutOff

tp = sum((hrtr$left==1)&(hrtr$fitted_label==1))
tp
fp = sum((hrtr$left==0)&(hrtr$fitted_label==1))
fp
prec = tp/(tp+fp)
cat("The precision is: ")
prec
#tp = sum((hrtrain1$left==1)&(hrtrain1$fitted_label==1))
fn = sum((hrtr$left==1)&(hrtr$fitted_label==0))
rec = tp/(tp+fn)
cat("The recall is: ")
rec

F1 = 2*(prec*rec)/(prec+rec)
cat("The F1 score is: ")
F1
#F1 score (also F-score or F-measure) is a measure of a test's accuracy. 
# It considers both the precision p and the recall r of the test to compute 
# the score: p is the number of correct positive results divided by the number 
# of all positive results returned by the classifier, and r is the number of 
# correct positive results divided by the number of all relevant samples 
# (all samples that should have been identified as positive). The F1 score 
# is the harmonic average of the precision and recall, where an F1 score 
# reaches its best value at 1 (perfect precision and recall) and worst at 0.
#This also suggest the test's accuracy of the model is not good.

#########################################################################
###########################2. Decision Tree##############################
#Decision Tree using tree()
#install.packages("tree")
library(tree)
hrtrain$salary <- as.factor(hrtrain$salary)
hrtrain$sales <- as.factor(hrtrain$sales)
hrtrain$left <- as.factor(hrtrain$left)
hrtest$left <- as.factor(hrtest$left)
hrtest$salary <- as.factor(hrtest$salary)
hrtest$sales <- as.factor(hrtest$sales)
#Build tree on train data

DT_Model = tree(left~.,data=hrtrain)
#summary(DT_Model)
#predict on test data
tree.pred = predict(DT_Model,newdata=hrtest,type="class")

#plot the tree to check how the tree model is

plot(DT_Model)
text(DT_Model,pretty=1)

#confusion matrix

table(tree.pred,hrtest$left)

sum(diag(table(hrtest$left ,tree.pred)))/nrow(hrtest)

#prune a tree using cross validation
set.seed(1234)
cv_dt_model=cv.tree(DT_Model,FUN=prune.misclass)

plot(cv_dt_model$size,cv_dt_model$dev,type="b")
DT_Pruned=prune.misclass(DT_Model,best=8)
plot(DT_Pruned)
text(DT_Pruned,pretty = 8)

#rpart
library(rpart)

library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)
rpartmodel=rpart(formula=left~.,data=hrtrain,method="class",
                 control = rpart.control(cp = 0.0))
#summary(rpartmodel)
windows()
fancyRpartPlot(model=rpartmodel,main="Hr model",cex=0.6)

printcp(rpartmodel)
plotcp(rpartmodel)

rpartmodel_test=predict(object=rpartmodel,newdata=hrtest,type="class")
#mean(rpartmodel_test==hrtest$left)
#confusion matrix

table(hrtest$left ,rpartmodel_test)
#model accuracy on test data
cat("Accuracy of the Decision Tree model is:")
baseaccuracy<-sum(diag(table(hrtest$left,rpartmodel_test)))/nrow(hrtest)
baseaccuracy
##Now we will prune tree to stop the tree at optimum
##Pre-prune the tree
rpartmodelpre=rpart(formula=left~.,data=hrtrain,method="class",
                 control = rpart.control(cp = 0.0,maxdepth = 9,minsplit = 100,minbucket = 20))
#summary(rpartmodelpre)
windows()
fancyRpartPlot(model=rpartmodelpre,main="Pre Prune Hr model",cex=0.6)

printcp(rpartmodelpre)
plotcp(rpartmodelpre)

rpartmodelpre_test=predict(object=rpartmodelpre,newdata=hrtest,type="class")
#mean(rpartmodel_test==hrtest$left)
#confusion matrix

table(hrtest$left ,rpartmodelpre_test)
#model accuracy on test data
cat("Accuracy of the Pre Prune Decision Tree model is:")
preaccuracy<-sum(diag(table(hrtest$left,rpartmodelpre_test)))/nrow(hrtest)
preaccuracy
##Post-Prune the tree
rpartmodelpost <- prune(rpartmodel,cp = 0.0045 )
rpartmodelpost_test=predict(object=rpartmodelpost,newdata=hrtest,type="class")
cat("Accuracy of the Post Prune Decision Tree model is:")
postaccuracy<-sum(diag(table(hrtest$left,rpartmodelpost_test)))/nrow(hrtest)
postaccuracy
data.frame(baseaccuracy,preaccuracy,postaccuracy)
#so the best model based on decision tree is the pre-pruned Model.

##1. Accuracy of the rpartmodelpre
cat("Accuracy of the Pre Prune Decision Tree model is:")
preaccuracy<-sum(diag(table(hrtest$left,rpartmodelpre_test)))/nrow(hrtest)
preaccuracy

#So the accuracy of our Pre-Prune Decision Tree is 97.44%, as we have calculated from
# different method above.

#2. ROC
predicthr <- predict(object=rpartmodelpre,newdata=hrtrain,type="class")
plotROC(actuals = hrtrain$left,predictedScores = as.numeric(predicthr))
#since the area under curve is 94.88, we can say  the discrimination  ability
# of our model is good.

#3. Kolmogrov Smirnov Chart
#measure the performance of classification models.

ks_plot(actuals = hrtrain$left,predictedScores = as.numeric(predicthr))
# as the measure between left and not-left is large, we can say that
# performance of the model is good.
ks_stat(actuals = hrtrain$left,predictedScores = as.numeric(predicthr))
#As the kolmogrov Smirnov statistic is 0.8426 which is good, we can say 
# that the model is efficient in capturing  the responders(1).

#########################################################################
##########################Random Forest##################################

library(randomForest)

#Random Forest Model 1: default settings
set.seed(1234)

rfmodel1=randomForest(left~.,data=hrtrain)

  #importance of each variable
#the more the decrease in GIni, the more the importance 
#of the independednt variable in the prediction the dependent varioable
varImpPlot(rfmodel1)

rf_predict=predict(object=rfmodel1,newdata=hrtest,type="response")

table(hrtest$left,rf_predict)

rfbaseaccuracy <-sum(diag(table(hrtest$left,rf_predict)))/nrow(hrtest) 
cat("Accuracy of the model is" )
rfbaseaccuracy
#remove some insignificant variables and build random forest model again

set.seed(1000)

rfmodel2=randomForest(formula=left~satisfaction_level+number_project
                      +time_spend_company+average_montly_hours+last_evaluation
                      ,data=hrtrain)
varImpPlot(rfmodel2)
rfpredict2=predict(object=rfmodel2,newdata=hrtest,type="response")

table(hrtest$left,rfpredict2)
sum(diag(table(hrtest$left,rfpredict2)))/nrow(hrtest) #99.09%

#2. ROC
predicttrainrf <- predict(object=rfmodel2,newdata=hrtrain,type="class")
plotROC(actuals = hrtrain$left,predictedScores = as.numeric(predicttrainrf))
#since the area under curve is 99.87, we can say  the discrimination  ability
# of our model is good.

#3. Kolmogrov Smirnov Chart
#measure the performance of classification models.

ks_plot(actuals = hrtrain$left,predictedScores = as.numeric(predicttrainrf))
# as the measure between left and not-left is large, we can say that
# performance of the model is good.
ks_stat(actuals = hrtrain$left,predictedScores = as.numeric(predicttrainrf))
#As the kolmogrov Smirnov statistic is 0.9134 which is good, we can say 
# that the model is efficient in capturing  the responders(1).

#so the best algo which provides the correctd prediction is from random forest which is
#rfmodel2 with few predictors.


###############################################################################
############UnRegularized Logistic Regression with optimization################
###############################################################################

#create dummy variable for categorical variables

hrtrain1 <- hrtrain
hrtest1 <- hrtest
hrtrain1$left <- ifelse(hrtrain1$left==1,0,1)
hrtest1$left <- ifelse(hrtest1$left==1,0,1)
# table(hrtrain$left)
# table(hrtrain1$left)
# table(hrtest1$left)
for(level in unique(hrtrain1$sales)){
  hrtrain1[paste("dummy", level, sep = "_")] <- ifelse(hrtrain1$sales == level, 1, 0)
}

for(level in unique(hrtrain1$salary)){
  hrtrain1[paste("dummy", level, sep = "_")] <- ifelse(hrtrain1$salary == level, 1, 0)
}
#View(hrtrain1)

for(level in unique(hrtest1$sales)){
  hrtest1[paste("dummy", level, sep = "_")] <- ifelse(hrtest1$sales == level, 1, 0)
}

for(level in unique(hrtest1$salary)){
  hrtest1[paste("dummy", level, sep = "_")] <- ifelse(hrtest1$salary == level, 1, 0)
}
#View(hrtest1)

hrtrain1_dummy <- hrtrain1[,-c(9,10)]
hrtest1_dummy <- hrtest1[,-c(9,10)]

#create sigmoid function
sigmoid <- function(z){
  1/(1+exp(-z))
}

#check whether our sigmoid is correct or not
sigmoid(0)#correct for 0
sigmoid(10000)#correct for large positive value
sigmoid(-10000)#correct for large negative value

#cost function and gradient
# h(x) = g(z)---sigmoid function
# cost function j(beta) = (sum(i,-y(i)*log(h_beta(x(i)))-(1-y(i))*log(1-h_beta(x(i)))))*1/nrow(x(i)) --> i for all rows/obs
# gradient dj(beta)/d(beta(k)) = (sum(i,h_beta(x(i))-y(i)x(k)))*1/nrow(x(i)) --> k is no of variables

#now we will create X vectors
# In X vector we will add 1 in each row for intercept
# hrtest1$left<- as.integer(hrtest1$left)
# hrtrain1$left<-as.numeric(hrtrain1$left)
# table(hrtrain1$left)
# table(hrtest1$left)
y <- (hrtrain1$left)
ytest <- (hrtest1$left)
class(hrtest1$left)
X <- cbind(1,hrtrain1[,-c(7,9,10)])
head(X)
class(X)

Xtest <- cbind(1,hrtest1[,-c(7,9,10)])
head(Xtest)

#Initialize fitting parameters
initial_beta = matrix(rep(0,ncol(X)))
initial_beta

#create cost function
costj <- function(beta, X, y){
  cgradient <- list()
  h_beta <- sigmoid(as.matrix(X)%*%beta)
  cost = 1/nrow(X)*sum(-y*log(h_beta)-(1-y)*log(1-h_beta))
  return(cost)
}

#check cost of initial parameters
costj(initial_beta,X,y)

#create gradient function

gradientd <- function(beta, X, y){
  h_beta <- sigmoid(as.matrix(X)%*%beta)
  gradient <- 1/nrow(X)*(sigmoid(t(beta)%*%t(as.matrix(X)))-t(y))%*%as.matrix(X)
  return(gradient)
}

#check gradient for intial parameters
gradientd(initial_beta,X,y)

#now we will optimize the cost to the get the optimum parameters

opt_para <- optim(par = initial_beta,X=X,y=y,fn=costj,gr=gradientd)
names(opt_para)

cat("optimized parameters are: ")
round(opt_para$par,3)

cat("cost at optimized parameters is: ")
round(opt_para$value,3)

hrtrain1$fitted_prob <- sigmoid(as.matrix(X)%*%opt_para$par)
hrtrain1$fitted_label <- ifelse(hrtrain1$fitted_prob>=0.5,1,0)

accuracy = sum(hrtrain1$left==hrtrain1$fitted_label)/nrow(hrtrain1)
accuracy

tp = sum((hrtrain1$left==1)&(hrtrain1$fitted_label==1))
tp
fp = sum((hrtrain1$left==0)&(hrtrain1$fitted_label==1))
fp
prec = tp/(tp+fp)
cat("The precision is: ")
prec
#tp = sum((hrtrain1$left==1)&(hrtrain1$fitted_label==1))
fn = sum((hrtrain1$left==1)&(hrtrain1$fitted_label==0))
rec = tp/(tp+fn)
cat("The recall is: ")
rec

F1 = 2*(prec*rec)/(prec+rec)
cat("The F1 score is: ")
F1
###############################################################################
############Regularized Logistic Regression with optimization##################
###############################################################################

#Initialize fitting parameters
initial_beta = matrix(rep(0,ncol(X)))
initial_beta

#create cost function for regularization
costr <- function(beta, X, y,lambda){
  cgradient <- list()
  h_beta <- sigmoid(as.matrix(X)%*%beta)
  cost = 1/nrow(X)*sum(-y*log(h_beta)-(1-y)*log(1-h_beta))+lambda/(2*nrow(X))*sum(beta^2)
  return(cost)
}

#check cost of initial parameters
costr(initial_beta,X,y,lambda = 10)

#create gradient function

gradientr <- function(beta, X, y,lambda){
  h_beta <- sigmoid(as.matrix(X)%*%beta)
  gradient <- 1/nrow(X)*(sigmoid(t(beta)%*%t(as.matrix(X)))-t(y))%*%as.matrix(X)+(lambda/(nrow(X)))*c(0,beta[-1])
  return(gradient)
}

#check gradient for intial parameters
gradientr(initial_beta,X,y,lambda = 10)

#now we will optimize the cost to the get the optimum parameters

opt_rpara <- optim(par = initial_beta,X=X,y=y,lambda=1,fn=costr,gr=gradientr,method = "BFGS")
names(opt_rpara)

cat("optimized parameters are: ")
round(opt_rpara$par,3)

cat("cost at optimized parameters is: ")
round(opt_rpara$value,3)



# predict <- sigmoid(as.matrix(Xtest)%*%opt_para$par)
# predicttest <- ifelse(predict>=0.5,1,0)
# View(predict)
# joinpredictactual <- cbind(ytest,predicttest)
# View(joinpredictactual)

hrtrain1$fitted_prob <- sigmoid(as.matrix(X)%*%opt_rpara$par)
hrtrain1$fitted_label <- ifelse(hrtrain1$fitted_prob>=0.5,1,0)
head(hrtrain1)
table(hrtrain$left)
table(hrtrain1$left,hrtrain1$fitted_label)
accuracy = sum(hrtrain1$left==hrtrain1$fitted_label)/nrow(hrtrain1)
accuracy
# though the accuracy has been improved but it does not seem good fit classification.

tp = sum((hrtrain1$left==1)&(hrtrain1$fitted_label==1))
tp
fp = sum((hrtrain1$left==0)&(hrtrain1$fitted_label==1))
fp
prec = tp/(tp+fp)
cat("The precision is: ")
prec
#tp = sum((hrtrain1$left==1)&(hrtrain1$fitted_label==1))
fn = sum((hrtrain1$left==1)&(hrtrain1$fitted_label==0))
rec = tp/(tp+fn)
cat("The recall is: ")
rec

F1 = 2*(prec*rec)/(prec+rec)
cat("The F1 score is: ")
F1
#F1 score (also F-score or F-measure) is a measure of a test's accuracy. 
# It considers both the precision p and the recall r of the test to compute 
# the score: p is the number of correct positive results divided by the number 
# of all positive results returned by the classifier, and r is the number of 
# correct positive results divided by the number of all relevant samples 
# (all samples that should have been identified as positive). The F1 score 
# is the harmonic average of the precision and recall, where an F1 score 
# reaches its best value at 1 (perfect precision and recall) and worst at 0.

# So regularization in this case does not give the best model.
#########################################################################
###########################Best Model Description########################
# #so the best algo which provides the correctd prediction is from random forest which is
#rfmodel2 with few predictors. 
#So we will create final model using Random forest with few predictors
# as suggested by the fit and validation.

set.seed(1000)
hrdata$left<-as.factor(hrdata$left)
class(hrdata$sales)
hrdata$sales<-as.factor(hrdata$sales)
hrdata$salary<-as.factor(hrdata$salary)
Best_Model=randomForest(formula=left~satisfaction_level+number_project
                     +time_spend_company+average_montly_hours+last_evaluation
                     ,data=hrdata,importance = TRUE,mtry=3,do.trace=100)

varImpPlot(Best_Model)
print(Best_Model)

Best_Model$confusion
# Best_Model$predicted
Best_fitted <- Best_Model$predicted

#Best_fitted=predict(object=Best_Model,newdata=hrdata,type="response")

table(hrdata$left,Best_fitted)
sum(diag(table(hrdata$left,Best_fitted)))/nrow(hrdata) #99.25%

#2. ROC

plotROC(actuals = hrdata$left,predictedScores = as.numeric(Best_fitted))
#since the area under curve is 98.55, we can say  the discrimination  ability
# of our model is good.

#3. Kolmogrov Smirnov Chart
#measure the performance of classification models.

ks_plot(actuals = hrdata$left,predictedScores = as.numeric(Best_fitted))
# as the measure between left and not-left is large, we can say that
# performance of the Best model is good.
ks_stat(actuals = hrdata$left,predictedScores = as.numeric(Best_fitted))
#As the kolmogrov Smirnov statistic is 0.91.8 which is good, we can say 
# that the Best model is efficient in capturing  the responders(1).

Best_Model$importance

#As we have also best model from pre-prune decision tree and also random
# forest creates multiple decision tree so we have considered the plot
# of one of decision tree for explaning the business.
dctree4plot=rpart(formula=left~satisfaction_level+number_project
                    +time_spend_company+average_montly_hours+last_evaluation
                    ,data=hrdata,method="class",
                    control = rpart.control(cp = 0.0,maxdepth = 9,minsplit = 100,minbucket = 20))
#summary(rpartmodelpre)
windows()
fancyRpartPlot(model=dctree4plot,main="Tree of  Hr model",cex=0.6)

# So the conclusion is:
#1. The critical attributes on which the business has to focus on are:
#a.satisfaction_level
#b.number_project
#c.time_spend_company
#d.average_montly_hours
#e.last_evaluation
#2.As the satisfaction level increases mostly people tend to leave the
# Organization.
#3. More people tend to leave if they have less than 3 projects, so 
# business has to focus on employee and the project they should be alligned to.
#4.People who spends daily on an average 6.5 hours in the office, they 
# do not tend to leave the organization, so policy need to brought in to
# strictly follow the business hours.
#5.Employees who spend less than 126 hours on monthly basis they are the
# ones who are leaving the organization in bigger number.
#6.Employess who has last_evaluation less than .44 they are not leaving 
# the organization, it mean employees need to evaluated frequently so
# they can understand their goals and do not leave the organization.
#7. Other attributes are not much impacting the attrition rate.
#8.This does not sums up the conclusion of HR Analytics Model, as the time
# progress we have to evolve this model and improve the model and remove
#the nuances.


