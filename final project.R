library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrgram)
library(sqldf)
library(reshape2)
library(gridExtra)
adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'FinalWeight', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
view(head(adult))
str(adult)
View(adult)
summary(adult)
any(is.na(adult))
adult <- na.omit(adult)

#combining workclass
adult$workclass <- as.character(adult$workclass)
adult$workclass[adult$workclass == "Without-pay" | adult$workclass == "Never-worked"] <- "Jobless"
adult$workclass[adult$workclass == "State-gov" |adult$workclass == "Local-gov"]  <- "govt" 
adult$workclass[adult$workclass == "Self-emp-inc" |adult$workclass == "Self-emp-not-inc"]  <- "Self-employed" 
adult[adult == "?"] <- NA
table(adult$workclass)

adult$workclass<-ifelse(adult$workclass=='?','Unknown',as.character(adult$workclass))
Work_class<-sqldf('SELECT workclass, count(workclass) as Count 
                  ,sum(income) as Above from adult group by workclass')
table<-data.frame(Class=Work_class$workclass, Proportion=Work_class$Above/Work_class$Count)
Work_class$Below<-Work_class$Count-Work_class$Above
Work_class<-Work_class[,c(1,3,4)]
Workclass<-melt(Work_class,id.vars = 'workclass')
gg<-ggplot(Workclass,aes(x=workclass,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different classes')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))

corrgram(adult, lower.panel=panel.shade, upper.panel=panel.density)

#histograms - continuous variables
colnames(adult)[12]<-'CapitalLoss'
gg<-qplot(CapitalLoss, data=adult, geom="histogram")+theme_bw()+ggtitle('Histogram of Capital Loss')
gg

colnames(adult)[13]<-'Hours'
gg<-qplot(Hours, data=adult, geom="histogram")+theme_bw()+ggtitle('Histogram of Working Hours')
gg

#combining marital status
adult$marital_status <- as.character(adult$marital_status)
adult$marital_status[adult$marital_status == "Married-AF-spouse" |
                       adult$marital_status == "Married-civ-spouse" |
                       adult$marital_status == "Married-spouse-absent"] <- "Married"
adult$marital_status[adult$marital_status == "Divorced" |
                       adult$marital_status == "Separated" |
                       adult$marital_status == "Widowed"] <- "Not-Married"
table(adult$marital_status)
#combining countries
adult$native_country <- as.character(adult$native_country)
north.america <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                   "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                   "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                   "United-States")
asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
south.america <- c("Columbia", "Ecuador", "Peru")
europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
other <- c("South", "?")
adult$native_country[adult$native_country %in% north.america] <- "North-America"
adult$native_country[adult$native_country %in% asia]  <- "Asia"
adult$native_country[adult$native_country %in% south.america] <- "South-America" 
adult$native_country[adult$native_country %in% europe] <-  "Europe"  
adult$native_country[adult$native_country %in% other] <- "Other"
table(adult$native_country)
adult <- na.omit(adult)

# EDA
ggplot(adult, aes(age)) + geom_histogram(aes(fill = income), color = "black",
                                         binwidth = 1)
ggplot(adult, aes(hours_per_week)) + geom_histogram(fill = 'darkblue')


# Export data
write.csv(adult,"C:/Users/sijia/Desktop/courses/BANA7038/final/adult.csv")

# cleaning test data
adult.test <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test', sep = ',', fill = F, strip.white = T,skip = 1)
colnames(adult.test) <- c('age', 'workclass', 'FinalWeight', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
View(adult.test)
any(is.na(adult.test))
adult.test <- na.omit(adult.test)
#combining workclass
adult.test$workclass <- as.character(adult.test$workclass)
adult.test$workclass[adult.test$workclass == "Without-pay" | adult.test$workclass == "Never-worked"] <- "Jobless"
adult.test$workclass[adult.test$workclass == "State-gov" |adult.test$workclass == "Local-gov"]  <- "govt" 
adult.test$workclass[adult.test$workclass == "Self-emp-inc" |adult.test$workclass == "Self-emp-not-inc"]  <- "Self-employed" 
adult.test[adult.test == "?"] <- NA
adult.test <- na.omit(adult.test)
table(adult.test$workclass)

#combining marital status
adult.test$marital_status <- as.character(adult.test$marital_status)
adult.test$marital_status[adult.test$marital_status == "Married-AF-spouse" |
                       adult.test$marital_status == "Married-civ-spouse" |
                       adult.test$marital_status == "Married-spouse-absent"] <- "Married"
adult.test$marital_status[adult.test$marital_status == "Divorced" |
                       adult.test$marital_status == "Separated" |
                       adult.test$marital_status == "Widowed"] <- "Not-Married"
table(adult.test$marital_status)
#combining countries
adult.test$native_country <- as.character(adult.test$native_country)
adult.test$native_country[adult.test$native_country %in% north.america] <- "North-America"
adult.test$native_country[adult.test$native_country %in% asia]  <- "Asia"
adult.test$native_country[adult.test$native_country %in% south.america] <- "South-America" 
adult.test$native_country[adult.test$native_country %in% europe] <-  "Europe"  
adult.test$native_country[adult.test$native_country %in% other] <- "Other"
table(adult.test$native_country)


# logistic model with all variables
adult$workclass <- as.factor(adult$workclass)
adult$marital_status<- as.factor(adult$marital_status)
adult$native_country <-as.factor(adult$native_country)

adult.test$workclass <- as.factor(adult.test$workclass)
adult.test$marital_status<- as.factor(adult.test$marital_status)
adult.test$native_country <-as.factor(adult.test$native_country)


# 1. logit link function
model1 <- glm(income~., family=binomial(link="logit"),data=adult)
AIC(model1)
BIC(model1)
model1$deviance

library(ROCR)
# in-sample prediction
pred.model1.train <- predict(model1, type="response")
pred1 <- prediction(pred.model1.train, adult$income)
unlist(slot(performance(pred1, "auc"), "y.values"))
class.model1.train <- (pred.model1.train>0.5)*1
MR1 <- mean(adult$income != class.model1.train)
MR1

# logit link out-of-sample prediction
#pred.model1.test <- predict(model1, newdata= adult.test,type="response")
#pred1_test <- prediction(pred.model1.test, adult.test$income)
#unlist(slot(performance(pred1_test, "auc"), "y.values"))
#class.model1.test <- (pred.model1.test>0.5)*1
#MR1_test <- mean(adult.test$income != class.model1.test)
#MR1_test

#2. probit link function
model2 <- glm(income~., family=binomial(link="probit"),data=adult)
AIC(model2)
BIC(model2)
model2$deviance
pred.model2.train <- predict(model2, type="response")
pred2 <- prediction(pred.model2.train, adult$income)
unlist(slot(performance(pred2, "auc"), "y.values"))
class.model2.train <- (pred.model2.train>0.5)*1
MR2 <- mean(adult$income != class.model2.train)
MR2

# probit link our-of-sample prediction
#pred.model2.test <- predict(model2, newdata= adult.test,type="response")
#pred2_test <- prediction(pred.model2.test, adult.test$income)
#unlist(slot(performance(pred2_test, "auc"), "y.values"))
#class.model2.test <- (pred.model2.test>0.5)*1
#MR2_test <- mean(adult.test$income != class.model2.test)
#MR2_test


# Variable selection
#1. AIC
model_aic <- step(model1)
summary(model_aic)

#AIC-ROC AUC
pred.aic.train <- predict(model_aic, type="response")
pred_aic <- prediction(pred.aic.train, adult$income)
unlist(slot(performance(pred_aic, "auc"), "y.values"))

#AIC-MR
class.model1.train.aic <- (pred.aic.train>0.5)*1
MR_aic_train <- mean(adult$income!= class.model1.train.aic)
MR_aic_train


#2. BIC
model_bic <- step(model1, k=log(nrow(adult)))
summary(model_bic)

#BIC-ROC AUC
pred.bic.train <- predict(model_bic, type="response")
pred_bic <- prediction(pred.bic.train, adult$income)
unlist(slot(performance(pred_bic, "auc"), "y.values"))

#BIC-MR
class.model1.train.bic <- (pred.bic.train>0.5)*1
MR_bic_train <- mean(adult$income!= class.model1.train.bic)
MR_bic_train


# LASSO
dummy<- model.matrix(~ ., data = adult)
head(dummy)
adult.data.lasso <- data.frame(dummy[,-1])
telecom.data.lasso
head(adult.data.lasso)
# prepare data for LASSO
adult.train.X = as.matrix(select(adult.data.lasso, -income))
adult.test.X = as.matrix(select(adult.data.lasso, -Churn)[-index,])
adult.train.Y = adult.data.lasso[, "income"]
telecom.test.Y =telecom.data.lasso[-index, "Churn"]

# LASSO model
library(glmnet)
adult.lasso <- glmnet(x=adult.train.X, y= adult.train.Y, family = "binomial")
adult.lasso.cv <- cv.glmnet(x=adult.train.X, y=adult.train.Y, family = "binomial", type.measure = "class")
plot(adult.lasso.cv)
coef(adult.lasso, s=adult.lasso.cv$lambda.min)

# AUC of LASSO
pred.lasso.train <- predict(adult.lasso,newx = adult.train.X, s=adult.lasso.cv$lambda.1se, type = "response")
pred_lasso <- prediction(pred.lasso.train, adult$income)
unlist(slot(performance(pred_lasso, "auc"), "y.values"))

# MR of LASSO
class.adult.train.lasso <- (pred.lasso.train>0.5)*1
MR_lasso_train <- mean(adult$income!= class.adult.train.lasso)
MR_lasso_train


#AIC_model ROC curve for training data
pref_aic <- performance(pred_aic,"tpr", "fpr")
plot(pref_aic, colorize=TRUE)

#AIC_model ROC curve for testing data
pred.aic.test <- predict(model_aic, newdata= adult.test,type="response")
pred_aic_test <- prediction(pred.aic.test, adult.test$income)
unlist(slot(performance(pred_aic_test, "auc"), "y.values"))
pref_aic_test <- performance(pred_aic_test,"tpr", "fpr")
plot(pref_aic_test, colorize=TRUE)

#AIC_model MR for testing data
class.model1.test.aic <- (pred.aic.test>0.5)*1
MR_aic_test <- mean(adult.test$income!= class.model1.test.aic)
MR_aic_test

# cross Validataion
full_data <- rbind(adult, adult.test)
library(boot)
adult.glm <- glm(income~. , family=binomial, data=full_data)
cv.result= cv.glm(data=full_data, glmfit = adult.glm, K=10)
cv.result$delta[2]

index <- sample(nrow(adult),nrow(adult)*0.80)
credit.train = adult[index,]
credit.test = adult[-index,]

hist(predict(model1))
hist(predict(model1,type="response"))
