## Author: Dushyant Bhavsar
## Group Name: FRA Assignment Grp11
## Logestic Regression


library(caret)
library(ggplot2)
library(Information)
library(caTools)
library(stringr)
library(car)
library(ROCR)
library(MASS)
library(gmodels)
library(dummies)
library(Hmisc)



bank <- Training_FRA
RFDF <- bank
# summary & structure of data
str(bank)
summary(bank)

# Normality analysis via Histogram
par(mfrow= c(2,3))

hist(bank$SeriousDlqin2yrs, main ="Default",xlab="") 
hist((bank$RevolvingUtilizationOfUnsecuredLines), main = "Utilization",xlab="")
hist((bank$DebtRatio), main = "Debt Ratio",xlab="")
hist(bank$NumberOfOpenCreditLinesAndLoans, main="No. of C Cards and Loans",xlab="")
hist(bank$NumberOfDependents, main ="No. of dependents",xlab="")

title("
      Histogram to check data distribution - Default Modeling", outer=TRUE)

# Outlier analysis via Boxplot
par(mfrow= c(2,3))

boxplot(bank$SeriousDlqin2yrs, main ="Default",xlab="") 
boxplot((bank$RevolvingUtilizationOfUnsecuredLines), main = "Utilization",xlab="")
boxplot((bank$DebtRatio), main = "Debt Ratio",xlab="")
boxplot(bank$NumberOfOpenCreditLinesAndLoans, main="No. of C Card and Loans",xlab="")
boxplot(bank$NumberOfDependents, main ="No. of Dependents",xlab="")

title("
      Boxplot to check data outlier - Default modeling", outer=TRUE)



CrossTable(bank$SeriousDlqin2yrs)

#Check For Missing Values in the Data set and replace with mode
sum(is.na(bank))

# Outlier Treatment
quantile(bank$RevolvingUtilizationOfUnsecuredLines, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

bank$RevolvingUtilizationOfUnsecuredLines[which(bank$RevolvingUtilizationOfUnsecuredLines>=1)]<- 0.897

quantile(bank$DebtRatio, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

bank$DebtRatio[which(bank$DebtRatio>10000)]<- 10000

quantile(bank$NumberOfOpenCreditLinesAndLoans, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

#bank$NumberOfOpenCreditLinesAndLoans[which(bank$NumberOfOpenCreditLinesAndLoans>24)]<- 24

quantile(bank$NumberOfDependents, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))


#dummy variable creation
train<- bank
train$term_deposit <- train$SeriousDlqin2yrs
train<- train[,-1]
View(train)

#testing<- test
test<-testing
test$term_deposit <- test$SeriousDlqin2yrs
test<- test[,-1]
View(test)

#Linear model to find significant features
library(lmtest)
ft1<- lm(SeriousDlqin2yrs ~ ., data = bank)
summary(ft1)
vif(ft1)

# Model with all variables
model1 <- glm(term_deposit ~ ., data = train, family = binomial)
summary(model1)
vif(model1)

# Stepwise selection of variables
#best_model = step(model1,direction = "both")
#summary(best_model)

model2 <- glm(term_deposit ~ NumberOfOpenCreditLinesAndLoans
              + RevolvingUtilizationOfUnsecuredLines, data = train, family = binomial)

summary(model2)
vif(model2)

predTrain <- predict(model1, type = "response") # in-sample accuracy
hist(predTrain)
table(train$term_deposit, predTrain >= 0.5)

predTest <- predict(model1, newdata = test, type = "response") # out-sample accuracy
hist(predTest)
table(test$term_deposit, predTest >= 0.5)

# ROC Curve
library(ROCR)
ROCRpred <- prediction(predTrain, train$term_deposit)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

auc.tmp <- performance(ROCRpred,"auc"); 
auc <- as.numeric(auc.tmp@y.values)
auc


## C-statistic
library(Hmisc)

train$predicted_prob = predict(model1,  type = "response")
rcorr.cens(train$predicted_prob,train$term_deposit) 

test$predicted_prob = predict(model1, newdata = test,type = "response")
rcorr.cens(test$predicted_prob,test$term_deposit)

#KS-statistic

model_score <- prediction(train$predicted_prob,train$term_deposit)

model_perf <- performance(model_score, "tpr", "fpr")

ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)

which(ks_table == ks)

ks

model_score_test <- prediction(test$predicted_prob,test$term_deposit)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

ks1=max(ks_table_test)

which(ks_table_test == ks1)


ks1


