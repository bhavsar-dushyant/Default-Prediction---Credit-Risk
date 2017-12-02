## Author: Dushyant Bhavsar
## Group Name: FRA Assignment Grp11
## Naive Bayes

## Let us first set the working directory path

#setwd ("g:/xxxxx")
#getwd()

## Data Import
##RFDF <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
bank <- Training_FRA
View(bank)
str(bank)
summary(bank)


##drop unwanted column 
#RFDF <- (RFDF[, -1])
ncol(RFDF)

#check for missing values
table(is.na(bank))
colSums(is.na(bank))


# Outlier Treatment
quantile(bank$RevolvingUtilizationOfUnsecuredLines, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

bank$RevolvingUtilizationOfUnsecuredLines[which(bank$RevolvingUtilizationOfUnsecuredLines>=1)]<- 0.897

quantile(bank$DebtRatio, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

#bank$DebtRatio[which(bank$DebtRatio>2.006825397)]<- 2.006825397

quantile(bank$NumberOfOpenCreditLinesAndLoans, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

#bank$NumberOfOpenCreditLinesAndLoans[which(bank$NumberOfOpenCreditLinesAndLoans>24)]<- 24

quantile(bank$NumberOfDependents, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

RFDF <- bank

## Response Rate
RFDF.dev <- RFDF
RFDF.dev$Target <- RFDF.dev$SeriousDlqin2yrs
RFDF.dev<- RFDF.dev[,-1]

RFDF.holdout<-testing
RFDF.holdout$Target <- RFDF.holdout$SeriousDlqin2yrs
RFDF.holdout <- RFDF.holdout[,-1]

sum(RFDF.dev$Target) / nrow(RFDF.dev)
sum(RFDF.holdout$Target) / nrow(RFDF.holdout)

#install.packages("e1071")
library(e1071)
#?naiveBayes

# Fitting model
tRF <-naiveBayes(as.factor(Target) ~ ., data = RFDF.dev)
summary(tRF)
tRF

## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="raw")
head(RFDF.dev)

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
library(data.table)
tmp_DT = data.table(RFDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)



library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")
auc
KS
gini

#confusion matrix for development
with(RFDF.dev, table(Target, predict.class))


## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="raw")
with(RFDF.holdout, table(Target, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);

#confusion matrix for holdout
with(RFDF.holdout, table(Target, predict.class))

View(h_rank)

