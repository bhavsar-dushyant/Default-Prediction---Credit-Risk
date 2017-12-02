RCDF<- train 
install.packages("openxlsx")
library(openxlsx)
table(RCDF$SeriousDlqin2yrs)
str(RCDF)
RCDF$SeriousDlqin2yrs <- as.factor(RCDF$SeriousDlqin2yrs)

install.packages("DMwR")
library(DMwR)
set.seed(1234)
?SMOTE
trainSet <- SMOTE(SeriousDlqin2yrs ~.,as.data.frame(RCDF),perc.over=500, perc.under = 100)
table(trainSet$SeriousDlqin2yrs)
trainSet$SeriousDlqin2yrs <- as.numeric(trainSet$SeriousDlqin2yrs)
write.xlsx(trainSet, "SmoteTraining.xlsx")
