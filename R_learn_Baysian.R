## Beysian Analysis ###########################################################
# 21.11.16(화)

  # 환경설정
rm(list=ls())
getwd()
setwd('c:/rwork/')

  # 라이브러리 모음
install.packages("e1071")
install.packages("caret")

library(e1071)
library(caret)


## Beysian Analysis ###########################################################
# 데이터 셋 가져오기
data <- read.csv(file = "heart.csv",header = T)

head(data)
str(data)

# 
set.seed(1234)
tr_data <- createDataPartition(y=data$AHD,p=0.7,list=FALSE)
tr <- data[tr_data,]
te <- data[-tr_data,]

Bayes <- naiveBayes(AHD~.,data=tr)
Bayes

predicted <- predict(Bayes, te, type = "class")
table(predicted,te$AHD)

AHD <- as.factor(te$AHD)
confusionMatrix(predicted,AHD)
