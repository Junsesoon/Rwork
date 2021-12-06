## Decision Tree ##############################################################
# 21.11.16(화)

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("party")
install.packages("cvTools")
install.packages("arules") #subset()
install.packages("ggplot2") #고급 시각화 패키지
install.packages("rpart") #CART 기법
install.packages("rpart.plot")

library(party)
library(datasets)
library(cvTools)
library(arules)
library(ggplot2)
library(rpart)
library(rpart.plot)

data(AdultUCI)
data(mpg)


## 의사결정 트리 생성 #########################################################
str(airquality)

# formula 생성
formula <- Temp ~ Solar.R+Wind+Ozone

# 분류모델 생성
air_ctree <- ctree(formula, data=airquality)
air_ctree

#분류분석 결과
plot(air_ctree)




## 학습데이터와 검정데이터 샘플링으로 분류분석 ################################
# 1) 샘플링
set.seed(1234)
idx <- sample(1:nrow(iris),nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]

# 2) formula생성
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 3) 학습데이터 이용 분류모델 생성
iris_ctree <- ctree(formula, data = train)
iris_ctree

# 4) 분류모델 플로팅
plot(iris_ctree, type = "simple")
plot(iris_ctree)

# 5) 분류모델 평가
pred <- predict(iris_ctree, test)
table(pred, test$Species)
  # 분류 정확도
(14 + 16 + 13) / nrow(test)




## K겹 교차 검정 샘플링으로 분류분석 ##########################################
# 1) K겹 교차 검정을 위한 샘플링
cross <- cvFolds(nrow(iris), K=3, R=2)

# 2) K겹 교차 검정 데이터 보기
str(cross)
cross
length(cross$which)
dim(cross$subsets)
table(cross$which)

# 3) K겹 교차 검정 수행
R = 1:2
K = 1:3
CNT = 0
ACC <- numeric()
for(r in R) {
  cat('\n R = ', r, '\n')
  for(k in K) {
    datas_ids <- cross$subsets[cross$which == k, r]
    test <- iris[datas_ids, ]
    cat('test : ', nrow(test), '\n')
    formual <- Species ~ .
    train <- iris[-datas_ids, ]
    cat('train : ', nrow(train), '\n')
    model <- ctree(Species ~ ., data = train)
    pred <- predict(model, test)
    t <- table(pred, test$Species)
    print(t)
    CNT <- CNT + 1
    ACC[CNT] <- (t[1, 1] + t[2, 2] + t[3, 3]) / sum(t)
  }
}
CNT

# 4) 교차 검정 모델 평가
ACC
length(ACC)
result_acc <- mean(ACC, na.rm = T)
result_acc




## 고속도로 주행거리에 미치는 영향변수 보기 ###################################
# 1) 패키지 설치 및 로딩
library(ggplot2)
data(mpg)

# 2) 학습데이터와 검정데이터 생성
t <- sample(1:nrow(mpg), 120)
train <- mpg[-t, ]
test <- mpg[t, ]
dim(train)
dim(test)

# 3) formula작성과 분류모델 생성
test$drv <- factor(test$drv)
formula <- hwy ~ displ + cyl + drv
tree_model <- ctree(formula, data = test)
plot(tree_model)




## Adultuci 데이터 셋을 이용한 분류분석 #######################################
# 1) 패키지 설치 및 데이터 셋 구조 보기
str(AdultUCI)
names(AdultUCI)

# 2) 데이터 샘플링
set.seed(1234)
choice <- sample(1:nrow(AdultUCI), 10000)
choice
adult.df <- AdultUCI[choice, ]
str(adult.df)

# 3) 변수 추출 및 데이터프레임 생성
  # 변수 추출
capital <- adult.df$`capital-gain`
hours <- adult.df$`hours-per-week`
education <- adult.df$`education-num`
race <- adult.df$race
age <- adult.df$age
income <- adult.df$income
  # 데이터 프레임 생성
adult_df <- data.frame(capital = capital, age = age, race = race,
                       hours = hours, education = education, income = income)
str(adult_df)

# 4) formula생성 – 자본이득(capita)에 영향을 미치는 변수
formula <- capital ~ income + education + hours + race + age

# 5) 분류모델 생성 및 예측
adult_ctree <- ctree(formula, data = adult_df)
adult_ctree

# 6) 분류모델 플로팅
plot(adult_ctree)

# 7) 자본이득(capital) 요약 통계량 보기
adultResult <- subset(adult_df,
                      adult_df$income == 'large' &
                        adult_df$education > 14)
length(adultResult$education)
summary(adultResult$capital)
boxplot(adultResult$capital)




## 조건부 추론나무 ############################################################
# sampling
str(iris)
set.seed(1000)
sampnum <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))
sampnum

# training & testing data 구분
trData <- iris[sampnum==1,]
head(trData)
teData <- iris[sampnum == 2, ]
head(teData)
shortvar <- Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width

# 학습
citreeResult <- ctree(shortvar, data=trData)

# 에측값과 실제값 비교
table(predict(citreeResult), trData$Species)
citreeResult2 <- ctree(shortvar, data=teData)

# 테스트 데이터를 이용하여 분류
forcasted2 <- predict(citreeResult2, data=teData)
# forcasted
# teData$Species

# 예측결과와 실제값 비교
table(forcasted2, teData$Species)

#시각화
plot(citreeResult2)

# 결과 해석
  # 종(Species) 판단
  # Petal.Length <= 1.9 : setosa로 판단
  # Petal.Length > 1.9 & Petal.Width <= 1.6 : versicolor로 판단
  # 나머지: virginica 로 판단




## CART #######################################################################
# 1) 패키지 설치 및 로딩
library(rpart)
library(rpart.plot)

# 2) 데이터 로딩
data(iris)

# 3) rpart()함수를 이용한 분류분석
rpart_model <- rpart(Species~.,data=iris)
rpart_model

# 4) 분류분석 시각화
rpart.plot(rpart_model)



# 날씨 데이터를 이용하여 비(rain)유무 예측
# 1) 데이터 가져오기
weather = read.csv("C:/Rwork/weather.csv", header = TRUE)

# 2) 데이터 특성 보기
str(weather)
head(weather)

# 3) 분류분석 데이터 가져오기
weather.df <- rpart(RainTomorrow ~ ., data = weather[ , c(-1, -14)], cp = 0.01)

# 4) 분류분석 시각화
rpart.plot(weather.df)

# 5) 예측치 생성과 코딩 변경
  # 예측치 생성
weather_pred <- predict(weather.df, weather)
weather_pred
  # y의 범주로 코딩 변환
weather_pred2 <- ifelse(weather_pred[ , 2] >= 0.5, 'Yes', 'No')

# 6) 모델 평가
table(weather_pred2, weather$RainTomorrow)
(278 + 53) / nrow(weather)
