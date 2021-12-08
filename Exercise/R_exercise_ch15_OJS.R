## R_exercise_ch15 ############################################################
# 21.11.09(화) 오준서

  # 환경설정
rm(list=ls())
getwd()
setwd('C:/rwork/')

  # 라이브러리 모음
install.packages("ggplot2")

library(ggplot2)
library(car)


## 문제1 ######################################################################
# product.csv 파일의 데이터를 이용하여 다음의 단계별로 다중 회귀분석을 수행하시오
product <- read.csv('product.csv',header = T)

# 1) 학습데이터(train), 검정데이터(test)를 7:3 비율로 샘플링
# 변수모델링: y변수는 제품_만족도, x변수는 제품_적절성과 제품_친밀도
spData <- sample(1:nrow(product), 0.7*nrow(product)) # 데이터 샘플링
train <- product[spData,] # 훈련 데이터 (184개의 데이터 샘플링)
test <- product[-spData,] # 검정 데이터 (80개의 데이터 샘플링)


# 2) 학습데이터 이용 회귀모델 생성
model <- lm(formula = 제품_만족도 ~ 제품_친밀도 + 제품_적절성 , data = train)
model
summary(model)
    # 제품 친밀도가 1씩 증가할 때마다 제품 만족도는 0.08223씩 증가한다.
    # 제품 적절성이 1씩 증가할 때마다 제품 만족도는 0.72758만큼 증가한다.
#par(mfrow=c(2,2))
#plot(model)
x11();par(mfrow=c(2,2))
plot(model)
    # Normal Q-Q: 두 변수간의 분포를 비교(주어진 데이터와 정규분포)
    #! Fitted, Leverage 복습 필요


# 3) 검정데이터 이용 모델 예측치 생성
pred <- predict(model, test)
pred # 80개의 검정 데이터를 이용한 예측치(랜덤)
mean(pred) #? 예측치의 평균값도 쓸모가 있나요?


# 4) 모델 평가: cor()함수 이용
cor(pred, test$제품_만족도)
    # 1차시도:0.6912744  2차시도:0.7375163  3차시도:0.8601113
    # 모델이 뚜렷한 양적 선형관계를 나타내므로 친밀도와 적절성은
    # 제품 만족도에 관련이 크다는 것을 의미한다.




## 문제2 ######################################################################
# ggplot2 패키지에서 제공하는 diamonds 데이터 셋을 대상으로
# carat, table, depth 변수 중에서 다이아몬드의 가격(price)에
# 영향을 미치는 관계를 다중회귀 분석을 이용하여 예측하시오
rm(list=ls())
data(diamonds)

# 회귀모델 생성
dia_model <- lm(formula = price ~ carat + table + depth, data = diamonds)


# 2-1) 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
cor(diamonds[,c(1,5,6,7)]) # 1:carat  5:depth  6:table  7:price
    # 답) price행에서 carat이 1에 가장 가까우므로 가장 큰 영향을 미친다
    # 해설) cor()함수는 상관관계를 나타내는 것이므로 인과관계를 평가할 수 없다.
    #       따라서, cor()함수를 쓰는 것은 옳지 않다.


# 2-2) 다중회귀 분석 결과를 정(+)과 부(-)관계로 해설
summary(dia_model)
  # 답) catat이 1씩 증가할 때마다 가격이 7,858.771만큼 증가한다.
  #     table이 1씩 증가할 때마다 가격이 104.473만큼 감소한다.
  #     depth가 1씩 증가할 때마다 가격이 151.236만큼 감소한다.



