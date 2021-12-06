## Logistic Regression ########################################################
# 21.11.10(수)

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/rwork/")

  # 라이브러리 모음
install.packages("ROCR") #로지스틱 회귀분석 패키지
install.packages("tseries") #시계열분석 패키지
install.packages("forecast")

library(car)
library(lmtest)
library(ROCR)
library(tseries)
library(forecast)


## 로지스틱 회귀분석 ##########################################################
# 1) 데이터 가져오기
weather = read.csv("weather.csv", stringsAsFactors=F)
dim(weather)
head(weather)
str(weather)

# 2) 변수 선택과 더미 변수 생성
weather_df <- weather[, c(-1,-6,-8,-14)]
str(weather_df)

weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <-  1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

# 3) 학습데이터와 검정데이터 생성(7:3비율)
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx,]
test <- weather_df[-idx,]

# 4) 로지스틱 회귀모델 생성
weather_model <- glm(RainTomorrow ~ ., data = train,
                     family = 'binomial', na.action=na.omit)
weather_model
summary(weather_model)

# 5) 로지스틱 회귀모델 예측치 생성
pred <-  predict(weather_model, newdata = test, type = "response")
pred

result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred

table(result_pred)

# 6) 모델평가 - 분류정확도 계산
table(result_pred, test$RainTomorrow)

# 7) ROC Curve를 이용한 모델 평가
pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.maeasure = 'fpr')
plot(prf)




## ncs 교재 42~45page #########################################################
data(mtcars)
dat <- subset(mtcars, select=c(mpg, am, vs))
dat

# 로지스틱 회귀분석 모델 생성
log_reg <- glm(vs ~ mpg, data=dat, family='binomial')
log_reg # 모델 확인
summary(log_reg) # 회귀분석 모델 요약정보 확인



## 예측: ARIMA 모델 46page ####################################################
# 시계열 분석과 로지스틱 회귀분석의 비교
# 1) 데이터 확인
data("AirPassengers")
AirPassengers
plot(AirPassengers)
plot(stl(AirPassengers, s.window = 'periodic'))

# 2) 차분
difflogAirPassengers <- diff(log(AirPassengers))
plot(difflogAirPassengers)

# 3) 시계열의 안정성 확인
adf.test(difflogAirPassengers, alternative="stationary", k=0)

# 4) 예측 모델 생성
auto.arima(difflogAirPassengers)
fitted <- arima(log(AirPassengers), c(1, 0, 1), seasonal =
                  list(order = c(0, 1, 1), period = 12))
fitted
predicted <- predict(fitted, n.ahead = 120)

# 5) 시각화
ts.plot(AirPassengers, exp(predicted$pred), lty = c(1,2))



