## TimeSeries Analysis ########################################################
# 21.11.11(목)

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/rwork/")

  # 라이브러리 모음
install.packages("forecast") #auto.arima()
install.packages('TTR')

library(forecast)
library(TTR)


## 비정상성 시계열을 정상성 시계열로 변경 #####################################
# 1) AirPassengers 데이터 셋 가져오기
data("AirPassengers")

# 2) 차분 적용 - 평균 정상화
par(mfrow = c(1, 2))
ts.plot(AirPassengers)
diff <- diff(AirPassengers)
plot(diff)

# 3) 로그 적용 - 분산 정상화
par(mfrow = c(1,2))
plot(AirPassengers)
log <- diff(log(AirPassengers))
plot(log)




## 시계열 자료 시각화 #########################################################
# 1) 데이터 가져오기
data("WWWusage")
str(WWWusage)
WWWusage

# 2) 시계열 자료 추세선 시각화
x11()
ts.plot(WWWusage, type = 'l', col = 'red')




## 다중 시계열 자료 시각화 ####################################################
# 1) 데이터 가져오기
data(EuStockMarkets)
head(EuStockMarkets)

# 2) 데이터프레임으로 변환
EuStock <- data.frame(EuStockMarkets)
head(EuStock)

# 3) 단일 시계열 자료 추세선 시각화(1,000개 데이터 대상)
plot(EuStock$DAX[1:1000], type ='l', col='red')

# 4) 다중 시계열 자료 추세선 시각화(1,000개 데이터 대상)
plot.ts(cbind(EuStock$DAX[1:1000], EuStock$SMI[1:1000]), main='주가지수 추세선')




## 시계열 요소 분해 시각화 ####################################################
# 1) 시계열 자료 준비
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65,
          55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75,
          56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

# 2) 시계열 자료 생성 - 시계열 자료 형식으로 객체 생성
tsdata <- ts(data, start = c(2016,1),frequency = 12)
tsdata

# 3) 추세선 확인 - 각 요인(추세, 순환, 계절, 불규칙)을 시각적으로 확인
ts.plot(tsdata)

# 4) 시계열 분해
plot(stl(tsdata,'periodic'))

# 5) 시계열 분해와 변동요인 제거
m <- decompose(tsdata)
attributes(m)

plot(m)

par(mfrow = c(1,1))
plot(tsdata - m$seasonal)

# 6) 추세요인과 불규칙요인 제거
plot(tsdata - m$trend)
plot(tsdata - m$seasonal - m$trend)




## 자기 상관 함수/부분 자기 상관 함수 시각화 ##################################
# 1) 시계열 자료 생성
input <- c(3180, 3000, 3200, 3100, 3300, 3200,
           3400, 3550, 3200, 3400, 3300, 3700)
length(input)
tsdata <- ts(input, start = c(2015,2), frequency = 12)

# 2) 자기 상관 함수 시각화
acf(na.omit(tsdata), main = "자기상관함수", col = 'red')
  # 점선은 유의미한 자기 상관관계에 대한 임계값을 의미

# 3) 부분 자기 상관 함수 시각화
pacf(na.omit(tsdata),main="부분 자기 상관 함수", col='red')

## 추세 패턴 찾기 시각화 ####
# 시계열 자료의 추세 패턴 찾기 시각화
# 1) 시계열 자료 생성
input <-  c(3180, 3000, 3200, 3100, 3300, 3200,
    3400, 3550, 3200, 3400, 3300, 3700)

# 2) 추세선 시각화
plot(tsdata, type = 'l', col = 'red')

# 3) 자기 상관 함수 시각화
acf(na.omit(tsdata), main = '자기 상관함수', col='red')

# 4) 차분 시각화
plot(diff(tsdata, differences = 1))




## 시계열분석 기법 ############################################################
# 평활법
# 1) 이동평균
#  : 일정한 기간의 자료를 평균으로 계산하고 이동시킨 추세를 파악하여
#   다음기간의 추세를 예측하는 방법

# 2) 지수평활법
#  : 최근 시계열에 더 큰 가중치를 적용하는 방법
# 2-1) 시계열 자료 생성
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65,
      55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75,
      56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

tsdata <- ts(data, start = c(2016,1),frequency = 12)
tsdata

# 2-2) 평활 관련 패키지 설치
library(TTR)

# 2-3) 이동평균법으로 평활 및 시각화
par(mfrow = c(2, 2))
plot(tsdata, main = "원 시계열 자료")
plot(SMA(tsdata, n = 1), main = "1년 단위 이동평균법으로 평활")
plot(SMA(tsdata, n = 2), main = "2년 단위 이동평균법으로 평활")
plot(SMA(tsdata, n = 3), main = "3년 단위 이동평균법으로 평활")
par(mfrow = c(1,1))
    
## ARIMA 모형 시계열 예측 ####
# 계절성이 없는 정상성 시계열 분석
# 1) 데이터 준비
input <- c(3180, 3000, 3200, 3100, 3300, 3200, 
           3400, 3550, 3200, 3400, 3300, 3700)
# 2) 시계열 객체 생성(12개월)
tsdata <- ts(input, start = c(2015, 2), frequency = 12)
tsdata

# 3) 추세선 시각화
plot(tsdata, type = 'l', col = 'red')

# 4) 정상성 시계열 변환
par(mfrow = c(1,2))
ts.plot(tsdata)
diff <- diff(tsdata)
plot(diff)

# 5) 모델 식별과 추정
arima <- auto.arima(tsdata)
arima

# 6) 모형 생성
model <- arima(tsdata, order = c(1, 1, 0))
model

# 7) 모형 타당성 검정: 자기 상관 함수에 의한 모형 진단
tsdiag(model)

# 8) Box-Ljung검정에 의한 잔차항 모형 진단
Box.test(model$residuals, lag = 1, type = 'Ljung')

# 9) 미래 예측
fore <- forecast(model)
fore
par(mfrow = c(1,2))
plot(fore)
model2 <- forecast(model, h = 6)
plot(model2)




## 정상성 시계열의 계절성 #####################################################
# 1) 계절성을 갖는 정상성 시계열의 분석: 데이터 준비
data <- c(55, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65,
    55, 49, 67, 55, 71, 78, 61, 65, 69, 53, 70, 75,
    56, 56, 65, 55, 68, 80, 65, 67, 77, 69, 79, 82,
    57, 55, 63, 60, 68, 70, 58, 65, 70, 55, 65, 70)
length(data)

# 2) 시계열 자료 생성
tsdata <- ts(data, start = c(2020, 1), frequency = 12)
tsdata

# 3) 시계열 요소 분해 시각화
ts_feature <- stl(tsdata, s.window = 'periodic')
plot(ts_feature)

# 4) 정상성 시계열 변환
par(mfrow=c(1,2))
ts.plot(tsdata)
diff <- diff(tsdata)
plot(diff)

# 5) 모형 식별과 추정
ts_model2 <- auto.arima(tsdata)
ts_model2

# 6) 모형 생성
model <- arima(tsdata, c(0, 1, 1), seasonal = list(order = c(1, 1, 0)))
model

# 7) 모형 타당성 검정: 자기 상관 함수에 의한 모형 진단
tsdiag(model)

# 8) Box-Ljung에 의한 잔차항 모형 진단
Box.test(model$residuals, lag = 1, type = 'Ljung')

# 9) 미래 예측
par(mfrow = c(1, 2))
fore <- forecast(model, h = 24)
plot(fore)
fore2 <- forecast(model, h=6)
plot(fore2)
