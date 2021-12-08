## R_exercise_ch17 ############################################################
# 21.11.12(금)
# R 시계열분석 연습문제 [오준서]

  # 환경설정
rm(list=ls())
setwd('C:/rwork/')

  # 라이브러리 모음
install.packages("forecast")
library(forecast)


## 1번문제 #####################################################################
# 시계열 자료를 대상으로 다음과 같은 단계별로 시계열 모형을 생성하고 예측하시오.
data(EuStockMarkets)
EuStock <- data.frame(EuStockMarkets)
head(EuStock)
Second <- 1:500
DAX <- EuStock$DAX[1001:1500]
EuStock_df <- data.frame(Second, DAX)

# 1) 시계열 자료 생성: EuStock.df$DAX 컬럼 대상 2017년 1월 기준 12개월
TimeSeries.ts <- ts(EuStock_df$DAX, start=c(2017/01), frequency = 12)
plot(TimeSeries.ts)

# 2) 단위 시계열 자료 분석:
# 2-1) stl()함수 이용 시계열 분해요소 시각화
plot(stl(TimeSeries.ts,'periodic'))

# 2-2) decomosed()함수 이용 분해 시각화와 불규칙 요인 시각화
Dax.ts <- decompose(TimeSeries.ts)
plot(Dax.ts)

# 2-3) 계절요인 , 추세요인 제거 그래프 불규칙 요인만 출력
plot(TimeSeries.ts - Dax.ts$seasonal - Dax.ts$trend)


# 3) ARIMA 시계열 모형 생성
ari <- auto.arima(TimeSeries.ts)
ari

# 4) 시계열 예측: 향후 3년, 95% 신뢰수준으로 예측 및 시각화
future.3Y <- forecast(ari,level=95, h = 36)
par(mfrow = c(1,1))
plot(future.3Y, main = '향후3년 시계열 예측\n95%신뢰수준')




## 2번문제 #####################################################################
# Sales.csv 자료를 대상으로 시계열 자료를 생성하고 단계별로 시계열 모형을
# 생성하여 예측하시오.
sales <- read.csv('Sales.csv', header = T)
# 1) 시계열 자료 생성 goods$ Goods 컬럼으로 2015 년 1 월 기준 12 개월 단위
sales.Ts <- ts(sales$Goods, start=c(2015, 1), frequency=12)
sales.Ts

# 2) 시계열 모형 추정과 모형 생성
ari.model <- auto.arima(sales.Ts) #시계형 모형 생성
ari.model

# 3) 시계열 모형 진단
Box.test(ari.model$residuals, lag = 1, type = 'Ljung')  

# 4) 향후 7 개월 예측
future.7M <- forecast(ari.model, h=7)

# 5) 향후 7 개월 예측결과 시각화
plot(future.7M)



