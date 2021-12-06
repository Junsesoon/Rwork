## Correlation Analysis #######################################################
# 21.11.10(수)

  # 환경설정
rm(list=ls())
getwd()
setwd('c:/rwork/')

  # 라이브러리 모음
install.packages("corrgram")
install.packages("PerformanceAnalytics")

library(corrgram)
library(PerformanceAnalytics)


## 기술통계량 구하기 ##########################################################
# 1) 데이터 가져오기
product <- read.csv('product.csv',header = T)
head(product)

# 2) 기술통계량
summary(product)
sd(product$제품_친밀도);sd(product$제품_적절성);sd(product$제품_만족도)




## 상관계수 보기 ##############################################################
# 형식: cor(x, y = NULL, use = ”everything”, 
#           method = c(“pearson”, “kendall”, “spearman"))

# 1) 변수 간의 상관계수 보기
cor(product$제품_친밀도, product$제품_적절성)
cor(product$제품_친밀도, product$제품_만족도)

# 2) 제품_적절성과 제품_만족도의 상관계수 보기
cor(product$제품_적절성 , product$제품_만족도)

# 3) (제품_적절성+제품_친밀도)와 제품_ 만족도의 상관계수 보기
cor(product$제품_적절성+product$제품_친밀도,product$제품_만족도)


# 전체 변수 간의 상관계수 보기
cor(product, method = "pearson")


# 방향성 있는 색상으로 표현
corrgram(product)
corrgram(product,upper.panel = panel.conf)
corrgram(product, lower.panel = panel.conf)


# 차트에 밀도곡선, 상관성, 유의확률(별표) 추가)
# 1) 상관성 , p 값 ((*), 정규분포 모수 검정 조건 시각화
chart.Correlation(product, histogram = , pch = "+")


# 서열척도 대상 상관계수
cor(product, method = "spearman")
# 피어슨 상관계수: 대상변수가 등간척도 또는 비율척도일 때
# 스피어만 상관계수: 대상변수가 서열척도일 때



