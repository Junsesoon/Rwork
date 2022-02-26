# exercise ch16 오준서 ####
# 220126(수)

  #환경설정
rm(list=ls())
setwd('c:/rwork/')

  #라이브러리
#install.packages("arules")
library(arules)
#install.packages("backports") #deparse1
library(backports)
#install.packages("arulesViz")
library(arulesViz)
#install.packages("dplyr") #파이프 연산자
library(dplyr)


# 1번문제 ####
# tranExam.csv 파일을 대상으로 중복된 트랜잭션 없이 1-2 컬럼만 single 형식으로
# 트랜잭션 객체를 생성하시오.
## 1)트랜잭션 객체 생성 및 확인
data <- read.transactions('tranExam.csv',
                          format = 'single',
                          cols = c(1,2),
                          rm.duplicates = T)
inspect(data)

## 2)각 items 별로 빈도수 확인
summary(data)

## 3)파라미터 supp = 0.3, conf = 0.1) 를 이용하여 규칙 rule) 생성
rules <- apriori(data, parameter = list(supp = 0.3, conf = 0.1))

## 4)연관규칙 결과 보기
inspect(head(rules))
# 생성된 규칙이 없으므로 결과 미출력



# 2번문제 ####
# Adult 데이터 셋을 대상으로 다음 조건에 맞게 연관분석을 수행하시오
## 1)최소 support = 0.5, 최소 confidence = 0.9 를 지정하여 연관규칙을 생성한다
data(Adult)
rules1 <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9))

## 2)수행한 결과를 lift 기준으로 정렬하여 상위 10 개 규칙을 기록한다
rules1 %>% sort(by='lift') %>% head(10) %>% inspect()

## 3)연관분석 결과를 LHS 와 RHS 의 빈도수로 시각화한다
plot(rules1, method = "grouped")

## 4)연관분석 결과를 연관어의 네트워크 형태로 시각화한다
plot(rules1, method = "graph")

## 5)연관어 중심 단어를 해설한다
  # sex=Male
  # native-country=United-States
  # race=White, 
  # capital-gain=None
  # capital-loss=None
  # workclass=Private 
  # hours-per-week=Full-time
  # 총 14개의 단어들로 7개의 규칙이 생성되었다.
  # 조합해보면 미국의 백인 남성들의 직장생활과 주로 관련된 데이터임을 알 수 있다.



