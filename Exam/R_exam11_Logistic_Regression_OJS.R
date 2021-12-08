## exam11 Logistic Regression #################################################
# 2021.11.15(월) 오준서

  # 환경설정
rm(list=ls())
getwd()
setwd('C:/rwork/')

  # 라이브러리 모음
install.packages("gmodels")
library(dplyr)
library(gmodels)


## 문제2 ######################################################################
# 제공된 cleanData.csv 파일 내 데이터에서 나이(age3)와 직위(position)간의
# 관련성을 단계별로 분석하시오.
# 1) 파일 가져오기(파일 내 데이터 저장)
clean <- read.csv('cleanData.csv',header=T)

# 2) 코딩 변경(변수 리코딩)
  # x <- data$position # 행 - 직위변수 이용
  # y <- data$age3 # 열 - 나이 리코딩 변수 이용
names(clean)
x <- clean$position
y <- clean$age3


# 3) 산점도를 이용한 변수간의 관련성 보기(plot(x,y)함수 이용)
plot(x,y)
clean %>% select(5,9) %>% pairs(panel=panel.smooth, main='변수 관련성')


# 4) 결과 해석
# 직급이 높아질수록 연령이 청년층>중년층>장년층 순서로 점차 높아진다.




## 문제3 ######################################################################
# 제공된 response.csv 파일 내 데이터에서 작업 유형에 따른 응답 정도에 차이가 
# 있는가를 단계별로 검정하시오.
# 1) 파일 가져오기(파일 내 데이터 저장)
response <- read.csv('response.csv',header=T)

# 2) 코딩 변경 리코딩
# Job 컬럼:  1:학생  2:직장인  3: 주부
# Response 컬럼:  1:무응답  2:낮음  3:높음
response$job[response$job == 1] <- '학생'
response$job[response$job == 2] <- '직장인'
response$job[response$job == 3] <- '주부'

response$response[response$response == 1] <- '무응답'
response$response[response$response == 2] <- '낮음'
response$response[response$response == 3] <- '높음'

head(response)

# 3) 교차 분할표 작성
CrossTable(response$job,response$response)




## 문제4 ######################################################################
# mtcars 데이터에서 엔진(vs)을 종속변수로, 연비(mpg)와 변속기종류(am)를
# 독립변수로 설정하여 로지스틱 회귀분석을 실시하시오.
# 1) 데이터 가져오기
data("mtcars")
str(mtcars)

# 2) 로지스틱 회귀분석 실행하고 회귀모델 확인
car.lm <- glm(vs ~ mpg + am ,data=mtcars, family = 'binomial', na.action = na.omit)
car.lm

# 3) 로지스틱 회귀모델 요약정보 확인
summary(car.lm)

# 4) 로지스틱 회귀식
  # vs = -12.7051 + 0.6809(mpg) - 3.0073(am)

# 5) 결과 해석
  # vs=0: v자형 엔진이고 vs=1: 직렬엔진을 의미하고
  # am=0: 자동 am=1: 수동 엔진임을 의미한다.
  # 연비가 좋거나 자동엔진을 쓸 수록 직렬엔진을 쓸 확률이 높고
  # 연비가 낮거나 수동엔진을 쓸 수록 v자형 엔진을 쓸 확률이 높다.

# 6) mpg가 30이고 자동변속기(am=0)일 때 승산(odds)?
car.pred <- predict(car.lm, newdata=data.frame(mpg=30,am=0), type = 'response')
car.pred
  # 직렬엔진을 쓸 확률(p)이 0.9995574이므로
  # odds = 0.9995574 / 1 - 0.9995574 = 2,258.37 이 나온다.




## test zone ##################################################################
mpg <- 30; am <- 0
vs <- -12.7051+0.6809*mpg-3.0073*am
vs
odds = exp(vs)
odds



