## Regression Analysis ########################################################
# 21.11.09(화)

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/rwork")

  # 라이브러리 모음
install.packages("lme4")
library(lme4)
install.packages("car") #다중공선성 확인
library(car)
install.packages('lmtest') #잔차분석
library(lmtest)


## 단순 회귀분석 ##########################################################
# 1)데이터 가져오기
product <- read.csv("product.csv", header = TRUE)

# 2)독립변수와 종속변수 생성
y = product$제품_만족도
x = product$제품_적절성
df <- data.frame(x, y)

# 3)단순 선형회귀 모델 생성
result.lm <- lm(formula = y ~ x, data = df)

# 4)회귀분석의 절편과 기울기
result.lm

# 5)모델의 적합값과 잔차 보기
names(result.lm)

# 6)적합값 보기
fitted.values(result.lm)[1:2]
  #fitted.values: 모델이 예측한 적합값

# 7)관측값 보기
head(df, 1)

# 8)회귀방정식을 적용하여 모델의 적합값 계산
Y = 0.7789 + 0.7393 * 4
Y

# 9)잔차(오차) 계산
3 - 3.735963

# 10)모델의 잔차 보기
residuals(result.lm)[1:2]
  #residuals: 모델의 잔차

# 11)모델의 잔차와 회귀방정식에 의한 적합값으로부터 관측값 계산
-0.7359630 + 3.735963



# 선형회귀 분석모델 시각화 ####
# 1)xy 산점도
plot(formula = y ~ x, data = product)

# 2선형 회귀모델 생성
result.lm <- lm(formula = y ~ x, data = product)

# 3)회귀선
abline(result.lm, col = "red")

# 4)회귀분석 결과보기
summary(result.lm)




## 다중 회귀분석 ##############################################################
# 1)변수 모델링
y = product$제품_만족도
x1 = product$제품_친밀도
x2 = product$제품_적절성
df <- data.frame(x1, x2, y)

# 2)다중 회귀분석
result.lm <- lm(formula = y ~x1 + x2, data = df)
result.lm




# 다중 공선성 문제 확인 ####
# 1)분산팽창요인(VIF)
vif(result.lm)

# 2)다중 회귀분석 결과보기
summary(result.lm)




# iris 데이터 셋을 이용한 다중 회귀분석 ####
# 1)iris 데이터 셋으로 다중 회귀분석
data(iris)
model <- lm(formula = Sepal.Length ~ Sepal.Width +
              Petal.Length + Petal.Width, data = iris)
vif(model)
sqrt(vif(model)) > 2

# 2)iris 변수 간의 상관계수 구하기
cor(iris[ ,5]) #상관계수로 변수간의 강한 상관관계 구분




# 예측치 ####
# 1)학습데이터와 검정데이터 표본 추출
x <- sample(1:nrow(iris), 0.7 * nrow(iris))
train <- iris[x, ]
test <- iris[ x, ]

# 2)변수 제거 및 다중 회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
model
summary(model)

# 3)예측치 생성
pred <- predict(model, test)
pred

# 4)상관계수를 이용한 회귀모델 평가
cor(pred, test$Sepal.Length)




## 회귀분석 수행 ##############################################################
  # 회귀분석은 선형성 다중 공선성 잔차의 정규성 등 몇가지 기본 가정이 충족되어야
  # 수행할 수있는 모수 검정방법이다

# 1)회귀모델 생성
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
model <- lm(formula = formula, data = iris)
model

# 2)독립성 검정 더빈 왓슨 값으로 확인
dwtest(model)

# 3)등분산성 검정 - 잔차와 적합값의 분포
plot(model,which = 1)

# 4)잔차의 정규성 검정
attributes(model)
res <- residuals(model)
shapiro.test(res)
par(mfrow = c(1, 2))
hist(res, freq = F)
qqnorm(res)

# 5)다중 공선성 검사
sqrt(vif(model)) > 2
  # Peteal.Length와 Petal.Width변수의 공선성이 의심된다.

# 6)회귀모델 재생성과 평가
formula = Sepal.Length ~ Sepal.Width + Petal.Length
model <- lm(formula = formula, data = iris)
summary(model)