## Artificial Neural Network ##################################################
# 21.11.17(수)

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("nnet")
install.packages("neuralnet")

library(nnet)
library(neuralnet)

data(iris)

## 인공신경망 모델 생성 #######################################################
# 1) 데이터 셋 생성
df <- data.frame(
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no', 'no', 'no', 'yes', 'yes', 'yes')
             )
  )
str(df)

# 2) 인공신경망 모델 생성
model_net = nnet(y ~ ., df, size = 1)

# 3) 모델 결과 변수 보기
model_net

# 4) 가중치(weights)보기
summary(model_net)

# 5) 분류모델의 적합값 보기
model_net$fitted.values

# 6) 분류모델의 예측치 생성과 분류 정확도
p <- predict(model_net, df, type = "class")
table(p,df$y)




## iris 데이터 셋을 이용한 인공신경망 모델 생성 ###############################
# 1) 데이터 생성
idx = sample(1:nrow(iris),0.7*nrow(iris))
training = iris[idx,]
testing = iris[-idx,]
nrow(training)
nrow(testing)

# 2) 인공신경망 모델(은닉층 1개와 은닉층 3개) 생성
model_net_iris1 = nnet(Species ~ ., training, size = 1)
model_net_iris1
model_net_iris3 = nnet(Species ~ ., training, size = 3)
model_net_iris3

# 3) 가중치 네트워크 보기 - 은닉층 1개 신경망 모델
summary(model_net_iris1)

# 4) 가중치 네트워크 보기 - 은닉층 3개 신경망 모델
summary(model_net_iris3)

# 5) 분류모델 평가
table(predict(model_net_iris1, testing, type = 'class'),testing$Species)
table(predict(model_net_iris3, testing, type = 'class'),testing$Species)




## neuralnet패키지를 이용한 인공신경망 모델 생성 ##############################
# 1) 데이터 셋 생성
idx = sample(1:nrow(iris),0.7*nrow(iris))
training_iris = iris[idx,]
testing_iris = iris[-idx,]
dim(training_iris)
dim(testing_iris)

# 2) 수치형으로 컬럼 생성
training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3

training_iris$Species <- NULL
head(training_iris)

testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <-3

testing_iris$Species <- NULL
head(testing_iris)

# 3) 데이터 정규화
normal <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }

# 4) 정규화 함수를 이용하여 학습데이터 검정데이터 정규화
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)

testing_nor <- as.data.frame(lapply(testing_iris, normal))
summary(testing_nor)

# 5) 인공신경망 모델 생성 - 은닉 노드 1개
model_net = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                        Petal.Length + Petal.Width,
                      data = training_nor, hidden = 1)
model_net
plot(model_net)

# 6) 분류모델 성능 평가
# 6-1) 모델의 예측치 생성 - compute()함수 이용
model_result <- compute(model_net,testing_nor[c(1:4)])
model_result$net.result

# 6-2) 상관관계 분석 - 상관계수로 두 변수 간 선형관계의 강도 측정
cor(model_result$net.result, testing_nor$Species2)

# 7) 분류모델 성능 향상 - 은닉층 노드 2개 지정, backprop속성 적용
# 7-1) 인공신경망 모델 생성
model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                         Petal.Length + Petal.Width,
                       data = training_nor, hidden = 2,
                       algorithm = "backprop", learningrate = 0.01)

# 7-2) 분류모델 예측치 생성과 평가
model_result <- compute(model_net, testing_nor[c(1:4)])
cor(model_result$net.result, testing_nor$Species2)



