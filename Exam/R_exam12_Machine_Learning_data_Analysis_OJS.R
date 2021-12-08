## Exam12 Machine Learning data Analysis ######################################
# 21.11.22(월) 오준서

  # 환경설정
rm(list = ls())

  # 라이브러리 모음
rm(list=ls())
install.packages("rpart") #의사결정트리: rpart
install.packages("party") #의사결정트리: ctree
install.packages("cluster") #계층형 군집 분석

library(rpart)
library(rpart.plot) #의사결정트리 시각화
library(party)
library(cluster)

data("iris")


# 2번문제 #####################################################################
# iris 데이터를 이용하여 CART 기법 적용(rpart()함수 이용)하여 분류분석 하시오.
# 1) 데이터 가져오기 &샘플링
iris_cart <- iris

# 2) 분류모델 생성
model_2 <- rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris_cart)

# 3) 테스트 데이터를 이용하여 분류
idx2 <- sample(1:nrow(iris_cart),nrow(iris_cart)*0.7)
train_2 <- iris_cart[idx2,]
test_2 <- iris_cart[-idx2,]

# 4) 예측정확도
pred_2 <- predict(model_2,newdata=iris,type = "class")

sum(pred_2 == iris$Species)/NROW(pred_2)

table(iris$Species,pred_2)
146/150*100




# 3번문제 #####################################################################
# iris 데이터를 이용하여 조건부 추론나무 적용(ctree()함수 이용)하여 분류분석 하시오.
# 1) 데이터 가져오기 & 샘플링
iris_party <- iris
idx_3 <- sample(2, nrow(iris_party), replace=TRUE, prob=c(0.7,0.3))
train_3 <- iris[idx_3==1,]
test_3 <- iris[idx_3==2,]

# 2) 분류모델 생성
model_3 <- ctree(Species~.,data=train_3)

# 3) 테스트 데이터를 이용하여 분류
result_3 <- predict(model_3,data=test_3)
# table(predict(model_3),train_3$Species)

# 4) 시각화
plot(model_3)




# 4번문제 #####################################################################
# 아래 문제를 R code로 작성하여 제출하시오.
# iris데이터 셋의 1~4번 변수를 대상으로 유클리드 거리 매트릭스를 구하여 idist에
# 저장한 후 계층적 클러스터링을 적용하여 결과를 시각화하시오.
# 1) 유클리드 거리 계산
iris_eucli <- iris[,-5]
dist_4 <- dist(iris_eucli,method = "euclidean")
head(dist_4)

# 2) 계층형 군집 분석(클러스터링)
iris_hc <- hclust(dist_4)

# 3) 분류결과를 대상으로 음수값을 제거하여 덴드로그램 시각화
plot(iris_hc,hang=-1)

# 4) 그룹 수를 3개로 지정하여 그룹별로 테두리 표시
rect.hclust(iris_hc, k=3,border="red")



