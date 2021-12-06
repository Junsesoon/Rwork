## Ensemble ###################################################################
# 21.11.18(목)

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("party") #배깅
install.packages("caret") #배깅
install.packages("adabag") #부스팅
install.packages("ada")
install.packages("randomForest") #랜덤포레스트트

library(party)
library(caret)
library(adabag)
library(ada)
library(randomForest)


## 배깅 #######################################################################
# 1) 데이터 샘플링
data1 <- iris[sample(1:nrow(iris), replace=T),]
data2 <- iris[sample(1:nrow(iris), replace=T),]
data3 <- iris[sample(1:nrow(iris), replace=T),]
data4 <- iris[sample(1:nrow(iris), replace=T),]
data5 <- iris[sample(1:nrow(iris), replace=T),]

# 2) 예측모형 생성
citree1 <- ctree(Species~., data1)
citree2 <- ctree(Species~., data2)
citree3 <- ctree(Species~., data3)
citree4 <- ctree(Species~., data4)
citree5 <- ctree(Species~., data5)

# 3) 예측수행
predicted1 <- predict(citree1, iris)
predicted2 <- predict(citree2, iris)
predicted3 <- predict(citree3, iris)
predicted4 <- predict(citree4, iris)
predicted5 <- predict(citree5, iris)

# 4) 예측모형 결합하여 새로운 예측모형 생성
newmodel <- data.frame(Species=iris$Species,
                       predicted1,predicted2,predicted3,predicted4,predicted5)
head(newmodel)
newmodel

# 5) 최종모형으로 통합
funcValue <- function(x) {
  result <- NULL
  for(i in 1:nrow(x)){
    xtab <- table(t(x[i,]))
    rvalue <- names(sort(xtab, decreasing = T) [1])
    result <- c(result,rvalue)
  }
  return (result)
}

# 6) 최종 모형의 2 번째에서 6 번째를 통합하여 최종 결과 생성
newmodel$result <- funcValue(newmodel[, 2:6])
newmodel$result

# 7) 최종결과 비교
table(newmodel$result, newmodel$Species)



# adabag 패키지 이용
iris.bagging <- bagging(Species~., data=iris, mfinal=10)
# mfinal=반복수 또는 트리의 수 (디폴트 = 100)
iris.bagging$importance #변수의 상대적인 중요도
# 변수의 중요도는 각 tree 에서 변수에 의해 주어지는
# 지니지수의 이익 (gain, 불확실성이 감소량 을 고려한 측도
windows()
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])
#
pred <- predict(iris.bagging, newdata=iris)
pred
table(pred$class, iris[,5])




## 부스팅 #####################################################################
# 1) boosting()함수를 이용한 부스팅
# 1-1) 데이터 셋 준비
data(iris)
boo.adabag <- boosting(Species ~., data=iris, boos=TRUE, mfinal=10)
boo.adabag$importance

# 1-2) 시각화
plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

# 1-3) 예측치
pred <- predict(boo.adabag, newdata=iris)
tb <- table(pred$class, iris[,5])
tb

# 1-4) 오분류율 계산
error.rpart <- 1-(sum(diag(tb))/sum(tb))
error.rpart



# 2) nnet()함수를 이용한 신경망분석 부스팅
# 2-1) 데이터 셋 준비(setosa 제외)
iris[iris$Species!="setosa",] -> iris
n <- dim(iris)[1]

# 2-2) 데이터 샘플링
trind <- sample(1:n, floor(.6*n), FALSE)
teind <- setdiff(1:n, trind) # set difference(차집합)
iris[,5] <- as.factor((levels(iris[,5])[2:3])[as.numeric(iris[,5])-1])
# as.numeric부분: 0,1,2가 차례대로 50개(총 150개)

# 2-3) 훈련용 데이터를 이용하여 부스팅 방법으로 모형 구축
gdis <- ada(Species~., data=iris[trind,], iter=20, nu=1, type="discrete")
# nu=1(디폴트)은 부스팅을 위한 축소(shrinkage) 모수
# type=“discrete”(디폴트)은 부스팅 알고리즘 지정. “real”, “gentle” 부스팅
gdis <- addtest(gdis, iris[teind, -5], iris[teind, 5])

# 2-4) 분류 실시
gdis

# 2-5) 시각화
plot(gdis, TRUE, TRUE) # 오차와 일치도를 나타내는 카파(kappa)계수를 그려준다.
  # 2개의 TRUE 옵션은 훈련용, 검증용 자료 모두에 대해 그림을 그려준다
varplot(gdis)
pairs(gdis, iris[trind,-5],maxvar=4)




## randomForest ###############################################################
# iris데이터를 이용한 랜덤포레스트
head(iris)

# 1) 데이터 샘플링
  # 70% training데이터, 30% testing 데이터로 구분
idx <- sample(2, nrow(iris), replace=T, prob=c(0.7, 0.3))
trData <- iris[idx == 1, ]
nrow(trData)
teData <- iris[idx == 2, ]
nrow(teData)

# 2) 랜덤포레스트 실행 (100 개의 tree 를 다양한 방법 (proximity= 으로 생성)
RFmodel <- randomForest(Species~., data=trData, ntree=100, proximity=T)
RFmodel
  # proximity=TRUE는 개체들 간의 근접도 행렬을 제공: 동일한 최종노드에 
  # 포함되는 빈도에 기초함

# 3) 시각화
plot(RFmodel, main="RandomForest Model of iris")

# 4) 모델에 사용된 변수 중 중요한 것 확인
importance(RFmodel)

# 5) 중요한 것 시각화
varImpPlot(RFmodel)

# 6) 실제값과 예측값 비교
table(trData$Species, predict(RFmodel))

# 7) 테스트 데이터로 예측
pred <- predict(RFmodel, newdata=teData)

# 8) 실제값과 예측값 비교
table(teData$Species, pred)

# 9) 시각화
plot(margin(RFmodel, teData$Species))
  #그래프에서 모델 오류가 안정적인 상태를 보이기 시작하는 시점의 tree 개수로 실행

