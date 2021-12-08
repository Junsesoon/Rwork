## R_exercise_ch5 ##############################################################
# 2021. 11. 30(화)
# R 산점도 연습문제 [오준서]

  # 환경설정
rm(list=ls())
getwd()
setwd('C:/Rwork/output')

  # 라이브러리 모음
install.packages("ggplot2")

library(ggplot2)  # 그래프 시각화 패키지

data("iris")
data("iris3")


## 1번문제 #####################################################################
# iris데이터 셋을 대상으로 다음 조건에 맞게 시각화 하시오

# 1) 1번 컬럼을 x 축으로 하고 3 번 컬럼을 y 축으로 한다
x <- iris[,1]
y <- iris[,3]
legend <- iris[,5]

# 2) 5번 컬럼으로 색상지정한다
iris_df <- data.frame(x,y)
iris_point <- ggplot(data = iris_df, aes(x = x, y = y, color=legend)) + geom_point()


# 3) 차트 제목을 iris 데이터 산포도로 추가한다
result <- iris_point + labs(title = "iris 데이터 산포도",
                            x = "Sepal.Length",
                            y = "Petal.Length")

# 4) 다음 조건에 맞추어 작성한 차트를 파일에 저장한다
  # - 작업 디렉토리 “C:/Rwork/output"
  # - 파일명 “iris.jpg"
  # - 크기: 폭(720픽셀), 높이(480픽셀)
path <- "c:/Rwork/output/iris.jpg" #경로 지정
jpeg(path, width = 720, height = 480) #경로 지정 부분 =파일이름, 폭, 높이 
result
dev.off() #열려 있는 모든 그래픽 장치를 종료




## 2번문제 #####################################################################
# iris3 데이터 셋을 대상으로 다음 조건에 맞게 산점도를 그리시오
# 1) iris3 데이터 셋의 컬럼명을 확인한다
colnames(iris3)
rownames(iris3)

# 2) iris3 데이터 셋의 구조를 확인한다
str(iris3)
dim(iris3)
class(iris3)
mode(iris3)
dimnames(iris3)
max(iris3[,1,])
min(iris3[,1,])
max(iris3[,2,])
min(iris3[,2,])

# 3) 꽃의 종별로 산점도 그래프를 그린다
plot(iris3[,,1], col = 'blue', xlim = c(4,8), ylim = c(0,5), main = 'iris3 산점도')
par(new = T)
plot(iris3[,,2], col = 'red', xlim = c(4,8), ylim = c(0,5))
par(new = T)
plot(iris3[,,3], col = 'green', xlim = c(4,8), ylim = c(0,5))



