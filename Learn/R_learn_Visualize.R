## BigData Analysis Visualize #################################################
# 21.11.19(월)

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/rwork")

  # 라이브러리 모음
install.packages("scatterplot3d")

library(scatterplot3d)


## 막대차트 시각화 ############################################################
# 자료 생성
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2018 1분기","2019 1분기","2018 2분기",
                       "2019 2분기","2018 3분기","2019 3분기",
                       "2018 4분기","2019 4분기")
str(chart_data)
chart_data

# 세로 막대 차트 그리기
barplot(chart_data, ylim = c(0, 600),
        ylab = "매출액 단위 : 만원",  # 레이블 추가
        xlab = "년도별 분기 현황",
        col = rainbow(8),  # 색상 지정
        main = "2018 년도 vs 2019 년도 매출현항 비교")

# 가로 막대 차트 그리기
barplot(chart_data, xlim = c(0, 600), horiz = T,
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황",
        col = rainbow(8), space = 1, cex.names = 0.8,  # 막대 사이 간격 조정
        main = "2018 년도 vs 2019 년도 매출현항 비교")
col = rep(c("red", "green"), 4)  # 막대 색상 지정


# 누적 막대 차트
# 데이터 불러오기
data("VADeaths")
VADeaths

# 개별 차트와 누적 차트 그리기
par(mfrow = c(1, 2))
barplot(VADeaths, beside = T, col = rainbow(5),
        main = "미국 버지니아주 하위계층 사망비율")
legend(19, 71,c("50-54","55-59","60-64","65-69","70-74"),
       cex = 0.8, fill = rainbow(5))

barplot(VADeaths, beside = F, col = rainbow(5))
title(main="미국 버지니아주 하위계층 사망비율",font.main=4)
legend(3.8, 200, c("50-54","55-59","60-64","65-69","70-74"),
       cex = 0.8, fill = rainbow(5))




## 점 차트 ####################################################################
par(mfrow = c(1, 1))
dotchart(chart_data,color=c("blue","red"),
         lcolor = "black", pch = 1:2,
         labels = names(chart_data),
         xlab = "매출액",
         main = "분기별 판매현황 : 점차트 시각화",
         cex = 1.2)




## 원형 차트 ##################################################################
par(mfrow = c(1,1))
pie(chart_data,labels=names(chart_data),col=rainbow(8),cex=1.2)
title("2018~2019년도 분기별 매출현황")




## 상자 그래프 ################################################################
# 1) “notch = FALSE일 때
boxplot(VADeaths, range = 0)

# 2) “notch = TRUE일 때
boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 3, col = "red")

# 3) VADeath 데이터 셋의 요약통계량 보기
summary(VADeaths)




## 히스토그램 #################################################################
hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "magenta",
     main = "iris 꽃 받침 길이 Histogram", xlim = c(4.3, 7.9))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose",
     main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))

# 1) 빈도수에 의해서 히스토그램 그리기
par(mfrow = c(1, 2))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width",
     col = "green",
     main = "iris 꽃받침 너비 Histogram: 빈도수 ", xlim = c(2.0,4.5))

# 2) 확률 밀도에 의해서 히스토그램 그리기
hist(iris$Sepal.Width, xlab = "iris.$Sepal.Width",
     col = "mistyrose", freq = F,
     main = "iris 꽃받침 너비 Histogram: 확률 밀도 ", xlim = c(2.0,4.5))

# 3) 밀도를 기준으로 line 추가하기
lines(density(iris$Sepal.Width), col = "red")


# 정규분포 추정 곡선
# 1) 계급을 밀도로 표현한 히스토그램
par(mfrow = c(1, 1))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose",
     freq = F, main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))

# 2) 히스토그램에 밀도를 기준으로 분포곡선 추가
lines(density(iris$Sepal.Width), col = "red")

# 3) 히스토그램에 정규분포 추정 곡선 추가
x <- seq(2.0, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width),
            sd = sd(iris$Sepal.Width)),
      col = "blue", add = T)




## 산점도 #################
# 1) 기본 산점도
price <- runif(10, min = 1, max = 100)
plot(price, col = "red")

# 2) 대각선 추가
par(new = T)
line_chart = 1:100
plot(line_chart, type = "l", col = "red", axes = F, ann = F)

# 3) 텍스트 추가
text(70, 80, "대각선 추가 ", col = "blue")

par(mfrow = c(2, 2))
plot(price, type = "l")
plot(price, type = "o")
plot(price, type = "h")
plot(price, type = "s")


# pch 속성으로 산점도 구리기
# 1) pch 속성과 col, ces 속성 사용
par(mfrow =
      c(2, 2))
plot(price, type = "o", pch = 5)
plot(price, type = "o", pch = 15)
plot(price, type = "o", pch = 20, col = "blue")
plot(price, type = "o", pch = 20, col = "orange", cex = 1.5)
plot(price, type = "o", pch = 20, col = "green", cex = 2.0, lwd = 3)

par(mfrow=c(1,1))
plot(price, type="o", pch=20,
     col = "green", cex=2.0, lwd=3)

# plot()함수의 시각화 도구 모음
methods("plot")




## 중첩 자료 시각화 ###########################################################
# 중첩된 자료의 수 만큼 점의 크기 확대하기
# 1) 두 개의 벡터 준비
x <- c(1, 2, 3, 4, 2, 4)
y <- rep( 2, 6)
x; y

# 2) 교차테이블 작성
table(x, y)

# 3) 산점도 시각화
plot(x, y)

# 4) 교차테이블로 데이터프레임 생성
xy.df <- as.data.frame(table(x, y))
xy.df

# 5) 좌표에 중복된 수 만큼 점을 확대
plot(x, y,
     pch = "@", col = "blue", cex = 0.5 * xy.df$Freq,
     xlab = "x 벡터의 원소 ", ylab = "y 벡터 원소")




## 변수간의 비교 시각화 #######################################################
attributes(iris)
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])




## 3차원 산점도 ###############################################################
# 1) 꽃의 종류별 분류
iris_setosa = iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ]
iris_virginica = iris[iris$Species == 'virginica', ]

# 2) 3차원 틀(Frame) 생성하기
d3 <- scatterplot3d(iris$Petal.Length,
                    iris$Sepal.Length,
                    iris$Sepal.Width,
                    type = 'n')

# 3) 3 차원 산점도 시각화
d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width,
            bg = 'orange', pch = 21)

d3$points3d(iris_versicolor$Petal.Length,
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue', pch = 23)

d3$points3d(iris_virginica$Petal.Length,
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width,
            bg = 'green', pch = 25)
