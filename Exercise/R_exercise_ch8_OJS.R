## R_exercise_ch8 ##############################################################
# 21.12.01(수)

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("ggmap") #지도 시각화
install.packages("carat")
install.packages("lattice")
install.packages("latticeExtra")

library(ggmap)
library(carat)
library(lattice)
library(latticeExtra)

data(diamonds)
data("SeatacWeather")
data("quakes")


## 1번문제 ####################################################################
# 서울 지역에 있는 주요대학교의 위치 정보를 이용하여 레이아웃 기법으로
# 다음과 같이 시각화하시오.

# 1) 지도 중심지역 Seoul, zoom=11, maptype = ‘watercolor’
seoul <- c(left = 126.77, bottom = 37.40,
           right = 127.17, top = 37.70)
map <- get_stamenmap(seoul, zoom = 12, maptype = 'watercolor')
ggmap(map)

# 2) 데이터 셋(“C:/Rwork/university.csv”)
university.data <- read.csv("c:/Rwork/university.csv",header = T)
university.data

# 3) 지도좌표: 위도(LAT), 경도(LON)
schoolnames <- university.data$학교명
LAT <- university.data$LAT
LON <- university.data$LON

# 4) 학교명을 이용하여 포인터의 크기와 텍스트 표시
result <- ggmap(map) + geom_point(data = university.data,
                              aes(x = LON, y = LAT,
                                  color = factor(schoolnames)))
result

# 5) 파일명을 “university.png” 로 하여 이미지 파일로 결과 저장
#이미지의 가로/세로 픽셀 크기(width = 10.24, height=7.68)
# 5-2) 변수에 저장된 그래프를 이미지 파일로 저장하기
ggsave(file = "C:/Rwork/university.png",
       plot = result, width = 10.24, height = 7.68)




## 2번문제 ####################################################################
# diamonds 데이터 셋을 대상으로 x축에 carat변수, y축에 price 변수를 지정하고,
# clarity변수를 선 색으로 지정하여 미적 요소 맵핑 객체를 생성한 후 산점도 그래프
# 주변에 부드러운 곡선이 추가되도록 레이아웃을 추가하시오.
qplot(carat,price, data = diamonds,colour = clarity,fill = cut,
      geom = "point") + geom_smooth(method = "loess")




## 3번문제 ####################################################################
# latticeExtra패키지에서 제공하는 SeatacWeather데이터 셋에서 월별로 최저기온과
# 최고기온을 선 그래프로 플로팅 하시오
# (힌트. Lattice패키지의 xyplot()함수 이용. 선그래프: type=”l”)
SW <- (SeatacWeather)

xyplot(max.temp + min.temp ~ month,data = SW,
       groups = month,type = 'l',
       auto.key = list(space = "right",points = T, lines = T))




## 4번문제 ####################################################################
# 다음 조건에 맞게 quakes 데이터 셋의 수심(depth)과 리히터 규모(mag)가 동일한
# 패널에 지진의 발생지를 산점도로 시각화하시오.
# 1) 수심(depth)을 3개 영역으로 범주화
depthgroup <- equal.count(quakes$depth, number = 3, overlap = 0)
depthgroup

# 2) 리히터 규모(mag)를 2개 영역으로 범주화
xyplot(lat ~ long | depthgroup, data = quakes,
       main = "Fiji Earthquakes(depthgroup)",
       ylab = "latitude", xlab = "longitude",
       pch = "@", col = "red")

# 3) 수심과 리히터 규모를 3행2열 구조의 패널로 산점도 그래프 그리기
# (힌트. Lattice 패키지의 equal.count()와 xyplot()함수 이용)
xyplot(lat ~ long | depthgroup * magnitudegroup, data = quakes,layout = c(3,2))



