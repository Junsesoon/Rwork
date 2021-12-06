## R Basic ####################################################################
# 2021.09.24(금)

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/rwork")


## R 개요 #####################################################################
# 콘솔 출력
a=5
b<-6
print(a)
print(b)

# R 패키지 보기
dim(available.packages())
a <- available.packages()
head(a)

# R 패키지 목록 보기
available.packages()

# R 세션 보기
sessionInfo()

# 패키지 설치
install.packages("stringr")
# 패키지 확인
installed.packages()
# 패키지 로드
library(stringr)
search() # 현재 load된 패키지 확인

require(stringr)

# 패키지 제거
remove.packages("stringr")

# 기본 데이터 셋 보기
data()

# 히스토그램 그리기
# 빈도수기준 히스토그램
hist(Nile)
# 밀도기준 히스토그램
hist(Nile, freq = F)
# 위 결과에 분포 곡선 추가
lines(density(Nile))

# working directory 설정
getwd()
setwd('C:/Rwork/')
getwd()




## 변수와 자료형 ##############################################################
# 변수 사용 방법
var1 <- 0
var1
var1 <- 1
var1
var2 <- 2
var2
var3 <- 3
var3

# '변수.멤버' 형식의 변수 선언
goods.code <- 'a001'
goods.name <- '냉장고'
goods.price <- 850000
goods.des <- '최고 사양, 동급 최고 품질'

# 벡터 변수 사용
age <- 35
names <- "홍길동"
age
names
# 벡터 변수 사용 예시
age <- "35"
names <- c("홍길동", "이순신", "유관순")
age
names

# 스칼라 변수 사용
int <- 20
int
string <- " 홍길동"
string
boolean <- T
boolean

# 주석 처리
sum(10, 20, 20)
sum(10, 20, 20, NA)
sum(10, 20, 20, NA, na.rm = T)
# max(1, 2, 3, 4, NA, na.rm = T)
# min(1, 2, 3, 4, NA, na.rm = T)
ls()

# 자료형 확인
is.character(string)

x <- is.numeric(int)
x

is.logical(boolean)
is.logical(x)
is.na(x)

# 문자 원소를 숫자 원소로 형 변환하기
x <- c(1, 2, "3")
x2 <- c(1, 2, 3)
x
x2

result <- x*3
result
result <- as.numeric(x)*3
result

# 복소수
z <- 5.3 - 3i
Re(z)
Im(z)
is.complex(z)
as.complex(5.3)

# 스칼라 변수의 자료형과 자료구조 확인
mode(int)
mode(string)
mode(boolean)

class(int)
mode(string)
mode(boolean)

# 요인형 변환
# 문자 벡터와 그래프 생성
gender <- c("man", "woman", "woman", "man", "man")
plot(gender) # error

Ngender <- as.factor(gender)
table(Ngender)

plot(Ngender)
mode(Ngender)
class(Ngender)
is.factor(Ngender)

# Factor Nominal 변수 내용 보기
Ngender

# factor() 함수를 이용한 Factor형 변환
args(factor)
Ogender <- factor(gender, levels = c("woman", "man"), ordered = T)
Ogender

# 순서가 없는 요인과 순서가 있는 요인형 변수로 차트 그리기
par(mfrow = c(1, 2))
plot(Ngender)
plot(Ogender)

# 날짜형 변환
as.Date("20/02/28", "%y/%m/%d")
class(as.Date("20/02/28", "%y/%m/%d"))
dates <- c("02/28/20", "02/30/20", "03/01/20")
as.Date(dates, "%m/%d/%y")




## Locale #####################################################################
# 시스템 로케일 정보 확인
Sys.getlocale(category = "LC_ALL")
Sys.getlocale(category = "LC_COLLATE")
Sys.time() #현재 날짜와 시간 확인

# 날짜형 변환
sdate <- " 2019-11-11 12:47:05"
sdate
class(sdate)

today <- strptime(sdate, format = "%Y-%m-%d %H:%M:%S")
today
class(today)

# Strptime()함수 이용
strptime("30-11-2019", format = {"%d-%m-%Y"})
strptime("2-1-19", format = {"%m-%d-%y"})


# 국가별 로케일 설정
Sys.setlocale(category = "LC_ALL", locale="Korean_Korea")
Sys.setlocale(category = "LC_ALL", locale="English_US")
Sys.setlocale(category = "LC_ALL", locale="Japanese_Japan")
Sys.getlocale()

# 날짜 형식을 인식 못하는 경우 예시
strptime("01-nov-19", format = "%d-%b-%y")

day <- strptime ("Tue, 19 nov 2019", format = "%a, %d %b %Y")





## 기본 함수와 작업공간 #######################################################
help("print")
?print

#실습: 함수 파라미터 확인
args(max)
max(10,20,NA,30)

#함수 사용 예제
example("sequence")
example(max)

# mean()함수 사용
x <- c(0:10, 50:60)
x
xm <- mean(x)
xm

x<- c(0:10, NA, 50)
x
xm <- mean(x)
xm

# 작업공간
# 작업공간 보기
getwd()
# 작업공간 변경
setwd("d:/R_temp")
# csv파일 쓰기
data <- read.csv("test.csv", header = T)
data

head(data, 10)
tail(data, 3)

getwd()

