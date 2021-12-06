## DataType & Structure1 ######################################################
# 21.09.27(월)

  # 환경설정
rm(list=ls())


## 벡터 객체 ##################################################################
# 벡터 객체 생성
# c()함수
a <- c(1:100)
a
1:20:2
c(1,2,3,4,5)
# seq()함수
seq(1,20,2)
# rep()함수
rep(1:3, 2)
rep(1:3, each=3)


# 벡터 자료 처리
# union(), setdiff(), intersect()함수를 이용한 벡터 자료 처리
x <- c(1, 3, 5, 7)
y <- c(3, 5)
union(x,y)
setdiff(x,y)
intersect(x,y)
find.package(package = "stringr")


# 숫자, 문자, 논리형 벡터 생성
v1 <- c(33, -5, 20:23, 12, -2:3)
v2 <- c("홍길동", "이순신", "유관순")
v3 <- c(T, TRUE, FALSE, T, TRUE, F, T)
v1; v2; v3

v4 <- c(33, 05, 20:23, 12, "4")
v4


# 세미콜론(;)의 활용
v1; mode(v1); class(v1)
v2; mode(v2); class(v2)
v3; mode(v3); class(v3)




## 칼럼과 인덱스 ##############################################################
# 칼럼명 지정과 NULL의 활용
age <- c(30, 35, 40)
age
names(age) <- c("홍길동", "이순신", "강감찬")
age
age <- NULL


# 벡터참조
a <- c(1:50)
a[10:45]
a[19:(length(a)-5)]
a[19:45]
a[1,2] #잘못된 index사용, 콤마는 2차원 이상 다차원 배열 시 사용

v1 <- c(13, -5, 20:23, 12, -2:3) #v1 <- c(13, -5, 20, 21, 22, 23, 12, -2, -1, 0, 1, 2, 3)와 같음
v1[1]
v1[c(2,4)]
v1[c(3:5)] #v1[c(3,4,5)]
v1[c(4, 5:8, 7)] #v1[c(4, 5, 6, 7, 8, 7)]


# 음수 index
v1
v1[-1]
v1
v1[-c(2,4)]
v1[-c(2, 5:10, 1)] #v1[3, 4, 11, 12, 13] 20, 21, 1, 2, 3




## 패키지 설치와 Matrix #######################################################
install.packages("RSADBE")
library(RSADBE)
data("Severity_Counts") #RSADBE 패키지에서 제공되는 데이터셋 로딩
str(Severity_Counts) #데이터 셋 구조 보기
head(Severity_Counts)

# Matrix 자료구조
m <- matrix(c(1:5))
m

m <- matrix(c(1:10), nrow =2)
m
# 행과 열의 수가 불일치
m <- matrix(c(1:11), nrow =2)
m

# 행 우선으로 행렬 생성
m <- matrix(c(1:10), nrow =2, byrow = T)
m

# rbind 행 추가
x1 <- c(m, 40, 50:52)
x2 <- c(30, 5, 6:8)
mr <- rbind(x1, x2)
mr

# 에러제거를 위한 m과 숫자 제거
x1 <- c(40, 50:52)
x2 <- c(30, 6:8)
mr <- rbind(x1, x2)
mr

# cbind() 열 추가
mc <- cbind(x1, x2)
mc

mc2 <- cbind(x2, x1)
mc2

# matrix() 행 추가 (2행으로 행렬 생성)
m3 <- matrix(10:19,nrow=2)
m4 <- matrix(10:20,2)
m3
m4
mode(m3)
class(m3)

# index이용 행렬 접근
m3
m3[1,]
m3[,5]
m3[2,3]
m3[1,c(2:5)]




## 사용자 정의 함수와 컬럼명 지정 #############################################
# 3행 3열 행렬 생성
x <- matrix(c(1:9), nrow =3, ncol =3, byrow=T)
x

# apply()함수
apply(x, 1, max)
apply(x, 2, max)
apply(x, 1, min)
apply(x, 2, mean)
x

# 사용자 정의 함수 적용
f <- function(x){
  x*c(1,2,3)
}
result <- apply(x, 1, f)
result
x

# 행렬에 이름 지정하기
colnames(x) <- c("one", "two", "three")
x
rownames(x) <- c("row one", "row two", "row three")
x



## Array, Matrix, DataFrame ####
# 배열 객체 생성
vec <- c(1:12)
arr <- array(vec, c(3,2,2))
arr


# 배열 객체의 자료 조회
arr[,,1]
arr[,,2]
arr[,2,]
mode(arr); class(arr)


# 데이터 셋 가져오기
library(RSADBE)
data("Bug_Metrics_Software")
# 데이터 셋 구조
str(Bug_Metrics_Software)
# 데이터 셋 자료 보기
head(Bug_Metrics_Software, 3)


# data.frame
no <- c(1,2,3)
name <- c("hong", "lee", "kim")
pay <- c(150,250,300)
vemp <- data.frame(No = no, Name = name, Pay = pay)
vemp ##문자, 숫자가 혼용된 표는 데이터프레임만 가능하다


# matrix 이용 data.frame 생성
m <- matrix(
  c(1, "hong", 150,
    2, "lee", 250,
    3, "kim", 300),3,by=T)
m #숫자와 문자가 섞여있기 때문에 매트릭스에서는 숫자를 문자로 바꿔버림
memp <- data.frame(m)
memp #숫자와 문자를 구분하여 출력하는 결과를 보여줌


# text파일을 이용하여 data.frame 생성
getwd()
setwd("c:/Rwork/data/dataset1/dataset1")
txtemp <- read.table('emp.txt', header = 1, sep = "")
txtemp

csvtemp <- read.csv('emp.csv', header = T)
csvtemp
help(read.csv)

read.csv('emp2,csv', header = F)
name <- c("사번", "이름", "급여")
read.csv('emp2.csv', header = F, col.names =name) 
#col.names와 colnames의 차이는 함수와 옵션의 차이


# data.frame 만들기
df <- data.frame(x=c(1:5), y=seq(2,10,2),z=c('a', 'b', 'c', 'd', 'e'))
df

# data.frame 컬럼명 참조
df$x
df$z

str(df) #3variables는 3개의 변수이고 컬럼이 변수이므로
ncol(df) # <<값은 3이 나옴
nrow(df) #5obs = 5개의 관측치 즉 5개의 row가 나옴
names(df) #각 컬럼명을 표기
df[c(2:3), 1] #2,3번째 행의 1번째 열을 표기

# data.frame의 부분객체
df
apply(df[,c(1,2)], 2, sum)

# data.frame의 부분객체
x
x1 <- subset(df, x >= 3)
x1

y1 <- subset(df, y <= 8)
y1

df
xyand <- subset(df, x<=2 & y<=6)
xyand
xyor <- subset(df, x<=2 | y<=0)
xyor


# student data.frame 만들기
sid <- c("A", "B", "C", "D")
score <- c(90,80,70,60)
subject <- c("컴퓨터", "국어국문", "소프트웨어", "유아교육")
student <- data.frame(sid, score, subject)
student
# 자료형과 자료구조 보기
mode(student); class(student)
str(sid)
str(score)
str(subject)
str(student)

# data.frame 병합
height <- data.frame(id = c(1,2), h = c(180,175))
height
weight <- data.frame(id = c(1,2), w = c(80,75))
weight

user <- merge(height, weight, by.x = "id", by.y = "id")
user


## data.frame 객체 데이터 셋 ##################################################
# galton 데이터 셋 다운
install.packages("UsingR")
library(UsingR)
data(galton) #RSADBE 데이터 불러올 땐 따옴표가 있었는데 이건 랜덤임

str(galton)
dim(galton)
head(galton, 15)
