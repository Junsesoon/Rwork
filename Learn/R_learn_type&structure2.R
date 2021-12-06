## DataType & Structure2 ######################################################
# 21.09.29(수)

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/Rwork/data/dataset1/dataset1")

  # 라이브러리 모음
install.packages("stringr") #문자열 핸들링 패키지

library(stringr)
library(RSADBE) #bug_Metrics_Software 데이터 셋

## Array, Matrix, DataFrame ###################################################
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




## list 자료구조 ##############################################################
# 21page부터
list <- list("lee", "이순신", 95)
list

unlist <- unlist(list)
unlist
num <- list(c(1:5), c(6, 10))
num

#실습: key와 balue형식으로 리스트 객체 생성
member <- list(name = c("홍길동", "유관순"), age = c(35, 25),
               address = c("한양", "충남"), gender = c("남자", "여자"),
               htype = c("아파트", "오피스텔") )
member

#특정 칼럼 추출
member$name
member$name[1]
member$name[3]
#객체 제거 후 값 변경
#member <- NULL
member$age[1] <- 45
member$age[2] <- 20
member$age

#key 생성
member$id <- "hong"
member

#member$age <- NULL

#pwd 추가
member$pwd <- "1234"

length(member)

mode(member)
class(member)

member

member$age[2] <- NULL #결과: 에러가 남

# 리스트 객체에 함수 적용
a <- list(c(1:5))
b <- list(c(6:10))
lapply(c(a,b), max)

sapply(c(a,b), max)
#실습: 다차원 리스트 객체 생성
multi_list <- list(c1 = list(1,2,3), c2 = list(10, 20, 30), 
                   c3 = list(100, 200, 300))

multi_list
multi_list$c1
multi_list$c2

do.call(cbind, multi_list)
do.call(rbind, multi_list)

# 문자열에서 한글, 영문, 숫자 추출하기
str_extract("홍길동35이순신45유관순25", "[1-9]{2}")
str_extract_all("홍길동35이순신45유관순25", "[1-9]{2}")

# 반복 수를 지정하여 영문자 추출하기
string <- "hongkd105leess1002you25강감찬2005"
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[a-z]{3,}")
str_extract_all(string, "[a-z]{3,5}")

str_extract(string, "hong")
str_extract(string, "25") #따옴표 없이 쓰면 타입이 안 맞아서 에러
str_extract(string, "[가-힣]{3}")
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[0-9]{4}")

# "[^제외문자열]"
# "[^제외문자열]{n}"

string

# 특정 문자열의 제외
str_extract_all(string, "[^a-z]")
str_extract_all(string, "[^a-z]{4}")
str_extract_all(string, "[^가-힣힣]{5}")
string
str_extract_all(string, "[^0-9]{3}")

# 한 개의 숫자와 단어 관련 정규표현식
jumin <- "123456-1234567"
str_extract(jumin, "[0-9]{6}-[1234][0-9]{6}") #![1234]=[1-4]
str_extract(jumin, "[0-9]{6}-[1-4][2-9][0-9]{5}")

str_extract_all(jumin, "\\d{6}-[1234]\\d{6}")

# 지정된 길이의 단어 추출
name <- "홍길동1234, 이순신5678, 강감찬101234"
str_extract_all(name, "\\w{8,}")

# 문자열 길이
string <- "hongkd105leess1002you25강감찬2005"
len <- str_length(string)
len

# 부분 문자열 만들기
string_sub <- str_sub(string, 1, len - 7)
string_sub
str_locate(string, "강감찬")

# 대,소문자 변경하기
ustr <- str_to_upper(string_sub); ustr
str_to_lower(ustr)
ustr2 <- str_to_lower(ustr)
ustr2

# 문자열 교체하기
string_rep <- str_replace(string_sub, "hongkd105", "홍길동35")
string_rep
string_rep <- str_replace(string_rep, "leess1002", " 이순신45")
string_rep <- str_replace(string_rep, "you25", " 유관순25")
string_rep

# 문자열 결합하기
string_c <- str_c(string_rep, " 강감찬55")
string_c

# 문자열 구분함수
string_sp <- str_split(string_c, ",")
string_sp

string_vec <- c("홍길동35", "이순신45", "유관순25", "강감찬55")
string_vec

string_join <- paste(string_vec, collapse = ",")
string_join
