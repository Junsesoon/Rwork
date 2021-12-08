## R_ch4 Control Statement & Function #########################################
# 21.10.01(금)

  # 환경설정
rm(list = ls())
setwd("c:/Rwork/data/dataset1")
getwd()


## 산술, 관계, 논리 연산자 ####################################################
# 산술연산자 ####
# 사칙연산(+, --, *, 나머지 계산 연산자 거듭제곱 ((^ 또는 계산 연산자
num1 <- 100
num2 <- 20
result <- num1 + num2
result
result <- num1 num2
result
result <- num1 * num2
result
result <- num1 / num2
result
result <- num1 %% num2
result
result <- num1 ^ 2
result
result <- num1 ^ num2
result




# 관계연산자 (==, !=, >, >=, <,<=) ####
# 관계식의 결과가 참이면 TRUE, 거짓이면 FALSE 값을 반환하는 연산자
boolean <- num1 == num2
boolean
boolean <- num1 != num2
boolean
boolean <- num1 > num2
boolean
boolean <- num1 >= num2
boolean
boolean <- num1 < num2
boolean
boolean <- num1 <= num2
boolean




# 논리연산자 ####
# TRUE / FALSE 값 반환
logical <- num1 >= 50 & num2 <= 10
logical
logical <- num1 >= 50 | num2 <= 10
logical
logical <- num1 >= 50
logical
logical <= !(num1 >= 50)
logical
x <- TRUE; y <- FALSE
xor(x, y)




## 조건문 #####################################################################
# if 함수 ####
x <- 50; y <- 4; z <- x * y
if(x * y >= 40) {
  cat("x * y 의 결과는 40 이상입니다 \n")
  cat("x * y = ", z)
} else {
 cat("x * y 의 결과는 40 미만입니다.x * y = ",z, "\n")
}


# if() 함수 이용 점수의 학점 산출
score <- scan()
score
result <- "노력"
if(score >= 80) {
  if(score >= 80) {
    result <- "우수"
  }
}
cat("당신의 학점은 ", result, score)


# if~else if 형식으로 학점 산출
score <- scan()
if(score >= 90) {
  result = "A 학점"
} else if(score >= 80) {
result = "B 학점"
} else if(score >= 70) {
  result = "C 학점"
} else if(score >= 60) {
result = "D 학점"
} else {
  result = "F 학점"
}
cat("당신의 학점은 ", result)
print(result)


# ifelse() 함수 사용
score <- scan()
ifelse(score >= 80, "우수 ", "노력")
ifelse(score <= 80, "우수 ", "노력")


#+ 응용
excel <- read.csv("C:/Rwork/Part I/excel.csv", header = T)
q1 <- excel$q1
q1
ifelse(q1 >= 3, sqrt(q1), q1)


# ifelse() 함수에서 논리연산자 사용
ifelse(q1 >= 2 & q1 <= 4, q1 ^ 2, q1)




# switch() 함수 ####
# 형식: switch(비교문, 실행문1 [, 실행문2 , 실행문3, …])
switch("name", id = "hong", pwd = "1234", age = 105, name = "홍길동")

# 사원명으로 급여정보 보기
empname <- scan(what = "")
empname
switch(empname,
       hong = 250,
       lee = 350,
       kim = 200,
       kang = 400
)




# which() 함수 ####
# 벡터에서 which() 함수 사용하여 index 값 반환
name <- c("kim", "lee", "choi", "park")
which(name == "choi")


# 1) 데이터프레임 생성
no <- c(1:5)
name <- c("홍길동", "이순신", "강감찬", "유관순", "김유신")
score <- c(85, 78, 89, 90, 74)
exam <-data.frame(학번 = no, 이름 = name, 성적 = score)
exam

# 2) 일치하는 이름의 인덱스 반환
which(exam$이름 == "유관순")
exam[4,]




## 반복문 #####################################################################
# for() 함수 ####
# 반복할 문장이 하나 뿐일 때는 { } 생략 가능
i <- c(1:10)
for(n in i) {
 print(n * 10)
 print(n)
}

# 짝수 값만 출력
i <- c(1:10)
for(n in i)
  if(n %% 2 == 0) print(n)

# 홀수 값만 출력
i <- c(1:10)
for(n in i) {
  if(n %% 2 == 0) {
    next #for() 함수의 반복 범위에서 문장을 실행하지 않고 계속 반복할 때 사용
  } else
    print(n)
}

# 변수의 컬럼명 출력
name <- c(names(exam))
for(n in name) {
  print(n)
}

# 벡터 데이터 사용
score <- c(85, 95, 98)
name <- c("홍길동", "이순신", "강감찬")
i <- 1
for(s in score) {
cat(name[i], " -> ", s, "\n")
i < i + 1
}




# while() ####
# for() 함수는 반복 회수를 결정하는 변수를 사용
# While() 함수는 사용자가 블록 내에서 증감식을 이용하여 반복 회수를 지정
i = 0
while(i < 10) {
  i < i + 1
  print(i)
}




## 사용자 정의 함수 ###########################################################
# 매개변수가 없는 사용자 함수 정의 ####
f1 <- function() {
    cat(" 매개변수가 없는 함수")
}
f1()


# 결과를 반환하는 사용자 함수 정의 ####
f3 <- function(x, y) {
    add < x + y
    return(add)
}
add <- f3(10, 20)


## 기술통계량을 계산하는 함수 정의 ####
# 기본함수를 이용하여 요약통계량과 빈도수 구하기 ####
# 1) 파일 불러오기
setwd("C:/Rwork/")
test <- read.csv("test.csv", header = TRUE)
head(test)

# 2) 요약 통계량 구하기
summary(test)

# 3) 특정 변수의 빈도수 구하기
table(test$A, test$B)


# 각 칼럼 단위의 빈도수와 최대값 , 최소값 계산을 위한 사용자 함수 정의하기 ####
data_pro <- function(x) {
  for(idx in 1:length(x)) {
    cat(idx, "번째 컬럼의 빈도 분석 결과")
    print(table(x[idx]))
    cat("\n")
  }

  for(idx in 1:length(x)){
    f <- table(x[idx])
    cat(idx, "번째 컬럼의 최대값/최소값 \n")
    cat("max = ", max(f), "min = ", min(f), "\n")
  }
}
print(table(x[2]))
data_pro(test)


# 분산과 표준편차를 구하는 사용자 함수 정의 ####
x <- c(7, 5, 12, 9, 15, 6)

var_sd <- function(x) {
  var <- sum((x-mean(x))^2) / (length(x)-1)
  sd <- sqrt(var)
  cat("표본분산: ", var, "\n")
  cat("표본표준편차: ", sd)
          }
var_sd(x)




## 피타고라스와 구구단 함수 정의 ##############################################
# 피타고라스식 정의 함수 만들기 ####
pytha <- function(s, t){
  a <- s^2 - t^2
  b <- 2*s*t
  c <- s^2 + t^2
  cat("피타고라스 정리: 3개의 변수: ", a, b, c)
}
pytha(2, 1)


# 구구단 출력 함수 만들기 ####
gugu <-  function(i, j){
    for(x in i){
      cat("**", x, "단**\n")
      for(y in j){
        cat(x, "*", y, " = ", x*y, "\n")
        cat("\n")
      }
    }
}

i <- c(2:9)
j <- c(1:9)

gugu(i, j)




## 결측치 포함 자료의 평균 계산 함수 정의 #####################################
data <- c(10,20,5,4,40,7,NA,6,3,NA,2,NA)

na <- function(x){
  # 1차 NA 제거
  print(x)
  print(mean(x, na.rm = T))

  # 2차 NA 0으로 대체
  data <- ifelse(!is.na(x), x, 0)
  print(mean(data))

  # 3차 NA 평균으로 대체
  data2 <- ifelse(!is.na(x), x, round(mean(x, na.rm = T), 2))
  print(data2)
  print(mean(data2))
  }

na(data)




## 몬테카를로 시뮬레이션 함수 정의 ############################################
# 동전 앞면과 뒷면의 난수 확률분포 함수 정의 ####
coin <- function(n) {
  r <- runif(n, min=0, max=1)
  result <- numeric()
  for(i in 1:n){
    if(r[i] <= 0.5)
      result[i] <- 0
  else
    result[i] <- 1
  }
return(result)
}
coin(10)

# 몬테카를로 시뮬레이션 함수 정의
montaCoin <- function(n){
  cnt <- 0
  for (i in 1:n){
    cnt <- cnt + coin(1)
  }
  
  result <- cnt / n
  return(result)
}

montaCoin(10)
montaCoin(100)
montaCoin(1000)
montaCoin(10000)




## 주요 내장함수 ##############################################################
# 기술통계량 관련 내장함수 사용 ####
# 행/열 단위의 합계와 평균 구하기
library(RSADBE)
data("Bug_Metrics_Software")
Bug_Metrics_Software[,,1]

# 행 단위 합계와 평균
rowSums(Bug_Metrics_Software[,,1])
rowMeans(Bug_Metrics_Software[,,1])

# 열 단위 합계와 평균
colSums(Bug_Metrics_Software[,,1])
colMeans(Bug_Metrics_Software[,,1])

# 기술통계량 처리 관련 내장함수
seq(-2,2,by = .2)
vec <- 1:10
min(vec)
max(vec)
range(vec)
mean(vec)
median(vec)
sum(vec)
sd(rnorm(10))
table(vec)

# 정규분포의 난수 생성
n <- 1000
rnorm(n, mean = 0, sd = 1)
hist(rnorm(n, mean = 0, sd = 1))

# 균등분포의 난수 생성
n <- 1000
runif(n, min = 0, max = 10)
hist(runif(n, min=0, max=10))

# 이항분포의 난수 생성
n <- 20
rbinom(n, 1, prob = 0.5)
rbinom(n, 15, 0.5)
n <- 1000
rbinom(n, 5, prob = 1/6)

# 종자값으로 동일한 난수 생성
rnorm(5, mean = 0, sd = 1)
set.seed(123)
rnorm(5, mean = 0, sd = 1)

set.seed(123)
rnorm(5, mean = 0, sd = 1)

set.seed(345)
rnorm(5, mean = 0, sd = 1)




# 수학 관련 내장함수 ####
exp(1)
vec <- 1:10
prod(vec)
factorial(5)
abs(-5)
sqrt(16)
vec
cumsum(vec)
cumprod(vec)
log(10)
log10(10)




# 행렬연산 관련 내장함수 ####
x <- matrix(1:9, nrow=3, ncol=3, byrow=T)
x
y <- matrix(1:3, nrow=3)
y
ncol(x)
nrow(x)
t(x)
cbind(x, 1:3)
rbind(x, 10:12)
diag(x)
det(x)
apply(x, 1, sum)
apply(x, 2, mean)
svd(x)
eigen(x)
x %*% y

# %any% 특별 연산자 ####
# 1) %% 나머지 연산자
x <- c(1,2,3,4,5)
y <- c(5)
x %% y

# 2) %*% 연산자
x<- matrix(c(1,4,2,3), nrow=2)
x
y<- matrix(c(1, 3, 2, 4, 5, 6), nrow=2)
y
x %*% y

z <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3)
z
x %*% z #2행2열인 매트릭스와 3행3열인 매트릭스를 곱하면 오류가 발생

c(1,2,3) %*% c(4,5,6) # 벡터의 곱

# 3) %/% 연산자
x <- matrix(c(1,4,2,3), nrow=2)
x
y <- matrix(c(1,3,2,4), nrow=2)
y
x %/% y

z <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3)

x %/% z #크기가 맞지 않아 오류발생

# %in% 연산자 ####
# 벡터 내 특정 값 포함 여부 확인 연산자
x %in% y

# 집합 연산 관련 내장 함수
sum(x %in% y)

x <- c(1, 3, 5, 7, 9)
y <- c(3, 7)

union(x,y)
setequal(x,y)
intersect(x,y)
setdiff(x,y)
setdiff(y,x)
5 %in% y



