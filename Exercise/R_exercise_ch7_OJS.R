## R_exercise_ch7 ##############################################################
## 21.11.04(목) 오준서 연습문제

  # 환경설정
rm(list=ls())
getwd()
setwd("C:/Rwork/")

  # 라이브러리 모음
install.packages("reshape2")
library(reshape2)
library(plyr)
library(cvTools)

dataset2 <- read.csv('dataset2.csv',header=T)


## 1번문제 #####################################################################
# 1) 본문에서 생성된 dataset2 의 직급(postion) 컬럼을 대상으로 1급 -> 5급,
# 5급 -> 1급 형식으로 역코딩하여 position2 컬럼에 추가하시오
position <- dataset2$position
position2 <- 6-position # 역순 재배열
dataset2$position <- position2
head(dataset2) # 결과 출력



# 2) 본문에서 생성된 dataset2 의 resident 컬럼을 대상으로 NA 값을 제거한 후
#    resident2 변수에 저장하시오
resident <- dataset2$resident
resident2 <- na.omit(resident)
resident2 # 15개의 결측치 발견



# 3) 본문에서 생성된 dataset2 의 gender 컬럼을 대상으로 1 -> 남자 ”, 
#    2 -> 여자로 코딩 변경하여 gender2 컬럼에 추가하고 파이차트로 결과를 확인하시오
dataset2$gender2[dataset2$gender == 1] <- '남자'
dataset2$gender2[dataset2$gender == 2] <- '여자'
chkgender2 <- table(dataset2$gender2) # 자료형 변경(문자형 -> 숫자형)
pie(chkgender2) # 차트 시각화



# 4) 본문에서 생성된 dataset2 의 age 컬럼을 대상으로
#   30 세이하 -> 1, 30~55 세 -> 2, 55 이상 -> 3 으로
#   리코딩하여 age3 컬럼에 추가한 뒤에 age, age2, age3 컬럼만 확인하시오
dataset2$age3[dataset2$age <= 30] <- 1
dataset2$age3[dataset2$age > 30 & dataset2$age <= 55] <- 2
dataset2$age3[dataset2$age > 55] <- 3
dataset2$age;dataset2$age2;dataset2$age3 # 결과 확인



# 5) 정제된 data 를 대상으로 작업 디렉터리 (“C/Rwork/") 에 파일 이름을 
#   "cleandata. csv” 으로 하여 따옴표와 행 이름을 제거하여 저장하고 저장된 파일의
#   내용을 읽어 new_data 변수에 저장하고 확인하시오.
setwd('C:/Rwork/')
write.csv(dataset2,'cleandata.csv',quote = F,row.names = F)
new_data <- read.csv('cleandata.csv',header = T)
View(new_data)



# 6) dataset#3 내 “user_data.csv”, “return_data.csv" 파일을 이용하여 고객별 
#   반품사유코드(return_code)를 대상으로 다음과 같이 파생변수를 추가하시오
rm(list=ls())
user <- read.csv('user_data.csv',header = T)
return <- read.csv('return_data.csv',header = T)

return$return_code[return$return_code == 1] <- 'return_code1'
return$return_code[return$return_code == 2] <- 'return_code2'
return$return_code[return$return_code == 3] <- 'return_code3'
return$return_code[return$return_code == 4] <- 'return_code4'

User_return <- dcast(return, user_id ~ return_code, length)
User_return

names(User_return) <- c('user_id','제품이상(1)','변심(2)','원인불명(3)','기타(4)')

User_return_data <- join(user, User_return, by = 'user_id')
User_return_data



# 7) iris 데이터를 이용하여 5겹 2회 반복하는 교차 검정 데이터를 샘플링하시오
iris_sample <- cvFolds(n = nrow(iris), K = 5, R = 2)
iris_spample



