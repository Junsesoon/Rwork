## exam10 Exploratory data analysis ###########################################
# 21.11.08(월) 오준서

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("dplyr")
install.packages("reshape2")

library(dplyr)
library(reshape2)
library(plyr)

data(iris)


## 2번문제 ####################################################################
# 1) dplyr패키지와 iris 데이터 셋을 대상으로 아래의 문제를 실행하는 R코드를 작성하여 제출하시오
# (1) iris의 꽃받침의 폭(Sepal.Width)이 3.7 이상의 값만 필터링하여 화면출력하시오.
result2_1 <- iris %>% filter(iris$Sepal.Length >= 3.7)
head(result2_1)

# (2) (1)의 결과에서 2, 4, 5번째 컬럼을 선택하시오
result2_2 <- result2_1 %>% select(,2,4,5)
head(result2_2)

# (3) (2)의 결과에서 2번 컬럼의 값에서 4번 컬럼의 값을 뺀 diff파생변수를 만들고,
# 앞부분 10개만 출력하시오
result2_2$diff <- result2_2$Sepal.Width-result2_2$Petal.Width
head(result2_2$diff,10)

# (4) (3)의 결과에서 꽃의 종(Species)별로 그룹화하여 Sepal.Width와
# Petal.Width 변수의 평균을 계산하시오.
result2_4 <- melt(result2_2)
result <- dcast(result2_4, Species ~ variable, mean)

# (5) (3)의 결과에서 위에서 4번째 꽃의 종(Species)는 무엇인가?
result2_2$Species[4]




## 3번문제 ####################################################################
# “user_data.csv”와 “return_data.csv”파일을 이용하여 고객별 반품사유코드
# (return_code)를 대상으로 다음과 같이 단계별로 실행하여 파생변수를 추가하시오

# 반품사유코드에 대한 파생변수 컬럼명 설명:
# 제품이상(1) <- return_code1
# 원인불명(2) <- return_code2
# 변심(3) <- return_code3
# 기타(4) <- return)_code4
rm(list=ls())
getwd()
setwd('C:/Rwork/')

# 1) 고객 정보 파일 가져오기
customer_data <- read.csv('user_data.csv',header=T)

# 2) 반품 정보 파일 가져오기
return <- read.csv('return_data.csv',header = T)

# 3) 고객별 반품사유코드에 따른 파생변수(customer_return) 생성하기
return$customer_return[return$return_code == 1] <- '제품이상(1)'
return$customer_return[return$return_code == 2] <- '원인불명(2)'
return$customer_return[return$return_code == 3] <- '변심(3)'
return$customer_return[return$return_code == 4] <- '기타(4)'

# 4) 고객정보(customer_data)에 파생변수 (customer_return, 반품사유 컬럼)를 추가하여
# 고객반품정보(customer_return_data)를 만들고 맨 앞 6개 데이터 화면 출력하기
customer_return_data <- join(customer_data,return,by='user_id')
head(customer_return_data,6)

# 5) 고객반품정보(customer_return_data) 테이블에서 맨 밑에서 10개를 화면 출력하기
tail(customer_return_data,10)



