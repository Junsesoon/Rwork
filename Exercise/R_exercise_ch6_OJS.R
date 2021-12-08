## R_exercise_ch6 ##############################################################
## 21.11.05(금) 오준서

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("reshape2")

library(reshape2)
library(dplyr)

data(iris)


## 1번문제 #####################################################################
# reshape2패키지와 iris데이터 셋을 사용하여 다음을 실행하시오.

# 1) 꽃의 종류(Species)를 기준으로 ‘넓은 형식’을 ‘긴 형식’으로 변경하기
View(iris)
flower <- iris
long <- melt(id = "Species",flower)
#View(long)
long

# 2) 꽃의 종별로 나머지 4가지 변수의 합계 구하기
flower <- dcast(long, Species ~ ... , sum)
#View(flower)
flower




## 2번문제 #####################################################################
# dplyr패키지와 iris 데이터 셋을 이용하여 다음을 실행하시오.

# 1) iris의 꽃잎의 길이(Petal.Length)컬럼을 대상으로 1.5이상의 값만 필터링하시오
one <- iris %>% filter(iris$Petal.Length >= 1.5)
#View(one);View(iris)
one

# 2) 1)번의 결과에서 1, 3, 5번 컬럼을 선택하시오
two <- one %>% select(1,3,5)
#View(two)
two

# 3) 2)번의 결과에서 1-3번 컬럼의 값을 뺀 diff 파생변수를 만들고, 앞부분 6개만 출력하시오.
three <- two %>% mutate(diff = Sepal.Length - Petal.Length)
head(three$diff, 6)
#View(three)
three

# 4) 3)번의 결과에서 꽃의 종(Species)별로 그룹화하여 
# Sepal.Length와 Petal.Length변수의 평균을 계산하시오.
four <- three %>% group_by(Species) %>% 
  summarise(Sepal_mean = mean(Sepal.Length), Petal_mean = mean(Petal.Length)) 
#View(four)
four



