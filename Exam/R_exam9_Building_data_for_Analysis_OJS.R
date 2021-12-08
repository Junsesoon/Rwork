## exam9 Building data for Analysis ###########################################
# 21.11.02(화) 오준서

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("MASS")

library(MASS)


## 2번문제 ####################################################################
# 아래 문제를 R code로 작성하여 제출하시오
# MASS 패키지에 있는 Animals 데이터 셋에 대해 R의 기본 함수를 이용하여
# body컬럼을 대상으로 다음의 기술통계량을 구하시오
# 1) Animals 데이터 셋 구조 보기
str(Animals)
# 2) 요약통계량
summary(Animals$body)
# 3) 평균
mean(Animals$body)
# 4) 표준편차
sd(Animals$body)
# 5) Animals 데이터 셋의 빈도수 구하기
table(Animals)




## 3번문제 ####################################################################
# 다음과 같이 데이터프레임을 구성하였다.
exam_data = data.frame(
  name = c('Anastasia','Dima','Katherine','James','Emily','Michael','Matthew','Laura','Kevin','Jonas'),
  score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
  attempts = c(1,3,2,3,2,3,1,1,2,1),
  qualify = c('yes','no','yes','no','no','yes','yes','no','no','yes')
)
# 다음을 실행하는 R code를 작성하시오

# 1) 각 이름의 국적은 다음과 같다. 각 개인의 국적을 데이터프레임에 추가하고
# 데이터 프레임을 화면에 출력하시오.
country = c('RUS','CHN','USA','USA','USA','USA','USA','USA','USA','USA')
exam_data <- data.frame(exam_data,country = country)
display_1 <- data.frame('이름'=exam_data$name, '국적'=exam_data$country)
display_1

# 2) 기존의 데이터프레임에 다음의 두 사람을 추가하고,
# 업데이트 된 데이터프레임을 화면 출력하시오.
kim <- c('kim',15,1,'yes','KOR')
lee <- c('Lee',10,3,'no','KOR')
exam_data2 <- rbind(exam_data,kim,lee);exam_data2

# 3) Qualify 항목을 제외한 데이터프레임을 화면 출력하시오
display_3 <- exam_data2[,c(1:3,5)]
display_3

# 4) Dima와 Jonas를 제외한 데이터프레임을 화면 출력하시오
display_4 <- exam_data2[c(1,3:9,11:12),]
display_4

# 5) 이름과 그들의 국적만 화면 출력하시오
display_5 <- data.frame('이름'=exam_data2$name,'국적'=exam_data2$country)
display_5



