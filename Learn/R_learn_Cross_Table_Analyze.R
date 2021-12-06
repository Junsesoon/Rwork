## Cross Table Analyze ########################################################
# 21.11.10(수)

  # 환경설정
rm(list=ls())
getwd()
setwd('c:/rwork')

  # 라이브러리 모음
install.packages("gmodels")
install.packages("ggplot2")

library(gmodels)
library(ggplot2)


## 교차분석 ###################################################################
# 1) 데이터 셋 가져오기
data <- read.csv('cleanDescriptive.csv', header = TRUE)
head(data)

# 2) 변수 리코딩
x <- data$level2
y <- data$pass2

# 3) 데이터프레임 생성
result <- data.frame(Level=x, Pass=y)
dim(result)

# 4) 기본 함수를 이용한 교차 분할표 작성
table(result)

# 5) 교차 분할표 작성
CrossTable(x=diamonds$color, y = diamonds$cut)




# 교차 분할표 작성: 부모의 학력수준과 자녀 대학 진학여부
# 변수모델: 학력수준(독립변수) -> 진학여부(종속변수)
x <- data$level2
y <- data$pass2
CrossTable(x,y)

# 부모의 학력 수준에 따른 자녀 의 대학 진학 여부를 설문 조사한 결과 
# 부모의 학력 수준에 상관없이 대학진학 합격률이 평균 60.0% 로 학력 수준별로 
# 유사한 결과가 나타났다.
# 전체 응답자 225 명을 대상으로 고졸 39.6%(89 명 중 55.1% 가 진학에 성공하였고
# 대졸 36.4%(82 명 중 68.4% 가 성공했으며 대학원졸은 24%(54 명 중 57.4% 가 
# 대학진학에 성공하였다. 특히, 대졸 부모의 대학진학 합격률이 평균보다 조금 높고 
# 고졸 부모의 대학진학 합격률이 평균보다 조금 낮은 것으로 분석된다



