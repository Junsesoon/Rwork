## R_exercise_ch3.0.0 #########################################################
# 오준서 연습문제3

  # 환경설정
rm(list=ls())
getwd()
setwd('c:/rwork/')

  # 라이브러리 모음
library(datasets)
library(stringr)


## 1번문제 ####################################################################
# R에서 제공하는 CO2 데이터 셋을 대상으로 다음과 같은 단계로 파일에 저장하시오.
# 1) Treatment 컬럼 값이 ‘nonchilled’ 인 경우 ‘CO2_df1.csv’ 파일로
#   행번호를 제외하고 저장한다.
data("CO2")
air <- subset(CO2, Treatment == 'nonchilled')
write.csv(air, 'CO2_df1.csv',row.names = F)
# sink("CO2_df1.csv")
# sink()

# 2) Treatment 컬럼 값이 ‘chilled’인 경우 ‘CO2_df2.csv’파일로
#       행 번호를 제외하고 저장한다.
data("CO2")
air2 <- subset(air, air$Treatment=="chilled")
write.csv(air2, "CO2_df2.csv",row.names=F)
# sink("co2_df2.csv")
# sink()




## 2번문제 ####################################################################
# 본문에서 작성한 titanic변수를 이용하여 다음을 실행하시오
write.csv(titanic, "C:/Rwork/titanic.csv")

# 1) ‘titanic.csv’파일을 titanicData변수로 가져와서 결과를 확인하고,
#    titanicData의 관측치와 컬럼수를 확인힌다.
titanicData <- read.csv(file = "titanic.csv", header = F); titanicData
str(titanicData) #! dim(titanicData)

# 2) 1, 3번 컬럼을 제외한 나머지 컬럼을 대상으로 상위 6개의 관측치를 확인한다
head(titanicData,-c(1,3))



