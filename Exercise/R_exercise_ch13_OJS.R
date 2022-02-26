## R_exercise_ch13 ############################################################
# 22.01.20(목)
# R 분산분석 연습문제 [오준서]

# 환경설정
rm(list=ls())
setwd('C:/rwork/')

# 라이브러리 모음
install.packages("prettyR") #4번문제 빈도율
library(prettyR)
library(stats) #정규성 검정
library(dplyr) #파이프 연산자



## 1번문제 #####################################################################
# 1. 교육 방법에 따라 시험성적에 차이가 있는지 검정하시오
  # 1) 데이터셋: twomethod.csv
  # 2) 변수: method(교육방법), score(시험성적)
  # 3) 모델: 교육방법(명목) -> 시험성적(비율)
  # 4) 전처리, 결측치 제거
data1 <- read.csv('c:/rwork/twomethod.csv')


# (1)분석기법 선정
  # 종속변수가 비율척도이고 독립변수가 명목척도(교육방법)일 때 사용할 수 있는 분석 방법
  # -> Paired T-test, wilcox, 2-sample T-test, mann-whitney, One-way ANOVA, Kruskal-Wallis
  # 교육 방법은 짝을 이루지 않는 2개의 독립적인 변수이므로 정규성 검정을 통해
  # 독립표본(2-sample) T-test 와 Kruskal-Wallis 검정 중 하나를 실시한다.



# (2)데이터 전처리
unique(data1$method) #교육방법=2가지 1,2

  # 결측값 처리
  gradiff <- subset(data1, !is.na(score), c(method, score))

  # 이상치 탐지
  par(mfrow = c(1, 2))
  plot(gradiff$score)
  barplot(gradiff$score)
  # 만점이 50점이라고 봤을 때 이상치는 존재하지 않음
  
  
  # 데이터 분리
  a <- subset(gradiff,method == 1)
  b <- subset(gradiff,method == 2)



# (3)정규성(동질성) 검정
  # 방법 1
  bartlett.test(score ~ method,data=gradiff)
  # p값이 0.05보다 큰 0.8738이므로 집단 간 분포의 모양이 동질하다고 볼 수 있다.

  # 방법 2
  var.test(a$score,b$score)




# (4)분석실시
  # 변수들이 짝을 이루지 않고 정규성을 만족하므로 독립표본 t-test분석을 실시한다.
  # 양측검정
  t.test(a$score,b$score, alter="two.sided", conf.int = TRUE, conf.level = 0.95)
  
# 방향성을 갖는 단측 가설 검정
  t.test(a$score,b$score, alter = "greater", conf.int = TRUE, conf.level = 0.95)
  # 귀무가설 기각 안함
  t.test(a$score,b$score, alter = "less",  conf.int = TRUE, conf.level = 0.95)
  # 귀무가설 기각 함
  



## 2번문제 #####################################################################
# 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 대해서 만족도에 차이가
# 있는가를 검정하시오. (힌트. 두 집단 비율 차이 검정)
  # 1) 데이터셋: two_sample.csv
  # 2) 변수: gender(1,2), survey(0, 1)
data2 <- read.csv('c:/rwork/two_sample.csv')
head(data2)
unique(data2$gender) #성별 확인
unique(data2$survey) #만족여부 확인(불만족=0,만족=1)

# (1)빈도분석
x <- data2$gender
y <- data2$survey
table(x)
table(y)
table(x, y, useNA = "ifany")



# (2)양측검정
prop.test(c(138, 107), c(174, 126),
          alternative = "two.sided", conf.level = 0.95)



# (3)방향성을 갖는 단측 가설 검정
prop.test(c(138, 107), c(174, 126),
          alter = "greater", conf.level =0.95)
prop.test(c(138, 107), c(174, 126),
          alter = "less", conf.level = 0.95)





## 3번문제 #####################################################################
# 우리나라 전체 중학교 2학년 여학생 평균 키가 148.5cm 로 알려진 상태에서
# A 중학교 2학년 전체 500명을 대상으로 10%인 50명을 표본으로 선정하여 표본평균
# 신장을 계산하고 모집단의 평균과 차이가 있는지를 단계별로 분석을 수행하여 검정하시오.
# 1) 데이터셋: student_height.csv
# 2) height <- stheight$height
# 3) 기술통계량 평균 계산
# 4) 정규성 검정
# 5) 가설 검정
data3 <- read.csv('c:/rwork/student_height.csv')
height <- data3$height

# (1)표본평균신장
summary(height)
mean(height)



# (2)정규성(동질성) 검정
shapiro.test(height)
  # p-값이 0.05보다 작으므로 데이터가 정규분포를 따른다는 귀무가설을 기각한다.



#? (3)wilcox 분석수행
wilcox.test(height, mu = 148.5)




## 4번문제 #####################################################################
# 중소기업에서 생산한 HDTV판매율을 높이기 위해서 프로모션을 진행한 결과 기존
# 구매비율보다 15% 향상되었는지를 단계별로 분석을 수행하여 검정하시오.
  # 귀무가설(H0): 기존 구매비율보다 15% 향상되지 않음
  # 연구가설(H1): 기존 구매비율보다 15% 향상됨
  # 1) 구매여부 변수: buy (1: 구매하지 않음, 2: 구매)
  # 2) 데이터셋: hdtv.csv
  # 3) 빈도수와 비율 계산
  # 4) 가설 검정
data4 <- read.csv('c:/rwork/hdtv.csv')
head(data4)
buy <- data4$buy


# (1)빈도수와 비율계산
summary(buy)
length(buy)
table(buy)
freq(buy)



# (2)이항분포 비율 검정
  binom.test(10, 50, p = 0.15, alternative = "two.sided", conf.level = 0.95)
  #p=0.05 이상이므로 귀무가설을 기각하지 않는다
  
  

# (3)방향성을 갖는 단측 가설 검정(필요x)
  # 프로모션 후 구매비율 > 프로모션 전 구매비율
  binom.test(10, 50, p = 0.15, alternative = "greater", conf.level = 0.95)
  # 프로모션 후 구매비율 < 프로모션 전 구매비율
  binom.test(10, 50, p = 0.15, alternative = "less", conf.level = 0.95)




