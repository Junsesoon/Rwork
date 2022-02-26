## exercise 12 ================================================================
# 220124(월) 오준서

  #환경설정
rm(list = ls())
setwd("C:/Rwork/")

  #라이브러리
#install.packages("gmodels")
library(gmodels)
#install.packages("ggplot2")
library(ggplot2)


# 1번문제 ====================================================================
# 직업 유형에 따른 응답 정도에 차이가 있는가를 단계별로 검정하시오 (동질성 검정)

  # 귀무가설(H0): 모든 표본의 비율이 동일하다.
  # 연구가설(H1): 모든 표본의 비율이 동일하지 않다.

  # 변수가 모두 범주형(직업유형, 응답률)이므로 카이제곱 기법을 적용한다.

  # 1) 파일 가져오기
  res <- read.csv("Response.csv",header = T)
  # 2) 코딩변경 - 리코딩
    # job 컬럼: 1. 학생, 2. 직장인, 3. 주부
    # response 컬럼: 1. 무응답, 2. 낮음. 3. 높음
  res$job[res$job == 1] <-  '학생'
  res$job[res$job == 2] <-  '직장인'
  res$job[res$job == 3] <-  '주부'
  
  res$response[res$response == 1] <-  '무응답'
  res$response[res$response == 2] <-  '낮음'
  res$response[res$response == 3] <-  '높음'
  
  # 3) 교차 분할표 작성
  CrossTable(x = res$job, y = res$response)
  
  # 4) 동질성 검정
  chisq.test(res$job,res$response)
  
  # 5) 검정결과 해석
  # p-값이 0.05보다 크기 때문에 귀무가설의 기각이 불가하다.
  # 따라서, 직업유형에 따른 응답 정도는 차이가 있다.

  
  
  
# 2번문제 =====================================================================
# 나이(age)와 직위(position)간의 관련성을 단계별로 분석하시오 (독립성 검정)
  
  # 귀무가설(H0): 나이와 직위는 관련성이 없다.
  # 연구가설(H1): 나이와 직위는 관련성이 있다.
  
  # 변수가 모두 범주형(연령대, 직위)이므로 카이제곱 기법을 적용한다.
  
  # 1) 파일 가져오기
  clean <- read.csv('cleandata.csv',header = T)
  clean <- na.omit(clean)
  
  # 2) 코딩 변경(변수 리코딩)
  X <- clean$position #행: 직위 변수 이용
  Y <- clean$age3 #열: 나이 리코딩 변수 이용

  X[X == 1] <- '부장'
  X[X == 2] <- '차장'
  X[X == 3] <- '과장'
  X[X == 4] <- '대리'
  X[X == 5] <- '사원'
  
  Y[Y == 1] <- '청년층'
  Y[Y == 2] <- '중년층'
  Y[Y == 3] <- '장년층'
  
  # 3) 산점도를 이용한 변수간의 관련성 보기(힌트.Plot(x,y)함수 이용)
  plot(clean$position,clean$age3)
  
  # 4) 독립성 검정
  CrossTable(X, Y, chisq = T)
  
  # 5) 검정 결과 해석
  # p-값이 0.05보다 작으므로 귀무가설을 기각한다.
  # 따라서, 나이와 직위는 관련성이 있다.



  
# 3번문제 =====================================================================
# 교육수준(education)과 흡연율(smoking)간의 관련성을 분석하기 위한 연구가설을 수립하고,
# 단계별로 가설을 검정하시오. (독립성 검정)
  
  # 귀무가설(H0): 학력과 흡연율은 관련성이 없다.
  # 연구가설(H1): 학력과 흡연율은 관련성이 있다.
  
  # 변수가 모두 범주형(학력, 흡연율)이므로 카이제곱 기법을 적용한다.
  
  # 1) 파일 가져오기
  smoke <- read.csv('smoke.csv',header = T)
  
  # 2) 코딩변경
  #education 컬럼(독립변수) : 1. 대졸, 2. 고졸, 3. 중졸
  #smoke 컬럼(종속변수): 1. 과다흡연, 2. 보통흡연, 3. 비흡연
  smoke$education[smoke$education == 1] <- '대졸'
  smoke$education[smoke$education == 2] <- '고졸'
  smoke$education[smoke$education == 3] <- '중졸'
  
  smoke$smoking[smoke$smoking == 1] <- '과다흡연'
  smoke$smoking[smoke$smoking == 2] <- '보통흡연'
  smoke$smoking[smoke$smoking == 3] <- '비흡연'
  
  
  #3) 교차분할표 작성
  CrossTable(smoke$education,smoke$smoking, chisq = T)
  
  #4) 검정 결과 해석
  # p-값이 0.05이하이므로 귀무가설을 기각한다.
  # 따라서, 학력과 흡연율은 관련성이 있다.

  

