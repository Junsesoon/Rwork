## exam2_Dataframe_Basic ######################################################
# 21.10.05(화) 오준서

  # 환경설정
rm(list=ls())


## 11번문제 ###################################################################
# 1) 3명 의사의 과목 점수를 이용하여 데이터 프레임을 생성하여 출력하시오
score <- c(95,85,70,80,95,80,75,60,95)
exam <- matrix(score, nrow=3, ncol=3)
colnames(exam) <- c("윤봉길(Yoon)","안중근(Ahn)","이봉창(Lee)")
rownames(exam) <- c("국어(Kor)", "영어(Eng)", "수학(Mat)"); exam

# 2) 수학과목에서 최고점을 구하시오
submax <- apply(exam, 1, max)
submax[3]

# 3) 안중근 의사의 과목 평균 점수를 구하시오
examinee <- apply(exam, 2, mean)
examinee[2]

# 4) 국어 과목의 분산을 구하시오
var(exam[,1])

# 5) 영어 과목의 표준편차를 구하시오
sd(exam[,2]) #sqrt(var(exam[,2]))




## 12번문제 ###################################################################
library(RSADBE)
data('Bug_Metrics_Software')
Bug_Metrics_Software

# 1) 소프트웨어 발표 후 행 단위 합계를 구하시오
colSums(Bug_Metrics_Software[,,2])

# 2) 소프트 웨어 발표 후 열 단위 평균을 구하시오
rowMeans(Bug_Metrics_Software[,,2])

# 3) 소프트 웨어 발표 후 칼럼 단위로 요약 통계량을 구하시오
apply(Bug_Metrics_Software,2,summary)

