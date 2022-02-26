## 요인분석 연습문제
# 2022.01.24(월)

  #환경설정
rm(list=ls())
getwd()
setwd("C:/Rwork/")

  #라이브러리
# install.packages("memisc")
library(memisc)


## 1번문제 ====================================================================
# 다음은 drinking,_water_example.sav파일의 데이터셋이 구성된 테이블이다.
# 전체 2개의 요인에 의해서 7개의 변수로 구성되어 있다.
# 아래에서 제시된 각 단계에 맞게 요인 분석을 수행하시오.

# 1)데이터파일 가져오기
data.spss <- as.data.set(spss.system.file('drinking_water_example.sav'))
data.spss
dw_exam <- data.spss[1:7]
dw_exam_df <- as.data.frame(dw_exam)
str(dw_exam_df)



# 2) 베리맥스 회전법, 요인수2, 요인점수 회귀분석 방법을 적용하여 요인 분석
result <- factanal(dw_exam_df,
                   factors = 2,
                   rotation = "varimax",
                   scores = "regression")
result
  # p-값이 0.05 이상이므로 요인 선택에 문제 없음



# 3) 요인적재량 행렬의 컬럼명 변경
str(result)
colnames(result$loadings) <- c("제품친밀도","제품만족도")
colnames(dw_exam_df) <- c("브랜드","친근감","익숙함","목넘김","맛","향","가격")
result


# 4) 요인점수를 이용한 요인적재량 시각화
plot(result$scores[ , c(1:2)],
     main = "Factor1 과 Factor2 요인점수 행렬")
text(result$scores[ , 1], result$scores[ , 2],
     labels = name, cex = 0.7, pos = 3, col = "blue")
points(result$loadings[ , c(1:2)], pch = 19, col = "red")
text(result$loadings[ , 1], result$loadings[ , 2],
     labels = rownames(result$loadings),
     cex = 0.8, pos = 3, col = "red")

# 5) 요인별 변수 묶기
f <- data.frame(dw_exam_df$브랜드, dw_exam_df$친근감, dw_exam_df$익숙함)
s <- data.frame(dw_exam_df$목넘김, dw_exam_df$맛, dw_exam_df$향, dw_exam_df$가격)




## 2번문제 ====================================================================
# 1번에서 생성된 두 개의 요인을 데이터프레임으로 생성한 후,
# 이를 이용하여 두 요인 간의 상관관계 계수를 제시하시오.
df <- data.frame(f,s)
colnames(df) <- c("브랜드","친근감","익숙함","목넘김","맛","향","가격")
head(df)

freindship <- round(
  (f$dw_exam_df.브랜드 + f$dw_exam_df.친근감 + f$dw_exam_df.익숙함) / ncol(s), 2)
satisfaction <- round(
  (s$dw_exam_df.목넘김 + s$dw_exam_df.맛 + s$dw_exam_df.향 + s$dw_exam_df.가격) / ncol(s), 2)

  
drinking_water_factor_df <- data.frame(freindship, satisfaction)
colnames(drinking_water_factor_df) <- c("제품친밀도","제품만족도")
cor(drinking_water_factor_df)


