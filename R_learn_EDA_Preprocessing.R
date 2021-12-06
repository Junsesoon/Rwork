## EDA & Data Preprocessing ###################################################
# 21.11.04(목)

  # 환경설정
rm(list = ls())
getwd()
setwd("C:/james/code/rwork/data/dataset3")

  # 라이브러리 모음
install.packages("lattice") #고급 시각화 패키지
install.packages("reshape2") #dcast(), melt()
install.packages("cvTools") #교차검정 패키지

library(lattice)
library(reshape2)
library(plyr) #파생변수 합치기
library(cvTools)

## 수집자료 이해 ##############################################################
# 1) 데이터 셋 가져오기
dataset <- read.csv("dataset.csv", header=T)
dataset

# 2) 전체 데이터 보기
print(dataset)
View(dataset)

# 3) 데이터 셋 구조 확인
names(dataset)
attributes(dataset)
str(dataset)

# 4) 데이터 셋에서 특정 변수 조회
dataset$age
dataset$resident
length(dataset$age)

# 5) 특정 변수의 조회 결과를 변수에 저장
x <-dataset$gender
y <- dataset$price
x
y

# 6) 산점도 그래프로 변수 조회
plot(dataset$price)

# 7) 컬럼명을 사용하여 특정 변수 조회
dataset['gender']
dataset['price']

# 8) index를 사용하여 특정 변수 조회
dataset[2]
dataset[6]
dataset[3,]
dataset[,3]

# 9) 2개 이상의 컬럼 조회
dataset[c("job","price")]
dataset[c(2,6)]
dataset[c(2,2,3)]
dataset[c(2,4:6,3,1)]

# 10) 특정 행/열을 조회
dataset[,c(2:4)]
dataset[c(2:4),]
dataset[-c(1:100),]




## 결측치 처리 ################################################################
# 결측치 제거 ####
# 1) 결측치 확인
summary(dataset$price) # 특정 변수의 결측치 확인
sum(dataset$price) # 결측치가 포함된 경우 'NA'가 출력

# 2) 함수 속성을 이용하여 결측치 제거
sum(dataset$price, na.rm = T)

# 3) 함수를 이용하여 결측치 제거
price2 <- na.omit(dataset$price)
sum(price2)
length(price2)




# 결측치 대체 ####
# 1) 결측치 0으로 대체
x <- dataset$price
x[1:30]
dataset$price2 = ifelse(!is.na(x),x,0)
dataset$price2[1:30]

# 2) 결측치를 평균으로 대체
x <-  dataset$price
x[1:30]
dataset$price3 = ifelse(!is.na(x), x, round(mean(x, na.rm = TRUE),2))
dataset$price3[1:30]
dataset[c('price', 'price2', 'price3')]



# 극단치 처리 ####
# 극단치(outlier): 정상적인 분포에서 벗어난 값

# 범주형 변수의 극단치 처리 ####
table(dataset$gender)
pie(table(dataset$gender))

# 1) subset()함수를 사용하여 데이터 정제 
dataset <- subset(dataset, gender == 1 | gender == 2)
dataset
length(dataset$gender)
pie(table(dataset$gender))
pie(table(dataset$gender), col = c("red", "blue"))



# 연속형 변수의 극단치 처리 ####
# 1) 연속형 변수의 극단치 보기
dataset <- read.csv("dataset.csv", header = T)
dataset$price
length(dataset$price)
plot(dataset$price)
summary(dataset$price)
# 산점도 또는 summary()에서 제공되는 요약을 통해 극단치 처리 방법 결정

# price변수의 데이터 정제와 시각화
dataset2 <- subset(dataset, price >= 2 & price <= 8)
length(dataset2$price)
stem(dataset2$price) # stem()함수를 사용하여 정보를 줄기와 잎 형태로 도표화

# age변수의 데이터 정제와 시각화
# 1) age 변수에서 NA 발견
summary(dataset2$age)
length(dataset2$age)

# 2) age 변수 정제(20~69)
dataset2 <-  subset(dataset2, age >= 20 & age <= 69)
length(dataset2)

# 3) box 플로팅으로 평균연령 분석
boxplot(dataset2$age) # boxplolt(): 정제된 결과를 상자 그래프로 시각화



# 극단치를 찾기 어려운 경우 ####
# 1)boxplot으로 price의 극단치 시각화
boxplot(dataset$price)

# 2) 극단치 통계 확인
boxplot(dataset$price)$stats

# 3) 극단치를 제거한 서브셋 만들기
dataset_sub <-  subset(dataset, price >= 2 & price <= 7.9)
summary(dataset_sub$price)



## 코딩변경 ###################################################################
# 1) 가독성 향상을 위한 코딩 변경
dataset2$resident2[dataset2$resident == 1] <- '서울특별시'
dataset2$resident2[dataset2$resident == 2] <- '인천광역시'
dataset2$resident2[dataset2$resident == 3] <- '대전광역시'
dataset2$resident2[dataset2$resident == 4] <- '대구광역시'
dataset2$resident2[dataset2$resident == 5] <- '시구군'

# 2) 코딩 변경 전과 변경 후의 칼럼 보기
dataset2[c("resident","resident2")]

# 3) 가독성을 위해 job 칼럼을 대상으로 코딩 변경하기
dataset2$job2[dataset2$job == 1] <- '공무원'
dataset2$job2[dataset2$job == 2] <- '회사원'
dataset2$job2[dataset2$job == 3] <- '개인사업'

# 4) 코딩 변경 전과 변경 후의 칼럼 보기
dataset2[c("job","job2")]

# 5) 척도 변경을 위한 코딩 변경
dataset2$age2[dataset2$age <= 30] <- '청년층'
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- '중년층'
dataset2$age2[dataset2$age > 55 ] <- '장년층'

# 6) 역 코딩을 위한 코딩 변경
survey <- dataset2$survey
csurvey <-  6- survey
csurvey
dataset2$survey <- csurvey




## 변수 간의 관계 분석 ########################################################
# 범주형 vs 범주형 데이터 분포 시각화
# 1) 데이터 가져오기
new_data <- read.csv("new_data.csv", header = TRUE)
str(new_data)

# 2) 코딩 변경된 거주지역(resident) 컬럼과 성별(gender) 컬럼을 대상으로 빈도수 구하기
resident_gender <- table(new_data$resident2, new_data$gender2)
resident_gender
gender_resident <- table(new_data$gender2, new_data$resident2)
gender_resident

# 3) 성별(gender)에 따른 거주지역(resident)의 분포 현황 시각화
barplot(resident_gender, beside = T, horiz = T,
        col = rainbow(5),
        legend = row.names(resident_gender),
        main = ' 성별에 따른 거주지역 분포 현황')

# 4) 거주지역 resident) 에 따른 성별 gender) 의 분포 현황 시각화
barplot(gender_resident, beside = T,
        col = rep(c(2, 4), 5), horiz = T,
        legend = c("남자", "여자"),
        main = '거주지역별 성별 분포 현황')




# 연속형 vs 범주형 데이터의 시각화
# 1) 직업 유형에 따른 나이 분포 현황
densityplot(~ age, data = new_data,
            groups = job2,
            # plot.points = T: 밀도 , auto.key = T: 범례
            plot.points = T, auot.key = T)




# 연속형 vs 범주형 vs 범주형 데이터 분포 시각화
# 1) 성별에 따른 직급별 구매비용 분석
densityplot(~ price | factor(gender),
            data = new_data,
            groups = position2,
            plot.points = T, auto.key = T)

# 2) 직급에 따른 성별 구매비용 분석
densityplot(~ price | factor(position2),
            data = new_data,
            groups = gender2,
            plot.points = T, auto.key = T)




# 연속형(2개) vs 범주형(1개)
xyplot(price ~ age | factor(gender2),
       data = new_data)




## 파생변수 ###################################################################
# 더미형식으로 파생변수 생성 ####
# 1) 데이터 가져오기
user_data <- read.csv("user_data.csv", header = T)
head(user_data)
table(user_data$house_type)

# 2) 더미변수 생성
house_type2 <- ifelse(user_data$house_type == 1 |
           user_data$house_type == 2, 0 , 1)
house_type2[1:10]

# 3) 파생변수 추가
user_data$house_type2 <- house_type2
head(user_data)




# 1:1 관계로 파생변수 생성 ####
# 1) 데이터 파일 가져오기
pay_data <- read.csv("pay_data.csv", header = T)
head(pay_data, 10)
table(pay_data$product_type)

# 2) 고객별 상품 유형에 따른 구매금액과 합계를 나타내는 파생변수 생성
product_price <- dcast(pay_data, user_id ~ product_type,
                       sum, na.rm = T)
head(product_price, 3)

# 3) 컬럼명 수정
names(product_price) <- c('user_id', ' 식표품 (1)', '생필품(2)', '의류 (3)', 
                          '잡화 (4)', '기타(5)')
head(product_price)

# 고객식별번호 (user_ 에 대한 지불유형 pay_method) 의 파생변수 생성
# 1) 고객별 지불유형에 따른 구매상품 개수를 나타내는 파생변수 생성
pay_price <- dcast(pay_data, user_id ~ pay_method, length)
head(pay_price, 3)

# 2) 컬럼명 변경
names(pay_price) <- c('user_id', ' 현금 (1)', '직불카드(2)',
                      '신용카드 (3)', '상품권(4)')
head(pay_price, 3)




# 파생변수 합치기 ####
# 1) 고객 정보 테이블과 고객별 상품 유형에 따른 구매금액 합계 병합
user_pay_data <- join(user_data, product_price, by = 'user_id')
head(user_pay_data, 10)

# 2) 고객별 지불유형에 따른 구매상품 개수 병합하기
user_pay_data <- join(user_pay_data, pay_price, by = 'user_id')
user_pay_data[c(1:10), c(1, 7:15)]




## 표본추출 ###################################################################
# 1) 데이터 저장
setwd("C:/Rwork/")
write.csv(user_pay_data, "cleanData.csv", quote = F, row.names = F)
data <- read.csv("cleanData.csv", header = TRUE)
data

# 2) 표본 샘플링
nrow(data)
choice1 <- sample(nrow(data), 30)
choice1
# 50 ~ (data길이) 사이에서 30 개 행을 무작위 추출
choice2 <- sample(50:nrow(data), 30)
choice2
# 50~100 사이에서 30 개 행을 무작위 추출
choice3 <- sample(c(50:100), 30)
choice3
# 다양한 범위를 지정하여 무작위 샘플링
choice4 <- sample(c(10:50, 80:150, 160:190), 30)
choice4

# 3) 샘플링 데이터로 표본 추출
data[choice1, ]


# iris 데이터 셋 대상 표본추출
# 1) 데이터 로딩
data("iris")
dim(iris)

# 2) 학습 데이터 70%), 검정 데이터 30%) 비율로 데이터 셋 구성
idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
training <-iris[idx, ]
testing <-iris[-idx, ]
dim(training)




## 교차 검정 샘플링 ###########################################################
# 1) 데이터프레임 생성
name <- c('a','b','c','d','e','f')
score <- c(90,85,99,75,65,88)
df <- data.frame(Name=name, Score=score)

# 2) 교차 검정을 위한 패키지 설치
install.packages("cvTools")
library(cvTools)

# 3) K겹 교차 검정 데이터 셋 생성
cross <- cvFolds(n=6,K=3, R=1, type='random')
cross

# 4) K겹 교차 검정 데이터 셋 구조 보기
str(cross)
cross$which

# 5) subsets 데이터 참조하기
cross$subsets[cross$which == 1,1]
cross$subsets[cross$which == 2,1]
cross$subsets[cross$which == 3,1]

# 6) 데이터프레임의 관측치 적용
r=1
K= 1:3
for(i in K){
  datas_idx <- cross$subsets[cross$which == i, r]
  cat('K=',i,'검정데이터 \n')
  print(df[datas_idx,])
  
  cat('K=',i,'훈련데이터 \n')
  print(df[-datas_idx,])
}
