## BigData Handling ###########################################################
# 21.11.04(목)

  # 환경설정
rm(list=ls())
getwd()
setwd('c:/james/code/rwork/data/dataset3')

  # 라이브러리 모음
install.packages("dplyr") #파이프 연산자 패키지
install.packages("hflights")

library(dplyr)
library(hflights)


## 데이터 조작 ################################################################
# 1) 데이터 로딩
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% head() %>% summary()

iris %>% head()

iris %>% head() %>% subset(Sepal.Length >= 5.0)

str(hflights)


# tbl_df(): 현재 R의 콘솔창 크기에서 볼 수 있는 만큼 결과를 출력
# 나머지는 아래에 생략된 행 수와 컬럼명 표시
hflights_df <- tbl_df(hflights)
hflights_df




# 조건에 맞는 데이터 필터링 ####
csvgrade %>% filter(class == 1) # ==(더블이퀄) 연산자
csvgrade %>% filter(class != 1) # !=(엑스클러메이션 포인트 이퀄) 연산자
csvgrade %>% filter(math > 50) # >(라이트 앵글 브래킷) 연산자
csvgrade %>% filter(math < 50) # <(레프트 앵글 브래킷) 연산자
csvgrade %>% filter(eng >= 80) # >=(라이트 앵글 브래킷 이퀄) 연산자
csvgrade %>% filter(eng <= 80) # <=(레프트 앵글 브래킷 이퀄) 연산자
csvgrade %>% filter(class == 1 & math >= 50) # &(앰퍼샌드) 연산자
csvgrade %>% filter(eng < 90 | sci < 50) # |(버티컬바) 연산자
csvgrade %>% filter(class == 1 | class == 3 | class == 5)
csvgrade %>% filter(class %in% c(1, 3, 5)) # %in% 연산자: 여러 객체에서 여러 조건을 줄 때는 사용할 수 없다.


# 객체 생성
class1 <- csvgrade %>% filter(class == 1)
# class1 객체의 math 컬럼에 접근하여 평균 산출
mean(class1$math)
# class1 객체의 english 컬럼에 접근하여 평균 산출
mean(class1$eng)
# class1 객체의 science 컬럼에 접근하여 평균 산출
mean(class1$sci)


# hflights_df 를 대상으로 특정일의 데이터 추출하기
filter(hflights_df, Month == 1 & DayofMonth == 2) # 1월 2일 데이터 추출
hflights_df %>% filter(Month == 1 & DayofMonth == 1) # 파이프 연산자를 이용한 1월 1일 데이터 추출
filter(hflights_df, Month == 1 | Month == 2) # 1월 또는 2월 데이터 추출




# 컬럼으로 데이터 정렬 ####
# 형식: arrange(dataframe, 컬럼1, 컬럼2, ...) 디폴트는 오름차순
#       arrange(dataframe, desc(컬럼1, 컬럼2, ...)) desc()로 지정하는 경우 내림차순

# 단일 객체의 오름차순 정렬
csvgrade %>% arrange(math)
# 단일 객체의 내림차순 정렬
csvgrade %>% arrange(desc(math))
# 다중 객체의 오름차순 정렬
csvgrade %>% arrange(class, math)
# hglights_df를 대상으로 데이터 정렬
arrange(hflights_df, Year, Month, DepTime, ArrTime)
#파이프연산자를 사용하여 hflights_df 데이터 셋 에 arrange() 함수 적용
hflights_df %>% arrange(Year, Month, DepTime, AirTime)




# 컬럼으로 데이터 검색 ####
# 데이터셋의 특정 컬럼 을 기준으로 데이터 검색 시 select() 함수 사용
# 형식: select(dataframe, 컬럼 1, 컬럼 2, ...)

# 단일 객체 추출
csvgrade %>% select(math)
# 다중 객체 추출
csvgrade %>% select(class, math)
# 객체 제외
csvgrade %>% select(-math)
# 특정 객체의 값 추출
csvgrade %>% filter(class == 1) %>% select(eng)
# 특정 객체의 값 일부를 추출
csvgrade %>% select(id, math) %>% head(3)

# hflights_df를 대상으로 지정된 컬럼 데이터 검색
select(hflights_df, Year, Month, DepTime, ArrTime)
hflights_df %>% select(hflights_df, Year, Month, DepTime, AirTime)
# 대상 컬럼의 범위로 검색
select(hflights_df, Year:ArrTime)




# 데이터셋에 컬럼 추가 ####
# 형식: mutate(dataframe, 컬럼명1=수식1, 컬럼명2=수식2, ...)

# hflights_df 에서 출발 지연시간과 도착 지연시간의 차이를 계산한 컬럼 추가
mutate(hflights_df, gain = ArrTime - DepTime,
           gain_per_hour = gain / (AirTime / 60))
hflights_df %>% mutate(gain = ArrDelay - DepDelay, 
              gain_per_hour = gain / (AirTime /60))

# mutata()함수에 의해 추가된 컬럼 보기
select(mutate(hflights_df, gain = ArrDelay - DepDelay,
                  gain_per_hour = gain / (AirTime / 60)),
       Year, Month, ArrDelay, DepDelay, gain, gain_per_hour)




## 요약통계 구하기 ############################################################
# 형식: summarise(dataframe, 추가할 컬럼명 = 함수(컬럼명), ...)
csvgrade %>% summarise(mean_math = mean(math))

# hflights_df에서 비행시간의 평균 구하기
summarise(hflights_df, avgAirTime = mean(AirTime, na.rm = TRUE))
hflights_df %>% summarise(avgAirTime = mean(AirTime, na.rm = TRUE))

# mean()함수를 사용하여 평균을 계산하여 avgAirTime 변수에 저장
hflights_df %>% summarise(avgAirTime = mean(AirTime, na.rm=TRUE))

# hflights_df의 관측치 길이 구하기
# n(): 데이터셋의 관측치 길이를 구하는 함수
summarise(hflights_df, cnt = n(), delay = mean(AirTime, na.rm = TRUE))

# 도착시간(AirTime)의 표춘편차와 분산 계산
summarise(hflights_df, arrTimeSd = sd(ArrTime, na.rm = TRUE),
          arrTimeVar = var(ArrTime, na.rm = TRUE))




## 집단변수 대상 그룹화 #######################################################
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% group_by(class) %>% summarise(mean_math = mean(math))

# 컬럼의 집단별 다중 요약 통계량
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% group_by(class) %>%
  summarise(mean_math = mean(math), sum_math = sum(math), median_math = median(math))


# 집단변수를 이용하여 그룹화
species <- group_by(iris, Species)
str(species)
species




## 데이터 프레임 병합 #########################################################
a <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
b <- data.frame(id = c(3, 4 , 5, 6, 7), weight = c(80, 90, 85, 60, 85))

merge(a, b, by="id")
inner_join(a, b, by = "id")




## 컬럼명 수정 ################################################################
# 1) 데이터 프레임 생성
df <- data.frame(one = c(4, 3, 8))
df
df <- rename(df, "원" = one)
df

# 2) 컬럼명 수정
df_rename <- rename(df_cols, x2 = x1)
df_rename <- rename(df_rename, y2 = y1)
df_rename




## reshape2 패키지 활용 #######################################################
data <- read.csv("c:/rwork/data.csv")

# 데이터를 넓은 형식으로 변환
wide <- dcast(data, Customer_ID ~ Date, sum)
wide
# write.csv(wide, "wide.csv", row.names = FALSE)
wide <- read.csv("wide.csv")
colnames(wide) <- c('Customer_ID', 'day1', 'day2', 'day3',
    'day4', 'day5', 'day6', 'day7')
wide

# 데이터를 긴 형식으로 변환
# 1) 데이터를 긴 형식으로 변경
long <- melt(wide, id = "Customer_ID")
long

# 2) 컬럼명 변경
name <- c("Customer_ID", "Date","Buy")
colnames(long) <- name
head(long)

# 스미스 데이터를 통한 긴형식 변환 예시
data("smiths")

# 1) 넓은 형식의 smiths 데이터 셋을 긴 형식으로 변경
long <- melt(id = 1:2, smiths)
long

# 2) 긴 형식을 넓은 형식으로 변경하기
dcast(long, subject + time ~ ...)




## 3차원 배열 형식으로 변경 ###################################################
data('airquality')

# 1) 컬럼제목을 대문자로 일괄 변경
names(airquality)
toupper(names(airquality))
head(airquality)

# 2) 함수를 이용하여 넓은 형식을 긴 형식으로 변경
air_melt <- melt(airquality, id = c("MONTH", "DAY"), na.rm = TRUE)
head(air_melt)

# 3) 함수를 이용하여 3 차원으로 구조 변경
names(air_melt) <- tolower(names(air_melt))
acast <- acast(air_melt, day ~ month ~ variable)
acast
class(acast)



