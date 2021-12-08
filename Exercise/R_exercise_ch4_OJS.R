## R_exercise_ch4 #############################################################
# 2021. 10. 05(화)
# R 벡터와 데이터프레임 핸들링 

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
library(stringr)


## 1번문제 ####################################################################
# 다음의 벡터 EMP는 ‘입사연도이름급여’순으로 사원의 정보가 기록된 데이터이다. 
# 벡터 EMP를 이용하여 다음과 같은 출력 결과가 나타나도록 함수를 정의하시오.

EMP <- c("2014홍길동220", "2002이순신300", "2010유관순260")
# 출력결과:
# 전체급여 평균: 260
# 평균 이상 급여 수령자
# 이순신 => 300
# 유관순 => 200

result <- function(x){
  RMbday <- str_replace(x,"[0-9]{4}", "z")
  name <- unlist(str_extract_all(x, "[가-힣]{3}")) 
  pay <- str_extract_all(RMbday, "[0-9]{3}") 
  pay <- as.numeric(pay) #리스트화 동시해제
  EMP_data <- data.frame("이름"=name, "급여"=pay)

cat("전체급여 평균:", mean(pay), "\n")
cat("평균 이상 급여 수령자", "\n")
  for(idx in 1:length(pay)){
    if(pay[idx] >= mean(pay)){
      cat(name[idx], "=>", pay[idx], "\n")
    }
    else{cat("\n")
    }
  }
}
result(EMP)




## 2번문제 ####################################################################
# 다음 조건에 맞게 client 데이터프레임을 생성하고, 데이터를 처리하시오
name <- c("유관순", "홍길동", "이순신", "신사임당")
gender <- c("F", "M", "M", "F")
price <- c(50, 65, 45, 75)

# 1)3개의 벡터 객체를 이용하여 client 데이터프레임을 생성하시오.
client <- data.frame(name, gender, price) 
client

# 2)price변수의 값이 65만원 이상이며 문자열 “beat”, 65만원 미만이면
#   문자열 “Normal” 을 변수result에 추가하시오.(힌트, ifelse()사용)
result <- ifelse(client$price >= 65, "beat", "Normal")
result
#풀이
client$result <- ifelse(client$price >= 65, "beat", "Normal") 
#result라는 변수를 데이터 프레임에 직접 추가
client

# 3)result변수를 대상으로 빈도수를 구하시오.
table(client$result)




### test zone #################################################################
name <- c("홍길동", "이순신", "유관순")
bday <- c(2014, 2002, 2010)
pay <- c(220, 300, 260)

EMP_data <- data.frame("출생연도"= bday, "이름"=name, "급여"=pay)
#되는 for()문
for(idx in 1:length(EMP_data)){
  if(pay[idx] >= mean(pay)){
    cat(name[idx], "=>", pay[idx], "\n")
  }
  else{cat("\n")
  }
}

#안되는 for()문
for(idx in 1:length(EMP_data)){
  ifelse(pay[idx] >= mean(pay), cat(name[idx], "=>", pay[idx]), cat("\n"))
}




## 기타방법 ###################################################################

EMP <- c("2014홍길동220", "2002이순신300", "2010유관순260")

Name <- str_extract(EMP,"[가-힣]{3}");Name
EMP1 <- str_replace(EMP, "[0-9]{4}[가-힣]{3}","");EMP1
Pay <- as.numeric(EMP1);Pay
Paymean <- mean(Pay)

emp_pay <- function(x){
  cat("전체 급여 평균: ", Paymean, "\n")
  cat("평균 이상 급여 수령자", "\n")
  for(i in 1:length(x)){
    if(Pay[i] >= Paymean){
      cat(Name[i], "=>", Pay[i], "\n")
      i = i+1
    }
  }
}
emp_pay(EMP)



