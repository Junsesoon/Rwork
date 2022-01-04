## Using DB ####################################################################
# 22.01.03(월)

  # 환경설정
rm(list=ls())
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_311/bin")
drv <- JDBC("oracle.jdbc.driver.OracleDriver", "C:/OracleTest/ojdbc6.jar")
conn <- dbConnect(drv,"jdbc:oracle:thin:@//127.0.0.1:1521/xe", "scott", "tiger")

  # 라이브러리 모음
remove.packages("rJava", lib="~/R/win-library/4.0")
install.packages("DBI")
library(DBI)
install.packages("RJDBC")
library(RJDBC)
install.packages("rJava")
library(rJava)


## DB 레코드 검색, 추가, 수정, 삭제 ###########################################
# 모든 레코드
query = "SELECT * FROM test_table"
dbGetQuery(conn,query)

# 나이 기준으로 내림차순 정렬
query = "SELECT * FROM test_table order by age desc"
dbGetQuery(conn, query)

# insert record
query = "insert into test_table values('kang', '1234', '강감찬', 45)"
dbSendUpdate(conn, query)

# 추가확인
query = "SELECT * FROM test_table"
dbGetQuery(conn, query)

# 나이가 40세 이상인 record
query = "select * from test_table where age >= 40"
result <- dbGetQuery(conn, query)
result

# name이 '강감찬'인 데이터의 age를 40으로 수정
query = "update test_table set age = 40 where name = '강감찬'"
dbSendUpdate(conn,query)

# 수정된 레코드 조회
query = "select * from test_table where name= '강감찬'"
dbGetQuery(conn,query)

# name이 '홍길동'인 레코드 삭제
query = "delete from test_table where name = '홍길동'"
dbSendUpdate(conn,query)

# 전체 레코드 조회
query = "select * from test_table"
dbGetQuery(conn,query)

