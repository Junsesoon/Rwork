## Exercise usingDB ###########################################################
# 22.01.04(화)

  # 환경설정
rm(list=ls())

  # 라이브러리 모음
install.packages("DBI")
library(DBI)
install.packages("dplyr")
library(dplyr)
install.packages("dbplyr")
library(dbplyr)
install.packages("odbc")
library(odbc)


con <- dbConnect(odbc::odbc(), "Oracle DB")
