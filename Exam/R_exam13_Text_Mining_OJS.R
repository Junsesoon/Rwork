## Exam13 Text Mining #########################################################
# 21.11.26(금) 오준서

  # 환경설정
rm(list=ls())
getwd()
setwd("c:/rwork/")
.libPaths()
install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP',
                        upgrade = "never", INSTALL_opts=c("--no-multiarch"))
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")
install.packages("rJava")
install.packages("tm")

  # 라이브러리 모음
install.packages("dplyr")
install_github("lchiffon/wordcloud2")
devtools::install_github("lchiffon/wordcloud2") # rtools 3.5 설치 후 사용

library(KoNLP);useNIADic() # extractnoun(),useNIADic(),buildDictionary
library(dplyr) # 파이프 연산자
library(stringr) # 문자열 추출 관련함수
library(hash) # KoNLP 관련 함수
library(tau) #
library(RSQLite) # 
library(devtools) # KoNLP 관련 함수
library(tm) # 텍스트 마이닝
library(wordcloud2) # 워드클라우드2



## 2번문제 ####################################################################
# 제공된 데이터에서 빈도수가 2회 이상 단어를 이용하여 단어 구름으로 시각화하시오.
# (wordcloud2 패키지 사용)

  # 데이터 불러오기
DrKing_data <- file("c:/rwork/DrKing.txt",encoding = "UTF-8")
DrKing <- readLines(DrKing_data)
close(DrKing_data)

# 1) 결과물 보정을 위한 말뭉치 생성 전 전처리
DrKing <- str_replace_all(DrKing,"[들이,하게]","")

exNouns <- function(x) {paste(extractNoun(as.character(x)), collapse = " ")}
DrKing_nouns <- sapply(DrKing, exNouns)
head(DrKing_nouns)

# 2) 말뭉치 생성
DrKing_corpus <- Corpus(VectorSource(DrKing_nouns))

# 3) 문장부호, 수치, 소문자, 불용어 제거
DrKing_pipe <- DrKing_corpus %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) %>% tm_map(tolower) %>% tm_map(removeWords,stopwords('english'))

# 4) 전처리 결과 확인
inspect(DrKing_pipe)

# 5) 2~8음절 대상 단어 선정
TDM5 <- TermDocumentMatrix(DrKing_pipe,control = list(wordLengths = c(4,16)))
TDM5

# 6) 자료구조 변경
DrKing_df <- TDM5 %>% as.matrix() %>% as.data.frame()

# 7) 단어 출현 빈도수 구하기
DrKing_wordtable <- sort(rowSums(DrKing_df), decreasing = TRUE)
DrKing_wordtable <- DrKing_wordtable[DrKing_wordtable >= 2]

# 8) 단어 이름과 빈도수로 데이터프레임 생성
myName <- names(DrKing_wordtable)
word.df <- data.frame(word=myName,freq=DrKing_wordtable)
str(word.df)

# 9) 단어 구름 시각화
wordcloud2(data=word.df)



