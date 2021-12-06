## Text Mining ################################################################
# 21.11.23(화)

# 환경설정 ###################################################################
rm(list=ls())
getwd()
setwd('c:/rwork/')


## KoNLP 패키지 설치 #####
# 환경설정 순서 바꾸지 말 것
.libPaths()
install.packages("multilinguer") # KoNLP 오류 커버링
library(multilinguer)
install_jdk()
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")
install.packages("rJava")
install.packages("wordcloud")
install.packages("tm")
library(KoNLP);useNIADic() # extractnoun(),useNIADic(),buildDictionary


## 라이브러리 모음 ######
install.packages("Sejong") #한글 사전
install.packages("wordcloud") #워드 클라우딩
install.packages("tm") #텍스트 마이닝: Corpus(),tm_map(), TermDocumentMatrix()
install.packages("hash")
install.packages("tau")
#install.packages("devtools",type = "win.binary")
install.packages("devtools")
install.packages("RSQLite")
devtools::install_github("lchiffon/wordcloud2")
install.packages("RColorBrewer") #단어 색상 및 글꼴 지정: brewer.pal()
install.packages("arules") #연관분석 트랜잭션 패키지: apriori()
install.packages("backports") #arules 패키지 연관 패키지
install.packages("igraph") #연관분석 시각화: graph.edgelist(), plot.igraph()
install.packages("httr")
install.packages("XML")

library(wordcloud) # 워드 클라우드
library(tm)
library(devtools)
library(wordcloud2);wordcloud2(data = demoFreq) # 워드 클라우드2 테스트
library(RColorBrewer)
library(arules)
library(backports)
library(igraph)
library(httr)
library(XML)


## 기타 패키지 오류 참고 ######

# KoNLP 패키지
#install.packages("https://cran.rstudio.com/bin/windows/contrib/3.4/KoNLP_0.80.1.zip",repos = NULL)
#install.packages("https://cran.rproject.org/src/contrib/Archive/KoNLP/KoNLP_0. 80.2.tar.gz"
#, repos =NULL, type="source", INSTALL_opts = c('--no-lock'))

# igraph 패키지
#install.packages("C:/Users/tj-bu/AppData/Local/Temp/Rtmp2PhW1X/downloaded_packages/igraph_i.2.8.tar")
#install.packages("igraph")
#load('igraph.RData')
#remotes::install_github("igraph/rigraph@master")

# install.packages("rJava")

# 윈도우 데이터 베이스에 폰트가 없을 경우 설치
# install.packages('showtext')
# library('showtext')

# 시스템 변수 설정 경로
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_311')

# 사전 다운로드
#devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)


## 데이터 전처리 ##############################################################
# 텍스트 자료 가져오기
facebook <- file("facebook_bigdata.txt",encoding = "UTF-8")
facebook_data <- readLines(facebook)
head(facebook_data)

# 세종 사전에 단어 추가하기
user_dic <- data.frame(term=c("R 프로그래밍","페이스북","홍길동","소셜네트워크"),tag='ncn')
buildDictionary(ext_dic = "sejong", user_dic = user_dic)

# 단어 추출하기
paste(extractNoun('홍길동은 많은 사람과 소통을 위해서 소셜네트워크에 
                  가입하였습니다.'), collapse = " ")

# 사용자 함수 정의하기
exNouns <- function(x) {paste(extractNoun(as.character(x)), collapse = " ")}
facebook_nouns <- sapply(facebook_data, exNouns)
facebook_nouns[1]




## 추출된 단어를 대상으로 전처리하기 ##########################################
# 1) 말뭉치 생성
myCorpus <- Corpus(VectorSource(facebook_nouns))

# 2) 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus,removePunctuation)

# 3) 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro,removeNumbers)

# 4) 소문자 제거
myCorpusPrepro <- tm_map(myCorpusPrepro,tolower)

# 5) 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro,removeWords,stopwords('english'))

# 6) 전처리 결과 확인
inspect(myCorpusPrepro[1:5])




## 단어 선별(2~8음절 사이 단어 선택)하기 ######################################
# 1) 전처리 된 단어집에서 2~8음절 단어 대상 선정
myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro,
                                         control = list(wordLengths = c(4, 16)))
myCorpusPrepro_term

# 2) matrix 자료구조를 data.frame 자료구조로 변경
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)


## 단어 출현 빈도수 구하기 ####################################################
wordResult <- sort(rowSums(myTerm_df),decreasing = TRUE)
wordResult[1:10]




## 불용어 제거하기 ############################################################
# 1) 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)

# 2) 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)

# 3) 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)

# 4) 제거할 단어 지정
myStopwords = c(stopwords('english'), "사용 ", "하기")

# 5) 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro,removeWords,myStopwords)

# 6) 단어 선별과 평서문 변환
myCorpusPrepro_term <-
  TermDocumentMatrix(myCorpusPrepro,
                     control = list(wordLengths = c(4, 16)))
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))

# 7) 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df),decreasing = TRUE)
wordResult[1:10]




## 단어 구름에 디자인(빈도수, 색상, 위치, 회전 등) 적용하기 ###################
# 1) 단어 이름과 빈도수로 data.frame 생성
myName <- names(wordResult)
word.df <- data.frame(word=myName,freq=wordResult)
str(word.df)

# 2) 단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired")

# 3) 단어 구름 시각화
wordcloud(word.df$word,word.df$freq,scale=c(5,1),
          min.freq = 3, random.order = F,
          rot.per = .1, colors = pal, family = "malgun")




## 연관어 분석 ################################################################
# 패킷 설치와 메모리 로딩
# 1) 텍스트 파일 가져오기
marketing <- file("marketing.txt", encoding = "UTF-8")
marketing2 <- readLines(marketing)
close(marketing)
head(marketing2)

# 2) 줄 단위 단어 추출
lword <- Map(extractNoun,marketing2)
length(lword)
lword <- unique(lword)
length(lword)

# 3) 중복 단어 제거와 추출 단어 확인
lword <- sapply(lword, unique)
length(lword)
lword


# 연관어 분석을 위한 전처리
# 1) 단어 필터링 함수 정의
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x){Filter(filter1,x)}

# 2) 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
lword


# 필터링 간단 예문 살펴보기
# 1) 벡터 이용 리스트 객체 생성
word <- list(c("홍길동","이순","만기","김"),
             c("대한민국","우리나라대한민구","한국","resu"))
class(word)

# 2) 단어 필터링 함수 정의(길이 2~4 사이 단어 추출)
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}

filter2 <- function(x){
  Filter(filter1,x)
}

# 3) 함수 적용 list 객체 필터링
filterword <- sapply(word, filter2)
filterword




## 트랜잭션 생성하기 ##########################################################
# 1) 연관분석 패키지 설치
  # 라이브러리 모음 참조

# 2) 트랜잭션 생성
wordtran <- as(lword,"transactions")
wordtran

# 단어 간 연관규칙 발견하기
# 1) 연관규칙 발견
tranrules <- apriori(wordtran,parameter = list(supp = 0.25, conf = 0.05))

# 2) 연관규칙 생성 결과보기
detach(package:tm, unload = TRUE)
inspect(tranrules)

## 연관규칙을 생성하는 간단한 예문 ############################################
# 1) Adult 데이터 셋 메모리 로딩
data("Adult")
Adult
str(Adult)
dim(Adult)
inspect(Adult)

# 2) 특정 항목의 내용을 제외한 itermsets 수 발견
apr1 <- apriori(Adult,
                  parameter = list(support = 0.1, target = "frequent"),
                  appearance = list(none =
                                      c("income=small", "income=large"),
                                    default = "both"))
apr1
inspect(apr1)

# 3) 특정 항목의 내용을 제외한 rules 수 발견
apr2 <- apriori(Adult,
                parameter = list(support = 0.1, target = "rules"),
                appearance = list(none =
                                    c("income=small", "income=large"),
                                  default = "both"))
apr2

# 4) 지지도와 신뢰도 비율을 높일 경우
apr3 <- apriori(Adult,
                  parameter = list(supp = 0.5, conf = 0.9, target = "rules"),
                  appearance = list(none =
                                      c("income=small", "income=large"),
                                    default = "both"))
apr3




## inspect() 함수를 사용하는 간단 예문 ########################################
data(Adult)
rules <- apriori(Adult)
inspect(rules[10])

# 연관어 시각화 하기
# 1) 자료구조 변경
rules <- labels(tranrules, ruleSep = " ")
rules

# 2) 문자열로 묶인 연관 단어를 행렬구조로 변경
rules <- sapply(rules,strsplit," ",USE.NAMES=F) # sapply: 리스트를 벡터로 변환
rules

# 3) 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind",rules) # do.call: 다차원 리스트를 행단위로 바인딩
class(rulemat)

# 4) 연관어 시각화를 위한 igraph 패키지 설치와 로딩
  # 라이브러리 모음 참조

# 5) edgelist 보기
ruleg <- graph.edgelist(rulemat[c(12:59), ], directed = F)
ruleg

# 6) edgelist 시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
vertex.label.cex = 1.2, vertext.label.color = 'black',
vertex.size = 20, vertext.color = 'green',
vertex.frame.co.or = 'blue')




## 실시간 뉴스 수집과 분석 ####################################################
# 1) 패키지 설치 및 준비
  # 라이브러리 모음 참조

# 2) url 요청
url <- "http://media.daum.net"
web <- GET(url)
web

# 3) HTML 파싱
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html)

# 4) 태그 자료 수집
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']",xmlValue)
news

# 5) 수집한 자료 전처리
news_pre <- gsub("[\r\n\t]",'',news)
news_pre <- gsub("[[:punct:]]",'',news_pre)
news_pre <- gsub("[[:cntrl:]]",'',news_pre)
news_pre <- gsub('[a-z]+','',news_pre)
news_pre <- gsub('[A-Z]+','',news_pre)
news_pre <- gsub('\\s+','',news_pre)
# news_pre <- gsub('\\d+','',news_pre) #corona19 때문에 숫자 제거 생략
news_pre # 결과확인

# 6) 결과 저장
news_data <- news_pre[1:59]
news_data

# 6) 파일 저장 및 읽기
# 수집한 자료를 파일로 저장하고 읽기
getwd()
setwd('c:/rwork/')
write.csv(news_data, "news_data.csv", quote = F)
news_data <- read.csv("news_data.csv", header = T, stringsAsFactors = F)
str(news_data)

names(news_data) <- c("no", "news_text")
head(news_data)

news_text <- news_data$news_text
news_text




## 토픽 분석 ##################################################################
# 세종 사전에 단어 추가
user_dic <- data.frame(term = c("팬데믹","코로나19","타다"),tag='ncn')
buildDictionary(ext_dic = 'sejong',user_dic = user_dic)

# 단어 추출 사용자 함수 정의하기
# 1) 사용자 정의 함수 작성
exNouns <- function(x){paste(extractNoun(x),collapse = " ")}

# 2) exNouns() 함수로 단어 추출
news_nouns <- sapply(news_text,exNouns)
news_nouns

# 3) 단어 추출 결과 확인
str(news_nouns)


# 말뭉치 생성과 집계 행렬 만들기
# 1) 추출된 단어를 이용한 말뭉치(corpus) 생성
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
inspect(newsCorpus[1:5])

# 2) 단어 vs 문서 집계 행렬 만들기
TDM <- TermDocumentMatrix(newsCorpus,control = list(wordLengths = c(4,16)))
TDM

# 3) 매트릭스 자료구조를 데이터 프레임으로 변경
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)


# 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:10]


# 단어 구름 생성
# 1) 단어 이름 추출
myNames <- names(wordResult)
myNames

# 2) 단어와 단어 빈도수 구하기
df <- data.frame(word = myNames, freq = wordResult)
head(df)

# 3) 단어 구름 생성
pal <- brewer.pal(12, "Paired")
wordcloud(df$word, df$freq, 
          min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family = "malgun")



