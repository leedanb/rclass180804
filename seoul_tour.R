library(rJava)
library(DBI)
library(RJDBC)
library(XML)
library(memoise)
library(KoNLP)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rvest)
library(RColorBrewer)
library(data.table)
library(reshape)
library(stringr)

getwd()
list.files()
useSejongDic()
target<-readLines("seoul_go.txt")
target<-gsub("\\d+","",target)
target<-sapply(target, extractNoun, USE.NAMES = F)
target<-unlist(target)
myGsub<-readLines("myGsub.txt")
i<-1
for(i in 1:length(myGsub)){
  target<-gsub(myGsub[i],"",target)
}
target <- Filter(function(x){!nchar(x)==1}, target)  
target <- Filter(function(x){nchar(x)<=10}, target)
head(sort(target,decreasing=T),20)
write(unlist(target), "seoul_go2.txt")
target <- read.table("seoul_go2.txt")
target <- table(target)
head(sort(target, decreasing=T), 20)
pal <- brewer.pal(8, "Dark2")
set.seed(1234)
wordcloud::wordcloud(names(target),
                     freq = target,
                     min.freq = 1,      ##최소 빈도
                     random.order = F,  ## T를 주면 원하는 단어 우선으로 찾는 것
                     rot.per = 0.25,     ##중심 단어를 기준으로 25%정도 각을 준 것 1이면 90도
                     scale = c(5,1),   ## 글자 크기. 빈도가 많으면 5크기, 적으면 1크기
                     colors = pal)

legend(
  0.3,
  1,
  "제주도 추천 여행 코스 분석",
  cex=0.8,
  fill=NA,
  border=NA,
  bg="white",
  text.col="red",
  #text.font=2 #cex와 같은 기능,
  box.col="red"
)
