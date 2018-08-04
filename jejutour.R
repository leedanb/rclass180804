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
target<-readLines("jeju.txt")
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
write(unlist(target), "jeju2.txt")
target <- read.table("jeju2.txt")
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

top<-head(
  sort(target,decreasing=T),
  50
)
top
top10<-c("중문","주상절리","성산","천지연폭포","한라산",
         "산방산","섭지코지","송악산","신창","오설록")
top10count<-c(13,12,10,9,9,8,6,6,7,5)

##꺽은선 차트
vJeju<-top10count
plot.new()
plot(
  vJeju, ##대상 object
  type = 'o', ##점과 선을 중첩시킴
  col = 'red',
  ylim = c(0,15),
  axes = F, # False이면 축을 숨김
  ann = F # False이면 축의 이름을 정하지 않음
)

axis(
  1,
  at = 1:10,
  lab = top10
)
axis(
  2,
  ylim = c(0,15)
)
title(
  main="제주여행",
  col.main="red", #제목의 글자색
  front.main=4 #글자크기
)
title(
  xlab='장소',
  col.lab="blacK"
)
title(
  ylab='빈도수',
  col.lab="blue"
)


#병렬배치
par(mfrow=c(1,3))  # 3개를 병렬 배치하겠다는 뜻 par가 병렬배치
plot(vJeju, type="o")
plot(vJeju, type="s")
plot(vJeju, type="l")

plot.new()
par(mfrow=c(1,3)) 
pie(vJeju)
plot(vJeju, type="o")
barplot(vJeju)

#제목, 지표값, 지표선 위치
par(mgp = c(3,2,1)) 
plot(
  vJeju,
  xlab = "place")

##막대그래프 그리기
barplot(vJeju)
axis(
  1,
  at = 1:10,
  lab = top10
)
axis(
  2,
  ylim = c(0,15)
)
title(
  main="제주여행",
  col.main="red", #제목의 글자색
  front.main=4 #글자크기
)
title(
  xlab='장소',
  col.lab="blacK"
)
title(
  ylab='빈도수',
  col.lab="blue"
)

##수평방향  horiz=수평선
barplot(vJeju, horiz = T)
title(
  main="제주여행",
  col.main="red", #제목의 글자색
  front.main=4 #글자크기
)
title(
  xlab='빈도수',
  col.lab="blacK"
)
title(
  ylab='장소',
  col.lab="blue"
)
plot.new()

##바그래프 색깔 입히기
plot.new()
top10<-c("중문","주상절리","성산","천지연폭포","한라산",
         "산방산","섭지코지","송악산","신창","오설록")
top10count<-c(13,12,10,9,9,8,6,6,7,5)
dJeju <- data.frame(
  중문 = 13,
  주상절리 = 12,
  성산 = 10,
  천지연폭포 = 9,
  한라산 = 9,
  산방산 = 8,
  섭시코지 = 6,
  송악산 = 6,
  신창 = 7,
  오설록 = 5
)
vYlim<-c(0,15)
dJeju
barplot(
  as.matrix(dJeju),
  main = "제주여행",
  beside = T,
  col = rainbow(nrow(dJeju)),  
  ylim = vYlim
)

legend(
  17,
  15,
  top10,
  cex = 0.8, ##글자크기
  fill = rainbow(nrow(dJeju))
)

##조건으로 색 입히기
plot.new()
top10<-c("중문","주상절리","성산","천지연폭포","한라산",
         "산방산","섭지코지","송악산","신창","오설록")
top10count<-c(13,12,10,9,9,8,6,6,7,5)
vColor<-c()
vLen<-c(1:length(top10count))
for(i in top10count){
  if(top10count[i]>=10){
    vColor <- c(vColor, "red")
  }else if(top10count[i]>=7){
    vColor <- c(vColor, "yellow")
  }else{
    vColor <- c(vColor, "green")
  }
}
barplot(
  top10count,
  main = "제주여행",
  names.arg = top10,
  col = vColor
)
