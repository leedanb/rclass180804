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

## ggplot
# ggplot2 문법 구성 기본요소
# 데이터 프레임 data.frame
# 색상, 크기 같은 기하학적 요소 aes
# 점, 선, 모양 같은 기하학적 요소 geoms
# 통계적 처리방법 stats
# aes 에서 사용할 스케일 (scale)
## ggplot(df, aes(x=val, y=val))+geom

getwd()
list.files()
tKorean <- read.table(
  "학생별국어성적.txt",
  header = T,    #이름, 과목, 점수 등 이름이 있다는 것을 의미
  sep = ",")
ggplot2::ggplot(
  tKorean,
  aes(x = 이름, y = 점수)) +
  geom_point()

ggplot2::ggplot(
  tKorean,
  aes(x = 이름, y = 점수)) +
  geom_bar(
    stat = "identity",
    fill = "green",
    color = "red") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      color = "blue",
      size = 8
    )
  )

getwd()
list.files()
tScores <- read.csv("학생별과목별성적_국영수.csv",
                    header = T,     #csv파일은 sep가 이미 있으므로 따로 값을 안 줘도 됨
                    )
tScores
tScores <- arrange(tScores, 이름, 과목)
tScores
install.packages("plyr")
library(plyr)
tScores <- ddply(
  tScores,
  "이름",
  transform,
  누적합계 = cumsum(점수),
  label = cumsum(점수)-0.5*점수)
tScores

ggplot2::ggplot(
  tScores,
  aes(x=이름, y=점수, fill=과목)
)+
  geom_bar(stat = "identity") +
  geom_text(
    aes(y=label, label = paste(점수, '점')),
    color = "black",
    size = 4
  )+
  guides(
    fill = guide_legend(reverse = T)
  )+
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      color = "black",
      size = 8
    )
  )

getwd()
list.files()
tAllScores<- read.table("학생별전체성적.txt",
                        header = T, sep=",")
tAllScores[,c("이름","영어")]   #행,열   행은 다 가져오고 열은 이름과 영어만 가져오라는 말
