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

#plot(x/y축 data)
vDemo<-c(1:5)
vDemo
plot(vDemo)

#plot(x축data, y축data)
plot(1:3, 3:1)

#plot(1:3, 3:1, xlim=c(1,10), ylim=c(1,5))
#lim 최대 한계값을 의미
plot(1:3, 3:1, xlim=c(1,10), ylim=c(1,5))

#x,y 축 제목, 그래프 제목 지정해서 출력
#lab = label
#main = 중앙 제목
plot(1:3, 3:1,
  xlim=c(1,10),
  ylim=c(1,5),
  xlab="X축 값",
  ylab="Y축 값",
  main= 'PLOT값 테스트'
)

# plot.new() 기존 그래프 지우고 새로 그리기
# dev.new() 새창에서 띄우기

##꺽은선 차트
vDemo<-c(100,130,120,160,150)
plot(
  vDemo, ##대상 object
  type = 'o', ##점과 선을 중첩시킴
  col = 'red',
  ylim = c(0,200),
  axes = F, # False이면 축을 숨김
  ann = F # False이면 축의 이름을 정하지 않음
)

axis(
  1,
  at = 1:5,
  lab = c("월", "화", "수", "목", "금")
)
axis(
  2,
  ylim = c(0,200)
)
title(
  main="과일",
  col.main="red", #제목의 글자색
  front.main=4 #글자크기
)
title(
  xlab='요일',
  col.lab="blacK"
)
title(
  ylab='가격',
  col.lab="blue"
)

##그래프를 배치해서 보여주기
vDemo<-c(100,130,120,160,150)
par(mfrow=c(1,3))  # 3개를 병렬 배치하겠다는 뜻 par가 병렬배치
plot(vDemo, type="o")
plot(vDemo, type="s")
plot(vDemo, type="l")

##타입정리
#p -> 점 point, default
#l -> 선 line
#b -> 점, 선
#c -> b에서 점 생략
#o -> 점과 선을 중첩함
#h -> 점에서 x축까지 수직선 그래프
#s -> 왼쪽값을 기초로 계단모양으로 연결한 그래프
#n -> 축만 그리고 그래프는 없음

##mfrow = c(x,y) 그래프의 배치 조정
#x는 행의 수, y는 열의 수


plot.new()
vDemo<-c(100,130,120,160,150)
par(mfrow=c(1,3)) 
pie(vDemo)
plot(vDemo, type="o")
barplot(vDemo)

## mgp = c(제목위치, 지표값위치, 지표선위치)
par(mgp = c(0,1,0)) #mgp를 썼더니 선이 아닌 점으로 찍힘
plot(
  vDemo,
  xlab = "TEST"
)

par(mgp = c(3,1,0)) 
plot(
  vDemo,
  xlab = "TEST"
)

par(mgp = c(3,2,0)) 
plot(
  vDemo,
  xlab = "TEST"
)

par(mgp = c(3,2,1)) 
plot(
  vDemo,
  xlab = "TEST"
)

##oma option 그래프 전체 여백을 조정
plot.new()
par(oma = c(2,1,0,0)) 
plot(
  vDemo,
  xlab = "TEST"
)

par(oma = c(0,2,0,0)) 
plot(
  vDemo,
  xlab = "TEST"
)

##이전 실습에서 3개로 출력한 것을 1개로 만들기 위한 예제
plot.new()
par(mfrow=c(1,1))  # 1행 1열로 바꿔라(분할 했던 화면을 원위치로 만드는 것)
t1 <- c(1, 2, 3, 4)
t2 <- c(5, 4, 3, 2, 1)
t3 <- c(3, 4, 5, 6, 7)
plot(
  t1,
  type="s",
  col="red",
  ylim=c(1,5)
)

lines(
  t2,
  type = 'o',
  col= 'blue',
  ylim=c(1,5)
)

lines(
  t3,
  type = 'l',
  col= 'green',
  ylim=c(1,5)
)

##새로 그려질때마다 x축 제목과 ylim 값이 새롭게 적용
##따라서, 아래와 같이 lines()를 사용해 보다 쉽게 그림

legend(
  3.5, #x축 위치
  5, #y축 위치
  c("국", "영", "수"),
  cex=0.9, #글자크기
  col=c("red", "blue", "green"), #컬러
  lty = 1 #라인타입
)

##막대그래프 그리기
#랜덤값 출력 runif(갯수, 시작값, 끝값)
vDemo <-runif(5,1,5)
vDemo
vDemo<-round(runif(5,1,5),0)
vDemo
barplot(vDemo)

##수평방향  horiz=수평선
barplot(vDemo, horiz = T)

##그룹으로 묶어서 출력
v1<-c(5,4,3,2)
v2<-c(5,3)
v3<-c("green", "yellow")
m1<-matrix(v1,2,2) #2행2열의 매트릭스
m1
barplot(
  m1,
  beside=T,
  nmaes=v2,
  col=v3
)

##그룹으로 묶어서 가로로 출력
v1<-c(5,4,3,2)
v2<-c("이전","이후")
v3<-c("green","yellow")
v4<-c(1,0.5,1,0.5)
m1<-matrix(v1,2,2)
par(oma=v4) #그래프여백 6시, 9시, 12시, 3시
barplot(
  m1,
  beside=T,
  names=v2,
  col=v3,
  horiz=T
)

##bar에 두개의 값을 합서
plot.new()
v1<-c(5,4,3,2)
v2<-c("이전","이후")
v3<-c("green","yellow")
v4<-c(0,12)
m1<-matrix(v1,2,2)
barplot(
  m1,
  xlim = v4,
  names=v2,
  col=v3,
  horiz=T
)

## 과일가격 응용
vBanana <- round(runif(5,100,180),0)
vCherry <- round(runif(5,100,180),0)
vOrange <- round(runif(5,100,180),0)
dFruit <- data.frame(
  바나나 = vBanana,
  체리 = vCherry,
  오렌지 = vOrange
)
vYlim<-c(0,400)
vDay<-c("월","화","수","목","금")
dFruit
#바나나, 체리, 오렌지 를 빼고 숫자만 계산하게 만들기 위해 as.matrix 명령어를 사용
barplot(
  as.matrix(dFruit),
  main = "과일판매량",
  beside = T,
  col = rainbow(nrow(dFruit)),  ##nrow는 행의 수만큼. 즉, 행의 수만큼 무지개 색을 입혀라
  ylim = vYlim
)
legend(
  14,
  400,
  vDay,
  cex = 0.8, ##글자크기
  fill = rainbow(nrow(dFruit))
)
## 무지개색으로 하되 개수는 dFruit변수 안에 있는 값의 개수로 하라

##하나의 바에 값을 합치기
plot.new()
vBanana <- round(runif(5,100,180),0)
vCherry <- round(runif(5,100,180),0)
vOrange <- round(runif(5,100,180),0)
dFruit <- data.frame(
  바나나 = vBanana,
  체리 = vCherry,
  오렌지 = vOrange
)
vYlim<-c(0,400)
vDay<-c("Mon","Tue","Wed","Thu","Fri")
dFruit

barplot(
  t(dFruit), #t() = 전치함수. 행과 열의 위치를 전환하는 것
  main = "qty",
  col = rainbow(nrow(dFruit)),  
  ylim = c(0,600),
  space = 0.1,
  cex.axis = 0.8,
  las = 1,
  names.arg = vDay,
  cex = 0.8
)

legend(
  0.2,
  600,
  names(dFruit),
  cex = 0.8, ##글자크기
  fill = rainbow(nrow(dFruit))
)

dFruit
t(dFruit)
##
plot.new()
vPeach <- round(runif(5,150,250),0)
vPeach
vDay<-c("Mon","Tue","Wed","Thu","Fri")
vColor<-c()
vLen<-c(1:length(vPeach))
for(i in vLen){
  if(vPeach[i]>=200){
    vColor <- c(vColor, "red")
  }else if(vPeach[i]>=180){
    vColor <- c(vColor, "yellow")
  }else{
    vColor <- c(vColor, "green")
  }
}
barplot(
  vPeach,
  main = "peach Qty",
  names.arg = vDay,
  col = vColor
)

#학급에 총 10명의 학생
vHeight1 <- round(runif(2,150,160))
vHeight2 <- round(runif(3,161,170))
vHeight3 <- round(runif(3,171,180))
vHeight4 <- round(runif(2,181,190))
vHeight <- c(vHeight1, vHeight2, vHeight3, vHeight4)

##히스토그램
hist(
  vHeight,
  main = "height"
)

barplot(
  vHeight,
  main = "height"
)

##파이차트
vPie<-c(10,20,30,40)
pie(
  vPie,
  radius = 1)   ##radius는 각도

#시작각도를 90도로 지정하기
plot.new()
vPie<-c(10,20,30,40)
pie(
  vPie,
  radius = 1,
  init.angle = 90    ##12시부터 시작하게 만드는 것. 초기값 각도라는 뜻
  )

#색깔과 라벨명 지정
plot.new()
vPie<-c(10,20,30,40)
vDay<-c("Mon","Tue","Wed","Thu","Fri")
pie(
  vPie,
  radius = 1,
  init.angle = 90,
  col = rainbow(length(vPie)),
  label = vDay
)

#수치값을 출력
plot.new()
vPie<-c(10,20,30,40)
vDay<-c("Mon","Tue","Wed","Thu","Fri")
vPct<-round(vPie/sum(vPie)*100,1)
vLab<-paste(vPct, " %")
pie(
  vPie,
  radius = 1,
  init.angle = 90,
  col = rainbow(length(vPie)),
  label = vLab
)
legend(
  1.3,
  1,
  vDay,
  cex = 0.8,
  fill = rainbow(length(vPie))
)

#3D파이
install.packages("plotrix")   ##3차원 그래프 만드는 패키지
library(plotrix)

plot.new()
vPie<-c(10,20,30,40)
vDay<-c("Mon","Tue","Wed","Thu","Fri")
vPct<-round(vPie/sum(vPie)*100,1)
vLab<-paste(vPct, " %")
pie3D(
  vPie,
  main = "3D PIE CHART",
  col = rainbow(length(vPie)),
  cex = 0.5,
  labels = vLab,
  explode = 0.05
)

legend(
  0.8,
  1,
  vDay,
  cex = 0.6,
  fill = rainbow(length(vPie))
)
