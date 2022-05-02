install.packages("moonBook")
library("moonBook")
install.packages("psych")
library(psych)
getwd()
library(tidtverse)
#data 불러오기
data_na1 = read.csv('/Users/joohyunlee/2021_PEERS/main_outcome/data/peers_na.csv')

#data 기술통계
describe(data_na1)

#속성변경
data_na1$time <- factor(data_na1$time,
levels = c("1","2"),
labels = c("before_treatment","after_treatment"))

data_na1$Sex<-factor(data_na1$Sex,
                    levels = c("1","2"),
                    labels = c("Male","Female"))

data_na1$Group<-factor(data_na1$Group,
                    levels = c("1","2"),
                    labels = c("Experimental","Control"))

# Group과 pre-post 치료에 따라 분석하기
##total 값들
mytable(Group+time ~ MAAS_total + CDI_total + RCMAS_total + SRS_total+ SCQ + CBCL_Total
, data = data_na1, method= 2, max.ylev = 1)
##vilneland 값
mytable(Group+time ~ Community + Daily_living_skill + Socialization, data = data_na1 , method=2, max.ylev = 1)
str(data_na1)
##SCL-R값
mytable(Group+time ~ + Depression + Anxiety + Panic + Agoraphobia + Obsessive + Obsessive_compulsive_personality + PTSD 
+ Anger_attack + Somatic + Mania + Paranoia + Psychosis + Suicide + Addiction + Sleep_disorder
+ Inter_personal_sensitivity, data = data_na1, method =2, max.ylev = 1)

##SRS(primary outcome) subgroup
mytable(Group+time ~ + Social.Awareness + Social.Cognition +Social.Communication+Social.Motivation+Autistic.Mannerism , data = data_na1, method =2, max.ylev = 1)

## CDI SUBGROUP
mytable(Group+time ~ + 부정적자아상+ 대인관계문제+ 기분신체증상
, data = data_na1, method =2, max.ylev = 1)

#RCMAS subgroup
mytable(Group+time ~ + 과도한걱정+예민함+신체수면문제+정서주의력
, data = data_na1, method =2, max.ylev = 1)

# experimenta group만 비교해보기 
data_na2 = data_na1[data_na1$Group=='Experimental',]
data_na2
mytable(time ~ MAAS_total + CDI_total + RCMAS_total + SRS_total+ SCQ + CBCL_Total
, data = data_na2, method= 2, max.ylev = 1)

str(data_na1)
## SRS값만 뽑기

data_SRS1 <- subset(data_na1, select=c("id","time","Group","SRS_total"))

# id별로 SRS 그래프 그리기 (pre-post 차이, experimental group)

id1 <- c(96, 84)
id2 <- c(70, 85)
id3 <- c(80, 99)
id4 <- c(112, 103)
id5 <- c(125, 117)
plot(id1, type="o", col="red", ylim=c(60,150), axes=F, ann=F,lty=2)
axis(1, at=1:2, lab=c("before_Tx","after_Tx"))
axis(2, ylime=c(60,150))
title(main="SRS_total",col.main="black")
title(xlab = "Time", col.lab="black")
title(ylab = "SRS_score", col.lab="black")
lines(id2, type="o", pch=21, col="blue")
lines(id3, type="o", pch=21, col="yellow")
lines(id4, type="o", pch=21, col="green", lty=2)
lines(id5, type="o", pch=21, col="purple", lty=2)
legend(4,100,c("id1","id2","id3","id4","id5"), cex=0.8, col=c("red","blue","yellow","green","purple"), pch=21, lty=1:5)