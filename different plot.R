library(downloader)
library(dplyr)
library(readr)
library(rafalib)
load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat[,i])
}

par(mfrow=c(3,3))
for (i in 1:9) {
  hist(dat[,i])
}

##
head(InsectSprays)
A <- filter(InsectSprays, spray=="A")%>% select(count)%>%unlist
B <- filter(InsectSprays, spray=="B")%>% select(count)%>%unlist
C <- filter(InsectSprays, spray=="C")%>% select(count)%>%unlist
D <- filter(InsectSprays, spray=="D")%>% select(count)%>%unlist
E <- filter(InsectSprays, spray=="E")%>% select(count)%>%unlist
FI <- filter(InsectSprays, spray=="F")%>% select(count)%>%unlist

mypar(2,3)
boxplot(A)
boxplot(B)
boxplot(C)
boxplot(D)
boxplot(E)
boxplot(FI)
#
boxplot(InsectSprays$count ~ InsectSprays$spray)
#

library(dplyr)
data(nym.2002, package="UsingR")
head(nym.2002)
mypair(1,2)
mypar(1,3)
boxplot(nym.2002$time ~ nym.2002$gender)
hist(filter(nym.2002, gender=="Female")%>%select(time)%>% unlist , xlim=c(range( nym.2002$time)))
hist(filter(nym.2002, gender=="Male")%>%select(time)%>% unlist , xlim=c(range( nym.2002$time)))

##
#
library(rafalib)
mypar(2,2)
females <- filter(nym.2002, gender=="Female")
males <- filter(nym.2002, gender=="Male")

plot(females$age, females$time)
plot(males$age, males$time)
group <- floor(females$age/5) * 5
boxplot(females$time~group)
group <- floor(males$age/5) * 5
boxplot(males$time~group)

##
time <-sort(nym.2002$time)
max(time)/median(time)

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)
