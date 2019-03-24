#Mann-Whitney-Wilcoxon Test Exercises----------------------------
#
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",direction="wide")
head(chick)
chick = na.omit(chick)
x <- filter(chick, Diet == "1" )%>% select(weight.4)%>%unlist
x1 <- c(x,200)
y <- filter(chick, Diet == "4" )%>% select(weight.4)%>%unlist
y
wilcox.test(x,y+100)
t1 <- t.test(x,y+10)$statistic 
t2 <- t.test(x,y+100)$statistic
t1-t2 #subtract ´îªk
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
##
wilcox.test(x,y+10)
wilcox.test(x,y+100)
x<-c(1,2,3)
y<-c(400,500,600)
wilcox.test(x,y)$p.value
