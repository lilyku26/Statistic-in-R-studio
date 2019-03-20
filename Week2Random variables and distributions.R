#Week2 for R--------------------------------------------
# Random variables and Probability distributions
#-------------------------------------------------------
library(readr)
library(dplyr)
filename <- ("femaleMiceWeights.csv")
dat <- read_csv(filename)
control <-filter(dat, Diet == "chow")%>%select(Bodyweight)%>%unlist
treatment <- filter(dat,Diet=="hf")%>% select(Bodyweight)%>% unlist

obs <- mean(treatment) - mean(control)
#--------------------------------------------------------
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)
set.seed(1)
n <- 1000
nulls <- vector("numeric",n)
for(i in 1:n){
  control <- sample(population, 50)
  nulls[i] <- mean(control) 
}
sum(abs(nulls - mean(population)) > 1 )
#--------------------------------------------------------
library(gapminder)
data(gapminder)
head(gapminder)
control <- filter(gapminder, year == "1952")%>% select(lifeExp)%>% unlist
mean(control<= 60)-mean(control<= 40)
#--------------------------------------------------------
#﹚竡propㄧ计lifeExp < 琘计┮ゑㄒ
prop = function(q){
  mean(control<=q)
}
#眖程程20翴
qs= seq(from=min(control),to=max(control),length=20)
#ノ硂20翴prop
props = (sapply(qs,prop))
#酶籹ㄧ计
plot(qs.props)
# props = sapply(qs, function(q) mean(control <= q))
# plot(ecdf(control)) 
# ㄧ计琌empirical cumulative distribution function