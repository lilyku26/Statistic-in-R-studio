library(readr)
library(dplyr)
filename <- ("femaleMiceWeights.csv")
dat <- read_csv(filename)
control <-filter(dat, Diet == "chow")%>%select(Bodyweight)%>%unlist
treatment <- filter(dat,Diet=="hf")%>% select(Bodyweight)%>% unlist
N <- length(treatment)
obs <- mean(treatment) - mean(control)
#standard error estimate se
se <- sqrt(var(treatment)/N + var(control)/N)
# form a t-statistic by simply dividing the observed difference
tstat <- obs/se
# p-value (what proportion of normally distributed data are lower than whatever value you put here)
# p- value = 1- pnorm(x)
1 - pnorm(tstat)
2*(1- pnorm(tstat))
##---------------------------------------
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)
set.seed(1)
n <- 1000
nulls <- vector("numeric",n)
for(i in 1:n){
  control <- sample(population, N)
  treatment <-sample(population, N)
  se <- sqrt(var(treatment)/N + var(control)/N)
  nulls[i] <- (mean(control)-mean(treatment))/se
}
library(rafalib)
mypar()
qqnorm(nulls)
abline(0,1)
qqline(nulls)
##------------------------------------------
# t- distribution approximation
library(readr)
library(dplyr)
filename <- ("femaleMiceWeights.csv")
dat <- read_csv(filename)
control <-filter(dat, Diet == "chow")%>%select(Bodyweight)%>%unlist
treatment <- filter(dat,Diet=="hf")%>% select(Bodyweight)%>% unlist
ttest <- t.test(treatment, control)
ttest
library(rafalib)
mypar(1,2)
qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)

##
##-------------------------------------------
set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x<-sample(1:sides, n, replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
})
mypar()
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)
#-------------------------------
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}
#--------------------------------------------
library(downloader)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mu_x <- mean(X)
mu_y <- mean(Y)
2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )
se <- sqrt(var(Y)/12 + var(X)/12)
tstat <- (mean(Y)-mean(X))/se
2*(1-pnorm(tstat))
ttest <- t.test(Y,X)
mypar(1,2)
qqnorm(X)
qqnorm(Y)