#Exercise: Cofidence Interval --------------------------------------------------------------------

library(downloader)
library(dplyr)
library(readr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile = filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke =="0" ) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies, smoke=="1") %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#
set.seed(1)
N <- 25
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
obs <- mean(dat.ns)- mean(dat.s)
se <- sqrt(var(dat.ns)/N + var(dat.s)/N)
tval <- obs/se
#t-distribution, add and subtract to obtain a 99% confidence interva
df <- 2*N-2
qt(0.995, df)*se 
#
set.seed(1)
N <- 5
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
obs <- mean(dat.ns)- mean(dat.s)
se <- sqrt(var(dat.ns)/N + var(dat.s)/N)
t.test(dat.ns, dat.s)

#-------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#Power Calculations
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex == "F" & Diet == "chow")%>% select(Bodyweight)%>% unlist
hfPopulation <- filter(dat, Sex == "F" & Diet == "hf")%>% select(Bodyweight)%>% unlist
mu_control <- mean(controlPopulation)
mu_hf <- mean(hfPopulation)
print( mu_hf - mu_control)
#if we do a statistical test and we are successful, 
# we should reject the null hypothesis because the difference in weight
print((mu_hf - mu_control)/mu_control *100) #percent increase

set.seed(1)
N <- 5
hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
t.test(hf, control)$p.value

N <- 12
alpha <- 0.05
B <- 2000
reject <- function(N, alpha = 0.05){
  hf <- sample(hfPopulation, N)
  control <- sample(controlPopulation, N)
  pval <- t.test(hf, control)$p.value
  pval < alpha

}
reject(100)
rejections <- replicate(B, reject(100))
mean(rejections) # how many times rejected
Ns <- seq(50, 100, 5)

power <-sapply(Ns, function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})

plot(Ns, power, type="b")

#-------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#Exercise: Power Calculations
library(downloader)
library(dplyr)
library(readr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile = filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke =="0" ) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies, smoke=="1") %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#1
set.seed(1)
N <- 5
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
obs <- mean(dat.ns)- mean(dat.s)
se <- sqrt(var(dat.ns)/N + var(dat.s)/N)
pval <- t.test(dat.ns, dat.s)$p.value

#2
set.seed(1)
reject <- function(N, alpha=0.01){
  dat.ns <- sample(bwt.nonsmoke, N)
  dat.s <- sample(bwt.smoke, N)
  pval <- t.test(dat.ns, dat.s)$p.value
  pval < alpha
  
}
rejections <- replicate(10000, reject(5))
mean(rejections)

#3
Ns <- seq(30, 120, 30)
power <- sapply(Ns, function(N){
  rejections <- replicate(10000, reject(N))
  mean(rejections)
})
plot(Ns, power, type= "b")
Ns[which.min(abs(power- 0.8))]