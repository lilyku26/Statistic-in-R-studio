library(downloader)
library(dplyr)
library(readr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile = filename)
babies <- read.table("babies.txt", header = TRUE)
#------------------------------------------------------
#split in two datasets
bwt.nonsmoke <- filter(babies, smoke == "0")%>%select(bwt)%>% unlist
bwt.smoke <- filter(babies, smoke == "1")%>%select(bwt)%>% unlist
library(rafalib)
obs1 <- mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#-------------------------------------------------------
set.seed(1)
N <-25
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
obs <- mean(dat.ns)-mean(dat.s)
se <- sqrt(var(dat.ns)/N + var(dat.s)/N)
tval <- obs/se
2*(1-pnorm(tval))
#2*pnorm(-abs(tval))
#qnorm:信賴區間的分布函數
qnorm(0.995)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )

#
#-------------------------------------------------------
set.seed(1)
chowPopulation <- read.csv("femaleControlsPopulation.csv")
chowPopulation <- unlist(population)

mu_chow <- mean(chowPopulation)
print(mu_chow)

N <-30
chow <- sample(chowPopulation, N)
print(mean(chow))
se <- sqrt(var(chow)/N)
print(se)

Q <- qnorm(1-0.05/2)
#-Q < (mean(chow)-mean(chowPopulation))/se < Q

interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
interval
interval[1] < mu_chow & interval[2] > mu_chow

library(rafalib)
B <- 250
mypar()
plot(mean(chowPopulation)+c(-7,7), c(1,1),type="n",xlab="weught",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
for (i in 1:B){
  chow <- sample(chowPopulation, N)
  se <- sqrt(var(chow)/N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <-mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered, 1, 2)
  lines(interval, c(i,i), col=color)
}
#----------
mypar()
Q <- qt(1-0.05/2, df=4)
N <- 5
B <- 250

plot(mean(chowPopulation)+c(-7,7), c(1,1),type="n",xlab="weught",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
for (i in 1:B){
  chow <- sample(chowPopulation, N)
  se <- sqrt(var(chow)/N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <-mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered, 1, 2)
  lines(interval, c(i,i), col=color)
}