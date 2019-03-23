#Permutations Exercises------------------------------------------------------
library(downloader)
library(dplyr)
library(readr)
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
#obs <- mean(smokers) - mean(nonsmokers)
obs <- median(smokers) - median(nonsmokers)
#----permutationa reshuffle dat and mean----------
set.seed(1)
dat <- c(smokers,nonsmokers)
tstats <- replicate(1000,{
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
#the proportion of permutations with larger difference: P-value
(sum (abs(tstats) >=abs(obs))+1) / ( length(tstats)+1 )
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(tstats) >= abs(obs)) ) / ( length(tstats) )
#

