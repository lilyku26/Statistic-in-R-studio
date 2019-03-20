#Week3 for R--------------------------------------------
# Central Limit Theorem
#-------------------------------------------------------
library(readr)
library(dplyr)
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)
set.seed(1)
n <- 1000
nulls <- vector("numeric",n)
nulls1 <- vector("numeric",n)

for(i in 1:n){
  control <- sample(population, 5)
  nulls[i] <- mean(control) 
}
for(i in 1:n){
  control1 <- sample(population, 50)
  nulls1[i] <- mean(control1) 
}
#--------------------------------------------------
#Normal distribution
pnorm(23.9, 0.43)
# Z units
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 

#--------------------------------------------------
#--------------------------------------------------
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
#remove missing data
dat <- na.omit( dat )
x <- filter(dat,Sex=="F" ,Diet=="chow" )%>%select(Bodyweight)%>% unlist
mean(x)
#--------------------------------------------------
library(rafalib)
popsd(x)
#-------------------------------------------------
set.seed(1)
X <- sample(x,25)
mean(X)
#-------------------------------------------------
y <- filter(dat,Sex=="F" ,Diet=="hf" )%>%select(Bodyweight)%>% unlist
mean(y)
popsd(y)
#-------------------------------------------------
set.seed(1)
Y <- sample(y,25)
mean(Y)
#-------------------------------------------------
abs((mean(y)-mean(x))-(mean(Y)-mean(X)))
#----------------------------------------------------------
#----------------------------------------------------------
#proportion of these numbers are within n standard deviations away from the list's average
pnorm(3)-pnorm(-3)
#--
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )
y <- filter(dat,Sex=="M",Diet=="chow" )%>%select(Bodyweight)%>% unlist
popsd(y)
z <- (y- mean(y))/popsd(y)
mean(abs(z) <=3)
qqnorm(z)
abline(0,1)
#-------------------------------------------------------------
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
#------------------------------------------------------------
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
popsd(avgs)
#---------------------------------------------------------------
#----------------------------------------------------------------
