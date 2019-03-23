# Monte Carlo Stimulation -------------------------------------------------------------------
library(downloader)
library(dplyr)
library(readr)
library(rafalib)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex == "F" & Diet == "chow")%>% select(Bodyweight)%>% unlist
controlPopulation <- unlist(controlPopulation)

ttestgenerator <- function(n){
  cases <- sample(controlPopulation, n)
  controls <- sample(controlPopulation, n)
  tstat <- (mean(cases)- mean(controls))/ sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}
ttests <- replicate(1000, ttestgenerator(10))
hist(ttests)

qqnorm(ttests)
abline(0,1)

ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps, df = 2*3-2), ttests, xlim = c(-6,6), ylim = c(-6,6))
abline(0,1)

qqnorm(controlPopulation)
qqline(controlPopulation)
#
controls <- rnorm(5000, mean= 24, sd= 3.5)
ttestgenerator <- function(n, mean= 24, sd= 3.5){
  cases <- rnorm(n, mean, sd)
  controls <- rnorm(n, mean, sd)
  tstat <- (mean(cases)- mean(controls))/ sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}
ttests <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
abline(0,1)

hist(ttests)