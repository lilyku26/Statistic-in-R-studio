# Exercise Monte Carlo Stimulation -------------------------------------------------------------------
library(downloader)
library(dplyr)
library(readr)
library(rafalib)
set.seed(1)
x <- rnorm(5,mean=0, sd= 1)
mu_x <- mean(x)
sd_x <- sqrt(var(x)/5)
tstat <- mu_x / sd_x
print(tstat)
#
set.seed(1)
N <- 5
B <- 1000
tstats <- replicate(B, {
  x <- rnorm(N,mean=0, sd= 1) 
  mu_x <- mean(x)
  sd_x <- sqrt(var(x)/5)
  tstat <- mu_x / sd_x
  return(tstat)
})
tstats
y <- replicate(1000,1- pt(2, df= 4))
qqplot(y, tstats)

#
B=100; ps = seq(1/(B+1), 1-1/(B+1),len=B)
y <- qt(ps,df=4)

set.seed(1)
N <-5
ttestgenerator <- replicate(1000,{
  x <- rnorm(N,mean=0, sd= 1)
  mu_x <- mean(x)
  sd_x <- sqrt(var(x)/N)
  tstat <- mu_x / sd_x
  return(tstat)
})
qqnorm(ttestgenerator)
abline(0,1)

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

#
set.seed(1)
Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

#
X=rnorm(15)
X1 =sample(c(-1,1), 15, replace=TRUE)
tstat <- sqrt(15)*mean(X1) / sd(X1)
tstat

set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,df=N -1), tstats, xlim=range(tstats))
abline(0,1)
#The population data is not normal thus the theory does not apply.
#We check with a Monte Carlo simulation. The qqplot shows a large tail. 
#Note that there is a small but positive chance that all the X are the same.
##In this case the denominator is 0 and the t-statistics is not defined
#
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B{
  X <- sample(c(-1,1),N,replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})

qqnorm(tstats)
abline(0,1)
#With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1
##Furthermore, t-distribution with df=999 and normal are practically the same.
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  return(X)
})
ps <- replicate(B, rnorm(N))
t.test(X, ps)
#data:  X and ps : t = -1.2556, df = 999.2, p-value = 0.2096
##
#
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  return(median(X))
})
qqnorm(tstats)
abline(0,1)

tstats <- replicate(B,{
  X <- rnorm(N)
  return(median(X))
})
qqnorm(tstats)
hist(tstats)
sd(tstats) > 1/sqrt(N)

#
set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}
##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)