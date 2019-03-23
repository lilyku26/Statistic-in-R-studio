#Associated test--------------------------------------------------
#Fisher test--------------------------------------------------------
#chi - test------------------------------------------------
library(downloader)
library(dplyr)
library(readr)
library(rafalib)
d = read.csv("assoctest.csv")
head(d)
x <- table(d)
chisq.test(x)
fisher.test(x)
#  allele   0     1   
#   0       17    17     34
#   1       10    28     38
#           27    45     72
#odd ratio : use fisher test
# or calculate 
x <- (17/27)/(10/27)
y <- (17/45)/(28/45)
x/y