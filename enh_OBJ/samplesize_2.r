##program for calculation of sample size
#program name: samplesize_2.r
#author: MH
#last modified: 10.3.19

setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")
install.packages("binomSamSize")
install.packages("binom")
#help(package="binomSamSize")
#help(package="binom")
library("binom")
library("binomSamSize")

##parametre
#d=half-width of confidence interval
#alpha=0.05 when 95% CI is calculated
#p0=desired thematic accuracy (p0/100)
#method=method to use to construct the interval, e.g. maximum likelihood ratio test(lrt), wilson, vald,...
sa_size<-binomSamSize::ciss.binom(p0=0.7, d=0.05, alpha=0.05, method="lrt")
#sa_size<-binomSamSize::ciss.binom(p0=0.6, d=0.1, alpha=0.05, method="asymptotic",nStart=2)
#sa_size<-binomSamSize::ciss.wald(p0=0.7, d=0.05, alpha=0.05)
#sa_size<-binomSamSize::ciss.binom(p0=0.95, d=0.05, alpha=0.05,method="lrt")
cat("The samplesize is: ", sa_size, "\n")
#end program
#