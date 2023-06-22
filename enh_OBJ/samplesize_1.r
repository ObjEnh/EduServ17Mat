##Program calculates sample size after formula of Congalton
#name of program: samplesize_1.r
#author: JH
#Solution after Congalton&Green
#class proportion is assumed with 50%
#last modified: 20.2.19

setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")
inits=readline("type the number of classes:  ") #manual input of number of classes required
#
k<-as.integer(inits)
k #number of classes
#at confidence level=95%: 
alph=1-0.95
alph
p=1-alph/k
p
B=qchisq(p,df=1)
B
acc=0.05 #desired accuracy
samplesize=B/(4*acc^2)
samplesize
samplesize_per_class<-samplesize/k
samplesize_per_class #result
#
##end of program


