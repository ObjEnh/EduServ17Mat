##Confidence interval for standard deviation
##name of program: ci_sigma.r
##reference article: "Automated mapping in urban areas" by J. Höhle, 2018, Equation #27
##reference presentation: Introduction at the pre-course seminar 2019

setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")

##parameters
p=0.975 #probability
df=72 #degree of freedom

## x-coordinate
s=1.0 #standard deviation

##calculation of CI
var_s=s^2
var_s
B=qchisq(p,df,lower.tail=TRUE) #Chi-Square function
B
CI_lower_limit_sigma<-sqrt(df*var_s/B)
CI_lower_limit_sigma<-round(CI_lower_limit_sigma,digits=2)
CI_lower_limit_sigma
#
p=0.025 
df=72
var_s=s^2
var_s
B=qchisq(p,df,lower.tail = TRUE)
B
CI_upper_limit_sigma<-sqrt(df*var_s/B)
CI_upper_limit_sigma<-round(CI_upper_limit_sigma,digits=2)
CI_upper_limit_sigma
##95% CI
cat("95% CI_x=[",CI_lower_limit_sigma,",",CI_upper_limit_sigma,"]",sep="")

##y-coordinate
s=0.7
#
p=0.975 
df=72
#
var_s=s^2
var_s
B=qchisq(p,df,lower.tail = TRUE)
B
CI_lower_limit_sigma<-sqrt(df*var_s/B)
CI_lower_limit_sigma
CI_lower_limit_sigma<-round(CI_lower_limit_sigma,digits=2)

##
p=0.025 
df=72
var_s=s^2
var_s
B=qchisq(p,df, lower.tail=TRUE)
B
CI_upper_limit_sigma<-sqrt(df*var_s/B)
CI_upper_limit_sigma<-round(CI_upper_limit_sigma,digits=2)
#
##95% CI
cat("95% CI_y=[",CI_lower_limit_sigma,",",CI_upper_limit_sigma,"]",sep="")
