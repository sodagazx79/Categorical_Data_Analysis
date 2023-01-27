

#last name: Sodagartojgi
#firt name: Zahra
#Homework 3, math 327

#QUESTION 2

#####WALD CONFIDENCE INTERVAL
#set parameters
y<- 180 # number of sucesses
n<-750  # number of trials (sample size)
alpha<-0.01  #Level of significance
pi<-.24   #Null hypothesis value

wald.test<-function(y,n,alpha=.01,pi=.24){
  pi.hat<-y/n
  se.wald<-sqrt(pi.hat*(1-pi.hat)/n)
  z<-(pi.hat-pi)/se.wald
  p.value<-2*pnorm(-abs(z))
  cat("Wald test statistics:",
      round(z,3),"\n","Two sided p-value: ", 
      round(p.value,4),"\n")
  
  ##Comfidence interval
  cat("\n",(1-alpha)*100, "% Confidence interval:",
      round(pi.hat + qnorm(p = c(alpha/2, 1-alpha/2)) * se.wald,4))
}

wald.test(y=180,n=750,alpha=.01)

##### score confidence interval

prop.test(x=180,n=750,0.24,conf.level =.99)

#####LIKELIHOOD CI

library(binom)
binom.confint(180,750, methods="lrt")


############QUESTION 4

y<- 0 # number of sucesses
n<-25  # number of trials (sample size)
alpha<-0.05  #Level of significance
pi<-0  

#compute statistics
pi.hat<-y/n       #Sample Proportion
se.wald<-sqrt(pi.hat*(1-pi.hat)/n)   #standard error

#Test of significance
z<-(pi.hat-pi)/se.wald      #Test statistic
#doesnt give us a number since prob is zero
p.value<-2*pnorm(-abs(z))   #p-value

wald.test1<-function(y,n,alpha=.05,pi=0){
  pi.hat<-y/n
  se.wald<-sqrt(pi.hat*(1-pi.hat)/n)
  z<-(pi.hat-pi)/se.wald
  p.value<-2*pnorm(-abs(z))
  cat("Wald test statistics:",
      round(z,3),"\n","Two sided p-value: ", 
      round(p.value,4),"\n")
  
  ##Comfidence interval
  cat("\n",(1-alpha)*100, "% Confidence interval:",
      round(pi.hat + qnorm(p = c(alpha/2, 1-alpha/2)) * se.wald,4))
}

wald.test1(y=0,n=25,alpha=.05) ###it gives us zero confidence interval 
##score test

prop.test(x=0,n=25,conf.level =.95)


##question 5


#  Binomial Test, 
# n=10, y=8, p=.5, onesided
#___________________________________________

##part a

#i
binom.test(8,10,.5, alternative = "greater")
#ii
binom.test(8,10,.5, alternative = "less")

##part b
library(exactci)
#i
binom.exact(8,10,.5,alternative = "greater",midp=TRUE)
#ii
binom.exact(8,10,.5,alternative = "less",midp=TRUE)



