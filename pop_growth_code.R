library(tidyverse)

##Exponential Population Growth-----------------------------
#set initial conditions
No<-50
r<-0.5
t<-100

#create the df
popDF<-matrix(NA,nrow=t+1,ncol=3)
colnames(popDF)<-c("t","N","dNdt")
popDF<-as_tibble(popDF)

#populate first row and t values
popDF$t<-0:t
popDF$N[1]<-No
popDF$dNdt[1]<-No * r

#populate cells in systematic fashion
for(i in 2:t){
  popDF$N[i]<-popDF$N[i-1] + popDF$dNdt[i-1]
  popDF$dNdt[i]<-popDF$N[i] * r
}

#populate remaining cells--hard coding
popDF$dNdt[1]<-popDF$N[1] * r
popDF$N[2]<-popDF$N[1] + popDF$dNdt[1]

popDF$dNdt[2]<-popDF$N[2] * r
popDF$N[3]<-popDF$N[2] + popDF$dNdt[2]


#create function
exp_pop_growth<-function(No,r,t){
  #create empty tibble
  popDF<-matrix(NA,nrow=t+1,ncol=3)
  colnames(popDF)<-c("t","N","dNdt")
  popDF<-as_tibble(popDF)
  
  #populate first row and t values
  popDF$t<-0:t
  popDF$N[1]<-No
  popDF$dNdt[1]<-No * r
  
  #populate cells in systematic fashion
  for(i in 2:t){
    popDF$N[i]<-popDF$N[i-1] + popDF$dNdt[i-1]
    popDF$dNdt[i]<-popDF$N[i] * r
  }
  popDF
}

#graph N vs. t
exp_pop_growth(100,.3,50) %>% ggplot(aes(t,N)) + geom_line()



#Logistic Population Growth-------------------------------------
#set initial conditions
No<-50
r<-0.5
t<-100
K<-500

#create the df
logDF<-matrix(NA,nrow=t+1,ncol=3)
colnames(logDF)<-c("t","N","dNdt")
logDF<-as_tibble(logDF)

#populate first row and t values
logDF$t<-0:t
logDF$N[1]<-No
logDF$dNdt[1]<-r * ((K - No)/K) * No

#populate cells in systematic fashion
for(i in 2:t){
  logDF$N[i]<-logDF$N[i-1] + logDF$dNdt[i-1]
  logDF$dNdt[i]<-r * ((K - logDF$N[i])/K) * logDF$N[i]
}

#create function
log_pop_growth<-function(No,r,t,K){
  #create empty tibble
  logDF<-matrix(NA,nrow=t+1,ncol=3)
  colnames(logDF)<-c("t","N","dNdt")
  logDF<-as_tibble(logDF)
  
  #populate first row and t values
  logDF$t<-0:t
  logDF$N[1]<-No
  logDF$dNdt[1]<-r * ((K - No)/K) * No
  
  #populate cells in systematic fashion
  for(i in 2:t){
    logDF$N[i]<-logDF$N[i-1] + logDF$dNdt[i-1]
    logDF$dNdt[i]<-r * ((K - logDF$N[i])/K) * logDF$N[i]
  }
  logDF
}

#graph N vs. t
log_pop_growth(50,0.5,10,500) %>% ggplot(aes(t,N)) + geom_line()




