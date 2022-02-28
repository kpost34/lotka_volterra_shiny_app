library(tidyverse)

##Exponential population growth-----------------------------
#create the df
popDF<-matrix(NA,nrow=t+1,ncol=3)
colnames(popDF)<-c("t","N","dNdt")
popDF<-as_tibble(popDF)

#set t values, initial N values and r
#initial conditions/parameters
No<-50
r<-0.5
t<-100

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
