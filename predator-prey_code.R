library(tidyverse)
library(deSolve)

### Predator-prey Model-------------------
## Create a vector of parameters
param_eq<-c(alpha<-0.8,
            beta<-0.1,
            delta<-0.02,
            gamma<-0.5)
#therefore, equilibrium occurs at (25, 8)
xo_eq<-gamma/delta
yo_eq<-alpha/beta

#include additional starting population sizes
#0.5x
xo1<-.5*xo_eq
yo1<-.5*yo_eq

#0.75x
xo2<-.75*xo_eq
yo2<-.75*yo_eq

#1.25x
xo3<-1.25*xo_eq
yo3<-1.25*yo_eq

#1.5x
xo4<-1.5*xo_eq
yo4<-1.5*yo_eq


## Create state variables (prey and predator population sizes) as a vector with their initial values
state_eq<-c(x=xo_eq,
            y=yo_eq)

state1<-c(x=xo1,
          y=yo1)

state2<-c(x=xo2,y=yo2)

state3<-c(x=xo3,y=yo3)

state4<-c(x=xo4,y=yo4)


## Model equations
LVpred<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    #rate of change equations
    dxdt<-(alpha*x)-(beta*x*y)
    dydt<-(delta*x*y)-(gamma*y)
    
    #returns rate of change
    list(c(dxdt,dydt))
  })
}


## Model application
#time specification
times<-seq(0,100,by=.1)

#model integration
ode(y=state_eq,times=times,func=LVpred,parms=param_eq) %>%
  as_tibble() -> outDF_eq

ode(y=state1,times=times,func=LVpred,parms=param_eq) %>%
  as_tibble() -> outDF1

ode(y=state2,times=times,func=LVpred,parms=param_eq) %>%
  as_tibble() -> outDF2

ode(y=state3,times=times,func=LVpred,parms=param_eq) %>%
  as_tibble() -> outDF3

ode(y=state4,times=times,func=LVpred,parms=param_eq) %>%
  as_tibble() -> outDF4


## Plot data
# Abundance vs. time
ggplot(outDF_eq) +
  geom_line(aes(time,x),color="red") +
  geom_line(aes(time,y),color="blue") 

ggplot(outDF1) +
  geom_line(aes(time,x),color="red") +
  geom_line(aes(time,y),color="blue") 

ggplot(outDF2) +
  geom_line(aes(time,x),color="red") +
  geom_line(aes(time,y),color="blue") 

ggplot(outDF3) +
  geom_line(aes(time,x),color="red") +
  geom_line(aes(time,y),color="blue") 

ggplot(outDF4) +
  geom_line(aes(time,x),color="red") +
  geom_line(aes(time,y),color="blue") 


# Predator vs. prey
ggplot(outDF_eq) +
  theme_bw() +
  geom_point(aes(x,y),color="black") +
  geom_hline(yintercept=yo_eq,linetype=2,color="black") +
  geom_vline(xintercept=xo_eq,linetype=2,color="black") +
  geom_path(data=outDF1,aes(x,y),color="gray50") +
  geom_path(data=outDF2,aes(x,y),color="gray50") +
  geom_path(data=outDF3,aes(x,y),color="gray50") +
  geom_path(data=outDF4,aes(x,y),color="gray50") +
  xlim(0,(2.5*xo_eq)) +
  ylim(0,(2.5*yo_eq))
  


### More flexible, concise coding
## Build DF of states
#set parameters
params<-c(alpha<-0.8,
          beta<-0.1,
          delta<-0.02,
          gamma<-0.5)

#select initial population sizes
xo<-22
yo<-7

inits<-c(x=xo,y=yo)

#build DF of 'graduated' initial population sizes
stateDF<-tibble(x=seq(0.5,1.5,.25)*(gamma/delta),
                y=seq(0.5,1.5,.25)*(alpha/beta),
                text=c("0.5x","0.75x","1x","1.25x","1.25x"))


## Develop population DFs
#create function for ode()
LVpred<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    #rate of change equations
    dxdt<-(alpha*x)-(beta*x*y)
    dydt<-(delta*x*y)-(gamma*y)
    
    #returns rate of change
    list(c(dxdt,dydt))
  })
}

#time specification
times<-seq(0,100,by=.1)

#iteratively run function on stateDF
apply(stateDF[,1:2],1,
      function(x) as_tibble(ode(x %>% as.numeric() %>% setNames(c("x","y")),
                                times,LVpred,params))) -> predList 

Map(cbind,predDF1,text=stateDF[[3]]) %>%
  do.call("rbind",.) -> predDF
                                            

#run function on initial population sizes
ode(inits,times,LVpred,params) %>%
  as_tibble() -> user_predDF


## Build plot phases and add plot of initial population sizes
ggplot(data=predDF) +
  theme_bw() +
  geom_path(aes(x,y,group=text),color="gray80") +
  geom_text(aes(max(x),max(y),group=text,label=text)) +
  geom_hline(yintercept=alpha/beta,linetype=2,color="black") +
  geom_vline(xintercept=gamma/delta,linetype=2,color="black") 


geom_path(data=predDFl[[1]],aes(x,y),color="gray50") +
  geom_text(data=predDFl[[1]],aes(x,y),label="test") +
  geom_path(data=predDFl[[2]],aes(x,y),color="gray50") +
  geom_path(data=predDFl[[4]],aes(x,y),color="gray50") +
  geom_path(data=predDFl[[5]],aes(x,y),color="gray50") +
  geom_path(data=user_predDF,aes(x,y),color="red") +
  xlim(0,pmax(2.5*(gamma/delta),2.5*xo)) +
  ylim(0,pmax(2.5*(alpha/beta),2.5*yo))


## Plot x and y vs time
ggplot(data=user_predDF) +
  geom_line(aes(time,x),color="red") +
  geom_line(aes(time,y),color="blue") +
  theme_bw()
  
  
