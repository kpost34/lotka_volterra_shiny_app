library(tidyverse)
library(deSolve)
library(ggrepel)

#### Predator-prey Model-------------------
### Hard coding approach
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
  

#------------------------------------------------------
### Flexible-coding approach
## Initial values 
#store parameters as a vector
params<-c(alpha<-0.8,
          beta<-0.1,
          delta<-0.02,
          gamma<-0.5)

#store initial population sizes as a vector
xo<-22
yo<-7

inits<-c(x=xo,y=yo)

#turn time into a vector
times<-seq(0,100,by=.1) #here t=100


## Build DF of 'graduated' initial population sizes
pop_mods<-c(0.5,0.75,1,1.25,1.5)
stateDF<-tibble(x=pop_mods*(gamma/delta),
                y=pop_mods*(alpha/beta),
                text=c("0.5x","0.75x","1x","1.25x","1.5x"))


## Model equations for ode()
LVpred<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    #rate of change equations
    dxdt<-(alpha*x)-(beta*x*y)
    dydt<-(delta*x*y)-(gamma*y)
    
    #returns rate of change
    list(c(dxdt,dydt))
  })
}


## Iteratively run ode() on stateDF
apply(stateDF[,1:2],1,
      function(x) as_tibble(ode(x %>% as.numeric() %>% setNames(c("x","y")),
                                times,LVpred,params))) -> predList 

Map(cbind,predList,text=stateDF[[3]]) %>%
  do.call("rbind",.) -> predDF


## Run function on initial population sizes
ode(inits,times,LVpred,params) %>%
  as_tibble() -> user_predDF

#NOTE: ends with two dfs: predDF (DFs of various proportions/multipliers of equilibrium pop sizes) and user_predDF 
#(the DF generated from the initial pop sizes selected by user)

#---------------------------------------------------------------------------
### Function-oriented approach
## Initial values
a<-0.8
b<-0.1
d<-0.02
g<-0.5

xo<-22
yo<-7

t<-100

## Create functions
# Model equations for ode()
LVpred<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    #rate of change equations
    dxdt<-(alpha*x)-(beta*x*y)
    dydt<-(delta*x*y)-(gamma*y)
    
    #returns rate of change
    list(c(dxdt,dydt))
  })
}

# Build stateDF & iterate ode() over varied initial population sizes
predDF_builder<-function(alpha,beta,delta,gamma,t,func){
  #deSolve needed for ode()
  require(deSolve)
  #start with a pop_modifier vector
  mods<-c(0.5,0.75,1,1.25,1.5)
  #create stateDF using parameters
  tibble(prey=mods*(gamma/delta),
         pred=mods*(alpha/beta),
         text=paste0(mods,"x"))->stateDF
  #created times vector from t
  seq(0,t,by=.1)->times
  #turn parameters into vector
  pars<-c(alpha=alpha,beta=beta,delta=delta,gamma=gamma)
  #iterate ode() over each combination of pop sizes and append multiplier as text in third col
  apply(stateDF[,1:2],1,
        function(x) as_tibble(ode(x %>% as.numeric() %>% setNames(c("x","y")),
                                  times,func,pars))) %>%
    Map(cbind,.,text=stateDF[[3]]) %>%
    do.call("rbind",.) 
}
#Additional notes about function: 
#1) stateDF contains x & y that have been modified based on parameter ratios and a third column, text, that contains
#the modifier as a character string (e.g., "1.5x")
#2) t contains time as a sequence vector
#3) func contains the function that will be fed into ode()
#4) pars contains a vector of the parameters (i.e., alpha, beta, delta, gamma)

# Output x & y from user-defined initial pop values
user_predDF_builder<-function(xo,yo,alpha,beta,delta,gamma,t,func){
  require(deSolve)
  inits<-c(x=xo,y=yo)
  pars<-c(alpha=alpha,beta=beta,delta=delta,gamma=gamma)
  times<-seq(0,t,by=0.1)
  ode(inits,times,LVpred,pars) %>%
    as_tibble() 
}


## Apply functions
predDF<-predDF_builder(a,b,d,g,t,LVpred)
user_predDF<-user_predDF_builder(xo,yo,a,b,d,g,t,LVpred)


#----------------------------------------------------------------------------
## Build plot phases and add plot of initial population sizes
#create label dataframe
predDF %>%
  group_by(text) %>%
  summarize(x_eq=gamma/delta,
            y_max=max(y))-> pop_mods_labels

#develop phase-space plot
ggplot(data=predDF) +
  theme_bw() +
  geom_hline(yintercept=alpha/beta,linetype=2,color="black") +
  geom_vline(xintercept=gamma/delta,linetype=2,color="black") +
  geom_path(data=. %>% filter(text!="1x"),aes(x,y,group=text),color="gray50") +
  geom_point(data=. %>% filter(text=="1x"),aes(x,y),size=3) +
  geom_label_repel(data=pop_mods_labels,aes(x_eq,y_max,label=text)) +
  geom_path(data=user_predDF,aes(x,y),color="steelblue") 
  
  
## Plot x and y vs time
ggplot(data=user_predDF) +
  geom_line(aes(time,x,color="Prey")) +
  geom_line(aes(time,y,color="Predator")) +
  scale_color_manual(name="",values=c("Prey"="blue","Predator"="red")) +
  theme_bw() +
  labs(y="Population size")
  
  
