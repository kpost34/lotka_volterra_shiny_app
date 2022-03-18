library(tidyverse)
library(gganimate)
library(gifski)

rm(list=ls())

### Competition Model----------------------------
## Hard code an example
#set initial conditions
No1<-150
r1<-0.9
alpha12<-0.5
K1<-130

No2<-160
r2<-0.9
alpha21<-0.5
K2<-150

t<-20

#create the df
compDF<-matrix(NA,nrow=t+1,ncol=5)
colnames(compDF)<-c("t","N1","dN1dt","N2","dN2dt")
compDF<-as_tibble(compDF)

#populate first row and t values
compDF$t<-0:t
compDF$N1[1]<-No1
compDF$dN1dt[1]<-(r1*No1)*(1-((No1+(alpha12*No2))/K1))
compDF$N2[1]<-No2
compDF$dN2dt[1]<-(r2*No2)*(1-((No2+(alpha21*No1))/K2))


#populate cells in systematic fashion
for(i in 2:(t+1)){
  compDF$N1[i]<-compDF$N1[i-1] + compDF$dN1dt[i-1]
  compDF$N2[i]<-compDF$N2[i-1] + compDF$dN2dt[i-1]
  compDF$dN1dt[i]<-(r1*compDF$N1[i])*(1-((compDF$N1[i]+(alpha12*compDF$N2[i]))/K1))
  compDF$dN2dt[i]<-(r2*compDF$N2[i])*(1-((compDF$N2[i]+(alpha21*compDF$N1[i]))/K2))
}

#determine zero-growth isoclines
iso1<-tibble(N1=c(0,K1),
             N2=c(K1/alpha12,0),
             text=c("K1/alpha12","K1"),
             isocline=rep("species1",2))
iso2<-tibble(N1=c(0,K2/alpha21),
             N2=c(K2,0),
             text=c("K2","K2/alpha21"),
             isocline=rep("species2",2))

#combine iso objects into DF
iso1 %>%
  bind_rows(iso2)-> isoDF


## Create functions
# Create competition dataframe
comp_mod<-function(No1,r1,alpha21,K1,No2,r2,alpha12,K2,t){
  #create empty tibble
  compDF<-matrix(NA,nrow=t+1,ncol=5)
  colnames(compDF)<-c("t","N1","dN1dt","N2","dN2dt")
  compDF<-as_tibble(compDF)
  
  #populate first row and t values
  compDF$t<-0:t
  compDF$N1[1]<-No1
  compDF$dN1dt[1]<-(r1*No1)*(1-((No1+(alpha12*No2))/K1))
  compDF$N2[1]<-No2
  compDF$dN2dt[1]<-(r2*No2)*(1-((No2+(alpha21*No1))/K2))
  
  #populate cells in systematic fashion
  for(i in 2:(t+1)){
    compDF$N1[i]<-compDF$N1[i-1] + compDF$dN1dt[i-1]
    compDF$N2[i]<-compDF$N2[i-1] + compDF$dN2dt[i-1]
    compDF$dN1dt[i]<-(r1*compDF$N1[i])*(1-((compDF$N1[i]+(alpha12*compDF$N2[i]))/K1))
    compDF$dN2dt[i]<-(r2*compDF$N2[i])*(1-((compDF$N2[i]+(alpha21*compDF$N1[i]))/K2))
  }
  compDF
}

# Create isocline dataframe
isocliner<-function(K1,alpha21,K2,alpha12){
  #determine zero-growth isoclines
  iso1<-tibble(N1=c(0,K1),
               N2=c(K1/alpha12,0),
               text=c("K1/alpha12","K1"),
               isocline=rep("species1",2))
  iso2<-tibble(N1=c(0,K2/alpha21),
             N2=c(K2,0),
             text=c("K2","K2/alpha21"),
             isocline=rep("species2",2))
  #combine iso objects into DF
  iso1 %>%
    bind_rows(iso2)-> isoDF
  isoDF
}



## Test functions
comp_mod(100,.8,.5,150, 120,.7,.9,120,200)
isocliner(200,.5,170,.3)



## Make plots
#1. N vs. t for both species
comp_mod(100,.8,.5,200, 120,.7,.3,170,25) %>%
  ggplot() +
    geom_line(aes(t,N1),color="red") +
    geom_line(aes(t,N2),color="blue") +
    scale_x_continuous(expand=c(0,0)) +
    labs(y="N") +
    theme_bw() +
    transition_reveal(t) -> comp_time_plot

animate(comp_time_plot,renderer = gifski_renderer(loop=FALSE))


#2. N1 vs. N2
isocliner(200,.5,170,.3) %>%
  ggplot(aes(N1,N2,color=isocline)) +
    geom_line() +
    geom_point() +
    geom_text(aes(label=text),color="black",hjust="inward",vjust="inward") +
    annotate(geom="text",x=Inf,y=Inf,hjust="right",vjust="top",fontface=2,
             label=case_when(
               (K1/alpha12 > K2) & (K1 > K2/alpha21) ~ "Species 1 wins",
               (K2 > K1/alpha12) & (K2/alpha21 > K1) ~ "Species 2 wins",
               (K2 > K1/alpha12) & (K1 > K2/alpha21) ~ "Unstable equilibrium",
               (K1/alpha12 > K2) & (K2/alpha21 > K1) ~ "Stable equilibrium: coexistence"),
    ) +
    geom_point(data=compDF,aes(N1,N2),color="darkgreen") +
    transition_time(compDF$t) +
    shadow_mark(past=TRUE,future=FALSE,alpha=0.3) +
    theme(legend.position="bottom") +
    theme_bw() -> comp_iso_plot

animate(comp_iso_plot,renderer = gifski_renderer(loop=FALSE))

