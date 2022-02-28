library(shiny)
library(tidyverse)

#Create user interface
ui<-fluidPage(
  fluidRow(
    column(6,
      "Population 1",
      numericInput("pop1","N",value=50,min=1,max=1000,width="50%"),
      numericInput("time1","t",value=10,min=2,max=500,width="50%"),
      sliderInput("per_cap_gr1","r",value=0,min=-1,max=1,step=0.1)
  ),
    column(6,
      "Population 2",
      numericInput("pop2","N",value=50,min=1,max=1000,width="50%"),
      numericInput("time2","t",value=10,min=2,max=500,width="50%"),
      sliderInput("per_cap_gr2","r",value=0,min=-1,max=1,step=0.1)
  )),
  fluidRow(
    column(6,
           numericInput("carry1","K",value=100,min=1,max=1000,width="50%")
    ),
    column(6,
           numericInput("carry2","K",value=100,min=1,max=1000,width="50%"))
  ),
  fluidRow(
    column(12,
      "Exponential Growth",
      plotOutput("exp_growth"))
  ),
  fluidRow(
    column(12,
      "Logistic Growth",
      plotOutput("log_growth"))
  )
)

#Create functions
#exponential population growth function
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


#logistic population growth function
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


#concatenate population function
pop_c<-function(pop1,pop2){
  pop1 %>% 
    bind_rows(pop2) %>%
    add_column(pop=c(rep("pop1",nrow(pop1)),rep("pop2",nrow(pop2)))) -> pops
}


#Create server
server<-function(input,output,session){
  #generate exponential pop growth data
  exp_data1<-reactive(exp_pop_growth(No=input$pop1,r=input$per_cap_gr1,t=input$time1))
  exp_data2<-reactive(exp_pop_growth(No=input$pop2,r=input$per_cap_gr2,t=input$time2))
  
  #generate logistic pop growth data
  log_data1<-reactive(log_pop_growth(No=input$pop1,r=input$per_cap_gr1,t=input$time1,K=input$carry1))
  log_data2<-reactive(log_pop_growth(No=input$pop2,r=input$per_cap_gr2,t=input$time2,K=input$carry2))

  #produce exponential growth plot
  output$exp_growth<-renderPlot({
    pop_c(exp_data1(),exp_data2()) %>%
      ggplot() +
      geom_line(aes(t,N,color=pop)) +
      theme_bw() +
      labs(x="time (t)",y="population size (N)",color="Population")
  },res=96)

  #produce logistic growth plot
  output$log_growth<-renderPlot({
    pop_c(log_data1(),log_data2()) %>%
      ggplot() +
      geom_line(aes(t,N,color=pop)) +
      theme_bw() +
      labs(x="time (t)",y="population size (N)",color="Population")
  })
  
}
shinyApp(ui,server)


