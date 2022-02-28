library(shiny)
library(tidyverse)


ui<-fluidPage(
  fluidRow(
    column(3,numericInput("pop","N",value=50,min=1,max=1000)),
    column(6,sliderInput("per_capita","r",value=0,min=-1,max=1,step=0.1)),
    column(3,numericInput("time","t",value=10,min=2,max=500))),
  fluidRow(
    column(12,plotOutput("population_growth"))
  )
)

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

server<-function(input,output,session){
  pop_data<-reactive(exp_pop_growth(No=input$pop,r=input$per_capita,t=input$time))
  
  output$population_growth<-renderPlot({
    pop_data() %>%
      ggplot(aes(t,N)) +
      geom_line(color="blue") +
      theme_bw()
  },res=96)
}
shinyApp(ui,server)


