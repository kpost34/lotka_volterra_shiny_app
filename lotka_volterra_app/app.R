library(shiny)
library(tidyverse)


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
    column(12,plotOutput("population_growth"))
  ),
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

#concatenate population function
pop_c<-function(pop1,pop2){
  pop1 %>% 
    bind_rows(pop2) %>%
    add_column(pop=c(rep("pop1",nrow(pop1)),rep("pop2",nrow(pop2)))) -> pops
}

server<-function(input,output,session){
  pop_data1<-reactive(exp_pop_growth(No=input$pop1,r=input$per_cap_gr1,t=input$time1))
  pop_data2<-reactive(exp_pop_growth(No=input$pop2,r=input$per_cap_gr2,t=input$time2))

  output$population_growth<-renderPlot({
    pop_c(pop_data1(),pop_data2()) %>%
      ggplot() +
      geom_line(aes(t,N,color=pop)) +
      theme_bw() +
      labs(x="time (t)",y="population size (N)",color="Population")
      
  },res=96)
}
shinyApp(ui,server)


