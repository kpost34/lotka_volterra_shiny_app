library(shiny)
library(tidyverse)
library(bslib)

#Create user interface
ui<-fluidPage(
  theme=bslib::bs_theme(bootswatch="flatly"),
  titlePanel("Exponential and Logistic Growth App"),
  sidebarLayout(
    sidebarPanel(
          h5("Population 1"),
          numericInput("pop1","N",value=50,min=1,max=1000,width="50%"),
          numericInput("time1","t",value=20,min=2,max=500,width="50%"),
          sliderInput("per_cap_gr1","r",value=0,min=-1,max=1,step=0.05),
          numericInput("carry1","K",value=100,min=1,max=1000,width="50%"),
            br(),
            br(),
          h5("Population 2"),
          numericInput("pop2","N",value=50,min=1,max=1000,width="50%"),
          numericInput("time2","t",value=20,min=2,max=500,width="50%"),
          sliderInput("per_cap_gr2","r",value=0,min=-1,max=1,step=0.05),
          numericInput("carry2","K",value=100,min=1,max=1000,width="50%"),
            br(),
            br(),
          actionButton("exp_button","Show exponential growth"),
          actionButton("log_button","Show logistic growth"),
      width=4),
    mainPanel(
      tabsetPanel(
        id="tabset",
        tabPanel("Plots",
            br(),
          htmlOutput("exp_title"),
          plotOutput("exp_growth",click="plot_click_exp"),
          tableOutput("coord_exp"),
            br(),
            br(),
          htmlOutput("log_title"),
          plotOutput("log_growth",click="plot_click_log"),
          tableOutput("coord_log")),
        tabPanel("Information",
            br(),
          p("This app was developed to illustrate the differences between exponential and logistic population growth",
            "and the impact of parameter (i.e., N, r, t, K) changes on their respective growth curves. The equations", 
            "to calculate growth rates are as follows:"),
          p(strong("Exponential growth"),": dN/dt = rN"),
            br(),
          p(strong("Logistic growth"),": dN/dt = r((K - N)/K) * N"))),
    position="left")
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


#Create server function
server<-function(input,output,session){
  #generate exponential pop growth data
  exp_data1<-reactive(exp_pop_growth(No=input$pop1,r=input$per_cap_gr1,t=input$time1))
  exp_data2<-reactive(exp_pop_growth(No=input$pop2,r=input$per_cap_gr2,t=input$time2))
  
  #generate logistic pop growth data
  log_data1<-reactive(log_pop_growth(No=input$pop1,r=input$per_cap_gr1,t=input$time1,K=input$carry1))
  log_data2<-reactive(log_pop_growth(No=input$pop2,r=input$per_cap_gr2,t=input$time2,K=input$carry2))
  
  #print exponential growth title
  observeEvent(input$exp_button,{
    output$exp_title<-renderText(paste("<b>Exponential Growth</b>"))
  })

  #produce exponential growth plot
  observeEvent(input$exp_button,{
    output$exp_growth<-renderPlot({
    pop_c(exp_data1(),exp_data2()) %>%
      ggplot() +
        geom_line(aes(t,N,color=pop)) +
        scale_color_manual(values=c("darkred","darkblue")) +
        theme_bw() +
        theme(axis.text=element_text(size=11)) +
        labs(x="time (t)",y="population size (N)",color="Population")
  },res=96) 
  })
  
  #print exponential plot click output
  output$coord_exp<-renderTable({
    req(input$plot_click_exp)
    nearPoints(pop_c(exp_data1(),exp_data2()),input$plot_click_exp,xvar="t",yvar="N",threshold=10)
  })
  
  #print logistic growth title
  observeEvent(input$log_button,{
    output$log_title<-renderText(paste("<b>Logistic Growth</b>"))
  })

  #produce logistic growth plot
  observeEvent(input$log_button,{
  output$log_growth<-renderPlot({
    pop_c(log_data1(),log_data2()) %>%
      ggplot() +
        geom_line(aes(t,N,color=pop)) +
        scale_color_manual(values=c("darkred","darkblue")) +
        theme_bw() +
        theme(axis.text=element_text(size=11)) +
        labs(x="time (t)",y="population size (N)",color="Population")
  },res=96) 
  })
  
  #print logistic plot click output
  output$coord_log<-renderTable({
    req(input$plot_click_log)
    nearPoints(pop_c(log_data1(),log_data2()),input$plot_click_log,xvar="t",yvar="N",threshold=10)
  })
  
}
shinyApp(ui,server)

#future changes
#1) dynamic UI so that K boxes and perhaps pop2 inputs and graphs change on buttons
#2) double click to remove graph (or disappears on second button click--then remove the "Show" part of each button)

#3) option (toggle button) to have line/point graph--when point graph is select, make mouse events available
#4) return final population size?
#5) two sections (via headers) in Information tab--background and how to use app
