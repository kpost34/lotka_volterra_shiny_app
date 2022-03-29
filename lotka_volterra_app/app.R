library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)
library(gganimate)
library(gifski)
library(deSolve)
library(ggrepel)


### Create user interface
ui<-navbarPage(position="fixed-bottom",
  useShinyjs(),
  theme=bslib::bs_theme(bootswatch="flatly"),
  tabPanel("Population Growth",
    titlePanel("Exponential and Logistic Population Growth Models"),
    fluidRow(
      column(3,
        wellPanel(
          h5("Population 1"),
          numericInput("pop1","N",value=50,min=1,max=1000,width="75%"),
          numericInput("time1","t",value=20,min=2,max=500,width="75%"),
          sliderInput("per_cap_gr1","r",value=0,min=-1,max=1,step=0.05,width="75%"),
          numericInput("carry1","K",value=100,min=1,max=1000,width="75%"),
            br(),
          h5("Population 2"),
          numericInput("pop2","N",value=50,min=1,max=1000,width="75%"),
          numericInput("time2","t",value=20,min=2,max=500,width="75%"),
          sliderInput("per_cap_gr2","r",value=0,min=-1,max=1,step=0.05,width="75%"),
          numericInput("carry2","K",value=100,min=1,max=1000,width="75%"),
            br(),
            br(),
          actionButton("exp_button","Exponential growth"),
          actionButton("log_button","Logistic growth")
        )
      ),
      column(width=6,
        tabsetPanel(
          id="growth_tabs",
          tabPanel("Plots",
                br(),
              fluidRow(style="height:400px",
                htmlOutput("exp_title"),
                plotOutput("exp_growth",click="plot_click_exp")
              ),
              fluidRow(style="height:400px",
                htmlOutput("log_title"),
                plotOutput("log_growth",click="plot_click_log")
              ),
          ), 
          tabPanel("More Information",
              br(),
            h4("Background"),
            p("This app was developed to illustrate the differences between exponential and logistic population growth",
              "and the impact of parameter (i.e., N, r, t, K) changes on their respective growth curves. The equations", 
              "to calculate growth rates are as follows:"),
            p(strong("Exponential growth"),": dN/dt = rN"),
            p(strong("Logistic growth"),": dN/dt = r((K - N)/K) * N"),
              br(),
            h4("Instructions"),
            p("Click the \"Plots\" tab. Input your N, t, and r values for each population, and select the type(s) of growth", 
              "curve(s) to display. Set K for logistic growth. Graphs will be re-drawn as you adjust the parameters. Click",
              "the curves to display N and dN/dt values at each t."),
              br(),
            h4("Competition and Predation Apps"),
            p("Click the \"Competition\" or \"Predation\" tabs at the bottom of the page to access interactive apps for",
              "Lotka-Volterra competition and predation models, respectively. Each page has a set of input boxes and sliders",
              "and accompanying instructions.")
          ),
          tabPanel("Developer",
            p(h4(strong("Keith Post"))),
            p("If you would like to see the code for this Shiny app, please visit my",
              tags$a(href="https://github.com/kpost34/lotka_volterra_shiny_app",
                     "Github repo.")
            ),
            p(tags$a(href="https://github.com/kpost34","GitHub Profile")),
            p(tags$a(href="https://www.linkedin.com/in/keith-post","LinkedIn")), 
          )
        )
      ),
      column(3, align="right",
        fluidRow(style="height:95px"),
        fluidRow(style="height:400px",
          tableOutput("coord_exp")
        ),
        fluidRow(style="height:400px",
          tableOutput("coord_log")
        )
      )
    )
  ),
  tabPanel("Competition",
    titlePanel("Lotka-Volterra Competition Model"),
    tabsetPanel(id="comp_tabs",
      tabPanel("Simulation",         
        fluidRow(style="height:600px",
          column(6,
            plotOutput("comp_time_plot")
            ),
          column(6,
            plotOutput("comp_iso_plot")
            ),
        ),
        wellPanel(
          fluidRow(
            column(4,
              h5(strong("Population 1"))
            ),
            column(4,
              actionButton("comp_button","Run model"),
            ),
            column(4,
              h5(strong("Population 2"))
            )
          ),
          fluidRow(
            column(4,
              numericInput("N1_comp","N",value=50,min=1,max=1000,width="50%"),
              sliderInput("r1_comp","r",value=0,min=0,max=1,step=0.05,width="50%"),
              numericInput("K1_comp","K",value=100,min=1,max=1000,width="50%")
            ),
            column(4,
              numericInput("t_comp","t",value=20,min=2,max=200,width="50%"),
              sliderInput("alpha21_comp","alpha21",value=0,min=0,max=1,step=0.1,width="50%"),
              sliderInput("alpha12_comp","alpha12",value=0,min=0,max=1,step=0.1,width="50%")
            ),
            column(4,
              numericInput("N2_comp","N",value=50,min=1,max=1000,width="50%"),
              sliderInput("r2_comp","r",value=0,min=0,max=1,step=0.05,width="50%"),
              numericInput("K2_comp","K",value=100,min=1,max=1000,width="50%")
            )
          )
        )
      ),
      tabPanel("Background")
    )
  ),
  tabPanel("Predation",
    titlePanel("Lotka-Volterra Predator-Prey Model"),
    sidebarLayout(
      mainPanel(
        tabsetPanel(
          id="pred_tabs",
          tabPanel("Plots",
              br(),
            htmlOutput("pred_time_title"),
            plotOutput("pred_time_plot"),
            htmlOutput("pred_phase_title"),
            plotOutput("pred_phase_plot")
          ),
          tabPanel("Theory and math")
        )
      ),
      sidebarPanel(
        numericInput("pred_x","x",value=50,min=0,max=1000),
        sliderInput("pred_alpha","alpha",value=0,min=0,max=1,step=0.05),
        sliderInput("pred_beta","beta",value=0,min=0,max=1,step=0.05),
        numericInput("pred_y","y",value=10,min=0,max=1000),
        sliderInput("pred_delta","delta",value=0,min=0,max=1,step=0.05),
        sliderInput("pred_gamma","gamma",value=0,min=0,max=1,step=0.05),
        numericInput("pred_t","t",value=100,min=2,max=200),
        actionButton("pred_button","Show/hide plots")
      )
    )
  )
)

  

### Create functions
## Population growth functions
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
  popDF[,c("N","dNdt")]<-signif(popDF[,c("N","dNdt")],3)
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
  logDF[,c("N","dNdt")]<-signif(logDF[,c("N","dNdt")],3)
  logDF
}


#concatenate population function
pop_c<-function(pop1,pop2){
  pop1 %>% 
    bind_rows(pop2) %>%
    add_column(pop=c(rep("pop1",nrow(pop1)),rep("pop2",nrow(pop2)))) -> pops
}



## Competition model functions
#competition model function
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

# isocline dataframe function
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


## Predator-prey Model Functions
# model equations for ode()
LVpred<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    #rate of change equations
    dxdt<-(alpha*x)-(beta*x*y)
    dydt<-(delta*x*y)-(gamma*y)
    
    #returns rate of change
    list(c(dxdt,dydt))
  })
}


#build stateDF & iterate ode() over varied initial population sizes
predDF_builder<-function(alpha,beta,delta,gamma,t,func){
  #deSolve needed for ode()
  require(deSolve)
  #start with a pop_modifier vector
  mods<-c(0.5,1,1.5,2,3,4)
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


#output x & y from user-defined initial pop values
user_predDF_builder<-function(xo,yo,alpha,beta,delta,gamma,t,func){
  require(deSolve)
  inits<-c(x=xo,y=yo)
  pars<-c(alpha=alpha,beta=beta,delta=delta,gamma=gamma)
  times<-seq(0,t,by=0.1)
  ode(inits,times,LVpred,pars) %>%
    as_tibble() 
}

### Create server function
server<-function(input,output,session){

  ## PAGE 1: Population Growth Models
  #Produce reactive functions of data
  #generate exponential pop growth data
  exp_data1<-reactive(exp_pop_growth(No=input$pop1,r=input$per_cap_gr1,t=input$time1))
  exp_data2<-reactive(exp_pop_growth(No=input$pop2,r=input$per_cap_gr2,t=input$time2))
  
  #generate logistic pop growth data
  log_data1<-reactive(log_pop_growth(No=input$pop1,r=input$per_cap_gr1,t=input$time1,K=input$carry1))
  log_data2<-reactive(log_pop_growth(No=input$pop2,r=input$per_cap_gr2,t=input$time2,K=input$carry2))
  
  #Exponential growth plot
  #start with exp plot and title hidden
  hide("exp_title")
  hide("exp_growth")
  
  #toggle print exponential growth title & plot
  observeEvent(input$exp_button,{
    toggle("exp_title")
    toggle("exp_growth")
  })
  
  #print exponential plot title
  output$exp_title<-renderText(paste("<b>Exponential Growth</b>"))

  #produce exponential growth plot
  output$exp_growth<-renderPlot({
    pop_c(exp_data1(),exp_data2()) %>%
      ggplot() +
        geom_line(aes(t,N,color=pop)) +
        scale_color_manual(values=c("darkred","darkblue")) +
        theme_bw() +
        theme(axis.text=element_text(size=11),
              legend.position="bottom",
              legend.title=element_blank(),
              legend.text=element_text(size=10)) +
        labs(x="time (t)",y="population size (N)",color="Population")
  },res=96)
  
  #print exponential plot click output on hits only
  output$coord_exp<-renderTable({
    req(input$plot_click_exp)
    coord_exp_dat<-nearPoints(pop_c(exp_data1(),exp_data2()),input$plot_click_exp,threshold=10)
    if(nrow(coord_exp_dat)==0) 
      return()
    coord_exp_dat
  })
  
  
  #Logistic growth plot
  #start with logistic plot and title hidden
  hide("log_title")
  hide("log_growth")
  
  #toggle print logistic growth title & plot
  observeEvent(input$log_button,{
    toggle("log_title")
    toggle("log_growth")
  })
  
  #print logistic plot title
  output$log_title<-renderText(paste("<b>Logistic Growth</b>"))

  #produce logistic growth plot
  output$log_growth<-renderPlot({
    pop_c(log_data1(),log_data2()) %>%
      ggplot() +
        geom_line(aes(t,N,color=pop)) +
        scale_color_manual(values=c("darkred","darkblue")) +
        geom_hline(yintercept=input$carry1,linetype=2,color="darkred") +
        geom_hline(yintercept=input$carry2,linetype=2,color="darkblue") +
        theme_bw() +
        theme(axis.text=element_text(size=11),
              legend.position="bottom",
              legend.title=element_blank(),
              legend.text=element_text(size=10)) +
        labs(x="time (t)",y="population size (N)",color="Population")
  },res=96) 
  
  #print logistic plot click output on hits only
  output$coord_log<-renderTable({
    req(input$plot_click_log)
    coord_log_dat<-nearPoints(pop_c(log_data1(),log_data2()),input$plot_click_log,threshold=10)
    if(nrow(coord_log_dat)==0) 
      return()
    coord_log_dat
  })
  
  #hide click output when in information tab
  observe(if(input$growth_tabs=="Information"){
    hide("coord_exp")
    hide("coord_log")}
    else{
      show("coord_exp")
      show("coord_log")
    }
  )
  
  
  ## PAGE 2: Competition Models
  # Produce reactive functions of data
  #generate competition model and isocline dataframes
  comp_data<-reactive({
    comp_mod(No1=input$N1_comp,r1=input$r1_comp,alpha21=input$alpha21_comp,K1=input$K1_comp,
            No2=input$N2_comp,r2=input$r2_comp,alpha12=input$alpha12_comp,K2=input$K2_comp,
            t=input$t_comp)
    })
  comp_iso_data<-reactive({
    isocliner(K1=input$K1_comp,alpha21=input$alpha21_comp,K2=input$K2_comp,alpha12=input$alpha12_comp)
    })

  # Start with plots hidden
  hide("comp_time_plot")
  hide("comp_iso_plot")

  # Toggle print competition model plots
  observeEvent(input$comp_button,{
    toggle("comp_time_plot")
    toggle("comp_iso_plot")
  })
  
  # Produce competition model plots
  #N vs t for both species
  output$comp_time_plot<-renderImage({
    outfile1<-tempfile(fileext=".gif")
    
    comp_data() %>%
      ggplot() +
        geom_line(aes(t,N1,color="species1")) +
        geom_line(aes(t,N2,color="species2")) +
        scale_x_continuous(expand=c(0,0)) +
        scale_color_manual(values=c("species1"="darkred","species2"="darkblue")) +
        labs(x="time",
             y="population size (N)") +
        theme_bw() +
        theme(axis.title=element_text(size=14),
              axis.text=element_text(size=12),
              legend.position="bottom",
              legend.title=element_blank(),
              legend.text=element_text(size=12)) +
        transition_reveal(t) -> c_plot1
    
      anim_save("outfile1.gif",animate(c_plot1,renderer = gifski_renderer(loop=FALSE)))
      
      list(src = "outfile1.gif",
           contentType = 'image/gif' 
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )}, deleteFile = TRUE)

  
  #N1 vs N2 
  output$comp_iso_plot<-renderImage({
    outfile2<-tempfile(fileext=".gif")
    
    comp_iso_data() %>%
      ggplot(aes(N1,N2,color=isocline)) +
        geom_line() +
        geom_point() +
        scale_color_manual(values=c("darkred","darkblue")) +
        geom_text(aes(label=text),color="black",size=4,fontface="bold",hjust="inward",vjust="inward") +
        annotate(geom="text",x=Inf,y=Inf,hjust="right",vjust="top",fontface="bold",size=6,
                 label=case_when(
                   (input$K1_comp/input$alpha12_comp > input$K2_comp) & (input$K1_comp > input$K2_comp/input$alpha21_comp) ~ "Species 1 wins",
                   (input$K2_comp > input$K1_comp/input$alpha12_comp) & (input$K2_comp/input$alpha21_comp > input$K1_comp) ~ "Species 2 wins",
                   (input$K2_comp > input$K1_comp/input$alpha12_comp) & (input$K1_comp > input$K2_comp/input$alpha21_comp) ~ "Unstable equilibrium",
                   (input$K1_comp/input$alpha12_comp > input$K2_comp) & (input$K2_comp/input$alpha21_comp > input$K1_comp) ~ "Stable equilibrium: coexistence"),
        ) +
        geom_point(data=comp_data(),aes(N1,N2),color="darkgreen") +
        transition_time(comp_data()$t) +
        shadow_mark(past=TRUE,future=FALSE,alpha=0.3) +
        labs(x="N (species1)",
             y="N (species2)") +
        theme_bw() +
        theme(axis.title=element_text(size=14),
              axis.text=element_text(size=12),
              legend.position="bottom",
              legend.title=element_text(size=14),
              legend.text=element_text(size=12)) -> c_plot2
    
    anim_save("outfile2.gif",animate(c_plot2,renderer = gifski_renderer(loop=FALSE)))
    
    list(src = "outfile2.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)


  ### PAGE 3: Predator-prey Model
  ## Produce reactive functions of data
  user_predDF<-reactive({
    user_predDF_builder(xo=input$pred_x,yo=input$pred_y,alpha=input$pred_alpha,beta=input$pred_beta,delta=input$pred_delta,
                        gamma=input$pred_gamma,t=input$pred_t,func=LVpred)
  })
  predDF<-reactive({
    predDF_builder(alpha=input$pred_alpha,beta=input$pred_beta,delta=input$pred_delta,gamma=input$pred_gamma,t=input$pred_t,func=LVpred)
  })
  
  ## Start with plots hidden
  hide("pred_time_title")
  hide("pred_time_plot")
  hide("pred_phase_title")
  hide("pred_phase_plot")

  ## Toggle print predator-prey plots
  observeEvent(input$pred_button,{
    toggle("pred_time_title")
    toggle("pred_time_plot")
    toggle("pred_phase_title")
    toggle("pred_phase_plot")
  })
  
  #print plot titles
  output$pred_time_title<-renderText(paste("<b>Population Size Over Time</b>"))
  output$pred_phase_title<-renderText(paste("<b>Phase-Space Plot</b>"))
  
  ## Develop two plots
  # Build pops vs. time plot
  output$pred_time_plot <- renderPlot({
    ggplot(data=user_predDF()) +
      geom_line(aes(x=time,y=x,color="Prey",linetype="Prey")) +
      geom_line(aes(x=time,y=y,color="Predator",linetype="Predator")) +
      scale_color_manual(name="",values=c("Prey"="blue","Predator"="red")) +
      scale_linetype_manual(name="",values=c("Prey"=1,"Predator"=2)) +
      theme_bw() +
      theme(axis.title=element_text(size=13),
            axis.text=element_text(size=11),
            legend.position="bottom",
            legend.title=element_text(size=13),
            legend.text=element_text(size=11)) +
      labs(y="population size")
    },res=96) 
  
  
  # Build phase plot
  #create label dataframe
  pop_mods_labs<-reactive({
    predDF() %>%
      group_by(text) %>%
      summarize(x_eq=input$pred_gamma/input$pred_delta,y_max=max(y))
  })
  
  #develop phase-space plot
  output$pred_phase_plot <- renderPlot({
    ggplot(data=predDF()) +
      theme_bw() +
      theme(axis.title=element_text(size=13),
            axis.text=element_text(size=11),
            legend.position="right",
            legend.title=element_text(size=13),
            legend.text=element_text(size=11)) +
      geom_hline(yintercept=input$pred_alpha/input$pred_beta,linetype=2,color="black") +
      geom_vline(xintercept=input$pred_gamma/input$pred_delta,linetype=2,color="black") +
      geom_path(data=predDF() %>% filter(text!="1x"),aes(x,y,group=text,color="orbits")) +
      geom_point(data=predDF() %>% filter(text=="1x"),aes(x,y,size="equilibrium")) +
      geom_label_repel(data=pop_mods_labs(),aes(x_eq,y_max,label=text)) +
      geom_path(data=user_predDF(),aes(x,y,color="user")) +
      scale_size_manual(name=NULL,values=c("equilibrium"=3)) +
      scale_color_manual(name=NULL,values=c("orbits"="gray60","user"="darkgreen")) +
      labs(x="prey population size (x)",
           y="predator population size (y)")
    },res=96) 
} 
shinyApp(ui,server)

#DONE
## Pop Growth
#add another tab that briefly explains rest of app
#add another tab that has my info
#signifing() the N and dN/dt values
#put K lines on the graph

## Competition
#fix layout of plots
#increase text sizes on plots

## Predation
#legends on bottom
#move top plot title down a line




#FUTURE CHANGES
#Population growth
#1) dynamic UI so that K boxes and perhaps pop2 inputs and graphs change on buttons
#2) remove trailing zeros of N and dN/dt for pop growth tab


#Competition
#1) put pics in comp info tab
#3) option to have animated/static plot(s)
#4) improve loading time of animated plots
#4b) warning message that animation takes long time to load
#5) user feedback while loading animated plot
#6) add background information


#Predation
#1) add background information
#3) move panel down
#5) display equation under panel on right side

#Both/all three
#1) user feedback around super high values--perhaps throws an error/warning message
#2) add some style/formatting to information panel
#3) hover over ui to get more information
#4) check that parameter ranges are accurate

#Other
#space out tabs to navbar

