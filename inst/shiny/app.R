library(shiny)

library(tidyverse)
library(deSolve)
library(ggplot2)
library(dygraphs)
library(reshape)
library(xts)
library(gbd)

library(knitr)

theme_set(theme_bw())

data(demo)
dplot<-ggplot(demo,aes(x=Low,y=l,label=l)) +
  geom_col(fill="lightgrey") + ylab("UK population millions")+
  geom_label() +
  xlab("Age") +ggtitle("UK demographic numbers for reference in deciding vulnerabile class numbers")

## Create an GBD function



# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Protect the vulnerable: Simulation experiment"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(type = "tabs",

        tabPanel("Phase 1",
         sliderInput("R01","R_0 for less vulnerable", min=0.8,max=5, step=0.1,value=2.5),
         sliderInput("R02","R_0 for the most vulnerable", min=0,max=1, step=0.1,value=1),
         sliderInput("R012","R_0 for infections of the most vulnerable", min=0,max=2, step=0.1,value=1),
         sliderInput("days","Number of days of phase 1", min=30,max=600, step=1,value=100)),


        tabPanel("Phase 2",
                 checkboxInput("second", "Run phase 2", FALSE),
                 sliderInput("R11","R_0 for less vulnerable", min=0.8,max=5, step=0.1,value=2.5),
                 sliderInput("R12","R_0 for the most vulnerable", min=0,max=1, step=0.1,value=1),
                 sliderInput("R112","R_0 for infections of the most vulnerable", min=0,max=2, step=0.1,value=1),
                 sliderInput("days2","Number of days of phase 2", min=30,max=600, step=1,value=100)),

         tabPanel("Population",
         sliderInput("ifr","Incidence fatality ratio for the vulnerable. Expressed per thousand", min=1,max=50, step=1,value=10),
         sliderInput("ifr2","Incidence fatality ratio for the less vulnerable. Expressed per ten thousand",min =0,max=20, step=0.1,value=2),
         sliderInput("pop","Population of vulnerable people in millions", min=1,max=60, step=1,value=2),
         sliderInput("pop2","Population of less vulnerable people in millions", min=1,max=60, step=1,value=40))




      )),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot all",  dygraphOutput("plot1")),
                    tabPanel("Plot most vulnerable", dygraphOutput("plot2")),
                    tabPanel("Plot less vulnerable", dygraphOutput("plot3")),
                    tabPanel("Plot mortality", plotOutput("mortplot"),
                             plotOutput("demoplot"))


        ))

   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

observe({
  days<-input$days
  days2<- input$days2
  if (input$second==FALSE)days2<-1

  R01<-input$R01
  R02<-input$R02
  R012<-input$R012
  ifr<-input$ifr/1000
  pop<-input$pop
  ifr2<-input$ifr2/10000
  pop2<-input$pop2
gamma <- 0.2


  sd<-run_sim(R01 = R01,R02=R02, R012=R012,gamma=gamma, days1=days,days2=days2)



  vdeaths<-round(max(sd$R2) * ifr * pop*1000,1)
  sdeaths<-round(max(sd$R1) * ifr2 * pop2*1000,1)
  deaths<-vdeaths + sdeaths
  titl<-sprintf("Modelled number of deaths %s thousand", deaths)

  dts<-data.frame(strata=c("Vulnerable","Less_vulnerable"), value=c(vdeaths,sdeaths))
 output$mortplot <- renderPlot({
   ggplot(dts,aes(x=strata,y=value,label=value)) +
     geom_col(width=0.4,fill="lightgrey") +
     geom_label() +ylab("Numbers of deaths in thousands") + ylim(0,deaths) +
     ggtitle(titl)
 })

 output$demoplot <- renderPlot(dplot)

  output$plot1 <- renderDygraph({
    dygraph(sd, main=titl)
  })

  output$plot2 <- renderDygraph({
    dygraph(sd[,c(1,5:7)], main = titl)
  })
  output$plot3 <- renderDygraph({
    dygraph(sd[,c(1:4)], main = titl)
  })
})
}

# Run the application
shinyApp(ui = ui, server = server)

