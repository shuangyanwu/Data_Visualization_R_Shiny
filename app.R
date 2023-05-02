source("namefun1.R")
source("namefun3.R")
library(shiny)
library(tidyverse)
library(dplyr)

ui <- fluidPage (
  titlePanel("Popularity of Baby Names"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("loc", h4("Choose the location"), 
                  choices = list('US', 'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 
                                  'Colorado', 'Connecticut', 'Delaware','District of Columbia', 'Florida', 
                                  'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas',
                                  'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 
                                  'Minnesota', 'Mississippi', 'Missouri', 'Montana', 
                                  'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 
                                 'New Mexico', 'New York', 
                                  'North Carolina', 'North Dakota', 'Ohio', 
                                  'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 
                                  'South Carolina', 'South Dakota', 'Tennessee', 'Texas',
                                  'Utah', 'Vermont', 'Virginia', 'Washington', 
                                  'West Virginia', 'Wisconsin', 
                                  'Wyoming'), selected = 'US'),
      
      selectInput("sex", h4("Choose the sex"), 
                  choices = list("male" = "M", "female" = "F"), selected = "M"),
      
      numericInput("value", 
                   h5("How many names should be on the top ranked list?"), 
                   min = 1, max = 20,value = 10,step=1),
      
      sliderInput(inputId ="year",
                  h5("What year should the top names be plotted for?"),
                  min = 1880,
                  max = 2021,sep = "",
                  step=1,
                  value = 2021), 
                  
      sliderInput(inputId ="range",
                  h5("Choose the range of years to plot popularity"),
                  min = 1880,
                  max = 2021,sep = "",
                  step=1,
                  value = c(1880,2021)),
    
      textInput("t1", h4("first name"), 
              value = ""),
      
      textInput("t2", h4("second name"), 
                value = ""),
      
      textInput("t3", h4("third name"), 
                value = ""),
      
      textInput("t4", h4("fourth name"), 
                value = ""),
      
      textInput("t5", h4("fifth name"), 
                value = ""),
      
      textInput("t6", h4("sixth name"), 
                value = ""),
      
      textInput("t7", h4("seventh name"), 
                value = ""),
      
      textInput("t8", h4("eigth name"), 
                value = ""),
      
    ),

      mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Most Popular Names", plotOutput("barplot")),
                  tabPanel("Name Popularity Over Time", plotOutput("lineplot"))
      )
  )))

server <- function(input, output) {
  output$barplot<-renderPlot({
    
    loc=input$loc
    sex=input$sex
    value=input$value
    year=input$year
    name (loc, sex, value, year)
  })
  
  output$lineplot<-renderPlot({

    validate(need(input$t1,'You must enter at least one name'))
    
    loc=input$loc
    sex=input$sex
    range=input$range
    t1=input$t1
    t2=input$t2
    t3=input$t3
    t4=input$t4
    t5=input$t5
    t6=input$t6
    t7=input$t7
    t8=input$t8
    linep (loc, sex, range, t1,t2,t3,t4,t5,t6,t7,t8)

  })
}

shinyApp(ui = ui, server = server)
