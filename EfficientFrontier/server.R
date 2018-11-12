#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output, session) {
   
  # create a reactive item
  nStock <- reactive({
    
    if(input$button_tickerGenerate)
    {
      
      isolate( 
        lapply(1:input$nInputs, function(i)
        {
          fluidRow(
            column(width = 10,offset = 1, wellPanel(h4(paste("No.", i,"Input Values")),
                                                    textInput(paste("tInput",i), "Ticker",""),
                                                    numericInput(paste("min",i),"Min",value=NA),
                                                    numericInput(paste("max",i),"Max",value=NA),
                                                    numericInput(paste("expect",i),"Expected Return",value=NA))))
          
        })
      )    
    }
    
  })
  
  
  
  
}
