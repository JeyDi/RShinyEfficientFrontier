#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#    https://rstudio.github.io/shinydashboard/get_started.html

library(shiny)
library(shinydashboard)

setwd("../")
print(paste("Default UI working directory:",getwd()))

#Edit the main header of the dashboard
header <- dashboardHeader(
  title = "Efficient Frontier"
)
  
  
#Edit the body of the dashboard
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "config",
      fluidRow(
        box(
          title = "Select Number of tickers", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          "Choose a number of title that you want to have", br(), "Insert the names of the titles inside the input box",
          sliderInput("input_slider_tickerNumber", "Slider input:", 1, 10, 1),
          textInput("input_text_tickerName", "Ticker name:"),
          dateRangeInput("input_date_tickerDates", 
                         "Date range",
                         # Default data from past three months
                         start = Sys.Date() - 90, 
                         end = Sys.Date()),
          actionButton("button_tickerGenerate","Generate"),
          br(),
          verbatimTextOutput("output_stock_result"),
          br(),
          uiOutput("output_stock")
          )
      )
    )
  )
  
)

#Edit the sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Configuration", tabName = "config",icon = icon("cogs"))
    , menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    , menuItem("Contacts", icon= icon("file-code-o"),href = "https://github.com/JeyDi/RShinyEfficientFrontier")
  )
)


#Function for rendering the page
dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)








