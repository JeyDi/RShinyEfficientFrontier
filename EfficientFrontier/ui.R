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

#Edit the main header of the dashboard
header <- dashboardHeader(
  title = "Efficient Frontier"
)
  
  
#Edit the body of the dashboard
body <- dashboardBody(
  
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








