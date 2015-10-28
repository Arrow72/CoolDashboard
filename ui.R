library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid)
library(lubridate)
library(scales)
library(Cairo)
library(gtable)

# setwd('C:/R/Shiny/CoolDashboard')

# A dashboard body with a row of infoBoxes and valueBoxes, and two rows of boxes
body <- dashboardBody(
  fluidRow(
    #       column(2,sliderInput("n1", "n1:",min = 1, max = 100, value=74)),
    #       column(2,sliderInput("n2", "n2:",min = 1, max = 100, value=14)),
    #       column(2,sliderInput("n3", "n3:",min = 1, max = 100, value=12)),
    
    column(5,plotOutput("plot3", height="20%")),
    column(7,plotOutput("plot4", height="20%")),
    
    column(12,
           box(
             box(background = 'black',solidHeader = TRUE,width = 2,#height = 140,
                 htmlOutput("CoolLabel1")),        
             box(background = 'black',solidHeader = TRUE,width = 2,#height = 140,
                 htmlOutput("CoolLabel2")),        
             box(background = 'black',solidHeader = TRUE,width = 2,height = 125,
                 plotOutput("plot1")),
             box(background = 'black',solidHeader = TRUE,width = 2,#height = 140,
                 htmlOutput("CoolLabel4")),        
             box(background = 'black',solidHeader = TRUE,width = 2,#height = 140,
                 htmlOutput("CoolLabel5")),        
             box(background = 'black',solidHeader = TRUE,width = 2,#height = 140,
                 htmlOutput("CoolLabel6")),
             solidHeader = TRUE,background = 'black',width = 12,height = 140
           )
    ),
    column(4,plotOutput("plot2", height="20%")),
    column(8,plotOutput("plot5", height="20%"))
  )
)


shinyUI(dashboardPage(skin = "black", 
                      dashboardHeader(disable = TRUE),
                      dashboardSidebar(disable = TRUE),
                      body
))
