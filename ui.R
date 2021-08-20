#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(party)
library(rsample)
library(readr)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        #Dashboard title
        dashboardHeader(
            title = 'IMMUNIZATION PREDICTION', 
            titleWidth = 290
        ), 
        
        #Sidebar layout
        dashboardSidebar(
            width = 290,
            sidebarMenu(
                menuItem(
                    "Plots", 
                    tabName = "plots", 
                    icon = icon('poll')
                ),
                menuItem(
                    "Predication", 
                    tabName = "dash", 
                    icon = icon('tachometer-alt')
                )
            )
        ),
        
        #Tabs layout
        dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                      tabItems(
                          #Plots tab content
                          tabItem('plots',
                                  #Frecuency plot filter
                                  box(status = 'primary', title = 'Filter for the frequency plot',
                                      selectInput('cat', 'Categorical variables:', c('Child Sex','Tetenus Jab','Counselled','HIV/AIDS Exposed','HIV/AIDS Tested',
                                                                                     'DPT3','Birt Month', 'Due1','Due2','Due3','Due3','BCG1','Polio1','DPT1','PCV1',
                                                                                     'ROTAV1','Polio2','DPT2','PCV2','ROTAV2')),
                                      footer = 'Frequency plot for categorical variables'),
                                  #Boxes to display the plots
                                  box(plotOutput('freqPlot'))),
                          
                          
                          tabItem('dash',
                                  #Filters for categorical variables
                                  box(title = 'Categorical variables', 
                                      status = 'primary', width = 12, 
                                      splitLayout(
                                          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                          cellWidths = c('0%', '19%', '4%', '19%', '4%', '19%', '4%', '19%', '4%', '8%'),
                                          # Input: Select a file ----
                                          fileInput("file1", "Choose CSV File",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          div(),
                                          # Input: Checkbox if file has header ----
                                          checkboxInput("header", "Header", TRUE),
                                          div(),
                                          # Input: Select separator ----
                                          radioButtons("sep", "Separator",
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"),
                                                       selected = ","),
                                          div(),
                                          # Input: Select quotes ----
                                          radioButtons("quote", "Quote",
                                                       choices = c(None = "",
                                                                   "Double Quote" = '"',
                                                                   "Single Quote" = "'"),
                                                       selected = '"'),
                                          
                                          
                                          
                                          div(),
                                          # Input: Select number of rows to display ----
                                          radioButtons("disp", "Display",
                                                       choices = c(Head = "head",
                                                                   All = "all"),
                                                       selected = "head")
                                          
                                      )),
                                  #Filters for categorical variables
                                  box(title = 'Categorical variables', 
                                      status = 'primary', width = 12, 
                                      fluidRow(
                                          column(12,
                                                 tableOutput("contents")
                                          )
                                      )),
                                  
                                  
                                  
                                  #Box to display the prediction results
                                  box(title = 'Prediction result',
                                      status = 'success', 
                                      solidHeader = TRUE, 
                                      width = 4, height = 260,
                                      actionButton('cal','Predict', icon = icon('calculator')),
                                      div(),
                                      radioButtons('format', 'Document type', c('HTML','Word'),
                                                   inline=TRUE),
                                      div(),
                                      downloadButton("downloadReport","Summary Report"),
                                      div(),
                                      div(),
                                      downloadButton("downloadcsv","CSV Report")),
                                
                                  
                                  #Box to display information about the model
                                  box(title = 'Model explanation',
                                      status = 'success',
                                      plotOutput('test_out')
                                  ),
                                  #Box to display information about the model
                                  box(plotOutput('sigparameter')),
                                  box(plotOutput('confMatrix'))
                          ))
                      
                      
        ),
    )
)
