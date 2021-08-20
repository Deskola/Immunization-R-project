#Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(randomForest)
library(Metrics)




# Get and print current working directory.
print(getwd())

# Set current working directory.
#setwd("deni")
#print(getwd())

#import datasets
bike <- read.csv('bike_sharing.csv')
bike$yr <- as.factor(bike$yr)
bike$mnth <- factor(bike$mnth, levels = c('Jan','Feb','Mar',
                                          'Apr','May','Jun','Jul',
                                          'Aug','Sep','Oct','Nov','Dec'))
bike$weekday <- factor(bike$weekday, levels = c('Sunday','Monday','Tuesday',
                                                'Wednesday','Thursday','Friday','Saturday'))
bike$season <- factor(bike$season, levels = c('Spring','Summer','Fall','Winter'))


train_set <- read.csv('bike_train.csv')
train_set$hr <- as.factor(train_set$hr)
test_set <- read.csv('bike_test.csv')
test_set$hr <- as.factor(test_set$hr)

levels(test_set$mnth) <- levels(train_set$mnth)
levels(test_set$hr) <- levels(train_set$hr)
levels(test_set$holiday) <- levels(train_set$holiday)
levels(test_set$weekday) <- levels(train_set$weekday)
levels(test_set$weathersit) <- levels(train_set$weathersit)



#UI display
ui <- dashboardPage(
  #Dashboard title
  dashboardHeader(
    title = 'BIKE SHARING EXPLORER',
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
        "Dashboard",
        tabName = "dash",
        icon = icon('tachometer-alt')
      ),
      menuItem(
        "Prediction", 
        tabName = "pred", 
        icon = icon('search')
      )
    )
  ),
  
  #Tabs layout
  dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                tabItems(
                  #Plots tab content
                  tabItem('plots', 
                          #Histogram filter
                          box(status = 'primary', title = 'Filter for the histogram plot', 
                              selectInput('num', "Numerical variables:", c('Temperature', 'Feeling temperature', 'Humidity', 'Wind speed', 'Casual', 'New', 'Total')),
                              footer = 'Histogram plot for numerical variables'),
                          #Frequency plot filter
                          box(status = 'primary', title = 'Filter for the frequency plot',
                              selectInput('cat', 'Categorical variables:', c('Season', 'Year', 'Month', 'Hour', 'Holiday', 'Weekday', 'Working day', 'Weather')),
                              footer = 'Frequency plot for categorical variables'),
                          #Boxes to display the plots
                          box(plotOutput(outputId = 'histPlot')),
                          box(plotOutput(outputId = 'freqPlot'))),
                  
                  #Dashboard tab content
                  tabItem('dash',
                          #Dashboard filters
                          box(title = 'Filters', status = 'primary', width = 12,
                              splitLayout(cellWidths = c('4%', '42%', '40%'),
                                          div(),
                                          radioButtons( 'year', 'Year:', c('2011 and 2012', '2011', '2012')),
                                          radioButtons( 'regis', 'Registrations:', c('Total', 'New', 'Casual')),
                                          radioButtons( 'weather', 'Weather choice:', c('All', 'Good', 'Fair', 'Bad', 'Very Bad')))),
                          #Boxes to display the plots
                          box(plotOutput('linePlot')),
                          box(plotOutput(outputId = 'barPlot'), 
                              height = 550, 
                              h4('Weather interpretation:'),
                              column(6, 
                                     helpText('- Good: clear, few clouds, partly cloudy.'),
                                     helpText('- Fair: mist, cloudy, broken clouds.')),
                              helpText('- Bad: light snow, light rain, thunderstorm, scattered clouds.'),
                              helpText('- Very Bad: heavy rain, ice pallets, thunderstorm, mist, snow, fog.'))),
                  
                  #Prediction tab content
                  tabItem('pred',
                          #Filters for categorical variables
                          box(title = 'Categorical variables', 
                              status = 'primary', width = 12, 
                              splitLayout(
                                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                cellWidths = c('0%', '19%', '4%', '19%', '4%', '19%', '4%', '19%', '4%', '8%'),
                                selectInput( 'p_mnth', 'Month', c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec")),
                                div(),
                                selectInput('p_hr', 'Hour', c('0', '1', '2', '3', '4', '5', '6', '7', '8','9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')),
                                div(),
                                selectInput( 'p_weekd', 'Weekday', c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')),
                                div(),
                                selectInput( 'p_weather', 'Weather', c('Good', 'Fair', 'Bad', 'Very Bad')),
                                div(),
                                radioButtons( 'p_holid', 'Holiday', c('Yes', 'No')))),
                          #Filters for numeric variables
                          box(title = 'Numerical variables',
                              status = 'primary', width = 12,
                              splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                          sliderInput( 'p_hum', 'Humidity (%)', min = 0, max = 100, value = 0),
                                          div(),
                                          numericInput( 'p_temp', 'Temperature (Celsius)', 0),
                                          div(),
                                          numericInput( 'p_ftemp', 'Feeling temperature (Celsius)', 0),
                                          div(),
                                          numericInput( 'p_wind', 'Wind speed (mph)', 0))),
                          #Box to display the prediction results
                          box(title = 'Prediction result',
                              status = 'success', 
                              solidHeader = TRUE, 
                              width = 4, height = 260,
                              div(h5('Total number of registrations:')),
                              verbatimTextOutput("value", placeholder = TRUE),
                              div(h5('Range of number of registrations:')),
                              verbatimTextOutput("range", placeholder = TRUE),
                              actionButton('cal','Calculate', icon = icon('calculator'))),
                          #Box to display information about the model
                          box(title = 'Model explanation',
                              status = 'success', 
                              width = 8, height = 260,
                              
                  ))))
  
)

# R Shiny server
server <- function(input, output) {
  #Univariate analysis
  output$histPlot <- renderPlot({
    #hist(rnorm(25))
    #Column name variable
    num_val = ifelse(input$num == 'Temperature', 'temp',
                     ifelse(input$num == 'Feeling temperature', 'atemp',
                            ifelse(input$num == 'Humidity', 'hum',
                                   ifelse(input$num == 'Wind speed', 'windspeed',
                                          ifelse(input$num == 'Casual', 'casual',
                                                 ifelse(input$num == 'New', 'new', 'total'))))))
    
    #Histogram plot
    ggplot(data = bike, aes(x = bike[[num_val]]))+ 
      geom_histogram(stat = "bin", fill = 'steelblue3', 
                     color = 'lightgrey')+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = 'bold'))+
      labs(title = sprintf('Histogram plot of the variable %s', num_val),
           x = sprintf('%s', input$num),y = 'Frequency')+
      stat_bin(geom = 'text', 
               aes(label = ifelse(..count.. == max(..count..), as.character(max(..count..)), '')),
               vjust = -0.6)
  })
  
  output$freqPlot <- renderPlot({
    #Column name variable
    cat_val = ifelse(input$cat == 'Season', 'season',
                     ifelse(input$cat == 'Year', 'yr',
                            ifelse(input$cat == 'Month', 'mnth',
                                   ifelse(input$cat == 'Hour', 'hr',
                                          ifelse(input$cat == 'Holiday', 'holiday',
                                                 ifelse(input$cat == 'Weekday', 'weekday',
                                                        ifelse(input$cat == 'Working day', 'workingday', 'weathersit')))))))
    
    #Frecuency plot
    ggplot(data = bike, aes(x = bike[[cat_val]]))+
      geom_bar(stat = 'count', fill = 'mediumseagreen', 
               width = 0.5)+
      stat_count(geom = 'text', size = 4,
                 aes(label = ..count..),
                 position = position_stack(vjust = 1.03))+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face="bold"))+
      labs(title = sprintf('Frecuency plot of the variable %s', cat_val),
           x = sprintf('%s', input$cat), y = 'Count')
    
  })
  
  #Dashboard analysis
  output$linePlot <- renderPlot({
    
    if(input$year != '2011 and 2012'){
      
      #Creating a table filter by year for the line plot
      counts <- bike %>% group_by(mnth) %>% filter(yr == input$year) %>% summarise(new = sum(new), casual = sum(casual), total = sum(total))
    } else{
      
      #Creating a table for the line plot
      counts <- bike %>% group_by(mnth) %>% summarise(new = sum(new), casual = sum(casual), total = sum(total))
    }
    #Column name variable
    regis_val = ifelse(input$regis == 'Total', 'total',
                       ifelse(input$regis == 'New', 'new','casual'))
    
    #Line plot
    ggplot(counts, aes(x = mnth, y = counts[[regis_val]],
                       group = 1))+
      geom_line(size = 1.25)+
      geom_point(size = 2.25,
                 color = ifelse(counts[[regis_val]] == max(counts[[regis_val]]), 'red','black'))+
      labs(title = sprintf('%s bike sharing registrations by month', input$regis),
           subtitle = sprintf('Throughout the year %s \nMaximum value for %s registrations: %s \nTotal amount of %s registrations: %s', input$year, regis_val, max(counts[[regis_val]]), regis_val, sum(counts[[regis_val]])),
           x = 'Month', 
           y = sprintf('Count of %s registrations', regis_val))+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = 'bold'),
            plot.subtitle = element_text(size = 14))+
      ylim(NA, max(counts[[regis_val]])+7000)+
      geom_text(aes(label = ifelse(counts[[regis_val]] == max(counts[[regis_val]]), as.character(counts[[regis_val]]),'')),
                col ='red',hjust = 0.5, vjust = -0.7)
  })
  
  
}

shinyApp(ui, server)
