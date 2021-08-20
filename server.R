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
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(party)
library(rsample)
library(readr)
library(e1071)

#data <- read.csv("vac_sharing.csv")
#test <- read.csv("vac_test.csv")

######################################################
# load data
registry <- read.csv('vac.csv')
#View(registry) 
#str(registry)
#dim(registry)
summary(registry)

#clean data of null variables
clean = registry %>% drop_na()


#convert to factors
clean$birthmonth <- as.factor(ifelse(clean$birthmonth  == 1, 'Jan',
                                     ifelse(clean$birthmonth==2,'Feb',
                                            ifelse(clean$birthmonth==3, 'Mar',
                                                   ifelse(clean$birthmonth == 4,'Apr',
                                                          ifelse(clean$birthmonth==5,'May',
                                                                 ifelse(clean$birthmonth==6,'Jun',
                                                                        ifelse(clean$birthmonth==7,'Jul',
                                                                               ifelse(clean$birthmonth==8,'Aug',
                                                                                      ifelse(clean$birthmonth==9,'Sep',
                                                                                             ifelse(clean$birthmonth==10,'Oct',
                                                                                                    ifelse(clean$birthmonth==11,'Nov','Dec'))))))))))))
clean$due1 <- as.factor(ifelse(clean$due1  == 1, 'Jan',
                               ifelse(clean$due1==2,'Feb',
                                      ifelse(clean$due1==3, 'Mar',
                                             ifelse(clean$due1 == 4,'Apr',
                                                    ifelse(clean$due1==5,'May',
                                                           ifelse(clean$due1==6,'Jun',
                                                                  ifelse(clean$due1==7,'Jul',
                                                                         ifelse(clean$due1==8,'Aug',
                                                                                ifelse(clean$due1==9,'Sep',
                                                                                       ifelse(clean$due1==10,'Oct',
                                                                                              ifelse(clean$due1==11,'Nov','Dec'))))))))))))
clean$due2 <- as.factor(ifelse(clean$due2  == 1, 'Jan',
                               ifelse(clean$due2==2,'Feb',
                                      ifelse(clean$due2==3, 'Mar',
                                             ifelse(clean$due2 == 4,'Apr',
                                                    ifelse(clean$due2==5,'May',
                                                           ifelse(clean$due2==6,'Jun',
                                                                  ifelse(clean$due2==7,'Jul',
                                                                         ifelse(clean$due2==8,'Aug',
                                                                                ifelse(clean$due2==9,'Sep',
                                                                                       ifelse(clean$due2==10,'Oct',
                                                                                              ifelse(clean$due2==11,'Nov','Dec'))))))))))))
clean$due3 <- as.factor(ifelse(clean$due3  == 1, 'Jan',
                               ifelse(clean$due3==2,'Feb',
                                      ifelse(clean$due3==3, 'Mar',
                                             ifelse(clean$due3 == 4,'Apr',
                                                    ifelse(clean$due3==5,'May',
                                                           ifelse(clean$due3==6,'Jun',
                                                                  ifelse(clean$due3==7,'Jul',
                                                                         ifelse(clean$due3==8,'Aug',
                                                                                ifelse(clean$due3==9,'Sep',
                                                                                       ifelse(clean$due3==10,'Oct',
                                                                                              ifelse(clean$due3==11,'Nov','Dec'))))))))))))
clean$due4 <- as.factor(ifelse(clean$due4  == 1, 'Jan',
                               ifelse(clean$due4==2,'Feb',
                                      ifelse(clean$due4==3, 'Mar',
                                             ifelse(clean$due4 == 4,'Apr',
                                                    ifelse(clean$due4==5,'May',
                                                           ifelse(clean$due4==6,'Jun',
                                                                  ifelse(clean$due4==7,'Jul',
                                                                         ifelse(clean$due4==8,'Aug',
                                                                                ifelse(clean$due4==9,'Sep',
                                                                                       ifelse(clean$due4==10,'Oct',
                                                                                              ifelse(clean$due4==11,'Nov','Dec'))))))))))))

#p <- function(x){is.na(x)/length(x)*100}
#apply(clean, 2, p)
#summary(clean)                                                  
convert <- lapply(clean[2:22], factor)
#View(convert)     

#bind with ID
immunization <- cbind(clean[1], convert)
#dim(immunization)
head(immunization)

#Write the new file
data <- immunization

write.csv(data, "vac_sharing.csv", row.names = FALSE)

#Split data into training and testing
set.seed(123)
# choosing 75% of the data to be the training data
data_split <- initial_split(immunization[-18], prop = .75)

# extracting training data and test data as two seperate dataframes
train <- training(data_split)
test  <- testing(data_split)

head(train)
less_data <- head(test)

#Write new files for the train and test sets
write.csv(train, "vac_train.csv", row.names = FALSE)
write.csv(test, "vac_test.csv", row.names = FALSE)
write.csv(less_data, "less_test.csv", row.names = FALSE)

#get optimal mtry
#mtry <- tuneRF(train[-1], train$dpt3, ntreeTry=100,
#               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
#best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
#print(mtry)
#print(best.m)


#change role
#install.packages("recipes")
library(recipes)
rec <- recipe(train) %>%
    update_role(birthmonth, due1, due2, due3, due4, bcg1, polio, polio1, dpt1,
                pcv1, rotav1, polio2, dpt2, pcv2, rotav2, pab, conselled, 
                hiv_exposed, hiv_tested,
                new_role = "predictor") %>%
    update_role(dpt3, new_role = "outcome") %>%
    update_role(public_id, new_role = "id variable")
#rec

#random forest

classifier = randomForest(dpt3~., data=train[-1],
                          importance = TRUE, ntree = 100,mtry = 4,
                          replace=TRUE, random_state = 0)
#print(classifier)

#Saving the model
saveRDS(classifier, file = "./model.rds")
#quit()

#Importing model
train_tiny = test[-1]
test_sample <- head(test, 50)
write.csv(test_sample, "sample1.csv", row.names = FALSE)
sample_set <- read.csv('sample1.csv')[-1]
tiny <- head(sample_set,3)
#View(sample_set)

xtest <- rbind(train_tiny[1, ] , tiny)
xtest <- xtest[-1,]


model_rf <- readRDS(file = './model.rds')

y_pred = predict(model_rf, newdata = xtest)
###################################################



#import model
model_rf <- readRDS(file = './model.rds')
#y_pred = predict(model_rf, newdata = xtest)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$freqPlot <- renderPlot({
        #Column name variable
        cat_val = ifelse(input$cat == 'Child Sex', 'childsex',
                         ifelse(input$cat == 'Tetenus Jab', 'pab',
                                ifelse(input$cat == 'Counselled', 'conselled',
                                       ifelse(input$cat == 'HIV/AIDS Exposed', 'hiv_exposed',
                                              ifelse(input$cat == 'HIV/AIDS Tested', 'hiv_tested',
                                                     ifelse(input$cat == 'DPT3', 'dpt3',
                                                            ifelse(input$cat == 'BCG1','bcg1', 
                                                                   ifelse(input$cat == 'Polio1','polio1',
                                                                          ifelse(input$cat == 'DPT1','dpt1',
                                                                                 ifelse(input$cat == 'PCV1','pcv1',
                                                                                        ifelse(input$cat == 'ROTAV1','rotav1',
                                                                                               ifelse(input$cat == 'Polio2','polio2',
                                                                                                      ifelse(input$cat == 'DPT2','dpt2',
                                                                                                             ifelse(input$cat == 'PCV2','pcv2',
                                                                                                                    ifelse(input$cat == 'ROTAV2','rotav2',
                                                                                                                    )))))))))))
                                       ))))
        
        #Frecuency plot
        ggplot(data = data, aes(x = data[[cat_val]]))+
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
    
    #calcuate button
    a <- reactiveValues(data = NULL)
    b <- reactiveValues(pred = NULL)
    c <- reactiveValues(predData = NULL)
    d <- reactiveValues(finalData = NULL)
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
                
                a$data <- read.csv(input$file1$datapath,
                                   header = input$header,
                                   sep = input$sep,
                                   quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    #Prediction model
    #React value when using the action button
    observeEvent(input$cal, {
        test_pred <- a$data
        d$finalData = test_pred
        test_pred <- test_pred[-1]
        #View(test_pred)
        train_tiny = test[-1]
        tiny <- test_pred
        
        xtest <- rbind(train_tiny[1, ] , tiny)
        xtest <- xtest[-1,]
        c$predData = xtest
        #View(xtest)
        new_pred = predict(model_rf, newdata = xtest)
        b$pred = new_pred
        d$finalData$predictions <- new_pred
        
        output$test_out <- renderPlot({
            #paste(b$pred)
           plot(new_pred)
        });
        
        #output$results <- renderPrint({
        #    paste(new_pred)
        #})
        
        output$sigparameter <- renderPlot({
            #Variable Importance
            randomForest::importance(model_rf)
            varImpPlot(model_rf)
        })
        
        output$confMatrix <- renderPlot({
            #Variable Importance
            plot(model_rf)
        })
        
    })
    
    #generate a report
    output$downloadReport <- downloadHandler(
        filename = function(){
            paste('my_report', sep=".", switch (
                input$format, HTML = 'html', Word = 'docx'
                
            ))
        },
        content = function(file){
            src <- normalizePath('report.Rmd')
            
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            
            library(rmarkdown)
            out <- render('report.Rmd', switch (
                input$format,
                HTML= html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )
    
    #Generte csv reposrt
    output$downloadcsv <- downloadHandler(
        filename = "prediction.csv",
        content = function(file) {
            write.csv(d$finalData, file, row.names = FALSE)
        }
    )

})
