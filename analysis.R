#load packages

library(tidyr)
library(caret)
library(randomForest)
library(party)
library(rsample)
library(readr)


# Get and print current working directory.
print(getwd())

# Set current working directory.
#setwd("bartha")
#print(getwd())

# load data
registry <- read.csv('vac.csv')
View(registry) 
str(registry)
dim(registry)
summary(registry)

#clean data of null variables
clean = registry %>% drop_na()
dim(registry)
summary(registry)
clean <- na.omit(clean)
dim(registry)
summary(registry)

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
summary(clean)                                                  
convert <- lapply(clean[2:22], factor)
View(convert)     

#bind with ID
immunization <- cbind(clean[1], convert)
dim(immunization)
str(immunization)
View(immunization)
summary(is.na(immunization$childsex))

#Write the new file
#data <- immunization
write.csv(immunization, "vac_sharing.csv", row.names = FALSE)

#Split data into training and testing
set.seed(123)
# choosing 75% of the data to be the training data
data_split <- initial_split(immunization[-18], prop = .75)

# extracting training data and test data as two seperate dataframes
train <- training(data_split)
test  <- testing(data_split)

train_less <- head(train, 200)
test_less <- head(test, 10)

#Write new files for the train and test sets
write.csv(train, "vac_train.csv", row.names = FALSE)
write.csv(test, "vac_test.csv", row.names = FALSE)
write.csv(test_less, "test_less_data.csv", row.names = FALSE)
write.csv(train_less, "train_less_data.csv", row.names = FALSE)

#get optimal mtry



#change role

library(recipes)
rec <- recipe(train) %>%
  update_role(birthmonth, due1, due2, due3, due4, bcg1, polio, polio1, dpt1,
              pcv1, rotav1, polio2, dpt2, pcv2, rotav2, pab, conselled, 
              hiv_exposed, hiv_tested,
              new_role = "predictor") %>%
  update_role(dpt3, new_role = "outcome") %>%
  update_role(public_id, new_role = "id variable")
rec

#random forest
classifier = randomForest(dpt3~., data=train_less[-1],
                          importance = TRUE, ntree = 100,mtry = 4,
                          replace=TRUE, random_state = 0)
print(classifier)

#Saving the model
saveRDS(classifier, file = "./model.rds")
#quit()

#Importing model
train_tiny = test[-1]
test_sample <- head(test, 50)
write.csv(test_sample, "sample1.csv", row.names = FALSE)
sample_set <- read.csv('sample1.csv')[-1]
tiny <- head(sample_set,3)
View(sample_set)

xtest <- rbind(train_tiny[1, ] , tiny)
xtest <- xtest[-1,]

View(xtest)


model_rf <- readRDS(file = './model.rds')

y_pred = predict(model_rf, newdata = xtest)
print(y_pred)
plot(y_pred)

#Variable Importance
randomForest::importance(classifier)
varImpPlot(classifier)

#check with test 
pred = predict(classifier, newdata = test, type = "class")
pred

plot(pred)

#confusion matrix
confusionMatrix(y_pred, xtest$dpt3)


