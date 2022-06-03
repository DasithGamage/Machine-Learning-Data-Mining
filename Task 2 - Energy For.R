
library(neuralnet)
library(readxl)
library(ggplot2)
library(tidyverse)
library(Metrics)
library(caret)

#importing the dataset
uowLoad <- read_excel("C:/Users/Pavithra Gayan/Desktop/ML CW/Task 2 - NN/UoW_load.xlsx")
View(uowLoad)

head(uowLoad)
names(uowLoad)
summary(uowLoad)
str(uowLoad)


#creating the normalization function
normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#creating the reverse normalization function
renormalization <- function(x, min, max) { 
  return((max - min)*x + min)
}

#Converting date column to numeric factors
dates <- factor(uowLoad$Dates)
dates <- as.numeric(uowLoad$Dates)
head(dates)

#11th hour division (for AR approach)
hour11 <- uowLoad[,4]

#Creating a data frame (with the necessary values for Neural Network)
class(hour11)
energyDf <- data.frame(dates, hour11)
str(energyDf)

#Normalizing Values (Input Vector 1)
energyDfNorm <- as.data.frame(lapply(energyDf, normalization))

names(energyDfNorm)[1] <- "date"
names(energyDfNorm)[2] <- "energyConsumption"

str(energyDfNorm)

#Applying scale to values (Input Vector 02)
energyDfScale <- as.data.frame(lapply(energyDf, scale))

names(energyDfScale)[1] <- "date"
names(energyDfScale)[2] <- "energyConsumption"

str(energyDfScale)

#Dividing data into training and test data sets

set.seed(194)
energyTrain <- energyDfNorm[1:430, ]
energyTest <- energyDfNorm[431:500, ]

set.seed(754)
energyTrain_1 <- energyDfScale[1:430, ]
energyTest_1 <- energyDfScale[431:500, ]


#Training the neural network models (Input Vector 01 - Normalized Data)

set.seed(234)
energyNN1 <- neuralnet(energyConsumption ~ ., data = energyTrain, 
                       hidden = c(3, 1))
plot(energyNN1)

set.seed(328)
energyNN2 <- neuralnet(energyConsumption ~ date + energyConsumption, 
                       data = energyTrain, hidden = c(5), 
                       act.fct = "logistic", linear.output = TRUE, 
                       learningrate = 3)
plot(energyNN2)

set.seed(730)
energyNN3 <- neuralnet(energyConsumption ~ date + energyConsumption, 
                       data = energyTrain, hidden = c(4), 
                       act.fct = "tanh", linear.output = FALSE, 
                       learningrate = 2)
plot(energyNN3)

set.seed(826)
energyNN4 <- neuralnet(energyConsumption ~ date + energyConsumption, 
                       data = energyTrain, hidden = c(5, 4), 
                       act.fct = "logistic", 
                       linear.output = TRUE, 
                       learningrate = 2,  algorithm = 'slr')
plot(energyNN4)

set.seed(324)
energyNN5 <- neuralnet(energyConsumption ~ date + energyConsumption, 
                       data = energyTrain, hidden = c(6, 4), 
                       act.fct = "tanh", linear.output = TRUE, learningrate = 1)
plot(energyNN5)

set.seed(597)
energyNN6 <- neuralnet(energyConsumption ~ date + energyConsumption, 
                       data = energyTrain, hidden = c(6, 4), 
                       act.fct = "logistic", linear.output = FALSE, 
                       learningrate = 2, algorithm = 'rprop+')
plot(energyNN6)

set.seed(295)
energyNN7 <- neuralnet(energyConsumption ~ date + energyConsumption, 
                       data = energyTrain, hidden = c(6, 4), 
                       act.fct = "tanh", linear.output = TRUE, 
                       learningrate = 3)
plot(energyNN7)

#Training the neural network models (Input Vector 02 - Scaled Data)

set.seed(648)
energyNN1_scale <- neuralnet(energyConsumption ~ date + energyConsumption, 
                             data = energyTrain_1, hidden = c(6), 
                             act.fct = "logistic", linear.output = FALSE, 
                             learningrate = 5)
plot(energyNN1_scale)

set.seed(619)
energyNN2_scale <- neuralnet(energyConsumption ~ date + energyConsumption, 
                             data = energyTrain_1, hidden = c(5), 
                             act.fct = "tanh", linear.output = TRUE, 
                             learningrate = 1)
plot(energyNN2_scale)

set.seed(973)
energyNN3_scale <- neuralnet(energyConsumption ~ date + energyConsumption, 
                             data = energyTrain_1, hidden = c(5, 3), 
                             act.fct = "logistic", linear.output = TRUE,         
                             algorithm = 'sag')
plot(energyNN3_scale)

set.seed(468)
energyNN4_scale <- neuralnet(energyConsumption ~ date + energyConsumption, 
                             data = energyTrain_1, hidden = c(4, 7), 
                             act.fct = "tanh", linear.output = FALSE, 
                             learningrate = 4, algorithm = 'rprop-')
plot(energyNN4_scale)

set.seed(394)
energyNN5_scale <- neuralnet(energyConsumption ~ date + energyConsumption, 
                             data = energyTrain_1, hidden = c(4, 7), 
                             act.fct = "logistic", linear.output = TRUE, 
                             learningrate = 2, algorithm = 'rprop-')
plot(energyNN5_scale)

set.seed(189)
energyNN6_scale <- neuralnet(energyConsumption ~ date + energyConsumption, 
                             data = energyTrain_1, hidden = c(4, 7), 
                             act.fct = "tanh", linear.output = TRUE, 
                             learningrate = 1)
plot(energyNN6_scale)

#Extracting our original outputs & finding min,max values for re-normalization
energyTrainTrue <- hour11[1:430, ]
energyTestTrue <- hour11[431:500,"11:00"]
energyTestTrue <- as.vector(energyTestTrue)

minTrain <- min(energyTrainTrue)
maxTrain <- max(energyTrainTrue)


#Evaluating Neural Network models (NN1)

energyNN1Result <- predict(energyNN1, energyTest)

NN1ResultRenorm <- renormalization(energyNN1Result, minTrain, maxTrain)
NN1ResultRenorm <- as.vector(NN1ResultRenorm)

rmse(NN1ResultRenorm, energyTestTrue$`11:00`)
mae(NN1ResultRenorm, energyTestTrue$`11:00`)
mape(NN1ResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN2)

energyNN2Result <- predict(energyNN2, energyTest)

NN2ResultRenorm <- renormalization(energyNN2Result, minTrain, maxTrain)
NN2ResultRenorm <- as.vector(NN2ResultRenorm)

rmse(NN2ResultRenorm, energyTestTrue$`11:00`)
mae(NN2ResultRenorm, energyTestTrue$`11:00`)
mape(NN2ResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN3)

energyNN3Result <- predict(energyNN3, energyTest)

NN3ResultRenorm <- renormalization(energyNN3Result, minTrain, maxTrain)
NN3ResultRenorm <- as.vector(NN3ResultRenorm)

rmse(NN3ResultRenorm, energyTestTrue$`11:00`)
mae(NN3ResultRenorm, energyTestTrue$`11:00`)
mape(NN3ResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN4)

energyNN4Result <- predict(energyNN4, energyTest)

NN4ResultRenorm <- renormalization(energyNN4Result, minTrain, maxTrain)
NN4ResultRenorm <- as.vector(NN4ResultRenorm)

rmse(NN4ResultRenorm, energyTestTrue$`11:00`)
mae(NN4ResultRenorm, energyTestTrue$`11:00`)
mape(NN4ResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN5)

energyNN5Result <- predict(energyNN5, energyTest)

NN5ResultRenorm <- renormalization(energyNN5Result, minTrain, maxTrain)
NN5ResultRenorm <- as.vector(NN5ResultRenorm)

rmse(NN5ResultRenorm, energyTestTrue$`11:00`)
mae(NN5ResultRenorm, energyTestTrue$`11:00`)
mape(NN5ResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN6)

energyNN6Result <- predict(energyNN6, energyTest)

NN6ResultRenorm <- renormalization(energyNN6Result, minTrain, maxTrain)
NN6ResultRenorm <- as.vector(NN6ResultRenorm)

rmse(NN6ResultRenorm, energyTestTrue$`11:00`)
mae(NN6ResultRenorm, energyTestTrue$`11:00`)
mape(NN6ResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN7)

energyNN7Result <- predict(energyNN7, energyTest)

NN7ResultRenorm <- renormalization(energyNN7Result, minTrain, maxTrain)
NN7ResultRenorm <- as.vector(NN7ResultRenorm)

rmse(NN7ResultRenorm, energyTestTrue$`11:00`)
mae(NN7ResultRenorm, energyTestTrue$`11:00`)
mape(NN7ResultRenorm, energyTestTrue$`11:00`)


#Evaluating Neural Network models (NN1-Scaled)

energyNN1ScaleResult <- predict(energyNN1_scale, energyTest)

NN1scaleResultRenorm <- renormalization(energyNN1ScaleResult, minTrain, 
                                        maxTrain)
NN1scaleResultRenorm <- as.vector(NN1scaleResultRenorm)

rmse(NN1scaleResultRenorm, energyTestTrue$`11:00`)
mae(NN1scaleResultRenorm, energyTestTrue$`11:00`)
mape(NN1scaleResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN2-Scaled)

energyNN2ScaleResult <- predict(energyNN2_scale, energyTest)

NN2scaleResultRenorm <- renormalization(energyNN2ScaleResult, minTrain, 
                                        maxTrain)
NN2scaleResultRenorm <- as.vector(NN2scaleResultRenorm)

rmse(NN2scaleResultRenorm, energyTestTrue$`11:00`)
mae(NN2scaleResultRenorm, energyTestTrue$`11:00`)
mape(NN2scaleResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN3-Scaled)

energyNN3ScaleResult <- predict(energyNN3_scale, energyTest)

NN3scaleResultRenorm <- renormalization(energyNN3ScaleResult, 
                                        minTrain, maxTrain)
NN3scaleResultRenorm <- as.vector(NN3scaleResultRenorm)

rmse(NN3scaleResultRenorm, energyTestTrue$`11:00`)
mae(NN3scaleResultRenorm, energyTestTrue$`11:00`)
mape(NN3scaleResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN4-Scaled)

energyNN4ScaleResult <- predict(energyNN4_scale, energyTest)

NN4scaleResultRenorm <- renormalization(energyNN4ScaleResult, 
                                        minTrain, maxTrain)
NN4scaleResultRenorm <- as.vector(NN4scaleResultRenorm)

rmse(NN4scaleResultRenorm, energyTestTrue$`11:00`)
mae(NN4scaleResultRenorm, energyTestTrue$`11:00`)
mape(NN4scaleResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN5-Scaled)

energyNN5ScaleResult <- predict(energyNN5_scale, energyTest)

NN5scaleResultRenorm <- renormalization(energyNN5ScaleResult, 
                                        minTrain, maxTrain)
NN5scaleResultRenorm <- as.vector(NN5scaleResultRenorm)

rmse(NN5scaleResultRenorm, energyTestTrue$`11:00`)
mae(NN5scaleResultRenorm, energyTestTrue$`11:00`)
mape(NN5scaleResultRenorm, energyTestTrue$`11:00`)

#Evaluating Neural Network models (NN6-Scaled)

energyNN6ScaleResult <- predict(energyNN6_scale, energyTest)

NN6scaleResultRenorm <- renormalization(energyNN6ScaleResult, 
                                        minTrain, maxTrain)
NN6scaleResultRenorm <- as.vector(NN6scaleResultRenorm)

rmse(NN6scaleResultRenorm, energyTestTrue$`11:00`)
mae(NN6scaleResultRenorm, energyTestTrue$`11:00`)
mape(NN6scaleResultRenorm, energyTestTrue$`11:00`)

#discussing the efficiency of best two models (energyNN2 & energyNN5)

energyNN2$weights
energyNN5$weights

energyNN2$startweights
energyNN5$startweights

summary(energyNN2$generalized.weights)
summary(energyNN5$generalized.weights)

gwplot(energyNN2, sub = "Generalized weight plot for energyNN2")
gwplot(energyNN5, sub = "Generalized weight plot for energyNN5") 

#Plot for the worst model (energyNN6)

plot(energyTestTrue$`11:00`, ylab = "Predicted vs Expected", 
     type="l", col="green" )
par(new = TRUE)
plot(NN6ResultRenorm, ylab = "", yaxt="n", type="l", col="red", 
     main='Predicted VS Expected Value (energyNN6)') 

#Plot for the winner model (energyNN5)

plot(energyTestTrue$`11:00`, ylab = "Predicted vs Expected",
     type="l", col="green" )
par(new = TRUE)
plot(NN5ResultRenorm, ylab = "", yaxt="n", type="l", col="red",
     main='Predicted VS Expected Value (energyNN5)') 

#Testing the accuracy

results <- data.frame(actual = energyTestTrue$`11:00`, 
                      prediction = NN5ResultRenorm)
results

actual <- results$actual
predicted <- results$prediction

comparison <- data.frame(predicted,actual)
deviation <- ((actual-predicted)/actual)
comparison <- data.frame(predicted,actual,deviation)

accuracy <- 1 - abs(mean(deviation))
accuracy

