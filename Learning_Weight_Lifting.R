# Library to manipulate data
library(dplyr)
library(caret)

# Download and read data from the training and testing sets
"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" %>%
        download.file(destfile = "pml-training.csv")
training <- read.csv("pml-training.csv")
training <- select(training
                   ,starts_with("magnet_")
                   ,starts_with("gyros_")
                   ,starts_with("accel_")
                   ,starts_with("roll_")
                   ,starts_with("pitch_")
                   ,starts_with("yaw_")
                   ,classe)

inTrain <- createDataPartition(y = training$classe
                               ,p = 0.6, list = FALSE)
trainCV <- training[inTrain,]
testCV <- training[-inTrain,]

"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" %>%
        download.file(destfile = "pml-testing.csv")
testing <- read.csv("pml-testing.csv")
testing <- select(testing
                   ,starts_with("magnet_")
                   ,starts_with("gyros_")
                   ,starts_with("accel_")
                   ,starts_with("roll_")
                   ,starts_with("pitch_")
                   ,starts_with("yaw_")
                   ,-contains("belt"))

featurePlot(x = select(trainCV, starts_with("pitch_")), y = trainCV$classe, plot = "pairs")

trainCV <- select(trainCV, -contains("belt"))
testCV <- select(testCV, -contains("belt"))

preProc <- preProcess(trainCV[,-which(names(trainCV) == "classe")]
                      ,method = "pca", pcaComp = 15)
trainPC <- predict(preProc
                   ,trainCV[,-which(names(trainCV) == "classe")])
testPC <- predict(preProc
                  ,testCV[,-which(names(trainCV) == "classe")])

# Model using linear discriminant analysis
wtlftldaPC <- train(trainCV$classe ~ ., data = trainPC
                  ,method = "lda"
                  ,trControl = trainControl(method = "CV"))

wtlftlda <- train(classe ~ ., data = trainCV
                    ,method = "lda"
                    ,trControl = trainControl(method = "CV"))

resultldaPC <- predict(wtlftldaPC, newdata = testPC)
resultlda <- predict(wtlftlda, newdata = testCV[,-which(names(trainCV) == "classe")])
confusionMatrix(testCV$classe, resultldaPC)
confusionMatrix(testCV$classe, resultlda)

finalresult <- predict(wtlftlda, newdata = testing)

