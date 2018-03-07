library(ggplot2)
library(caret)
library(Metrics)
library(scales)
library(caTools)

set.seed(101)

# READ DATA
cattle <- read.csv('data/cattle_data.csv')
date <- cattle$date
cattle <- subset(cattle, select = c(-date))

# TRAIN/TEST SPLIT
sample <- sample.split(Y = cattle$live, SplitRatio = 0.99)
train <- subset(cattle, sample == TRUE)
test <- subset(cattle, sample == FALSE)
test.labels <- test$live
test <- subset(test, select = c(-live))

# REGRESSION MODEL
control = trainControl(method = "repeatedcv", repeats = 10, number = 10)
lin.model <- caret::train(data = train, live ~ ., 'kknn', trControl = control, metric = "RMSE")
lin.data <- predict(lin.model, test)
lin.mape <- mape(lin.data, test.labels)