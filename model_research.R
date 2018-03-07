library(caret)
library(caTools)
library(Metrics)

dates <- read.csv('data/dates.csv', stringsAsFactors = FALSE)

corn <- read.csv('data/corn.csv', stringsAsFactors = FALSE)[, 1:2]
corn$Date <- as.character(corn$Date)
corn <- corn[3:length(corn$Date), ]

feeder <- read.csv('data/feeder.csv', stringsAsFactors = FALSE)[, 1:2]
feeder$Date <- as.character(feeder$Date)

live <- read.csv('data/live_cattle.csv', stringsAsFactors = FALSE)[, 1:2]
live$Date <- as.character(live$Date)
live <- live[3:length(live$Date), ]

live <- live[live$Date %in% dates$Date, ]
corn <- corn[corn$Date %in% dates$Date, ]
feeder <- feeder[feeder$Date %in% dates$Date, ]

hist.df <- data.frame(date=as.character(dates$Date), live=live$Open, corn=corn$Open,feeder=feeder$Open)
hist.df$date <- as.character(hist.df$date)

# TRAIN/TEST SPLIT
sample <- sample.split(hist.df$date, SplitRatio = 0.9)
train <- hist.df[sample == TRUE, ]
test <- hist.df[sample == FALSE, ]
test.labels <- test$live
train <- subset(train, select = -c('date'))
test <- subset(test, select = -c('live'))

# REGRESSION ALGORITHMS
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
kknn.model <- caret::train(live ~ corn + feeder, train, trControl = control)
kknn.data <- predict(kknn.model, test)
kknn.mape <- mape()