library(caret)
train_model <- function(data, control) {
  kknn.model <- caret::train(live ~ corn + feeder, data, 'kknn', trControl = control)
  return(kknn.model)
}