source('train_model.R')
library(plyr)
library(scales)

# READ AND FORMAT DATA
set.seed(101)
WINDOW_LENGTH <- 30 # n days of backtesting at present time
ACTION_TIME <- 50 # act every n days
SEED_MONEY <- 100 # starting capital
TOTAL_FUNDS <- 100 # portfolio worth
POSITIONS_SIGNAL <- c()
POSITIONS_PRICES <- c()
PL <- c()
ASSETS <- 0

dates <- read.csv('data/dates.csv', stringsAsFactors = FALSE)

corn <- read.csv('data/corn.csv', stringsAsFactors = FALSE)[, 1:2]
corn$Date <- as.character(corn$Date)
corn <- corn[3:length(corn$Date), ]

feeder <- read.csv('data/feeder.csv', stringsAsFactors = FALSE)[, 1:2]
feeder$Date <- as.character(feeder$Date)

live <- read.csv('data/live_cattle.csv', stringsAsFactors = FALSE)
lr <- live$Returns
live <- live[, 1:2]
live$Date <- as.character(live$Date)
live <- live[3:length(live$Date), ]

live <- live[live$Date %in% dates$Date, ]
corn <- corn[corn$Date %in% dates$Date, ]
feeder <- feeder[feeder$Date %in% dates$Date, ]

hist.df <- data.frame(date = as.character(dates$Date), live = live$Open, corn = corn$Open,feeder=feeder$Open)
hist.df$date <- as.character(hist.df$date)

current_day <- hist.df[length(hist.df$date) - WINDOW_LENGTH, ]$date
current_index <- which(hist.df$date == current_day)

for(i in 1 : length(hist.df$date)) {
  if(current_index < 0) {
    break
  }
  window_data <- hist.df[(current_index + 1):length(hist.df$date), ]

  control <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 10)
  model <- train_model(window_data, control)
  current_data <- hist.df[current_index, ]
  predictions <- predict(model, current_data[, c('corn', 'feeder')])
  
  if(current_data$live < predictions && length(predictions) > 0) {
    POSITIONS_SIGNAL <- append(POSITIONS_SIGNAL, 1)
    POSITIONS_PRICES <- append(POSITIONS_PRICES, current_data$live)
    TOTAL_FUNDS <- TOTAL_FUNDS - current_data$live
    ASSETS <- ASSETS + 1
  }
  if(current_data$live > predictions && length(predictions) > 0) {
    POSITIONS_SIGNAL <- append(POSITIONS_SIGNAL, -1)
    POSITIONS_PRICES <- append(POSITIONS_PRICES, current_data$live)
    TOTAL_FUNDS <- TOTAL_FUNDS + current_data$live
    ASSETS <- ASSETS - 1
  }
  print(paste("Current Date: ", current_data$date, sep = ""))
  print(paste("Backtest % Complete: ", 100 * (i / (length(hist.df$date) / ACTION_TIME)), sep = ""))
  print(paste("Total Funds: $", TOTAL_FUNDS))
  PL <- append(PL, (TOTAL_FUNDS - SEED_MONEY)/TOTAL_FUNDS)
  current_index <- current_index - ACTION_TIME
}
POSITIONS <- as.data.frame(cbind(POSITIONS_SIGNAL, POSITIONS_PRICES))

# GRAPHS
sp <- read.csv('data/^GSPC.csv')$Returns
m1 <- mean(sp, na.rm = TRUE)
ax1 <- as.Date(dates$Date[1 : length(sp)])
sp.df <- data.frame(axis = ax1, returns = sp)
m2 <- mean(PL)
pp <- c()
ind <- 1
for(i in 1 : length(sp.df$axis)) {
  pp <- append(pp, PL[ind])
  if(i %% ACTION_TIME == 0) {
    ind <- ind + 1
  }
}
pl.df <- data.frame(axis = ax1, returns = pp)
na.array <- rep(NA, 14)
lr <- append(lr, na.array)
m3 <- mean(lr, na.rm = TRUE)
live.df <- data.frame(axis = ax1, returns = lr)

p <- ggplot(sp.df, aes(x = axis, y = returns)) + geom_line(size = 1.5, color = '#34495e') + geom_hline(yintercept = m1, color = '#8e44ad', linetype = 'dashed', size = 1.5)
p <- p + geom_line(data = pl.df, aes(x = axis, y = returns), size = 1.5, color = '#e74c3c') + geom_hline(yintercept = m2, color = '#2980b9', linetype = 'dashed', size = 1.5)
p <- p + geom_line(data = live.df, aes(x = axis, y = returns), size = 1.5, color = '#27ae60') + geom_hline(yintercept = m3, color = '#e67e22', linetype = 'dashed', size = 1.5)
p <- p + ggtitle('Cattle Futures Return vs. S&P 500 (2005 - 2015)', subtitle = "K-Nearest Neighbors Cattle Crush Strategy")
p <- p + scale_x_date(date_breaks = "1 year", labels = date_format('%Y')) + scale_y_continuous()
p <- p + ylab("Returns [.01%]") + xlab("Date (2005 - 2015)")
p