# required libraries to run algorithm

require("tseries")
require("quantmod")
require("caTools")
require("forecast")

library(tseries)
library(quantmod)
library(caTools)
library(forecast)

# MACHINE LEARNING PART OF ALGORITHM

# Takes symbol from temp.csv that saves selected stock on website
df = read.csv("temp.csv", header = FALSE)
N = df[1,]
N # Prints selected stock (check to make sure it is the correct stock
# otherwise it will be a graph of whatever stock N is.)

ind <- getSymbols(Symbols = N, src = "yahoo", from = Sys.Date() - 2000, to = Sys.Date(), auto.assign = FALSE)
ind <- Cl(ind)

# The following commented out code plots historical stock data along with simple
# moving averages, bollinger bands, relative strength index, and moving average
# convergence divergence.

chart_Series(ind, col = "black")
add_SMA(n = 100, on = 1, col = "red")
add_SMA(n = 20, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_BBands(n = 20, maType = "SMA", sd = 1, on = -1)
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)

# Log transformation stock data
indf_log <- log(ind)
#head(indf_log, n = 10)
#plot(indf_log, main = "log indf_data chart")

acf_log <- acf(indf_log, lag.max = 320)
pacf_log <- pacf(indf_log, lag.max = 320)

# difference logged data
indf_diff <- diff(indf_log, lag = 1) # makes models stationary
indf_diff <- na.locf(indf_diff, na.rm = TRUE, fromLast = TRUE) # fills in missing values 
#plot(indf_diff)

adf <- adf.test(indf_log, alternative = c("stationary", "explosive"), k = 0)
#adf

adf_diff <- adf.test(indf_diff, alternative = c("stationary", "explosive"), k = 0)
#adf_diff

diff.acf <- acf(indf_diff)
diff.pacf <- pacf(indf_diff)

# trains data set
train_data <- indf_diff[1:1270]

set.seed(123)
arima_model <- auto.arima(train_data, stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)

#summary(arima_model) # summary for best arima(p,d,q) model to select
#checkresiduals(arima_model)
arima <- arima(train_data, order = c(0, 0, 1))
#summary(arima)
forecast1 <- forecast(arima, h = 100)
#plot(forecast1)
#checkresiduals(arima)
arima <- arima(indf_log[1:1270], order = c(0, 1, 1))
#summary(arima)

forecast_ori <- forecast(arima, h = 100)
a <- ts(indf_log)
forecast_ori %>% autoplot() + autolayer(a) # plots the stock that will lll

# The following code saves the plot of the stock's forecasted value to be shown
# on the website.
png(file ="temp.png", width=600, height=350)
plot(forecast_ori %>% autoplot() + autolayer(a))
dev.off()

# OPTIMIZATION PART OF ALGORITHM

# uses values from a week ago to 
sdate <- Sys.Date() - 8
#sdate
edate <- Sys.Date() - 7
#edate

budget <- 50000 # automatically runs algorithm with a set budget of 50000 to invest

# list of all stocks that user's can purchase from our website
Symbols <- c("WMT","AMZN","AAPL","CVS","UNH","XOM","GOOG","GOOGL","MCK","ABC","COST","CI","T","MSFT","CAH","CVX","HD","WBA","MPC","ELV","KR","F","VZ","JPM","GM","CNC","META","CMCSA","PSX","VLO","DELL","TGT","UPS","LOW","BAC","JNJ","ADM","FDX","HUM","WFC","PFE","C","PEP","INTC","PG","GE","IBM","MET");

# gets the prices for each playable stock from a week ago
prices_data <- lapply(Symbols,function(x) Ad(getSymbols(x, from=sdate, to=edate, periodicity = "daily",auto.assign=FALSE)))
prices <- sort(round(as.data.frame(do.call(merge,prices_data)),0),TRUE)

colnames(prices) <- gsub(".Adjusted","",colnames(prices)) # gets adjusted stock data

scales <- rep(0, length(prices)) #initialize vector of stock quantities as 0
risk <- pnorm(as.numeric(prices[1,]), mean(as.numeric(prices[1,])), sd(as.numeric(prices[1,])))
value_to_risk <- sort(prices / risk, TRUE)
prices_old_sorted <- prices[, colnames(value_to_risk)]

# Risk Ratio
for(i in 1:length(prices_old_sorted)){
  while(budget-sum(prices_old_sorted*scales)>=prices_old_sorted[i]){
    scales[i]=scales[i]+1
  }
}

df_scales <- as.data.frame(t(scales))
colnames(df_scales) <- colnames(prices_old_sorted)
spent <- sum(df_scales * prices_old_sorted) # how much of budget was spent
remaining <- budget - sum(df_scales * prices_old_sorted) # how much of budget is leftover

yesterday <- Sys.Date() - 1
today <- Sys.Date()

# gets the stock prices for today
prices_today <- lapply(Symbols,function(x) Ad(getSymbols(x, from=yesterday, to=today, periodicity = "daily", auto.assign=FALSE)))
prices_today <- sort(round(as.data.frame(do.call(merge,prices_today)),0),TRUE)
colnames(prices_today) <- gsub(".Adjusted","",colnames(prices_today))
prices_today_sorted <- prices_today[, colnames(value_to_risk)]

# difference in stock prices over the past week
price_change <- prices_today_sorted - prices_old_sorted
portfolio_change <- sum(scales * price_change)

# returns
budget # budget
spent # $ spent ( of budget)
remaining # leftover $ from budget
df_scales # Amount of each stock chosen
prices_old_sorted # price a week ago
prices_today_sorted # today's stock value
portfolio_change # net gain/loss over the past week

