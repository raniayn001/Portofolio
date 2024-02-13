training <- readxl::read_excel("Train.xlsx")
testing <- readxl::read_excel("Test.xlsx")
dim(testing)

#####Preprocessing data####
sum(is.na(training))
sum(is.na(testing))
str(training)
str(testing)
plot(training, main = "Time Series Data", type = "l")
library(tseries)
result_adf <- adf.test(training$Data)
p_value_adf <- result_adf$p.value
cat("Augmented Dickey-Fuller Test p-value:", p_value_adf, "\n")
if (p_value_adf < 0.05) {
  cat("Tolak H0.\n")
} else {
  cat("Gagal Tolak H0.\n")
}

library(forecast)

####ARIMA####
set.seed(123)
arima_model <- auto.arima(training$Data)
summary(arima_model)

#Prediksi training data
pred_arimatr <- predict(arima_model, n.ahead = length(training$Data))
mape <- 100 * mean(abs((training$Data - pred_arimatr$pred) / training$Data[training$Data != 0]))
mape <- mean(abs((training$Data - pred_arimatr$pred) / training$Data[training$Data != 0]) * 100)
smape_value <- 100 * 200 * mean(
  abs(training$Data[(n_steps + 1):length(training$Data)] - mean(pred_arimatr$pred)) /
    (abs(training$Data[(n_steps + 1):length(training$Data)]) + abs(mean(pred_arimatr$pred))),
  na.rm = TRUE
)
cat("MAPE for training data:", mape, "%\n")
cat("SMAPE for training data:", smape_value, "%\n")

#prediksi testing data
pred_arimatest <- predict(arima_model,n.ahead = length(testing$Data))
mape_value <- mean(abs((testing$Data - pred_arima$mean) / testing$Data)) * 100
smape_value <- 200 * mean(
  abs(testing$Data[(n_steps + 1):length(testing$Data)] - pred_arimatest$pred) /
    (abs(testing$Data[(n_steps + 1):length(testing$Data)]) + abs(pred_arimatest$pred)),
  na.rm = TRUE
)
cat("MAPE:", mape_value, "%\n")
cat("sMAPE:", smape_value, "%\n")

#plot(pred_arima, main = "ARIMA Forecast")
plot.ts(c(training$Data, rep(NA, length(testing$Data))), col = "blue", ylab = "Value", main = "ARIMA Forecasting")
lines(ts(c(rep(NA, length(training$Data)), testing$Data), frequency = frequency(testing$Data)), col = "red")
lines(pred_arima$mean, col = "dark green", lty = 2)
legend("topright", legend = c("Training", "Testing", "Forecast"), col = c("blue", "red", "dark green"), lty = c(1, 1, 2))


####Exponential Smoothing State Space Models (ETS)####
training$lagged_value <- c(NA, head(training$Data, -1))
training$lagged_value2 <- c(NA, NA, head(training$Data, -2))
testing$lagged_value <- c(NA, head(testing$Data, -1))
testing$lagged_value2 <- c(NA, NA, head(testing$Data, -2))
training <- na.omit(training)
model_ets <- ets(training$Data)
testing <- na.omit(testing)

pred_ets <- forecast(model_ets, h = nrow(training))
mape_value <- mean(abs((training$Data-pred_ets$mean) / training$Data[training$Data != 0])) * 100
smape_value <- smape(training$Data, pred_ets$mean)*100
cat("MAPE:", mape_value, "%\n")
cat("sMAPE:", smape_value, "%\n")


pred_ets <- forecast(model_ets, h = nrow(testing))
mape_value <- mape(testing$Data, pred_ets$mean)*100
smape_value <- smape(testing$Data, pred_ets$mean)*100
cat("MAPE:", mape_value, "%\n")
cat("sMAPE:", smape_value, "%\n")

plot(training$Data, type = "l", x = training$Waktu, main = "ETS Forecast", xlab = "Time", ylab = "Value")
lines(testing$Data, col = "blue", lwd = 2, x = testing$Waktu)  # Original time series in blue
lines(pred_ets$mean, col = "red", lwd = 2, x = testing$Waktu)  # Predicted values in red
lines(testing$Waktu, pred_ets$lower[,1], col = "dark green", lty = 2)  # Lower forecast interval in green
lines(testing$Waktu, pred_ets$upper[,2], col = "dark green", lty = 2)  # Upper forecast interval in green
legend("topright", legend = c("Actual", "Predicted", "Forecast Interval"), col = c("blue", "red", "green"), lty = 1:2, lwd = 2)
plot(pred_ets, main = "ETS Forecast", xlab = "Time", ylab = "Value", start = start(testing$Data), frequency = frequency(testing$Data))


####Long Short Term Memory####
#di python

####Random Forest####
library(randomForest)
library(caret)
library(Metrics)
rf_model <- randomForest(Data ~ ., data = training)
#Prediksi Training
rf_predictions <- predict(rf_model, newdata = training)
mape_value <- mean(abs((training$Data - rf_predictions) / training$Data[training$Data != 0])) * 100
smape_value <- smape(training$Data, rf_predictions)
cat("MAPE:", mape_value, "%\n")
cat("sMAPE:", smape_value, "\n")
#Prediksi Testing
rf_predictions <- predict(rf_model, newdata = testing)
mape_value <- mean(abs((testing$Data - rf_predictions) / testing$Data)) * 100
smape_value <- smape(testing$Data, rf_predictions)
cat("MAPE:", mape_value, "%\n")
cat("sMAPE:", smape_value, "\n")

####TS-SVR####
library(e1071)
library(xts)
lag_orders <- 1:5
lagged_data <- cbind(training, sapply(lag_orders, function(lag) lag(training$Data, lag)))
lagged_data <- na.omit(lagged_data)

param_grid <- expand.grid(
  kernel = c("linear", "radial", "polynomial"),
  cost = c(0.1, 1, 10)
)
tuned_model <- tune(
  svm,
  Data ~ .,
  data = lagged_data,
  ranges = param_grid,
  tunecontrol = tune.control(sampling = "cross")
)
svr_model <- tuned_model$best.model
pred_svrtr <- predict(svr_model, newdata = lagged_data)
mape_value <- mean(ifelse(lagged_data$Data != 0, abs(lagged_data$Data - pred_svrtr) / abs(lagged_data$Data), 0)) * 100
smape <- function(actual, forecast) {
  200 * mean(ifelse(actual != 0, abs(actual - forecast) / (abs(actual) + abs(forecast)), 0))}

smape_value <- smape(lagged_data$Data, pred_svrtr)
cat("MAPE for training set:", mape_value, "%\n")
cat("sMAPE for training set:", smape_value, "%\n")

lagged_data <- cbind(testing, sapply(lag_orders, function(lag) lag(testing$Data, lag)))
lagged_data <- na.omit(lagged_data)
pred_svrtr <- predict(svr_model, newdata = lagged_data)
mape_value <- mean(ifelse(lagged_data$Data != 0, abs(lagged_data$Data - pred_svrtr) / abs(lagged_data$Data), 0)) * 100
smape <- function(actual, forecast) {
  200 * mean(ifelse(actual != 0, abs(actual - forecast) / (abs(actual) + abs(forecast)), 0))
}
smape_value <- smape(lagged_data$Data, pred_svrtr)
cat("MAPE for testing set:", mape_value, "%\n")
cat("sMAPE for testing set:", smape_value, "%\n")

####XGBoost ####
library(xgboost)
train_matrix <- xgb.DMatrix(as.matrix(training$Data), label = as.matrix(training$Data))
test_matrix <- xgb.DMatrix(as.matrix(testing$Data))
params <- list(
  objective = "reg:squarederror",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8
)
xgboost_model <- xgboost(params, data = train_matrix, nrounds = 100)
xgboost_pred <- predict(xgboost_model, newdata = test_matrix)
plot(xgboost_pred, main = "XG Boost Forecast", type = "l")

plot(testing, col = "blue", type = "l", lwd = 2, ylim = range(c(testing$Data, xgboost_pred)))
lines(xgboost_pred, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "XGBoost Forecast"), col = c("blue", "red"), lty = 1, cex = 0.8)

#Prediksi data training
xboost_tr <- predict(xgboost_model, newdata = train_matrix)
mape_value <- mean(ifelse(training$Data != 0, abs(training$Data - xboost_tr) / abs(training$Data), 0)) * 100
smape <- function(actual, forecast) {
  200 * mean(ifelse(actual != 0, abs(actual - forecast) / (abs(actual) + abs(forecast)), 0))
}
smape_value <- smape(training$Data, xboost_tr)
cat("MAPE for test set:", mape_value, "%\n")
cat("SMAPE for test set :", smape_value, "%\n")

# Prediksi data testing
xboost_test <- predict(xgboost_model, newdata = test_matrix)
mape_value <- mean(ifelse(testing$Data != 0, abs(testing$Data - xboost_tr) / abs(testing$Data), 0)) * 100
smape <- function(actual, forecast) {
  200 * mean(ifelse(actual != 0, abs(actual - forecast) / (abs(actual) + abs(forecast)), 0))
}
smape_value <- smape(testing$Data, xboost_tr)
cat("MAPE for test set:", mape_value, "%\n")
cat("SMAPE for test set :", smape_value, "%\n")

plot(training$Data, type = "l", x = training$Waktu, main = "XGBoost Forecast", xlab = "Time", ylab = "Value")
lines(testing$Data, col = "blue", lwd = 2, x = testing$Waktu)  # Original time series in blue
lines(xgboost_pred, col = "red", lwd = 1, x = testing$Waktu)  # Predicted values in red
legend("topright", legend = c("training", "tesing", "Forecast Testing"), col = c("black", "blue", "red"), lty = 1:2, lwd = 2)
