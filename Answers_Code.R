rm(list=ls())
gc()

# Libraries
library(data.table)
library(lubridate)
library(ggplot2)
library(glmnet) # for lasso regressions
library(ranger)
library(caret) # for PLS regressions

# Setting time equal to English
Sys.setlocale("LC_TIME","English")

# Importing raw data
data <- setDT(readRDS("C:/2021_Electricity_data.RDS"))

# Question 1.1 --------------------------------------------------------------
# Calculating average hourly prices for all days
temp <- data
temp[, hour := hour(date)]
temp[, weekday := weekdays(date)]
temp[, dutch_avg := mean(dutch_power, na.rm = TRUE), by = hour]
dutch_avg <- unique(temp$dutch_avg)

# Calculating average hourly prices in weekends
temp <- temp[weekday == "Saturday" | weekday == "Sunday"]
temp[, dutch_avg_wday := mean(dutch_power, na.rm = TRUE), by = hour]
dutch_avg_wday <- unique(temp$dutch_avg_wday)

# Making 1 dataframe
temp <- data.frame(hour = c(0:23), dutch_avg, dutch_avg_wday)

# Plotting values
ggplot() + 
  geom_line(data=temp, aes(x=hour, y=dutch_avg, color = "blue")) +
  geom_line(data=temp, aes(x=hour, y=dutch_avg_wday, color = "red")) +
  scale_color_manual("", labels = c("All days", "Weekends"), values = c("blue", "red")) +
  labs(title="Average Hourly Prices",
       x ="Hour", y = "Price") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  )

rm(list = c("temp", "dutch_avg", "dutch_avg_wday")) # cleans up the dataframes


# Question 1.2 --------------------------------------------------------------
# Calculating volatility per hour
temp <- data
temp[, hour := hour(date)]
temp[, hourly_vol := sd(dutch_power, na.rm = TRUE), by = hour]

# Plotting hourly volatility
ggplot(temp, aes(x=hour)) + 
  geom_line(aes(y = hourly_vol), color = "blue") + 
  labs(title="Hourly Volatility",
       x ="Hour", y = "Standard Deviation") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  )

rm(temp) # clean up


# Question 1.3 --------------------------------------------------------------
# Calculating daily mean
temp <- data
temp[, hour := hour(date)]
temp[, day := yday(date)]
temp[, daily_avg := mean(dutch_power, na.rm = TRUE), by = day]

# Plotting average daily prices
ggplot(temp, aes(x=day)) + 
  geom_line(aes(y = daily_avg), color = "blue") + 
  labs(title="Average Daily Prices",
       x ="Day", y = "Price") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  )

rm(temp) # clean up


# Question 1.4 --------------------------------------------------------------
# Calculating average hourly prices between April and September
month_vector <- c(4:9)
temp <- data
temp[, hour := hour(date)]
temp[, month := month(date)]
temp_apr_sep <- temp[month %in% month_vector]
temp_apr_sep[, avg_solar_aprsep := mean(solar, na.rm = TRUE), by = hour]

# Calculating average hourly prices between October and March
month_vector <- c(10:12, 1:3)
temp_oct_mar <- temp[month %in% month_vector]
temp_oct_mar[, avg_solar_octmar := mean(solar, na.rm = TRUE), by = hour]

# Plotting average hourly prices for Apr-Sep and Oct-Mar, respectively
ggplot() + 
  geom_line(data=temp_apr_sep, aes(x=hour, y=avg_solar_aprsep, color='blue')) + 
  geom_line(data=temp_oct_mar, aes(x=hour, y=avg_solar_octmar, color='red')) +
  scale_color_manual("", labels = c("Apr-Sep", "Oct-Mar"), values = c("blue", "red")) +
  labs(title="Average Hourly Solar Generation",
       x ="Hour", y = "Solar Generation") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  )

rm(list = c("temp", "temp_apr_sep", "temp_oct_mar", "month_vector")) # clean up


# Question 1.5 ------------------------------------------------------------
# Calculates daily average solar generation + off-shore wind generation
temp <- data
temp[, hour := hour(date)]
temp[, day := yday(date)]
temp[, ':=' (
  daily_avg_solar = mean(solar, na.rm = TRUE), 
  daily_avg_offshore_wind = mean(wind_off_shore, na.rm = TRUE)
  ), 
  by = day]

# Plotting average hourly prices for Apr-Sep and Oct-Mar, respectively
ggplot() + 
  geom_smooth(data=temp, aes(x=day, y=daily_avg_solar, color='blue'), se = FALSE) + 
  geom_smooth(data=temp, aes(x=day, y=daily_avg_offshore_wind, color='red'), se = FALSE) +
  scale_color_manual("", labels = c("Solar", "Off-shore Wind"), values = c("blue", "red")) +
  labs(title="Average Daily Solar and Off-shore Wind Generation",
       x ="Day", y = "Generation") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  )

rm(temp) # clean up


# Question 2.1 ------------------------------------------------------------
# Importing raw data (again)
data <- setDT(readRDS("C:/2021_Electricity_data.RDS"))

# Computes hours which I will use in for loop
temp <- data
temp[, hour := hour(date)]

# For loop that makes a vector per hour for each of the 9 variables, which I ultimately turn into 1 dataframe
df <- setDT(data.frame(date = unique(as.Date(temp$date, format = "%Y-%m-%d %H:%M:%S")))) # empty dataframe with correct dates
empty_list <- list() # list in which I will store vectors
new_colnames <- list() # empty list to store column names
colnames <- names(temp)[2:(ncol(temp)-1)] # column names to iterate over

for (i in 1:length(colnames)) {
  for (j in unique(temp$hour)) {
    empty_list[[j+1]] <- as.vector(temp[hour == j, list(get(colnames[i]))]) # generates vector per hour per variable
    df <- cbind(df, empty_list[[j+1]]) # adds vector to dataframe
  }
  new_colnames[[i]] <- setDT(data.frame(paste0(rep(colnames[i], each=1), "_", 1:24))) # generates the right column names
}
new_colnames <- rbindlist(new_colnames) # binds all the column names in 1 column
names(df)[-1] <- new_colnames[[1]] # [[1]] changes single column data.table to vector (i.e. replaces V1 by dutch_power_1 etc.)

rm(list = c("empty_list", "new_colnames", "temp", "colnames")) # clean up


# Question 2.2 ------------------------------------------------------------
# Creating lagged feature dataframe
features <- df[,26:ncol(df)] # takes 
features <- lapply(features, function(x) shift(x, n=1L))
features <- setDT(as.data.frame(do.call(cbind, features)))
features <- features[-1,]

# Creating corresponding labels dataframe
labels <- df[-1,1:25] # drops first row since we don't have any features for that date (because we lagged the features by 1 day)


# Question 2.3 ------------------------------------------------------------
# Counting 0 values per feature
zero_df <- vector()
for (i in 1:ncol(features)) {
  zero_df[i] <- nrow(features[get(names(features)[i]) == 0.00])
}
zero_df <- setDT(data.frame(feature = names(features), zero_values = (zero_df / nrow(features) * 100))) # computes % of values equal to 0 per variable 
zero_df <- zero_df[zero_values > 20] # highlights the variables with more than 20% missing values

# Plotting values that are equal to 0
ggplot(zero_df, aes(x=feature, y=zero_values)) + 
  geom_bar(stat = "identity", fill = "red") +
  labs(title="Features with Values Equal to 0",
       x ="Feature", y = "Zero Values (in %)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black')
  )

# Removing variables shown in graph from feature set
zero_df <- zero_df$feature # creates vector with names of columns we want to remove
features <- features[ , !(colnames(features) %in% zero_df), with=FALSE] # filters out columns based on vector that contains names of columns we want to remove


# Question 3.1 ------------------------------------------------------------
# I first create a new dataframe for this question (note that we will use 1 of the labels as an input for this question, which have not been lagged)
temp <- labels[, list(date, dutch_power_1, dutch_power_24)]
temp[, dutch_power_24 := shift(dutch_power_24, n=1L)]
temp <- temp[-1,]
train <- temp[date < "2020-07-01"][, date := NULL]
test <- temp[date >= "2020-07-01"][, date := NULL]

# Benchmark model with OLS (I)
model <- lm(dutch_power_1 ~ dutch_power_24, data = train) 
print(paste0("In-sample R-squared is: ", round(summary(model)$r.squared, 2)))

# Out-of-sample prediction with OLS model
predictions <- predict(model, newdata = test[,2], type = "response") # takes dutch_power_24 as input and predicts dutch_power_1

# Calculating RMSE
print(paste0("Out-of-sample RMSE is: ", round(sqrt(mean((test$dutch_power_1 - predictions)^2)), 2)))

rm(list = c("temp", "train", "test", "predictions", "model"))


# Question 3.2 ------------------------------------------------------------
# Function to calculate R-squared
rsq <- function(preds, actual) {
  rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
  tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
}

# Creating 1 large features dataframe with the right lags
lagged_features <- df[,1:25] # grabs dutch_power_1 to dutch_power_24 which I will add to features we created before (and remove dutch_power_1)
lagged_features <- lagged_features[, -c("dutch_power_1")] # removes dutch_power_1 as it won't be a feature
lagged_features <- lagged_features[, (colnames(lagged_features)[-1]) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = colnames(lagged_features)[-1]][-1,]  # lags all vars (except date) by 1 day and drops first row
features <- cbind(lagged_features, features) # binds features list and dutch_power_2 to dutch_power_24 
features[, (colnames(features)[-1]) := lapply(.SD, function(x) scale(x)), .SDcols = colnames(features)[-1]] # scales all features (except date)
rm(lagged_features) # clean up

# Creating training, validation and test set with features
train_x <- features[date < "2020-01-01"][,-1]
val_x <- features[date >= "2020-01-01" & date < "2020-07-01"][,-1]
test_x <- features[date >= "2020-07-01"][,-1]

# Creating training, validation and test set for labels
train_y <- labels[date < "2020-01-01", list(dutch_power_1)]
val_y <- labels[date >= "2020-01-01" & date < "2020-07-01", list(dutch_power_1)]
test_y <- labels[date >= "2020-07-01", list(dutch_power_1)]

# Lasso regression 
# Grid + empty vector to store RMSE in
lasso_grid <- 10^seq(10, -2, length = 100)
rmse_lasso <- vector()

# Training model 
for (i in 1:length(lasso_grid)) {
  lasso_model <- glmnet(train_x, train_y$dutch_power_1, alpha = 1, lambda = lasso_grid[i])
  predictions <- predict(lasso_model, newx = as.matrix(val_x), s = lasso_grid[i])
  rmse_lasso[i] <- sqrt(mean((val_y$dutch_power_1 - predictions)^2))
  message(paste0("Iteration ", i, " of ", length(lasso_grid), " complete."))
  gc() # clears memory
}

optimal_lambda_lasso <- lasso_grid[which.min(rmse_lasso)] # optimal lambda based on validation set

# Out-of-sample predictions 
lasso_model <- glmnet(train_x, train_y$dutch_power_1, alpha = 1, lambda = optimal_lambda_lasso)
predictions <- predict(lasso_model, newx = as.matrix(test_x), type = "response", s = optimal_lambda_lasso)
rsq_lasso <- rsq(predict(lasso_model, newx = as.matrix(train_x), type = "response", s = optimal_lambda_lasso), train_y$dutch_power_1) # in-sample R-squared
rmse_lasso <- sqrt(mean((test_y$dutch_power_1 - predictions)^2)) # out-of-sample RMSE

rm(list = c("lasso_grid", "predictions", "lasso_model")) # clean up

# Partial least squares regression
# creating grid for ncomp, for testing 20 ncomps on the validation set
ncomp_grid <- c(1:20)
rmse_pls <- vector()

# Training and validating model
for (i in 1:length(ncomp_grid)){
  pls_model <- caret::train(
    dutch_power_1 ~ . ,
    data = cbind(train_y, train_x),
    method = "pls",
    tuneGrid = expand.grid(ncomp = ncomp_grid[i]),
    trControl = trainControl(method = "none"))
  
  predictions <- predict(pls_model, ncomp = ncomp_grid[i], newdata = val_x)
  rmse_pls[i] <- sqrt(mean((val_y$dutch_power_1 - predictions)^2))
  
  message(paste0("Iteration ", i, " of ", length(ncomp_grid), " complete."))
  gc()
}

optimal_ncomp_pls <- ncomp_grid[which.min(rmse_pls)] # optimal number of ncomps

# Out-of-sample predictions 
pls_model <- caret::train(dutch_power_1 ~ . ,
                          data = cbind(train_y, train_x),
                          method = "pls",
                          tuneGrid = expand.grid(ncomp = optimal_ncomp_pls),
                          trControl = trainControl(method = "none"))
predictions <- predict(pls_model, ncomp = optimal_ncomp_pls, newdata = test_x)
rsq_pls <- rsq(predict(pls_model, ncomp = optimal_ncomp_pls, newdata = train_x), train_y$dutch_power_1) # in-sample r-squared
rmse_pls <- sqrt(mean((test_y$dutch_power_1 - predictions)^2)) # out-of-sample RMSE

# Random forest
# Creating grid + vector to store RMSE of models when predicting on validation set
random_forest_grid <- expand.grid(
  mtry = c(ncol(train_x) / 3), 
  num.trees = c(1000), 
  max.depth = seq(1, 20)
)
rmse_rf <- vector()

# Training and validating model
for (i in 1:nrow(random_forest_grid)) {
  rf_model <- ranger(dutch_power_1 ~ . , 
                     data = cbind(train_y, train_x),
                     mtry = random_forest_grid$mtry[i],
                     num.trees = random_forest_grid$num.trees[i],
                     max.depth = random_forest_grid$max.depth[i],
                     verbose = TRUE)
  
  predictions <- predict(rf_model, data = val_x)
  rmse_rf[i] <- sqrt(mean((val_y$dutch_power_1 - as.vector(predictions$predictions))^2))
  message(paste0("Iteration ", i, " of ", nrow(random_forest_grid)))
}

optimal_mtry_rf <- random_forest_grid$mtry[which.min(rmse_rf)]
optimal_ntrees_rf <- random_forest_grid$num.trees[which.min(rmse_rf)]
optimal_depth_rf <- random_forest_grid$max.depth[which.min(rmse_rf)]

# Out of sample predictions
rf_model <- ranger(dutch_power_1 ~ . , 
                   data = cbind(train_y, train_x),
                   mtry = optimal_mtry_rf,
                   num.trees = optimal_ntrees_rf,
                   max.depth = optimal_depth_rf,
                   verbose = TRUE)

predictions <- predict(rf_model, data = test_x)
rsq_rf <- rsq(predict(rf_model, data = train_x)$predictions, train_y$dutch_power_1) # in-sample r-squared
rmse_rf <- sqrt(mean((test_y$dutch_power_1 - as.vector(predictions$predictions))^2))


# Question 4.1 ------------------------------------------------------------
# Makes training set (note that because I am performing a simple linear regression
# I cannot tune hyperparameters and hence I include the validation period (2020-01-01 
# to 2020-06-30) in the training set )
labels <- df[,1:25]

# Hardcoding column names
colnames <- colnames(labels)[-1]
predictions_ols <- list()
r2_ols <- vector()
rmse_ols <- vector()

for (i in 1:length(colnames)) {
  # Creating label and features set
  train_y <- as.data.frame(labels[[i+1]]) # grabs response variable and removes first element
  train_y <- setDT(cbind(date = labels$date, train_y))
  train_y <- train_y[date < "2020-07-01"][-1,-1]
  names(train_y) <- colnames[i]
  # Checks whether y is dutch_power_1 so we pick the price of dutch_power_24 as x rather than n-1 (i.e. dutch_power_2 uses dutch_power_1)
  if (colnames(train_y) == "dutch_power_1") {
    cols <- tail(colnames, 1)
    features <- labels[, ..cols]
    features <- setDT(as.data.frame(scale(features)))
    features <- features[, (colnames(features)) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = colnames(features)][-1]
    train_x <- setDT(cbind(date = labels$date[-1], features))
    train_x <- train_x[date < "2020-07-01"][,-1]
  } else {
    cols <- colnames[(i-1)]
    features <- labels[, ..cols]
    features <- setDT(as.data.frame(scale(features)))
    features <- features[, (colnames(features)) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = colnames(features)][-1]
    train_x <- setDT(cbind(date = labels$date[-1], features))# lags feature set and removes first row
    train_x <- train_x[date < "2020-07-01"][,-1]
  }
  
  # Performing linear regression
  data <- cbind(train_y, train_x)
  names(data) <- c("y", "x") # changes column names to y and x for easy referencing
  model <- lm(y ~ x, data = data)
  
  # In-sample R-squared 
  r2_ols[i] <- round(summary(model)$r.squared, 2) # stores in-sample rsquared in vector
  
  # Out-of-sample RMSE
  # Creating response variable of test set
  test_y <- as.data.frame(labels[[i+1]]) # grabs response variable 
  test_y <- setDT(cbind(date = labels$date, test_y))
  test_y <- test_y[date >= "2020-07-01"][,-1]
  names(test_y) <- colnames[i]
  # Checks whether y is dutch_power_1 
  if (colnames(train_y) == "dutch_power_1") {
    cols <- tail(colnames, 1)
    features <- labels[, ..cols]
    features <- setDT(as.data.frame(scale(features)))
    features <- features[, (colnames(features)) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = colnames(features)][-1]
    test_x <- setDT(cbind(date = labels$date[-1], features))
    test_x <- test_x[date >= "2020-07-01"][,-1]
  } else {
    features <- labels[, ..cols]
    features <- setDT(as.data.frame(scale(features)))
    features <- features[, (colnames(features)) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = colnames(features)][-1]
    test_x <- setDT(cbind(date = labels$date[-1], features))# lags feature set and removes first row
    test_x <- test_x[date >= "2020-07-01"][,-1]
  }
  
  # Out-of-sample RMSE
  setnames(test_y, colnames(test_y), c("y"))
  setnames(test_x, colnames(test_x), c("x"))
  predictions <- predict(model, newdata = test_x, type = "response")
  predictions_ols[[i]] <- predictions
  rmse_ols[i] <- round(sqrt(mean((test_y$y - predict(model, newdata = test_x, type = "response"))^2)), 2)  
  
  gc() # cleans garbage
  message(paste0("Iteration ", i, " of ", length(colnames)))
}

# Plots in-sample Rsquared
plot(r2_ols,
     type = "l",
     main = "In-Sample R-squared (OLS)",
     xlab = "Hour",
     ylab = "R-squared")

# Plots OOS R-squared
plot(rmse_ols, 
     type = "l",
     main = "Out-of-Sample RMSE (OLS)",
     xlab = "Hour",
     ylab = "RMSE")


# Question 4.2 ------------------------------------------------------------
# Lasso
lasso_grid <- 10^seq(10, -2, length = 100) # hyperparameter grid
predictions_lasso <- list() # an empty list to store the LASSO predictions of each hour
colnames <- names(df)[2:25] # saves names of all 24 dependent variables to loop over (excludes the date, of course)
date <- df$date # dates that I cbind when I need to filter by date
rmse_val <- vector() # stores RMSE of validation set in order to determine optimal lambda
rmse_lasso <- vector() # stores out-of-sample RMSE
r2_lasso <- vector() # stores in-sample R2

for (i in 1:length(colnames)) {
  # Features
  features <- df[, .SD, .SDcols = !colnames[i]] # deselects y variable, keeps rest of the features
  features[, (colnames(features)[-1]) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = (colnames(features)[-1])]
  features <- features[, (colnames(features)[-1]) := lapply(.SD, function(x) scale(x)), .SDcols = (colnames(features)[-1])][-1,] # drops first row (NA after lagging variables)
  features <- features[, !(colnames(features) %in% zero_df), with=FALSE] # drops features that should be removed (see question 2)
  train_x <- features[date < "2020-01-01"][,-1]
  val_x <- features[date >= "2020-01-01" & date < "2020-07-01"][,-1]
  test_x <- features[date >= "2020-07-01"][,-1]
  
  # Label
  label <- df[, .SD, .SDcols = colnames[i]]
  label <- setDT(cbind(date = date, label))[-1,]
  train_y <- label[date < "2020-01-01"][,-1]
  val_y <- label[date >= "2020-01-01" & date < "2020-07-01"][,-1]
  test_y <- label[date >= "2020-07-01"][,-1]
  
  # Lasso regression
  for (j in 1:length(lasso_grid)) {
    lasso_model <- glmnet(train_x, train_y[[1]], alpha = 1, lambda = lasso_grid[j])
    predictions <- predict(lasso_model, newx = as.matrix(val_x), s = lasso_grid[j])
    rmse_val[j] <- sqrt(mean((val_y[[1]] - predictions)^2))
    gc() # clears memory
  }
  
  optimal_lambda_lasso <- lasso_grid[which.min(rmse_val)] # optimal lambda
  
  lasso_model <- glmnet(train_x, train_y[[1]], alpha = 1, lambda = optimal_lambda_lasso) # trains model with optimal parameters
  predictions <- predict(lasso_model, newx = as.matrix(test_x), type = "response", s = optimal_lambda_lasso)
  predictions_lasso[[i]] <- predictions # stores predictions in list
  r2_lasso[i] <- rsq(as.vector(predict(lasso_model, newx = as.matrix(train_x), type = "response", s = optimal_lambda_lasso)), train_y[[1]]) # in-sample R-squared
  rmse_lasso[i] <- sqrt(mean((test_y[[1]] - predictions)^2)) # out-of-sample RMSE
  
  message(paste0("Iteration ", i, " of ", length(colnames), " complete."))
}

# Plots in-sample Rsquared
plot(r2_lasso,
     type = "l",
     col = "blue",
     main = "In-Sample R-squared (LASSO)",
     xlab = "Hour",
     ylab = "R-squared")

# Plots OOS R-squared
plot(rmse_lasso, 
     type = "l",
     col = "blue",
     main = "Out-of-Sample RMSE (LASSO)",
     xlab = "Hour",
     ylab = "RMSE")


# Partial least squares regression
ncomp_grid <- c(1:20)
predictions_pls <- list()
colnames <- names(df)[2:25]
date <- df$date
rmse_val <- vector()
rmse_pls <- vector()
r2_pls <- vector()

for (i in 1:length(colnames)) {
  # Features
  features <- df[, .SD, .SDcols = !colnames[i]] # deselects y variable, keeps rest of the features
  features[, (colnames(features)[-1]) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = (colnames(features)[-1])]
  features <- features[, (colnames(features)[-1]) := lapply(.SD, function(x) scale(x)), .SDcols = (colnames(features)[-1])][-1,] # drops first row (NA after lagging variables)
  features <- features[, !(colnames(features) %in% zero_df), with=FALSE] # drops features that should be removed (see question 2)
  train_x <- features[date < "2020-01-01"][,-1]
  val_x <- features[date >= "2020-01-01" & date < "2020-07-01"][,-1]
  test_x <- features[date >= "2020-07-01"][,-1]
  
  # Label
  label <- df[, .SD, .SDcols = colnames[i]]
  label <- setDT(cbind(date = date, label))[-1,]
  train_y <- label[date < "2020-01-01"][,-1]
  setnames(train_y, colnames(train_y), c("y"))
  val_y <- label[date >= "2020-01-01" & date < "2020-07-01"][,-1]
  setnames(val_y, colnames(val_y), c("y"))
  test_y <- label[date >= "2020-07-01"][,-1]
  setnames(test_y, colnames(test_y), c("y"))
  
  # PLS regression (training + validation)
  for (j in 1:length(ncomp_grid)){
    pls_model <- caret::train(
      y ~ . ,
      data = cbind(train_y, train_x),
      method = "pls",
      tuneGrid = expand.grid(ncomp = ncomp_grid[j]),
      trControl = trainControl(method = "none"))
    
    predictions <- predict(pls_model, ncomp = ncomp_grid[j], newdata = val_x)
    rmse_val[j] <- sqrt(mean((val_y$y - predictions)^2))
  }
  
  optimal_ncomp_pls <- ncomp_grid[which.min(rmse_val)] # optimal number of ncomps
  
  # Out-of-sample predictions
  pls_model <- caret::train(
    y ~ . ,
    data = cbind(train_y, train_x),
    method = "pls",
    tuneGrid = expand.grid(ncomp = optimal_ncomp_pls),
    trControl = trainControl(method = "none"))
  
  predictions <- predict(pls_model, ncomp = optimal_ncomp_pls, newdata = test_x)
  predictions_pls[[i]] <- predictions # stores predictions in list
  rmse_pls[i] <- sqrt(mean((test_y$y - predictions)^2)) # out-of-sample RMSE
  r2_pls[i] <- rsq(predict(pls_model, ncomp = optimal_ncomp_pls, newdata = train_x), train_y$y) # in-sample r-squared
  
  message(paste0("Iteration ", i, " of ", length(colnames), " complete."))
}

# Plots in-sample Rsquared
plot(r2_pls,
     type = "l",
     col = "red",
     main = "In-Sample R-squared (PLS)",
     xlab = "Hour",
     ylab = "R-squared")

# Plots OOS R-squared
plot(rmse_pls, 
     type = "l",
     col = "red",
     main = "Out-of-Sample RMSE (PLS)",
     xlab = "Hour",
     ylab = "RMSE")


# Random Forest
# Creating grid + vector to store RMSE of models when predicting on validation set
random_forest_grid <- expand.grid(
  mtry = c(201 / 3), 
  num.trees = c(1000), 
  max.depth = seq(1, 20)
)
predictions_rf <- list()
colnames <- names(df)[2:25]
date <- df$date
rmse_val <- vector()
rmse_rf <- vector()
r2_rf <- vector()

for (i in 1:length(colnames)) {
  # Features
  features <- df[, .SD, .SDcols = !colnames[i]] # deselects y variable, keeps rest of the features
  features[, (colnames(features)[-1]) := lapply(.SD, function(x) shift(x, n=1L)), .SDcols = (colnames(features)[-1])]
  features <- features[, (colnames(features)[-1]) := lapply(.SD, function(x) scale(x)), .SDcols = (colnames(features)[-1])][-1,] # drops first row (NA after lagging variables)
  features <- features[, !(colnames(features) %in% zero_df), with=FALSE] # drops features that should be removed (see question 2)
  train_x <- features[date < "2020-01-01"][,-1]
  val_x <- features[date >= "2020-01-01" & date < "2020-07-01"][,-1]
  test_x <- features[date >= "2020-07-01"][,-1]
  
  # Label
  label <- df[, .SD, .SDcols = colnames[i]]
  label <- setDT(cbind(date = date, label))[-1,]
  train_y <- label[date < "2020-01-01"][,-1]
  setnames(train_y, colnames(train_y), c("y"))
  val_y <- label[date >= "2020-01-01" & date < "2020-07-01"][,-1]
  setnames(val_y, colnames(val_y), c("y"))
  test_y <- label[date >= "2020-07-01"][,-1]
  setnames(test_y, colnames(test_y), c("y"))
  
  # Random forest (training + validation)
  for (j in 1:nrow(random_forest_grid)) {
    rf_model <- ranger(y ~ . , 
                       data = cbind(train_y, train_x),
                       mtry = random_forest_grid$mtry[j],
                       num.trees = random_forest_grid$num.trees[j],
                       max.depth = random_forest_grid$max.depth[j],
                       verbose = TRUE)
    
    predictions <- predict(rf_model, data = val_x)
    rmse_val[j] <- sqrt(mean((val_y$y - as.vector(predictions$predictions))^2))
  }
  
  optimal_mtry_rf <- random_forest_grid$mtry[which.min(rmse_val)]
  optimal_ntrees_rf <- random_forest_grid$num.trees[which.min(rmse_val)]
  optimal_depth_rf <- random_forest_grid$max.depth[which.min(rmse_val)]
  
  # Out-of-sample predictions
  rf_model <- ranger(y ~ . , 
                     data = cbind(train_y, train_x),
                     mtry = optimal_mtry_rf,
                     num.trees = optimal_ntrees_rf,
                     max.depth = optimal_depth_rf,
                     verbose = TRUE)
  
  predictions <- predict(rf_model, data = test_x)
  predictions_rf[[i]] <- predictions # stores predictions in list
  r2_rf[i] <- rsq(predict(rf_model, data = train_x)$predictions, train_y$y) # in-sample r-squared
  rmse_rf[i] <- sqrt(mean((test_y$y - as.vector(predictions$predictions))^2)) # out-of-sample RMSE
  
  message(paste0("Iteration ", i, " of ", length(colnames), " complete."))
}

# Plots in-sample Rsquared
plot(r2_rf,
     type = "l",
     col = "green",
     main = "In-Sample R-squared (RF)",
     xlab = "Hour",
     ylab = "R-squared")

# Plots OOS R-squared
plot(rmse_rf, 
     type = "l",
     col = "green",
     main = "Out-of-Sample RMSE (RF)",
     xlab = "Hour",
     ylab = "RMSE")

# Plotting all values
# Creates dataframe with r-squared values
r2_plot <- data.frame(r2_ols, r2_lasso, r2_pls, r2_rf)
# Generates plot
plot(r2_plot$r2_ols, type = "l", col = "black", ylim=c(0.45,1),
     main = "In-Sample R-squared", xlab = "Hour", ylab = "R-squared")
lines(r2_plot$r2_lasso, type = "l", col = "blue")
lines(r2_plot$r2_pls, type = "l", col = "red")
lines(r2_plot$r2_rf, type = "l", col = "green")
legend("bottomleft", legend=c("OLS", "LASSO", "PLS", "RF"),
       col=c("black", "blue", "red", "green"), lty=1:1, cex=0.5)

# Dataframe with RMSE values
rmse_plot <- data.frame(rmse_ols, rmse_lasso, rmse_pls, rmse_rf)
# Generates plot
plot(rmse_plot$rmse_ols, type = "l", col = "black",
     main = "Out-of-Sample RMSE", xlab = "Hour", ylab = "RMSE")
lines(rmse_plot$rmse_lasso, type = "l", col = "blue")
lines(rmse_plot$rmse_pls, type = "l", col = "red")
lines(rmse_plot$rmse_rf, type = "l", col = "green")
legend("topleft", legend=c("OLS", "LASSO", "PLS", "RF"),
       col=c("black", "blue", "red", "green"), lty=1:1, cex=0.5)


# Question 5.1 ------------------------------------------------------------
# Calculates average low and high prices and their corresponding hours
prices <- df[,1:25]
prices <- prices[date < "2020-07-01"][,-1]
prices <- t(colMeans(prices)) # transposes dataframe (long to wide)
low <- prices[,which.min(prices)] # 04:00 - 05:00 is lowest
high <- prices[,which.max(prices)] # 19:00 - 20:00 is highest

# Benchmark trading strategy
prices <- df[,1:25]
prices <- prices[date >= "2020-07-01"][,-1]
profit <- prices[, list(dutch_power_20, dutch_power_5)]
profit[, profit := dutch_power_20 - dutch_power_5]
message(paste0("Cumulative profit: ", sum(profit$profit)))
message(paste0("Volatility: ", round(sd(profit$profit), 2)))


# Question 5.2 ------------------------------------------------------------
# LASSO
# Creating 1 large dataframe with predictions
predictions_lasso_df <- setDT(as.data.frame(do.call(cbind, predictions_lasso)))
names(predictions_lasso_df) <- colnames(df[,2:25]) # assigns right column names
min_vector <- apply(predictions_lasso_df, 1, FUN = which.min) # determines index of column with min value
max_vector <- apply(predictions_lasso_df, 1, FUN = which.max) # determines index of column with max value

# Calculating profit of LASSO trading strategy
profit_vector_lasso <- vector()
for (i in 1:nrow(predictions_lasso_df)) {
  profit_vector_lasso[i] <- t(prices[i,])[max_vector[i]] - t(prices[i,])[min_vector[i]]
}
message(paste0("Cumulative profit: ", sum(profit_vector_lasso)))
message(paste0("Volatility: ", round(sd(profit_vector_lasso), 2)))

# Partial Least Squares
# Creating 1 large dataframe with predictions
predictions_pls_df <- setDT(as.data.frame(do.call(cbind, predictions_pls)))
names(predictions_pls_df) <- colnames(df[,2:25]) # assigns right column names
min_vector <- apply(predictions_pls_df, 1, FUN = which.min) # determines index of column with min value
max_vector <- apply(predictions_pls_df, 1, FUN = which.max) # determines index of column with max value

# Calculating profit of LASSO trading strategy
profit_vector_pls <- vector()
for (i in 1:nrow(predictions_pls_df)) {
  profit_vector_pls[i] <- t(prices[i,])[max_vector[i]] - t(prices[i,])[min_vector[i]]
}
message(paste0("Cumulative profit: ", sum(profit_vector_pls)))
message(paste0("Volatility: ", round(sd(profit_vector_pls), 2)))

# Random Forest
# Creating 1 large dataframe with predictions
predictions_rf_df <- list()
for (i in 1:length(predictions_rf)) {
  predictions_rf_df[[i]] <- predictions_rf[[i]]$predictions
}
predictions_rf_df <- setDT(as.data.frame(do.call(cbind, predictions_rf_df)))
names(predictions_rf_df) <- colnames(df[,2:25]) # assigns right column names
min_vector <- apply(predictions_rf_df, 1, FUN = which.min) # determines index of column with min value
max_vector <- apply(predictions_rf_df, 1, FUN = which.max) # determines index of column with max value

# Calculating profit of LASSO trading strategy
profit_vector_rf <- vector()
for (i in 1:nrow(predictions_rf_df)) {
  profit_vector_rf[i] <- t(prices[i,])[max_vector[i]] - t(prices[i,])[min_vector[i]]
}
message(paste0("Cumulative profit: ", sum(profit_vector_rf)))
message(paste0("Volatility: ", round(sd(profit_vector_rf), 2)))


# Question 5.3 ------------------------------------------------------------
# Determines best model for each hour
min_rmse <- vector() # empty vector
for (i in 1:nrow(rmse_plot)) {
  min_rmse[i] <- colnames(t(which.min(rmse_plot[i,])))
}

# Generates prediction dataframe using predictions of best model for each hour
# Generates OLS predictions dataframe
predictions_ols_df <- setDT(as.data.frame(do.call(cbind, predictions_ols)))
names(predictions_ols_df) <- colnames(df[,2:25]) # assigns right column names

final_prediction_df <- list()
for(i in 1:24) {
  pred_df <- as.data.frame(cbind(predictions_ols_df[[i]], predictions_lasso_df[[i]], predictions_pls_df[[i]], predictions_rf_df[[i]]))
  names(pred_df) <- c("rmse_ols", "rmse_lasso", "rmse_pls", "rmse_rf")
  final_prediction_df[[i]] <- pred_df[,min_rmse[i]]
}
final_prediction_df <- setDT(as.data.frame(do.call(cbind, final_prediction_df))) # cbinds all vectors within list
names(final_prediction_df) <- colnames(df[,2:25]) # assigns right column names

# Creates strategy
profit_strat_vec <- vector()
for (i in 1:nrow(final_prediction_df)) {
  # charge (morning)
  pred <- t(final_prediction_df[i,])
  charge_pred_morning <- which.min(pred[3:6])
  charge_pred_morning <- c(3:6)[charge_pred_morning]
  charge_real_morning <- -(t(prices[i,])[charge_pred_morning])
  # discharge (morning)
  discharge_pred_morning <- which.max(pred[8:11])
  discharge_pred_morning <- c(8:11)[discharge_pred_morning]
  discharge_real_morning <- t(prices[i,])[discharge_pred_morning]
  
  # charge (afternoon)
  charge_pred_after <- which.min(pred[14:17])
  charge_pred_after <- c(14:17)[charge_pred_after]
  charge_real_after <- -(t(prices[i,])[charge_pred_after])
  # discharge (afternoon)
  discharge_pred_after <- which.max(pred[18:21])
  discharge_pred_after <- c(18:21)[discharge_pred_after]
  discharge_real_after <- t(prices[i,])[discharge_pred_after]
  
  # Calculates total profit per day
  profit_strat_vec[i] <- (charge_real_morning + discharge_real_morning) + (charge_real_after + discharge_real_after)
}

message(paste0("Total profit is: ", sum(profit_strat_vec)))
message(paste0("Volatility is: ", round(sd(profit_strat_vec), 2)))

# Extra strategy ----------------------------------------------------------
library(schoolmath) # for question 5.3, is.odd() function

profit_strat_vec <- vector()
profit_list <- list()
for (i in 1:nrow(final_prediction_df)) {
  test <- t(final_prediction_df[i,])
  for (j in 1:24) {
    if (is.odd(j) == TRUE) {
      hour_1 <- test[j]
      hour_tplus1 <- test[j+1]
      if (hour_tplus1 > hour_1) {
        actual_price <- t(prices[i,])
        price_1 <- actual_price[j]
        price_tplus1 <- actual_price[j+1]
        profit_strat_vec[j] <- price_tplus1 - price_1
      } else {
        next
      }
    } else {
      next
    }
  }
  profit_strat_vec[is.na(profit_strat_vec)] <- 0
  profit_list[[i]] <- sum(profit_strat_vec)
  profit_strat_vec <- vector()
}

do.call(sum, profit_list) # very poor result 
