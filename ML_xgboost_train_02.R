#845_proj_02

all_seasons <- read.csv("all_seasons.csv")
data_2025 <- read.csv("data_2025.csv")


dim(all_seasons)
dim(data_2025)

# check for any obvious issues

all_seasons <- all_seasons %>% 
  select(-ends_with("n_pitches"))

data_2025 <- data_2025 %>%
  select(-ends_with("n_pitches"))

#look for NA
sum(is.na(all_seasons$era)) #no NA
sum(is.na(all_seasons$whip)) #no NA


#ML packages 
library(xgboost)
library(dplyr)
library(caret)

# Step 1: Set up features and targets
# drop non-numeric and ID columns
drop_cols <- c("player_name", "season", "innings_pitched")


#create training set, turn into a matrix 
train_x <- all_seasons %>%
  select(-any_of(drop_cols), -era, -whip, -pitcher) %>%
  as.matrix()

#create testing set, drop output values, turn into a matrix 
test_x <- data_2025 %>%
  select(-any_of(drop_cols), -era, -whip, -pitcher) %>%
  as.matrix()

#pull out the target vars 
train_era  <- all_seasons$era
train_whip <- all_seasons$whip
test_era   <- data_2025$era
test_whip  <- data_2025$whip


# Step 2: Convert to DMatrix 
dtrain_era  <- xgb.DMatrix(data = train_x, label = train_era)
dtest_era   <- xgb.DMatrix(data = test_x,  label = test_era)

dtrain_whip <- xgb.DMatrix(data = train_x, label = train_whip)
dtest_whip  <- xgb.DMatrix(data = test_x,  label = test_whip)

# set the parameters 
params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,      # learning rate
  max_depth        = 4,         # tree depth
  subsample        = 0.8,       # row sampling
  colsample_bytree = 0.8,       # column sampling
  min_child_weight = 5          # minimum observations per leaf
)

# Step 4: Train ERA model
set.seed(123)
model_era <- xgb.train(
  params    = params,
  data      = dtrain_era,
  nrounds   = 500,
  watchlist = list(train = dtrain_era, test = dtest_era),
  print_every_n = 50,
  early_stopping_rounds = 30
)

model_era$params

# Step 5: Train WHIP model
set.seed(123)
model_whip <- xgb.train(
  params    = params,
  data      = dtrain_whip,
  nrounds   = 500,
  watchlist = list(train = dtrain_whip, test = dtest_whip),
  print_every_n = 50,
  early_stopping_rounds = 30
)

# Step 6: Generate predictions
pred_era  <- predict(model_era,  dtest_era)
pred_whip <- predict(model_whip, dtest_whip)


test_era  <- as.numeric(data_2025$era)
test_whip <- as.numeric(data_2025$whip)

# Step 7: Evaluate ERA model
rmse_era <- sqrt(mean((test_era - pred_era)^2))
mae_era  <- mean(abs(test_era - pred_era))
r2_era   <- 1 - sum((test_era - pred_era)^2) / sum((test_era - mean(test_era))^2)

# Step 8: Evaluate WHIP model
rmse_whip <- sqrt(mean((test_whip - pred_whip)^2))
mae_whip  <- mean(abs(test_whip - pred_whip))
r2_whip   <- 1 - sum((test_whip - pred_whip)^2) / sum((test_whip - mean(test_whip))^2)

# Step 9: Print results
cat("=== ERA Model ===\n")
cat("RMSE:", round(rmse_era, 4), "\n")
cat("MAE: ", round(mae_era,  4), "\n")
cat("R2:  ", round(r2_era,   4), "\n\n")

cat("=== WHIP Model ===\n")
cat("RMSE:", round(rmse_whip, 4), "\n")
cat("MAE: ", round(mae_whip,  4), "\n")
cat("R2:  ", round(r2_whip,   4), "\n")


#Results
#RMSE, average magnitude of errors, but squares so larger errors are more penalized
#model prediction is off by about .7 points for ERA

#MAE similar but treats all errors equally, more robust, notably lower than RMSE, means that could be making some errors 

#R2 - proportion of variance in the outcome explained by your model. .6 - 60% of variance in ERA explained by model.


#ERA RMSE - 0.7405
#ERA MAE - 0.5547
#ERA R2 - 0.6001

#WHIP RMSE - 0.1248
#WHIP MAE - 0.0933
#WHIP R2 - 0.5791

class(test_era)
class(pred_era)

#hyper parameter tuning, trying to get model predicting even better 
tune_grid <- expand.grid(
  nrounds = c(100, 200, 300),
  max_depth = c(3,6,9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

#using carat this time 

#FIND BEST PARAMETERS
set.seed(123)
xgb_tune <- train(era~. - whip - innings_pitched - season, 
                  data = all_seasons, 
                  method = "xgbTree",
                  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE ),
                  tuneGrid = tune_grid,
                  na.action = na.pass)

xgb_tune_whip <- train(whip~. - era - innings_pitched - season, 
                  data = all_seasons, 
                  method = "xgbTree",
                  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
                  tuneGrid = tune_grid,
                  na.action = na.pass)


#HOWEVER, THESE MODELS WERE NOT TESTED ON 2025 model so need to test on 2025 model
#DOING SO BELOW

#ERA MODEL 
#extract best parameters
xgb_tune
xgb_tune$bestTune

tune_preds_ERA <- predict(xgb_tune, newdata = data_2025, na.action = na.pass)

#re reun on 2025 season
params_tuned <- list(
  objective        = "reg:squarederror",
  eta              = 0.1,       # from bestTune
  max_depth        = 3,         # from bestTune
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1          # from bestTune
)

xgb_tuned_ERA <- xgb.train(
  params  = params_tuned,
  data    = dtrain_era,
  nrounds = 100               # from bestTune
)

#asseess again
# Create test matrix the same way you created dtrain_era
dtest_ERA <- xgb.DMatrix(
  data = as.matrix(data_2025 %>% 
                     select(all_of(xgb_tuned_ERA$feature_names))),
  missing = NA
)

# Then predict
predict1 <- predict(xgb_tuned_ERA, newdata = dtest_ERA)

# Metrics
get_metrics(predict1, data_2025$era)


#WHIP MODEL 
#extract best parameters
xgb_tune_whip
xgb_tune_whip$bestTune
tune_preds_WHIP <- predict(xgb_tune_whip, newdata = data_2025, na.action = na.pass)

#re run on 2025 season
params_tuned_whip <- list(
  objective        = "reg:squarederror",
  eta              = 0.1,       # from bestTune
  max_depth        = 9,         # from bestTune
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1          # from bestTune
)

xgb_tuned_WHIP <- xgb.train(
  params  = params_tuned_whip,
  data    = dtrain_whip,
  nrounds = 100               # from bestTune
)

# Create test matrix
dtest_WHIP <- xgb.DMatrix(
  data = as.matrix(data_2025 %>% 
                     select(all_of(xgb_tuned_WHIP$feature_names))),
  missing = NA
)

# Then predict
predict1_whip <- predict(xgb_tuned_WHIP, newdata = dtest_WHIP)

# Metrics
get_metrics(predict1_whip, data_2025$whip)


#The WHIP MODEL IS SO MUCH BETTER




#functiun for extracting parameters
get_metrics <- function(preds, actuals) {
  rmse <- sqrt(mean((preds - actuals)^2, na.rm = TRUE))
  mae  <- mean(abs(preds - actuals), na.rm = TRUE)
  r2   <- 1 - sum((actuals - preds)^2, na.rm = TRUE) / 
    sum((actuals - mean(actuals, na.rm = TRUE))^2, na.rm = TRUE)
  
  cat("RMSE:", round(rmse, 4), "\n")
  cat("MAE: ", round(mae, 4), "\n")
  cat("R2:  ", round(r2, 4), "\n")
}


#chekcing metrics of the non-2025 models
get_metrics(tune_preds_ERA, data_2025$era)
get_metrics(tune_preds_WHIP, data_2025$whip)



