#845_proj_02

all_seasons <- read.csv("all_seasons.csv")
data_2025 <- read.csv("data_2025.csv")


dim(all_seasons)
dim(data_2025)

# check for any obvious issues

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
