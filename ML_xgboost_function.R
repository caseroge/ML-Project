#XGBOOST FUNCTION

run_xgb_model <- function(train_data, test_data, params, nrounds = 500, seed = 123) {
  
  # drop non-numeric and ID columns
  drop_cols <- c("player_name", "season", "innings_pitched", "pitcher")
  
  # create matrices
  train_x <- train_data %>% select(-any_of(drop_cols), -era, -whip) %>% as.matrix()
  test_x  <- test_data  %>% select(-any_of(drop_cols), -era, -whip) %>% as.matrix()
  
  # pull out target vars
  train_era  <- train_data$era
  train_whip <- train_data$whip
  test_era   <- as.numeric(test_data$era)
  test_whip  <- as.numeric(test_data$whip)
  
  # convert to DMatrix
  dtrain_era  <- xgb.DMatrix(data = train_x, label = train_era)
  dtest_era   <- xgb.DMatrix(data = test_x,  label = test_era)
  dtrain_whip <- xgb.DMatrix(data = train_x, label = train_whip)
  dtest_whip  <- xgb.DMatrix(data = test_x,  label = test_whip)
  
  # train ERA model
  set.seed(seed)
  model_era <- xgb.train(
    params               = params,
    data                 = dtrain_era,
    nrounds              = nrounds,
    watchlist            = list(train = dtrain_era, test = dtest_era),
    print_every_n        = 50,
    early_stopping_rounds = 30
  )
  
  # train WHIP model
  set.seed(seed)
  model_whip <- xgb.train(
    params               = params,
    data                 = dtrain_whip,
    nrounds              = nrounds,
    watchlist            = list(train = dtrain_whip, test = dtest_whip),
    print_every_n        = 50,
    early_stopping_rounds = 30
  )
  
  # generate predictions
  pred_era  <- predict(model_era,  dtest_era)
  pred_whip <- predict(model_whip, dtest_whip)
  
  # evaluate
  rmse_era  <- sqrt(mean((test_era  - pred_era)^2))
  mae_era   <- mean(abs(test_era   - pred_era))
  r2_era    <- 1 - sum((test_era   - pred_era)^2)  / sum((test_era  - mean(test_era))^2)
  rmse_whip <- sqrt(mean((test_whip - pred_whip)^2))
  mae_whip  <- mean(abs(test_whip  - pred_whip))
  r2_whip   <- 1 - sum((test_whip  - pred_whip)^2) / sum((test_whip - mean(test_whip))^2)
  
  # print results
  cat("=== ERA Model ===\n")
  cat("RMSE:", round(rmse_era,  4), "\n")
  cat("MAE: ", round(mae_era,   4), "\n")
  cat("R2:  ", round(r2_era,    4), "\n\n")
  cat("=== WHIP Model ===\n")
  cat("RMSE:", round(rmse_whip, 4), "\n")
  cat("MAE: ", round(mae_whip,  4), "\n")
  cat("R2:  ", round(r2_whip,   4), "\n")
  
  # return models and predictions invisibly
  invisible(list(
    model_era  = model_era,
    model_whip = model_whip,
    pred_era   = pred_era,
    pred_whip  = pred_whip,
    metrics    = list(rmse_era = rmse_era, mae_era = mae_era, r2_era = r2_era,
                      rmse_whip = rmse_whip, mae_whip = mae_whip, r2_whip = r2_whip)
  ))
}


plot_xgb_diagnostics <- function(model, train_x, top_n_importance = 10, top_n_shap = 6) {
  
  # Feature importance
  importance <- xgb.importance(model = model)
  print(importance)
  xgb.plot.importance(importance, top_n = top_n_importance)
  
  # SHAP summary — manages its own layout internally
  xgb.plot.shap(
    data  = train_x,
    model = model,
    top_n = top_n_shap
  )
  
  invisible(importance)
}
