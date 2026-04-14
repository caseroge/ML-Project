#Decision Tree Model 
library(rpart)
library(tidyverse)
#testing xgboost against a more basic decsion tree model 
data_2025 <- data_2025 %>%
  mutate(across(c(KN_avg_velo, KN_avg_spin, KN_avg_pfx_x, KN_avg_pfx_z, KN_avg_extension, KN_n_pitches), as.numeric))
test_data <- data_2025 %>% select(-c("season", "innings_pitched", "pitcher", "player_name"))

tree_ERA <- rpart(era ~ . - whip - season - innings_pitched - pitcher - player_name, 
                  data = all_seasons,
                  na.action = na.pass)

tree_preds_ERA <- predict(tree_ERA, newdata = data_2025, na.action = na.pass)

# Metrics
actuals <- data_2025$era

rmse_tree <- sqrt(mean((tree_preds_ERA - actuals)^2, na.rm = TRUE))
mae_tree  <- mean(abs(tree_preds_ERA - actuals), na.rm = TRUE)
r2_tree   <- 1 - sum((actuals - tree_preds_ERA)^2, na.rm = TRUE) / sum((actuals - mean(actuals, na.rm = TRUE))^2, na.rm = TRUE)



rmse_tree #1.16 
mae_tree #0.923
r2_tree #0.164


#WHIP 

tree_WHIP <- rpart(whip ~ . - era - season - innings_pitched - pitcher - player_name, 
                   data = all_seasons,
                   na.action = na.pass)

tree_preds_WHIP <- predict(tree_WHIP, newdata = data_2025, na.action = na.pass)

# Metrics
actuals_whip <- data_2025$whip

rmse_tree_whip <- sqrt(mean((tree_preds_WHIP - actuals_whip)^2, na.rm = TRUE))
mae_tree_whip  <- mean(abs(tree_preds_WHIP - actuals_whip), na.rm = TRUE)
r2_tree_whip   <- 1 - sum((actuals_whip - tree_preds_WHIP)^2, na.rm = TRUE) / sum((actuals_whip - mean(actuals_whip, na.rm = TRUE))^2, na.rm = TRUE)

rmse_tree_whip #1.194
mae_tree_whip #1.521
r2_tree_whip #-0.180

install.packages('rpart.plot')
library(rpart.plot)

# Variable importance
importance_tree <- tree_ERA$variable.importance
barplot(sort(importance_tree, decreasing = TRUE),
        las = 2,
        main = "Decision Tree Variable Importance - ERA",
        ylab = "Importance")

rpart.plot(tree_ERA, 
           main = "Decision Tree - ERA",
           type = 4,
           extra = 101)
