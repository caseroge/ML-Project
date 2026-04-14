#Baseline means 
library(caret)


#Calculating the mean of ERA and WHIP to see if xgboost model is better than nothing 


#set control method
control <- trainControl(method = 'none')

mean_era <- mean(all_seasons$era, na.rm = T)
mean_whip <- mean(all_seasons$whip, na.rm = T)
mean_era #3.96
mean_whip #1.21

rmse_1 <- sqrt(mean((mean_era - data_2025$era)^2))
rmse_1 #1.17
rmse_2 <- sqrt(mean((mean_whip - data_2025$whip)^2))
rmse_2 #0.19

mae_1 <- mean(abs(mean_era - data_2025$era))
mae_1 #0.93
mae_2 <- mean(abs(mean_whip - data_2025$whip))
mae_2 #0.151

R2 <- 1 - sum((data_2025$era - mean_era)^2) / sum((data_2025$era - mean(data_2025$era))^2)
R2 #-0.01
R22 <- 1 - sum((data_2025$whip - mean_whip)^2) / sum((data_2025$whip - mean(data_2025$whip))^2)
R22 #-0.025

#comparing these numbers to xgboost, they are much worse.
