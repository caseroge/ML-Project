#More Visualizations 

#bar chart comparing the RSME, MAE, and R2 in each of the three models 


library(ggplot2)

# Create a dataframe of all results
results <- data.frame(
  Model = rep(c("Mean Baseline", "Decision Tree", "XGBoost"), each = 2),
  Outcome = rep(c("ERA", "WHIP"), 3),
  RMSE = c(1.17, 0.19, 1.16, 0.19, 0.74, 0.12),
  MAE  = c(0.93, 0.15, 0.92, 0.15, 0.55, 0.09),
  R2   = c(0, 0, 0.016, -0.018, 0.600, 0.579)
)

# RMSE comparison
ggplot(results, aes(x = Model, y = RMSE, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE Comparison Across Models", y = "RMSE") +
  theme_minimal()

# MAE comparison
ggplot(results, aes(x = Model, y = MAE, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "MAE Comparison Across Models", y = "MAE") +
  theme_minimal()

# R2 comparison
ggplot(results, aes(x = Model, y = R2, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "R² Comparison Across Models", y = "R²") +
  theme_minimal()

