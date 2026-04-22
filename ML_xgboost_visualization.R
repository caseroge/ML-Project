


library(pdp)
install.packages('pdp')

# You need to pass the raw matrix, not a data frame
# Use the same matrix you used to build dtrain_era
partial_fb <- pdp::partial(model_era,
                      pred.var = "ff_release_speed",
                      train = as.matrix(train_data %>% select(-era, -whip, -season, -innings_pitched)))

pdp::plotPartial(partial_fb,
            ylab = "Predicted ERA",
            xlab = "Fastball Velocity (mph)",
            main = "Fastball Velo vs Predicted ERA")



importance <- xgb.importance(model = model_era)
print(importance)

par(mfrow = c(1, 2))
xgb.plot.importance(importance, top_n = 10)
par
xgb.plot.shap(
  data  = train_x,
  model = model_era,
  top_n = 6
)

importance_whip <- xgb.importance(model = model_whip)
print(importance_whip)

xgb.plot.importance_whip(importance_whip, top_n = 10)

xgb.plot.shap(
  data  = train_x,
  model = model_whip,
  top_n = 6
)

#add actual vs predicted plot 
#add residual distribution 
#add predicted vs actual ERA by bucket

