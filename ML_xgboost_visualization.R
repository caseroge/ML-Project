


importance <- xgb.importance(model = model_era)
print(importance)

xgb.plot.importance(importance, top_n = 10)

xgb.plot.shap(
  data  = train_x,
  model = model_era,
  top_n = 6
)

#add actual vs predicted plot 
#add residual distribution 
#add predicted vs actual ERA by bucket

