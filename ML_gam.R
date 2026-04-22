#GAM Model 

#Will be creating and fitting a GAM model to test against the xgboost model

#create gam data
#ony keep fastball, slider, changeup data.

# See what proportion of pitchers have each pitch type
colSums(!is.na(all_seasons[, c("FF_avg_velo", "SL_avg_velo", "CH_avg_velo", 
                               "SI_avg_velo", "FC_avg_velo")])) / nrow(all_seasons)
