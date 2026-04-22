#ML Manupulating Dataset

#plan 1
#create ranking for each category and each pitcher 
#turn those rankings into deciles 
#run ML 

#plan 2
#create only two pitches - fastball and breaking ball.

#usage weighted average

#step 1
fastballs <- c('FF', 'SI', 'FC', 'FA', 'FS')
breakingballs <- c('SL', 'ST', 'CU', 'KC', 'SV')
offspeed <- c('CH')

fastballs1 <- paste0(fastballs, '_n_pitches' )
breakingballs1 <- paste0(breakingballs, '_n_pitches' )
offspeed1 <- paste0(offspeed, '_n_pitches' )

#step 2, pull data out 

dataset1 <- all_seasons %>% select(all_of(fastballs1), all_of(breakingballs1), all_of(offspeed1))

#step 3, calculat weights for each pitch type within eahc category 

#calculate totals
fastball_sums <- dataset1 %>% select(fastballs1) %>% rowSums(na.rm = T)
breakingballs_sums <- dataset1 %>% select(breakingballs1) %>% rowSums(na.rm = T)
offspeed_sums <- dataset1 %>% select(offspeed1) %>% rowSums(na.rm = T)


#calculate weights for each pitch type 

fastball_cols <- dataset1 %>% select(all_of(fastballs1)) %>% replace(is.na(.), 0)
fastball_weights <- as.data.frame(fastball_cols/fastball_sums) %>% mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

breakingball_cols <- dataset1 %>% select(all_of(breakingballs1)) %>% replace(is.na(.), 0)
breakingball_weights <- as.data.frame(breakingballs_cols/breakingballs_sums) %>% mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

offspeed_cols <- dataset1 %>% select(all_of(offspeed1)) %>% replace(is.na(.), 0)
offspeed_weights <- as.data.frame(offspeed_cols/offspeed_sums) %>% mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

rowSums(fastball_weights)

#step 4 - calculate weighted average for each metric within each category

#multiple each pitch by its weight 
metrics <- c("spin", 'pfx_x', 'pfx_z', 'extension')


fastballs2 <- paste0(fastballs, '_avg_velo')
dataset2 <- all_seasons %>% select(all_of(fastballs2))
dataset2[is.na(dataset2)] <- 0
names(fastball_weights) <- names(dataset2)
fastballs3 <- dataset2 * fastball_weights
fastballs4 <- rowSums(fastballs3, na.rm = T)
fastballs4[fastballs4 == 0] <- NA
fastballs4

metrics <- c("velo", "spin", 'pfx_x', 'pfx_z', 'extension')

#fastballs 
for (i in metrics) {
  fastballs2.a <- paste0(fastballs, '_avg_', i)
  dataset2.a <- all_seasons %>% select(all_of(fastballs2.a))
  dataset2.a[is.na(dataset2.a)] <- 0
  names(fastball_weights) <- names(dataset2.a)
  fastballs3.a <- dataset2.a * fastball_weights
  fastballs4.a <- rowSums(fastballs3.a, na.rm = T)
  fastballs4.a[fastballs4.a == 0] <- NA
  assign(paste0('fastballs_', i), fastballs4.a)
}

fastballs_extension
fastballs_pfx_x
fastballs_pfx_z
fastballs_spin

#breaking balls 
for (i in metrics) {
  breakingballs2.a <- paste0(breakingballs, '_avg_', i)
  dataset2.a <- all_seasons %>% select(all_of(breakingballs2.a))
  dataset2.a[is.na(dataset2.a)] <- 0
  names(breakingballs_weights) <- names(dataset2.a)
  breakingballs3.a <- dataset2.a * breakingballs_weights
  breakingballs4.a <- rowSums(breakingballs3.a, na.rm = T)
  breakingballs4.a[breakingballs4.a == 0] <- NA
  assign(paste0('breakingballs_', i), breakingballs4.a)
}

#offspeed
for (i in metrics) {
  offspeed2.a <- paste0(offspeed, '_avg_', i)
  dataset2.a <- all_seasons %>% select(all_of(offspeed2.a))
  dataset2.a[is.na(dataset2.a)] <- 0
  names(offspeed_weights) <- names(dataset2.a)
  offspeed3.a <- dataset2.a * offspeed_weights
  offspeed4.a <- rowSums(offspeed3.a, na.rm = T)
  offspeed4.a[offspeed4.a == 0] <- NA
  assign(paste0('offspeed_', i), offspeed4.a)
}

#assemble back into final datset
final_dataset <- data.frame(
  all_seasons$pitcher,
  all_seasons$player_name,
  all_seasons$season,
  all_seasons$era,
  all_seasons$whip,
  all_seasons$innings_pitched,
  fastballs_velo,
  fastballs_spin,
  fastballs_pfx_x,
  fastballs_pfx_z,
  fastballs_extension,
  breakingballs_velo,
  breakingballs_spin,
  breakingballs_pfx_x,
  breakingballs_pfx_z,
  breakingballs_extension,
  offspeed_velo,
  offspeed_spin,
  offspeed_pfx_x,
  offspeed_pfx_z,
  offspeed_extension
)

#sanity check 
head(final_dataset)
dim(final_dataset)     

#clean names
colnames(final_dataset) <- c('pitcher', 'player_name', 'season', 'era', 'whip', 
                             'innings_pitched', 'fastballs_velo', 'fastballs_spin', 
                             'fastballs_pfx_x', 'fastballs_pfx_z', 'fastballs_extension',
                             'breakingballs_velo', 'breakingballs_spin', 'breakingballs_pfx_x', 
                             'breakingballs_pfx_z', 'breakingballs_extension', 'offspeed_velo', 
                             'offspeed_spin', 'offspeed_pfx_x', 'offspeed_pfx_z', 
                             'offspeed_extension')

#check missingness
colSums(is.na(final_dataset))



#now ranking
ranked_dataset <- final_dataset %>% 
  group_by(season) %>% 
  mutate(across(names(select(final_dataset, -c('pitcher', 'player_name', 'season', 'innings_pitched', 'era', 'whip'))), dense_rank))
head(ranked_dataset)
dim(ranked_dataset)


#do it all again for data_2025
# step 2
dataset1_2025 <- data_2025 %>% select(all_of(fastballs1), all_of(breakingballs1), all_of(offspeed1))

# step 3
fastball_sums_2025 <- dataset1_2025 %>% select(fastballs1) %>% rowSums(na.rm = T)
breakingballs_sums_2025 <- dataset1_2025 %>% select(breakingballs1) %>% rowSums(na.rm = T)
offspeed_sums_2025 <- dataset1_2025 %>% select(offspeed1) %>% rowSums(na.rm = T)

fastball_cols_2025 <- dataset1_2025 %>% select(all_of(fastballs1)) %>% replace(is.na(.), 0)
fastball_weights_2025 <- as.data.frame(fastball_cols_2025/fastball_sums_2025) %>% mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
breakingball_cols_2025 <- dataset1_2025 %>% select(all_of(breakingballs1)) %>% replace(is.na(.), 0)
breakingball_weights_2025 <- as.data.frame(breakingball_cols_2025/breakingballs_sums_2025) %>% mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
offspeed_cols_2025 <- dataset1_2025 %>% select(all_of(offspeed1)) %>% replace(is.na(.), 0)
offspeed_weights_2025 <- as.data.frame(offspeed_cols_2025/offspeed_sums_2025) %>% mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

# step 4 - fastballs
for (i in metrics) {
  fastballs2.a <- paste0(fastballs, '_avg_', i)
  dataset2.a <- data_2025 %>% select(all_of(fastballs2.a))
  dataset2.a[is.na(dataset2.a)] <- 0
  names(fastball_weights_2025) <- names(dataset2.a)
  fastballs3.a <- dataset2.a * fastball_weights_2025
  fastballs4.a <- rowSums(fastballs3.a, na.rm = T)
  fastballs4.a[fastballs4.a == 0] <- NA
  assign(paste0('fastballs_2025_', i), fastballs4.a)
}

# breaking balls
for (i in metrics) {
  breakingballs2.a <- paste0(breakingballs, '_avg_', i)
  dataset2.a <- data_2025 %>% select(all_of(breakingballs2.a))
  dataset2.a[is.na(dataset2.a)] <- 0
  names(breakingball_weights_2025) <- names(dataset2.a)
  breakingballs3.a <- dataset2.a * breakingball_weights_2025
  breakingballs4.a <- rowSums(breakingballs3.a, na.rm = T)
  breakingballs4.a[breakingballs4.a == 0] <- NA
  assign(paste0('breakingballs_2025_', i), breakingballs4.a)
}

# offspeed
for (i in metrics) {
  offspeed2.a <- paste0(offspeed, '_avg_', i)
  dataset2.a <- data_2025 %>% select(all_of(offspeed2.a))
  dataset2.a[is.na(dataset2.a)] <- 0
  names(offspeed_weights_2025) <- names(dataset2.a)
  offspeed3.a <- dataset2.a * offspeed_weights_2025
  offspeed4.a <- rowSums(offspeed3.a, na.rm = T)
  offspeed4.a[offspeed4.a == 0] <- NA
  assign(paste0('offspeed_2025_', i), offspeed4.a)
}

# assemble
final_dataset_2025 <- data.frame(
  data_2025$pitcher,
  data_2025$player_name,
  data_2025$season,
  data_2025$era,
  data_2025$whip,
  data_2025$innings_pitched,
  fastballs_2025_velo,
  fastballs_2025_spin,
  fastballs_2025_pfx_x,
  fastballs_2025_pfx_z,
  fastballs_2025_extension,
  breakingballs_2025_velo,
  breakingballs_2025_spin,
  breakingballs_2025_pfx_x,
  breakingballs_2025_pfx_z,
  breakingballs_2025_extension,
  offspeed_2025_velo,
  offspeed_2025_spin,
  offspeed_2025_pfx_x,
  offspeed_2025_pfx_z,
  offspeed_2025_extension
)

colnames(final_dataset_2025) <- c('pitcher', 'player_name', 'season', 'era', 'whip',
                                  'innings_pitched', 'fastballs_velo', 'fastballs_spin',
                                  'fastballs_pfx_x', 'fastballs_pfx_z', 'fastballs_extension',
                                  'breakingballs_velo', 'breakingballs_spin', 'breakingballs_pfx_x',
                                  'breakingballs_pfx_z', 'breakingballs_extension', 'offspeed_velo',
                                  'offspeed_spin', 'offspeed_pfx_x', 'offspeed_pfx_z',
                                  'offspeed_extension')

ranked_dataset_2025 <- final_dataset_2025 %>% 
  group_by(season) %>% 
  mutate(across(names(select(final_dataset, -c('pitcher', 'player_name', 'season', 'innings_pitched', 'era', 'whip'))), dense_rank))
head(ranked_dataset)
dim(ranked_dataset)


run_xgb_model <- function(train_data, test_data, params, nrounds = 500, seed = 123) 
  
  
weighted_model <- run_xgb_model(train_data = final_dataset, test_data = final_dataset_2025, params = params)
ranked_model <- run_xgb_model(train_data = ranked_dataset, test_data = ranked_dataset_2025, params = params)



plot_xgb_diagnostics(weighted_model, final_dataset)
