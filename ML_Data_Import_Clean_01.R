#845 Project Creating the Dataset 

install.packages("devtools")
devtools::install_github("BillPetti/baseballr")
pacman::p_load(baseballr, tidyverse, purrr, readr, dplyr)

library(baseballr)
library(tidyverse)

# Pull one season at a time and save locally
# statcast_search pulls pitch-level data, max ~30k rows per call
# so we loop by month to stay under the limit

pull_season_pitching <- function(season) {
  
  # Define date ranges by month to avoid hitting row limits
  # Spring season: April through October roughly
  months <- list(
    c(paste0(season, "-04-01"), paste0(season, "-04-30")),
    c(paste0(season, "-05-01"), paste0(season, "-05-31")),
    c(paste0(season, "-06-01"), paste0(season, "-06-30")),
    c(paste0(season, "-07-01"), paste0(season, "-07-31")),
    c(paste0(season, "-08-01"), paste0(season, "-08-31")),
    c(paste0(season, "-09-01"), paste0(season, "-09-30")),
    c(paste0(season, "-10-01"), paste0(season, "-10-15"))  # postseason cutoff
  )
  
  season_data <- purrr::map_dfr(months, function(dates) {
    message("Pulling ", dates[1], " to ", dates[2])
    Sys.sleep(5)  # be polite to the server
    statcast_search(
      start_date = dates[1],
      end_date   = dates[2],
      player_type = "pitcher"
    )
  })
  
  # Save raw CSV locally so you don't have to re-pull
  readr::write_csv(season_data, paste0("statcast_raw_", season, ".csv"))
  message("Saved season ", season)
  
  return(season_data)
}

# Run for one season first to test
data_2024 <- pull_season_pitching(2024)

getwd()
test123 <- read_csv("statcast_raw_2024.csv")



#lengthen table to separate by pitch types 

pitcher_season_2024 <- test123 %>%
  select(pitcher, player_name, pitch_type, release_speed, 
         release_spin_rate, pfx_x, pfx_z, release_extension) %>%
  filter(!is.na(pitch_type), pitch_type != "") %>%
  group_by(pitcher, player_name, pitch_type) %>%
  dplyr::summarise(
    n_pitches = n(),
    avg_velo = mean(release_speed, na.rm = T),
    avg_spin = mean(release_spin_rate, na.rm = T),
    avg_pfx_x = mean(pfx_x, na.rm = T),
    avg_pfx_z = mean(pfx_z, na.rm = T),
    avg_extension = mean(release_extension, na.rm = T),
    .groups = "drop"
  ) %>%
  filter(n_pitches >= 50)

#check it
dim(pitcher_season_2024)
head(pitcher_season_2024)

#pivot it wider now 
wide_2024 <- pitcher_season_2024 %>%
  pivot_wider(
    id_cols = c(pitcher, player_name),
    names_from = pitch_type,
    values_from = c(avg_velo, avg_spin, avg_pfx_x, avg_pfx_z, avg_extension, n_pitches),
    names_glue = "{pitch_type}_{.value}"
  )

#check it
head(wide_2024)         


#add era and whip 
pitching_stat_2024 <- mlb_stats(
  stat_type = "season",
  player_pool = "All",
  stat_group = "pitching",
  season = 2024
) %>% select(player_id, era, whip, innings_pitched) %>%
  filter(as.numeric(innings_pitched) >= 50)

wide_2024_f <- wide_2024 %>% 
  left_join(pitching_stat_2024, by = c("pitcher" = "player_id")) %>%
  filter(!is.na(era), !is.na(whip))

head(wide_2024_f)                                         


clean_data <- function(season){
  message("Now Cleaning")
  #read in data
  raw <- read.csv(paste0("statcast_raw_", season, ".csv"))
  
  #aggregate by pitcher and pitch type 
  long <- raw %>% select(pitcher, player_name, pitch_type,
                         release_speed, release_spin_rate,
                         pfx_x, pfx_z, release_extension) %>%
    filter(!is.na(pitch_type), pitch_type != "") %>%
    dplyr::group_by(pitcher, player_name, pitch_type) %>%
    dplyr::summarise(
      n_pitches     = n(),
      avg_velo      = mean(release_speed, na.rm = TRUE),
      avg_spin      = mean(release_spin_rate, na.rm = TRUE),
      avg_pfx_x     = mean(pfx_x, na.rm = TRUE),
      avg_pfx_z     = mean(pfx_z, na.rm = TRUE),
      avg_extension = mean(release_extension, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_pitches >= 50)
  
  #pivot wide 
  
  wide <- long %>%
    pivot_wider(
      id_cols     = c(pitcher, player_name),
      names_from  = pitch_type,
      values_from = c(avg_velo, avg_spin, avg_pfx_x, avg_pfx_z, avg_extension, n_pitches),
      names_glue  = "{pitch_type}_{.value}"
    )
  
  #pull and add era and join
  pitching_stats <- mlb_stats(
    stat_type   = "season",
    player_pool = "All",
    stat_group  = "pitching",
    season      = season
  ) %>%
    select(player_id, era, whip, innings_pitched) %>%
    filter(as.numeric(innings_pitched) >= 50)
  
  final_data <- wide %>%
    left_join(pitching_stats, by = c("pitcher" = "player_id")) %>%
    filter(!is.na(era), !is.na(whip)) %>%
    mutate(season = season)
  
  message("Done cleaning season")
  
  return(final_data)
}         



seasons <- c(2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024)

all_seasons <- do.call(rbind, lapply(seasons, clean_pitcher_season))


pull_season_pitching(2024)
data_2024 <- clean_data(2024)

pull_season_pitching(2023)
data_2023 <- clean_data(2023)

pull_season_pitching(2022)
data_2022 <- clean_data(2022)

pull_season_pitching(2021)
data_2021 <- clean_data(2021)

pull_season_pitching(2019)
data_2019 <- clean_data(2019)

pull_season_pitching(2018)
data_2018 <- clean_data(2018)

pull_season_pitching(2017)
data_2017 <- clean_data(2017)

pull_season_pitching(2016)
data_2016 <- clean_data(2016)

pull_season_pitching(2015)
data_2015 <- clean_data(2015)

pull_season_pitching(2014)
data_2014 <- clean_data(2014)

pull_season_pitching(2025)
data_2025 <- clean_data(2025)


all_seasons <- dplyr::bind_rows(data_2014, data_2015, data_2016, data_2017, data_2018,
                                data_2019, data_2021, data_2022, data_2023, data_2024)
head(all_seasons)
dim(all_seasons)

write.csv(all_seasons, "all_seasons.csv", row.names = F)
write.csv(data_2025, "data_2025.csv", row.names = F)
all_seasons <- read.csv("all_seasons.csv")
data_2025 <- read.csv("data_2025.csv")

dim(all_seasons)
dim(data_2025)