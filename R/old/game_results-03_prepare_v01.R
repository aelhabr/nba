
#'
#'
#'
#+ include = FALSE
rm(list = ls())
setwd("O:/_other/projects/nba/")

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
library("readr")

#'
#'
#'
# Parameters. ----
filename_import_base <- "game_results"
filename_import_suffix <- "-cleaned"
filename_import_ext <- ".csv"
dir_import <- "data/"
filepath_locations <- "data/locations-scraped.csv"
filepath_import <-
  str_c(dir_import, filename_import_base, filename_import_suffix, filename_import_ext)

#'
#'
#'
#+ include = FALSE
export <- TRUE
remove_tempvars <- TRUE

if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "-prepared"
  filename_export_ext <- ".csv"
  dir_export <- "data/"
  filepath_export <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      filename_export_ext
    )
}

#'
#'
#'
# Import. ----
results_cleaned <- read_csv(filepath_import)
tms_geo <- read_csv(filepath_locations)

#'
#'
#'
# Prepare data set for modeling. ----
results_geo <-
	results_cleaned %>%
  mutate(pd = pts_home - pts_away) %>% 
  mutate(pts_total = pts_home + pts_away) %>% 
  mutate(season = if_else(month(date) >= 10, year(date), year(date) - 1)) %>% 
  inner_join(tms_geo %>% select(tm, lat, lng) %>% rename_all(funs(str_c(., "_home"))),
             by = c("tm_home")) %>% 
  inner_join(tms_geo %>% select(tm, lat, lng) %>% rename_all(funs(str_c(., "_away"))),
             by = c("tm_away")) %>% 
  select(date, season, tm_home, tm_away, pts_home, pts_away, pd, everything())
results_geo
#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  rm(list = c("results_cleaned", "tms_geo"))
}
#'
#'
#'
results_side1 <-
  results_geo %>% 
  rename_at(vars(contains("_home")), funs(str_replace_all(., "_home", "_1"))) %>% 
  rename_at(vars(contains("_away")), funs(str_replace_all(., "_away", "_2"))) %>% 
  mutate(tm_home = tm_1, tm_away = tm_2, pts_home = pts_1, pts_away = pts_2)

results_side2 <-
  results_geo %>% 
  rename_at(vars(contains("_home")), funs(str_replace_all(., "_home", "_2"))) %>% 
  rename_at(vars(contains("_away")), funs(str_replace_all(., "_away", "_1"))) %>% 
  mutate(tm_home = tm_2, tm_away = tm_1, pts_home = pts_2, pts_away = pts_1)

results_2x <-
  results_side1 %>%
  bind_rows(results_side2) %>%
  select(date, season, tm_home, tm_away, pts_home, pts_away, pd, pts_total, everything()) %>% 
  arrange(date, season, tm_1, tm_2)

#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  rm(list = c("results_geo", "results_side1", "results_side2"))
}

#'
#'
#'
colnames_exclude <- 
  names(results_2x) %>% 
  str_subset("(_[0-9]$|_diff)$") %>% 
  # str_subset("[^(tm_)]") %>%
  str_subset("^(?!tm_[1-2]|pts_[1-2]).*$") %>% 
  str_subset("^(?!lng_[1-2]|lat_[1-2])") %>% 
  str_subset("^(?!ortg_[1]|tspct_[1])") %>% 
  str_subset("^(?!2p_[1-2]|3p_[1-2]|ft_[1-2])") %>% 
  c("mp")

results_selected <-
  results_2x %>% 
  select(-one_of(colnames_exclude)) %>% 
  arrange(date, season, tm_home)

results_selected <-
  results_selected %>% 
  rename(ortg = ortg_1,
         tspct = tspct_1,
         `3p` = `3p_1`,
         `2p` = `2p_1`,
         ft = ft_1)
#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  rm(list = c("results_2x"))
}
#'
#'
#'
#'

# Modified from http://shofuj.blogspot.com/2016/07/distance-traveled-by-nba-teams-in-2015.html.
calc_dist <-
  function(lng_1,
           lat_1,
           lng_2,
           lat_2,
           R = 3959) {
    results_cleaned <- acos(
      sin(lat_1 * pi / 180) * sin(lat_2 * pi / 180) +
        cos(lat_1 * pi / 180) * cos(lat_2 * pi / 180) * cos(lng_2 *
                                                              pi / 180 - lng_1 * pi / 180)
    ) * R
    results_cleaned
  }

# The temporary row number column is used to prevent calculation with the first game.
results_with_dist <-
  results_selected %>%
  group_by(season, tm_1) %>%
  arrange(date) %>%
  mutate(rn = row_number()) %>%
  mutate(dist = ifelse(rn == 1, 0,
                       ifelse(
                         lag(tm_1) == lag(tm_home),
                         ifelse(tm_1 == tm_home,
                                0,
                                calc_dist(lag(lng_1), lag(lat_1), lng_2, lat_2)),
                         ifelse(tm_1 == tm_home,
                                calc_dist(lag(lng_2), lag(lat_2), lng_1, lat_1),
                                0)
                       ))) %>%
  select(-rn) %>%
  ungroup()

#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  rm(list = c("results_selected"))
}
#'
#'
#'
#+ include = FALSE
# Debugging...
results_with_dist %>% 
  filter(tm_1 == "SAS") %>% 
  select(date, season, tm_1, tm_2, tm_home, lng_1, lat_1, lng_2, lat_2, dist)
#'
#'
#'
results_with_dist <-
  results_with_dist %>% 
  select(-lng_1, -lat_1, -lng_2, -lat_2)

#'
#'
#'
determine_tm_winner <- function(val_1, val_2, tm_1, tm_2) {
  ifelse(val_1 > val_2, tm_1,
         ifelse(val_1 < val_2, tm_2, as.character(NA)))
}

determine_result <- function(tm_1, tm_2, tm_match) {
  ifelse(tm_1 == tm_match, "W",
         ifelse(tm_2 == tm_match, "L", as.character(NA)))
}

determine_w <- function(tm_1, tm_2, tm_match) {
  ifelse(tm_1 == tm_match, 1,
         ifelse(tm_2 == tm_match, 0, as.integer(NA)))
}

results_cumcalcs <-
  results_with_dist %>%
  # mutate(pd_1 = pts_1 - pts_2, pd_2 = pts_2 - pts_1) %>% 
  mutate(tm_winner = determine_tm_winner(pts_1, pts_2, tm_1, tm_2)) %>% 
  select(-result) %>% 
  mutate(result = determine_result(tm_1, tm_2, tm_winner)) %>% 
  select(date, season, tm_home, tm_away, pts_home, pts_away, tm_winner, everything()) %>% 
  mutate(
    g = 1,
    hfa = ifelse(tm_1 == tm_home, 1, 0),
    w = determine_w(tm_1, tm_2, tm_winner)
  ) %>%
  group_by(season, tm_1) %>%
  mutate(days_off = as.numeric(date - lag(date))) %>% 
  mutate(days_off = ifelse(is.na(days_off), 0, days_off)) %>% 
  mutate(
    gtd = cumsum(g),
    wtd = cumsum(!is.na(w) & w == 1),
    ltd = cumsum(!is.na(w) & w == 0),
    ttd = cumsum(is.na(w)),
    pftd = cummean(pts_1),
    patd = cummean(pts_2)
  ) %>%
  mutate(
    pdtd = (pftd - patd),
    wptd = wtd / gtd
  ) %>%
  mutate(disttd = cumsum(dist)) %>% 
  mutate(
    ortgtd = cummean(ortg),
    tspcttd = cummean(tspct)
  ) %>%
  mutate(
    `2ptd` = cummean(`2p`),
    `3ptd` = cummean(`3p`),
    fttd = cummean(ft)
  ) %>%
  ungroup()

#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  rm(list = c("results_with_dist"))
}

#'
#'
#'
#+ include = FALSE
# Debugging... 
results_cumcalcs %>% 
  summarise(days_off_avg = mean(days_off),
            dist_avg = mean(dist))
# Debugging... These should be equal.
results_cumcalcs %>% group_by(result) %>% count()
results_cumcalcs %>% group_by(hfa) %>% count()
results_cumcalcs %>%
  filter(tm_1 == "SAS") %>%
  head()

results_cumcalcs %>%
  filter(tm_1 == "SAS") %>%
  tail()
#'
#'
#'
colnames_join <- c("date", "tm_1")
colnames_vars <-
  c("pd",
    "dist",
    "days_off",
    "ortg",
    "tspct",
    "wptd",
    "pdtd",
    "disttd",
    "ortgtd",
    "tspcttd",
    "2ptd",
    "3ptd",
    "fttd"
    )

results_cumcalcs_2sided <-
  results_cumcalcs %>%
  inner_join(
    results_cumcalcs %>%
      select(one_of(c(colnames_join, colnames_vars))) %>% 
      rename_all(funs(str_c(., "_opp"))),
    by = c("date" = "date_opp", "tm_2" = "tm_1_opp")
    )
glimpse(results_cumcalcs_2sided)
#'
#'
#'
# Lag td variables to make make them appropriate for prediction.
colnames_lag1 <-
  names(results_cumcalcs_2sided) %>%
  str_subset("td$|_opp$")
colnames_lag1

lag1_0 <- function(x) lag(x, default = 0)
results_cumcalcs_lag1 <-
  results_cumcalcs_2sided %>%
  group_by(season, tm_1) %>%
  mutate_at(vars(colnames_lag1), funs(lag1 = lag1_0)) %>%
  ungroup()
#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  # rm(list = c("results_cumcalcs", "results_cumcalcs_2sided"))
}
#'
#'
#'
output <- results_cumcalcs_lag1
#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  # rm(list = c("results_cumcalcs_lag1""))
}
#'
#'
#'
if(export == TRUE) {
  write_csv(output, filepath_export)
}
#'
#'
#'
