
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
library("lubridate")
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
  str_c(dir_import,
        filename_import_base,
        filename_import_suffix,
        filename_import_ext)

#'
#'
#'
#+ include = FALSE
export <- TRUE
remove_tempvars <- TRUE
add_roll <- FALSE

if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "-prepared"
  filename_export_ext <- ".csv"
  dir_export <- "data/"
  filepath_export <-
    str_c(dir_export,
          filename_export_base,
          filename_export_suffix,
          filename_export_ext)
  filepath_export_eoy <-
    str_c(
      dir_export,
      filename_export_base,
      str_c(filename_export_suffix, "-eoy"),
      filename_export_ext
    )
}

if (add_roll == TRUE) {
  m <- 10
  library("zoo")
}

#'
#'
#'
# Import. ----
results_cleaned <- read_csv(filepath_import)
tms_geo <- read_csv(filepath_locations)

#'
#' Add a couple of basic, useful calculated values.
#' Also, add location data.
#'
# Prepare. ----
results_base <-
  results_cleaned %>%
  mutate(season = if_else(month(date) >= 10, year(date), year(date) - 1)) %>% 
  mutate(mp_home = mp, mp_away = mp) %>%
  select(-mp) %>% 
  mutate(pd_h2a = pts_home - pts_away) %>%
  mutate(pts_total = pts_home + pts_away) 

results_geo <-
  results_base %>%
  inner_join(tms_geo %>% select(tm, lat, lng) %>% rename_all(funs(str_c(., "_home"))),
             by = c("tm_home")) %>%
  inner_join(tms_geo %>% select(tm, lat, lng) %>% rename_all(funs(str_c(., "_away"))),
             by = c("tm_away"))

#'
#'
#'
# Reordering...
results_geo <-
  results_geo %>% 
  select(date,
         season,
         tm_home,
         tm_away,
         pts_home,
         pts_away,
         pd_h2a,
         pts_total,
         everything())
results_geo
#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("results_cleaned", "tms_geo", "results_base"))
}
#'
#' These columns are renamed simply to identify them as single game statistics.
#'
# Add symmetric variables for minutes played.
colnames_homeaway <-
  names(results_geo) %>%
  str_subset("_(home|away)$") %>%
  str_subset("^(?!pts_|tm_).*")
colnames_homeaway

results_renamed <-
  results_geo %>%
  rename_at(vars(colnames_homeaway), funs(str_c(., "_1g")))

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("results_geo"))
}
#'
#' Prepare, then join, data upon itself for "duplicated" games.
#' This is done so that "a" versions of calculations are correct for each individual
#' team. If data is not joined upon itself in this manner, then calculations
#' would only be correct for home teams. "a" and "b" suffixes are used as
#' temporary identifiers. (These are to be replaced by "off" and "def" later.)
#'
# Explicitly distinguish team and points columns to use for future calculations.
results_side1 <-
  results_renamed %>%
  rename_at(vars(contains("_home_1g")), funs(str_replace_all(., "_home_", "_off_"))) %>%
  rename_at(vars(contains("_away_1g")), funs(str_replace_all(., "_away_", "_def_"))) %>%
  mutate(tm = tm_home,
         tm_opp = tm_away,
         pts = pts_home,
         pts_opp = pts_away)

results_side2 <-
  results_renamed %>%
  rename_at(vars(contains("_home_1g")), funs(str_replace_all(., "_home_", "_def_"))) %>%
  rename_at(vars(contains("_away_1g")), funs(str_replace_all(., "_away_", "_off_"))) %>%
  mutate(tm = tm_away,
         tm_opp = tm_home,
         pts = pts_away,
         pts_opp = pts_home)

results_2x <-
  results_side1 %>%
  bind_rows(results_side2)

#'
#'
#'
# Reordering...
results_2x <-
  results_2x %>% 
  select(
    date,
    season,
    tm_home,
    tm_away,
    pts_home,
    pts_away,
    pd_h2a,
    pts_total,
    tm,
    tm_opp,
    pts,
    pts_opp,
    everything()
  ) %>%
  arrange(date, season, tm_home, tm_away, tm)
results_2x

#'
#'
#'
# Debugging...
results_2x %>% 
  group_by(season) %>% 
  count()

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("results_renamed"))
  rm(list = c("results_side1", "results_side2"))
}

#'
#' Calculate distance traveled. (This could have been done earlier.)
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
  results_2x %>%
  group_by(season, tm) %>%
  arrange(date, season, tm_home, tm_away, tm) %>%
  mutate(rn = row_number()) %>%
  mutate(dist = ifelse(rn == 1, 0,
                       ifelse(
                         lag(tm) == lag(tm_home),
                         ifelse(tm == tm_home,
                                0,
                                calc_dist(
                                  lag(lng_off_1g), lag(lat_off_1g), lng_def_1g, lat_def_1g
                                )),
                         ifelse(tm == tm_home,
                                calc_dist(
                                  lag(lng_def_1g), lag(lat_def_1g), lng_off_1g, lat_off_1g
                                ),
                                0)
                       ))) %>%
  select(-rn) %>%
  ungroup()

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("results_2X"))
}
#'
#'
#'
#+ include = FALSE
# Debugging...
results_with_dist %>%
  filter(tm == "SAS") %>%
  select(date,
         season,
         tm,
         tm_opp,
         tm_home,
         lng_off_1g,
         lat_off_1g,
         lng_def_1g,
         lat_def_1g,
         dist)
#'
#'
#'
results_with_dist <-
  results_with_dist %>%
  select(-lng_off_1g,-lat_off_1g,-lng_def_1g,-lat_def_1g)

#'
#'
#'
# Could technically just use point differential for this first function.
# determine_tm_winner <- function(val_1, val_2, tm_1, tm_2) {
#   ifelse(val_1 > val_2, tm_1,
#          ifelse(val_1 < val_2, tm_2, as.character(NA)))
# }

determine_tm_winner <- function(pd_h2a, tm_1, tm_2, tm_home) {
  ifelse(pd_h2a > 0, ifelse(tm_1 == tm_home, tm_1, tm_2), ifelse(tm_1 == tm_home, tm_2, tm_1))
}

determine_result <- function(tm_1, tm_2, tm_match) {
  ifelse(tm_1 == tm_match,
         "W",
         ifelse(tm_2 == tm_match, "L", as.character(NA)))
}

determine_w <- function(tm_1, tm_2, tm_match) {
  ifelse(tm_1 == tm_match, 1,
         ifelse(tm_2 == tm_match, 0, as.integer(NA)))
}

results_cumcalcs <-
  results_with_dist %>%
  # mutate(tm_winner = determine_tm_winner(pts, pts_opp, tm, tm_opp)) %>%
  mutate(tm_winner = determine_tm_winner(pd_h2a, tm, tm_opp, tm_home)) %>% 
  select(-result) %>%
  mutate(result = determine_result(tm, tm_opp, tm_winner)) %>%
  mutate(
    g = 1,
    hfa = ifelse(tm == tm_home, 1, 0),
    w = determine_w(tm, tm_opp, tm_winner)
  ) %>%
  group_by(season, tm) %>%
  mutate(drest = as.numeric(date - lag(date))) %>%
  mutate(drest = ifelse(is.na(drest), 0, drest)) %>%
  mutate(
    gtd = cumsum(g),
    wtd = cumsum(!is.na(w) & w == 1),
    ltd = cumsum(!is.na(w) & w == 0),
    ttd = cumsum(is.na(w)),
    pftd = cummean(pts),
    patd = cummean(pts_opp)
  ) %>%
  select(-pts, -pts_opp) %>% 
  mutate(pdtd = (pftd - patd),
         wptd = (wtd / gtd)) %>%
  mutate(disttd = cumsum(dist)) %>%
  ungroup()
#'
#' Rename variables such that "one game" suffix is eliminated from
#' variables that are calculated as "to date" sums/averages.
#'
colnames_1g <-
  names(results_cumcalcs) %>%
  str_subset("_1g$") %>% 
  str_subset("^(?!tm_).*")

results_cumcalcs_temp <-
  results_cumcalcs %>%
  group_by(season, tm) %>%
  mutate_at(vars(colnames_1g), funs(td = cummean)) %>%
  ungroup() %>%
  rename_at(vars(ends_with("_1g_td")), funs(str_replace_all(., "_1g_td", "_td")))
glimpse(results_cumcalcs_temp)

results_cumcalcs <- results_cumcalcs_temp
#'
#' Now rename "a" and "b" to "off" and "def".
#'
colnames_calc_manual <-
  c(
    "dist",
    "tm_winner",
    "result",
    "g",
    "hfa",
    "w",
    "drest",
    "gtd",
    "wtd",
    "ltd",
    "ttd",
    "pftd",
    "patd",
    "pdtd",
    "wptd",
    "disttd"
  )

results_cumcalcs_renamed <-
  results_cumcalcs %>%
  rename_at(vars(colnames_calc_manual), funs(str_c(., "_off_1g"))) %>%
  rename_at(vars(ends_with("td_off_1g")), funs(str_replace_all(., "td_off_1g$", "_off_td")))

#'
#' Prepare the "end of year" statistical ouptut. This is exported separately
#' from the data prepared explicitly for modeling.
#'
colnames_td <-
  names(results_cumcalcs_renamed) %>%
  str_subset("_td$")

colnames_calc_manual_off_td <-
  colnames_calc_manual %>%
  str_c("_off_1g") %>%
  str_replace_all("td_off_1g$", "_off_td") %>%
  str_subset("_off_td$")
colnames_calc_manual_off_td

# Debugging...
results_cumcalcs_renamed %>%
  group_by(season) %>%
  count()

results_cumcalcs_eoy <-
  results_cumcalcs_renamed %>%
  group_by(season, tm) %>%
  filter(g_off_td == max(g_off_td)) %>%
  select(one_of(c("season", "tm", colnames_td))) %>%
  ungroup() %>%
  rename(tm = tm) %>%
  rename_at(vars(colnames_calc_manual_off_td), funs(str_replace_all(., "_off", ""))) %>%
  rename_at(vars(ends_with("_td")), funs(str_replace_all(., "_td$", "")))
results_cumcalcs_eoy
#'
#'
#'
#+ include = FALSE
if (export == TRUE) {
  write_csv(results_cumcalcs_eoy, filepath_export_eoy)
}

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("results_with_dist"))
  rm(list = c("results_cumcalcs_temp"))
  rm(list = c("results_cumcalcs"))
}
#'
#' Need to be careful with renaming opponent statistics when joining.
#'
colnames_join <- c("date", "tm_home", "tm_away", "tm")
colnames_vars_join <-
  c(
    str_c(
    grep(
      "td$",
      colnames_calc_manual,
      invert = TRUE,
      value = TRUE
    ), "_off"),
    names(results_cumcalcs_renamed) %>%
      str_subset("_(off|def)_td$")
  )
colnames_vars_join

results_cumcalcs_2sided <-
  results_cumcalcs_renamed %>%
  inner_join(
    results_cumcalcs_renamed %>%
      select(one_of(c(
        colnames_join, colnames_vars_join
      ))) %>%
      rename_at(vars(ends_with("_off_1g")), funs(
        str_replace_all(., "_off_1g$", "_opp_off_1g")
      )) %>% 
      rename_at(vars(ends_with("_off_td")), funs(
        str_replace_all(., "_off_td$", "_opp_off_td")
      )) %>%
      rename_at(vars(ends_with("_def_td")), funs(
        str_replace_all(., "_def_td$", "_opp_def_td")
      )),
    # # rename_at(vars(colnames_calc_manual), funs(str_c(., "_b"))) %>%
    # rename_all(funs(str_replace_all(., "_b$", "_opp"))),
    by = c("date" = "date", "tm_home", "tm_away", "tm_opp" = "tm")
  )
glimpse(results_cumcalcs_2sided)
#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("results_cumcalcs_renamed"))
}
#'
#'
#'
# Lag td variables to make make them appropriate for prediction.
colnames_mutate <-
  names(results_cumcalcs_2sided) %>%
  str_subset("_td$")
colnames_mutate

lag1_0 <- function(x) {
  lag(x, default = 0)
}

results_cumcalcs_lag1 <-
  results_cumcalcs_2sided %>%
  group_by(season, tm) %>%
  mutate_at(vars(colnames_mutate), funs(lag1_0)) %>%
  # mutate_at(vars(colnames_mutate), funs(lag1 = lag1_0)) %>%
  ungroup()

if (add_roll == TRUE) {
  results_cumcalcs_lag1 <-
    results_cumcalcs_lag1 %>%
    group_by(season, tm) %>%
    mutate_at(
      vars(colnames_mutate),
      # vars(str_c(colnames_mutate, "_lag1")),
      funs(lastn = rollapplyr),
      width = n,
      FUN = mean,
      na.rm = TRUE,
      partial = TRUE
    ) %>%
    ungroup()
}
glimpse(results_cumcalcs_lag1)

#+ include = FALSE
if (remove_tempvars == TRUE) {
  # rm(list = c("results_cumcalcs", "results_cumcalcs_2sided"))
}
#'
#'
#'
output <- results_cumcalcs_lag1

colnames_off_1g <-
  names(output) %>% 
  str_subset("_off_1g$")
colnames_off_1g

#  #  Could do som eadditional sorting.
# sort(colnames_off_1g)
# c("mp_off_1g", colnames_off_1g[!(colnames_off_1g == "mp_off_1g")])

colnames_def_1g <-
  names(output) %>% 
  str_subset("_def_1g$")
colnames_def_1g
sort(colnames_def_1g)

colnames_off_td <-
  names(output) %>% 
  str_subset("_off_td$") %>% 
  grep("opp", ., invert = TRUE, value = TRUE)
colnames_off_td

colnames_def_td <-
  names(output) %>% 
  str_subset("_def_td$") %>% 
  grep("opp", ., invert = TRUE, value = TRUE)
colnames_def_td

colnames_opp_off_td <-
  names(output) %>% 
  str_subset("_opp_off_td$")
colnames_opp_off_td

colnames_opp_def_td <-
  names(output) %>% 
  str_subset("_opp_def_td$")
colnames_opp_def_td

colnames_stats <-
  c(
    colnames_off_1g,
    colnames_def_1g,
    colnames_off_td,
    colnames_def_td,
    colnames_opp_off_td,
    colnames_opp_def_td
  )

colnames_base <- setdiff(names(output), colnames_stats)

output <-
  output %>% 
  select(one_of(c(colnames_base, colnames_stats)))

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  # rm(list = c("results_cumcalcs_lag1""))
}
#'
#'
#'
if (export == TRUE) {
  write_csv(output, filepath_export)
}
#'
#'
#'
