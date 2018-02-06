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
# library("lubridate")
#'
#'
#'
# Parameters. ----
filename_import_base <- "bigfour"
filename_import_suffix <- "-imported"
filename_import_ext <- ".csv"
dir_import <- "data/"
filepath_import <-
  str_c(dir_import,
        filename_import_base,
        filename_import_suffix,
        filename_import_ext)
filepath_import_results <- "data/game_results-cleaned.csv"

export <- TRUE
if (export == TRUE) {
  filename_export_base <- "bigfour"
  filename_export_suffix <- "-cleaned"
  filename_export_ext <- ".csv"
  dir_export <- "data/"
  filepath_export <-
    str_c(dir_export,
          filename_export_base,
          filename_export_suffix,
          filename_export_ext)
}
#'
#'
#'
# Import. ----
lines <- readr::read_csv(filepath_import)
results_cleaned <- readr::read_csv(filepath_import_results)

#'
#' Add a couple of basic, useful calculated values.
#' Also, add location data.
#'
# Prepare. ----
results_base <-
  results_cleaned %>%
  mutate(season = if_else(lubridate::month(date) >= 10, lubridate::year(date), lubridate::year(date) - 1))

#'
#'
#'
#+ include = FALSE
results_lines_bad <-
  results_base %>%
  left_join(
    lines %>% rename(tm_home_lines = tm_home, tm_away_lines = tm_away),
    by = c("date", "season", "pts_home", "pts_away")
  )


# There are some days with the same final scores for two games.
# This is the problem with just joining on points.
# Must join on team names as well.
results_lines_bad %>%
  group_by(season, date, pts_home, pts_away) %>%
  mutate(pts_ha = as.numeric(str_c(pts_home, ".", pts_away))) %>%
  mutate(rank_max = rank(pts_ha, ties.method = "max")) %>%
  ungroup() %>%
  select(rank_max, everything()) %>%
  filter(rank_max > 1)

rm("results_lines_bad")

results_base %>%
  filter(!(season %in% c(2004, 2016))) %>%
  left_join(
    lines %>% rename(tm_home_lines = tm_home, tm_away_lines = tm_away),
    by = c("date", "season", "pts_home", "pts_away")
  )

results_base %>%
  filter(tm_home %in% c("NJN", "BRK", "CHA", "CHO", "NOH", "NOK", "NOP", "SEA", "OKC")) %>%
  filter(tm_home %in% c("CHA", "CHO", "NOH", "NOK", "NOP")) %>%
  group_by(season, tm_home) %>%
  count() %>%
  select(-n) %>%
  group_by(tm_home, season) %>%
  count() %>%
  tidyr::spread(season, n)

#'
#'
#'

lines_edited <-
  lines %>%
  mutate_at(vars(tm_home, tm_away), funs(ifelse(season <= 2011, ifelse(. == "BRK", "NJN", .), .))) %>%
  mutate_at(vars(tm_home, tm_away), funs(ifelse(season <= 2013, ifelse(. == "CHO", "CHA", .), .))) %>%
  mutate_at(vars(tm_home, tm_away), funs(ifelse(season <= 2004, ifelse(. == "NOP", "NOH", .), .))) %>%
  mutate_at(vars(tm_home, tm_away), funs(ifelse(season >= 2005 & season <= 2006, ifelse(. == "NOP", "NOK", .), .))) %>%
  mutate_at(vars(tm_home, tm_away), funs(ifelse(season >= 2007 & season <= 2012, ifelse(. == "NOP", "NOH", .), .))) %>%
  mutate_at(vars(tm_home, tm_away), funs(ifelse(season <= 2007, ifelse(. == "OKC", "SEA", .), .)))

results_lines_edited <-
  results_base %>%
  left_join(lines_edited %>% select(-pts_home, -pts_away))

#'
#'
#'
#+ include = FALSE
# Debugging...
results_lines_edited %>%
  filter(!(season %in% c(2004, 2016))) %>%
  filter(tm_home %in% c("NJN", "BRK", "CHA", "CHO", "NOH", "NOK", "NOP", "SEA", "OKC")) %>%
  filter(is.na(prob_home)) %>%
  group_by(season, tm_home) %>%
  count()

results_lines_edited %>%
  filter(!(season %in% c(2004, 2016))) %>%
  filter(is.na(prob_home))

results_lines_edited %>% summarise(p_home_mean = mean(prob_home))

#'
#'
#'
if(export == TRUE) {
  readr::write_csv(lines_edited, filepath_export)
}
