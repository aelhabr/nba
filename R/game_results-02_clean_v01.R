
rm(list = ls())
setwd("O:/_other/projects/nba/")

# Packages. ----
library("dplyr")
library("stringr")
library("readr")
library("tidyr")

# Parameters. ----
filename_import_base <- "game_results"
filename_import_suffix <- "-scraped"
filename_import_ext <- ".csv"
dir_import <- "data/"

filepath_import <-
  str_c(dir_import, filename_import_base, filename_import_suffix, filename_import_ext)

export <- TRUE

if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "-cleaned"
  filename_export_ext <- filename_import_ext
  dir_export <- dir_import
  filepath_export <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      filename_export_ext
    )
}

# Clean. ----

d <- read_csv(filepath_import)
# d2 <- read.csv(filepath_import, stringsAsFactors = FALSE)
# setdiff(names(d), names(d2))
# rm(d2)

# X is the "@" column.
# cols_fix <- names(d) %>% str_which("^X")
# cols_fix

# Filtering to only game results associated with home team.
d_1 <-
  d %>%
  select(-rk) %>% 
  rename(X = X4) %>%
  # select(date, X, tm, opp, result) %>%
  filter(is.na(X)) %>% 
  mutate(
    # tm_1 = tm,
    # tm_2 = opp,
    tm_home = if_else(is.na(X), tm, opp),
    tm_away = if_else(is.na(X), opp, tm)
  ) %>%
  select(-tm, -opp, -X) %>% 
  select(date, tm_home, tm_away, result, everything())
d_1

d_1 %>% distinct()

# 'Too many values' warning is due to '(OT)'. This is not an issue.
# Points columns are removed because they actually already exist.
d_2 <-
  d_1 %>% 
  separate(result, c("result", "pts_home", "pts_away")) %>% 
  select(-pts_home, -pts_away)

colnames_home <-
  names(d_2) %>% 
  str_subset("_1$") %>% 
  str_replace_all("_1$", "")
colnames_home

names(d_2) <-
  names(d_2) %>% 
  str_replace_all("_2$", "_diff") %>% 
  str_replace_all("_1$", "_away")
names(d_2)[names(d_2) %in% colnames_home] <- str_c(colnames_home, "_home")
names(d_2)

# colnames_nonnumeric <- c("date", "result", "tm_home", "tm_away")
d_3 <-
  d_2 %>%
  # mutate_at(vars(-colnames_nonnumeric), funs(as.numeric))
  mutate_at(vars(-date, -result, -tm_home, -tm_away), funs(as.numeric))

output <-
  d_3 %>% 
  select(date, tm_home, tm_away, result, pts_home, pts_away, everything())
output

if (export == TRUE) {
  write_csv(output, filepath_export)
}

