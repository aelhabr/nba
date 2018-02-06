
rm(list = ls())
setwd("O:/_other/projects/nba/")

# Packages. ----
library("dplyr")
library("stringr")
library("readr")

# Parameters. ----
filename_import_base <- "sched"
filename_import_suffix <- "_cleaned"
filename_import_ext <- ".csv"
dir_import <- "data/"

filepath_import <-
  str_c(dir_import, filename_import_base, filename_import_suffix, filename_import_ext)

export <- TRUE

if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "_prepared"
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

# Prepare data set for modeling. ----
imported <- read_csv(filepath_import)
rbind_prep_1 <-
	imported %>%
	mutate(# tm_sched = tm_home,
				 tm_1 = tm_home,
				 tm_2 = tm_away)

rbind_prep_2 <-
	imported %>%
	mutate(# tm_sched = tm_away,
				 tm_1 = tm_away,
				 tm_2 = tm_home)

rbind_prep <-
	rbind_prep_1 %>%
	bind_rows(rbind_prep_2) %>%
	arrange(date, tm_home, tm_away)

rm(list = c("rbind_prep_1", "rbind_prep_2"))

determine_winner <- function(x_1, x_2, x_match) {
	if_else(x_1 == x_match, 1,
				 ifelse(x_2 == x_match, 0, as.integer(NA)))
}

cols_group <- c("season", "tm_1")

...

# library("readxl")
# filepath_db <- "data/db_nba.xlsm"
# excel_sheets(filepath_db)
# picks_all <- read_excel(filepath_db, sheet = "nba_win_total_picks")
# 
# library("tidyr")
# picks <-
# 	picks_all %>%
# 	separate(win_total_results_name, c("season", "tm")) %>%
# 	mutate(season = as.numeric(season)) %>%
# 	mutate_at(vars(confidence), funs(as.numeric)) %>%
# 	filter(season == 2017) %>%
# 	filter(person == "t") %>%
# 	group_by(person, season)
# tail(picks)
# 





