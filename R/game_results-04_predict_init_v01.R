

# TODO: THIS SCRIPT NEEDS TO BE UPDATED. FOLLOW EXPERIMENT TEMPLATE!
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
library("ggplot2")
seed <- 42
theme_set(theme_minimal())

#'
#'
#'
# Parameters. ----
filename_import_base <- "game_results"
filename_import_suffix <- "-prepared"
filename_import_ext <- ".csv"
dir_import <- "data/"
filepath_import <-
  str_c(dir_import,
        filename_import_base,
        filename_import_suffix,
        filename_import_ext)
filepath_import_colnames <-
  str_c(
    dir_import,
    filename_import_base,
    filename_import_suffix,
    "-colnames",
    filename_import_ext
  )

#'
#'
#'
#+ include = FALSE
export <- TRUE
export_rdata <- TRUE
remove_tempvars <- TRUE

if (export_rdata == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "-predict_init"
  filename_export_ext_rdata <- ".RData"
  dir_export <- "data/"
  filepath_export_rdata <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      filename_export_ext_rdata
    )
}
if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "-predict"
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
results_prepared <- read_csv(filepath_import)
results_prepared_colnames <- read_csv(filepath_import_colnames)

#'
#'
#'
# colnames_xy_# ----
names(results_prepared)

colname_y_1rgs <- "pd_h2a"
colname_y_2rgs <- colname_y_1rgs
colname_y_1cls <- "result"
colname_y_2cls <- colname_y_1cls

colnames_extra <- c("season", "pd_h2a", "result", "tm_home", "tm_away", "pts_home", "pts_away")

colnames_x_1_noopp <-
  c("pd_td",
    "ortg_off_td",
    "ortg_def_td",
    # "dist",
    # "g_last7",
    "dist_last7",
    "drest",
    # "drest_1rgscls",
    # "b2b",
    # "no2drestin4",
    # "no2drestin5",
    "rtrip")

colnames_x_1_opp <-
  colnames_x_1_noopp %>%
  str_c("_opp")

colnames_x_1 <- c(colnames_x_1_noopp, colnames_x_1_opp)
# colnames_x_1rgs <- c(colnames_x_1_noopp)
colnames_x_1

colnames_x_2 <-
  results_prepared_colnames %>%
  # filter(type %in% c("calc", "calc_opp")) %>%
  filter(type %in% c("off_td", "def_td", "off_td_opp", "def_td_opp")) %>%
  pull(value)
colnames_x_2

#'
#'
#'
# Debugging...
# This should return nothing.
setdiff(colnames_x_1, names(results_prepared))
# # Or...
# setdiff(colnames_x_1, results_prepared_colnames$value)
#'
#'
# data_0 ----
data_0 <-
  results_prepared %>%
  # distinct(date, tm_home, tm_away, .keep_all = TRUE) %>%
  # filter((pd > 0 & tm_off == tm_winner) | (pd < 0 & tm_off != tm_winner)) %>%
  # filter(tm == tm_home) %>%
  mutate(temp = str_c(date, tm_home, tm_away)) %>%
  group_by(temp) %>%
  mutate(rn = row_number(temp)) %>%
  ungroup() %>%
  select(rn, temp, everything()) %>%
  filter(rn == 1) %>%
  select(-rn,-temp) %>%
  filter(g_td > 0)

#'
#'
#'
#+ include = FALSE
# Checking to see that there is about an even mix of home and away games.
summary(data_0 %>% select(hfa))

#'
#'
#'
# data_# ----
data_1 <-
  data_0 %>%
  mutate_at(colname_y_1cls, as.factor) %>%
  select(one_of(c(colname_y_1rgs, colname_y_1cls, colnames_extra, colnames_x_1)))

data_2 <-
  data_0 %>%
  select(one_of(c(colname_y_2rgs, colname_y_2cls, colnames_extra, colnames_x_2)))


#'
#'
#'
# fmla_# ----
add_tickmarks <- function(x) {
  ifelse(grepl("^[0-9]", x) == TRUE, paste0("`", x, "`"), x)
}

get_fmla <- function(var_y, vars_x) {
  vars_x <- add_tickmarks(vars_x)
  var_y <- add_tickmarks(var_y)
  paste0(var_y, " ~ ", paste(vars_x, collapse = " + ")) %>%
    as.formula()
}

fmla_1rgs <- get_fmla(colname_y_1rgs, colnames_x_1)
fmla_2rgs <- get_fmla(colname_y_2rgs, colnames_x_2)
fmla_1cls <- get_fmla(colname_y_1cls, colnames_x_1)

#'
#'
#'
# xy_# ----
# Doing this because the predictors and response variable data
# must be treated separately
# in some analysis to be done, and because some functions require
# matrices as inputs.

get_mat_data <- function(d, colnames) {
  d %>%
    select(one_of(colnames)) %>%
    as.matrix() %>%
    na.exclude()
}


seasons_unique <-
  data_0 %>% 
  distinct(season) %>% 
  arrange(season) %>% 
  pull(season)

seasons_tst <- c(first(seasons_unique), last(seasons_unique))
seasons_trn <- setdiff(seasons_unique, seasons_tst)
data_1_trn <- data_1 %>% filter(season %in% seasons_trn)
data_2_trn <- data_1 %>% filter(season %in% seasons_trn)

# There is no need to add the "rgs/cls" suffix to x becasue the predictors
# are the same for each. (Only the response variable is different.)

x_1_trn <- get_mat_data(data_1_trn, colnames_x_1)
y_1rgs_trn <- get_mat_data(data_1_trn, colname_y_1rgs)
y_1cls_trn <- get_mat_data(data_1_trn, colname_y_1cls)
x_2_trn <- get_mat_data(data_2_trn, colnames_x_2)

x_1_tst <- get_mat_data(data_1_trn, colnames_x_1)
y_1rgs_tst <- get_mat_data(data_1_tst, colname_y_1rgs)
y_1cls_tst <- get_mat_data(data_1_tst, colname_y_1cls)
x_2_tst <- get_mat_data(data_2_tst, colnames_x_2)

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("results_prepared"))
  rm(list = c("results_prepared_colnames"))
  rm(list = c("data_0"))
  # rm(list = c("data_1", "data_2"))
}

#'
#'
#+ include = FALSE
if(export_rdata == TRUE) {
  save(file = filepath_export_rdata)
}
#'
#'
#'

