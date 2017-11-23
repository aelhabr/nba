
# TODO: Move variables that are only necessary for certain scripts/functions to their corresponding .R files.  remove(list = ls())

# Import libraries. ####
library(rvest)  # for read_* functions
library(tidyverse)  # Use primarily for dplyr.
library(stringr)  # Use for str_* functions.
library(readxl)  # Use for read_excel; don't sue read.xlsx().

if (substr(getwd(), 1, 1) == "C") {
    library(openxlsx)  # no Java requirement; recommended for tidyverse
} else if (substr(getwd(), 1, 1) == "O") {
    library(xlsx)  # Java requirement
}

# This function is only necessary for the get_stat_tbl function.  Needed to choose correct xlsx package when working from home/work.
# Function is used to set xlsx_package boolean, which is used in other functions.
check_xlsx_package <- function() {
    if ("xlsx" %in% (.packages())) {
        xlsx_package <- "xlsx"
    }
    if ("openxlsx" %in% (.packages())) {
        xlsx_package <- "openxlsx"
    }
    return(xlsx_package)
}

xlsx_package <- check_xlsx_package()

# Not needed anymore.  library(rio) library(pipeR)

# Set globals. #### generic_error_string <- 'Required input is NA.  Function aborted.'

if (substr(getwd(), 1, 1) == "C") {
    dir_nba <- "C:/Users/aelhabr/Dropbox/data_science/projects/nba/"
} else if (substr(getwd(), 1, 1) == "O") {
    dir_nba <- "O:/_other/code/tony/nba/"
}

setwd(paste0(dir_nba, "tr/"))
dir_data <- paste0(dir_nba, "tr/data/")

# yrs <- seq(2007, 2016)

# g_teams <- try(readRDS(paste0(dir_data, 'g_teams.rds')))
# d_win_totals <- try(readRDS(paste0(dir_data, 'd_win_totals.rds')))
# p_win_totals <- try(readRDS(paste0(dir_data, 'p_win_totals.rds')))
# tbl_stat_links <- try(readRDS(paste0(dir_data, 'tbl_stat_links.rds')))
