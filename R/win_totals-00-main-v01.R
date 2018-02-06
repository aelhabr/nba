

rm(list = ls())
setwd("O:/_other/projects/nba/")
dir_scrape <- "data/scraped/"
filename_base <- "sched"
export <- FALSE

source("win_totals-01_scrape_sched.R")
source("win_totals-02_clean_sched.R")