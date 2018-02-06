
rm(list = ls())
setwd("O:/_other/projects/nba/")

# Packages. ----
library("dplyr")
library("stringr")
library("readr")

# Parameters. ----
filename_import_base <- "sched"
filename_import_suffix <- "_scraped"
filename_import_ext <- ".csv"
dir_import <- "data/"

filepath_import <-
  str_c(dir_import, filename_import_base, filename_import_suffix, filename_import_ext)

export <- TRUE

if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "_cleaned"
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

# Find most recent scraped file. ----

# filename_import <- str_c(filename_base, "_scraped")
# filenames_import <- str_subset(list.files(dir_import, full.names = TRUE), filename_import)
# filenames_import
# file_info <- file.info(filenames_import) %>% tibble::rownames_to_column(var = "filename")
# filepath_import <- file_info %>% arrange(mtime) %>% slice(1) %>% pull(filename)
# # sched <- read.csv(filepath_import, stringsAsFactors = FALSE) %>% as_tibble()
# sched <- read_csv(filepath_import)

# Clean team names. ----
imported <- read_csv(filepath_import)
idx_cols_tms <- str_which(names(imported), c("tm"))

imported %>% distinct(tm_home) %>% arrange(tm_home) %>% pull(tm_home)

output <- imported
output[idx_cols_tms] <-
  lapply(output[idx_cols_tms],
         function(d) {
           d %>%
             str_replace_all("Los Angeles Lakers", "LAL") %>%
             str_replace_all("Los Angeles Clippers", "LAC") %>%
             str_replace_all("New York Knicks", "NY") %>%
             str_replace_all("New Orleans Pelicans", "NO")
         })
output <-
  output %>%
  mutate_at(vars(tm_home, tm_away), funs(str_to_upper(str_sub(., 1, 3))))

output[idx_cols_tms] <-
  lapply(output[idx_cols_tms],
         function(d) {
           d %>%
             str_replace_all("BRO", "BKN") %>%
             str_replace_all("GOL", "GS") %>%
             str_replace_all("BRO", "BKN") %>%
             str_replace_all("GOL", "GS") %>%
             str_replace_all("OKL", "OKC") %>%
             str_replace_all("PHO", "PHX") %>%
             str_replace_all("SAN", "SA") %>%
             str_replace_all("UTA", "UTAH")
         })
output %>% distinct(tm_home) %>% arrange(tm_home) %>% pull(tm_home)


if (export == TRUE) {
  write_csv(output, filepath_export)
}
