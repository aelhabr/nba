


#'
#'
#'
#+ include = FALSE
rm(list = ls())


#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
library("readr")
library("ggplot2")
library("lubridate")
library("tidyr")
theme_set(theme_minimal())
# theme_set(hrbrthemes::theme_ipsum_rc())

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
# export <- TRUE
remove_tempvars <- TRUE

# if (export == TRUE) {
#   filename_export_base <- filename_import_base
#   filename_export_suffix <- "-predict"
#   filename_export_ext <- ".csv"
#   dir_export <- "data/"
#   filepath_export <-
#     str_c(dir_export,
#           filename_export_base,
#           filename_export_suffix,
#           filename_export_ext)
# }

#'
#'
#'
# Import. ----
results_prepared <- read_csv(filepath_import)

#'
#'
#'
# results_calendar ----
# colnames_base <- results_prepared_colnames %>% filter(type == "base")
colnames_base <- c("date", "season", "tm")
colnames_calc_dates <-
  c("yyyy", "mm", "dd", "wd", "mm_yyyy", "mm_w")
# Look at a couple of different metrics.
colnames_viz <- c("g_td", "pd")

results_calendar_tm <-
  results_prepared %>%
  filter(tm == "SAS") %>%
  mutate(
    yyyy = year(date),
    mm = month(date),
    dd = day(date),
    wd = wday(date, label = TRUE, abbr = TRUE),
    mm_yyyy = zoo::as.yearmon(date)
  ) %>%
  group_by(mm_yyyy) %>%
  mutate(mm_w = ceiling(dd / 7)) %>%
  ungroup() %>%
  select(one_of(colnames_base, colnames_calc_dates, colnames_viz)) %>%
  arrange(season, g_td, tm)
results_calendar_tm

# Tidy up because I was experimenting with different metrics, not just point differential.
results_calendar_tm_tidy <-
  results_calendar_tm %>%
  # mutate_if(is.character, funs(as.factor)) %>%
  gather(metric, value, colnames_viz)

season <- 2016
wd_labels <- levels(results_calendar_tm$wd)
wd_labels[2:6] <- ""
title <- str_c("San Antonio Spurs Point Differential in ", season, " NBA Season")

results_calendar_tm_tidy %>%
  filter(season %in% seasons) %>%
  filter(metric == "pd") %>%
  ggplot() +
  geom_tile(aes(x = wd, y = mm_w, fill = value), colour = "white") +
  scale_y_reverse() +
  scale_x_discrete(labels = wd_labels) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_gradient2(low = "red", mid = "grey", high = "green") +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", title = title) +
  facet_wrap( ~ mm_yyyy, nrow = 2)

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(
    list = c(
      "colnames_base",
      "colnames_calc_dates",
      "colnames_viz",
      "results_calendar_tm",
      "results_calendar_tm_tidy",
      "seeasons",
      "wd_labels"
    )
  )
}

#'
#'
#'


