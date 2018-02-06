


#'
#'
#'
#+ include = FALSE
rm(list = ls())

projroot <- rprojroot::find_rstudio_root_file()
# setwd("O:/_other/projects/nba/")
# load("data_filtered/experiment_session.RData")
setwd(projroot)

filepath_import <-
  paste0(projroot, "/R/RData/", "explore-setup.RData")
file.exists(filepath_import)
if (file.exists(filepath_import)) {
  session::restore.session(filepath_import)
} else {

}
source("R/game_results-explore-functions.R")

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
# library("readr")
# library("tidyr")
library("ggplot2")
# seed <- 42
theme_set(theme_minimal())

#'
#'
#'
#+ include = FALSE
remove_tempvars <- FALSE
run_unused_code <- TRUE

seasons_exclude <- c(2004, 2016)
results_prepared_min <-
  results_prepared_min %>%
  filter(!(season %in% seasons_exclude))

season_cutoff <- 2014
seasons_trn <-
  results_prepared_min %>%
  distinct(season) %>%
  filter(season < season_cutoff) %>%
  pull(season)

seasons_tst <-
  results_prepared_min %>%
  distinct(season) %>%
  filter(!(season %in% seasons_trn)) %>%
  pull(season)

#'
#'
#'
if (run_unused_code == TRUE) {
  create_tile_ggplot <-
    function(data = NULL,
             x_char = NULL,
             y_char = NULL,
             var_fill = NULL,
             var_text = NULL,
             var_alpha = NULL,
             x_cutoff = NULL,
             y_cutoff = NULL,
             fill_alpha = FALSE,
             xy_breaks = seq(0, 12, 2)) {
      # data <-
      #   rest_counts %>%
      #   mutate(freq_log10 = log10(freq)) %>%
      #   mutate_at(vars("freq"), funs("freq_char_pct" = paste(sprintf("%.1f", 100 * .), "%")))
      # var_fill <- "freq_log10"
      # var_text <- "freq_pct_char"

      viz <-
        data %>%
        ggplot(aes_string(x = x_char, y = y_char))

      if (fill_alpha == FALSE) {
        viz <-
          viz +
          geom_tile(aes_string(fill = var_fill))
      } else {
        viz <-
          viz + geom_tile(aes_string(fill = var_fill, alpha = var_alpha))
      }

      viz <-
        viz +
        geom_text(aes_string(label = var_text), fontface = "bold") +
        # geom_text(aes_string(label = scales::percent(var_fill)), fontface = "bold") +
        # geom_text(aes_string(label = paste(sprintf(text_fmt, 100 * var_fill), "%")), fontface = "bold") +
        # geom_text(aes_string(label = sprintf("%s \%", sprintf("round(100 * as.numeric(%s), 2)", var_fill))), fontface = "bold") +
        scale_fill_gradient(low = "cyan", high = "red") +
        scale_x_continuous(breaks = xy_breaks) +
        scale_y_continuous(breaks = xy_breaks) +
        theme(
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none"
        )

      viz
      if (!is.null(x_cutoff) & !is.null(y_cutoff)) {
        viz <- viz +
          geom_segment(aes(
            x = x_cutoff + 0.5,
            y = -0.5,
            xend = x_cutoff + 0.5,
            yend = y_cutoff + 0.5
          ),
          size = 2) +
          geom_segment(aes(
            x = -0.5,
            y = y_cutoff + 0.5,
            xend = x_cutoff + 0.5,
            yend = y_cutoff + 0.5
          ),
          size = 2)
      }
      viz
    }

  results_prepared %>%
    group_by(hstand, rtrip_opp) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(freq = n / sum(n)) %>%
    mutate(freq_log10 = log10(freq)) %>%
    mutate_at(vars(starts_with("freq")), funs(pct_char = paste0(sprintf("%.1f", 100 * .), "%"))) %>%
    create_tile_ggplot(
      x_char = "hstand",
      y_char = "rtrip_opp",
      var_fill = "freq_log10",
      var_text = "freq_pct_char"
      # x_cutoff = 3,
      # _cutoff = 3
    ) +
    labs(title = "Frequency of Every Combination of Home Stand Games and Opponent Road Trip Games",
         x = "Length of Home Stand (in Games)",
         y = "Length of Opponent Road Trip (in Games)")

  results_prepared %>%
    group_by(drest, drest_opp) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(freq = n / sum(n)) %>%
    mutate(freq_log10 = log10(freq)) %>%
    mutate_all(funs(pct_char = paste0(sprintf("%.2f", 100 * .), "%"))) %>%
    create_tile_ggplot(
      x_char = "drest",
      y_char = "drest_opp",
      var_fill = "freq",
      var_text = "freq_pct_char"
    ) +
    labs(title = "Frequency of Every Combination of Rest Games",
         x = "Days of Rest",
         y = "Days of Rest for Opponent")
}
#'
#' Parameters for this script.
#'

vars_join <- c("date", "season", "tm", "tm_home", "tm_away")
rtrip_cutoff <- 2
hstand_cutoff <- rtrip_cutoff
drest_cutoff_high <- 2
drest_cutoff_low <- 0

# These variables are to eliminate "unusually" long home stands/road trips.
rtrip_cutoff_max <- 6
hstand_cutoff_max <- rtrip_cutoff_max
# This "max" variable gets rid of All-Start break "rest".
drest_cutoff_max <- 5

vars_groups_meanings <-
  list(
    group1a = str_c(
      "hstand >= ",
      hstand_cutoff,
      " (and hstand <= ",
      hstand_cutoff_max,
      ")"
    ),
    group1b = str_c("rtrip >= ", rtrip_cutoff,  " (and rtrip <= ", rtrip_cutoff_max, ")"),
    group2a = str_c(
      "drest >= ",
      drest_cutoff_high,
      " (and drest <= ",
      drest_cutoff_max,
      ")"
    ),
    group2b = str_c("drest <= ", drest_cutoff_low)
  )
vars_groups <- names(vars_groups_meanings)
vars_all <- c("hstand", "rtrip", "drest")
vars_summarise <- c("w", "pd", "prob")

# Should implement `!!` here with functions...
add_groups <- function(data, use_max = TRUE) {
  if (use_max == TRUE) {
    data %>%
      mutate(
        group1a = ifelse(
          hstand >= hstand_cutoff,
          ifelse(hstand < hstand_cutoff_max, 1, 0),
          0
        ),
        group1b = ifelse(
          rtrip >= rtrip_cutoff,
          ifelse(rtrip < rtrip_cutoff_max, 1, 0),
          0
        ),
        group2a = ifelse(
          drest >= drest_cutoff_high,
          ifelse(drest < drest_cutoff_max, 1, 0),
          0
        ),
        group2b = ifelse(drest <= drest_cutoff_low, 1, 0)
      )
  } else {
    data %>%
      mutate(
        group1a = ifelse(hstand >= hstand_cutoff, 1, 0),
        group1b = ifelse(rtrip >= rtrip_cutoff, 1, 0) ,
        group2a = ifelse(drest >= drest_cutoff_high, 1, 0),
        group2b = ifelse(drest <= drest_cutoff_low, 1, 0)
      )
  }
}

filter_groups <- function(data) {
  data %>% filter((group1a == 1 |
                     group1b == 1) & (group2a == 1 | group2b == 1))
}

#'
#'
#'
if (run_unused_code == TRUE) {
  # This isn't actually used.
  rest_counts_all <-
    results_prepared %>%
    # filter(tm == tm_home) %>%
    group_by_at(vars_all) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(freq = n / sum(n))
  rest_counts_all %>% arrange(desc(n))

  # This isn't actually used.
  rest_counts_groups_all <-
    results_prepared %>%
    # filter(tm == tm_home) %>%
    add_groups() %>%
    group_by_at(vars_groups) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(freq = n / sum(n))
  rest_counts_groups_all
}

#'
#'
#'
rest_counts_groups_filtered <-
  results_prepared %>%
  # filter(tm == tm_home) %>%
  add_groups() %>%
  filter_groups() %>%
  group_by_at(vars_groups) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(freq = n / sum(n))
rest_counts_groups_filtered

#'
#'
#'
if (run_unused_code == TRUE) {
  rest_calcs_counts_all <-
    results_prepared %>%
    group_by_at(vars_all) %>%
    summarise_at(vars(vars_summarise), funs(mean)) %>%
    ungroup() %>%
    inner_join(rest_counts_all)
  rest_calcs_counts_all

  rest_calcs_counts_all %>%
    arrange(desc(pd)) %>%
    filter(hstand < hstand_cutoff_max,
           rtrip < rtrip_cutoff_max,
           drest < drest_cutoff_max)

  rest_calcs_counts_groups_all <-
    results_prepared %>%
    add_groups() %>%
    group_by_at(vars_groups) %>%
    summarise_at(vars(vars_summarise), funs(mean)) %>%
    ungroup() %>%
    inner_join(rest_counts_groups_all)
  rest_calcs_counts_groups_all
}
#'
#'
#'
rest_calcs_counts_groups_filtered <-
  results_prepared %>%
  add_groups() %>%
  filter_groups() %>%
  group_by_at(vars_groups) %>%
  summarise_at(vars(vars_summarise), funs(mean)) %>%
  ungroup() %>%
  inner_join(rest_counts_groups_filtered)
rest_calcs_counts_groups_filtered
vars_groups_meanings

colnames_match_idx <-
  str_which(names(rest_calcs_counts_groups_filtered),
            names(vars_groups_meanings))
names(rest_calcs_counts_groups_filtered)[colnames_match_idx] <-
  vars_groups_meanings
rest_calcs_counts_groups_filtered

rest_calcs_baseline <-
  results_prepared %>%
  filter(tm == tm_home) %>%
  summarise_at(vars(vars_summarise), funs(mean))
rest_calcs_baseline
#'
#'
#'
# bookmark ----
if (run_unused_code == TRUE) {

  var_y_rgs <- "pd"
  var_y_cls <- "w"
  vars_ff_1g <-
    names(results_prepared) %>%
    str_subset("efg|tov|orb|ftr") %>%
    str_subset("_1g$")
  vars_ff_1g
  vars_ff_td <-
    names(results_prepared) %>%
    str_subset("efg|tov|orb|ftr") %>%
    str_subset("_td$")
  vars_ff_td
  vars_rest_direct <- c("hfa", "drest", "drest_opp")

  fmla_ff_rgs <- create_fmla(var_y_rgs, c(vars_ff_td, " 0"))
  fmla_ff_rest_groups_rgs <- create_fmla(var_y_rgs, c(vars_ff_td, vars_groups, " 0"))
  fmla_ff_rest_direct_rgs <- create_fmla(var_y_rgs, c(vars_ff_td, vars_rest_direct, " 0"))

  fmla_ff_cls <- create_fmla(var_y_cls, c(vars_ff_td, " 0"))
  fmla_ff_rest_groups_cls <- create_fmla(var_y_cls, c(vars_ff_td, vars_groups, " 0"))
  fmla_ff_rest_direct_cls <- create_fmla(var_y_cls, c(vars_ff_td, vars_rest_direct, " 0"))

  data_tst <-
    results_prepared %>%
    mutate_at(var_y_cls, as.factor) %>%
    # distinct(season, date, tm_home, tm_away, .keep_all = TRUE) %>%
    # filter(season >= season_cutoff) %>%
    add_groups()

  data_tst <-
    data_tst %>%
    mutate_at(vars(vars_ff_td), funs(BBmisc::normalize(
      ., method = "range", range = c(0, 1)
    )))


  lm_ff <- lm(fmla_ff_rgs, data_tst)
  lm_ff_rest_groups <- lm(fmla_ff_rest_groups_rgs, data_tst)
  lm_ff_rest_direct <- lm(fmla_ff_rest_direct_rgs, data_tst)

  stargazer::stargazer(lm_ff, lm_ff_rest_groups, lm_ff_rest_direct, type = "text")
  # summary(lm_ff)
  # summary(lm_ff_rest_groups)
  # summary(lm_ff_rest_direct)
  # broom::tidy(lm_ff)
  # broom::tidy(lm_ff_rest_groups)
  # broom::tidy(lm_ff_rest_direct)

  glm_ff <- glm(fmla_ff_cls, data_tst, family = "binomial")
  glm_ff_rest_groups <- glm(fmla_ff_rest_groups_cls, data_tst, family = "binomial")
  glm_ff_rest_direct <- glm(fmla_ff_rest_direct_cls, data_tst, family = "binomial")

  stargazer::stargazer(glm_ff, glm_ff_rest_groups, glm_ff_rest_direct, type = "text")
  # summary(glm_ff)
  # summary(glm_ff_rest_groups)
  # summary(glm_ff_rest_direct)
  # broom::tidy(glm_ff)
  # broom::tidy(glm_ff_rest_groups)
  # broom::tidy(glm_ff_rest_direct)


  calc_explpct <- function(fmla, data) {
    lm_i <- lm(fmla, data)
    lm_coefs <- broom::tidy(lm_i)

    lm_coefs_tidy <-
      lm_coefs %>%
      mutate(term_copy = term) %>%
      separate(term_copy, c("stat", "side", "td"), sep = "_") %>%
      select(-`td`) %>%
      arrange(side, stat) %>%
      mutate_at(vars(estimate), funs(estimate_abs = abs)) %>%
      mutate(estimate_abs_sum = sum(estimate_abs)) %>%
      mutate(estimate_abs_cumsum = cumsum(estimate_abs)) %>%
      mutate(estimate_abs_sum = max(estimate_abs_cumsum))

    lm_coefs_explpct <-
      lm_coefs_tidy %>%
      mutate(term_explpct  = 100 * (estimate_abs / estimate_abs_sum)) %>%
      arrange(desc(term_explpct)) %>%
      mutate(term_explpct_cumsum  = cumsum(term_explpct))

    lm_coefs_explpct %>%
      select(term, stat, side, term_explpct, term_explpct_cumsum) %>%
      group_by(stat) %>%
      summarise(stat_explpct_sum = sum(term_explpct)) %>%
      arrange(desc(stat_explpct_sum))
  }

  coefs_explpct_baseline <- calc_explpct(fmla_ff_rgs, data_tst)
  coefs_explpct_1a_2a <-
    calc_explpct(fmla_ff_rgs, data_tst %>% filter(group1a == 1 &
                                                   group2a == 1))
  coefs_explpct_1a_2b <-
    calc_explpct(fmla_ff_rgs, data_tst %>% filter(group1a == 1 &
                                                   group2b == 1))
  coefs_explpct_1b_2a <-
    calc_explpct(fmla_ff_rgs, data_tst %>% filter(group1b == 1 &
                                                   group2a == 1))
  coefs_explpct_1b_2b <-
    calc_explpct(fmla_ff_rgs, data_tst %>% filter(group1b == 1 &
                                                   group2b == 1))

  coefs_explpct_baseline
  coefs_explpct_1a_2a
  coefs_explpct_1a_2b
  coefs_explpct_1b_2a
  coefs_explpct_1b_2b

}

#'
#'
#'
# BOOKMARK ----

var_y <- "pd"
vars_baseline <- c("pd_td", "pd_td_opp")
fmla_rest_1 <- create_fmla(var_y, c(vars_groups, " 0"))
fmla_rest_2 <- create_fmla(var_y, c(vars_baseline, " 0"))
fmla_rest_3 <- create_fmla(var_y, c(vars_baseline, vars_groups, " 0"))

data_trn <-
  results_prepared %>%
  # distinct(season, date, tm_home, tm_away, .keep_all = TRUE) %>%
  filter(season < season_cutoff) %>%
  add_groups()

data_tst <-
  results_prepared %>%
  # distinct(season, date, tm_home, tm_away, .keep_all = TRUE) %>%
  filter(season >= season_cutoff) %>%
  add_groups()

lm_rest_1 <- lm(fmla_rest_1, data_trn)
# lm_rest_1_fct <- lm(fmla_rest_1, data_trn %>% mutate_at(vars_groups, as.factor))
lm_rest_2 <- lm(fmla_rest_2, data_trn)
lm_rest_3 <- lm(fmla_rest_3, data_trn)

broom::tidy(lm_rest_1)
broom::tidy(lm_rest_2)
broom::tidy(lm_rest_3)

broom::glance(lm_rest_1)
broom::glance(lm_rest_2)
broom::glance(lm_rest_3)

#'
#'
#'
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_rmse <- function(model, data, response) {
  rmse(actual = data[, response],
       predicted = predict(model, data))
}

lm_list <- list(lm_rest_1, lm_rest_2, lm_rest_3)

lm_trn_rmse <-
  sapply(lm_list, get_rmse, data = data_trn, response = var_y)
lm_trn_rmse
lm_tst_rmse <-
  sapply(lm_list, get_rmse, data = data_tst, response = var_y)
lm_tst_rmse

