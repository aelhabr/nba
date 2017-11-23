
#'
#'
#'
#+ include = FALSE
rm(list = ls())

projroot <- rprojroot::find_rstudio_root_file()
# setwd("O:/_other/projects/nba/")
# load("data/experiment_session.RData")
setwd(projroot)

filepath_import <-
  file.path(projroot, "data", "experiment_session.RData")
if (file.exists(filepath_import)) {
  load(filepath_import)
} else {
  
}
source("R/game_results-functions_experiment.R")

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
library("readr")
library("tidyr")
library("ggplot2")
seed <- 42
theme_set(theme_minimal())

#'
#'
#'
#+ include = FALSE
remove_tempvars <- FALSE

#'
#'
#' # Four Factor Evaluation.
#'
#' This is a quick evaluation of the four factors weidghtings.
#' (I originally did this to verify the numbers calculated by
#' CountingTheBasekts.)
#' 
#' This method is different that what he did because I scale the co-variates directly.
#' (instead of doing a with-or-without-you model with each permutation
#' of variables and normalizing the difference in the predictiona accuracy with
#' the response variable).
#'
vars_ff_1g <-
  names(results_prepared) %>%
  str_subset("efg|tov|orb|ftr") %>% 
  str_subset("_1g$")
vars_ff_1g

source("R/game_results-functions_experiment.R")
fmla_ff_1g <- create_fmla("pd", c(vars_ff_1g, " + 0"))

model_data_tst <- 
  results_prepared %>% 
  distinct(season, date, tm_home, tm_away, .keep_all = TRUE) %>% 
  filter(season >= season_cutoff)

# lm_ff_1g <- lm( fmla_ff_1g, data = model_data_recent)
lm_ff_1g_scaled <-
  lm(fmla_ff_1g, data = model_data_tst %>% mutate_at(vars(vars_ff_1g), funs(
    BBmisc::normalize(., method = "range", range = c(0, 1))
  )))

# broom::glance(lm_ff_1g)
broom::glance(lm_ff_1g_scaled)

# lm_ff_1g_coefs <- broom::tidy(lm_ff_1g)
# lm_ff_1g_coefs

lm_ff_1g_scaled_coefs <- broom::tidy(lm_ff_1g_scaled)

lm_ff_1g_scaled_coefs_2 <-
  lm_ff_1g_scaled_coefs %>%
  mutate(term_copy = term) %>% 
  separate(term_copy, c("stat", "side", "1g"), sep = "_") %>% 
  select(-`1g`) %>% 
  arrange(side, stat) %>% 
  mutate_at(vars(estimate), funs(estimate_abs = abs)) %>%
  mutate(estimate_abs_sum = sum(estimate_abs)) %>% 
  mutate(estimate_abs_cumsum = cumsum(estimate_abs)) %>% 
  mutate(estimate_abs_sum = max(estimate_abs_cumsum))
lm_ff_1g_scaled_coefs_2

lm_ff_1g_scaled_coefs_explpct <-
  lm_ff_1g_scaled_coefs_2 %>%
  mutate(term_explpct  = 100 * (estimate_abs / estimate_abs_sum)) %>% 
  arrange(desc(term_explpct)) %>% 
  mutate(term_explpct_cumsum  = cumsum(term_explpct)) 

lm_ff_1g_scaled_coefs_explpct %>% 
  select(term, stat, side, term_explpct, term_explpct_cumsum)

lm_ff_1g_scaled_coefs_explpct %>% 
  select(term, stat, side, term_explpct, term_explpct_cumsum) %>% 
  group_by(stat) %>% 
  summarise(stat_explpct_sum = sum(term_explpct)) %>% 
  arrange(desc(stat_explpct_sum))

#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  rm(
    list = c(
      "vars_ff_1g",
      "model_data_tst",
      "lm_ff_1g_scaled",
      "lm_ff_1g_scaled_coefs",
      "lm_ff_1g_scaled_coefs_2",
      "lm_ff_1g_scaled_coefs_explpct"
    )
  )
}

