#' ---
#' title: ""
#' author: "Tony"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output:
#'  html_document:
#'    toc: true
#'    toc_depth: 3
#' ---
#'
#+ global_options, include = FALSE
knitr::opts_chunk$set(
  echo = TRUE,
  cache = FALSE,
  fig.show = "hide",
  fig.align = "center",
  results = "hide",
  warning = FALSE,
  message = FALSE
)

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
library("tidyr")
library("printr")


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
  filepath_import %>%
  str_replace(filename_import_ext, str_c("-colnames", filename_import_ext))
filepath_import_eoy <-
  filepath_import %>%
  str_replace(filename_import_ext, str_c("-eoy", filename_import_ext))

#'
#'
#'
#+ include = FALSE
remove_tempvars <- TRUE
try_old_code <- FALSE
seed <- 42
season_cutoff <- 2015
#'
#'
#'
# Import. ----
results_prepared <- read_csv(filepath_import)
results_prepared_colnames <- read_csv(filepath_import_colnames)
results_eoy <- read_csv(filepath_import_eoy)

#'
#'
#'
results_prepared_colnames %>% distinct(type)

var_types_1g <- 
  results_prepared_colnames %>% 
  distinct(type) %>% 
  filter(str_detect(type, "_1g")) %>% 
  pull(type)

vars_remove_1g <-
  results_prepared_colnames %>% 
  filter(type %in% var_types_1g) %>% 
  pull(value)

var_types_td <- 
  results_prepared_colnames %>% 
  distinct(type) %>% 
  filter(str_detect(type, "_td")) %>% 
  pull(type)

vars_remove_td <-
  results_prepared_colnames %>% 
  filter(type %in% var_types_td) %>% 
  pull(value)
vars_remove <- c(vars_remove_1g, vars_remove_td)

results_prepared_min <-
  results_prepared %>%
  select(-one_of(vars_remove))


#'
#' Create a "key" to identify unique games (becuase there are two records for each game).
#'
# results_prepared_min <-
#   results_prepared_min %>%
#   distinct(date, tm_home, tm_away, .keep_all = TRUE)
#
# results_prepared_min <-
#   results_prepared_min %>%
#   filter((pd > 0 & tm_off == tm_winner) | (pd < 0 & tm_off != tm_winner))

# set.seed(seed)
# results_prepared_min %>%
#   mutate(rn = row_number()) %>%
#   group_by(date, tm_home, tm_away, rn) %>%
#   mutate(g_key = ifelse(1, paste0(sort(c(tm_home, tm_away)))), 0) %>%
#   ungroup() %>%
#   select(rn, tm_home, tm_away, g_key)

results_prepared_min <-
  results_prepared_min %>%
  # mutate(temp = str_c(date, sample(c(tm_home, tm_away)))) %>%
  mutate(temp = str_c(date, tm_home, tm_away)) %>%
  group_by(temp) %>%
  mutate(rn = row_number(temp)) %>%
  ungroup() %>%
  select(rn, everything())
#'
#'
#'
vars_factor <-
  c("result", "g", "w", "hfa", "b2b", "no2drestin4", "no2drestin5")
vars_factor <- c(vars_factor, str_c(vars_factor, "_opp"))

results_prepared_min <-
  results_prepared_min %>%
  mutate_at(vars_factor, as.factor)
#'
#'
#'
#+ include = FASE
# Filter for only relevant games.
# results_prepared_min <-
#   results_prepared_min %>%
#   filter(rn == 1) %>%
#   select(-rn)

# results_prepared_min <-
#   results_prepared_min %>%
#   filter(g_td > 0)

#'
#'
#'
#+ include = FALSE
# Check data for possible "corner cases".
results_prepared_min %>%
  select(pd_h2a, pd, pd_opp) %>%
  summary()

results_prepared_min %>%
  filter(pd_td == 0 | pd_td_opp == 0) %>%
  select(date, tm, tm_opp)

results_prepared_min %>%
  filter(pd_td != 0 & pd_td_opp != 0 & pd_td == pd_td_opp) %>%
  select(date, tm, tm_opp, g_td, pd_td, pd_td_opp)

#'
#'
#'
#+ include = FALSE
results_prepared_pd <-
  results_prepared_min %>%
  filter(pd_td != 0 & pd_td_opp != 0 & pd_td != pd_td_opp)

results_prepared_pd %>%
  # filter(tm == tm_home) %>%
  mutate(pd_td_diff = pd_td - pd_td_opp) %>%
  mutate(pd_td_diff_sign = ifelse(pd_td_diff >= 0, 1, 0)) %>%
  group_by(hfa, result, pd_td_diff_sign) %>%
  summarise(n = n())

pts_hfa <- 0
results_prepared_pd %>%
  #filter(tm == tm_home) %>%
  mutate(pd_correct = ifelse(((pd_td + pts_hfa) - pd_td_opp) > pd, 1, 0)) %>%
  group_by(hfa, pd_correct, w) %>%
  count()

if (remove_tempvars == TRUE) {
  rm("results_prepared_pd")
}

#'
#'
#'
# Ignore the "attributes are not identical across measure variables; they will be dropped" warning.
results_tiers4 <-
  results_prepared_min %>%
  summarise(
    lmax = quantile(pd, 0),
    lmid = quantile(pd, 1 / 4),
    t = quantile(pd, 1 / 2),
    wmid = quantile(pd, 3 / 4),
    wmax = quantile(pd, 1)
  ) %>%
  gather(tier, value) %>%
  arrange(value)
results_tiers4

results_tiers6 <-
  results_prepared_min %>%
  summarise(
    lmax = quantile(pd, 0),
    lbig = quantile(pd, 1 / 6),
    lclose = quantile(pd, 2 / 6),
    t = quantile(pd, 3 / 6),
    wclose = quantile(pd, 4 / 6),
    wbig = quantile(pd, 5 / 6),
    wmax = quantile(pd, 1)
  ) %>%
  gather(tier, value) %>%
  arrange(value)
results_tiers6

# This is a very custom funciton only to be used here.
get_tier_labels <- function(d) {
  d %>%
    mutate(tier_lag1 = lag(tier, default = "l0")) %>%
    mutate(label = str_c(tier, "_", tier_lag1)) %>%
    select(tier, label, value)
}

results_tiers4_labels <-
  results_tiers4 %>%
  get_tier_labels()
results_tiers4_labels

results_tiers6_labels <-
  results_tiers6 %>%
  get_tier_labels()
results_tiers6_labels

# This is a nother highly-specific function.
cut_results <- function(vals_cont, vals_disc, labs) {
  cut(vals_cont, vals_disc, labs, include.lowest = TRUE)
}

results_prepared_min <-
  results_prepared_min %>%
  mutate(
    result_tier4 = cut_results(
      results_prepared_min$pd,
      results_tiers4_labels$value,
      results_tiers4_labels$label[-1]
    )
  ) %>%
  mutate(
    result_tier6 = cut_results(
      results_prepared_min$pd,
      results_tiers6_labels$value,
      results_tiers6_labels$label[-1]
    )
  ) %>%
  select(rn, result_tier4, everything())

results_prepared_min <-
  results_prepared_min %>%
  mutate_at(vars(starts_with("result")), funs(as.factor(str_to_lower(.))))

#'
#'
#'
#+ include = FALSE
# Examine the data.
results_prepared_min %>% filter(rn == 1) %>% group_by(result) %>% count()
results_prepared_min %>% filter(rn == 1) %>% group_by(result_tier4) %>% count()
results_prepared_min %>% filter(rn == 1) %>% group_by(result_tier6) %>% count()

#'
#' Identify varaibles that are "significant" for predicting wins.
#'
#+ include = FALSE
if (try_old_code == TRUE) {
  # vars_eoy_ignore <-
  #   names(results_eoy) %>%
  #   str_subset("^season$|^g$|^t$|^tm|^dist|^mp_")
  # vars_eoy_ignore
  #
  # vars_eoy_stats <-
  #   setdiff(names(results_eoy), vars_eoy_ignore)
  # vars_eoy_stats
  
  vars_eoy_stats <-
    results_prepared_colnames %>%
    # filter(type %in% c("calc", "calc_opp")) %>%
    filter(type %in% c("off_td", "def_td")) %>%
    mutate(value = str_replace_all(value, "_td", "")) %>%
    pull(value)
  vars_eoy_stats
  
  results_eoy_stats <-
    results_eoy %>%
    select(one_of(vars_eoy_stats))
  
  x <- results_eoy_stats
  lcombo_info <- caret::findLinearCombos(x)
  lcombo_info
  lcombo_info$remove
  for (i in 1:length(lcombo_info$linearCombos)) {
    print(names(results_eoy_stats)[lcombo_info$linearCombos[[i]]])
    print(names(results_eoy_stats)[lcombo_info$remove[[i]]])
  }
  x_nolcombo <- x[, -lcombo_info$remove]
  
  set.seed(seed)
  anneal_info <-
    subselect::anneal(
      cor(x_nolcombo),
      kmin = 1,
      kmax = ncol(x_nolcombo),
      nsol = ncol(x_nolcombo),
      niter = 10,
      setseed = TRUE
    )
  # anneal_info$bestsets
  
  num_vars <- 10
  vars_best_idx <- anneal_info$bestsets[num_vars, 1:num_vars]
  vars_best <- names(x_nolcombo)[vars_best_idx]
  vars_best
  x_annealed <- x_nolcombo[vars_best_idx]
  
  # Other subsetting techniques...
  
  if (remove_tempvars == TRUE) {
    rm("x")
    rm("x_nolcombo")
    rm("x_annealed")
  }
}
#'
#' Use the four factor statistics, along with offensive and defensive rating.
#' These statistics are highlighted on Ben Falk's website "Cleaning the Glass".
#' (This is the reason for the "ctg" suffix in variable names.)
#'

vars_ctg <-
  names(results_eoy) %>%
  # str_subset("ortg|efg|orb|ast|tov")
  str_subset("efg|tov|orb|ftr")
vars_ctg

#'
#' Try some unsupervised learning.
#'
if (try_old_code == TRUE) {
  results_eoy_ctg <-
    results_eoy %>%
    select(one_of(vars_ctg))
  
  x <- results_eoy_ctg
  
  # Arbitrary pick based on when difference between consecutive points is deemed "insignificant".
  # See .get_withinSS() function
  # at https://github.com/kassambara/factoextra/blob/master/R/fviz_nbclust.R.
  factoextra::fviz_nbclust(x, kmeans, method = "wss")
  # # This isn't too helpful...
  factoextra::fviz_nbclust(x, kmeans, method = "wss")$data$y %>% diff(differences = 2) %>% which.min() + 2
  
  factoextra::fviz_nbclust(x, kmeans, method = "silhouette")
  factoextra::fviz_nbclust(x, kmeans, method = "silhouette")$data$y %>% which.max()
  
  set.seed(seed)
  factoextra::fviz_nbclust(x,
                           kmeans,
                           nstart = 10,
                           method = "gap_stat",
                           nboot = 50)
  
  nb <-
    NbClust::NbClust(
      x,
      distance = "euclidean",
      min.nc = 2,
      max.nc = 10,
      method = "kmeans"
    )
  
  factoextra::fviz_nbclust(nb)
}
#'
#'
#'
#'
vars_ctg_rank_asc <-
  vars_ctg %>%
  str_subset("tovpct")

vars_ctg_rank_desc <- setdiff(vars_ctg, vars_ctg_rank_asc)
results_eoy_ctg_ranks <-
  results_eoy %>%
  group_by(season) %>%
  select(one_of(c("season", "tm", vars_ctg))) %>%
  mutate_at(vars_ctg_rank_asc, row_number) %>%
  mutate_at(vars_ctg_rank_desc, funs(row_number(desc(.)))) %>%
  ungroup()
results_eoy_ctg_ranks

results_eoy_ctg_ranks <-
  results_eoy_ctg_ranks %>%
  mutate_at(vars_ctg, funs(class = cut(
    .,
    breaks = seq(0, 30, by = 10),
    labels = c("good", "ok", "bad")
  )))
results_eoy_ctg_ranks
#'
#'
#'
#+ include = FALSE
results_eoy_ctg_ranks %>%
  filter(season == 2016) %>%
  filter(efgpct_off <= 5)

#'
#'
#'
#+ include = FALSE
results_eoy_ctg_ranks_0to1 <-
  results_eoy_ctg_ranks %>%
  mutate_at(vars_ctg, scale)

results_eoy_ctg_ranks_0to1 %>% select(-season, -tm) %>% apply(2, summary)
results_eoy_ctg_ranks_0to1 %>% summarise_at(vars_ctg, funs(max))
results_eoy_ctg_ranks_0to1[which.max(results_eoy_ctg_ranks_0to1$efgpct_off), ]
if (remove_tempvars == TRUE) {
  rm("results_eoy_ctg_ranks_0to1")
}
#'
#'
#'
# vars_base <-
#   results_prepared_colnames %>%
#   filter(type %in% c("base")) %>%
#   pull(value)

vars_y <- c("result", "result_tier4", "result_tier6")

results_eoy_ctg_ranks <-
  results_eoy_ctg_ranks %>% 
  rename_at(vars(c(vars_ctg, str_c(vars_ctg, "_class"))), funs(str_c(., "_eoy_rank")))

model_data <-
  results_prepared_min %>%
  # select(one_of(c("rn", vars_y, vars_base))) %>%
  inner_join(results_eoy_ctg_ranks, by = c("season", "tm")) %>%
  inner_join(
    results_eoy_ctg_ranks %>%
      rename_all(funs(str_c(., "_opp"))),
    by = c("season" = "season_opp", "tm_opp" = "tm_opp")
  )
#'
#'
#'
if(remove_tempvars == TRUE) {
  # rm("results_prepared_min")
  # rm("results_prepared_colnames")
  # rm("results_eoy")
  # rm("results_eoy_ctg_ranks")
}
#'
#'
#'
# Note that session::save.session() saves packages as well (while base:;save() does not).
session::save.session(file = "data/experiment_session.RData")
#'
#'
#'
