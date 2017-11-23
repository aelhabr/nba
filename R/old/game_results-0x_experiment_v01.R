



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
library("ggplot2")
seed <- 42
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

results_prepared <-
  results_prepared %>%
  select(-one_of(vars_remove))


#'
#' Create a "key" to identify unique games (becuase there are two records for each game).
#'
# results_prepared <-
#   results_prepared %>%
#   distinct(date, tm_home, tm_away, .keep_all = TRUE)
#
# results_prepared <-
#   results_prepared %>%
#   filter((pd > 0 & tm_off == tm_winner) | (pd < 0 & tm_off != tm_winner))

# set.seed(42)
# results_prepared %>%
#   mutate(rn = row_number()) %>%
#   group_by(date, tm_home, tm_away, rn) %>%
#   mutate(g_key = ifelse(1, paste0(sort(c(tm_home, tm_away)))), 0) %>%
#   ungroup() %>%
#   select(rn, tm_home, tm_away, g_key)

results_prepared <-
  results_prepared %>%
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

results_prepared <-
  results_prepared %>%
  mutate_at(vars_factor, as.factor)
#'
#'
#'
#+ include = FASE
# Filter for only relevant games.
# results_prepared <-
#   results_prepared %>%
#   filter(rn == 1) %>%
#   select(-rn)

# results_prepared <-
#   results_prepared %>%
#   filter(g_td > 0)

#'
#'
#'
#+ include = FALSE
# Check data for possible "corner cases".
results_prepared %>%
  select(pd_h2a, pd, pd_opp) %>%
  summary()

results_prepared %>%
  filter(pd_td == 0 | pd_td_opp == 0) %>%
  select(date, tm, tm_opp)

results_prepared %>%
  filter(pd_td != 0 & pd_td_opp != 0 & pd_td == pd_td_opp) %>%
  select(date, tm, tm_opp, g_td, pd_td, pd_td_opp)

#'
#'
#'
#+ include = FALSE
results_prepared_pd <-
  results_prepared %>%
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
  results_prepared %>%
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
  results_prepared %>%
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

results_prepared <-
  results_prepared %>%
  mutate(
    result_tier4 = cut_results(
      results_prepared$pd,
      results_tiers4_labels$value,
      results_tiers4_labels$label[-1]
    )
  ) %>%
  mutate(
    result_tier6 = cut_results(
      results_prepared$pd,
      results_tiers6_labels$value,
      results_tiers6_labels$label[-1]
    )
  ) %>%
  select(rn, result_tier4, everything())

results_prepared <-
  results_prepared %>%
  mutate_at(vars(starts_with("result")), funs(as.factor(str_to_lower(.))))

#'
#'
#'
#+ include = FALSE
# Examine the data.
results_prepared %>% filter(rn == 1) %>% group_by(result) %>% count()
results_prepared %>% filter(rn == 1) %>% group_by(result_tier4) %>% count()
results_prepared %>% filter(rn == 1) %>% group_by(result_tier6) %>% count()

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
  str_subset("efg|orb|ast|tov")
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
  
  set.seed(42)
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
  results_prepared %>%
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
  # rm("results_prepared")
  # rm("results_prepared_colnames")
  # rm("results_eoy")
  # rm("results_eoy_ctg_ranks")
}
#'
#' Try some supervised learning.
#'
# These are "general" functions that can be applied in different situations.
add_tickmarks <- function(x) {
  ifelse(grepl("^[0-9]", x) == TRUE, paste0("`", x, "`"), x)
}

create_fmla <- function(var_y, vars_x, keep_as_char = FALSE) {
  vars_x <- add_tickmarks(vars_x)
  var_y <- add_tickmarks(var_y)
  fmla <- paste0(var_y, " ~ ", paste(vars_x, collapse = " + "))
  
  if(keep_as_char == FALSE) {
    fmla <- as.formula(fmla)
  }
}

convert_fmla_to_char <- function(fmla) {
  if(class(fmla) != "formula") {
    warning("fmla is not of type 'formula'")
    return(fmla)
  }
  paste(fmla[2], fmla[3], sep = "~")
}

var_y_1 <- "result"
var_y_2 <- "result_tier4"
var_y_2 <- "result_tier6"

season_cutoff <- 2015
seasons_trn <-
  model_data %>%
  distinct(season) %>%
  filter(season < season_cutoff) %>%
  pull(season)

seasons_tst <-
  model_data %>%
  distinct(season) %>%
  filter(!(season %in% seasons_trn)) %>%
  pull(season)

#'
#' Create "basic" models
#'
calculate_class_accuracy <- function(y_pred, y_actual) {
  if (class(y_pred) != "character" | class(y_actual) != "character") {
    # stop("y_pred and/or y_actual should be a character vector")
    if (class(y_pred) != "character") {
      # stop("y_pred should be a character vector")
      y_pred <- as.character(y_pred)
      message("converting y_pred to a character vector")
    }
    if (class(y_actual) != "character") {
      # stop("y_actual should be a character vector")
      y_actual <- as.character(y_actual)
      message("converting y_actual to a character vector")
    }
  }
  # sum(diag(table(y_actual, y_pred))) / sum(table(y_actual, y_pred))
  mean(y_actual == y_pred)
}

convert_pred_to_numeric <-
  function(y_pred,
           high_char = "w",
           low_char = "l",
           high_num = 1,
           low_num = 0) {
    ifelse(y_pred == high_char,
           high_num,
           ifelse(y_pred == low_char, low_num, as.numeric(NULL)))
  }

calculate_model_accuracy <-
  function(y_prob,
           y_actual,
           method = "glm",
           high = "w",
           low = "l",
           k = 1,
           cutoff = 0.5,
           choose_opt = FALSE) {
    # y_actual <- y_tst
    # y_prob <- y_pred_tst
    # high <- "w"
    # low <- "l"
    # cutoff <- 0.5
    # choose_opt <- TRUE
    if (class(y_actual) != "character") {
      y_actual <- as.character(y_actual)
      message("y_actual converted to a character vector")
    }
    
    if(method == "glm") {
      if (choose_opt == TRUE) {
        # InformationValue::optimalCutoff(convert_pred_to_numeric(y_actual), y_prob, returnDiagnostics = TRUE)
        cutoff <-
          InformationValue::optimalCutoff(convert_pred_to_numeric(y_actual), y_prob)
      }
      y_pred <- y_prob
      if (!is.null(names(y_pred))) {
        names(y_pred) <- NULL
      }
      y_pred[y_pred >= cutoff] <- high
      y_pred[y_pred < cutoff] <- low
    } else if (method %in% c( "knn", "lda", "qda", "nb")) {
      y_pred <- y_prob
    }
    
    calculate_class_accuracy(y_actual, y_pred)
  }

diagnose_model_performance <-
  function(fmla = NULL,
           trn_data = NULL,
           tst_data = NULL,
           method = NULL,
           k = 1,
           ...) {
    fmla <- fmla_ctg_2
    trn_data <- filter(model_data, season %in% seasons_trn)
    tst_data <- filter(model_data, season %in% seasons_tst)
    high <- "w"
    low <- "l"
    cutoff <- 0.5
    choose_opt <- FALSE
    method <- "svm"
    # k <- 50
    
    var_y <- as.character(fmla)[2]

    y_trn <-
      trn_data %>%
      select(one_of(var_y)) %>%
      pull(var_y) %>%
      as.character()
    
    y_tst <-
      tst_data %>%
      select(one_of(var_y)) %>%
      pull(var_y) %>%
      as.character()
    
    if(method %in% c("glm", "lda", "qda", "nb", "svm")) {
      
      if(method == "glm") {
        trn_time_details <-
          system.time(fit <- stats::glm(fmla, trn_data, family = "binomial"))
      } else if(method %in% c("lda", "qda")) {
        if(method == "lda") {
          trn_time_details <-
            system.time(fit <- MASS::lda(fmla, trn_data))
        } else if (method == "qda") {
          trn_time_details <-
            system.time(fit <- MASS::qda(fmla, trn_data))
        }
      } else if(method == "nb") {
        trn_time_details <-
          system.time(fit <- e1071::naiveBayes(fmla, trn_data))
      } else if (method == "svm") {
        trn_time_details <-
          system.time(fit <- e1071::svm(fmla, trn_data, kernel = "ineaer", cost = 0.01, scale = FALSE))

      }
      fit

      if(choose_opt == FALSE) {
        if(method == "glm") {
          y_pred_trn <- predict(fit, type = "response")
          y_pred_tst <- predict(fit, tst_data, type = "response")
        } else if(method %in% c("lda", "qda")) {
          y_pred_trn <- predict(fit)$class %>% as.character()
          y_pred_tst <- predict(fit, tst_data)$class %>% as.character()
        } else if(method %in% c("nb", "svm")) {
          # Must specify newdata.
          # Also, must specify type = "class" instead of "response".
          y_pred_trn <- predict(fit, newdata = trn_data, type = "class") %>% as.character()
          y_pred_tst <- predict(fit, newdata = tst_data, type = "class") %>% as.character()
        }
      }
    }
    
    if (method == "knn") {
      vars_x <-
        as.character(fmla)[3] %>% str_split(pattern = "\\s\\+\\s") %>% unlist()
      x_trn <- trn_data %>% select(one_of(vars_x))
      x_tst <- tst_data %>% select(one_of(vars_x))

      trn_time_details_1 <-
        system.time(y_pred_trn <-
                      class::knn(x_trn, x_trn, y_trn, k = k))
      trn_time_details_2 <-
        system.time(y_pred_tst <-
                      class::knn(x_trn, x_tst, y_trn, k = k))
      # trn_time_details <-  rbind(trn_time_details_1, trn_time_details_2)
      trn_time_details <- trn_time_details_1
    }
    
    # acc_trn <- calculate_model_accuracy(y_pred_trn, y_trn, method)
    # acc_tst <- calculate_model_accuracy(y_pred_tst, y_tst, method)
    # The "..." argument doesn't work (unless k as included as an argument to calculate_model_accuracy().
    acc_trn <- calculate_model_accuracy(y_pred_trn, y_trn, method, ...)
    acc_tst <- calculate_model_accuracy(y_pred_tst, y_tst, method, ...)

    
    trn_time <- as.numeric(trn_time_details[3])
    diagnostics <-
      tibble(
        train_time = trn_time,
        accuracy_train = acc_trn,
        accuracy_test = acc_tst
      )
    diagnostics
  }

vars_basic <- "wp_td"
vars_basic_1 <- c(vars_basic, str_c(vars_basic, "_opp"))
fmla_basic_1 <- create_fmla(var_y_1, vars_basic_1)

vars_basic_2 <- c(vars_basic_1, "hfa")
fmla_basic_2 <- create_fmla(var_y_1, vars_basic_2)

vars_rest <- c("g_last7", "dist_last7", "rtrip")
vars_rest_1 <- c(vars_rest, str_c(vars_rest, "_opp"))
fmla_rest_1 <- create_fmla(var_y_1, vars_rest_1)

vars_rest_2 <- c(vars_basic_2, vars_rest_1)
fmla_rest_2 <- create_fmla(var_y_1, vars_rest_2)

fmla_ctg_1 <- create_fmla(var_y_1, str_c(vars_ctg, "_eoy_rank"))
fmla_ctg_2  <- create_fmla(var_y_1, str_c(vars_ctg, "_class_eoy_rank"))

# fmlas <-
#   c(fmla_basic_1,
#     fmla_basic_2,
#     fmla_rest_1,
#     fmla_rest_2,
#     fmla_ctg_1,
#     fmla_ctg_2)
fmlas <- c(fmla_ctg_1, fmla_ctg_2)

fmlas_chars <- sapply(fmlas, convert_fmla_to_char)
fmlas_names <-
  paste(substitute(
    c(fmla_ctg_1, fmla_ctg_2)
  ), sep = ", ")[-1]
fmlas_info <-
  tibble(fmla_name = fmlas_names, fmla = fmlas_chars)
fmlas_info

# methods <- c("glm", "knn")
# methods <- c("lda", "qda", "nb")
methods <- c("glm", "knn", "lda", "qda", "nb", "svm")

# Don't convert to a tibble because the formulas will get nested.
model_params_grid <-
  expand.grid(method = methods, 
              # fmla = fmlas) %>% 
              fmla = fmlas_chars) %>% 
  # as.data.frame(stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  inner_join(fmlas_info) %>% 
  mutate_all(as.character) %>% 
  select(method, fmla_name, fmla) %>% 
  arrange(method, fmla_name)
model_params_grid %>% select(-fmla)

model_params_grid <-
  model_params_grid %>% 
  mutate(params = "none") # %>% mutate(value = 0)

if("knn" %in% methods) {
  ks <- tibble(method = "knn", params = "k", value = 50) #seq(50, 100, by = 50))
  model_params_grid <-
    model_params_grid %>% 
    full_join(ks, by = "method") %>% 
    filter(!is.na(fmla))
}
model_params_grid %>% select(-fmla)

i <- 1
if(exists("diagnostics_details_all")) {
  rm("diagnostics_details_all")
}

while(i <= nrow(model_params_grid)) {
  # while(i < 3) {
  method <- model_params_grid$method[[i]]
  fmla_name <- model_params_grid$fmla_name[[i]]
  fmla <- as.formula(model_params_grid$fmla[[i]])
  param <- model_params_grid$param[[i]]
  value <- model_params_grid$value[[i]] 
  
  trn_data <- model_data %>% filter(season %in% seasons_trn)
  tst_data <- model_data %>% filter(season %in% seasons_tst)
  try(
  if(param == "k") {
    trn_data <- trn_data %>% mutate_if(is.factor, funs(as.numeric))
    tst_data <- tst_data %>% mutate_if(is.factor, funs(as.numeric))
    
    diagnostics <-
      diagnose_model_performance(
        fmla = fmla,
        trn_data = trn_data,
        tst_data = tst_data,
        method = method,
        k = value
      )
  } else {
    diagnostics <-
      diagnose_model_performance(
        fmla = fmla,
        trn_data = trn_data,
        tst_data = tst_data,
        method = method
      )
  }
  )
  
  diagnostics_details <-
    diagnostics %>%
    mutate(
      method = method,
      fmla_name = fmla_name,
      num_vars = length(all.vars(as.formula(fmla))[-1]),
      fmla = convert_fmla_to_char(fmla),
      param = param,
      value = ifelse(is.null(value), 0, as.numeric(NULL))
    ) %>%
    select(method, fmla_name, fmla, param, value, everything())
  
  if(!exists("diagnostics_details_all")) {
    diagnostics_details_all <- diagnostics_details
  } else {
    diagnostics_details_all <- bind_rows(diagnostics_details_all, diagnostics_details)
  }
  
  cat(method)
  cat("\n")
  cat(fmla_name)
  cat("\n")
  cat(i)
  cat("\n")
  
  i <- i + 1
}

diagnostics_details_all %>% select(-fmla)
# diagnostics_details_all %>%
#   group_by(method) %>% 
#   # sample_frac(0.5) %>%
#   # arrange(accuracy_test) %>%
#   slice(1:(nrow(diagnostics_details_all) / 2)) %>%
#   # ggplot(aes(x = num_vars, y = accuracy_test)) +
#   ggplot(aes(x = factor(fmla_name), y = accuracy_test)) +
#   ggrepel::geom_text_repel(aes(label = ifelse(
#     !is.na(value), str_c(fmla_name,  "_", param, value), fmla_name
#   ))) +
#   geom_point(aes(color = method))

diagnostics_details_all %>%
  ggplot(aes(x = fmla_name, y = accuracy_test)) +
  geom_col(aes(fill = method), position = "dodge") 

diagnostics_details_all %>%
  ggplot(aes(x = method, y = accuracy_test)) +
  geom_col(aes(fill = fmla_name), position = "dodge") 


deparse(fmla_basic_1) %>% class()
paste(fmla_basic_1, format(frm))
glm_basic_1_diagnostics <-
  diagnose_model_performance(
    fmla = fmla_basic_1,
    trn_data = filter(model_data, season %in% seasons_trn),
    tst_data = filter(model_data, season %in% seasons_tst),
    method = "glm",
    # cutoff = 0.25,
    choose_opt = TRUE
  )
paste0(as.character(fmla_basic_1))
glm_basic_1_diagnostics %>% mutate(formula = paste0(as.character(fmla_basic_1)))

glm_basic_2_diagnostics <-
  diagnose_model_performance(
    fmla = fmla_basic_2,
    trn_data = filter(model_data, season %in% seasons_trn),
    tst_data = filter(model_data, season %in% seasons_tst),
    method = "glm"
  )
knn_basic_1_diagnostics <-
  diagnose_model_performance(
    fmla = fmla_basic_1,
    trn_data = filter(model_data, season %in% seasons_trn),
    tst_data = filter(model_data, season %in% seasons_tst),
    method = "knn",
    k_choice = 50
  )
knn_basic_2_diagnostics <-
  diagnose_model_performance(
    fmla = fmla_basic_2,
    trn_data = filter(model_data, season %in% seasons_trn),
    tst_data = filter(model_data, season %in% seasons_tst),
    method = "knn",
    k_choice = 50
  )

model_diagnostics <-
  bind_rows(
    glm_basic_1_diagnostics,
    glm_basic_2_diagnostics,
    knn_basic_1_diagnostics,
    knn_basic_2_diagnostics
  )
model_diagnostics

#'
#'
#'
glm_fit_basic_1 <-
  caret::train(fmla_basic_1,
               filter(model_data, season %in% seasons_trn),
               method = "glm")
# glm_fit_basic_1$finalModel
# glm_fit_basic_1$results
calculate_model_accuracy(predict(
  glm_fit_basic_1,
  filter(model_data, season %in% seasons_tst)
), y_tst)
glm_fit_basic_2 <-
  caret::train(fmla_basic_2,
               filter(model_data, season %in% seasons_trn),
               method = "glm")
calculate_model_accuracy(predict(
  glm_fit_basic_2,
  filter(model_data, season %in% seasons_tst)
), y_tst)

knn_fit_basic_1_time <-
  system.time(
    knn_fit_basic_1 <-
      caret::train(
        fmla_basic_1,
        filter(model_data, season %in% seasons_trn),
        method = "knn",
        trControl = NULL,
        # trainControl("cv"),
        tuneGrid = data.frame(k = c(10, 50, 75))
      )
  )
knn_fit_basic_1_time
knn_fit_basic_1$results
calculate_accuracy(predict(
  knn_fit_basic_1,
  filter(model_data, season %in% seasons_tst)
), y_tst)

knn_fit_basic_2 <-
  caret::train(fmla_basic_2,
               filter(model_data, season %in% seasons_trn),
               
               method = "knn")
knn_fit_basic_2$results
calculate_accuracy(predict(
  knn_fit_basic_2,
  filter(model_data, season %in% seasons_tst)
), y_tst)

model_list <-
  list(
    glm_1 = glm_fit_basic_1,
    glm_2 = glm_fit_basic_2,
    knn_1 = knn_fit_basic_1,
    knn_2 = knn_fit_basic_2
  )
model_resamples <- resamples(model_list)
summary(model_resamples)
compare_models(glm_fit_basic_1, glm_fit_basic_2)
#'
#'
#'
knn_1 <- class::knn(x_trn, x_tst, y_trn, k = 25)
# table(knn_1, y_tst)
sum(diag(table(knn_1, y_tst))) / sum(table(knn_1, y_tst))

library("caret")
set.seed(42)
# trn_ctrl <- trainControl(method = "repeatedcv", repeats = 1)
trn_ctrl <- trainControl(method = "cv", folds = 5)
knn_1 <-
  train(x_trn,
        y_trn,
        method = "knn")

knn_fit_1
plot(knn_fit_1)
confusionMatrix(predict(knn_fit_1, x_tst), y_tst)

#'
#'
#'
fmla_1 <- create_fmla(var_y_1, vars_ctg)
fmla_2 <- create_fmla(var_y_2, vars_ctg)
fmla_3 <- create_fmla(var_y_3, vars_ctg)
