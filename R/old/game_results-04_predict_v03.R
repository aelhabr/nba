





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
library("lubridate")
library("tidyr")
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
results_prepared_colnames <- read_csv(filepath_import_colnames)

names(results_prepared) <-
  names(results_prepared) %>%
  str_replace_all("^[2]", "two") %>%
  str_replace_all("^[3]", "three")

colnames_fix <-
  results_prepared_colnames %>% 
  pull(value) %>% 
  str_subset("^[2-3]")
colnames_fix

colnames_fixed <-
  str_c("`", colnames_fix, "`")
colnames_fixed


results_prepared_colnames <-
  results_prepared_colnames %>%
  mutate(value = str_replace_all(value, "^[2]", "two")) %>%
  mutate(value = str_replace_all(value, "^[3]", "three"))

#'
#'
#'
# results_calendar ----
# colnames_base <- results_prepared_colnames %>% filter(type == "base")
colnames_base <- c("date", "season", "tm")
colnames_calc_dates <-
  c("yyyy", "mm", "dd", "wd", "mm_yyyy", "mm_w")
colnames_viz <- c("g_td", "pd_h2a", "ortg_off_1g")

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

results_calendar_tm_tidy <-
  results_calendar_tm %>%
  # mutate_if(is.character, funs(as.factor)) %>%
  gather(metric, value, colnames_viz)

seasons <- c(2014, 2016)
wd_labels <- levels(results_calendar_tm$wd)
wd_labels[2:6] <- ""
wd_labels

results_calendar_tm_tidy %>%
  filter(season %in% seasons) %>%
  filter(metric == "pd_h2a") %>%
  ggplot() +
  geom_tile(aes(x = wd, y = mm_w, fill = value), colour = "white") +
  scale_y_reverse() +
  scale_x_discrete(labels = wd_labels) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "") +
  scale_fill_gradient(low = "cyan", high = "red") +
  facet_wrap(~ mm_yyyy, nrow = length(seasons))

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
# colnames_xy_123 ----
names(results_prepared)

colname_y_1 <- "pd_h2a"
colname_y_2 <- "result"
colname_y_3 <- "pd_h2a"

colnames_x_1_noopp <-
  c("pd_td",
    "ortg_off_td",
    "ortg_def_td",
    # "dist",
    # "g_last7",
    "dist_last7",
    "drest",
    # "drest_2",
    # "b2b",
    # "no2drestin4",
    # "no2drestin5",
    "rtrip")

colnames_x_1_opp <-
  colnames_x_1_noopp %>%
  str_c("_opp")

colnames_x_1 <- c(colnames_x_1_noopp, colnames_x_1_opp)
# colnames_x_1 <- c(colnames_x_1_noopp)
colnames_x_1

colnames_x_1b <- colnames_x_1_noopp
colnames_x_2 <- colnames_x_1

colnames_x_3 <-
  results_prepared_colnames %>%
  # filter(type %in% c("calc", "calc_opp")) %>%
  filter(type %in% c("off_td", "def_td", "off_td_opp", "def_td_opp")) %>%
  pull(value)
colnames_x_3

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
# data_0 ----#'
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
# data_123 ----
data_1 <-
  data_0 %>%
  select(one_of(c(colname_y_1, colnames_x_1)))

data_2 <-
  data_0 %>%
  mutate_at(colname_y_2, as.factor) %>%
  select(one_of(c(colname_y_2, colnames_x_2)))

data_3 <-
  data_0 %>%
  select(one_of(c(colname_y_3, colnames_x_3)))

#'
#'
#'
# data_xy_123 ----
# Doing this because the predictors and response variable data
# must be treated separately
# in some analysis to be done, and because some functions require
# matrices as inputs.
data_x_1 <-
  data_1 %>%
  select(one_of(colnames_x_1)) %>%
  as.matrix() %>%
  na.exclude()

data_y_1 <-
  data_1 %>%
  select(one_of(colname_y_1)) %>%
  as.matrix() %>%
  na.exclude()

data_x_3 <-
  data_3 %>%
  select(one_of(colnames_x_3)) %>%
  as.matrix() %>%
  na.exclude()

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  # rm(list = c("results_prepared"))
  rm(list = c("data_0"))
}

#'
#'
#'
# get_fmla ----
get_fmla <- function(var_y, vars_x) {
  paste0(var_y, " ~ ", paste(vars_x, collapse = " + ")) %>%
    as.formula()
}

fmla_1 <- get_fmla(colname_y_1, colnames_x_1)
fmla_1b <- get_fmla(colname_y_1, colnames_x_1_noopp)
fmla_1c <- get_fmla(colname_y_1, colnames_x_1_opp)
fmla_2 <- get_fmla(colname_y_2, colnames_x_1)
fmla_3 <- get_fmla(colname_y_3, colnames_x_3)

#'
#' # References
#'
#' The sources of most of the code that is emulated in this code walk-through include:
#' + The caret package e-book
#' + The r.statistics.co website.
#' + The "R for Statistical Learning" e-book.
#'
#' The caret package e-book can be found at https://topepo.github.io/caret.
#' The caret package provides a common interface for working with over 200 different
#' types of models in R. (In a way, it might be
#' compared to the scikitlearn library
#' for python.)
#' Additionally, the caret package and its accompanying e-book are written by the authors of the
#' AppliedPredictiveModeling book (and package), which is considered one
#' of the best resources for learning machine learning techniques in R.
#' The caret e-book itself  is similar
#' to the AppliedPredictiveModeling book
#'
#' The r.statistics.co has several web-pages for topics that are not specific
#' to any one type of model, including pages for
#' statistical tests, missing value treatment,
#' outlier analysis, ffeature selection, and model selection .
#' For specific model types, it has web pages for linear regression, logistic regression,
#' and a placeholder page for advanced linear regression, which links to different
#' types of linear regression.
#'
#' The "R for Statistical Learning" e-book covers hypothesis testing, prediction,
#' unusual observations, and methods for adding complexity for linear models
#' in its chapter "4: Modeling Basics in R".
#' In regards to specific model types, the e-book
#' covers linear and logistic regression in more depth in its
#' sixth and tenth chapters ("Linear Models" and "Logistic Regression") respectively.
#' Additionally, it has more chapter dedicated to other model types and machine learning
#' techniques. Of these other chapters, the material in "Generative Models",
#' "20: Resampling", "21: The caret Package", "22: Subset Selection", "24: Regularization",
#' "25: Elastic Net" have additional information that could be implemented in
#' demonstration of sampling, validation, and other topics discussed here.
#'
#' Drawing strong influence from the ISLR book,
#' the "R for Statistical Learning" e-book covers
#' nearly all of the same concepts covered in the ISLR book.
#' (It even uses a nearly identical
#' structure and many of the same data sets to illustrate the same concepts.)
#' Consequently, in order to avoid redundancy,
#' the ISLR book is not referenced explicitly throughout this code-walkthrough.
#'
#' # Pre-Processing
#'
#' This sections roughly follows the outline of the chapter "3 Pre-Processing" in the
#' caret e-book. (Note that this chapter has sections for creating dummy variables,
#' centering and scaling, imputation, trnansformation,
#' and calculating class distances (for the purpose of creating new predictors)
#' that are not covered explicitly here.)
#'
#' ### Using the caret package. [^fn_only_caret]
#'
# caret: "3.2 Identifying Zero- and Near Zero-Variance Predictors. ####
nzv_1_metrics <- nearZeroVar(data_x_1, saveMetrics = TRUE)
nzv_1_metrics

nzv_3_metrics <- nearZeroVar(data_x_3, saveMetrics = TRUE)
nzv_3_metrics
# nzv_3_metrics[nzv_metrics$nzv_metrics , ][1:10, ]

# nzv_3 <- nearZeroVar(data_x_3, saveMetrics = FALSE
# data_1_filtered <- data_x_1[, -nzv_3]

# caret: "3.3 Identifying Correlated Predictors" ####
data_x_1_cor <- cor(data_x_1)
summary(data_x_1_cor[upper.tri(data_x_1_cor)])

data_x_1_corhigh <-
  findCorrelation(cor(data_x_1), cutoff = .75)
data_x_1_corhigh

data_x_3_corhigh <-
  findCorrelation(cor(data_x_3), cutoff = .75)
data_x_3_corhigh

data_x_3_nocorhigh <- data_x_3[, -data_x_3_corhigh]
data_x_3_nocorhigh_cor <- cor(data_x_3_nocorhigh)
summary(data_x_3_nocorhigh_cor[upper.tri(data_x_3_nocorhigh_cor)])

# # caret: "3.4 Linear Dependencies" ####
lcombo_info_1 <- findLinearCombos(data_x_1)
lcombo_info_1

lcombo_info_3 <- findLinearCombos(data_x_3)
lcombo_info_3

data_x_3_nolcombo <- data_x_3[,-lcombo_info_3$remove]

# # caret: "3.5 The `preProcess' function"
# (This is re-shown in the the section "4.1 Simple Splitting Based on the Outcome".
# The example shown in the
# section "4.2 Splitting Based on the Predictors" is not implemented.)
#
# use the same training rows for both the quantitiatve and qualitative models.
# Here, use the createDataPartition() function with the qualitative data
# in order to get a more representative sample of Ws and Ls. If the
# quantitative data set were used, the testing and training sets might not
# be split in the same manner.
set.seed(seed)
trn_2_idx <-
  createDataPartition(data_x_1,
                      p = 0.8,
                      list = FALSE,
                      times = 1)

head(trn_2_idx)
data_1_trn <- data_1[trn_2_idx,] %>% na.exclude()
data_1_tst <- data_1[-trn_2_idx,] %>% na.exclude()
data_3_trn <- data_3[trn_2_idx,] %>% na.exclude()
data_3_tst <- data_3[-trn_2_idx,] %>% na.exclude()

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("nzv_1_metrics", "nzv_3_metrics", "nzv_3"))
  rm(
    list = c(
      "data_x_1_cor",
      "data_x_1_corhigh",
      "data_x_3_corhigh",
      # "data_x_3_nocorhigh",
      "data_x_3_nocorhigh_cor"
    )
  )
  rm(list = c(# "data_3_x_nolcombo",
    "lcombo_info_1",
    "lcombo_info_3"))
}

#'
#' ### Using custon techniques. [^fn_ggally]
#'
#' [^fn_ggally]:
#' I believe I originally discovered the GGally package
#'  when reading through the Quick-R e-book.
#'
# custom: pre-processing ####
GGally::ggcorr(
  data_1[,-1],
  label = TRUE,
  label_round = 2,
  label_size = 3
)

#'
#'
#' # Linear Regression.
#'
#' ## Creating and evaluating.
#'
#' ### Using a basic approach. [^fn_lm_basic]
#'
#' The methods for creating and reviewing the most basic
#'  model diagnostics for a linear regression model in R is fairly universal.
#'
#' [^fn_lm_basic]:
#' This code is not emulated from any one particular source in specific.
#'
# basic: lm_1 ####
lm_1 <- lm(fmla_1, data = data_1_trn)
lm_1

# Fitted model.
summary(lm_1)
summary(lm_1)$coef
coef(lm_1)

car::vif(lm_1)
#'
#' ### Using the broom package. [^fn_broom_link]
#'
#' [^fn_broom_link]
#' https://cran.r-project.org/web/packages/broom/vignettes/broom.html
#
# broom: lm_1 ####
broom::tidy(lm_1)
broom::glance(lm_1)
head(broom::augment(lm_1))

#'
#' ### Using the caret package.
#'
# caret: lm_1 ####
# Need to use na.exclude() because the train() function is able to detect
# the missing rows.
set.seed(seed)
lm_1_caret <-
  train(fmla_1, data = data_1_trn, method = "lm")
lm_1_caret$finalModel
summary(lm_1_caret$finalModel)
summary(lm_1_caret)$r.squared
summary(lm_1_caret$finalModel)$r.squared

#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("lm_1_caret"))
}

#'
#' ### Using techniques shown in "R for Statistical Learning"
#'
# r4sl: lm_1 ####
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
get_rmse <- function(model, data, response) {
  rmse(actual = data[, response],
       predicted = predict(model, data))
}

lm_1b <- lm(fmla_1b, data = data_1_trn)
lm_1c <- lm(fmla_1c, data = data_1_trn)

lm_1_list <- list(lm_1, lm_1b, lm_1c)

lm_1_trn_rmse <-
  sapply(lm_1_list, get_rmse, data = data_1_trn, response = colname_y_1)
lm_1_trn_rmse
lm_1_tst_rmse <-
  sapply(lm_1_list, get_rmse, data = data_1_tst, response = colname_y_1)
lm_1_tst_rmse


fmla_3b <- get_fmla(colname_y_3, colnames(data_x_3_nocorhigh))
fmla_3c <- get_fmla(colname_y_3, colnames(data_x_3_nolcombo))
lm_3 <- lm(fmla_3, data = data_3_trn)
lm_3b <- lm(fmla_3b, data = data_3_trn)
lm_3c <- lm(fmla_3c, data = data_3_trn)

lm_3_list <- list(lm_3, lm_3b, lm_3c)

lm_3_trn_rmse <-
  sapply(lm_3_list, get_rmse, data = data_3_trn, response = colname_y_3)
lm_3_trn_rmse
lm_3_tst_rmse <-
  sapply(lm_3_list, get_rmse, data = data_3_tst, response = colname_y_3)
lm_3_tst_rmse

#'
#' ### Using techniques shown at r.statistic.co.
#'
#' This section follows (somoe of) the examples on the "Linear Regression" webpage.
#' [^http://r-statistics.co/Linear-Regression.html]
#'
# rsco: "Predicting Linear Models" ----
## "Step 2: Develop the model on the training data and use it to predict the distance on test data"
lm_1_predict <- predict(lm_1, newdata = data_1_tst)

# ## "Step 4: Calculate prediction accuracy and error rates"
data_1_tst_y <- data_1_tst %>% pull(colname_y_1)
lm_1_compare <-
  bind_cols(actual = data_1_tst_y,
            predicted = lm_1_predict)
corr_accuracy <- cor(lm_1_compare)[2]
corr_accuracy

minmax_accuracy <-
  mean(apply(lm_1_compare, 1, min) / apply(lm_1_compare, 1, max))
minmax_accuracy

mape <-
  mean(abs((
    lm_1_compare$predicted - lm_1_compare$actual
  )) / lm_1_compare$actual)
mape

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("lm_1_predict", "data_1_tst_y", "lm_1_compare"))
}

#'
#' ### Using custom techniques.
#'
lm_1_z <-
  # scale(lm_1_$fitted.values)[,1]
  (lm_1$fitted.values - mean(lm_1$fitted.values)) / sd(lm_1$fitted.values)
summary(lm_1_z)

lm_1_p <- pnorm(lm_1_z)
summary(lm_1_p)

lm_1_diagnostics <-
  lm_1_p %>%
  as_tibble() %>%
  rename(prob = value) %>%
  bind_cols(
    lm_1_z %>% as_tibble() %>% rename(z = value),
    lm_1$model[[1]] %>% as_tibble() %>% rename(actual = value),
    lm_1$fitted.values %>% as_tibble() %>% rename(fitted = value),
    lm_1$residuals %>% tbl_df() %>% rename(residual = value)
  ) %>%
  select(actual, fitted, residual, z, prob)

lm_1_diagnostics %>%
  arrange(desc(fitted))

lm_1_diagnostics %>%
  filter(prob > 0.95)

calc_pct <- function(x, n, digits = 6) {
  round(sum(x) / max(n), digits)
}

lm_1_diagnostics_calcs <-
  lm_1_diagnostics %>%
  mutate(
    true_positive = if_else(actual > 0 & fitted > 0, 1, 0),
    true_negative = if_else(actual < 0 & fitted < 0, 1, 0),
    false_positive = if_else(actual < 0 & fitted > 0, 1, 0),
    false_negative = if_else(actual > 0 & fitted < 0, 1, 0)
  )

calc_pct(lm_1_diagnostics_calcs$true_positive,
         nrow(lm_1_diagnostics_calcs))
lm_1_diagnostics_calcs %>%
  summarise_all(calc_pct, nrow(lm_1_diagnostics_calcs))
# summarise(
#   true_positive_pct = sum(true_positive) / n(),
#   true_negative_pct = sum(true_negative) / first(n),
#   false_positive_pct = sum(false_positive) / first(n),
#   false_negative_pct = sum(false_negative) / first(n)
# )
#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(
    list = c(
      "lm_1_z",
      "lm_1_p",
      "lm_1_diagnostics",
      "calc_pct",
      "lm_1_diagnostics_calcs"
    )
  )
}

#'
#' ### Performing feature/model selection.
#'
#'
#' This section follows (somoe of) the examples on the "Feature Selection" webpage.
#' [^http://r-statistics.co/Model-Selection-in-R.html]
#'
# # "1. Random Forest Method"
# # Note that this can take a while.
# # library("party")
# cf_1 <- party::cforest(fmla_1, data = data_1, control= party::cforest_unbiased(mtry=2, ntree=50))
# party::varimp(cf_1)
# party::varimp(cf_1, conditional = TRUE)
# party::varimpAUC(cf_1)

# # "2. Relative Imporatnce"
# library("realimpo")
relimp_1 <- relaimpo::calc.relimp(lm_1, type = "lmg", rela = TRUE)
sort(relimp_1$lmg, decreasing = TRUE)

# relimp_3 <- relaimpo::calc.relimp(lm_3, type = "lmg", rela = TRUE)
# sort(relimp_3$lmg, decreasing = TRUE)

# # "4. MARS"
# library("earth")
mars_1 <- earth::earth(fmla_1, data = data_1)
earth::evimp(mars_1)

# mars_3 <- earth::earth(fmla_3, data = data_3)
# earth::evimp(mars_3)

# # "5. Step-wise Regression"
fmla_1_base <- get_fmla("pd_h2a", 1)
fmla_1_base
lm_1_base <- lm(fmla_1_base, data = data_1)
lm_1_all <- lm(fmla_1, data = data_1)
lm_1_step <-
  step(
    lm_1_base,
    scope = list(lower = lm_1_base, upper = lm_1_all),
    direction = "both",
    trace = 0,
    steps = 1000
  )
vars_shortlist_1 <- names(unlist(lm_1_step[[1]]))
vars_shortlist_1 <-
  vars_shortlist_1[!vars_shortlist_1 %in% "(Intercept)"]
vars_shortlist_1

# lm_3_step <- step(lm_3, direction = "backward")

# # "6. Boruta"
# Note that this can take a while.
# library("Boruta")
# boruta_1 <- Boruta::Boruta(fmla_1, data = data_1, doTrace = 2)
# boruta_signif_1 <-
#   names(boruta_1$finalDecision[boruta_1$finalDecision %in% c("Confirmed", "Tentative")])
# boruta_signif_1
# plot(boruta_1)

# # "7. Information Value (IV) and Weights of Evidence (WOE)"
# Note these methods depend on categorical predictros, so they are not implemented here.
# "WOE provides a method of recoding
# a categorical X variable to a continuous variable."
# IV is a measure of the predictive capability of a categorical x variable
# to accurately predict the goods and bads."

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(
    list = c(
      "cf_1",
      "cf_3",
      "relimp_1",
      "relimp_3",
      "mars_1",
      "mars_3",
      "fmla_1_base",
      "lm_1_base",
      "lm_1_all",
      "lm_1_step",
      "lm_3_step",
      "vars_shortlist_1",
      "boruta_1",
      "boruta_signif_1"
    )
  )
}
#'
#' This section follows (some of) the examples on the "Model Selection" webpage.
#' [^http://r-statistics.co/Variable-Selection-and-Importance-With-R.html]
#' (However, the methods presented are applicable to feature selection as well.)
#'
# Not completely necessary to split between training and testing sets
# for model selection.
# rsco:  "Stepwise Regression" ----
lm_1_step <- step(lm_1, direction = "both")
# "backward" returns the same result.
summary(lm_1_step)

# rsco: "Multicollinearity and Statistical Significance" ----
## "Recursively remove variables with VIF > 4" (based on VIF values)
remove_vars_nonsignif_vif <-
  function(lm_fit, data_trn, threshold = 4) {
    # lm_fit <- lm_1
    # data_trn <- data_1_trn
    # threshold <- 4
    
    vifs <- car::vif(lm_fit)
    x_vars <- names(vifs)
    x_vars
    
    y_var <- names(lm_fit$model[1])
    while (any(vifs > threshold)) {
      var_with_max_vif <-
        names(which(vifs == max(vifs)))
      var_with_max_vif
      x_vars <- x_vars[!(x_vars) %in% var_with_max_vif]
      x_vars
      fmla_new <-
        as.formula(paste0(y_var, " ~ ", paste(x_vars, collapse = " + ")))
      lm_fit_new <- lm(fmla_new, data = data_trn)
      vifs <- car::vif(lm_fit_new)
      vifs
    }
    if (missing(lm_fit_new)) {
      lm_fit
    } else {
      lm_fit_new
    }
  }

lm_1_onlysignif_vif <-
  remove_vars_nonsignif_vif(lm_1, data_1)
summary(lm_1_onlysignif_vif)

## "Recursively remove non-significant variables" (based on p values)
remove_vars_nonsignif_p <-
  function(lm_fit,
           data_trn,
           threshold = 0.1,
           no_intercept = FALSE) {
    # lm_fit <- lm_1
    # data_trn <- data_1_trn
    # threshold <- 0.1
    # intercept <- FALSE
    
    get_non_signif <- function(lm_fit, threshold, intercept) {
      p_vals <- summary(lm_fit)[[4]][, 4]
      not_signif <- character()
      not_signif <- names(which(p_vals > threshold))
      if (no_intercept == FALSE) {
        not_signif <- not_signif[!not_signif %in% "(Intercept)"]
      }
      not_signif
    }
    if (no_intercept == FALSE) {
      x_vars <- names(lm_fit[[1]])[-1]
    } else {
      x_vars <- names(lm_fit[[1]])
    }
    x_vars
    not_signif <- get_non_signif(lm_fit, threshold, intercept)
    not_signif
    
    y_var <- names(lm_fit$model[1])
    while ((length(not_signif) > 0)) {
      x_vars <- x_vars[!x_vars %in% not_signif[1]]
      x_vars
      fmla_new <-
        as.formula(paste0(y_var, " ~ ", paste(x_vars, collapse = " + ")))
      lm_fit_new <- lm(fmla_new, data = data_trn)
      
      not_signif <- get_non_signif(lm_fit_new, threshold, intercept)
      not_signif
    }
    if (missing(lm_fit_new)) {
      lm_fit
    } else {
      lm_fit_new
    }
  }

lm_1_onlysignif_p <-
  remove_vars_nonsignif_p(lm_1, data_1)
summary(lm_1_onlysignif_p)

# rsco: "Best subsets" ----
regsubsets_1 <-
  regsubsets(x = data_x_1,
             y = data_y_1,
             nvmax = (ncol(data_1) - 1))
regsubsets_1
plot(regsubsets_1, scale = "adjr2")

regsubsets_1 <-
  regsubsets(
    x = data_x_1,
    y = data_y_1,
    nbest = 2,
    really.big  = TRUE
  )
regsubsets_1
plot(regsubsets_1, scale = "adjr2")

# rsco: "Leaps" ----
# leaps() implements a different method than
# regsubsets() (and has different parameters.)
leaps_1 <- leaps(
  x = data_x_1,
  y = data_y_1,
  nbest = 1,
  method = "adjr2"
)
leaps_1

# num_vars <- 4
# leaps_1_idx <- leaps_1$which[num_vars,]
# Could create a new model using the best variables...

# rsco: `RegBest()` from FactoMineR" ----
# library("FactoMineR")
regbest_1 <- FactoMineR::RegBest(x = data_x_1,
                                 y = data_y_1)
regbest_1$all
regbest_1$best

# rsco: "Simulated Annealing" ----
# library("subselect")
set.seed(seed)
anneal_1 <-
  subselect::anneal(
    cor(data_x_1),
    kmin = 1,
    kmax = ncol(data_x_1),
    nsol = ncol(data_x_1),
    niter = 10,
    setseed = TRUE
  )
anneal_1$bestsets

# num_vars <- 4
# vars_best_idx <- anneal_1$bestsets[num_vars, 1:num_vars]
# data_x_1[vars_best_idx]
# Could create a new model using the best variables...

# rsco: "Comparing Models using ANOVA" ----
fmla_1b_qut <- get_fmla(colname_y_1b, colnames_x_1_noopp)
lm_1b_qut <- lm(fmla_1b_qut, data = data_1_trn)
lm_1b_qut
anova(lm_1, lm_1b_qut)

# The null hypothesis is that the two models are equal in
# fitting the data (i.e. the Y variable), while
# the alternative hypothesis is that the full model is better
# (i.e. the additional X variable improves the model)."
#  Thus, if significance is shown between two models, then it means
# that the removed variables are significant.

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(
    list = c(
      "lm_1_step",
      "remove_vars_nonsignif_p",
      "lm_1_onlysignif_p",
      "remove_vars_nonsignif_vif",
      "lm_1_onlysignif_vif",
      "regsubsets_1",
      "leaps_1",
      "regbest_1",
      "anneal_1"
    )
  )
}

#'
#' ### Using custom techniques.
#'
# custom: regsubsets_1 ----
# Set nvmax equal to the maximum allowable number of variables.
regsubsets_1 <-
  regsubsets(x = data_x_1,
             y = data_y_1,
             nvmax = ncol(data_x_1))
regsubsets_1_summ <- summary(regsubsets_1)
# regsubsets_1_summ

display_regsubsets_coefs <- function(fit, value) {
  fit %>%
    coef(value) %>%
    as_tibble() %>%
    tibble::rownames_to_column()
}

regsubsets_1 %>%
  display_regsubsets_coefs(which.max(regsubsets_1_summ$adjr2))
regsubsets_1 %>%
  display_regsubsets_coefs(which.min(regsubsets_1_summ$cp))
regsubsets_1 %>%
  display_regsubsets_coefs(which.min(regsubsets_1_summ$bic))

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Don't add '()' to normalize in mutate_all unless specifying '(.)'
regsubsets_1_summ_tidy <-
  bind_cols(
    regsubsets_1_summ$adjr2 %>% as_tibble() %>% rename(adjr2 = value),
    regsubsets_1_summ$cp %>% as_tibble() %>% rename(cp = value),
    regsubsets_1_summ$bic %>% as_tibble() %>%  rename(bic = value)
  ) %>%
  mutate_all(funs(normalize)) %>%
  mutate(num_vars = row_number()) %>%
  select(num_vars, everything()) %>%
  gather(metric, value,-num_vars)
regsubsets_1_summ_tidy

regsubsets_1_summ_tidy %>%
  ggplot(aes(x = num_vars, y = value, color = metric)) +
  geom_point() +
  geom_line()

analyze_regsubsets <- function(regsubsets_qut, norm = TRUE) {
  regsubsets_summ <- summary(regsubsets_qut)
  regsubsets_summ_tidy <-
    bind_cols(
      regsubsets_summ$adjr2 %>% as_tibble() %>% rename(adjr2 = value),
      regsubsets_summ$cp %>% as_tibble() %>% rename(cp = value),
      regsubsets_summ$bic %>% as_tibble() %>%  rename(bic = value)
    )
  
  if (norm == TRUE) {
    regsubsets_summ_tidy <-
      regsubsets_summ_tidy %>%
      mutate_all(funs(normalize))
  }
  regsubsets_summ_tidy <-
    regsubsets_summ_tidy %>%
    mutate(num_vars = row_number()) %>%
    select(num_vars, everything()) %>%
    gather(metric, value,-num_vars)
  regsubsets_summ_tidy
}

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(
    list = c(
      "regsubsets_1",
      "regsubsets_1_summ",
      "display_regsubsets_coefs",
      "normalize",
      "regsubsets_1_summ_tidy",
      "analyze_regsubsets"
    )
  )
}

#'
#' # Logistic Regression
#'
#' ## Creating and evaluting the model.
#'
#' ### Using a basic approach. [^fn_lm_basic]
#'
# basic: glm_2 ----
# glm_2 <- glm(fmla_2, data = data_2_trn, family = binomial(link = "logit"))
glm_2 <-
  glm(fmla_2, data = data_2_trn, family = "binomial")
glm_2

summary(glm_2)
summary(glm_2)$coef
coef(glm_2)

car::vif(glm_2)

#'
#' ### Using the broom bakcage. [^fn_broom_link]
#'
#' The same broom functions shown before could be used in the same fashion.
#'
#' ### Using the caret package.
#'
# caret: glm_2 ----
set.seed(seed)
glm_2_caret <-
  train(fmla_2,
        data = na.exclude(data_2_trn),
        method = "glm")
glm_2_caret$finalModel
summary(glm_2_caret$finalModel)
summary(glm_2_caret)$deviance
summary(glm_2_caret$finalModel)$deviance

#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("glm_2_caret"))
}

#'
#' ## Creating and evaluating.
#'
#' ### Following the example at r.statistics.co
#'
#' http://r-statistics.co/Logistic-Regression-With-R.html
#'
# rsco: "Check class bias." ---
table(select(results_prepared, one_of(colname_y_2)))
table(select(data_2, colname_y_2))

# rsco: "Create Training Data" ----
# filter(select(data_1, colname_y_2), pd_h2a < 0)
# mutate(select(data_2, colname_y_2), !!(colname_y_2) := colname_y_2)
# ...

# rsco: "Create WOE for Categorical Variables (optional)" ----
# ...

#
# rsco: "Compute Informational Values" ----
# library("smbinning")
vars_fct <- NULL
vars_cont <- colnames(data_x_1)
ivs <-
  data.frame(var = c(vars_fct, vars_cont), iv = numeric(ncol(data_x_1)))

# Converting to fit requirements for smbinning() function.
data_2_binary <-
  data_2 %>%
  mutate(result = as.numeric(if_else(result == "W", 1, 0))) %>%
  as.data.frame()

for (var_cont in vars_cont) {
  # var_cont <- "pd_td_opp"
  smb <-
    smbinning::smbinning(data_2_binary, y = colname_y_2, x = var_cont)
  smb
  if (class(smb) != "character") {
    ivs[ivs$var == var_cont, "iv"] <- smb$iv
  }
}

ivs <- ivs[order(-ivs$iv),]
ivs

# rsco: "Build Logit Models and Predict" ----
# library("InformationValue")
data_2_binary_trn <-
  data_2_binary[trn_2_idx,] %>% na.omit()
head(data_2_binary_trn)
data_2_binary_tst <-
  data_2_binary[-trn_2_idx,] %>% na.omit()
head(data_2_binary_tst)

# glm_2_binary <- glm(fmla_2, data = data_2_binary_trn, family = binomial(link = "logit"))
glm_2_binary <-
  glm(fmla_2, data = data_2_binary_trn, family = "binomial")
# glm_2_predict <- plogis(predict(glm_2, newdata = data_2_tst))
glm_2_predict <-
  predict(glm_2, newdata = data_2_binary_tst, type = "response")

## "Decide on optimal prediction probability cutoff for the model"
data_2_tst_y <-
  data_2_tst %>%
  mutate(result = as.numeric(if_else(result == "W", 1, 0))) %>%
  pull(result)
head(data_2_tst_y)
cutoff_optimal <-
  InformationValue::optimalCutoff(actuals = data_2_tst_y , predictedScores = glm_2_predict)
cutoff_optimal

# rsco: "Model Diagnostics" ----
# summary(glm_2_binary)

## "VIF"
# car::vic(glm_2_binary)

## "Misclassification Error"
InformationValue::misClassError(actuals = data_2_tst_y,
                                predictedScores = glm_2_predict,
                                threshold = cutoff_optimal)

## "ROC"
InformationValue::plotROC(actuals = data_2_tst_y, predictedScores = glm_2_predict)

## "Concordance"
InformationValue::Concordance(actuals = data_2_tst_y, predictedScores = glm_2_predict)

## "Specificity and Sensitivity"
InformationValue::specificity(actuals = data_2_tst_y,
                              predictedScores = glm_2_predict,
                              threshold = cutoff_optimal)
InformationValue::sensitivity(actuals = data_2_tst_y,
                              predictedScores = glm_2_predict,
                              threshold = cutoff_optimal)

## "Confusion Matrix"
InformationValue::confusionMatrix(actuals = data_2_tst_y,
                                  predictedScores = glm_2_predict,
                                  threshold = cutoff_optimal)

#'
#' ### Using the glmnet package.
#'
# glmnet: glmnet_2 ----
glmnet_2 <-
  glmnet(as.matrix(data_2_trn[, -1]), as.matrix(data_2_trn[, 1]), family = "binomial")

# Warning: Do not print the variable directly!
summary(glmnet_2)
plot(glmnet_2)
coef(glmnet_2, s = 0.1)

#'
#' ### Using the caret() package.
#'
# caret: glmnet_2 ----
glmnet_2_caret <-
  train(fmla_2, data = data_2_trn, method = "glmnet")
summary(glmnet_2_caret$finalModel)
summary(glmnet_2_caret$finalModel)$r.squared
