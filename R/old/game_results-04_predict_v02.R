



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
export <- TRUE
remove_tempvars <- TRUE
run_caret <- FALSE

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
names(results_prepared)

colname_1_y_qut <- "pd_h2a"
colname_1_y_qul <- "result"
colname_2_y <- "result"

# colnames_1_x_home_only <- c("hfa")
colnames_1_x_home_only <- NULL

colnames_1_x_noopp <-
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

colnames_1_x_opp <-
  colnames_1_x_noopp %>%
  str_c("_opp")
# colnames_1_x_stats_opp
colnames_1_x <- c(colnames_1_x_noopp, colnames_1_x_opp)
# colnames_1_x <- c(colnames_1_x_noopp)
colnames_1_x

colnames_2_x <-
  results_prepared_colnames %>%
  # filter(type %in% c("calc", "calc_opp")) %>%
  filter(type %in% c("off_td", "def_td", "off_td_opp", "def_td_opp")) %>% 
  pull(value)
colnames_2_x

#'
#'
#'
# Debugging...
# This should return nothing.
setdiff(colnames_1_x, names(results_prepared))
# # Or...
# setdiff(colnames_1_x, results_prepared_colnames$value)
#'
#'
#'
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
data_1_qut <-
  data_0 %>%
  select(one_of(c(colname_1_y_qut, colnames_1_x)))

data_1_qul <-
  data_0 %>%
  mutate_at(colname_1_y_qul, as.factor) %>%
  select(one_of(c(colname_1_y_qul, colnames_1_x)))

data_2 <-
  data_0 %>%
  mutate_at(colname_2_y, as.factor) %>%
  select(one_of(c(colname_2_y, colnames_2_x)))

#'
#'
#'
# Doing this because the predictors and response variable data must be treated separately
# in some analysis to be done, and because some functions require
# matrices as inputs.
data_1_x <-
  select(data_1_qut, one_of(colnames_1_x)) %>% 
  asmatrix() %>% 
  na.exclude()
data_1_qut_y <-
  select(data_1_qut, one_of(colname_1_y_qut)) %>%
  as.matrix() %>% 
  na.exclude()
data_1_qul_y <-
  select(data_1_qul, one_of(colname_1_y_qul)) %>%
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
get_fmla <- function(var_y, vars_x) {
  paste0(var_y, " ~ ", paste(vars_x, collapse = " + ")) %>%
    as.formula()
}

fmla_1_qut <- get_fmla(colname_1_y_qut, colnames_1_x)
fmla_1_qul <- get_fmla(colname_1_y_qul, colnames_1_x)
fmla_2 <- get_fmla(colname_2_y, colnames_2_x)

#'
#' # References {.title}
#'
#' The sources of most of the code that is emulated in this code walk-through include:
#' + The caret package e-book
#' + The r.statistics.co website.
#' + The "R for Statistical Learning" e-book.
#'
#' The caret package e-book can be found at https://topepo.github.io/caret.
#' The caret package provides a common interface for working with over 200 different
#' types of models in R. (In a way, it might be compared to the scikitlearn library
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
#' in its chapter "4: Modeling Basics in R". In regards to specific model types, the e-book
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
#' ## Identifying Zero- (and Near Zero-Variance) Predictors.
#'
#' ### Using the caret package. [^fn_only_caret]
#'
#' This follows the example in the section "3.2 Zero- and Near Zero-Variance Predictors"
#' in the caret e-book.
#'
nzv_metrics <- nearZeroVar(data_1_x, saveMetrics = TRUE)
nzv_metrics
nzv_metrics[nzv_metrics$nzv_metrics ,][1:10,]

nzv <- nearZeroVar(data_1_x)
nzv
# data_1_qut_filtered <- data_1_x[, -nzv]

#'
#'
#'
if (remove_tempvars == TRUE) {
  rm(list = c("nzv_metrics", "nzv"))
}

#'
#' [^fn_only_caret]:
#' The other resources either do not have explicit sections and/or code demonstrating
#' this concept, or this concept is not covered in such a different manner
#' that it is worth emulating here.
#'
#' ## Identifying correlated predictors.
#'
#' ### Using the caret package.[^fn_only_caret]
#'
#' This follows the example in the section "3.3 Identifying Correlated Predictors"
#' in the caret e-book.
#'
data_1_x_cor <- cor(data_1_qut[,-1])
summary(data_1_x_cor[upper.tri(data_1_x_cor)])

data_1_x_cor_high <-
  findCorrelation(cor(data_1_qut[,-1]), cutoff = .75)
data_1_x_cor_high
# data_1_x_filtered <- data_1_x[,-data_1_x_cor_high]
# data_1_x_filtered
# data_1_x_filtered_cor <- cor(data_1_x_filtered)
# summary(data_1_x_filtered_cor[upper.tri(data_1_x_filtered_cor)])

#'
#'
#'
if (remove_tempvars == TRUE) {
  rm(list = c("data_1_x_cor", "data_1_x_cor_high"))
}
#'
#' ### Using my methods. [^fn_ggally]
#'
#' [^fn_ggally]:
#' I believe I originally discovered the GGally package when reading through the Quick-R e-book.
#'
GGally::ggcorr(
  data_1_qut[,-1],
  label = TRUE,
  label_round = 2,
  label_size = 3
)
#'
#' ## Identifying linear dependencies. [^fn_caret_only]
#'
#' ### using the caret package.
#'
#' This follows the example in the section "3.4 Linear Dependencies"
#' in the caret e-book.
#'
#'
combo_info <- findLinearCombos(data_1_qut[,-1])
combo_info
# data_1_x_filtered <- data_1_x[, -ccombo_info$remove]
#'
#'
#'
if (remove_tempvars == TRUE) {
  rm(list = c("combo_info"))
}
#'
#' ## Splitting the Data.
#'
#' ### Using the caret package. [^fn_caret_only]
#'
#' This follows the method show in the section "3.5 The `preProcess' function"
#' which is re-shown in the chapter "4 Data Splitting" in
#' the section "4.1 Simple Splitting Based on the Outcome".
#' The example shown in the
#' section "4.2 Splitting Based on the Predictors" is not implemented.
#
# use the same training rows for both the quantitiatve and qualitative models.
# Here, use the createDataPartition() function with the qualitative data
# in order to get a more representative sample of Ws and Ls. If the
# quantitative data set were used, the testing and training sets might not
# be split in the same manner.
set.seed(seed)
trn_1_qul_idx <-
  createDataPartition(data_1_x,
                      p = 0.8,
                      list = FALSE,
                      times = 1)

head(trn_1_qul_idx)
data_1_qut_trn <- data_1_qut[trn_1_qul_idx,] %>% na.exclude()
data_1_qut_tst <- data_1_qut[-trn_1_qul_idx,] %>% na.exclude()
data_1_qul_trn <- data_1_qul[trn_1_qul_idx,] %>% na.exclude()
data_1_qul_tst <- data_1_qul[-trn_1_qul_idx,] %>% na.exclude()

#'
#'
#' # Linear Regression.
#'
#' ## Creating and evaluting the model.
#'
#' ### Using a basic approach. [^fn_lm_basic]
#'
#' The methods for creating and reviewing the most basic
#'  model diagnostics for a linear regression model in R is fairly universal.
#'
#' [^fn_lm_basic]:
#' This code is not emulated from any one particular source in specific.
#'
lm_1_qut <- lm(fmla_1_qut, data = data_1_qut_trn)
lm_1_qut

# Fitted model.
summary(lm_1_qut)
summary(lm_1_qut)$coef
coef(lm_1_qut)

car::vif(lm_1_qut)
#'
#' ### Using the broom package. [^fn_broom_link]
#'
#' [^fn_broom_link]
#' https://cran.r-project.org/web/packages/broom/vignettes/broom.html
#
broom::tidy(lm_1_qut)
broom::glance(lm_1_qut)
head(broom::augment(lm_1_qut))

#'
#' ### Using the caret package.
#'
# Need to use na.exclude() because the train() function is able to detect
# the missing rows.
set.seed(seed)
lm_1_qut_caret <-
  train(fmla_1_qut, data = data_1_qut_trn, method = "lm")
lm_1_qut_caret$finalModel
summary(lm_1_qut_caret$finalModel)
summary(lm_1_qut_caret)$r.squared
summary(lm_1_qut_caret$finalModel)$r.squared

#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("lm_1_qut_caret"))
}

#'
#' ### Using techniques shown at r.statistic.co.
#'
#' #### Linear regression model diangostics.
#' 
#' 
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
get_rmse <- function(model, data, response) {
  rmse(actual <- data[, response], 
       predicted <- predict(model, data))
}

fit_1 <- lm(fmla_1_qut, data = data_1_x_trn)
fit_2 <- lm(fmla_2, data = data_1_x_trn)

model_list <- list(fit_1, fit_2, fit_3, fit_4, fit_5)

train_rmse <- sapply(model_list, get_rmse, data <- data_1_x_trn, response <- "Sales")
test_rmse <- sapply(model_list, get_rmse, data <- test_data, response <- "Sales")

#'
#' This section follows (somoe of) the examples on the "Linear Regression" webpage.
#' [^http://r-statistics.co/Linear-Regression.html]
#'
# # "Predicting Linear Models" ####
# ## "Step 2: Develop the model on the training data and use it to predict the distance on test data"
lm_1_qut_predict <- predict(lm_1_qut, newdata = data_1_qut_tst)

# ## "Step 4: Calculate prediction accuracy and error rates"
data_1_qut_tst_y <- data_1_qut[-trn_1_qul_idx, colname_1_y_qut]
lm_1_qut_compare <-
  bind_cols(actual = data_1_qut_tst_y,
            predicted = lm_1_qut_predict)
corr_accuracy <- cor(lm_1_qut_compare)[2]
corr_accuracy

minmax_accuracy <-
  mean(apply(lm_1_qut_compare, 1, min) / apply(lm_1_qut_compare, 1, max))
minmax_accuracy

mape <-
  mean(abs((
    lm_1_qut_compare$predicted - lm_1_qut_compare$actual
  )) / lm_1_qut_compare$actual)
mape

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("lm_1_qut_predict", "data_1_qut_tst_y", "lm_1_qut_compare"))
}

#'
#' ### Using custom techniques.
#'
lm_1_qut_z <-
  # scale(lm_1_qut_$fitted.values)[,1]
  (lm_1_qut$fitted.values - mean(lm_1_qut$fitted.values)) / sd(lm_1_qut$fitted.values)
summary(lm_1_qut_z)

lm_1_qut_p <- pnorm(lm_1_qut_z)
summary(lm_1_qut_p)

lm_1_qut_diagnostics <-
  lm_1_qut_p %>%
  as_tibble() %>%
  rename(prob = value) %>%
  bind_cols(
    lm_1_qut_z %>% as_tibble() %>% rename(z = value),
    lm_1_qut$model[[1]] %>% as_tibble() %>% rename(actual = value),
    lm_1_qut$fitted.values %>% as_tibble() %>% rename(fitted = value),
    lm_1_qut$residuals %>% tbl_df() %>% rename(residual = value)
  ) %>%
  select(actual, fitted, residual, z, prob)

lm_1_qut_diagnostics %>%
  arrange(desc(fitted))

lm_1_qut_diagnostics %>%
  filter(prob > 0.95)

calc_pct <- function(x, n, digits = 6) {
  round(sum(x) / max(n), digits)
}

lm_1_qut_diagnostics_calcs <-
  lm_1_qut_diagnostics %>%
  mutate(
    true_positive = if_else(actual > 0 & fitted > 0, 1, 0),
    true_negative = if_else(actual < 0 & fitted < 0, 1, 0),
    false_positive = if_else(actual < 0 & fitted > 0, 1, 0),
    false_negative = if_else(actual > 0 & fitted < 0, 1, 0)
  )

calc_pct(lm_1_qut_diagnostics_calcs$true_positive,
         nrow(lm_1_qut_diagnostics_calcs))
lm_1_qut_diagnostics_calcs %>%
  summarise_all(calc_pct, nrow(lm_1_qut_diagnostics_calcs))
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
      "lm_1_qut_z",
      "lm_1_qut_p",
      "lm_1_qut_diagnostics",
      "calc_pct",
      "lm_1_qut_diagnostics_calcs"
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
# cf <- party::cforest(fmla_1_qut, data = data_1_qut, control= party::cforest_unbiased(mtry=2, ntree=50))
# party::varimp(cf)
# party::varimp(cf, conditional = TRUE)
# party::varimpAUC(cf)

# # "2. Relative Imporatnce"
# library("realimpo")
relimp <- relaimpo::calc.relimp(lm_1_qut, type = "lmg", rela = TRUE)
sort(relimp$lmg, decreasing = TRUE)

# # "4. MARS"
# library("earth")
mars <- earth::earth(fmla_1_qut, data = data_1_qut)
earth::evimp(mars) 

# # "5. Step-wise Regression"
fmla_base <- get_fmla("pd_h2a", 1)
fmla_base
lm_1_qut_base <- lm(fmla_base, data = data_1_qut)
lm_1_qut_all <- lm(fmla_1_qut, data = data_1_qut)
lm_1_qut_step <- step(lm_1_qut_base, scope = list(lower = lm_1_qut_base, upper = lm_1_qut_all), direction = "both", trace = 0, steps = 1000)
vars_shortlist <- names(unlist(lm_1_qut_step[[1]]))
vars_shortlist <- vars_shortlist[!vars_shortlist %in% "(Intercept)"] 
vars_shortlist

# # "6. Boruta"
# Note that this can take a while.
# library("Boruta")
boruta <- Boruta::Boruta(fmla_1_qut, data = data_1_qut, doTrace = 2)
boruta_signif <- names(boruta$finalDecision[boruta$finalDecision %in% c("Confirmed", "Tentative")])
boruta_signif
plot(boruta) 

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
      "cf",
      "relimp",
      "mars",
      "fmla_base",
      "lm_1_qut_base",
      "lm_1_qut_all",
      "lm_1_qut_step",
      "vars_shortlist",
      "boruta",
      "boruta_signif"
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
# # "Stepwise Regression" ####
lm_1_qut_step <- step(lm_1_qut, direction = "both")
# "backward" returns the same result.
summary(lm_1_qut_step)

# # "Multicollinearity and Statistical Significance" ####
# ## "Recursively remove variables with VIF > 4" (based on VIF values)
remove_vars_nonsignif_vif <-
  function(lm_fit, data_trn, threshold = 4) {
    # lm_fit <- lm_1_qut
    # data_trn <- data_1_qut_trn
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

lm_1_qut_onlysignif_vif <-
  remove_vars_nonsignif_vif(lm_1_qut, data_1_qut)
summary(lm_1_qut_onlysignif_vif)

# ## "Recursively remove non-significant variables" (based on p values)
remove_vars_nonsignif_p <-
  function(lm_fit,
           data_trn,
           threshold = 0.1,
           no_intercept = FALSE) {
    # lm_fit <- lm_1_qut
    # data_trn <- data_1_qut_trn
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

# Could use the stepwise regressed fit here too.
lm_1_qut_onlysignif_p <-
  remove_vars_nonsignif_p(lm_1_qut, data_1_qut)
summary(lm_1_qut_onlysignif_p)

# # "Best subsets" ####
regsubsets_1_qut <-
  regsubsets(x = data_1_x,
             y = data_1_qut_y,
             nvmax = (ncol(data_1_qut) - 1))
regsubsets_1_qut
plot(regsubsets_1_qut, scale = "adjr2")

regsubsets_1_qut <-
  regsubsets(
    x = data_1_x,
    y = data_1_qut_y,
    nbest = 2,
    really.big  = TRUE
  )
regsubsets_1_qut
plot(regsubsets_1_qut, scale = "adjr2")

# # "Leaps" ####
# leaps() implements a different method than regsubsets() (and has different parameters.)
leaps_1_qut <- leaps(
  x = data_1_x,
  y = data_1_qut_y,
  nbest = 1,
  method = "adjr2"
)
leaps_1_qut

# num_vars <- 4
# leaps_1_qut_idx <- leaps_1_qut$which[num_vars,]
# Could create a new model using the best variables...

# # `RegBest()` from FactoMineR" ####
# library("FactoMineR")
regbest_1_qut <- FactoMineR::RegBest(x = data_1_x,
                                     y = data_1_qut_y)
regbest_1_qut$all
regbest_1_qut$best

# # "Simulated Annealing" ####
# library("subselect")
set.seed(seed)
anneal_1_qut <-
  subselect::anneal(
    cor(data_1_x),
    kmin = 1,
    kmax = ncol(data_1_x),
    nsol = ncol(data_1_x),
    niter = 10,
    setseed = TRUE
  )
anneal_1_qut$bestsets

# num_vars <- 4
# vars_best_idx <- anneal_1_qut$bestsets[num_vars, 1:num_vars]
# data_1_x[vars_best_idx]
# Could create a new model using the best variables...

# "Comparing Models using ANOVA" ####
fmla_2_qut <- get_fmla(colname_1_y_qut, colnames_1_x_noopp)
lm_2_qut <- lm(fmla_2_qut, data = data_1_qut_trn)
lm_2_qut
anova(lm_1_qut, lm_2_qut)

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
      "lm_1_qut_step",
      "remove_vars_nonsignif_p",
      "lm_1_qut_onlysignif_p",
      "remove_vars_nonsignif_vif",
      "lm_1_qut_onlysignif_vif",
      "regsubsets_1_qut",
      "leaps_1_qut",
      "regbest_1_qut",
      "anneal_1_qut",
      "fmla_2_qut",
      "lm_2_qut"
    )
  )
}

#'
#' ### Using custom techniques.
#'
# Set nvmax equal to the maximum allowable number of variables.
regsubsets_1_qut <-
  regsubsets(x = data_1_x,
             y = data_1_qut_y,
             nvmax = ncol(data_1_x))
regsubsets_1_qut_summ <- summary(regsubsets_1_qut)
# regsubsets_1_qut_summ

display_regsubsets_coefs <- function(fit, value) {
  fit %>%
    coef(value) %>%
    as_tibble() %>%
    tibble::rownames_to_column()
}

regsubsets_1_qut %>%
  display_regsubsets_coefs(which.max(regsubsets_1_qut_summ$adjr2))
regsubsets_1_qut %>%
  display_regsubsets_coefs(which.min(regsubsets_1_qut_summ$cp))
regsubsets_1_qut %>%
  display_regsubsets_coefs(which.min(regsubsets_1_qut_summ$bic))

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Don't add '()' to normalize in mutate_all unless specifying '(.)'
regsubsets_1_qut_summ_tidy <-
  bind_cols(
    regsubsets_1_qut_summ$adjr2 %>% as_tibble() %>% rename(adjr2 = value),
    regsubsets_1_qut_summ$cp %>% as_tibble() %>% rename(cp = value),
    regsubsets_1_qut_summ$bic %>% as_tibble() %>%  rename(bic = value)
  ) %>%
  mutate_all(funs(normalize)) %>%
  mutate(num_vars = row_number()) %>%
  select(num_vars, everything()) %>%
  gather(metric, value,-num_vars)
regsubsets_1_qut_summ_tidy

regsubsets_1_qut_summ_tidy %>%
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

# This is actually for the logistic regression model.
regsubsets_1_qul <-
  regsubsets(x = data_1_x,
             y = as.factor(data_1_qul_y),
             nvmax = ncol(data_1_x))
regsubsets_1_qul_summ <- summary(regsubsets_1_qul)

regsubsets_1_qul_summ_tidy_nonorm <-
  analyze_regsubsets(regsubsets_1_qul, norm = FALSE)
regsubsets_1_qul_summ_tidy_nonorm

regsubsets_1_qul_summ_tidy <- analyze_regsubsets(regsubsets_1_qul)
regsubsets_1_qul_summ_tidy %>%
  ggplot(aes(x = num_vars, y = value, color = metric)) +
  geom_point() +
  geom_line()
#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(
    list = c(
      "regsubsets_1_qut",
      "regsubsets_1_qut_summ",
      "display_regsubsets_coefs",
      "normalize",
      "regsubsets_1_qut_summ_tidy",
      "analyze_regsubsets",
      "regsubsets_1_qul",
      "regsubsets_1_qul_summ",
      "regsubsets_1_qul_summ_tidy_nonorm",
      "regsubsets_1_qul_summ_tidy"
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
# glm_1_qul <- glm(fmla_1_qul, data = data_1_qul_trn, family = binomial(link = "logit"))
glm_1_qul <-
  glm(fmla_1_qul, data = data_1_qul_trn, family = "binomial")
glm_1_qul

summary(glm_1_qul)
summary(glm_1_qul)$coef
coef(glm_1_qul)

car::vif(glm_1_qul)

#'
#' ### Using the broom bakcage. [^fn_broom_link]
#'
#' The same broom functions shown before could be used in the same fashion.
#'
#' ### Using the caret package.
#'
set.seed(seed)
glm_1_qul_caret <-
  train(fmla_1_qul,
        data = na.exclude(data_1_qul_trn),
        method = "glm")
glm_1_qul_caret$finalModel
summary(glm_1_qul_caret$finalModel)
summary(glm_1_qul_caret)$deviance
summary(glm_1_qul_caret$finalModel)$deviance

#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("glm_1_qul_caret"))
}

#'
#' ## Creating and evaluating the model.
#'
#' ### Using a "
#'
#' ### Following the example at r.statistics.co
#'
#' http://r-statistics.co/Logistic-Regression-With-R.html
#'
# "Check class bias."
table(select(results_prepared, one_of(colname_1_y_qul)))
table(select(data_1_qul, colname_1_y_qul))

# "Create Training Data"
# filter(select(data_1_qut, colname_1_y_qut), pd_h2a < 0)
# mutate(select(data_1_qul, colname_1_y_qul), !!(colname_1_y_qul) := colname_1_y_qul)
# ...

# "Create WOE for Categorical Variables (optional)" ####
# ...

#
# "Compute Informational Values" ####
# library("smbinning")
vars_fct <- NULL
vars_cont <- colnames(data_1_x)
ivs <-
  data.frame(var = c(vars_fct, vars_cont), iv = numeric(ncol(data_1_x)))

# Converting to fit requirements for smbinning() function.
data_1_qul_binary <-
  data_1_qul %>%
  mutate(result = as.numeric(if_else(result == "W", 1, 0))) %>%
  as.data.frame()

for (var_cont in vars_cont) {
  # var_cont <- "pd_td_opp"
  smb <-
    smbinning::smbinning(data_1_qul_binary, y = colname_1_y_qul, x = var_cont)
  smb
  if (class(smb) != "character") {
    ivs[ivs$var == var_cont, "iv"] <- smb$iv
  }
}

ivs <- ivs[order(-ivs$iv),]
ivs

# # "Build Logit Models and Predict" ####
# library("InformationValue")
data_1_qul_binary_trn <-
  data_1_qul_binary[trn_1_qul_idx,] %>% na.omit()
head(data_1_qul_binary_trn)
data_1_qul_binary_tst <-
  data_1_qul_binary[-trn_1_qul_idx,] %>% na.omit()
head(data_1_qul_binary_tst)

# glm_1_qul_binary <- glm(fmla_1_qul, data = data_1_qul_binary_trn, family = binomial(link = "logit"))
glm_1_qul_binary <-
  glm(fmla_1_qul, data = data_1_qul_binary_trn, family = "binomial")
# glm_1_qul_predict <- plogis(predict(glm_1_qul, newdata = data_1_qul_tst))
glm_1_qul_predict <-
  predict(glm_1_qul, newdata = data_1_qul_binary_tst, type = "response")

# ## "Decide on optimal prediction probability cutoff for the model"
data_1_qul_tst_y <-
  data_1_qul_tst %>%
  mutate(result = as.numeric(if_else(result == "W", 1, 0))) %>%
  pull(result)
head(data_1_qul_tst_y)
cutoff_optimal <-
  InformationValue::optimalCutoff(actuals = data_1_qul_tst_y , predictedScores = glm_1_qul_predict)
cutoff_optimal

# # "Model Diagnostics" ####
# summary(glm_1_qul_binary)

# ## "VIF"
# car::vic(glm_1_qul_binary)

# ## "Misclassification Error"
InformationValue::misClassError(actuals = data_1_qul_tst_y,
                                predictedScores = glm_1_qul_predict,
                                threshold = cutoff_optimal)

# ## "ROC"
InformationValue::plotROC(actuals = data_1_qul_tst_y, predictedScores = glm_1_qul_predict)

# ## "Concordance"
InformationValue::Concordance(actuals = data_1_qul_tst_y, predictedScores = glm_1_qul_predict)

# ## "Specificity and Sensitivity"
InformationValue::specificity(actuals = data_1_qul_tst_y,
                              predictedScores = glm_1_qul_predict,
                              threshold = cutoff_optimal)
InformationValue::sensitivity(actuals = data_1_qul_tst_y,
                              predictedScores = glm_1_qul_predict,
                              threshold = cutoff_optimal)

# ## "Confusion Matrix"
InformationValue::confusionMatrix(actuals = data_1_qul_tst_y,
                                  predictedScores = glm_1_qul_predict,
                                  threshold = cutoff_optimal)

#'
#' ### Using the glmnet package.
#'
glmnet_1_qul <- glmnet(as.matrix(data_1_qul_trn[, -1]), as.matrix(data_1_qul_trn[, 1]), family = "binomial")

# Warning: Do not print the variable directly!
summary(glmnet_1_qul)
plot(glmnet_1_qul)
coef(glmnet_1_qul, s = 0.1)

#'
#' ### Using the caret() package.
#'
glm_1_qul_caret <- train(fmla_1_qul, data = data_1_qul_trn, method = "glm")
glm_1_qul_caret$finalModel
summary(glm_1_qul_caret$finalModel)
summary(glmnet_1_qul_caret$finalModel)$r.squared

glmnet_1_qul_caret <- train(fmla_1_qul, data = data_1_qul_trn, method = "glmnet")
summary(glmnet_1_qul_caret$finalModel)
summary(glmnet_1_qul_caret$finalModel)$r.squared

