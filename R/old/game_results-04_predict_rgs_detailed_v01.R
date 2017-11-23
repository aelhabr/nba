


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
# caret: "3.2 Identifying Zero- and Near Zero-Variance Predictors. ----
nzv_1_metrics <- nearZeroVar(x_1, saveMetrics = TRUE)
nzv_1_metrics

nzv_2_metrics <- nearZeroVar(x_2, saveMetrics = TRUE)
nzv_2_metrics
# nzv_2_metrics[nzv_metrics$nzv_metrics , ][1:10, ]

# nzv_2 <- nearZeroVar(x_2, saveMetrics = FALSE
# data_1rgs_filtered <- x_1[, -nzv_2]

# caret: "3.3 Identifying Correlated Predictors" ----
x_1_cor <- cor(x_1)
summary(x_1_cor[upper.tri(x_1_cor)])

x_1_corhigh <-
  findCorrelation(cor(x_1), cutoff = .75)
x_1_corhigh

x_2_corhigh <-
  findCorrelation(cor(x_2), cutoff = .75)
x_2_corhigh

x_2_nocorhigh <- x_2[, -x_2_corhigh]
x_2_nocorhigh_cor <- cor(x_2_nocorhigh)
summary(x_2_nocorhigh_cor[upper.tri(x_2_nocorhigh_cor)])

# # caret: "3.4 Linear Dependencies" ----
lcombo_info_1 <- findLinearCombos(x_1)
lcombo_info_1

lcombo_info_2 <- findLinearCombos(x_2)
lcombo_info_2

x_2_nolcombo <- x_2[,-lcombo_info_2$remove]

# caret: "3.5 The 'preProcess' function" ----
# (This is re-shown in the the section "4.1 Simple Splitting Based on the Outcome".
# The example shown in the
# section "4.2 Splitting Based on the Predictors" is not implemented.)
# set.seed(seed)
# trn_1_idx <-
#   createDataPartition(y_1rgs,
#                       p = 0.8,
#                       list = FALSE,
#                       times = 1)
# 
# head(trn_1_idx)
# data_1_trn <- data_1[trn_1_idx,] %>% na.exclude()
# data_1_tst <- data_1[-trn_1_idx,] %>% na.exclude()
# data_2_trn <- data_2[trn_1_idx,] %>% na.exclude()
# data_2_tst <- data_2[-trn_1_idx,] %>% na.exclude()

#'
#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("nzv_1_metrics", "nzv_2_metrics", "nzv_2"))
  rm(
    list = c(
      "x_1_cor",
      "x_1_corhigh",
      "x_2_corhigh",
      # "x_2_nocorhigh",
      "x_2_nocorhigh_cor"
    )
  )
  rm(list = c(# "data_2rgs_x_nolcombo",
    "lcombo_info_1",
    "lcombo_info_2"))
}

#'
#' ### Using custon techniques. [^fn_ggally]
#'
#' [^fn_ggally]:
#' I believe I originally discovered the GGally package
#'  when reading through the Quick-R e-book.
#'
# custom: pre-processing ----
GGally::ggcorr(
  data_1rgs[,-1],
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
# basic: lm_1 ----
lm_1 <- lm(fmla_1rgs, data = data_1rgs_trn)
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
# broom: lm_1 ----
broom::tidy(lm_1)
broom::glance(lm_1)
head(broom::augment(lm_1))

#'
#' ### Using the caret package.
#'
# caret: lm_1 ----
# Need to use na.exclude() because the train() function is able to detect
# the missing rows.
set.seed(seed)
lm_1_caret <-
  train(fmla_1rgs, data = data_1rgs_trn, method = "lm")
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
# r4sl: lm_1 ----
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_rmse <- function(model, data, response) {
  rmse(actual = data[, response],
       predicted = predict(model, data))
}

fmla_1b <- get_fmla(colname_y_1, colnames_x_1_noopp)
fmla_1c <- get_fmla(colname_y_1, colnames_x_1_opp)
lm_1b <- lm(fmla_1b, data = data_1rgs_trn)
lm_1c <- lm(fmla_1c, data = data_1rgs_trn)

lm_1_list <- list(lm_1, lm_1b, lm_1c)

lm_1_trn_rmse <-
  sapply(lm_1_list, get_rmse, data = data_1rgs_trn, response = colname_y_1)
lm_1_trn_rmse
lm_1_tst_rmse <-
  sapply(lm_1_list, get_rmse, data = data_1rgs_tst, response = colname_y_1)
lm_1_tst_rmse

fmla_2b <- get_fmla(colname_y_2, colnames(x_2_nocorhigh))
fmla_2c <- get_fmla(colname_y_2, colnames(x_2_nolcombo))
lm_2 <- lm(fmla_2rgs, data = data_2rgs_trn)
lm_2b <- lm(fmla_2b, data = data_2rgs_trn)
lm_2c <- lm(fmla_2c, data = data_2rgs_trn)

lm_2_list <- list(lm_2, lm_2b, lm_2c)

lm_2_trn_rmse <-
  sapply(lm_2_list, get_rmse, data = data_2rgs_trn, response = colname_y_2)
lm_2_trn_rmse
lm_2_tst_rmse <-
  sapply(lm_2_list, get_rmse, data = data_2rgs_tst, response = colname_y_2)
lm_2_tst_rmse

#'
#' ### Using techniques shown at r.statistic.co.
#'
#' This section follows (somoe of) the examples on the "Linear Regression" webpage.
#' [^http://r-statistics.co/Linear-Regression.html]
#'
# rsco: "Predicting Linear Models" ----
## "Step 2: Develop the model on the training data and use it to predict the distance on test data"
lm_1_predict <- predict(lm_1, newdata = data_1rgs_tst)

# ## "Step 4: Calculate prediction accuracy and error rates"
data_1rgs_tst_y <- data_1rgs_tst %>% pull(colname_y_1)
lm_1_compare <-
  bind_cols(actual = data_1rgs_tst_y,
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
  rm(list = c("lm_1_predict", "data_1rgs_tst_y", "lm_1_compare"))
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
# cf_1 <- party::cforest(fmla_1rgs, data = data_1rgs, control= party::cforest_unbiased(mtry=2, ntree=50))
# party::varimp(cf_1)
# party::varimp(cf_1, conditional = TRUE)
# party::varimpAUC(cf_1)

# # "2. Relative Imporatnce"
# library("realimpo")
relimp_1 <- relaimpo::calc.relimp(lm_1, type = "lmg", rela = TRUE)
sort(relimp_1$lmg, decreasing = TRUE)

# relimp_2 <- relaimpo::calc.relimp(lm_2, type = "lmg", rela = TRUE)
# sort(relimp_2$lmg, decreasing = TRUE)

# # "4. MARS"
# library("earth")
mars_1 <- earth::earth(fmla_1rgs, data = data_1rgs)
earth::evimp(mars_1)

# mars_2 <- earth::earth(fmla_2rgs, data = data_2rgs)
# earth::evimp(mars_2)

# # "5. Step-wise Regression"
fmla_1_base <- get_fmla("pd_h2a", 1)
fmla_1_base
lm_1_base <- lm(fmla_1_base, data = data_1rgs)
lm_1_all <- lm(fmla_1rgs, data = data_1rgs)
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

# lm_2_step <- step(lm_2, direction = "backward")

# # "6. Boruta"
# Note that this can take a while.
# library("Boruta")
# boruta_1 <- Boruta::Boruta(fmla_1rgs, data = data_1rgs, doTrace = 2)
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
      "cf_2",
      "relimp_1",
      "relimp_2",
      "mars_1",
      "mars_2",
      "fmla_1_base",
      "lm_1_base",
      "lm_1_all",
      "lm_1_step",
      "lm_2_step",
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
    # data_trn <- data_1rgs_trn
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
  remove_vars_nonsignif_vif(lm_1, data_1rgs)
summary(lm_1_onlysignif_vif)

## "Recursively remove non-significant variables" (based on p values)
remove_vars_nonsignif_p <-
  function(lm_fit,
           data_trn,
           threshold = 0.1,
           no_intercept = FALSE) {
    # lm_fit <- lm_1
    # data_trn <- data_1rgs_trn
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
  remove_vars_nonsignif_p(lm_1, data_1rgs)
summary(lm_1_onlysignif_p)

# rsco: "Best subsets" ----
regsubsets_1 <-
  regsubsets(x = x_1,
             y = y_1,
             nvmax = (ncol(data_1rgs) - 1))
regsubsets_1
plot(regsubsets_1, scale = "adjr2")

regsubsets_1 <-
  regsubsets(
    x = x_1,
    y = y_1,
    nbest = 2,
    really.big  = TRUE
  )
regsubsets_1
plot(regsubsets_1, scale = "adjr2")

# rsco: "Leaps" ----
# leaps() implements a different method than
# regsubsets() (and has different parameters.)
leaps_1 <- leaps(
  x = x_1,
  y = y_1,
  nbest = 1,
  method = "adjr2"
)
leaps_1

# num_vars <- 4
# leaps_1_idx <- leaps_1$which[num_vars,]
# Could create a new model using the best variables...

# rsco: `RegBest()` from FactoMineR" ----
# library("FactoMineR")
regbest_1 <- FactoMineR::RegBest(x = x_1,
                                 y = y_1)
regbest_1$all
regbest_1$best

# rsco: "Simulated Annealing" ----
# library("subselect")
set.seed(seed)
anneal_1 <-
  subselect::anneal(
    cor(x_1),
    kmin = 1,
    kmax = ncol(x_1),
    nsol = ncol(x_1),
    niter = 10,
    setseed = TRUE
  )
anneal_1$bestsets

# num_vars <- 4
# vars_best_idx <- anneal_1$bestsets[num_vars, 1:num_vars]
# x_1[vars_best_idx]
# Could create a new model using the best variables...

# rsco: "Comparing Models using ANOVA" ----
fmla_1b_qut <- get_fmla(colname_y_1b, colnames_x_1_noopp)
lm_1b_qut <- lm(fmla_1b_qut, data = data_1rgs_trn)
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
  regsubsets(x = x_1,
             y = y_1,
             nvmax = ncol(x_1))
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
#'
#'

