
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
library("tidyr")
library("leaps")
library("glmnet")
library("tree")
library("rpart")
library("caret")
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
  str_c(dir_import, filename_import_base, filename_import_suffix, filename_import_ext)

#'
#'
#'
#+ include = FALSE
export <- TRUE
remove_tempvars <- TRUE
specify_stats <- TRUE
run_caret <- FALSE

if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "-predict"
  filename_export_ext <- ".csv"
  dir_export <- "data/"
  filepath_export <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      filename_export_ext
    )
}

#'
#'
#'
# Import. ----
results_prepared <- read_csv(filepath_import)

#'
#'
#'
# Model. ----
names(results_prepared)

colname_y_quant <- "pd"
colname_y_qual <- "result"

# colnames_x_home_only <- c("hfa")
colnames_x_home_only <- NULL

if(specify_stats == TRUE) {
  colnames_x_stats_0 <- c("dist", "drest", "pdtd")
  # colnames_x_stats_0 <- c(colnames_x_stats_0, "wptd")
  colnames_x_stats <-
    c(colnames_x_stats_0, paste0(colnames_x_stats_0, "_opp"))
  colnames_x_no_lag1 <-
    colnames_x_stats %>% str_subset("^(dist|drest)")
  colnames_x_lag1 <-
    colnames_x_stats %>% str_subset("^(?!dist|drest)") %>% paste0("_lag1")
  
  colnames_x <-
    c(colnames_x_home_only, colnames_x_no_lag1, colnames_x_lag1)
} else {
  colnames_x <- c(colnames_x_home_only)
}
colnames_x
#'
#'
#'
#+ include = FALSE, evaluate = FALSE
# colname_y_andy <- "pts_total"
# colnames_x_stats_andy <- c("2ptd", "3ptd", "fttd")
# colnames_x_stats_andy <- c(colnames_x_stats_andy, paste0(colnames_x_stats_andy, "_opp"))
# colnames_x_andy <- colnames_x_stats_andy
# colnames_x_andy
#'
#'
#'
model_data_0 <-
  results_prepared %>%
  # distinct(date, tm_home, tm_away, .keep_all = TRUE) %>% 
  # filter((pd > 0 & tm_a == tm_winner) | (pd < 0 & tm_a != tm_winner)) %>% 
  filter(tm_a == tm_home) %>%
  filter(drest != 0 & drest_opp != 0)
summary(model_data_0$hfa)

model_data_quant <-
  model_data_0 %>% 
  select(one_of(c(colname_y_quant, colnames_x)))

model_data_qual <-
  model_data_0 %>% 
  mutate_at(colname_y_qual, as.factor) %>% 
  select(one_of(c(colname_y_qual, colnames_x)))
#'
#'
#'
#+ include = FALSE, evaluate = FALSE
# model_data_andy <-
#   results_prepared %>%
#   filter(tm_a == tm_home) %>% 
#   filter(drest != 0 & drest_opp != 0) 
#   select(one_of(c(colname_y_andy, colnames_x_andy))) 

#'
#'
#'
#+ include = FALSE
if(remove_tempvars == TRUE) {
  # rm(list = c("results_prepared"))
  rm(list = c("model_data_0"))
}

#'
#'
#'
model_fmla_quant <-
  paste(colname_y_quant, paste(colnames_x, collapse = " + "), sep = " ~ ") %>% 
  as.formula()
model_fmla_quant

model_fmla_qual <-
  paste(colname_y_qual, paste(colnames_x, collapse = " + "), sep = " ~ ") %>% 
  as.formula()
model_fmla_qual
#'
#'
#'
#+ include = FALSE, evaluate = FALSE
# model_fmla_andy <-
#   paste(colname_y_andy, paste(colnames_x_andy, collapse = " + "), sep = " ~ ")
# model_fmla_andy
#'
#'
#'
#+ include = FALSE, evaluate = FALSE
# model_data_tidy_andy <-
#   model_data_andy %>% 
#   gather(metric, value, -pts_total)
# 
# viz_andy_1 <-
#   model_data_tidy_andy %>% 
#   filter(metric == "fttd") %>% 
#   ggplot(aes(x = pts_total)) +
#   geom_histogram() +
#   labs(
#     x = "", 
#     y = "", 
#     title = "Distribution of Response Variable",
#     subtitle = colname_y_andy
#   )
# viz_andy_1
# viz_andy_2 <-
#   model_data_tidy_andy %>% 
#   ggplot(aes(x = value, fill = metric)) +
#   geom_histogram() +
#   facet_wrap( ~ metric, nrow = 2, scales = "free") +
#   labs(
#     x = "", 
#     y = "", 
#     title = "Distribution of Covariates", 
#     subtitle = model_fmla_andy,
#     caption = "*'td' stands for 'to date'."
#   ) +
#   guides(legend.title = "Covariate")
# viz_andy_2
# 
# viz_andy_3 <-
#   model_data_tidy_andy %>% 
#   ggplot(aes(x = value, y = pts_total)) +
#   geom_point() +
#   # geom_line() +
#   geom_smooth(aes(color = metric), size = 2, method = "lm", se = FALSE) +
#   facet_wrap( ~ metric, nrow = 2, scales = "free") +
#   labs(
#     x = "", 
#     y = "", 
#     title = "Covariates VS. Response Variable", 
#     subtitle = model_fmla_andy,
#     caption = "*'td' stands for 'to date'."
#   ) +
#   guides(legend.title = "Covariate")
# viz_andy_3
#'
#'
#'
#+ include = FALSE, evaluate = FALSE
# rm(list = c("viz_andy_1", "viz_andy_2", "viz_andy_3"))
#'
#' ## cor
#'
#+ include = FALSE
model_data_x <- model_data_quant %>% select(one_of(colnames_x))
model_data_x
# round(cor(model_data_x), 2)
GGally::ggcorr(model_data_x, label = TRUE, label_round = 2, label_size = 3)
#'
#'
#'
if(remove_tempvars == TRUE) {
  rm(list = c("model_data_x"))
}

#'
#' ## lm
#'

lm_fit_full <- lm(model_fmla_quant, data = model_data_quant)
lm_fit_full
summary(lm_fit_full)
summary(lm_fit_full)$coef
coef(lm_fit_full)

#'
#' Evaluate.
#'
head(lm_fit_full$model$pd)
head(lm_fit_full$fitted.values)
head(lm_fit_full$residuals)
summary(lm_fit_full$fitted.values)
mean(lm_fit_full$fitted.values)
sd(lm_fit_full$fitted.values)

lm_fit_full_z <-
  # scale(lm_fit_full$fitted.values)[,1]
  (lm_fit_full$fitted.values - mean(lm_fit_full$fitted.values)) / sd(lm_fit_full$fitted.values)
summary(lm_fit_full_z)

lm_fit_full_p <- pnorm(lm_fit_full_z)
summary(lm_fit_full_p)

lm_fit_full_diagnostics <-
  lm_fit_full_p %>% 
  as_tibble() %>% 
  rename(prob = value) %>% 
  bind_cols(lm_fit_full_z %>% as_tibble() %>% rename(z = value),
            lm_fit_full$model$pd %>% as_tibble() %>% rename(actual = value),
            lm_fit_full$fitted.values %>% as_tibble() %>% rename(fitted = value),
            lm_fit_full$residuals %>% tbl_df() %>% rename(residual = value)
  ) %>% 
  select(actual, fitted, residual, z, prob)

lm_fit_full_diagnostics %>% 
  arrange(desc(fitted))

lm_fit_full_diagnostics %>% 
  filter(prob > 0.95)

#'
#'
#'
calc_pct <- function(x, n, digits = 6) {
  round(sum(x) / max(n), digits)
}

lm_fit_full_diagnostics_calcs <-
  lm_fit_full_diagnostics %>% 
  mutate(
    true_positive = if_else(actual > 0 & fitted > 0, 1, 0),
    true_negative = if_else(actual < 0 & fitted < 0, 1, 0),
    false_positive = if_else(actual < 0 & fitted > 0, 1, 0),
    false_negative = if_else(actual > 0 & fitted < 0, 1, 0)
  )

calc_pct(lm_fit_full_diagnostics_calcs$true_positive, nrow(lm_fit_full_diagnostics_calcs))
lm_fit_full_diagnostics_calcs %>% 
  summarise_all(calc_pct, nrow(lm_fit_full_diagnostics_calcs))
  # summarise(
  #   true_positive_pct = sum(true_positive) / n(),
  #   true_negative_pct = sum(true_negative) / first(n),
  #   false_positive_pct = sum(false_positive) / first(n),
  #   false_negative_pct = sum(false_negative) / first(n)
  # )

#'
#'
#'
#+ include = FALSE, evaluate = FALSE
# lm_fit_full_andy <- lm(pts_total ~ ., data = model_data_andy)
# lm_fit_full_andy
# summary(lm_fit_full_andy)
#'
#'
#'
#+ include = FALSE
# rm(
#   list = c(
#     "colnames_x_stats_andy",
#     "colnames_x_andy",
#     "colname_y_andy",
#     "model_data_andy",
#     "model_fmla_andy",
#     "lm_fit_full_andy",
#     "viz_andy_3"
#   )
# )

#'
#' ## glm
#'
glm_fit_full <- glm(model_fmla_qual, data = model_data_qual, family = "binomial")
glm_fit_full
summary(glm_fit_full)
summary(glm_fit_full)$coef
coef(glm_fit_full)

#'
#' ## regsubsets
#'
# regsbusets() requires inputs to be matrices.
model_data_x <-
  data.matrix(model_data_quant %>% select(one_of(colnames_x)))
model_data_y_quant <-
  data.matrix(model_data_quant %>% select(one_of(colname_y_quant)))
model_data_y_qual <-
  data.matrix(model_data_qual %>% select(one_of(colname_y_qual)))

# Set nvmax equal to the maximum allowable number of variables.
subset_fit_full_quant <-
  regsubsets(model_data_x, model_data_y_quant, nvmax = length(colnames_x))
subset_summary_quant <- summary(subset_fit_full_quant)
# subset_summary_quant 
#'
#'
#'
display_subset_coefs <- function(model_fit, value) {
  model_fit %>% 
    coef(value) %>% 
    as_tibble() %>% 
    tibble::rownames_to_column()
}

subset_fit_full_quant %>% 
  display_subset_coefs(which.max(subset_summary_quant$adjr2))
subset_fit_full_quant %>% 
  display_subset_coefs(which.min(subset_summary_quant$cp))
subset_fit_full_quant %>% 
  display_subset_coefs(which.min(subset_summary_quant$bic))

#'
#'
#'
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Don't add '()' to normalize in mutate_all unless specifying '(.)'
subset_summary_quant_tidy <-
  bind_cols(
    subset_summary_quant$adjr2 %>% as_tibble() %>% rename(adjr2 = value),
    subset_summary_quant$cp %>% as_tibble() %>% rename(cp = value),
    subset_summary_quant$bic %>% as_tibble() %>%  rename(bic = value)
  ) %>% 
  mutate_all(funs(normalize)) %>% 
  mutate(num_vars = row_number()) %>% 
  select(num_vars, everything()) %>% 
  gather(metric, value, -num_vars) 
subset_summary_quant_tidy

subset_summary_quant_tidy %>% 
  ggplot(aes(x = num_vars, y = value, color = metric)) +
  geom_point() +
  geom_line()

#'
#'
#'
#+ include = FALSE
analyze_regsubsets <- function(subset_fit_full, norm = TRUE) {
  subset_summary <- summary(subset_fit_full)
  subset_summary_tidy <-
    bind_cols(
      subset_summary$adjr2 %>% as_tibble() %>% rename(adjr2 = value),
      subset_summary$cp %>% as_tibble() %>% rename(cp = value),
      subset_summary$bic %>% as_tibble() %>%  rename(bic = value)
    )
  
  if(norm == TRUE) {
    subset_summary_tidy <-
      subset_summary_tidy %>% 
      mutate_all(funs(normalize))
  }
  subset_summary_tidy <-
    subset_summary_tidy %>% 
    mutate(num_vars = row_number()) %>% 
    select(num_vars, everything()) %>% 
    gather(metric, value, -num_vars) 
  subset_summary_tidy
}
#'
#'
#'
subset_fit_full_qual <-
  regsubsets(model_data_x, model_data_y_qual, nvmax = length(colnames_x))
subset_summary_qual <- summary(subset_fit_full_qual)

subset_summary_qual_tidy_nonorm <- analyze_regsubsets(subset_fit_full_qual, norm = FALSE)
subset_summary_qual_tidy_nonorm

subset_summary_qual_tidy <- analyze_regsubsets(subset_fit_full_qual)
subset_summary_qual_tidy %>% 
  ggplot(aes(x = num_vars, y = value, color = metric)) +
  geom_point() +
  geom_line()
#'
#' ## glmnet
#'
set.seed(seed)
glmnet_fit_full <- glmnet(model_data_x, model_data_y_qual, family = "binomial")
glmnet_fit_full
#'
#' ## caret glm
#'
if(run_caret == TRUE) {
  set.seed(seed)
  # glm_fit_full <- train(model_fmla_qual, data = model_data_qual, method = "glm")
  lm_fit_full_caret <- train(pd ~ ., data = model_data_quant, method = "lm")
  lm_fit_full_caret
  attributes(lm_fit_full_caret)
  lm_fit_full_caret$finalModel
  summary(lm_fit_full_caret$finalModel)
  summary(lm_fit_full_caret$finalModel)$r.squared
}
#'
#'
#' ## caret glmnet
#'
if(run_caret == TRUE) {
  set.seed(seed)
  glmnet_fit_full_caret <- train(result ~ ., data = model_data_qual, method = "glmnet")
  glmnet_fit_full_caret
}
#'
#' ## tree
#'
set.seed(seed)
# tree_fit_full <- tree(model_fmla_qual, data = model_data_qual)
tree_fit_full <- tree(result ~ ., data = model_data_qual)
summary(tree_fit_full)
attributes(tree_fit_full)

plot(tree_fit_full)
text(tree_fit_full, pretty = 0)
#'
#'
#'
set.seed(seed)
tree_fit_cv <- cv.tree(tree_fit_full, FUN = prune.tree, K = 10)

#'
#' ## rpart
#'
set.seed(seed)
rpart_fit_full <- rpart(model_fmla_qual, data = model_data_qual)
summary(rpart_fit_full)
rsq.rpart(rpart_fit_full)
#'
#' ## caret rf
#'
if(run_caret == TRUE) {
  set.seed(seed)
  rf_fit_full <- train(model_fmla_qual, data = model_data_qual, method = "rf")
  rf_fit_full$finalModel
  summary(rf_fit_full$finalModel)$r.squared
}
#'
#'
#'

