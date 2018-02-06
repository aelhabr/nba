
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
  file.path(projroot, "data/sessions", "experiment_session_data.RData")
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
var_y1a <- "result"
var_y1b <- "result_tier4"
var_y1c <- "result_tier6"

var_y2a <- "pd"

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
#'
#'
# Formula creation. ----
vars_basic <- "wp_td"
vars_basic_x1_y1a <- c(vars_basic, str_c(vars_basic, "_opp"))
fmla_basic_x1_y1a <- create_fmla(var_y1a, vars_basic_x1_y1a)

vars_basic_x2_y1a <- c(vars_basic_x1_y1a, "hfa")
fmla_basic_x2_y1a <- create_fmla(var_y1a, vars_basic_x2_y1a)

vars_rest <- c("g_last7", "dist_last7", "rtrip")
vars_rest_x1_y1a <- c(vars_rest, str_c(vars_rest, "_opp"))
fmla_rest_x1_y1a <- create_fmla(var_y1a, vars_rest_x1_y1a)

vars_rest_x2_y1a <- c(c("dist", "drest", vars_rest), str_c(vars_rest, "_opp"))
fmla_rest_x2_y1a <- create_fmla(var_y1a, vars_rest_x2_y1a)

vars_mix_1 <- c(vars_basic_x2_y1a, vars_rest_x1_y1a)
fmla_mix_1 <- create_fmla(var_y1a, vars_mix_1)

fmla_ctg_x1_y1a <- create_fmla(var_y1a, str_c(vars_ctg, "_eoy_rank"))
fmla_ctg_x2_y1a  <-
  create_fmla(var_y1a, str_c(vars_ctg, "_class_eoy_rank"))

#'
#'
fmla_ctg_x1_y2a <- create_fmla(var_y2a, c(str_c(vars_ctg, "_eoy_rank"), " + 0"))

lm_ctg_x1_y2a <- lm(fmla_ctg_x1_y2a, data = model_data)
broom::glance(lm_ctg_x1_y2a)
broom::tidy(lm_ctg_x1_y2a)
#'
#'
#'
# model_name parameter creation. ----
# fmlas <-
#   c(fmla_basic_x1_y1a,
#     fmla_basic_x2_y1a,
#     fmla_rest_x1_y1a,
#     fmla_rest_x2_y1a,
#     fmla_mix_1,
#     fmla_ctg_x1_y1a,
#     fmla_ctg_x2_y1a)

fmlas <- c(fmla_basic_x2_y1a, fmla_rest_x1_y1a, fmla_rest_x2_y1a)

convert_var_to_char <- function(a, ...) {
  arg <- deparse(substitute(a))
  dots <- substitute(list(...))[-1]
  c(arg, sapply(dots, deparse))
}
fmlas_chars <- sapply(fmlas, convert_fmla_to_char)

# for(i in seq_along(fmlas)) {
#   substitute(fmlas[i])
#   f_name <- convert_var_to_char(fmlas[i])
#   if(i == 1) {
#     fmla_names <- f
#   } else {
#     fmla_names <- c(fmla_names, f)
#   }
# }
# fmlas_names
# # Or...
# fmlas_names <- convert_var_to_char(fmla_basic_x1_y1a, fmla_basic_x2_y1a)
# fmlas_names
# Or...
fmlas_names <- Hmisc::Cs(fmla_basic_x2_y1a, fmla_rest_x1_y1a, fmla_rest_x2_y1a)

fmlas_info <-
  tibble(fmla_name = fmlas_names, fmla = fmlas_chars)
fmlas_info

methods <- c("glm", "nb")
# methods <- c("glm", "knn", "lda", "qda", "nb") # , "svm")

# Don't convert to a tibble because the formulas will get nested.
models_grid <-
  expand.grid(method = methods,
              fmla_name = fmlas_names,
              stringsAsFactors = FALSE) %>%
  as_tibble()
models_grid

model_params_info <-
  models_grid %>%
  inner_join(fmlas_info)
model_params_info

# knn_params_details <- tibble(method = "knn", k = 100)
knn_params_details <-
  expand.grid(
    method = "knn",
    k = c(25, 50, 100),
    stringsAsFactors = FALSE
  ) %>%
  as_tibble()
knn_params_details

svm_params_details <-
  expand.grid(
    method = "svm",
    # kernel = c("linear", "radial"),
    kernel = "linear",
    # cost = c(0.01, 0.1, 1),
    cost = 100,
    scale = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  as_tibble()
svm_params_details

# These joins are meaningless if "knn" and "svm" aren't specified as methods.
models_params_nest <-
  model_params_info %>%
  left_join(knn_params_details) %>%
  left_join(svm_params_details) %>%
  arrange(method, fmla_name) %>%
  group_by(method, fmla_name) %>%
  nest()
models_params_nest
models_params_nest %>% unnest() %>% select(-fmla)

models_params <-
  models_params_nest %>%
  unnest()
models_params
#'
#'
#'
# Loop. ----
i <- 1
if (exists("diagnostics_details_all")) {
  rm("diagnostics_details_all")
}

show_loop_info <- TRUE
return_preds <- FALSE
while (i <= nrow(models_params)) {
  # while(i < 3) {
  models_params_row <- models_params %>% slice(i)
  method <- models_params_row$method
  fmla_name <- models_params_row$fmla_name
  fmla_char <- models_params_row$fmla
  fmla <- as.formula(fmla_char)
  params <-
    models_params_row %>% select(-method,-fmla_name,-fmla) %>% as.list()

  trn_data <- model_data %>% filter(season %in% seasons_trn)
  tst_data <- model_data %>% filter(season %in% seasons_tst)

  try({
    if (method == "knn") {
      trn_data <- trn_data %>% mutate_if(is.factor, funs(as.numeric))
      tst_data <-
        tst_data %>% mutate_if(is.factor, funs(as.numeric))
    }
    diagnostics <-
      diagnose_model_performance(
        fmla = fmla,
        trn_data = trn_data,
        tst_data = tst_data,
        method = method,
        params = params,
        return_preds = return_preds
      )

    if (return_preds == FALSE) {
      num_vars <- length(all.vars(as.formula(fmla))[-1])
      diagnostics_details <-
        models_params_row %>%
        mutate(num_vars = num_vars) %>%
        bind_cols(diagnostics)
    } else {
      diagnostics_details <-
        models_params_row %>%
        # bind_cols(diagnostics)
        cbind(diagnostics)
    }


    if (!exists("diagnostics_details_all")) {
      diagnostics_details_all <- diagnostics_details
    } else {
      diagnostics_details_all <-
        bind_rows(diagnostics_details_all, diagnostics_details) %>%
        as_tibble()
    }
  })
  if (show_loop_info == TRUE) {
    cat("loop:", i)
    cat(", ")
    cat(method)
    # sprintf("%.3s", str_c(method, "   "))
    cat(", ")
    cat(fmla_name)
    cat("\n")
  }
  i <- i + 1
}

#'
#'
#'
# Interpret loop diagnostics. ----
if (return_preds == TRUE) {
  vars_params <- c("k", "kernel", "cost", "scale")

  vars_append <-
    c(
      "date",
      "season",
      "tm",
      "tm_opp",
      "hfa",
      "pts_home",
      "pts_away",
      "g_td",
      "wp_td",
      "wp_td_opp",
      "result",
      "result_tier4",
      "result_tier6"
    )

  preds_details_test <-
    diagnostics_details_all %>%
    filter(set == "test") %>%
    mutate(pred_correct = ifelse(pred == actual, TRUE, FALSE)) %>%
    mutate_at(vars(pred, actual), funs(as.factor)) %>%
    mutate(params = paste2(k, kernel, cost, scale)) %>%
    mutate(model_name = str_c(method, "_", fmla_name)) %>%
    mutate(model_name =
             ifelse(is.na(params), model_name, str_c(model_name, "-", params))) %>%
    select(-one_of(c(vars_params, "fmla")))

  vars_keep <- c(vars_append, names(preds_details_test))

  preds_details_test_joined <-
    preds_details_test %>%
    inner_join(tst_data %>% mutate(n = row_number()), by = "n") %>%
    select(one_of(vars_keep))

  seeason_filter <- 2016
  tm_filter <- "CHI"

  preds_details_test_joined %>%
    filter(season %in% seeason_filter) %>%
    filter(tm %in% tm_filter) %>%
    group_by(season, tm, model_name, pred) %>%
    summarise(n = n()) %>%
    spread(pred, n)


  preds_details_test_joined %>%
    filter(season %in% seeason_filter) %>%
    filter(tm %in% tm_filter) %>%
    # group_by(season, tm, model_name, pred) %>%
    ggplot(aes(x = pred)) +
    geom_bar(aes(fill = pred, group = pred), position = "dodge") +
    facet_wrap(~ model_name)


  preds_details_test_joined %>%
    # filter(season %in% seeason_filter) %>%
    filter(tm %in% tm_filter) %>%
    filter(pred %in% "w") %>%
    group_by(season, tm, model_name, pred) %>%
    ggplot(aes(x = model_name)) +
    geom_bar(aes(fill = model_name, group = model_name), position = "dodge") +
    facet_wrap( ~ season) +
    theme(axis.text.x = element_blank()) +
    theme(legend.position = "bottom") +
    labs(
      x = "",
      y = "",
      title = "",
      subtitle = ""
    )

  preds_details_test_joined %>%
    filter(season %in% seeason_filter) %>%
    filter(tm %in% tm_filter) %>%
    filter(g_td <= 40) %>%
    ggplot(aes(x = model_name, y = g_td)) +
    geom_raster(aes(fill = model_name, alpha = pred)) +
    facet_wrap( ~ season) +
    theme(axis.text.x = element_blank()) +
    # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = "", y = "") +
    # scale_fill_gradient(low = "cyan", high = "red") +
    theme(legend.position = "bottom") +
    labs(
      x = "",
      y = "",
      title = "",
      subtitle = ""
    )

  preds_details_test_joined %>%
    filter(season %in% seeason_filter) %>%
    filter(tm %in% tm_filter) %>%
    filter(g_td <= 40) %>%
    ggplot(aes(x = model_name, y = g_td)) +
    geom_raster(aes(fill = model_name, alpha = pred_correct)) +
    facet_wrap( ~ season) +
    theme(axis.text.x = element_blank()) +
    # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = "", y = "") +
    # scale_fill_gradient(low = "cyan", high = "red") +
    theme(legend.position = "bottom") +
    labs(
      x = "",
      y = "",
      title = "",
      subtitle = ""
    )

  preds_details_test_joined %>%
    filter(season %in% seeason_filter) %>%
    filter(tm %in% tm_filter) %>%
    filter(g_td <= 40) %>%
    filter(method == "glm") %>%
    gather(pred_type, pred_value, pred) %>%
    ggplot() +
    geom_tile(aes(x = pred_value, y = g_td, fill = pred_correct)) +
    # geom_tile(aes(x = pred_correct, y = g_td, fill = pred_value)) +
    facet_wrap( ~ pred_type) +
    # theme(axis.text.x = element_blank()) +
    # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = "", y = "") +
    # scale_fill_gradient(low = "cyan", high = "red") +
    theme(legend.position = "bottom") +
    labs(
      x = "",
      y = "",
      title = "",
      subtitle = ""
    )

} else {
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
    ggplot(aes(x = fmla_name, y = acc_tst)) +
    geom_col(aes(fill = method), position = "dodge")

  # ggsave("figs/diagnostics_details_all_2.png", width = 5, height = 7, units = "in")
  diagnostics_details_all %>%
    ggplot(aes(x = method, y = acc_tst)) +
    geom_col(aes(fill = fmla_name), position = "dodge")

  diagnostics_details_all %>%
    filter(method == "glm") %>%
    select(method, fmla_name, k, acc_trn, acc_tst) %>%
    arrange(desc(acc_tst))
}


