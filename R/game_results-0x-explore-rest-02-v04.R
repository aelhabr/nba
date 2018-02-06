#' ---
#' title: "Andrew's Final Project"
#' output:
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#' params:
#'   drest_cut: 2
#' ---
#'
#'
#'
#+ include = FALSE
knitr::opts_chunk$set(
  include = TRUE,
  echo = FALSE,
  fig.align = "center",
  # width = 100,
  warning = FALSE,
  message = FALSE
)

# rm(list = ls())
projroot <- rprojroot::find_rstudio_root_file()

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
library("ggplot2")
theme_set(theme_minimal())

#'
#' ## User-defined parameters.
#'
#+ echo = TRUE
# User-defined parameters. ----
g_td_cut_low <- 10
# drest_cut_low <- 2
drest_cut_low <- params$drest_cut
rtrip_low <- 2
rtrip_ctr <- 1

show_fig <- FALSE
save_fig <- FALSE

#'
#'
#'
#+ include = FALSE
# Constants based on parameters. ----
vars_ortg_opp_idx <-
  names(results_prepared_min) %>% str_which("ortg.*opp")
vars_ortg_opp_idx
vars_ortg_opp <-
  names(results_prepared_min) %>% str_subset("ortg.*opp")
vars_defrtg <-
  vars_ortg_opp %>% str_replace_all("_opp", "") %>% str_replace_all("ortg", "drtg")
vars_defrtg
names(results_prepared_min)[vars_ortg_opp_idx] <- vars_defrtg

vars_opp <-
  names(results_prepared_min) %>%
  str_subset("_opp")
vars_tm <-
  vars_opp %>%
  str_replace_all("_opp", "")
vars_ignore <-
  names(results_prepared_min) %>% str_subset("result_t|prob_t|_class")

data_0 <-
  results_prepared_min %>%
  mutate(tm_ha = ifelse(tm == tm_home, "h", "a")) %>%
  select(tm_ha, everything()) %>%
  select(-one_of(vars_ignore)) %>%
  filter(rn == 1)

vars_tm <- setdiff(vars_tm, vars_ignore)
vars_opp <- setdiff(vars_opp, vars_ignore)
vars_constant <-
  setdiff(names(data_0), c(vars_tm, vars_opp, vars_ignore))

data_filtered <-
  data_0 %>%
  filter(g_td >= g_td_cut_low) %>%
  filter(drest >= drest_cut_low | drest_opp >= drest_cut_low)

#'
#'
#'
# Other constants. ----
vars_ctg <-
  names(data_filtered) %>%
  str_subset("ortg|drtg|efg|tov|orb|ftr")
vars_ctg_td <- vars_ctg %>% str_subset("_td")

vars_ctg_td_rescale <-
  # vars_ctg_td %>% str_subset("ortg|drtg|tov|orb")
  vars_ctg_td %>% str_subset("efg|ftr")

vars_ff <-
  names(data_filtered) %>%
  str_subset("efg|tov|orb|ftr")
vars_ff_td <- vars_ff %>% str_subset("_td")
vars_ff_off_td <- vars_ff_td %>% str_subset("_off")
vars_ff_off_td_opp <- vars_ff_td %>% str_subset("_opp")
vars_ff_off_td_tm <- setdiff(vars_ff_off_td, vars_ff_off_td_opp)
vars_ff_off_td_ha <-
  c(str_c(vars_ff_off_td_tm, "_h"),
    str_replace_all(vars_ff_off_td_opp, "_opp", "_a"))

vars_rtg <-
  names(data_filtered) %>%
  str_subset("ortg|drtg")
vars_rtg_td <- vars_rtg %>% str_subset("_td")
vars_rtg_off_td <- vars_rtg_td %>% str_subset("_off")
#'
#'
#'
# data_filtered %>% summarise_at(vars(vars_ctg_td_rescale), funs(mean)) %>% t()
data_processed <-
  data_filtered %>%
  # mutate_at(vars(vars_ctg_td_rescale), funs(. / 100)) %>%
  mutate_at(vars(vars_ctg_td_rescale), funs(. * 100)) %>%
  mutate(ind = ifelse(rtrip >= rtrip_low |
                        rtrip_opp >= rtrip_low, 1, 0)) %>%
  select(ind, everything())
#'
#'
#'
data_processed %>%
  group_by(ind) %>%
  summarise_at(vars("prob"), funs(mean, sd))
data_processed %>%
  group_by(ind, hfa) %>%
  summarise_at(vars("prob"), funs(mean, sd))
#'
#'
#'
#'
#'
#'
tidy_ha_metrics <- function(d, vars, vars_base = "") {
  d %>%
    select(one_of(c(vars_base, vars))) %>%
    gather(metric_full, value, vars) %>%
    separate(metric_full, c("metric", "off", "td", "ha")) %>%
    select(-off, -td)
}

data_ha <-
  bind_rows(
    data_processed %>%
      filter(tm_ha == "h") %>%
      rename_at(vars(vars_ff_off_td_tm), funs(str_c(., "_h"))) %>%
      rename_at(vars(vars_ff_off_td_opp), funs(str_replace_all(., "_opp", "_a"))),
    data_processed %>%
      filter(tm_ha == "a") %>%
      rename_at(vars(vars_ff_off_td_tm), funs(str_c(., "_a"))) %>%
      rename_at(vars(vars_ff_off_td_opp), funs(str_replace_all(., "_opp", "_h")))
  )


if (show_fig == TRUE) {
  vars_base <- c("date", "season", "tm_ha", "ind")
  viz_ha_ff <-
    data_ha %>%
    tidy_ha_metrics(vars = vars_ff_off_td_ha, vars_base = vars_base) %>%
    ggplot(aes(x = value, fill = ha)) +
    # geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity") +
    geom_density(alpha = 0.5) +
    facet_wrap(~ metric, scales = "free")
  viz_ha_ff
  if (save_fig == TRUE) {
    ggsave(
      "figs/viz_ha_ff.png",
      width = 11,
      height = 11,
      units = "in"
    )
  }
}

if (show_fig == TRUE) {
  viz_ha_ff_ind <-
    data_ha %>%
    tidy_ha_metrics(vars = vars_ff_off_td_ha, vars_base = vars_base) %>%
    mutate(ind = ifelse(ind == 1, "treatment", "control")) %>%
    mutate(ha = str_c(ha, "_", ind)) %>%
    ggplot(aes(x = value, fill = ha)) +
    # geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity") +
    geom_density(alpha = 0.5) +
    facet_wrap(~ metric, scales = "free")
  # facet_grid(ind ~ metric, scales = "free")
  viz_ha_ff_ind
  if (save_fig == TRUE) {
    ggsave(
      "figs/viz_ha_ff_ind.png",
      width = 11,
      height = 11,
      units = "in"
    )
  }
}

if (show_fig == TRUE) {
  viz_pd <-
    data_ha %>%
    ggplot(aes(x = pd_h2a)) +
    geom_histogram()
  viz_pd
  if (save_fig == TRUE) {
    ggsave(
      "figs/viz_pd.png",
      width = 11,
      height = 11,
      units = "in"
    )
  }
}

#'
#' ## Linear regression models for point differential.
#'
# Model. ----
var_y_rgs <- "pd"
var_y_cls <- "w"

# unitize <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }

get_glm <-
  function(d,
           var_y,
           vars_x,
           intercept = TRUE,
           normalize = FALSE,
           vars_normalize = vars_x,
           classification = FALSE,
           print = FALSE) {
    if (intercept == TRUE) {
      fmla <- create_fmla(var_y, vars_x)
    } else {
      fmla <- create_fmla(var_y, c(vars_x, " 0"))
    }
    fmla
    if (normalize == TRUE) {
      d <-
        d %>%
        mutate_at(vars(vars_normalize), funs(BBmisc::normalize(
          ., method = "range", range = c(0, 1)
        )))
      # mutate_at(vars(vars_x), funs(unitize))
    }

    if (classification == TRUE) {
      d <- d %>% mutate_at(vars(var_y), funs(as.factor))
    }

    if (classification == FALSE) {
      model <- glm(fmla, data = d)
    } else if (classification == TRUE) {
      model <- glm(fmla, data = d, family = "binomial")
    }
    if (print == TRUE) {
      d %>% select(one_of(var_y, vars_x)) %>% head() %>% print()
      # model %>% summary() %>% print()
      model %>% broom::glance() %>% print()
      model %>% broom::tidy() %>% print()
    }
    model
  }
#'
#'
#'
# lm ----
lm_hfa <-
  data_processed %>%
  get_glm(var_y_rgs, c("hfa"),
          intercept = FALSE)

lm_hfa_ind <-
  data_processed %>%
  get_glm(var_y_rgs, c("hfa", "ind"),
          intercept = FALSE)

lm_ff_ind <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_ff_off_td, "ind"),
          intercept = FALSE)

lm_rtg_ind <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_rtg_off_td, "ind"),
          intercept = FALSE)
#'
#' ### Stargazer Compilation
#'
#+ echo = TRUE
stargazer::stargazer(
  lm_hfa,
  lm_hfa_ind,
  lm_ff_ind,
  lm_rtg_ind,
  type = "text",
  no.space = TRUE,
  column.labels = c("hfa only", "hfa", "ff", "rtg")
)
#'
#' ### Four Factors Model Only
#'
#+ echo = TRUE
summary(lm_ff_ind)
#'
#' ### Offense-Defense Rating Model Only
#'
#+ echo = TRUE
summary(lm_rtg_ind)
#'
#'
#'
#'
tms <- data_processed %>% distinct(tm) %>% arrange(tm) %>% pull(tm)
i <- 1
while (i <= length(tms)) {
  tm_i <- tms[i]
  lm_i <-
    data_processed %>%
    filter(tm_home == tm_i) %>%
    get_glm(var_y_rgs,
            c(vars_ff_off_td, "ind"),
            intercept = FALSE)

  coefs_i_df <- broom::tidy(lm_i)
  coef_i_tm <- cbind(tm = tm_i, coefs_i_df) %>% as_tibble()
  if (i == 1) {
    summ <- coef_i_tm
  } else {
    summ <- bind_rows(summ, coef_i_tm)
  }
  i <- i + 1
}
summ
#'
#' ### Sensitivity of Indicator P-Value to Each Home Court
#'
#+ echo = TRUE
summ %>%
  gather(metric, value, -tm, -term) %>%
  filter(metric %in% c("estimate", "p.value")) %>%
  filter(term == "ind") %>%
  spread(metric, value) %>%
  arrange(p.value)
#'
#'
#'
if (show_fig == TRUE) {
  viz_ha_ff_ind_byteam_pvals <-
    summ %>%
    gather(metric, value, -tm, -term) %>%
    filter(metric %in% c("p.value")) %>%
    filter(term %in% c("ind")) %>%
    ggplot(aes(x = value)) + #, fill = term)) +
    geom_histogram() +
    guides(fill = FALSE) +
    geom_vline(
      aes(xintercept = 0.05),
      color = "red",
      linetype = "dashed",
      size = 3
    ) +
    facet_grid(term ~ metric, scales = "free")
  viz_ha_ff_ind_byteam_pvals
  if (save_fig == TRUE) {
    ggsave(
      "figs/viz_ha_ff_ind_byteam_pvals.png",
      width = 11,
      height = 11,
      units = "in"
    )
  }
}
#'
#' ## Classification Models for Probability of Win.
#'
# glm ----
glm_hfa <-
  data_processed %>%
  get_glm(var_y_cls,
          c("hfa"),
          intercept = FALSE,
          classification = TRUE)

glm_hfa_ind <-
  data_processed %>%
  get_glm(var_y_cls,
          c("hfa", "ind"),
          intercept = FALSE,
          classification = TRUE)

glm_ff_ind <-
  data_processed %>%
  get_glm(
    var_y_cls,
    c(vars_ff_off_td, "ind"),
    intercept = FALSE,
    classification = TRUE
  )

glm_rtg_ind <-
  data_processed %>%
  get_glm(
    var_y_cls,
    c(vars_rtg_off_td, "ind"),
    intercept = FALSE,
    classification = TRUE
  )
#'
#' ### Stargazer Compilation
#'
#+ echo = TRUE
stargazer::stargazer(glm_hfa,
                     glm_hfa_ind,
                     glm_ff_ind,
                     glm_rtg_ind,
                     type = "text",
                     no.space = TRUE)
#'
#' ### Four Factors Model Only
#'
#+ echo = TRUE
summary(glm_ff_ind)

#'
#' ### Offense-Defense Rating Model Only
#'
#+ echo = TRUE
summary(glm_rtg_ind)

#'
#' ## Fixed Effects Attempt (with Four Factors Linear Regression Model)
#'
# fixed effect. ----
library("plm")
data_processed_pdf <-
  data_processed %>%
  mutate_at(vars("tm_home"), funs(as.factor)) %>%
  pdata.frame(index = c("tm_home"))
# data_processed_pdf %>% select(tm_home, time) %>% head()

fmla_ff <-
  create_fmla(var_y_rgs, c(vars_ff_off_td, "ind", "tm_home"))
plm_ff_fe <-
  plm(fmla_ff, data = data_processed_pdf, model = "within")

plm_ff_ols <-
  plm(fmla_ff, data = data_processed_pdf, model = "pooling")

#
#'
#' ### Stargazer Compilation
#'
#+ echo = TRUE
stargazer::stargazer(
  plm_ff_ols,
  plm_ff_fe,
  type = "text",
  no.space = TRUE,
  column.labels = c("ols", "fe")
)
#'
#' ### Fixed Effect Model Only
#'
#+ echo = TRUE
summary(plm_ff_fe)

#'
#' ### OLS Model Only
#'
#+ echo = TRUE
summary(plm_ff_ols)
