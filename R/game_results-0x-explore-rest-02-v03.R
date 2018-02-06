#' ---
#' title: "Exploring Texas High School Academic UIL Competition Results"
#' author: "Tony"
#' output:
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#'    css: markdown7.css
#' ---
#'
#'
#'
#+ include = FALSE
knitr::opts_chunk$set(
  include = FALSE,
  echo = FALSE,
  cache = TRUE,
  fig.align = "center",
  # width = 100,
  warning = FALSE,
  message = FALSE
)

rm(list = ls())
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
#'
#'
#+ include = FALSE
# User-defined parameters. ----
g_td_cut_low <- 10
drest_cut_low <- 1
rtrip_low <- 2
rtrip_ctr <- 1
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
  names(results_prepared_min) %>% str_subset("result_t|prob|_class")

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

data_0 %>% filter(tm == "GSW", season == 2016, rn == 1)

#'
#'
#'
# Other constants. ----
vars_ctg <-
  names(data_filtered) %>%
  str_subset("ortg|drtg|efg|tov|orb|ftr")
vars_ctg_td <- vars_ctg %>% str_subset("_td")

vars_ctg_td_rescale <-
  vars_ctg_td %>% str_subset("ortg|drtg|tov|orb")

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
data_filtered %>% summarise_at(vars(vars_ctg_td_rescale), funs(mean)) %>% t()
data_processed <-
  data_filtered %>%
  mutate_at(vars(vars_ctg_td_rescale), funs(. / 100)) %>%
  mutate(ind = ifelse(rtrip >= rtrip_low |
                        rtrip_opp >= rtrip_low, 1, 0)) %>%
  mutate(ind2 = ifelse(rtrip == rtrip_ctr |
                         rtrip_opp == rtrip_ctr, 1, 0)) %>%
  select(ind, ind2, everything())
#'
#'
#'
data_processed %>% select(ind, ind2) %>% table()

data_processed %>%
  filter(tm_home == "SAS", season == 2016)
data_processed %>% group_by(tm) %>% count() %>% arrange(desc(n))
#'
#'
#'

tidy_ha_metrics <- function(d, vars, vars_base = "") {
  # d <- data
  # vars <- vars_ff_off_td
  # vars_base <- "" # c("date", "season", "tm_h", "tm_a")
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
vars_base <- c("date", "season", "tm_ha", "ind")
viz_ha_ff <-
  data_ha %>%
  tidy_ha_metrics(vars = vars_ff_off_td_ha, vars_base = vars_base) %>%
  ggplot(aes(x = value, fill = ha)) +
  # geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.5) +
  facet_wrap(~ metric, scales = "free")
viz_ha_ff
# ggsave(
#   "figs/viz_ha_ff.png",
#   width = 11,
#   height = 11,
#   units = "in"
# )

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
# ggsave(
#   "figs/viz_ha_ff_ind.png",
#   width = 11,
#   height = 11,
#   units = "in"
# )

viz_pd <-
  data_ha %>%
  ggplot(aes(x = pd_h2a)) +
  geom_histogram()
viz_pd
# ggsave(
#   "figs/viz_pd.png",
#   width = 11,
#   height = 11,
#   units = "in"
# )

#'
#' Four Factors model(s).
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
    #
    #     d <- data
    #     var_y <- var_y_rgs
    #     vars_x <- c(vars_ff_off_td, "hfa")
    #     intercept <- FALSE
    #     normalize <- TRUE
    #     classification <- FALSE

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
lm_hfa <-
  data_processed %>%
  get_glm(var_y_rgs, c("hfa"), intercept = FALSE)

lm_hfa_ind <-
  data_processed %>%
  get_glm(var_y_rgs, c("hfa", "ind"), intercept = FALSE)

lm_ff <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_ff_off_td),
          intercept = FALSE)

lm_ff_hfa <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_ff_off_td, "hfa"),
          intercept = FALSE)

lm_ff_ind <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_ff_off_td, "ind"),
          intercept = FALSE)

# data_processed %>%
#   filter(hfa == 0) %>%
#   get_glm(var_y_rgs,
#           c(vars_ff_off_td_tm),
#           intercept = TRUE,
#           print = TRUE)

lm_ff_hfa_ind  <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_ff_off_td, "hfa", "ind"),
          intercept = FALSE)


lm_ff_hfa_ind_normalized <-
  data_processed %>%
  get_glm(
    var_y_rgs,
    c(vars_ff_off_td, "hfa", "ind"),
    intercept = FALSE,
    normalize = TRUE,
    vars_normalize = vars_ff_off_td
  )

stargazer::stargazer(lm_hfa,
                     lm_hfa_ind,
                     type = "text",
                     no.space = TRUE)
stargazer::stargazer(
  lm_ff,
  lm_ff_hfa,
  lm_ff_ind,
  lm_ff_hfa_ind,
  lm_ff_hfa_ind_normalized,
  type = "text",
  no.space = TRUE
)

lm_rtg <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_rtg_off_td),
          intercept = FALSE)

lm_rtg_hfa <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_rtg_off_td, "hfa"),
          intercept = FALSE)

lm_rtg_ind <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_rtg_off_td, "ind"),
          intercept = FALSE)

lm_rtg_hfa_ind <-
  data_processed %>%
  get_glm(var_y_rgs,
          c(vars_rtg_off_td, "hfa", "ind"),
          intercept = FALSE)

lm_rtg_hfa_ind_normalized <-
  data_processed %>%
  get_glm(
    var_y_rgs,
    c(vars_rtg_off_td, "hfa", "ind"),
    intercept = FALSE,
    normalize = TRUE,
    vars_normalize = vars_rtg_off_td
  )

stargazer::stargazer(
  lm_rtg,
  lm_rtg_hfa,
  lm_rtg_ind,
  lm_rtg_hfa_ind,
  lm_rtg_hfa_ind_normalized,
  type = "text",
  no.space = TRUE
)


#'
#'
#'
#+ include = TRUE
stargazer::stargazer(
  lm_hfa,
  lm_hfa_ind,
  lm_ff_ind,
  lm_rtg_ind,
  type = "text",
  no.space = TRUE,
  column.labels = c("hfa only", "hfa", "ff", "rtg")
)
summary(lm_ff_ind)
summary(lm_rtg_ind)
#'
#'
#'
#'
tms <- data_processed %>% distinct(tm) %>% arrange(tm) %>% pull(tm)
tms
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
#'
#'
#+ include = TRUE
summ %>%
  gather(metric, value, -tm, -term) %>%
  filter(metric %in% c("estimate", "p.value")) %>%
  filter(term == "ind") %>%
  spread(metric, value) %>%
  arrange(p.value)
#'
#'
#'
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
# ggsave("figs/viz_ha_ff_ind_byteam_pvals.png", width = 11, height = 11, units = "in")

#'
#'
#'
glm_hfa <-
  data_processed %>%
  get_glm(
    var_y_cls,
    c("hfa"),
    intercept = FALSE,
    classification = TRUE
  )

glm_hfa_ind <-
  data_processed %>%
  get_glm(
    var_y_cls,
    c("hfa", "ind"),
    intercept = FALSE,
    classification = TRUE
  )

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
#'
#'
stargazer::stargazer(
  glm_hfa,
  glm_hfa_ind,
  glm_ff_ind,
  glm_rtg_ind,
  type = "text",
  no.space = TRUE
)

summary(glm_ff_ind)
summary(glm_rtg_ind)
