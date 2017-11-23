

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
library("stats4")
# library("ggplot2")

# seed <- 42
# theme_set(theme_minimal())

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


if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "-analyze"
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
a <- c(1, 3, 5)
b <- c(2, 4, 9)
c <- c(4, 2, 9)
cor(a, b)
cor(a, c)
cor(b, c)


perm.groups <- function(x,n){
  nx <- length(x)
  ning <- nx/n
  
  group1 <- 
    rbind(
      matrix(rep(x[1],choose(nx-1,ning-1)),nrow=1),
      combn(x[-1],ning-1)
    )
  ng <- ncol(group1)
  
  if(n > 2){
    out <- vector('list',ng)
    
    for(i in seq_len(ng)){
      other <- perm.groups(setdiff(x,group1[,i]),n=n-1)
      out[[i]] <- lapply(seq_along(other),
                         function(j) cbind(group1[,i],other[[j]])
      )
    }
    out <- unlist(out,recursive=FALSE)
  } else {
    other <- lapply(seq_len(ng),function(i) 
      matrix(setdiff(x,group1[,i]),ncol=1)
    )
    out <- lapply(seq_len(ng),
                  function(i) cbind(group1[,i],other[[i]])
    )
  }
  out    
}

library("combinat")
split.groups <- function(x, nb.groups) {
  length.groups <- length(x)/nb.groups
  perm <- permn(x)
  perm <- lapply(perm, function(v) {
    m <- as.data.frame(matrix(v, length.groups, nb.groups))
    m <- apply(m,2,sort)
    m <- t(m)
    m <- m[order(m[,1]),]
    rownames(m) <- NULL
    m})
  unique(perm)
}

z <- 14
z_combos <- perm.groups(1:z, z / 2)
# z_groups <- split.groups(1:z, z / 2)
length(z_combos)
dim(z_combos)
nrow(z_combos)
as.data.frame(z_combos)
rm(z_combos)
gc()

lm_1 <- lm(orbpct_a_1g ~ orbpct_off_td + 0, data = results_prepared)
summary(lm_1)

lm_2 <- lm(`3ppct_a_1g` ~ `3ppct_off_td`, data = results_prepared %>% filter(g_off_td <= 18))
summary(lm_2)

lm_3 <- lm(`pts_a` ~ `pf_off_td`, data = results_prepared %>% filter(g_off_td <= 10))
summary(lm_3)

colnames_a_1g <-
  names(results_prepared) %>% 
  str_subset("_a_1g$")
sapply(results_prepared %>% select(colnames_a_1g), var)
summary(results_prepared %>% select(colnames_a_1g))


stat1 <- results_prepared %>% filter(tm_a == "SAS") %>% filter(season == "2016") %>% pull(orbpct_a_1g)
sample_vect <- sample(c(TRUE, FALSE), size = length(stat1), replace = TRUE, prob = c(0.99, 0.01))
summary(sample_vect)
stat1_sampled <- stat1[sample_vect]
# sample_vect_2 <- sample(c(TRUE, FALSE), size = length(stat1_sampled), replace = TRUE)
sample_vect_2 <- seq(1, length(stat1_sampled), by = 2)
stat1_sampled_a <- stat1_sampled[sample_vect_2]
# stat1_sampled_b <- stat1_sampled[!sample_vect_2]
stat1_sampled_b <- stat1_sampled[-sample_vect_2]
stat1_sampled_min <- min(length(stat1_sampled_a), length(stat1_sampled_b))
stat1_sampled_a2 <- stat1_sampled_a[1:stat1_sampled_min]
stat1_sampled_b2 <- stat1_sampled_b[1:stat1_sampled_min]
cor(stat1_sampled_a2, stat1_sampled_b2)

results_prepared %>% 
  filter(tm_a == "SAS") %>% 
  filter(season > 2010) %>% 
  filter(g_off_td > 2) %>% 
  ggplot(aes(x = date, y = ortg_off_td)) +
  geom_line(size = 2, color = "red") +
  geom_point(aes(x = date, y = ortg_a_1g)) +
  geom_smooth(aes(x = date, y = ortg_a_1g), size = 1, color = "blue", method = "loess", se = FALSE) +
  geom_smooth(aes(x = date, y = ortg_a_1g), size = 1, color = "green", method = "lm") +
  facet_wrap(~ season, scales = "free")
loess1 <- lm(ortg_a_1g ~ lag(ortg_a_1g), data = results_prepared %>% group_by(season, tm_a) %>% filter(g_off_td >= 5))
loess1
summary(loess1)
# results_filtered <-
#   results_prepared %>%
#   filter()

results_selected <-
  results_prepared %>% 
  select(date, season, tm_home, tm_away, pts_home, pts_away, pts_a, pts_b, ortg_a_1g, ortg_b_1g)

# log-likelihood function
ll <- function(alpha, beta) {
  alpha <- 1
  beta <- 10
  x <- results_selected$ortg_a_1g
  total <- results_selected$pts_a
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}


# maximum likelihood estimation
m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B",
         lower = c(0.0001, .1))
ab <- coef(m)
alpha0 <- ab[1]
beta0 <- ab[2]




## Avoid printing to unwarranted accuracy
od <- options(digits = 5)
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)

## Easy one-dimensional MLE:
nLL <- function(lambda) -sum(stats::dpois(y, lambda, log = TRUE))
fit0 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y))
# For 1D, this is preferable:
fit1 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y),
            method = "Brent", lower = 1, upper = 20)
stopifnot(nobs(fit0) == length(y))

## This needs a constrained parameter space: most methods will accept NA
ll <- function(ymax = 15, xhalf = 6) {
  if(ymax > 0 && xhalf > 0)
    -sum(stats::dpois(y, lambda = ymax/(1+x/xhalf), log = TRUE))
  else NA
}
(fit <- mle(ll, nobs = length(y)))
mle(ll, fixed = list(xhalf = 6))
## alternative using bounds on optimization
ll2 <- function(ymax = 15, xhalf = 6)
  -sum(stats::dpois(y, lambda = ymax/(1+x/xhalf), log = TRUE))
mle(ll2, method = "L-BFGS-B", lower = rep(0, 2))
