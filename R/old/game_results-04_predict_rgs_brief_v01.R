#'
#'
#'
#+ include = FALSE
rm(list = ls())
setwd("O:/_other/projects/nba/")

#'
#'
#'
#+ include = FALSE
# Parameters. ----
filename_import_path <- "data/game_results-predict_init.RData"
load(filename_import_path)

#'
#'
#'
#'
set.seed(seed)
trn_1_idx <-
  caret::createDataPartition(y_1rgs,
                      p = 0.8,
                      list = FALSE,
                      times = 1)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

set.seed(seed)
knn_rgs_1 <-
  FNN::knn.reg(
    train = scale(x_1[trn_1_idx, ]),
    test = scale((x_1[-trn_1_idx, ])),
    y = y_1rgs[trn_1_idx],
    k  = 5
  )
rmse(knn_rgs_1$pred, y_1rgs[-trn_1_idx])

make_knn_pred <- function(k = 1, x_trn, x_tst, y_trn, y_tst) {
  pred <- FNN::knn.reg(
    train = x_trn,
    test = x_tst,
    y = y_trn,
    k = k
  )$pred
  rmse(predicted = pred, actual = y_tst)
}

# ks <- 1 * 10^(0:4)
# ks <- 0.25 * 10^(2:6)
# ks <- 1 * 10^(seq(0, 4, by = 0.5))
ks <- seq(10, 200, by = 10)

knn_trn_rmse <-
  sapply(
    ks,
    make_knn_pred,
    x_trn = x_1[trn_1_idx, ],
    x_tst = x_1[-trn_1_idx, ],
    y_trn = y_1rgs[trn_1_idx],
    y_tst = y_1rgs[trn_1_idx]
  )
knn_trn_rmse

knn_tst_rmse <-
  sapply(
    ks,
    make_knn_pred,
    x_trn = x_1[trn_1_idx, ],
    x_tst = x_1[-trn_1_idx, ],
    y_trn = y_1rgs[trn_1_idx],
    y_tst = y_1rgs[-trn_1_idx]
  )
knn_tst_rmse

best_k <- ks[which.min(knn_tst_rmse)]
best_k

fit_status <- ifelse(ks < best_k, "Over", ifelse(ks == best_k, "Best", "Under"))
fit_status

knn_results <- tibble(
  ks,
  round(knn_trn_rmse, 2),
  round(knn_tst_rmse, 2),
  fit_status
)
colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)


