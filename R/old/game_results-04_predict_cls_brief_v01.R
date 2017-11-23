

trn_control_cv <- caret::trainControl(method = "cv", number = 10)

set.seed(seed)
glm_cv_1 <-
  caret::train(fmla_1cls,
               data = data_1,
               method = "glm",
               trControl = trn_control_cv)
glm_cv_1
glm_cv_1$results
glm_cv_1$finalModel

glmnet_default_1 <-
  caret::train(x = x_1, y = y_1cls, method = "glmnet")
glmnet_default_1

grid_rdg_1 <-
  expand.grid(alpha = 0, lambda = seq(0.00, 1, by = 0.1))
grid_lso_1 <-
  expand.grid(alpha = 1, lambda = seq(0.00, 1, by = 0.1))
grid_elnet_1 <-
  expand.grid(alpha = seq(0, 1, by = 0.25),
              lambda = seq(0.1, 10, by = (10 - 0.1) / 5))

glmnet_lso_1 <-
  caret::train(
    x = x_1,
    y = y_1cls,
    method = "glmnet",
    tuneGrid = grid_lso_1
  )
glmnet_lso_1

set.seed(seed)
knn_1 <-
  class::knn(
    train = scale(x_1[trn_1_idx, ]),
    test = scale((x_1[-trn_1_idx, ])),
    cl = y_1cls[trn_1_idx],
    k  = 5
  )
knn_1

calculate_class_err <- function(actual, predicted) {
  mean(actual != predicted)
}

calculate_class_err(actual = y_1cls[-trn_1_idx], predicted = knn_1)

set.seed(seed)
k_to_try <- 1:100
err_k <- rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred <-
    class::knn(
      train = scale(x_1[trn_1_idx, ]),
      test = scale((x_1[-trn_1_idx, ])),
      cl = y_1cls[trn_1_idx],
      k = k_to_try[i]
    )
  err_k[i] = calculate_class_err(y_1cls[-trn_1_idx], pred)
}

min(err_k)
which(err_k == min(err_k))

plot(
  err_k,
  type = "b",
  col = "dodgerblue",
  cex = 1,
  pch = 20,
  xlab = "k, number of neighbors",
  ylab = "classification error",
  main = "(Test) Error Rate vs Neighbors"
)
abline(h = min(err_k),
       col = "darkorange",
       lty = 3)
