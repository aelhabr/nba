
#'
#' Use `caret` for supervised learning.
#'
# caret models. ----
fmla_experiment_1 <- fmla_rest_x1_y1a
fmla_experiment_2 <- fmla_rest_x2_y1a
if (try_old_code == TRUE) {
  glm_fit_experiment_x1_y1a <-
    caret::train(fmla_experiment_1,
                 filter(model_data, season %in% seasons_trn),
                 method = "glm")
  # glm_fit_experiment_x1_y1a$finalModel
  # glm_fit_experiment_x1_y1a$results
  calculate_class_accuracy(
    predict(
      glm_fit_experiment_x1_y1a,
      filter(model_data, season %in% seasons_tst)
    ),
    filter(model_data, season %in% seasons_tst) %>% pull(var_y1a)
  )
  
  glm_fit_experiment_x2_y1a <-
    caret::train(fmla_experiment_2,
                 filter(model_data, season %in% seasons_trn),
                 method = "glm")
  calculate_class_accuracy(
    predict(
      glm_fit_experiment_x2_y1a,
      filter(model_data, season %in% seasons_tst)
    ),
    filter(model_data, season %in% seasons_tst) %>% pull(var_y1a)
  )
  
  knn_fit_experiment_x1_y1a <-
    caret::train(
      fmla_experiment_1,
      filter(model_data, season %in% seasons_trn) %>% mutate_all(as.numeric),
      method = "knn",
      # trControl = trainControl("cv"),
      tuneGrid = data.frame(k = c(10, 50, 75))
    )
  
  knn_fit_experiment_x1_y1a$results
  calculate_class_accuracy(
    predict(
      knn_fit_experiment_x1_y1a,
      filter(model_data, season %in% seasons_tst)
    ),
    filter(model_data, season %in% seasons_tst) %>% pull(var_y1a)
  )
  
  knn_fit_experiment_x2_y1a <-
    caret::train(
      fmla_experiment_2,
      filter(model_data, season %in% seasons_trn) %>% mutate_all(as.numeric),
      method = "knn",
      # trControl = trainControl("cv"),
      tuneGrid = data.frame(k = c(10, 50, 75))
    )
  
  knn_fit_experiment_x2_y1a$results
  calculate_class_accuracy(
    predict(
      knn_fit_experiment_x2_y1a,
      filter(model_data, season %in% seasons_tst)
    ),
    filter(model_data, season %in% seasons_tst) %>% pull(var_y1a)
  )
  
  model_list <-
    list(
      # glm_1 = glm_fit_experiment_x1_y1a,
      # glm_2 = glm_fit_experiment_x2_y1a,
      knn_1 = knn_fit_experiment_x1_y1a,
      knn_2 = knn_fit_experiment_x2_y1a
    )
  model_resamples <- resamples(model_list)
  summary(model_resamples)
  compare_models(glm_fit_experiment_x1_y1a, glm_fit_experiment_x2_y1a)
  
}

#'
#'
#'
