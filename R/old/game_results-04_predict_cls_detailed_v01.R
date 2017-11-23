

#'
#' # Logistic Regression
#'
#' ## Creating and evaluting the model.
#'
#' ### Using a basic approach. [^fn_lm_basic]
#'
# basic: glm_1cls ----
# glm_1cls <- glm(fmla_1cls, data = data_1cls_trn, family = binomial(link = "logit"))
glm_1cls <-
  glm(fmla_1cls, data = data_1cls_trn, family = "binomial")
glm_1cls

summary(glm_1cls)
summary(glm_1cls)$coef
coef(glm_1cls)

car::vif(glm_1cls)

#'
#' ### Using the broom package. [^fn_broom_link]
#'
#' The same broom functions shown for regression could be used in the same fashion.
#'
#' ### Using the caret package.
#'
# caret: glm_1cls ----
set.seed(seed)
glm_1cls_caret <-
  train(fmla_1cls,
        data = na.exclude(data_1cls_trn),
        method = "glm")
glm_1cls_caret$finalModel
summary(glm_1cls_caret$finalModel)
summary(glm_1cls_caret)$deviance
summary(glm_1cls_caret$finalModel)$deviance

#'
#'
#+ include = FALSE
if (remove_tempvars == TRUE) {
  rm(list = c("glm_1cls_caret"))
}

#'
#' ## Creating and evaluating.
#'
#' ### Following the example at r.statistics.co
#'
#' http://r-statistics.co/Logistic-Regression-With-R.html
#'
# rsco: "Check class bias." ---
table(select(results_prepared, one_of(colname_y_1cls)))
table(select(data_1cls, colname_y_1cls))

# rsco: "Create Training Data" ----
# filter(select(data_1, colname_y_1cls), pd_h2a < 0)
# mutate(select(data_1cls, colname_y_1cls), !!(colname_y_1cls) := colname_y_1cls)
# ...

# rsco: "Create WOE for Categorical Variables (optional)" ----
# ...

#
# rsco: "Compute Informational Values" ----
# library("smbinning")
vars_fct <- NULL
vars_cont <- colnames(x_1)
ivs <-
  data.frame(var = c(vars_fct, vars_cont), iv = numeric(ncol(x_1)))

# Converting to fit requirements for smbinning() function.
data_1cls_binary <-
  data_1cls %>%
  mutate(result = as.numeric(if_else(result == "W", 1, 0))) %>%
  as.data.frame()

for (var_cont in vars_cont) {
  # var_cont <- "pd_td_opp"
  smb <-
    smbinning::smbinning(data_1cls_binary, y = colname_y_1cls, x = var_cont)
  smb
  if (class(smb) != "character") {
    ivs[ivs$var == var_cont, "iv"] <- smb$iv
  }
}

ivs <- ivs[order(-ivs$iv),]
ivs

# rsco: "Build Logit Models and Predict" ----
# library("InformationValue")
data_1cls_binary_trn <-
  data_1cls_binary[trn_1cls_idx,] %>% na.omit()
head(data_1cls_binary_trn)
data_1cls_binary_tst <-
  data_1cls_binary[-trn_1cls_idx,] %>% na.omit()
head(data_1cls_binary_tst)

# glm_1cls_binary <- glm(fmla_1cls, data = data_1cls_binary_trn, family = binomial(link = "logit"))
glm_1cls_binary <-
  glm(fmla_1cls, data = data_1cls_binary_trn, family = "binomial")
# glm_1cls_predict <- plogis(predict(glm_1cls, newdata = data_1cls_tst))
glm_1cls_predict <-
  predict(glm_1cls, newdata = data_1cls_binary_tst, type = "response")

## "Decide on optimal prediction probability cutoff for the model"
data_1cls_tst_y <-
  data_1cls_tst %>%
  mutate(result = as.numeric(if_else(result == "W", 1, 0))) %>%
  pull(result)
head(data_1cls_tst_y)
cutoff_optimal <-
  InformationValue::optimalCutoff(actuals = data_1cls_tst_y , predictedScores = glm_1cls_predict)
cutoff_optimal

# rsco: "Model Diagnostics" ----
# summary(glm_1cls_binary)

## "VIF"
# car::vic(glm_1cls_binary)

## "Misclassification Error"
InformationValue::misClassError(actuals = data_1cls_tst_y,
                                predictedScores = glm_1cls_predict,
                                threshold = cutoff_optimal)

## "ROC"
InformationValue::plotROC(actuals = data_1cls_tst_y, predictedScores = glm_1cls_predict)

## "Concordance"
InformationValue::Concordance(actuals = data_1cls_tst_y, predictedScores = glm_1cls_predict)

## "Specificity and Sensitivity"
InformationValue::specificity(actuals = data_1cls_tst_y,
                              predictedScores = glm_1cls_predict,
                              threshold = cutoff_optimal)
InformationValue::sensitivity(actuals = data_1cls_tst_y,
                              predictedScores = glm_1cls_predict,
                              threshold = cutoff_optimal)

## "Confusion Matrix"
InformationValue::confusionMatrix(actuals = data_1cls_tst_y,
                                  predictedScores = glm_1cls_predict,
                                  threshold = cutoff_optimal)

#'
#' ### Using the glmnet package.
#'
# glmnet: glmnet_1cls ----
glmnet_1cls <-
  glmnet(as.matrix(data_1cls_trn[, -1]), as.matrix(data_1cls_trn[, 1]), family = "binomial")

# Warning: Do not print the variable directly!
summary(glmnet_1cls)
plot(glmnet_1cls)
coef(glmnet_1cls, s = 0.1)

#'
#' ### Using the caret() package.
#'
# caret: glmnet_1cls ----
glmnet_1cls_caret <-
  train(fmla_1cls, data = data_1cls_trn, method = "glmnet")
summary(glmnet_1cls_caret$finalModel)
summary(glmnet_1cls_caret$finalModel)$r.squared
