
#'
#'
#'
# These are "general" functions that can be applied in different situations.
add_tickmarks <- function(x) {
  ifelse(grepl("^[0-9]", x) == TRUE, paste0("`", x, "`"), x)
}

create_fmla <- function(var_y, vars_x, keep_as_char = FALSE) {
  vars_x <- add_tickmarks(vars_x)
  var_y <- add_tickmarks(var_y)
  fmla <- paste0(var_y, " ~ ", paste(vars_x, collapse = " + "))

  if (keep_as_char == FALSE) {
    fmla <- as.formula(fmla)
  }
}

convert_fmla_to_char <- function(fmla) {
  if (class(fmla) != "formula") {
    warning("fmla is not of type 'formula'")
    return(fmla)
  }
  paste(fmla[2], fmla[3], sep = "~")
}

calculate_class_accuracy <- function(y_pred, y_actual) {
  if (class(y_pred) != "character" | class(y_actual) != "character") {
    # stop("y_pred and/or y_actual should be a character vector")
    if (class(y_pred) != "character") {
      # stop("y_pred should be a character vector")
      y_pred <- as.character(y_pred)
      message("converting y_pred to a character vector")
    }
    if (class(y_actual) != "character") {
      # stop("y_actual should be a character vector")
      y_actual <- as.character(y_actual)
      message("converting y_actual to a character vector")
    }
  }
  # sum(diag(table(y_actual, y_pred))) / sum(table(y_actual, y_pred))
  mean(y_actual == y_pred)
}

convert_pred_to_numeric <-
  function(y_pred,
           high_char = "w",
           low_char = "l",
           high_num = 1,
           low_num = 0) {
    ifelse(y_pred == high_char,
           high_num,
           ifelse(y_pred == low_char, low_num, as.numeric(NULL)))
  }

convert_numeric_to_pred <-
  function(y_pred,
           high_char = "w",
           low_char = "l",
           high_num = 2,
           low_num = 1) {
    ifelse(y_pred == high_num,
           high_char,
           ifelse(y_pred == low_num, low_char, as.character(NULL)))
  }

convert_prob_to_pred <-
  function(y_pred_prob,
           y_actual,
           cutoff = 0.5,
           choose_opt = FALSE,
           high_char = "w",
           low_char = "l",
           high_num = 1,
           low_num = 0) {
    # y_pred_prob <- y_pred_tst
    # cutoff <- 0.5
    # choose_opt <- TRUE
    # high_char <- "w"
    # low_char <- "l"

    if (class(y_actual) != "character") {
      y_actual <- as.character(y_actual)
      message("y_actual converted to a character vector")
    }

    if (choose_opt == TRUE) {
      y_pred_numeric <-
        convert_pred_to_numeric(y_actual, high_char, low_char, high_num, low_num)
      cutoff <-
        InformationValue::optimalCutoff(y_actual_numeric, y_pred_prob)
    }

    y_pred <- y_pred_prob
    if (!is.null(names(y_pred))) {
      names(y_pred) <- NULL
    }
    y_pred[y_pred >= cutoff] <- high_char
    y_pred[y_pred < cutoff] <- low_char
    y_pred
  }

diagnose_model_performance <-
  function(fmla = NULL,
           trn_data = NULL,
           tst_data = NULL,
           method = NULL,
           params = NULL,
           cutoff = 0.5,
           choose_opt = FALSE,
           high_char = "w",
           low_char = "l",
           high_num = 1,
           low_num = 0,
           return_preds = FALSE) {
    # fmla <- fmla_ctg_1
    # trn_data <- filter(model_data, season %in% seasons_trn)
    # tst_data <- filter(model_data, season %in% seasons_tst)
    # method <- "nb"
    # high <- "w"
    # low <- "l"
    # choose_opt <- FALSE
    # # k <- 50
    # # k <- params$k

    var_y <- as.character(fmla)[2]

    y_trn <-
      trn_data %>%
      select(one_of(var_y)) %>%
      pull(var_y) %>%
      as.character()

    y_tst <-
      tst_data %>%
      select(one_of(var_y)) %>%
      pull(var_y) %>%
      as.character()

    if (method %in% c("glm", "lda", "qda", "nb", "svm")) {
      if (method == "glm") {
        trn_time_details <-
          system.time(fit <-
                        stats::glm(fmla, trn_data, family = "binomial"))
      } else if (method %in% c("lda", "qda")) {
        if (method == "lda") {
          trn_time_details <-
            system.time(fit <- MASS::lda(fmla, trn_data))
        } else if (method == "qda") {
          trn_time_details <-
            system.time(fit <- MASS::qda(fmla, trn_data))
        }
      } else if (method == "nb") {
        trn_time_details <-
          system.time(fit <- e1071::naiveBayes(fmla, trn_data))
      } else if (method == "svm") {
        kernel <- params$kernel
        cost <- params$cost
        scale <- params$scale
        # kernel <- "Linear"
        # cost <- 0.01
        # scale <- FALSE
        trn_time_details <-
          system.time(fit <-
                        e1071::svm(
                          fmla,
                          trn_data,
                          kernel = kernel,
                          cost = cost,
                          scale = scale
                        ))

      }
      fit

      if (method == "glm") {
        y_prob_trn <- predict(fit, type = "response")
        y_prob_tst <- predict(fit, tst_data, type = "response")
        y_pred_trn <-
          convert_prob_to_pred(y_prob_trn,
                               y_trn,
                               cutoff,
                               choose_opt,
                               high_char,
                               low_char,
                               high_num,
                               low_num)
        y_pred_tst <-
          convert_prob_to_pred(y_prob_tst,
                               y_tst,
                               cutoff,
                               choose_opt,
                               high_char,
                               low_char,
                               high_num,
                               low_num)

      } else if (method %in% c("lda", "qda")) {
        y_pred_trn <- predict(fit)$class %>% as.character()
        y_pred_tst <-
          predict(fit, tst_data)$class %>% as.character()
      } else if (method %in% c("nb", "svm")) {
        # Must specify newdata.
        # Also, must specify type = "class" instead of "response".
        y_pred_trn <-
          predict(fit, newdata = trn_data, type = "class") %>% as.character()
        y_pred_tst <-
          predict(fit, newdata = tst_data, type = "class") %>% as.character()
      }
    } else if (method == "knn") {
      vars_x <-
        as.character(fmla)[3] %>% str_split(pattern = "\\s\\+\\s") %>% unlist()
      x_trn <- trn_data %>% select(one_of(vars_x))
      x_tst <- tst_data %>% select(one_of(vars_x))

      k <- params$k
      trn_time_details_1 <-
        system.time(y_pred_trn <-
                      class::knn(x_trn, x_trn, y_trn, k = k))
      trn_time_details_2 <-
        system.time(y_pred_tst <-
                      class::knn(x_trn, x_tst, y_trn, k = k))
      # trn_time_details <-  rbind(trn_time_details_1, trn_time_details_2)
      trn_time_details <- trn_time_details_1
    }

    if (return_preds == FALSE) {
      # Calculate accuracy.
      acc_trn <- calculate_class_accuracy(y_pred_trn, y_trn)
      acc_tst <- calculate_class_accuracy(y_pred_tst, y_tst)

      trn_time <- as.numeric(trn_time_details[3])
      tibble(trn_time = trn_time,
             acc_trn = acc_trn,
             acc_tst = acc_tst)
    } else {
      if (method == "knn") {
        y_pred_trn <-
          convert_numeric_to_pred(y_pred_trn)
        y_pred_tst <-
          convert_numeric_to_pred(y_pred_tst)
        y_trn <-
          convert_numeric_to_pred(y_trn)
        y_tst <-
          convert_numeric_to_pred(y_tst)
      }

      preds_trn <-
        tibble(set = "train",
               pred = y_pred_trn,
               actual = y_trn) %>% mutate(n = row_number())
      preds_tst <-
        tibble(set = "test",
               pred = y_pred_tst,
               actual = y_tst) %>% mutate(n = row_number())
      bind_rows(preds_trn, preds_tst)
    }
  }

# This is a modified version of paste3() from
# https://stackoverflow.com/questions/13673894/suppress-nas-in-paste.
# It implements paste() and suppresses NAs.
paste2 <- function(..., sep = "_") {
  l <- list(...)
  l <- lapply(l, function(x) {
    x[is.na(x)] <- ""
    x
  })
  output <- gsub(paste0("(^", sep, "|", sep, "$)"),
                 "",
                 gsub(paste0(sep, sep), sep,
                      do.call(paste, c(
                        l, list(sep = sep)
                      ))))
  is.na(output) <- output == ""
  output <- gsub("_$", "", output)
  output
}
