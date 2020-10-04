#' Performing CV using random forest
#' @param cv_obj outputs of the cvPartition function
#' @author Kevin Wang
#' @importFrom purrr map2 map2_dbl
#' @importFrom randomForest randomForest
#' @importFrom tibble lst
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = factor(iris[51:150, 5])
#' dat = data.frame(x, y)
#' library(tidymodels)
#'
#' ## Making rsplit objects in a tibble
#' cv_obj = rsample::vfold_cv(data = dat, v = 5, repeats = 10)
#'
#' ## Specify RF model through parsnip
#' rf_model = rand_forest() %>%
#' set_args(trees = 100) %>%
#' set_engine("randomForest") %>%
#' set_mode("classification")
#'
#' ## Fitting model
#' rf_model %>%
#'  fit(formula = Species ~ ., data = cv_obj)
#'
#' #### cv_obj = cv_partition(x = x, y = y, nfolds = 5)
#' rf_cv(cv_obj)
rf_cv = function(cv_obj, ...){
  ## This looks at each pairings of training data (a total of nfolds pairings) and create a random forest model
  rf_objs = purrr::map2(.x = cv_obj$train_x,
                        .y = cv_obj$train_y,
                        ~ randomForest::randomForest(
                          x = .x,
                          y = factor(.y)), ....)

  ## prediction are then made on each of the test data. A total of nfolds test data are created.
  ## So the prediction is always on the complement of samples in the training data.
  rf_predict_class = purrr::map2(.x = rf_objs,
                                 .y = cv_obj$test_x,
                                 ~ predict(object = .x,
                                           newdata = .y,
                                           type = "response"))

  rf_predict_prob = purrr::map2(.x = rf_objs,
                                .y = cv_obj$test_x,
                                ~ predict(object = .x,
                                          newdata = .y,
                                          type = "prob"))

  rf_predict_class_vec = lapply(rf_predict_class, as.vector)

  rf_fold_error = purrr::map2_dbl(.x = rf_predict_class_vec,
                                  .y = cv_obj$test_y,
                                  ~ mean( .x != .y))

  rf_mean_error = mean(rf_fold_error, na.rm = TRUE)

  ## We combine the prediction results across all samples across all folds,
  ## and assign them with the same names as before
  rf_predict_class_vec = unlist(rf_predict_class)
  rf_predict_prob_mat = do.call(rbind, rf_predict_prob)

  rf_predict_class_vec_ordered = rf_predict_class_vec[cv_obj$original_index]
  names(rf_predict_class_vec_ordered) = names(cv_obj$original_names)

  rf_predict_prob_mat_ordered = rf_predict_prob_mat[cv_obj$original_index, , drop = FALSE]
  rownames(rf_predict_prob_mat_ordered) = names(cv_obj$original_names)


  result = tibble::lst(fold_error = rf_fold_error,
                       mean_error = rf_mean_error,
                       predict_class = rf_predict_class_vec_ordered,
                       predict_prob = rf_predict_prob_mat_ordered)

  return(result)
}
