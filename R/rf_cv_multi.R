#' Performing CV using random forest, allowing multiple cores
#' @param x design matrix
#' @param y factor
#' @param nfolds number of CV folds
#' @param nexp number of loops
#' @author Kevin Wang
#' @importFrom furrr future_map
#' @importFrom future multisession
#' @export
#' @rdname rf_cv
#' @examples
#' x = iris[51:150, -5]
#' y = factor(iris[51:150, 5])
#' res = rf_cv_multi(x = x, y = y)
#' \dontrun{
#' library(furrr)
#' plan(multisession(workers = 5))
#' res = rf_cv_multi(x = x, y = y, nexp = 20)
#' plan(future::sequential)
#' }

rf_cv_multi = function(x, y,
                       nfolds = 5,
                       nexp = 5){

  list_data_partitions = replicate(nexp,
                                   {cv_partition(x = x, y = y, nfolds = nfolds)},
                                   simplify = FALSE)
  d = paste0("exp_%0", ceiling(log10(nexp)) + 1L, "d")

  names(list_data_partitions) = sprintf(d, seq_len(nexp))

  list_prediction = furrr::future_map(.x = seq_len(nexp),
                                      .f = ~ rf_cv(list_data_partitions[[.x]]),
                                      .progress = TRUE)
  return(list_prediction)
}


#' Performing CV using random forest
#' @param cv_obj outputs of the cvPartition function
#' @author Kevin Wang
#' @importFrom purrr map2 map2_dbl
#' @importFrom randomForest randomForest
#' @importFrom tibble lst
#' @rdname rf_cv
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = factor(iris[51:150, 5])
#' cv_obj = cv_partition(x = x, y = y, nfolds = 5)
#' rf_cv(cv_obj)
rf_cv = function(cv_obj){
  ## This looks at each pairings of training data (a total of nfolds pairings) and create a random forest model
  rf_objs = purrr::map2(.x = cv_obj$train_x,
                        .y = cv_obj$train_y,
                        ~ randomForest::randomForest(
                          x = .x,
                          y = factor(.y)))

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
