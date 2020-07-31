#' Performing CV using svm
#' @param cv_obj outputs of the cvPartition function
#' @author Kevin Wang
#' @importFrom purrr map2 map2_dbl
#' @importFrom e1071 svm
#' @importFrom tibble lst
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = factor(iris[51:150, 5])
#' cv_obj = cv_partition(x = x, y = y, nfolds = 5)
#' svm_cv(cv_obj)
svm_cv = function(cv_obj){
  ## This looks at each pairings of training data (a total of nfolds pairings) and create a random forest model
  svm_objs = purrr::map2(.x = cv_obj$train_x,
                         .y = cv_obj$train_y,
                         .f = ~ e1071::svm(
                           x = .x,
                           y = factor(.y), probability = TRUE))

  ## prediction are then made on each of the test data. A total of nfolds test data are created.
  ## So the prediction is always on the complement of samples in the training data.
  svm_predict_prob = purrr::map2(.x = svm_objs,
                                 .y = cv_obj$test_x,
                                 ~ attr(predict(object = .x,
                                                newdata = .y,
                                                probability = TRUE),
                                        "probabilities"))

  svm_predict_class = purrr::map2(.x = svm_objs,
                                  .y = cv_obj$test_x,
                                  ~ predict(object = .x,
                                            newdata = .y))

  svm_predict_class_vec = lapply(svm_predict_class, as.vector)

  svm_fold_error = purrr::map2_dbl(.x = svm_predict_class_vec,
                                   .y = cv_obj$test_y,
                                   ~ mean( .x != .y))

  svm_mean_error = mean(svm_fold_error, na.rm = TRUE)

  ## We combine the prediction results across all samples across all folds,
  ## and assign them with the same names as before
  svm_predict_class_vec = unlist(svm_predict_class)
  svm_predict_prob_mat = do.call(rbind, svm_predict_prob)

  svm_predict_class_vec_ordered = svm_predict_class_vec[cv_obj$original_index]
  names(svm_predict_class_vec_ordered) = names(cv_obj$original_names)

  svm_predict_prob_mat_ordered = svm_predict_prob_mat[cv_obj$original_index, , drop = FALSE]
  rownames(svm_predict_prob_mat_ordered) = names(cv_obj$original_names)


  result = tibble::lst(fold_error = svm_fold_error,
                       mean_error = svm_mean_error,
                       predict_class = svm_predict_class_vec_ordered,
                       predict_prob = svm_predict_prob_mat_ordered)

  return(result)
}
