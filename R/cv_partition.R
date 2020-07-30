#' Creating CV data partition
#' @param x x should be a matrix without rownames
#' @param y y should be true labels
#' @param nfolds number of folds
#' @importFrom caret createFolds
#' @author Kevin Wang
#' @importFrom dplyr %>%
#' @importFrom caret createFolds
#' @importFrom tibble lst
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = factor(iris[51:150, 5])
#' cvp = cv_partition(x = x, y = y, nfolds = 5)
#' cvp
cv_partition = function(x, y, nfolds){
  n = length(y) ## The number of observations
  # obsNum = paste0("obs", seq_len(n))
  # rownames(x) = obsNum
  # names(y) = obsNum


  ## These steps create indices to create CV paritions
  test_index = caret::createFolds(y, k = nfolds) ## Creating test index
  train_index = lapply(test_index, function(i){(1:n)[-i]}) ## The train index is mutually exclusive to the test index
  original_index = order(unlist(test_index))

  ## Each train/test index for each fold is then used to subset the X and y data.
  test_x = lapply(test_index, function(k) {x[k, ,drop = FALSE]})
  test_y = lapply(test_index, function(k) {y[k]})
  train_x = lapply(train_index, function(k) x[k, ,drop = FALSE])
  train_y = lapply(train_index, function(k) {y[k]})

  result = tibble::lst(fold_num = names(test_index),
                       test_x,
                       test_y,
                       train_x,
                       train_y,
                       original_index,
                       original_names = names(y))
  class(result) = "cv_object"
  return(result)
}


cv_object <- function(x, ...) UseMethod("cv_object")

#' @title Printing cv_partition object
#' @param cv_object Output from cv_partition
cv_object = function(cv_object){
  cat("Number of samples in training data folds")
  print(purrr::map_int(cv_object$train_y, length))

  cat("Number of samples in test data folds")
  purrr::map_int(cv_object$test_y, length)
}



cv_pred_result <- function(x, ...) UseMethod("cv_pred_result")

#' @title Printing cv_partition object
#' @param cv_pred_result Output from *_cv functions
#' @importFrom utils head
print.cv_pred_result = function(cv_pred_result){
  cat("First 5 sample predictions")
  print(head(cv_pred_result$fold_error, 5))
  print(head(cv_pred_result$predict_class, 5))
  print(head(cv_pred_result$predict_prob, 5))
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
