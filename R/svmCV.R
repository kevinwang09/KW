#' Performing CV using svm
#' @param cvObj outputs of the cvPartition function
#' @import purrr
#' @import e1071
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = iris[51:150, 5] %>% as.factor
#' cvp = cvPartition(x = x, y = y, nFolds = 5)
#' table(pred = svmCV(cvp)$svmPredictOrderedVector, y)
#'


svmCV = function(cvObj){
  svmObj = purrr::map2(.x = cvObj$trainX,
                       .y = cvObj$trainY,
  ~ e1071::svm(x = .x, y = .y))

  svmPredict = purrr::map2(.x = svmObj,
                           .y = cvObj$testX,
                           ~ predict(object = .x,
                                     newdata = .y,
                                     decision.values = TRUE))

  svmPredictAsVector = lapply(svmPredict, as.vector)
  svmFoldError = purrr::map2_dbl(.x = svmPredictAsVector,
                                 .y = cvObj$testY,
                                 ~ mean( .x != .y))
  svmMeanError = mean(svmFoldError)

  svmPredictBindVector = unlist(svmPredict)
  svmPredictOrderedVector = svmPredictBindVector[cvObj$originalIndex]

  result = list(svmFoldError = svmFoldError,
                svmMeanError = svmMeanError,
                svmPredictOrderedVector = svmPredictOrderedVector)
  return(result)
}
