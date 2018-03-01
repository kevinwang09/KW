#' Performing CV using svm
#' @param cvObj outputs of the cvPartition function
#' @examples
# x = iris[50:150, -5]
# y = iris[50:150, 5] %>% as.factor
# tmp = cvPartition(x = x, y = y, nFolds = 5)
# str(tmp)
# table(pred = svmCV(tmp)$svmPredictOrderedVector, y)


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
