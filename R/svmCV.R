#' Performing CV using svm
#' @param cvObj outputs of the cvPartition function
#' @examples
#' x = iris[1:100, -5]
#' y = 2*(iris[1:100, 5] %>% as.factor %>% as.integer()) - 3L
#' s = sample(1:100)
#' x = x[s,]
#' y = y[s]
#' tmp = cvPartition(x = x, y = y, nFolds = 5)
#' str(tmp)
#' table(pred = svmCV(tmp)$svmPredictOrderedVector, y)


svmCV = function(cvObj){
  svmObj = purrr::map2(.x = cvObj$trainX,
                       .y = cvObj$trainY,
  ~ e1071::svm(x = .x, y = .y))

  svmPredict = purrr::map2(.x = svmObj,
                           .y = cvObj$testX,
                           ~ predict(object = .x,
                                     newdata = .y,
                                     probability = TRUE,
                                     decision.values = TRUE))
  svmPredictVectors = lapply(svmPredict, function(d) d < 0)
  svmPredictBindVector = 2L*(unlist(svmPredictVectors) + 0L) - 1L
  svmPredictOrderedVector = svmPredictBindVector[cvObj$originalIndex]
  result = list(svmPredictOrderedVector = svmPredictOrderedVector)
  return(result)
}
