#' Performing CV using glm(family = "binomial")
#' @param cvObj outputs of the cvPartition function
#' @import purrr
#' @export
#' @examples
#' x = iris[51:150, -5] + rnorm(100, 0, 2)
#' y = ifelse(iris[51:150, 5] == "versicolor", 0, 1)
#' cvObj = cvPartition(x = x, y = y, nFolds = 5)
#' table(pred = logitCV(cvObj)$logitPredictIntOrderedVector, y)
#'

logitCV = function(cvObj, cutoff = 0.5){
  logitTrainDataFrame = purrr::map2(.x = cvObj$trainX,
                                   .y = cvObj$trainY,
                                   ~ cbind(.x,  y = .y))

  logitObj = lapply(logitTrainDataFrame,
                    function(df){
                      glm(y ~ ., family = "binomial", data = df)
                    })

  logitPredict = purrr::map2(.x = logitObj,
                             .y = cvObj$testX,
                           ~ predict(object = .x,
                                     newdata = .y,
                                     type = "response"))

  logitPredictAsVector = lapply(logitPredict, as.vector)

  logitFoldError = purrr::map2_dbl(.x = logitPredictAsVector,
                                   .y = cvObj$testY,
                                   ~ mean( ifelse(.x > cutoff, 1L, 0L) != .y))

  logitMeanError = mean(logitFoldError)

  logitPredictBindVector = unlist(logitPredict)

  logitUnorderedY = cvObj$testY
  names(logitUnorderedY) = NULL
  logitUnorderedY = unlist(logitUnorderedY)
  logitOrderedY = logitUnorderedY[cvObj$originalIndex]

  logitPredictProbOrderedVector = logitPredictBindVector[cvObj$originalIndex]
  logitPredictIntOrderedVector = ifelse(logitPredictProbOrderedVector > cutoff, 1L, 0L)


  result = list(logitFoldError = logitFoldError,
                logitMeanError = logitMeanError,
                logitOrderedY = logitOrderedY,
                # logitPredictAsVector = logitPredictAsVector,
                logitPredictProbOrderedVector = logitPredictProbOrderedVector,
                logitPredictIntOrderedVector = logitPredictIntOrderedVector)
  return(result)
}
