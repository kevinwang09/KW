#' Performing CV using random forest
#' @param cvObj outputs of the cvPartition function
#' @param ntree number of trees
#' @author Kevin Wang
#' @importFrom purrr map2 map2_dbl
#' @importFrom randomForest randomForest
#' @export


rfCV = function(cvObj, ntree){

  rfObj = purrr::map2(.x = cvObj$trainX,
                      .y = cvObj$trainY,
                      ~ randomForest::randomForest(x = .x,
                                                   y = factor(.y)
                      ))

  rfPredict = purrr::map2(.x = rfObj,
                          .y = cvObj$testX,
                          ~ predict(object = .x,
                                    newdata = .y,
                                    type = "response"))

  rfVote = purrr::map2(.x = rfObj,
                       .y = cvObj$testX,
                       ~ predict(object = .x,
                                 newdata = .y,
                                 type = "vote"))

  rfPredictAsVector = lapply(rfPredict, as.vector)

  rfFoldError = purrr::map2_dbl(.x = rfPredictAsVector,
                                .y = cvObj$testY,
                                ~ mean( .x != .y))

  rfMeanError = mean(rfFoldError)

  rfPredictBindVector = unlist(rfPredict)
  rfVoteBindMatrix = do.call(rbind, rfVote)

  rfPredictOrderedVector = rfPredictBindVector[cvObj$originalIndex]
  rfVoteOrderedMatrix = rfVoteBindMatrix[cvObj$originalIndex,]


  result = list(rfFoldError = rfFoldError,
                rfMeanError = rfMeanError,
                rfPredictOrderedVector = rfPredictOrderedVector,
                rfVoteOrderedMatrix = rfVoteOrderedMatrix)
  return(result)
}
