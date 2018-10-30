#' Performing CV using lasso
#' @param cvObj outputs of the cvPartition function
#' @author Kevin Wang
#' @import purrr
#' @import e1071
#' @export
#' @examples
#' x = iris[51:150, -5] %>% as.matrix
#' y = as.matrix(as.integer(iris[51:150, 5])-2)
#' library(glmnet)
#' cvp = cvPartition(x = x, y = y, nFolds = 5)
#' lassoCV(cvp, family = "binomial", s = "lambda.min")
#'


lassoCV = function(cvObj, family, s){
  
  cvLassoObj = purrr::map2(
    .x = cvObj$trainX,
    .y = cvObj$trainY,
    .f = ~ glmnet::cv.glmnet(
      x = as.matrix(.x), 
      y = as.matrix(.y), 
      family = family))
  
  cvLassoPredictProb = purrr::map2(
    .x = cvLassoObj,
    .y = cvObj$testX,
    .f = ~ predict(
      object = .x,
      newx = as.matrix(.y),
      s = s,
      type = "response")
  )
  
  
  cvLassoPredictClass = purrr::map2(
    .x = cvLassoObj,
    .y = cvObj$testX,
    .f = ~ predict(
      object = .x,
      newx = as.matrix(.y),
      s = s,
      type = "class")
  )
  
  cvLassoCoefList = lapply(cvLassoObj, getLassoCoef, s = s)
  
  cvLassoCoefUnion = purrr::map(
    .x = cvLassoCoefList,
    .f = ~ rownames(.x)) %>% 
    Reduce(f = union, x = .)
  
  cvLassoCoefUnion = cvLassoCoefUnion[cvLassoCoefUnion != "(Intercept)"]
  
  
  
  cvLassoPredictProbAsVector = lapply(cvLassoPredictProb, as.vector)
  cvLassoPredictClassAsVector = lapply(cvLassoPredictClass, as.vector)
  
  cvLassoFoldError = purrr::map2_dbl(
    .x = cvLassoPredictClassAsVector,
    .y = lapply(cvObj$testY, as.character),
    ~ mean( .x != .y))
  
  cvLassoMeanError = mean(cvLassoFoldError)
  
  cvLassoPredictProbBindVector = unlist(cvLassoPredictProbAsVector)
  cvLassoPredictProbOrderedVector = cvLassoPredictProbBindVector[cvObj$originalIndex]
  
  cvLassoPredictClassBindVector = unlist(cvLassoPredictClassAsVector)
  cvLassoPredictClassOrderedVector = cvLassoPredictClassBindVector[cvObj$originalIndex]
  
  
  result = list(
    cvLassoCoefList = cvLassoCoefList,
    cvLassoCoefUnion = cvLassoCoefUnion,
    cvLassoFoldError = cvLassoFoldError, 
    cvLassoMeanError = cvLassoMeanError,
    cvLassoPredictProbBindVector = cvLassoPredictProbBindVector, 
    cvLassoPredictClassOrderedVector = cvLassoPredictClassOrderedVector
  )
  
  
  
  return(result)
}
