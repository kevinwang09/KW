#' Compare the true classification with a matrix of predictions
#'
#' @param lassoObj Lasso object, either cv.glmnet or glmnet outputs
#' @param s lambda choice as in glmnet
#' @import glmnet
#' @author Kevin Wang
#' @export
#' @examples
#' x = iris[51:150, -5] %>% as.matrix
#' y = as.matrix(as.integer(iris[51:150, 5])-2)
#' library(glmnet)
#' lassoObj = glmnet(x = x, y = y, family = "binomial")
#' getLassoCoef(lassoObj, s = 0.001)
#' cvLassoObj = cv.glmnet(x = x, y = y, family = "binomial")
#' getLassoCoef(cvLassoObj, s = "lambda.min")

getLassoCoef = function(lassoObj, s){

  if("cv.glmnet" %in% class(lassoObj)){
    coefMatrix = as.matrix(glmnet::coef.cv.glmnet(lassoObj, s = s))

  }

  if("glmnet" %in% class(lassoObj)){
    coefMatrix = as.matrix(glmnet::coef.glmnet(lassoObj, s = s))
  }

  result = coefMatrix[coefMatrix[, 1] != 0,, drop = FALSE]
  return(result)
}
