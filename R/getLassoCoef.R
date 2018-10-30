#' Compare the true classification with a matrix of predictions
#'
#' @param lassoObj Lasso object, either cv.glmnet or glmnet outputs
#' @param s 
#' @import glmnet
#' @author Kevin Wang
#' @export
#' @examples
#' x = iris[51:150, -5] %>% as.matrix
#' y = as.matrix(as.integer(iris[51:150, 5])-2)
#' library(glment)
#' lassoObj = glmnet(x = x, y = y, family = "binomial")
#' getLassoCoef(lassoObj, s = 0.001)
#' cvLassoObj = cv.glmnet(x = x, y = y, family = "binomial")
#' getLassoCoef(cvLassoObj, s = "lambda.min")

getLassoCoef = function(lassoObj, s){
  coefMatrix = as.matrix(coef(lassoObj, s = s))  
  return(coefMatrix[coefMatrix[, 1] != 0,, drop = FALSE])
}
