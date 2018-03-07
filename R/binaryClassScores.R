#' Compare the true classification with a matrix of predictions
#'
#' @param y True labels of the observations. Ordering of y must match that of the predictMatrix.
#' @param predictMatrix Predicted labels of the observations.
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = as.factor(iris[51:150, 5])
#' tmp2 = svmCV_multi(x = x, y = y,
#'                    nFolds = 5, nExp = 100, cores = 1)
#'
#' predictMatrix = purrr::map(tmp2, "svmPredictOrderedVector") %>%
#'   purrr::map(as.character) %>%
#'   do.call(rbind,.)
#' binaryClassScores(y = y, predictMatrix = predictMatrix)

binaryClassScores = function(y, predictMatrix){
  stopifnot(identical(length(y),
                      ncol(predictMatrix)))

  y = as.character(y)
  n = length(y)
  obsNum = paste0("obs", seq_len(n))
  names(y) = obsNum

  stopifnot(length(unique(y)) == 2)

  binaryErrors = apply(predictMatrix, 1, function(row){row == y})
  errorForEachObs = rowMeans(binaryErrors)

  return(errorForEachObs)
}
