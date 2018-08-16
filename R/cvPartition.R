#' Creating CV data partition
#' @param x x should be a matrix without rownames
#' @param y y should be true labels
#' @importFrom caret createFolds
#' @author Kevin Wang
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = iris[51:150, 5] %>% as.factor
#' cvp = cvPartition(x = x, y = y, nFolds = 5)
#' str(cvp)
cvPartition = function(x, y, nFolds){

  n = length(y) ## The number of observations

  obsNum = paste0("obs", seq_len(n))
  rownames(x) = obsNum
  names(y) = obsNum

  testIndex = caret::createFolds(y, k = nFolds) ## Creating test index
  trainIndex = lapply(testIndex, function(i){(1:n)[-i]}) ## The train index is mutually exclusive to the test index
  originalIndex = order(unlist(testIndex))
  ## Index version
  # res = list(testIndex = testIndex,
  #            trainIndex = trainIndex,
  #            foldNum = names(testIndex), ## The foldNumber
  #            originalIndex = originalIndex
  #   )

  testX = lapply(testIndex, function(k) {x[k, ,drop = FALSE]})
  testY = lapply(testIndex, function(k) {y[k]})
  trainX = lapply(trainIndex, function(k) x[k, ,drop = FALSE])
  trainY = lapply(trainIndex, function(k) {y[k]})

  result = list(foldNum = names(testIndex),
                testX = testX,
                testY = testY,
                trainX = trainX,
                trainY = trainY,
                originalIndex = originalIndex)
  return(result)
}
