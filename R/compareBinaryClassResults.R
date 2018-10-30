#' Compare the true classification with a matrix of predictions
#'
#' @param y True labels of the observations. Ordering of y must match that of the predictMatrix.
#' @param classifierMatrix Classifiers are rows for each column being observations
#' @import forcats
#' @import ggplot2
#' @import magrittr
#' @author Kevin Wang
#' @export
#' @examples
#' x = iris[51:150, -5] + rnorm(100*4)
#' y = as.factor(iris[51:150, 5])
#' tmp1 = svmCV_multi(x = x, y = y,
#'                    nFolds = 10, nExp = 100, cores = 1)
#'
#' tmp2 = svmCV_multi(x = x, y = y,
#'                    nFolds = 5, nExp = 100, cores = 1)
#'
#' predictMatrix1 = purrr::map(tmp1, "svmPredictOrderedVector") %>%
#'   purrr::map(as.character) %>%
#'   do.call(rbind,.)
#'
#' predictMatrix2 = purrr::map(tmp2, "svmPredictOrderedVector") %>%
#'   purrr::map(as.character) %>%
#'   do.call(rbind,.)
#'
#'
#' classifierMatrix = rbind(
#'   binaryClassScores(y = y, predictMatrix = predictMatrix1),
#'   binaryClassScores(y = y, predictMatrix = predictMatrix2)
#' )
#' rownames(classifierMatrix) = c("TenFold", "FiveFold")
#'
#' compareBinaryClassResults(y, classifierMatrix)













compareBinaryClassResults = function(y, classifierMatrix){

  stopifnot(identical(length(y),
                      ncol(classifierMatrix)))

  y = as.character(y)
  n = length(y)
  obsNum = paste0("obs", seq_len(n))
  names(y) = obsNum

  originalMethodNames = rownames(classifierMatrix)

  stopifnot(length(unique(y)) == 2)
  ##############################################

  y1Label = unique(y)[1]
  y2Label = unique(y)[2]
  ySigned = 2L*as.integer(factor(y)) - 3L
  names(ySigned) = names(y)

  aveError = colMeans(classifierMatrix)
  y1AveError = aveError[y == y1Label]
  y2AveError = aveError[y == y2Label]

  y1Ordered = y1AveError[order(y1AveError)]
  y2Ordered = y2AveError[order(y2AveError, decreasing = T)]

  columnOrder = c(names(y1Ordered), names(y2Ordered))
  signedClassifierMatrix = apply(classifierMatrix, 1, function(row){row * ySigned})

  plotdf = data.frame(trueLabel = ySigned[columnOrder],
                      signedClassifierMatrix[columnOrder,]) %>%
    tibble::rownames_to_column(var = "sampleName") %>%
    dplyr::mutate(sampleName = forcats::as_factor(sampleName)) %>%
    tidyr::gather(key = methodName,
                  value = signedScores,
                  -sampleName) %>%
    dplyr::mutate(methodName =
                    forcats::fct_relevel(methodName, c(originalMethodNames, "trueLabel")))

  plotdf %>% head

  individualClassPlot = plotdf %>%
    ggplot(aes(x = sampleName,
               y = methodName,
               fill = signedScores)) +
    geom_tile() +
    scale_fill_distiller(palette = "RdBu") +
    theme_bw(18) +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "bottom")


  return(individualClassPlot)
}
