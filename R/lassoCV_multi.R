#' Performing CV using lasso, allowing multiple cores
#' @param cvObj outputs of the cvPartition function
#' @param ntree number of trees
#' @author Kevin Wang
#' @import parallel
#' @import glmnet
#' @export
#' @examples
#' x = iris[51:150, -5] %>% as.matrix
#' y = as.matrix(as.integer(iris[51:150, 5])-2)
#' library(glmnet)
#' lassoCvObj = lassoCV_multi(x = x, y = y, family = "binomial", s = "lambda.min", nExp = 10)
#' purrr::map_dbl(lassoCvObj, "cvLassoMeanError") %>% mean


lassoCV_multi = function(x, y, family, s = "lambda.min",
                       nFolds = 5,
                       nExp,
                       cores = 1){

  listDataPartitions = replicate(nExp,
                                 {cvPartition(x = x, y = y, nFolds = nFolds)},
                                 simplify = FALSE)

  names(listDataPartitions) = paste0("exp", seq_len(nExp))


  if (cores == 1){
    listPrediction = lapply(listDataPartitions, lassoCV, family = family, s = s)
  } else {
    listPrediction = parallel::mclapply(listDataPartitions, lassoCV, family = family, s = s, mc.cores = cores)
  }
  return(listPrediction)
}
