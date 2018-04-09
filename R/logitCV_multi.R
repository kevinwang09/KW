#' Performing CV using glm(family = "binomial"), allowing multiple cores
#' @param cvObj outputs of the cvPartition function
#' @import parallel
#' @export
#' @examples
#' x = iris[51:150, -5] + rnorm(100, 0, 2)
#' y = ifelse(iris[51:150, 5] == "versicolor", 0, 1)
#' tmp2 = logitCV_multi(x = x, y = y, nFolds = 5, nExp = 10, cores = 1)
#'
#' purrr::map_dbl(tmp2, "logitMeanError")


logitCV_multi = function(x, y,
                         nFolds,
                         nExp,
                         cores = 1, cutoff = 0.5){

  listDataPartitions = replicate(nExp,
                                 {cvPartition(x = x, y = y, nFolds = 5)},
                                 simplify = FALSE)
  names(listDataPartitions) = paste0("exp", seq_len(nExp))


  if (cores == 1){
    listPrediction = lapply(listDataPartitions, logitCV, cutoff = cutoff)
  } else {
    listPrediction = parallel::mclapply(listDataPartitions, logitCV, mc.cores = cores, cutoff = cutoff)
  }
  return(listPrediction)
}
