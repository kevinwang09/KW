#' Performing CV using svm, allowing multiple cores
#' @param cvObj outputs of the cvPartition function
#' @import parallel
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = iris[51:150, 5] %>% as.factor
#' tmp2 = svmCV_multi(x = x, y = y,
#'                    nFolds = 5, nExp = 100, cores = 1)
#'
#' purrr::map_dbl(tmp2, "svmMeanError")


svmCV_multi = function(x, y,
                       nFolds,
                       nExp,
                       cores = 1,
                       seed = NULL){

  if(is.null(seed)){
    listDataPartitions = replicate(nExp,
                                   {cvPartition(x = x, y = y, nFolds = 5)},
                                   simplify = FALSE)
  } else {
    listDataPartitions = replicate(nExp,
                                   {cvPartition(x = x, y = y, nFolds = 5, seed = seed)},
                                   simplify = FALSE)
  }

  names(listDataPartitions) = paste0("exp", seq_len(nExp))


  if (cores == 1){
    listPrediction = lapply(listDataPartitions, svmCV)
  } else {
    listPrediction = parallel::mclapply(listDataPartitions, svmCV, mc.cores = cores)
  }
  return(listPrediction)
}
