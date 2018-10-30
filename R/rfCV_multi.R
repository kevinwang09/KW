#' Performing CV using random forest, allowing multiple cores
#' @param x design matrix
#' @param y factor
#' @param nFolds number of CV folds
#' @param nExp number of loops
#' @param cores number of cores to perform the CV
#' @author Kevin Wang
#' @import parallel
#' @export
#' @examples
#' x = iris[51:150, -5]
#' y = iris[51:150, 5] %>% as.factor
#' tmp2 = rfCV_multi(x = x, y = y,
#'                    nFolds = 5, nExp = 100, cores = 1, ntree = 100)
#'
#' purrr::map_dbl(tmp2, "rfMeanError")
#' purrr::map(tmp2, "rfVoteOrderedMatrix")

rfCV_multi = function(x, y,
                      nFolds = 5,
                      nExp,
                      cores = 1,
                      ntree = 500){

  listDataPartitions = replicate(nExp,
                                 {cvPartition(x = x, y = y, nFolds = nFolds)},
                                 simplify = FALSE)

  names(listDataPartitions) = paste0("exp", seq_len(nExp))


  if (cores == 1){
    listPrediction = lapply(listDataPartitions, rfCV, ntree = ntree)
  } else {
    listPrediction = parallel::mclapply(listDataPartitions, rfCV, mc.cores = cores, ntree = ntree)
  }
  return(listPrediction)
}
