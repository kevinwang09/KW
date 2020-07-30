#' Performing CV using random forest, allowing multiple cores
#' @param x design matrix
#' @param y factor
#' @param method Method of the classification algorithm
#' @param nfolds number of CV folds
#' @param nexp number of loops
#' @author Kevin Wang
#' @importFrom furrr future_map
#' @importFrom future multisession
#' @export
#' @rdname cv_multi
#' @examples
#' x = iris[51:150, -5]
#' y = factor(iris[51:150, 5])
#' res = cv_multi(x = x, y = y, method = "rf")
#' \dontrun{
#' library(furrr)
#' plan(multisession(workers = 5))
#' res = cv_multi(x = x, y = y, method = "rf", nexp = 20)
#' plan(future::sequential)
#' }

cv_multi = function(x, y,
                    method = "svm",
                    nfolds = 5,
                    nexp = 5){

  list_data_partitions = replicate(nexp,
                                   {cv_partition(x = x, y = y, nfolds = nfolds)},
                                   simplify = FALSE)
  d = paste0("exp_%0", ceiling(log10(nexp)) + 1L, "d")

  names(list_data_partitions) = sprintf(d, seq_len(nexp))

  if(method == "rf"){
    list_prediction = furrr::future_map(.x = seq_len(nexp),
                                        .f = ~ rf_cv(list_data_partitions[[.x]]),
                                        .progress = TRUE)
  } else if(method == "svm"){
    list_prediction = furrr::future_map(.x = seq_len(nexp),
                                        .f = ~ rf_cv(list_data_partitions[[.x]]),
                                        .progress = TRUE)
  }
  return(list_prediction)
}
