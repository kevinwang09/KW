#' Performing CV using random forest, allowing multiple cores
#' @param x design matrix
#' @param y factor
#' @param method Method of the classification algorithm
#' @param nfolds number of CV folds
#' @param nexp number of loops
#' @author Kevin Wang
#' @importFrom furrr future_map
#' @importFrom future multisession
#' @importFrom dplyr bind_rows
#' @importFrom purrr map2 map2_dfr
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

  if(method == "rf"){
    list_prediction = furrr::future_map(.x = seq_len(nexp),
                                        .f = ~ rf_cv(list_data_partitions[[.x]]),
                                        .progress = TRUE)
  } else if(method == "svm"){
    list_prediction = furrr::future_map(.x = seq_len(nexp),
                                        .f = ~ rf_cv(list_data_partitions[[.x]]),
                                        .progress = TRUE)
  }

  d = paste0("exp_%0", ceiling(log10(nexp)) + 1L, "d")
  exp_num = sprintf(d, seq_len(nexp))
  names(list_prediction) = exp_num

  fold_error_tbl = purrr::map(list_prediction, "fold_error") %>%
    purrr::map(v2df, variable_name = "fold_num", value_name = "error") %>%
    bind_rows(.id = "exp_num")

  mean_error_tbl = purrr::map_dbl(list_prediction, "mean_error") %>%
    v2df(variable_name = "exp_num", value_name = "error")

  predict_class_tbl = purrr::map(list_prediction, "predict_class") %>%
    purrr::map_dfr(v2df, variable_name = "samples", value_name = "error",
                   .id = "exp_num")

  predict_prob_tbl = tibble(
    exp_num = exp_num,
    predict_prob = purrr::map(list_prediction, "predict_prob") %>% purrr::map(as.data.frame))

  result = tibble::lst(fold_error = fold_error_tbl,
                       mean_error = mean_error_tbl,
                       predict_class = predict_class_tbl,
                       predict_prob = predict_prob_tbl)

  class(result) = c(class(result), "cv_pred_result")
  return(result)
}
