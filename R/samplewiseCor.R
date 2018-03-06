#' Calculate samplwise correlation
#' @param x a matrix
#' @param y a matrix, potentially has more columns than x
#' @param method See methods in cor
#' @export

samplewiseCor = function(x, y, method = "pearson"){
  reduceX = as.data.frame(x)
  reduceY = y[,colnames(x)] %>% as.data.frame()
  res = purrr::map2_dbl(reduceX,reduceY, ~cor(.x, .y, method = method))
  return(res)
}
