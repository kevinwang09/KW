#' Clean the digits in a data frame
#' @param df A data frame with numbers and categorical variables
#' @param digits Number of significant figures
#' @param method Either "round" or "signif"
#' @importFrom dplyr mutate_if funs
#' @author Kevin Wang
#' @export
#' @return A data frame with less digits
clean_digits <- function(df, digits = 2, method = c("round", "signif")){
  if(method == "round"){
    res = dplyr::mutate_if(.tbl = df,
                           is.numeric,
                           dplyr::funs(round), digits = digits)
  } else{
    if(method == "round"){
      res = dplyr::mutate_if(.tbl = df,
                             is.numeric,
                             dplyr::funs(signif), digits = digits)
    }
  }
  return(res)
}

#' Display the first k rows and first k columns of the data
#' @param x data.frame
#' @author Kevin Wang
#' @param k number of rows and columns to view
#' @export
#' @examples
#' top(mtcars)
top = function(x, k = 6){
  stopifnot(is.data.frame(x) | is.matrix(x))
  c = min(k, ncol(x))
  r = min(k, nrow(x))
  x[1:r, 1:c, drop = FALSE]
}
