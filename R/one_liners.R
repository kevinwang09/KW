#' Clean the digits in a data frame
#' @param df A data frame with numeric columns
#' @param digits Number of significant figures to round the numeric columns to
#' @importFrom dplyr mutate across
#' @author Kevin Wang
#' @export
#' @return A data frame with all numeric columns rounded to the specified
#' number of significant figures
#' @examples
#' head(iris)
#' head(clean_digits(iris, digits = 1))
clean_digits <- function(df, digits = 2){
  res = dplyr::mutate(
    .data = df,
    dplyr::across(.cols = where(base::is.numeric),
                  .fns = ~ signif(x = .x, digits = digits)))
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

#' Sequence of variable names, all with the same length
#' @param prefix Prefix of the variable names
#' @param p Number of variables
#' @export
#' @examples
#' paste0("X", 1:10)
#' padded_names("X", 10)
padded_names = function(prefix, p){
  n_digits = base::floor(base::log10(p)) + 1L
  col_names = base::sprintf(base::paste0(prefix, "%0", n_digits, "d"), base::seq_len(p))
  return(col_names)
}
