#' Add together two numbers
#'
#' @param df A data frame with numbers and categorical variables
#' @param sig Number of significant figures
#' @return A data frame with less digits
#'
#'
#'
#'
cleanDigits <- function(df, sig = 2){
  res = dplyr::mutate_if(.tbl = df,
                         is.double,
                         dplyr::funs(signif), digits = sig)
  return(res)
}
