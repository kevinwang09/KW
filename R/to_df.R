#' Convert a named vector into a data.frame
#' @param vec A named vector
#' @author Kevin Wang
#' @rdname to_df
#' @param variable_name variable_name
#' @param value_name value_name
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' x = 1:5
#' names(x) = letters[1:5]
#' v2df(x)
v2df = function(vec,
                variable_name = "variable",
                value_name = "value"){
  assertthat::assert_that(is.vector(vec) | is.factor(vec))

  if(is.null(names(vec))){
    # message("vector does not have names, using index instead")
    result = tibble(variable = seq_along(vec), value = vec)
  } else {
    result = tibble(variable = names(vec), value = vec)
  }

  colnames(result) = c(variable_name, value_name)
  return(result)
}


#' Turn a one-row table into a data frame
#' @param table a frequency table (i.e. only one row)
#' @rdname to_df
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @author Kevin Wang
#' @export
#' @examples
#' x = table(sample(letters[1:5], 100, replace = TRUE))
#' t2df(x)
t2df = function(table, variable_name = "variable", value_name = "value"){
  res = tibble::tibble(
    names = names(table),
    table = as.numeric(table))

  names(res) = c(variable_name, value_name)
  return(res)
}
