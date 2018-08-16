#' Turn a table into a data frame
#' @param x a frequency table (i.e. only one row)
#' @param variable.name a character
#' @param value.name a character
#' @author Kevin Wang
#' @export
table2DataFrame = function(x, variable.name = "variable.name", value.name = "value.name"){
  res = data.frame(
    names(x),
    as.numeric(x)
  )

  names(res) = c(variable.name, value.name)
  return(res)
}
