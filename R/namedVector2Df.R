#' Convert a named vector into a data.frame
#' @param vec A named vector
#' @export


namedVector2Df = function(vec,
                          variable.name = "variable",
                          value.name = "value"){
 result = base::data.frame(variable = names(vec),
                           value = vec)
 colnames(result) = c(variable.name, value.name)
 return(result)
}
