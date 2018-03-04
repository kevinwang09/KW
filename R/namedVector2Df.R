#' Convert a named vector into a data.frame
#' @param vec A named vector


namedVector2Df = function(vec, names = names("variable", "value")){
 result = data.frame(variable = names(vec),
                     value = vec)
 colnames(result) = names
 return(result)
}
