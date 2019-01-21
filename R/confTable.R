#' @title Confusion table with forced levels
#' @param x a logical vector
#' @param y a logical vector
#' @author Kevin Wang
#' @export
#' @examples 
#' set.seed(11)
#' x = sample(c(TRUE), 100, replace = TRUE)
#' y = sample(c(TRUE, FALSE), 100, replace = TRUE)
#' table(x, y)
#' confTable(x, y)


confTable = function(x, y){
  x = factor(x, levels = c("FALSE", "TRUE"))
  y = factor(y, levels = c("FALSE", "TRUE"))
  return(table(x, y))
}

#' @title Confusion table statistics from caret
#' @param x a logical vector
#' @param y a logical vector
#' @author Kevin Wang
#' @export
#' @examples 
#' set.seed(11)
#' x = sample(c(TRUE), 100, replace = TRUE)
#' y = sample(c(TRUE, FALSE), 100, replace = TRUE)
#' confTableStats(x, y)

confTableStats = function(x, y){
  res1 = confTable(x, y)
  return(data.frame(t(caret::confusionMatrix(res1)$byClass)))
}
