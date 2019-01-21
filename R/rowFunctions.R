#' row SD
#' @param x matrix
#' @author Kevin Wang
#' @export
rowSd = function(x, na.rm = TRUE){
  apply(x, 1, sd, na.rm = TRUE)
}

#' row median
#' @param x matrix
#' @author Kevin Wang
#' @export
rowMedian = function(x, na.rm = TRUE){
  apply(x, 1, median, na.rm = TRUE)
}

#' column sd
#' @param x matrix
#' @author Kevin Wang
#' @export
colSd = function(x, na.rm = TRUE){
  apply(x, 2, sd, na.rm = TRUE)
}

#' column median
#' @param x matrix
#' @author Kevin Wang
#' @export
colMedian = function(x, na.rm = TRUE){
  apply(x, 2, median, na.rm = TRUE)
}
