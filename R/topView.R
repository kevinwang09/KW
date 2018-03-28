#' Display the first k rows and first k columns of the data
#' @param x data.frame
#' @param k number of rows and columns to view
#' @export

topView = function(x, k){
  x[1:k, 1:k]
}
