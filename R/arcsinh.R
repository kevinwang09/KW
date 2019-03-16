#' @title Arcsinh
#' @param x a vector
#' @author Kevin Wang
#' @export
#' @examples 
#' set.seed(11)
#' x = rnorm(100)
#' plot(x, arcsinh(x))
arcsinh = function(x){
  log(x + sqrt(x^2 + 1L))
}