#' @title Arcsinh transform
#' @param x A vector, but also takes matrix and purely numeric data.frame
#' @author Kevin Wang
#' @rdname transformations
#' @export
#' @examples
#' set.seed(11)
#' x = rnorm(100)
#' plot(x, arcsinh(x))
arcsinh = function(x){
  log(x + sqrt(x^2 + 1L))
}

#' The expit function
#' @export
#' @rdname transformations
#' @examples
#' curve(expit, from = -5, to = 5)
expit = function(x){
  1/(1+exp(-x))
}

logit = function(x){
  assertthat::assert_that(any(x > 0) | any(x < 1),
                          msg = "x must be strictly between zero and 1")
  return(log(x) - log(1-x))
}
