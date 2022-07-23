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
#' @param x A number between -Inf and Inf
#' @export
#' @rdname transformations
#' @examples
#' curve(expit, from = -5, to = 5)
expit = function(x){
  1/(1+exp(-x))
}

#' The logit function
#' @export
#' @param x A number between 0 and 1
#' @rdname transformations
#' @examples
#' curve(logit, from = 0.01, to = 0.99)
logit = function(x){
  assertthat::assert_that(any(x > 0) | any(x < 1),
                          msg = "x must be strictly between zero and 1")
  return(log(x) - log(1-x))
}
