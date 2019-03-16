#' A better pairs plot function
#'
#' @param x a vector
#' @param y a vector
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5], lower.panel = panel_cor)
panel_cor <- function(x, y)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # r <- cor(x[subset], y[subset], method ="pearson")
  r <- cor(x, y, method ="pearson")
  txt <- format(c(r, 0.123456789), digits = 3)[1]
  # if(all(subset)){
    text(0.5, 0.25, paste("",txt), cex = 2)
  # } else {
  #   text(0.5, 0.25, paste("subset Corr=",txt), cex = 2)
  # }

}

#' Identity Distance
#' @param x a vector
#' @param y a vector
#' @export
#' @examples
#' set.seed(1)
#' x = rnorm(100)
#' identityDist(x = x, y = x)
#' identityDist(x = x, y = 2*x)
identityDist = function(x, y){
  res = median(abs(x - y)/sqrt(2))
  return(res)
}

#' Identity Distance for a matrix
#' @param matrix a matrix
#' @export
#' @examples
#' set.seed(1)
#' x = matrix(rnorm(100), ncol = 10)
#' calcIdenDist(x)
calcIdenDist = function(matrix){
  proxy::dist(matrix, method = identityDist)
}

#' @title pairs panel with identity distance
#' @param data a data frame
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5],
#' upper.panel = panel_scatter_abline,
#' lower.panel = panel_idenDist)
panel_idenDist <- function(x, y)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  cor <- cor(x, y, method ="pearson")
  corTxt <- format(c(cor, 0.123456789), digits = 3)[1]
  idenDist <- identityDist(x, y)
  idenDistTxt <- format(c(idenDist, 0.123456789), digits = 3)[1]


  text(0.5, 0.25, paste("", corTxt, "\n (", idenDistTxt, ")"), cex = 2)
}
