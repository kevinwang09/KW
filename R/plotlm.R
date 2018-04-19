#' Plot scatter plot of two vectors and fit a linear model
#' @param x x vector
#' @param y y vector
#' @import ggplot2
#' @export
#' @examples
#' set.seed(10)
#' x = rnorm(100)
#' y = rnorm(100)
#' plotlm(x = x, y = y)
plotlm = function(x, y, se = FALSE){
  plotdf = data.frame(x, y)
  lmObj = lm(y ~ x)
  lmObjSumm = summary(lmObj)

  slopePvalue = lmObjSumm$coefficients["x",4]

  coefVect = signif(lmObj$coefficients, 3)
  coefExpression = paste0("y = (", coefVect[1], ") + (", coefVect[2], ")*x")

  ggplot(plotdf, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", se = se) +
    labs(title = coefExpression,
         subtitle = paste0("Correlation = ", signif(cor(x, y), 3),
                           ", R2 = ", signif(cor(x, y)^2, 3)
                           ),
         caption = paste0("Slope Pvalue = ", signif(slopePvalue, 3))
         )
}
