#' A better pairs plot function
#'
#' @param data a data frame
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5],
#' lower.panel = panel_cor,
#' upper.panel = panel_scatter_abline)

panel_scatter_abline <- function(x, y, a = 0, b = 1, col = "red")
{
  points(x, y)
  abline(a = a, b = b, col = col)
}


panel_scatter_vline <- function(x, y, v = 0, col = "red")
{
  points(x, y)
  abline(v = v, col = col)
}


panel_scatter_hline <- function(x, y, h = 0, col = "red")
{
  points(x, y)
  abline(h = h, col = col)
}
