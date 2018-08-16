#' A better pairs plot function
#'
#' @param data a data frame
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
