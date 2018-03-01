#' A better pairs plot function
#'
#' @param data a data frame
#' @param subset A subsetting index
#' @examples
#' pairs_cor(iris[,-5])

panel.cor <- function(x, y)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # r <- cor(x[subset], y[subset], method ="pearson")
  r <- cor(x, y, method ="pearson")
  txt <- format(c(r, 0.123456789), digits = 3)[1]
  # if(all(subset)){
    text(0.5, 0.25, paste("Corr=",txt), cex = 2)
  # } else {
  #   text(0.5, 0.25, paste("subset Corr=",txt), cex = 2)
  # }

}

panel.scatter <- function(x, y)
{
  # usr <- par("usr"); on.exit(par(usr))
  # par(usr = c(0, 1, 0, 1))
  # points(x, y, col = ifelse(subset, "black", "red"))
  points(x, y)
}


pairs_cor = function(data){
  pairs(x = data,
        lower.panel = panel.cor,
        upper.panel = panel.scatter)
}
