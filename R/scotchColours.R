#' @param n The number of colours needed from Set 1 from library(RColorBrewer)
#' @title Better colour palette than Set 1 from library(RColorBrewer)
#' @return Returns a function of colours
#' @examples
#' scales::show_col(scotchColour()(10))
scotchColour = function(stata){
  set1Colours = RColorBrewer::brewer.pal(9, "Set1")
  set1Colours[6] = "#FFC200"
  set1Colours[9] = "#666666"

  # if (n <= 9){
  #   nColours = set1Colours[1:n]
  # } else {
  #   nColours = grDevices::colorRampPalette(set1Colours)(n)
  # }

  nColours = grDevices::colorRampPalette(set1Colours)
  return(nColours)
}
