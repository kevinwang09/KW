#' @param n The number of colours needed from Set 1 from library(RColorBrewer)
#' @title Better colour palette than Set 1 from library(RColorBrewer)
#' @examples
#' df = data.frame(
#' x = rnorm(100),
#' y = rnorm(100),
#' colour = factor(1:10))
#' ggplot(df, aes(x = x, y = y, colour = colour)) +
#' geom_point() +
#' scale_colour_manual(values = scotchColour(n = df$colour %>% unique %>% length))
#'
scotchColour = function(n){
  set1Colours = RColorBrewer::brewer.pal(9, "Set1")
  set1Colours[6] = "#FFFF33"
  set1Colours[9] = "#666666"

  if (n <= 9){
    nColours = set1Colours[1:n]
  } else {
    nColours = grDevices::colorRampPalette(set1Colours)(n)
  }

  return(nColours)
}
