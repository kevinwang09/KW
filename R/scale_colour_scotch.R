#' @title Scotch colours.
#' @param n The number of colours
#' @return ggproto object
#' @export
#' @examples
#' df = data.frame(
#' x = rnorm(100),
#' y = rnorm(100),
#' colour = factor(1:10))
#' p = ggplot(df, aes(x = x, y = y, colour = colour)) +
#' geom_point()
#' p + scale_colour_scotch()
#'
scale_colour_scotch = function (stata = FALSE, ...)
{
  discrete_scale(aesthetics = "colour",
                 scale_name = "scotch",
                 palette = SmokyScotch::scotchColour(stata = stata),
                 ...)
}

