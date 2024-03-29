#' @author Kevin Wang
#' @title Shiny app for ggplot exploration
#' @param uploaded_plot a ggplot object
#' @import ggplot2
#' @import shiny
#' @export
#' @examples
#' if(interactive()){
#' uploaded_plot = ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width,
#'            colour = Species)) +
#'   geom_point()
#' ggplot_explorer(uploaded_plot = uploaded_plot)
#' }

ggplot_explorer <- function(uploaded_plot) {
  .GlobalEnv$uploaded_plot <- uploaded_plot
  # on.exit(rm(X, envir = .GlobalEnv))
  appDir <- system.file("shiny", package = "KW")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `KW`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
