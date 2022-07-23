#' Execute roxygen2::roxygenise().
#'
#' @description Call this function to send "roxygen2::roxygenise()" to
#' the console of rstudio and execute.
#' Steps:
#' 1. Install the KW package.
#' 2. Go to Tools -> Addins -> Browse Addins in a RStudio session.
#' 3. Create a shortcut for this function.
#'
#'
#' @importFrom rstudioapi sendToConsole
#' @export
execute_roxygenise <- function() {
  rstudioapi::sendToConsole("roxygen2::roxygenise()")
}
