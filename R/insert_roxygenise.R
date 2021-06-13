#' Insert roxygen2::roxygenise().
#'
#' Call this function as an addin to insert \code{ roxygen2::roxygenise() } at the cursor position.
#' @importFrom rstudioapi insertText
#' @export
insert_roxygenise <- function() {
  rstudioapi::insertText("roxygen2::roxygenise()")
}
