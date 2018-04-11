#' Clean a character vector in the same algorithm as janitor::clean_names
#'
#' @param x a character vector
#' @import magrittr
#' @import snakecase
#' @export
#' @return A cleaned character vector
#'
#'
#'
#'
clean_names_char <- function(x, case = c("snake", "lower_camel", "upper_camel",
                                         "screaming_snake", "lower_upper", "upper_lower", "all_caps",
                                         "small_camel", "big_camel", "old_janitor", "parsed", "mixed")){
  old_names <- x
  new_names <- old_names %>% gsub("'", "", .) %>% gsub("\"",
                                                       "", .) %>% gsub("%", ".percent_", .) %>% gsub("#", ".number_",
                                                                                                     .) %>% gsub("^[[:space:][:punct:]]+", "", .) %>% make.names(.) %>%
    snakecase::to_any_case(case = case, sep_in = "\\.", transliterations = c("Latin-ASCII"),
                           parsing_option = 4)
  dupe_count <- vapply(seq_along(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  }, integer(1))
  new_names[dupe_count > 1] <- paste(new_names[dupe_count >
                                                 1], dupe_count[dupe_count > 1], sep = "_")
  return(new_names)
}
