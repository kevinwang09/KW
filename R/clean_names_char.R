#' Clean a character vector in the same algorithm as janitor::clean_names
#'
#' @param x a character vector
#' @import magrittr
#' @export
#' @return A cleaned character vector
#'
#'
#'
#'
clean_names_char <- function(x){
  old_names <- x
  new_names <- old_names %>% gsub("'", "", .) %>% gsub("\"",
                                                       "", .) %>% gsub("%", "percent", .) %>% gsub("^[ ]+",
                                                                                                   "", .) %>% make.names(.) %>% gsub("[.]+", "_", .) %>%
    gsub("[_]+", "_", .) %>% tolower(.) %>% gsub("_$", "",
                                                 .)
  dupe_count <- sapply(1:length(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count >
                                                 1], dupe_count[dupe_count > 1], sep = "_")

  return(new_names)
}
