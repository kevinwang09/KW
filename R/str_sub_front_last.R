#' @title Substring based on number of first and last characters
#' @param string string
#' @param front number of characters at the front
#' @param last number of characters at the back
#' @param sep separator in the middle
#' @author Kevin Wang
#' @return Returns a function of colours
#' @importFrom stringr str_sub str_length
#' @export
#' @examples
#' string = c("Apples", "Banana")
#' front = 2
#' last = 2
#' str_sub_front_last(string, front, last)

str_sub_front_last = function(string, front, last, sep = "..."){
  front = as.integer(front)
  last = as.integer(last)

  frontPart = stringr::str_sub(string, start = 1L, end = front)
  lastPart = stringr::str_sub(string, start = stringr::str_length(string) - last + 1L, end = stringr::str_length(string))

  result = paste(frontPart, lastPart, sep = sep)

  if(!identical(length(unique(string)),
                length(unique(result)))){
    stop("Unique levels of original string does not match unique levels of result")
  }
  return(result)
}
