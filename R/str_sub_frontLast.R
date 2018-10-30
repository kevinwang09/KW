#' @title Substring based on number of first and last characters
#' @param string string
#' @param frontNum number of characters at the front
#' @param lastNum number of characters at the back
#' @param sep separator in the middle
#' @author Kevin Wang
#' @return Returns a function of colours
#' @export
#' @examples
#' string = c("Apples", "Banana")
#' frontNum = 2
#' lastNum = 2
#' str_sub_frontLast(string, frontNum, lastNum)
#' string = c("Apples", "Apnaes")
#' str_sub_frontLast(string, frontNum, lastNum)

str_sub_frontLast = function(string, frontNum, lastNum, sep = "..."){
  frontNum = as.integer(frontNum)
  lastNum = as.integer(lastNum)

  frontPart = stringr::str_sub(string, start = 1L, end = frontNum)
  lastPart = stringr::str_sub(string, start = str_length(string) - lastNum + 1L, end = str_length(string))

  result = paste(frontPart, lastPart, sep = sep)

  if(!identical(length(unique(string)),
                length(unique(result)))){
    stop("Unique levels of original string does not match unique levels of result")
  }
  return(result)
}
