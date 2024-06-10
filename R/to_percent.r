#' Convert float to percentage (text)'
#'
#'This is a basic function
#'
#' @param x input float (e.g., .78)
#' @param digits number of digits (4 is usual
#' @export
to_percent <- function(x, digits = 4) {
  x <- x * 100
  return(paste0(format(x, digits = digits), "%"))
}
