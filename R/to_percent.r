#' Convert float to percentage (text)'
#'
#' This is a basic function
#'
#' @param x input float (e.g., .78)
#' @param digits number of digits (4 is usual
#' @export
to_percent <- function(x, digits = 4) {
  x <- x * 100
  return(paste0(format(x, digits = digits), "%"))
}

#' Convert a 3-vector to range with parenthesis, i.e., 1(2-3) '
#'
#' This is a basic function
#'
#' @param r input vector of length 3
#' @export

parenthesize <- function(r, digits = 3) {
  g <- paste0(
    round(r[1], digits), " (", round(r[2], digits), "-",
    round(r[3], digits), ")"
  )
  return(g)
}

