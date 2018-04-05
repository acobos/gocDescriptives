#' Get number of decimals
#'
#' Gets the number of decimals for each element of a nuemric vector.
#'
#' @param x (\code{numeric}) vector. 
#'
#' @return A numeric vector, which values are the number of decimals of corresponding \code{x} elements.
#' 
gd_get_decimals <- function (x) {

  # input validation
  if (!is.vector(x))
    stop("x is not a vector")
  if (!is.numeric(x))
    stop("x is not numeric")
  if (length(x)==0)
    stop("x has zero length")

  # this returns the number of decimal places in a scalar: doesn't work with a vector!
  decimals_in_a_single_number <- function(number) {
    if (is.na(number)) {
      return(NA)
    }
    if ((number %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(number)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }

  # apply to all elements in x
  sapply(as.list(x), FUN = decimals_in_a_single_number)
}

