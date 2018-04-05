#' Describes a date variable
#'
#' Describes a date variable by computing its range.
#'
#' @param x The date variable to be described (\code{c("Date", "POSIXlt",
#' "POSIXct", "POSIXt")}).
#' @param date_format Optional. If specified, should ba a valid date format. See
#' the Details section in \code{\link{strptime}} for more details.
#'
#' @return A \code{dataframe} with three columns: \code{Variable}, \code{Key},
#' \code{Value}.
#'
#' @examples
#' # Create example data
#' inclusion_date <- as.Date(c("2017-01-01", "2017-03-25", "2018-01-31"))
#'
#' # Default date_format
#' gd_date(inclusion_date)
#'
#' # Month as number, and day with leading space for single digit numbers
#' gd_date(inclusion_date, date_format="%e-%m-%y")
#'
#' # Four-digit year
#' gd_date(inclusion_date, date_format="%e-%m-%Y")
#'
#'
#' @export
gd_date <- function(x, date_format="%d-%b-%y") {

  # input validation
  if (!(class(x)[1] %in% c("Date", "POSIXlt", "POSIXct", "POSIXt"))) stop("x is not a Date/POSIXt object")
  if (sum(!is.na(x))==0) stop("x has no data, all elements are NA")

  # To get stats as character ----
  # median and range
  mr <- function (x, decimals=dec) {
   #  md <- format(median(as.Date(x), na.rm=TRUE), date_format)
    min <- format(min(as.Date(x), na.rm=TRUE), date_format)
    max <- format(max(as.Date(x), na.rm=TRUE), date_format)
    paste0(min, ", ", max)
  }

  # return dataframe
  data.frame(Variable = deparse(substitute(x)),
             Key = "min, max",
             Value = mr(x),
             stringsAsFactors = FALSE)
}



