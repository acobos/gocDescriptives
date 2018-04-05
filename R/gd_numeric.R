#' Describes a quantitative variable
#'
#' Describes a quantitative variable by computing common statistics: mean (SD),
#' median (IRQ), range, n (\%).
#'
#' @param x The quantitative variable to be described (\code{numeric}).
#' @param decimals Optional. Integer controling the rounding (decimals) of
#' computed statistics. See Details.
#'
#' @return A \code{dataframe} with three columns: \code{Variable}, \code{Key},
#' \code{Value}.
#'
#' @details If \code{decimals} is not specified, statistics are computed
#' according to the (median) precision of (\code{x}). Suppose (\code{x}) is
#' expressed with two decimals. Then, the mean, SD, median and IQR are rounded
#' to three decimals, while min and max are expressed with two decimals. If
#' \code{decimals} is specified, mean, SD, median and IQR are rounded to
#' \code{decimals+1} decimals, while min and max are expressed with \code{decimals}
#' decimals. However, note that non-significant digits are never printed, e.g.,
#' 17.0 is printed as 17).
#'
#' @examples
#' # Example data (first lines)
#' head(iris)
#'
#' gd_numeric(iris$Sepal.Length)
#'
#' # Avoid variable name prefixed with dataframe name
#' with(iris, gd_numeric(Sepal.Length))
#'
#' # See Sepal.Length values and note their precision (1 decimal)
#' iris$Sepal.Length
#'
#' # Increase precision of statistics
#' gd_numeric(iris$Sepal.Length, decimals = 2)
#'
#' # Decrease precision of statistics
#' Note that min and max are rounded to zero!
#' gd_numeric(iris$Sepal.Length, decimals = 0)
#'
#'
#' @export
#'
gd_numeric <- function(x, decimals=NA) {

  # input validation
  if (!is.vector(x)) stop("x is not a vector")
  if (!is.numeric(x)) stop("x is not a vector")
  if (sum(!is.na(x))==0) stop("x has no data, all elements are NA")
  if (length(decimals)!=1) stop("decimals should be of length 1")
  if (!is.na(decimals) & !is.numeric(decimals)) stop("incorrect decimals")

  # value for decimals
  dec <- ifelse(is.na(decimals),
                median(gd_get_decimals(x), na.rm=TRUE),
                decimals)

  # To get stats as character ----

  m <- round(mean(x, na.rm=TRUE), dec+1)
  s <- round(sd(x, na.rm=TRUE), dec+1)
  md <- round(median(x, na.rm=TRUE), dec + 1)
  min <- round(min(x, na.rm=TRUE), dec)
  max <- round(max(x, na.rm=TRUE), dec)
  IQR <- round(IQR(x, na.rm=TRUE), dec + 1)
  n   <- length(x[!is.na(x)])
  p   <- round(100 * n / length(x),1)

  # return dataframe
  # require(stringr)
  data.frame(Variable = deparse(substitute(x)),
             Key = c("Media (DE)",
                     "Mediana (IQR)",
                     "min, max",
                     "n (%)"),
             Value =  c(stringr::str_interp("${m} (${s})"),
                        stringr::str_interp("${md} (${IQR})"),
                        stringr::str_interp("${min}, ${max}"),
                        stringr::str_interp("${n} (${p})")),
             stringsAsFactors = FALSE)
}

