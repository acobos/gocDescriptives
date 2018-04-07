#' Describes a numeric variable
#'
#' Describes a numeric variable by computing common summary statistics.
#'
#' @param x \code{numeric}. Variable to be described.
#' @param decimals Optional. Controls the rounding of summary statistics. If 
#' specified, should be \code{numeric (integer)}. See Details.
#' @param language \code{character}. Language for descriptive statistic 
#' descriptors. Should be on of "sp", "en" or "ca". Defaults to "sp".

#'
#' @return A \code{dataframe} with three columns: \code{Variable}, \code{Key},
#' \code{Value}.
#'
#' @details Statistics are computed with adequate precision (rounding) in each 
#' case: mean, median, sandard deviation and IQR values are rounded to one 
#' additional decimal wrt to \code{x} values, while extremes (min, max) are 
#' rounded to the same precision as \code{x} values. This will be convenient in 
#' most cases, but may be changed by specifying \code{decimals}. If specified, 
#' the mean, median, SD and IQR values are rounded to \code{decimals+1}, while 
#' extremes are rounded to \code{decimals}. See examples.  
#' 
#' In any case, note that non-significant digits are not printed (e.g.,17.0 is 
#' printed as 17). 
#' 
#' 
#'
#' @examples
#' # Example data 
#' weight <- seq(60,95,5)
#' height <- 100 + weight
#' bmi <- weight/(height/100)^2
#'
#' gd_numeric(weight)
#' gd_numeric(height, language = "ca")
#' 
#' # Excessive precission due to lack of rounding in computed bmi
#' gd_numeric(bmi)
#' 
#' # Reduce precision
#' gd_numeric(bmi, decimals = 1)
#' 
#' @export
#'
gd_numeric <- function(x, decimals=NA, language="sp") {

  # input validation
  if (!is.vector(x)) stop("x is not a vector")
  if (!is.numeric(x)) stop("x is not a vector")
  if (sum(!is.na(x))==0) stop("x has no data, all elements are NA")
  if (length(decimals)!=1) stop("decimals should be of length 1")
  if (!is.na(decimals) & !is.numeric(decimals)) stop("incorrect decimals")
  if (!(language %in% c("sp","en","ca"))) stop("incorrect language")

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
  if (language == "sp") stat_names = c("Media (DE)",
                                      "Mediana (RIC)",
                                      "min, max",
                                      "n (%)")
  if (language == "en") stat_names = c("Mean (SD)",
                                       "Median (IQR)",
                                       "min, max",
                                       "n (%)")
  if (language == "ca") stat_names = c("Mitjana (SD)",
                                       "Mediana (IQR)",
                                       "min, max",
                                       "n (%)")
      
  # require(stringr)
  data.frame(Variable = deparse(substitute(x)),
             Key = stat_names,
             Value =  c(stringr::str_interp("${m} (${s})"),
                        stringr::str_interp("${md} (${IQR})"),
                        stringr::str_interp("${min}, ${max}"),
                        stringr::str_interp("${n} (${p})")),
             stringsAsFactors = FALSE)
}

