#' Describes a categorical variable
#'
#' Describes a categorical variable; creates its frequency table, with both
#'   absolute (n) and relative(\%) frequencies.
#'
#' @param x The categorical variable to be described (\code{character} or
#'   \code{factor}) .
#' @param useNA Optional. Controls the handling of missing values. If provided,
#' shoud be one of the following \code{character} values: "no", "ifany",
#' "always". See Details.
#'
#' @return A \code{dataframe} with three columns: \code{Variable}, \code{Key},
#' \code{Value}.
#'
#' @details When \code{useNA = "ifany"} (default), missing values are included
#' and percentages are computed on the total number of cases. When
#' \code{useNA = "no"}, missing values are excluded and percentages are computed
#' on the number of non-missing cases. \code{useNA = "always"} forces to inform
#' on missings, even if there are no missings.
#'
#' @examples
#' # Example data (first lines)
#' head(iris)
#'
#' # Just to have some missings in variable Species
#' iris[sample(1:length(iris)), "Species"] <- NA
#'
#' # Frequency table, including missings (total %)
#' gd_categ(iris$Species)
#'
#' # Frequency table, excluding missings (valid %)
#' gd_categ(iris$Species, useNA = "no")
#'
#' # Avoid variable name prefixed with dataframe name
#' with(iris, gd_categ(Species))
#' with(iris, gd_categ(Species, useNA = "no"))
#'
#' # Force information of missings
#' gd_categ(iris$Species, useNA = "always" )
#'
#' @export
gd_categ <- function (x, useNA = "ifany") {

  # input validation
  if (!is.vector(x) & !is.factor(x)) stop("x is neither a vector nor a factor")
  if (length(x)==0) stop("x has zero length")

  # Counts
  res <- as.data.frame(table(x, useNA = useNA))

  total <- sum(res$Freq)

  valid_n <- sum(res$Freq[!(is.na(res$x) | res$x == "No disponible")])


  res$Perc <- formatC(sprintf("(%.1f %%)", 100 * res$Freq / sum(res$Freq)),
                      width=9)

  # si hay missings y
  # if (sum(is.na(x)) > 0 & useNA !="no") {
  if (valid_n < total) {
    res$Perc <- ifelse(!is.na(res$x) & (res$x != "No disponible") ,
                       paste(res$Perc,
                             formatC(sprintf("[%.1f %%]", 100 * res$Freq / valid_n),
                                     width=9)),
                       paste(res$Perc,
                             "         "))
  }

  res$Value <- paste0("n = ", res$Freq, res$Perc)

  res$Freq <- NULL
  res$Perc <- NULL
  res$Variable <- deparse(substitute(x))
  res$Key <- as.character(res$x)
  res[is.na(res$Key),"Key"] <- "Missing"
  res %>% select(Variable, Key, Value)
}

