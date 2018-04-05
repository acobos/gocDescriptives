#' Describes all variables in a dataframe
#'
#' Describes all variables in a dataframe.
#'
#' @param df The \code{dataframe} to be described.
#' @param percent Optional. If specified, should be one of the following
#' \code{chacarter} values: \code{"total"} or \code{"valid"}. See details.
#' @param decimals Optional. Integer controling the rounding (decimals) of
#' statistics computed for quantitative variables, as in \code{gd_numeric}.
#' See \code{?gd_numeric} for details.
#'
#' @details When \code{percent = "total"} (default), missing values are included
#' and percentages are computed on total number of cases. When
#' \code{percent = "valid"}, missing values are excluded and percentages are computed
#' on the number of non-missing cases.
#'
#' @return A \code{dataframe} with three columns: \code{Variable}, \code{Key},
#' \code{Value}.
#'
#' @examples
#' # Example data (first lines)
#' head(iris)
#'
#' # Just to have some missings
#' iris[sample(1:length(iris)), "Species"] <- NA
#' iris[sample(1:length(iris)), "Sepal.Length"] <- NA
#'
#' gd_df(iris)
#' gd_df(iris, percent = "total")
#'
#' # Frequency table, excluding missings (valid %)
#' gd_df(iris, percent = "valid")
#' @export
gd_df <- function (df, useNA = "ifany", decimals = NA, date_format = "%d-%b-%y") {

  # input validation
  if (!is.data.frame(df)) stop("df is not a dataframe !!")

  # to collect results
  results <- data.frame(Variable = character(),
                        Key = character(),
                        Value = character(), stringsAsFactors = FALSE)

  # looping vars in df
  for (varname in names(df)) {

    # if no data (all obs are NA), skip and output message
    if (sum(is.na(df[[varname]])) == length(df[[varname]])) {
      message(paste("Dropping", varname, "since all obs are missing."))
      next
    }

    # if numeric
    if (is.numeric(df[[varname]])) {
      res <- gd_numeric(df[[varname]], decimals = NA) # decimals
    }

    # if character or factor
    if (is.factor(df[[varname]]) | is.character(df[[varname]])) {
      res <- gd_categ(df[[varname]], useNA = useNA) }

    # if date
    if (class(df[[varname]][1]) %in% c("Date", "POSIXlt", "POSIXct", "POSIXt")) {
      res <- gd_date(df[[varname]], date_format = date_format)
    }

    # sets the Variable in results and accumulates results
    res$Variable <- varname
    # results <- rbind(results, res)
    results <- bind_rows(results, res)

  }

  # returns
  results
}
