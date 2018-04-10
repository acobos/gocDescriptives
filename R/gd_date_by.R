#' Describes a dataframe date variable, by groups
#'
#' Describes a dataframe date variable, by groups defined by a
#' categorical variable in the same dataframe.
#'
#' @param df The \code{dataframe} containing varibles.
#' @param var The \code{date} variable to be described.
#' @param by The categorical variable for grouping. Should be either
#' @param ... Additional arguments to \code{gd_date()}
#' \code{character} or \code{factor}.
#'
#'
#' @return A \code{dataframe} with columns: \code{Variable}, \code{Key}, plus
#' one additional column for each group in the \code{by} variable.
#'
#' @examples
#' # Create example data
#' inclus_dt <- as.Date(c("2017-01-01", "2017-03-25", "2018-01-31",
#'                        "2018-01-01", "2018-02-20", "2017-12-07"))
#' treatment <- rep(LETTERS[1:2],3)
#' inclusion <- data.frame(inclus_dt, treatment)
#'
#' gd_date_by(inclusion, "inclus_dt", "treatment")
#'
#' gd_date_by(inclusion, "inclus_dt", "treatment", "%e-%b-%y")
#'
#'
#' @export
gd_date_by <- function (df, var, by, ...) {

  # input validation
  if (!exists(deparse(substitute(df)))) stop(paste(deparse(substitute(df)),"not found"))
  if (!(var %in% names(df))) stop(paste(var, "not found in dataframe"))
  if (!(by %in% names(df))) stop(paste(by, "not found in dataframe"))
  if (!(is.factor(df[[by]]) | is.character(df[[by]]))) stop ("by variable is neither character nor factor")

  #' @import dplyr

  tapply(df[[var]], df[[by]], gd_date, ...) %>%
    .gd_combine_df_list %>%
    mutate(Variable = var)
}
