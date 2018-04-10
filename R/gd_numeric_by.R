#' Describes a dataframe numeric variable, by groups
#'
#' Describes a dataframe numeric variable, by groups defined by a
#' categorical variable in the same dataframe.
#'
#' @param df The \code{dataframe} containing varibles.
#' @param var The \code{numeric} variable to be described.
#' @param by The categorical variable for grouping. Should be either
#' \code{character} or \code{factor}.
#' @param ... Additional arguments to \code{gd_numeric()}
#'
#'
#' @return A \code{dataframe} with columns: \code{Variable}, \code{Key}, plus
#' one additional column for each group in the \code{by} variable.
#'
#' @examples
#' gd_numeric_by(iris, "Sepal.Length", "Species")
#'
#' gd_numeric_by(iris, "Sepal.Length", "Species", decimals=2)
#'
#'
#' @export
gd_numeric_by <- function (df, var, by, ...) {

  # input validation
  # if (!exists(deparse(substitute(df)))) stop(paste(deparse(substitute(df)),"not found"))
  # if (!(var %in% names(df))) stop(paste(var, "not found in dataframe"))
  if (!(by %in% names(df))) stop(paste(by, "not found in dataframe"))
  if (!(is.factor(df[[by]]) | is.character(df[[by]]))) stop ("by variable is neither character nor factor")

  #' @import dplyr

  if (class(df[[by]]) == "factor") {

    # to avoid empty factor levels making crush the gc_combine function

    f_levels <- levels(df[[by]])
    used_levels <- f_levels[f_levels %in% unique(df[[by]])]

    # redefine factor using used_levels
    df[[by]] <- factor((df[[by]]), levels=used_levels)
  }

  tapply(df[[var]], df[[by]], gd_numeric, ...)  %>%
  .gd_combine_df_list %>%
  mutate(Variable = var)
}
