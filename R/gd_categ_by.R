#' Describes a dataframe categorigal variable, by groups
#'
#' Describes a dataframe categorigal variable, by groups defined by another
#' categorical variable in the same dataframe.
#'
#' @param df The \code{dataframe} containing varibles.
#' @param var The categorical variable to be described. Should be either
#' \code{character} or \code{factor}.
#' @param by The categorical variable for grouping. Should be either
#' \code{character} or \code{factor}.
#' @param ... Additional arguments to \code{gd_categ()}
#'
#' @return A \code{dataframe} with columns: \code{Variable}, \code{Key}, plus
#' one additional column for each group in the \code{by} variable.
#'
#' @examples
#' # Example data 
#' set.seed(123)
#' dat <- data.frame(Sex = sample(c("Male", "Female"), 100, replace=TRUE), 
#'                   Age = floor(sample(50 + 10 * rnorm(100))), 
#'                   Group = sample(LETTERS[1:2], 100, replace=TRUE))
#'                   
#' Describe Sex by Group
#' gd_categ_by(dat, "Sex", "Group")
#' 
#' Same using pipes
#' library(dplyr)
#' dat %>%
#'     gd_categ_by("Sex", "Group") 
#' 
#' @export
gd_categ_by <- function (df, var, by, ...) {

  # input validation
  if (!(var %in% names(df))) stop(paste(var, "not found in dataframe"))
  if (!(by %in% names(df))) stop(paste(by, "not found in dataframe"))
  if (!(is.factor(df[[by]]) | is.character(df[[by]]))) stop ("by variable is neither character nor factor")

  #' @import dplyr

  tapply(df[[var]], df[[by]], gd_categ, ...) %>%
    .gd_combine_df_list %>%
    mutate(Variable = var)

}
