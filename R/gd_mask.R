#' Masks numbers in columns of a dataframe
#'
#' Masks columns of a dataframe, changing numbers by character "x". This is 
#' useful to prepare templates for tables in Statistcal Analysis Plans.
#'
#' @param df \code{dataframe} with column(s) to be masked.
#' @param col Columns to be masked. Can be \code{character} or 
#' \code{numeric}, indicating column names or positions. See examples.
#' 
#' @return A \code{dataframe} with same columns as \code{df}, with masked values 
#' for  \code{cols}.
#' 
#' @examples
#' # Example data
#' set.seed(123) 
#' Sex <- sample(c("Male", "Female"), 100, replace=TRUE)
#' Age <- floor(sample(50 + 10 * rnorm(100)))
#' dat <- data.frame(Sex, Age)
#' 
# Get results and mask them
#' library(dplyr)
#' dat %>%
#'     gd_df() %>%
#'     gd_mask()
#' 
#' @export
gd_mask <- function (df, col = "Value") {
    
    for (i in col) {
        df[[i]] <- stringr::str_replace_all(df[[i]], "[:digit:]", "x")
    }
    
    df
}


