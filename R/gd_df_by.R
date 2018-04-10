#' Describes a dataframe by groups
#'
#' Describes a dataframe by groups, defined by the \code{by} variable.
#' @param df \code{dataframe} to be described.
#' @param by Name of the variable defining groups. Should be \code{character} or
#' \code{factor}. 
#' @param ... Further arguments passed to function \code{gd_df}.
#' 
#' @return A \code{dataframe} with columns \code{Variable}, \code{Key}, and one
#' additional column for each level/value of the \code{by} variable.
#'
#' @examples
#' # Example data
#' set.seed(123)
#' Sex <- sample(c("Male", "Female"), 100, replace=TRUE)
#' Age <- floor(sample(50 + 10 * rnorm(100)))
#' Group <- sample(LETTERS[1:2], 100, replace=TRUE)
#' dat <- data.frame(Sex, Age, Group)
#' 
#' # Describe dataframe dat by Group
#' gd_df_by(dat, "Group")
#' 
#' # Same, using pipe
#' library(dplyr)
#' dat %>% 
#'   gd_df_by("Group")
#' 
#' # Introduce some missings and pass further arguments
#' dat$Sex[1:5] <- NA
#' gd_df_by(dat, "Group")
#' 
#' # Change the descriptor for missings
#' gd_df_by(dat, "Group", NA_label = "Unknown")

#' @export
gd_df_by <- function (df, by, ...) {
    
    # input validation
    if (!is.data.frame(df)) stop("df is not a dataframe")
    if (!(by %in% names(df))) stop(paste(by, "by variable not found in dataframe"))
    if (!(is.factor(df[[by]]) | is.character(df[[by]]))) stop ("by variable is neither character nor factor")
    
    # required packages
    #' @import dplyr
    #' @import purrrlyr
    #' @import tidyr
  
  # cannot compute in one step, because
  # Variable is reordered alphabetically !!
  df %>%
    purrrlyr::slice_rows(by) %>%
    purrrlyr::by_slice(gd_df, ... , .collate = "rows") %>%
    tidyr::spread(1, Value) %>%        # this reorders Variable !!!!
    data.frame() -> res_by

# compute in the whole sample, to use given Variable order 
  df %>%
    gd_df(...) %>%
    dplyr::filter(Variable != by) -> res_all
  
# merge with res_by to get given Variable order
  res_all %>%
    select(-Value) %>%
    left_join(res_by)  
}


