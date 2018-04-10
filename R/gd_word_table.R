#' Creates a RTF/Word version of standard descriptive result dataframes.
#'
#' Creates a RTF/Word version of standard dataframes with descriptive results, 
#' by means of the `flextable` function/package. 
#'
#' @param df A dataframe with  columns (\code{Variable}, \code{Key},and \code{Value}) containing
#' descriptive results.
#' @param title Optional. Character to be sed as table title.
#'
#' @return A list (of class `complextable`).
#'
#' @examples
#' # Example data 
#' set.seed(123)
#' Sex <- sample(c("Male", "Female"), 100, replace=TRUE)
#' Age <- floor(sample(50 + 10 * rnorm(100)))
#' Group <- sample(LETTERS[1:2], 100, replace=TRUE)
#' dat <- data.frame(Sex, Age, Group)
#' 
#' # Describe dat and save as result
#' gd_df(dat) -> result
#' 
#' # Create a word table object 
#' gd_word_table(result, "Demographic data")
#' 
#' # Same using pipes
#' library(dplyr)
#' dat %>%
#'     gd_df() %>%
#'     gd_word_table("Demographic data")
#' 
#' # An example with by-group description
#' dat %>%
#'     gd_df_by("Group") %>% 
#'     gd_word_table("Demo data")
#'     
#'     
#' @export
gd_word_table <- function (df, title="") {

  #' @import dplyr flextable

  df %>%
    flextable() %>%
    merge_v(j = c("Variable")) %>%
    theme_booktabs() %>%
    align(j = 1:2, align="left", part="all") %>%
    align(j = 3, align="center", part="all") %>%
    set_header_labels(Variable = title,
                      Key = title,
                      Value = title) %>%
    merge_h(part = "header") %>%
    autofit()
}
