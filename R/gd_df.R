#' Describes all variables in a dataframe
#'
#' Describes all variables in a dataframe.
#'
#' @param df The \code{dataframe} to be described.
#' @param useNA,NA_label,exclude Options to control the description of 
#' categorical variables. See \code{\link{gd_categ}} for details. 
#' @param decimals Option to control the rounding of statistics for numeric 
#' variables. See \code{gd_numeric} for details.
#' @param date_format Option to control the formatting of dates. See 
#' \code{gd_date} for details.
#'
#'
#' @return A \code{dataframe} with three columns: \code{Variable}, \code{Key},
#' \code{Value}.
#'
#' @examples
#' # Example data 
#' set.seed(123)
#' Sex <- sample(c("Male", "Female"), 100, replace=TRUE)
#' Age <- floor(sample(50 + 10 * rnorm(100)))
#' dat <- data.frame(Sex, Age)
#' 
#' # Describing dataframe dat                       
#' gd_df(dat)                  
#' 
#' @export
gd_df <- function (df, 
                   var_labels = NA,
                   useNA = "ifany", 
                   NA_label = "Missing", 
                   exclude = "No disponible",
                   decimals = NA, 
                   date_format = "%d-%b-%y") {

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
      res <- gd_numeric(df[[varname]], 
                        decimals = NA) # decimals
    }

    # if character or factor
    if (is.factor(df[[varname]]) | is.character(df[[varname]])) {
      res <- gd_categ(df[[varname]], 
                      useNA = useNA,
                      NA_label = NA_label,
                      exclude = exclude) }

    # if date
    if (class(df[[varname]])[1] %in% c("Date", "POSIXlt", "POSIXct", "POSIXt")) {
      res <- gd_date(df[[varname]], 
                     date_format = date_format)
    }

    # sets the Variable in results and accumulates results
    res$Variable <- varname
    # results <- rbind(results, res)
    results <- dplyr::bind_rows(results, res)
  }

  # returns
  results
  
}
