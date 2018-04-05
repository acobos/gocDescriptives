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
#' @param NA_label \code{character}. Label for missings in the output. Defaults to "Missing".
#' @param exclude \code{character}. Levels to be excluded in the computation of valid %. Defaults to
#' "No disponible". Can specify more than one, e.g.: c("No sabe", "No contesta", "Desconocido").
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
#' # Example data without missings
#' sex <- c(rep("Male",10), rep("Female", 15))
#' 
#' # Frequency table
#' gd_categ(sex)
#' 
#' # Show info on missings, even if none
#' gd_categ(iris$Species, useNA = "always")
#' 
#' Example data with missings
#' sex <- c(sex, NA, NA)
#' 
#' # Frequency table, including missings (total %)
#' gd_categ(sex)
#' gd_categ(sex, NA_label = "Faltan")
#'
#' # Frequency table, excluding missings (Not recommended!)
#' gd_categ(sex, useNA = "no")
#'
#' Example data with levels to be excluded for valid % calculation
#' sex <- c(sex, rep("Unknown", 5))
#' gd_categ(sex, exclude = "Unknown")
#'
#' @export
gd_categ <- function (x, useNA = "ifany", NA_label = "Missing", exclude = "No disponible") {

  # input validation
    if (!is.vector(x) & !is.factor(x)) stop("x is neither a vector nor a factor")
    if (length(x)==0) stop("x has zero length")
    if ((length(NA_label) != 1) | (class(NA_label)[1] != "character")) 
        stop("NA_label not a character of length 1")
    if (class(exclude) != "character") stop("exclude is not a character vector")
    
    
    # Counts
    res <- as.data.frame(table(x, useNA = useNA))
    
    total <- sum(res$Freq)
    
    valid_n <- sum(res$Freq[!(is.na(res$x) | res$x %in% exclude)])
    
    
    res$Perc <- formatC(sprintf("(%.1f %%)", 100 * res$Freq / sum(res$Freq)),
                        width=9)
    
    # si hay missings y
    # if (sum(is.na(x)) > 0 & useNA !="no") {
    if (valid_n < total) {
        res$Perc <- ifelse(!is.na(res$x) & !(res$x %in% exclude) ,
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
    res[is.na(res$Key),"Key"] <- NA_label
    res %>% select(Variable, Key, Value)
}

