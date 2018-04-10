#' Describes a categorical variable
#'
#' Describes a categorical variable; creates its frequency table, including both
#'   absolute (n) and relative frequencies (\%). Percentages are computed on the total number of 
#'   observations, and also on the total number of valid (e.g., non-missing) observations. 
#'
#' @param x \code{character} or \code{factor} variable to be described.
#' @param useNA \code{character}. Controls the handling of missing values, should be one of 
#' \code{c("no", "ifany", "always")}. Defaults to \code{"ifany"}. See Details and examples.
#' @param NA_label \code{character} . Label for missings in the output. Defaults to "Missing".
#' @param exclude \code{character}. Level(s) to be excluded in the computation of valid \%. Defaults 
#' to \code{"No disponible"}. More than one level can be specified, e.g.: \code{c("No sabe", 
#' "No contesta", "Desconocido")}. See examples.
#' @return A \code{dataframe} with three columns: \code{Variable}, \code{Key},
#' \code{Value}.
#'
#' @details When \code{useNA = "ifany"} (default), missing values are included
#' and percentages are computed on the total number of cases. When
#' \code{useNA = "no"}, missing values are excluded. When \code{useNA = "always"} info on
#' missings is shown, even if there is none.
#'
#' @examples
#' # Example data without missings
#' sex <- c(rep("Male",10), rep("Female", 15))
#' sex
#' 
#' # Frequency table
#' gd_categ(sex)
#' 
#' # Show info on missings, even if none
#' gd_categ(sex, useNA = "always")
#' 
#' Example data with missings
#' sex <- c(sex, NA, NA)
#' sex
#' 
#' # Frequency table, including missings
#' gd_categ(sex)
#' gd_categ(sex, NA_label = "Not available")
#'
#' # Frequency table, excluding missings (Not recommended!)
#' gd_categ(sex, useNA = "no")
#'
#' Example data with levels to be excluded for valid % calculation
#' sex <- c(sex, rep("Unknown", 5))
#' sex
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
    
    
    res$Perc <- formatC(sprintf("(%.1f%%)", 100 * res$Freq / sum(res$Freq)),
                        width=8)
    
    # si hay missings y
    # if (sum(is.na(x)) > 0 & useNA !="no") {
    if (valid_n < total) {
        res$Perc <- ifelse(!is.na(res$x) & !(res$x %in% exclude) ,
                           paste(res$Perc,
                                 formatC(sprintf("[%.1f%%]", 100 * res$Freq / valid_n),
                                         width=8)),
                           paste(res$Perc,
                                 "        "))
    }
    
    res$Value <- paste0("n = ", res$Freq, res$Perc)
    
    res$Freq <- NULL
    res$Perc <- NULL
    res$Variable <- deparse(substitute(x))
    res$Key <- as.character(res$x)
    res[is.na(res$Key),"Key"] <- NA_label
    dplyr::select(res, Variable, Key, Value)
}

