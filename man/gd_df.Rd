% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_df.R
\name{gd_df}
\alias{gd_df}
\title{Describes all variables in a dataframe}
\usage{
gd_df(df, useNA = "ifany", NA_label = "Missing",
  exclude = "No disponible", decimals = NA, date_format = "\%d-\%b-\%y")
}
\arguments{
\item{df}{The \code{dataframe} to be described.}

\item{useNA, NA_label, exclude}{Options to control the description of 
categorical variables. See \code{\link{gd_categ}} for details.}

\item{decimals}{Option to control the rounding of statistics for numeric 
variables. See \code{gd_numeric} for details.}

\item{date_format}{Option to control the formatting of dates. See 
\code{gd_date} for details.}
}
\value{
A \code{dataframe} with three columns: \code{Variable}, \code{Key},
\code{Value}.
}
\description{
Describes all variables in a dataframe.
}
\examples{
# Example data 
set.seed(123)
Sex <- sample(c("Male", "Female"), 100, replace=TRUE)
Age <- floor(sample(50 + 10 * rnorm(100)))
dat <- data.frame(Sex, Age)

# Describing dataframe dat                       
gd_df(dat)                  

}
